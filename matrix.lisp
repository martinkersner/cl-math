;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/04/27 
;;;;
;;;; Marix 2.0
;;;; Perform mathematical operations, mainly with matrices (vector is considered
;;;; as matrix as well). Matrices are represented by lists. Most operations have
;;;; strict prototype in order to avoid running additional functions and don't
;;;; control underflow or overflow of dimension indices. Rows and cols are
;;;; counted from zero.

;;; MATRIX CREATION
;;; * (empty-matrix rows cols)
;;; * (empty-matrix-like mat)
;;; * (zero-matrix-like mat)
;;; * (initialize-matrix rows cols val)
;;; * (rand-norm-matrix rows cols)
;;; * (matrix-from-data data)
;;; * (matrix-from-data-peel data)
;;; * (matrix-data-peel data)
;;;
;;; SINGLE MATRIX OPERATIONS
;;; * (shape mat)
;;; * (transpose mat)
;;; * (nth-row row mat)
;;; * (nth-col col mat)
;;; * ([] from to mat)
;;; * ([][] row col mat)
;;; * (remove-col col mat)
;;; * (remove-row col mat)
;;; * (remove-row-list-matrix row list-mat)
;;; * (prefix-const-val val mat)
;;; * (suffix-const-val val mat)
;;; * (insert-const-val idx val mat)
;;; * (matrix-indices rows cols)
;;; * (sigmoid mat)
;;; * (sigmoid-prime mat)
;;; * (shuffle-rows mat)
;;; * (shuffle-rows-spec mat idx-list)
;;; * (det mat)
;;; * (inv mat)
;;;
;;; MATRIX MULTIPLICATION
;;; ** number of cols of mat1 has to be equal number of rows of mat2
;;; * (dot mat1 mat2)
;;;
;;; ELEMENT-WISE OPERATIONS
;;; ** requires the same dimensions of both matrices
;;; ** uniqueness of parameter order depends on commutative property of employed
;;;    mathematical function
;;; * (add mat1 mat2)
;;; * (subtract mat1 mat2)
;;; * (matrix-mult mat1 mat2)
;;; * (matrix-div mat1 mat2)
;;;
;;; MATRIX & VALUE OPERATIONS
;;; TODO unite names
;;; * (value-matrix-subtract val mat)
;;; * (add-value val mat)
;;; * (multiply val mat)
;;; * (power val mat)
;;;
;;; MATRIX-ROW/COL OPERATIONS
;;; * (subtract-row mat row)
;;; * (subtract-col mat col)
;;; * (subtract-val-col val col mat)
;;; * (sum-rows mat)
;;; * (sum-cols mat)
;;; * (sum mat)
;;; * (mean-cols mat)
;;; * (std-cols mat mean)
;;; * (mean-rows mat)
;;; *(arg-sort-col-mat col_mat) TODO accept matrices with more than one column
;;; * TODO (arg-sort-row-mat row_mat)
;;; * (nth-col-max col mat)
;;; * (nth-col-min col mat)
;;; * (nth-row-max row mat)
;;; * (nth-row-min row mat)
;;;
;;; MATRIX to MATRIX OPERATIONS
;;; * (vstack mat-left mat-right)
;;; * TODO (hstack mat-top mat-bottom)

;;; TODO 
;;; smart operations (add vector to all rows or columns of matrix)
;;; nth-row nth-col base functions for returning complete matrices
;;; unit test for EVERY FUNCTION
;;; merge similar code from multiply, power, value-matrix-subtract functions together
;;; more inspiration from numpy
;;; generate all functions, keep them in categories

(in-package :cl-math)

(setf *matrix-namespace* '())

(defstruct matrix rows cols data)

(define-condition matrix-error (error)
  ((text :initarg :text :reader text)))

;;; Compare rows and columns of two matrices and their data.
(push 'compare-matrix *matrix-namespace*)
(defun compare-matrix (mat_a mat_b)
  (cond 
    ((not (equal (matrix-rows mat_a) (matrix-rows mat_b))) nil)
    ((not (equal (matrix-cols mat_a) (matrix-cols mat_b))) nil)
    ((not (equal (matrix-data mat_a) (matrix-data mat_b))) nil)
    (t t)))

;;; MATRIX CREATION

(push 'empty-matrix-macro *matrix-namespace*)
(defmacro empty-matrix-macro (rows cols &rest default)
  `(flet ((generate-empty-matrix (,rows ,cols)
       (loop for i from 1 to ,rows
          collect ,@default)))

    (make-matrix :rows ,rows
                 :cols ,cols
                 :data (generate-empty-matrix ,rows ,cols))))

;;; Create a matrix of size [rows; cols] filled with NIL values.
(push 'empty-matrix *matrix-namespace*)
(defun empty-matrix (rows cols &optional (default NIL))
  (empty-matrix-macro rows cols (make-list cols :initial-element default)))

;;; Create an empty matrix of the same size as given matrix.
(push 'empty-matrix-like *matrix-namespace*)
(defun empty-matrix-like (mat)
  (empty-matrix (matrix-rows mat) (matrix-cols mat)))

;;; Create a matrix of the same size as given matrix with 0 values.
(push 'zero-matrix-like *matrix-namespace*)
(defun zero-matrix-like (mat)
  (empty-matrix (matrix-rows mat) (matrix-cols mat) 0))

;;; Generate and initialize matrix with a given value.
(push 'initialize-matrix *matrix-namespace*)
(defun initialize-matrix (rows cols val)
  (empty-matrix-macro rows cols (make-list cols :initial-element val)))

;;; Generate matrix filled with random normal values.
;;; mean 0
;;; std  1
;;; TODO: keep here?
(push 'rand-norm-matrix *matrix-namespace*)
(defun rand-norm-matrix (rows cols)
  (empty-matrix-macro rows cols (make-list-rand-normal cols)))

;;; Control if all rows have the same number of columns.
;;; TODO where to assign?
(push 'valid-matrix *matrix-namespace*)
(defmacro valid-matrix (row_lengths)
  `(if (= 0
          (apply #'+ (mapcar #'(lambda (x) (- x (car ,row_lengths))) ,row_lengths)))
     t
     nil))

;;; Create a matrix structure from given data (lists of lists).
;;; TODO should we use valid-matrix?
(push 'matrix-from-data *matrix-namespace*)
(defun matrix-from-data (data)
  (if (not (valid-matrix (mapcar #'length data)))
      (error 'matrix-error :text "Length of matrix rows is not consistent."))

  (let ((rows (length data))
        (cols (car (mapcar #'length data))))

    (make-matrix :rows rows
                 :cols cols
                 :data data)))

;;; Adds additional layer (list) around given data in order to be able to create
;;; matrix from this data.
(push 'matrix-from-data-peel *matrix-namespace*)
(defun matrix-from-data-peel (data)
  (matrix-from-data (list data)))

;;; Removes layer (access the first item of list) from given matrix.
(push 'matrix-data-peel *matrix-namespace*)
(defun matrix-data-peel (data)
  (car (matrix-data data)))

;;; SINGLE MATRIX OPERATIONS

(push 'shape *matrix-namespace*)
(defun shape (mat)
  (list (matrix-rows mat)
          (matrix-cols mat)))

;;; Auxiliary function. Shouldn't be employed by itself? TODO move to somewhere else?
(push 'transpose-list *matrix-namespace*)
(defun transpose-list (lst)
  (apply #'mapcar (cons #'list lst)))

;;; Transpose matrix.
(push 'transpose *matrix-namespace*)
(defun transpose (mat)
  (let ((rows (matrix-rows mat))
        (cols (matrix-cols mat))
        (data (matrix-data mat)))

    (make-matrix :rows cols
                 :cols rows
                 :data (transpose-list data))))

;;; Return n-th row from given matrix.
(push 'nth-row *matrix-namespace*)
(defun nth-row (row mat)
  (list (nth row (matrix-data mat))))

;;; Return n-th col from given matrix.
(push 'nth-col *matrix-namespace*)
(defmacro nth-col (col mat)
  `(mapcar #'(lambda (x) (list (nth ,col x))) (matrix-data ,mat)))

;;; Access subset of rows from given matrix.
;;; idx >= from AND idx <= to
(push '[] *matrix-namespace*)
(defun [] (from to mat)
  (let ((mat-list (matrix-data mat))
        (to-verif (min to (1- (matrix-rows mat))))
        (mat-tmp nil))

    (setf mat-tmp
          (matrix-from-data
            (mapcar #'(lambda (idx) (nth idx mat-list))
                    (iota (1+ (- to-verif from)) from))))

    (if (and (= (matrix-rows mat-tmp) 1)
             (= (matrix-cols mat-tmp) 1))
      (caar (matrix-data mat-tmp))
      mat-tmp)))

(defun (setf []) (val from to mat)
  (let ((mat-list (matrix-data mat))
        (to-verif (min to (1- (matrix-rows mat))))
        (mat-tmp nil))

    (setf mat-tmp
          (matrix-from-data
            (mapcar #'(lambda (idx) (setf (nth idx mat-list) (list val)))
                    (iota (1+ (- to-verif from)) from))))

    (if (and (= (matrix-rows mat-tmp) 1)
             (= (matrix-cols mat-tmp) 1))
      (caar (matrix-data mat-tmp))
      mat-tmp)))

;;; Access value of 2D matrix.
;;; Does not control access outside of matrix.
(push '[][] *matrix-namespace*)
(defun [][] (row col mat)
  (let ((selected-row ([] row row mat)))
    (setf val (mapcar #'(lambda (x) (nth col x)) (matrix-data selected-row)))

    (nth 0 val)))

;;; Remove column from given matrix.
;;; Create new matrix.
(push 'remove-col *matrix-namespace*)
(defun remove-col (col mat)
  (matrix-from-data
    (mapcar #'(lambda (x) (remove-nth col x)) (matrix-data mat))))

;;; Removes row from given matrix.
;;; Create a new matrix.
(push 'remove-row *matrix-namespace*)
(defun remove-row (row mat)
  (matrix-from-data (remove-nth row (matrix-data mat))))

;;; Remove row from matrix composed of lists.
(push 'remove-row-list-matrix *matrix-namespace*)
(defun remove-row-list-matrix (row list-mat)
  (remove-nth row list-mat))

;;; Append constant number at the beginning of each row of given matrix.
(push 'prefix-const-val *matrix-namespace*)
(defun prefix-const-val (val mat)
  (matrix-from-data (mapcar #'(lambda (x) (push val x)) (matrix-data mat))))

;;; Append constant number at the end of each row of given matrix.
(push 'suffix-const-val *matrix-namespace*)
(defun suffix-const-val (val mat)
  (matrix-from-data (mapcar #'(lambda (x) (append x (list val))) (matrix-data mat))))

;;; Insert constant value (whole column) at given position (column) in matrix.
;;; Does not control access out of boundaries.
(push 'insert-const-val *matrix-namespace*)
(defun insert-const-val (idx val mat)
  (let ((end (matrix-cols mat)))
    (matrix-from-data (mapcar #'(lambda (x) (append (subseq x 0 idx) (list val) (subseq x idx end)))
                              (matrix-data mat)))))

;;; Auxiliary function for MATRIX-INDICES.
(push 'matrix-indices-rec *matrix-namespace*)
(defun matrix-indices-rec (rows cols orig_cols)
  (if (eq rows 0)
    nil
    (cons (cons (1- rows) (1- cols))
          (if (eq (1- cols) 0)
            (matrix-indices-rec (1- rows) orig_cols orig_cols)
            (matrix-indices-rec rows (1- cols) orig_cols)))))

;;; Generate list of matrix indices from given matrix dimensions.
(push 'matrix-indices *matrix-namespace*)
(defun matrix-indices (rows cols)
  (matrix-indices-rec rows cols cols))

;;; Basic sigmoid function.
;;; Accept only single number.
;;; TODO move to only mathematical library for common functions?
(push 'sigmoid-base *matrix-namespace*)
(defun sigmoid-base (num)
    (handler-bind
      ((floating-point-underflow #'(lambda (x) (return-from sigmoid-base 1.0)))
       (floating-point-overflow  #'(lambda (x) (return-from sigmoid-base 0.0))))

      (/ 1 (+ 1 (exp (- num))))))

;;; Calculate sigmoid of each value in given matrix.
(push 'sigmoid *matrix-namespace*)
(defun sigmoid (mat)
  (matrix-from-data
    (mapcar #'(lambda (x) (mapcar #'(lambda (y) (sigmoid-base y)) x))
            (matrix-data mat))))

;;; Derivation of sigmoid function.
(push 'sigmoid-prime *matrix-namespace*)
(defun sigmoid-prime (mat)
  (let ((s (sigmoid mat)))
    (matrix-mult s (value-matrix-subtract 1 s))))

;;; Randomly shuffle rows of matrix.
;;; TODO unit test?
(push 'shuffle-rows *matrix-namespace*)
(defun shuffle-rows (mat)
  (let ((n-rows (matrix-rows mat))
        (mat-list (matrix-data mat))
        (mat-shuffled NIL)
        (rand-idx 0))

  (dotimes (i n-rows)
    (setf rand-idx (random (- n-rows i)))
    (push (nth rand-idx mat-list) mat-shuffled)
    (setf mat-list (remove-row-list-matrix rand-idx mat-list)))

  (matrix-from-data mat-shuffled)))

;;; Shuffle rows according to specification.
;;; Specification is list with index ordering.
;;; Function assumes number of indexes less than number of matrix rows.
(push 'shuffle-rows-spec *matrix-namespace*)
(defun shuffle-rows-spec (mat idx-list)
  (let ((mat-list (matrix-data mat)))

    (matrix-from-data
      (mapcar #'(lambda (idx) (nth idx mat-list)) idx-list))))

;;; Calculate determinant of given matrix.
;;; Matrix has to have square shape.
(push 'det *matrix-namespace*)
(defun det (mat)
  (let ((rows (matrix-rows mat))
        (cols (matrix-cols mat))
        (first-row (nth 0 (nth-row 0 mat))))

    (cond
      ((not (eq rows cols))
        (error 'matrix-error :text "Matrix must be square"))

      ((eq rows 2)
        (- (* ([][] 0 0 mat) ([][] 1 1 mat))
           (* ([][] 0 1 mat) ([][] 1 0 mat))))

      (t
        (apply #'+ (mapcar #'(lambda (idx val) (if (is-odd idx)
                                                  (apply #'* (list -1 val (det (det-submatrix idx mat))))
                                                  (apply #'* (list val (det (det-submatrix idx mat))))))
                                (iota cols) first-row)))
    )))

;;; Extract submatrix used in recursive determinant calculation.
(push 'det-submatrix *matrix-namespace*)
(defun det-submatrix (col mat)
  (let ((last-row-idx (- (matrix-rows mat) 1)))
    (remove-col col ([] 1 last-row-idx mat))))

;;; Compute inverse of a given matrix.
(push 'inv *matrix-namespace*)
(defun inv (mat)
  (let ((rows (matrix-rows mat))
        (cols (matrix-cols mat))
        (tmp-row-flag NIL)
        (tmp-col-flag NIL))

    (cond
      ((not (eq rows cols))
        (error 'matrix-error :text "Matrix must be square"))

      ((eq rows 2)
        (multiply (/ 1 (det mat))
                  (matrix-from-data (list (list    ([][] 1 1 mat)  (- ([][] 0 1 mat)))
                                          (list (- ([][] 1 0 mat))    ([][] 0 0 mat))))))

      (t
        (multiply (/ 1 (det mat))
           (transpose (matrix-from-data
             (mapcar #'(lambda (row) (progn (setf tmp-row-flag (if (is-odd row) -1 1))
                                            (mapcar #'(lambda (col) (progn (setf tmp-col-flag (if (is-odd col) -1 1))
                                                                           (apply #'* (list tmp-row-flag tmp-col-flag (det (remove-col col (remove-row row mat)))))))
                               (iota cols)))) (iota rows)))))))))

;;; Apply lambda function to each value of matrix.
(push 'apply-matrix *matrix-namespace*)
(defmacro apply-matrix (lmbd-fun mat)
  `(let ((matrix-lst (matrix-data ,mat)))

     (matrix-from-data
       (mapcar #'(lambda (row)
                   (mapcar #',lmbd-fun row)) matrix-lst))))

;;; MATRIX MULTIPLICATION

;;; Element-wise product of two vectors.
;;; Assume correct vector dimensions.
;;; TODO move to some library with simplier functions?
(push 'vec-mult *matrix-namespace*)
(defun vec-mult (vec1 vec2)
  (apply #'+ (mapcar #'* vec1 vec2)))

;;; Calculate the new values for one cell of result matrix.
;;; Auxiliary function for DOT-REC.
(push 'dot-cell-calc *matrix-namespace*)
(defun dot-cell-calc (mat_out row_idx col_idx row_vec col_vec)
  (setf (nth col_idx (nth row_idx (matrix-data mat_out)))
        (vec-mult (car row_vec)
                   (car (transpose-list col_vec))))

  mat_out)

;;; Auxiliary function for DOT function.
(push 'dot-rec *matrix-namespace*)
(defun dot-rec (mat_out mat_l mat_r mat_idxs)
  (if (eq (car mat_idxs) nil)
    mat_out
    (let* ((row_idx (caar mat_idxs))
           (col_idx (cdar mat_idxs))
           (row_vec (nth-row row_idx mat_l))
           (col_vec (nth-col col_idx mat_r)))

    (dot-rec (dot-cell-calc mat_out row_idx col_idx row_vec col_vec) mat_l mat_r (cdr mat_idxs)))))

;;; Control matrix dot product validity.
(push 'valid-dot-op *matrix-namespace*)
(defun valid-dot-op (mat_l mat_r)
  (if (not (= (matrix-cols mat_l)
              (matrix-rows mat_r)))
    (error 'matrix-error :text "Matrices cannot be multiplied. Dimensions do not fit.")))

;;; Matrix multiplication of two matrices.
;;; TODO should we slow down performance with VALID-DOT-OP?
(push 'dot *matrix-namespace*)
(defun dot (mat_l mat_r)
  (valid-dot-op mat_l mat_r)

  (let* ((rows (matrix-rows mat_l))
         (cols (matrix-cols mat_r))
         (mat_out (empty-matrix rows cols))
         (mat_idxs (matrix-indices rows cols)))

    (setf mat_out (dot-rec mat_out mat_l mat_r mat_idxs))

    (if (and (= (matrix-rows mat_out) 1)
             (= (matrix-cols mat_out) 1))
      (caar (matrix-data mat_out))
      mat_out)))

;;; ELEMENT-WISE OPERATIONS
;;; TODO simplify/shorten definition of those functions, too much repetition

;;; Auxiliary function for ADD, SUBTRACT and MATRIX-MULT.
(push 'element-wise-op *matrix-namespace*)
(defun element-wise-op (lst_l lst_r op)
  (mapcar #'(lambda (x y) (mapcar op x y)) lst_l lst_r))

;;; Element-wise add for matrices.
(push 'add *matrix-namespace*)
(defun add (mat_l mat_r)
  (matrix-from-data
    (element-wise-op (matrix-data mat_l) (matrix-data mat_r) #'+)))

;;; Elementwise subtract for matrices.
(push 'subtract *matrix-namespace*)
(defun subtract (mat_l mat_r)
  (matrix-from-data
    (element-wise-op (matrix-data mat_l) (matrix-data mat_r) #'-)))

;;; Elementwise matrix multiplication.
(push 'matrix-mult *matrix-namespace*)
(defun matrix-mult (mat_l mat_r)
  (matrix-from-data
    (element-wise-op (matrix-data mat_l) (matrix-data mat_r) #'*)))

(push 'matrix-div *matrix-namespace*)
(defun matrix-div (mat_l mat_r)
  (matrix-from-data
    (element-wise-op (matrix-data mat_l) (matrix-data mat_r) #'/)))

;;; MATRIX & VALUE OPERATIONS

;;; Subtract matrix value from given matrix.
(push 'value-matrix-subtract *matrix-namespace*)
(defun value-matrix-subtract (val mat)
  (matrix-from-data
    (mapcar #'(lambda (x) (mapcar #'(lambda (y) (- val y)) x))
            (matrix-data mat))))

;;; Add constant value to given matrix.
;;; TODO merge base of value-matrix based functions
(push 'add-value *matrix-namespace*)
(defun add-value (val mat)
  (matrix-from-data
    (mapcar #'(lambda (x) (mapcar #'(lambda (y) (+ y val)) x))
            (matrix-data mat))))

;;; Multiply matrix with a given value.
(push 'multiply *matrix-namespace*)
(defun multiply (val mat)
  (matrix-from-data
    (mapcar #'(lambda (x) (mapcar #'(lambda (y) (* y val)) x))
            (matrix-data mat))))

;;; Compute power using given exponent at each cell of matrix.
(push 'power *matrix-namespace*)
(defun power (val mat)
  (matrix-from-data
    (mapcar #'(lambda (x) (mapcar #'(lambda (y) (expt y val)) x))
            (matrix-data mat))))

;;; MATRIX-ROW/COL OPERATIONS

;;; Auxiliary function for ELWISE-MAT-ROW-OP.
(push 'elwise-row-row-op *matrix-namespace*)
(defun elwise-row-row-op (lst_row_l lst_row_r op)
  (mapcar #'(lambda (x y) (apply op (list x y))) lst_row_l lst_row_r))

;;; Auxiliary function for SUBTRACT-ROW.
(push 'elwise-mat-row-op *matrix-namespace*)
(defun elwise-mat-row-op (lst_mat lst_row op)
  (mapcar #'(lambda (x) (elwise-row-row-op x lst_row op)) lst_mat))

;;; Element-wise subtract values of given row from all rows in matrix.
(push 'subtract-row *matrix-namespace*)
(defun subtract-row (mat row)
  (matrix-from-data
    (elwise-mat-row-op (matrix-data mat) (car (matrix-data row)) #'-)))

;;; Element-wise subtract values of given column from all columns in matrix.
(push 'subtract-col *matrix-namespace*)
(defun subtract-col (col mat)
  (let ((col-trans (transpose col))
        (mat-trans (transpose mat)))

    (transpose
      (subtract-row col-trans mat-trans))))

;;; Subtract value from specified column in matrix.
(push 'subtract-val-col *matrix-namespace*)
(defun subtract-val-col (val col mat)
  (matrix-from-data
    (mapcar #'(lambda (row) (let ((row-val (nth col row)))
                                   (setf (nth col row) (- row-val val))
                                   row))
              (matrix-data mat))))

;;; Perform aggregating operation on each row of matrix.
;;; Auxiliary function for SUM-ROWS.
(push 'rows-op *matrix-namespace*)
(defun rows-op (mat_lst op)
  (mapcar #'(lambda (x) (list (apply op x))) mat_lst))

;;; Sum values at each row of matrix.
(push 'sum-rows *matrix-namespace*)
(defun sum-rows (mat)
  (matrix-from-data
    (rows-op (matrix-data mat) #'+)))

;;; Sum values at each column of matrix.
(push 'sum-cols *matrix-namespace*)
(defun sum-cols (mat)
  (transpose (matrix-from-data
               (rows-op (matrix-data (transpose mat)) #'+))))

;;; Sum all values in a given matrix.
(push 'sum *matrix-namespace*)
(defun sum (mat)
  (let ((total-sum 0))
    (mapcar #'(lambda (x) (incf total-sum (reduce #'+ x)))
            (matrix-data mat))

    total-sum))

;;; Compute mean for all columns.
(push 'mean-cols *matrix-namespace*)
(defun mean-cols (mat)
  (let ((num-rows (matrix-rows mat)))

    (matrix-from-data (transpose-list
      (mapcar #'(lambda (column)
                  (list (/ (apply #'+ column) num-rows)))
              (transpose-list (matrix-data mat)))))))

;;; Compute mean for all rows
(push 'mean-rows *matrix-namespace*)
(defun mean-rows (mat)
  (let ((num-cols (matrix-cols mat)))
    
    (matrix-from-data 
      (mapcar #'(lambda (row) 
                  (list (/ (apply #'+ row) num-cols)) )
              (matrix-data mat)))))

;;; Compute standard deviation for all columns.
;;; TODO unit tests
(push 'std-cols *matrix-namespace*)
(defun std-cols (mat mean)
  (let ((mat-list (matrix-data mat))
        (mean-list (matrix-data-peel mean))
        (row-num (matrix-rows mat))
        (std-list (make-list (matrix-cols mat) :initial-element 0)))

    (mapcar #'(lambda (row)
      (setf std-list
        (mapcar #'+ std-list ;; add to temporary value
                (mapcar #'(lambda (val) (expt val 2)) ;; compute squares
                                     (mapcar #'- row mean-list)))))
      mat-list)

    (matrix-from-data-peel (mapcar #'sqrt
      (mapcar #'(lambda (val) (/ val row-num)) std-list)))))

;;; Normalize data.
(push 'normalize *matrix-namespace*)
(defun normalize (mat mean std)
  (let ((mean-list (matrix-data-peel mean))
        (std-list (matrix-data-peel std)))

    (matrix-from-data
      (mapcar #'(lambda (row)
                  (mapcar #'/ (mapcar #'- row mean-list) std-list))
              (matrix-data mat)))))

;;; Sorts column vector and return indices of sorted vector.
(push 'arg-sort-col-mat *matrix-namespace*)
(defun arg-sort-col-mat (col_mat)
  (let* ((vec (matrix-data col_mat))
         (idxs (iota (length vec)))
         (join-vec-idxs (mapcar #'(lambda (x y) (cons (car x) y)) vec idxs)))

    (mapcar #'(lambda (x) (cdr x)) (stable-sort join-vec-idxs #'< :key #'car))))

;;; Find the largest value in specific column of a given matrix.
;;; TODO merge common function for nth-col-max and nth-col-min
(push 'nth-col-max *matrix-namespace*)
(defun nth-col-max (col mat)
  (maximum (nth col (transpose-list (matrix-data mat)))))

;;; Find the smallest value in specific column of a given matrix.
(push 'nth-col-min *matrix-namespace*)
(defun nth-col-min (col mat)
  (minimum (nth col (transpose-list (matrix-data mat)))))

;;; Find the largest value in specific row of a given matrix.
(push 'nth-row-max *matrix-namespace*)
(defun nth-row-max (row mat)
  (maximum (nth row (matrix-data mat))))

;;; Find the largest value in specific row of a given matrix.
(push 'nth-row-min *matrix-namespace*)
(defun nth-row-min (row mat)
  (minimum (nth row (matrix-data mat))))

;;; Concatenate matrices vertically.
(push 'vstack *matrix-namespace*)
(defun vstack (mat-left mat-right)
  (if (not (and
        (= (matrix-rows mat-left) (matrix-rows mat-right))))
    (error 'matrix-error :text "Number of matrix rows does not correspondent each other.")

    (matrix-from-data (mapcar #'(lambda (l r)
                                  (concatenate 'list l r))
                              (matrix-data mat-left)
                              (matrix-data mat-right)))))

;;; Concatenate matrices horizontally.
(push 'hstack *matrix-namespace*)
(defun hstack (mat-top mat-bottom)
  (if (not (and
        (= (matrix-cols mat-top) (matrix-cols mat-bottom))))
    (error 'matrix-error :text "Number of matrix columns does not correspondent each other.")

    (matrix-from-data
      (concatenate 'list (matrix-data mat-top) (matrix-data mat-bottom)))))
