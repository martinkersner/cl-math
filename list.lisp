;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/06/04

(setf *list-namespace* '())

;;; Generate range of numbers.
;;; Output similar to Python.
(push 'range *list-namespace*)
(defun range (start end &optional (step 1) (lst NIL))
  (if (< start end)
    (range (+ start step) end step (append lst (list start)))
    lst))

;;; http://aima.cs.berkeley.edu/lisp/utilities/utilities.lisp
;;; Return a list of n consecutive integers, by default starting at 0.
(push 'iota *list-namespace*)
(defun iota (n &optional (start-at 0))
  (if (<= n 0) nil (cons start-at (iota (- n 1) (+ start-at 1)))))

(push 'iota-range *list-namespace*)
(defun iota-range (from to)
  (iota (- (1+ to) from) from))

;;; Return the last element of given list.
(push 'last-elem *list-namespace*)
(defun last-elem (lst)
  (car (last lst)))

;;; Creates valid index in list.
(push 'circular-index *list-namespace*)
(defun circular-index (idx lst)
  (let* ((l (length lst))
         (new-idx (mod (abs idx) l))
         (start-idx (if (< idx 0)
                      (if (equal new-idx 0)
                        (progn
                          (setf new-idx 0)
                          0)
                        (progn
                          (setf new-idx (- new-idx))
                          l))
                      0)))

    (+ start-idx new-idx)))

;;; Access list with positive or negative index.
(push 'nth-pos-neg *list-namespace*)
(defun nth-pos-neg (idx lst)
  (nth (circular-index idx lst) lst))

;;; SETF expander for NTH set with positive or negative indiex.
;;; TODO unit test?
(defun (setf nth-pos-neg) (val idx lst)
  (setf (nth (circular-index idx lst) lst) val))

;;; Find the minimum value and its index in list.
(push 'minimum *list-namespace*)
(defun minimum (lst)
  (let ((min-idx 0)
        (min-val (car lst)))

  (mapcar #'(lambda (x y) (if (< x min-val)
                            (progn
                              (setf min-val x)
                              (setf min-idx y))))
          lst (iota (length lst)))

  (values min-val min-idx)))

;;; Find minimum value in a list.
(push 'minimum-val *list-namespace*)
(defun minimum-val (lst)
  (apply 'min lst))

;;; Find the index of minimum value in a list.
(push 'minimum-idx *list-namespace*)
(defun minimum-idx (lst)
  (cadr (multiple-value-list (minimum lst))))

;;; Find the maximum value and its index in list.
(push 'maximum *list-namespace*)
(defun maximum (lst)
  (let ((max-idx 0)
        (max-val (car lst)))

  (mapcar #'(lambda (x y) (if (> x max-val)
                            (progn
                              (setf max-val x)
                              (setf max-idx y))))
          lst (iota (length lst)))

  (values max-val max-idx)))

;;; Find maximum value in a list.
(push 'maximum-val *list-namespace*)
(defun maximum-val (lst)
  (apply 'max lst))

;;; Find the index of maximum value in a list.
(push 'maximum-idx *list-namespace*)
(defun maximum-idx (lst)
  (cadr (multiple-value-list (maximum lst))))

;;; Samuel Edwin Ward
;;; http://stackoverflow.com/questions/9444885/common-lisp-how-to-return-a-list-without-the-nth-element-of-a-given-list
;;; Remove the nth element from list.
(push 'remove-nth *list-namespace*)
(defun remove-nth (n list)
  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))

;;; Reorganize values from given list in random fashion.
(push 'randomize-list *list-namespace*)
(defun randomize-list (lst)
  (let* ((len-lst (length lst))
         (rand-idx 0) ; 0 or NIL?
         (rand-lst NIL))

    (dotimes (i len-lst)
      (setf rand-idx (random (- len-lst i)))
      (push (nth rand-idx lst) rand-lst)
      (setf lst (remove-nth rand-idx lst)))

    rand-lst))

;; TODO description
(push 'sum-two-lists *list-namespace*)
(defun sum-two-lists (lst-1 lst-2)
  (mapcar #'(lambda (num-1 num-2) (+ num-1 num-2)) lst-1 lst-2))

;;; Sum lists.
;;; ((1 1) (2 2)) = > (3 3)
(push 'sum-list-of-lists *list-namespace*)
(defun sum-list-of-lists (lst-lsts)
  (let ((zero-vec (make-list (length (car lst-lsts)) :initial-element 0)))

    (mapcar #'(lambda (lst) (setf zero-vec (sum-two-lists zero-vec lst))) lst-lsts)

    zero-vec))
