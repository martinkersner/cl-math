# cl-math

Mathematical library for Common Lisp.

Martin Kersner, m.kersner@gmail.com

## Matrices and Vectors
Matrix is defined as a structure with three slots
* rows
* cols
* data.

*rows* and *cols* slots hold size of matrix. *data* is formed by list of lists, where each list represents row of matrix and individual values of lists represent cells of matrix. Matrix with only one row or one column is considered as vector, therefore any function for matrices can be also applied to vectors. Row and columns indexes starts at 0.

For the large part of functions there aren't any control checks on parameters that are provided.

### Matrix Creation
Matrix with 2 rows and 2 columns containing with two values (1 and 2) in the first row and two values (3 and 4) in the second row can be created with the following command.

```lisp
(make-matrix :rows 2
             :cols 2
             :data '((1 2)
                     (3 4)))
```

However, this approach of creating matrix with known data values is rather error prone. Recommended command for creating new matrix is following.

```lisp
(matrix-from-data '((1 2)
                    (3 4)))
```

If you need to create matrix of specific size (*rows* and *cols*) and fill it with specific *value*, you can use function called *initialize-matrix*.
```lisp
(initialize-matrix rows cols val)
```

In order to generate matrix of the same size as some already existing matrix you can use one of two functions *zero-matrix-like* or *empty-matrix-like*. 
```lisp
(zero-matrix-like mat)  ; matrix will be filled with zeros
(empty-matrix-like mat) ; matrix will be filled with NIL
```

### Matrix Information
After matrix is created we can request information about it.

```lisp
(matrix-rows mat)
(matrix-cols mat)
(matrix-shape mat) ; return list with number of rows and cols
(matrix-data mat)
```

### Data Access
Any row or column of matrix can be accesed via
```lisp
(nth-row row-idx mat)
(nth-col col-idx mat)
```

For more complicated value accesses you should use *[]* function.
```lisp
([] mat :row 0)                  ; first row of matrix
([] mat :row '(0 2))             ; first, second and third row of matrix
([] mat :col 0)                  ; first column of matrix
([] mat :col '(0 2))             ; first, second and third column of matrix
([] mat :row 0 :col 0)           ; value in second row and second column
([] mat :row '(0 1) :col '(0 1)) ; submatrix of size 2x2 from the first two rows and columns
```

### Data Modification
Values of matrix can be replaced with *setf* function. To specify concrete rows and columns, *[]* function should be used.
```lisp
(setf m (matrix-from-data '((1 2 3)(4 5 6))))
(setf ([] m :row 1) '((9 9 9)))
```

### Matrix Operations
cl-math provides various matrix operations, from simple ones like matrix transpose to more complicated ones (e.g. matrix inverse). Matrix operations are divided to several groups

* single matrix operations
* matrix to matrix operations
* matrix and single value operations
* matrix and row/column operations
* row/column operations

#### Single Matrix Operations
```lisp
(transpose m)
(det m) ; determinant
(inv m) ; inverse of matrix
(sigmoid m) ; element-wise sigmoid operation on matrix
```

```lisp
(remove-col idx m)
(remove-row idx m)
```

#### Matrix To Matrix Operations
```lisp
(dot m1 m2) ; matrix multiplication
(+mm m1 m2) ; element-wise matrix addition
(-mm m1 m2) ; element-wise matrix subtraction
(*mm m1 m2) ; element-wise matrix multiplication
(/mm m1 m2) ; element-wise matrix division
```

Matrix concatenation can be performed using *vstack* and *hstack* functions.
```lisp
(vstack m-left m-right)
(hstack m-top m-bottom)
```

#### Matrix And Single Value Operations
Value *v* represents single number or expression leading to single number.

```lisp
(+mv m v) ; add value v to each element of matrix m
(-mv m v) ; subtract value v from each element of matrix m
(*mv m v) ; multiply value v with each element of matrix m
(/mv m v) ; divide value v with each element of matrix m
```

#### Matrix And Row/Column Operations
##### Row Operations
```lisp
(+mr m r) ; add row r to each row of matrix m
(-mr m r) ; subtract row r from each row of matrix m
(*mr m r) ; multiply row r with each row of matrix m
(/mr m r) ; divide row r with each row of matrix m
```

##### Column Operations
```lisp
(+mc m c) ; add column c to each column of matrix m
(-mc m c) ; subtract column c from each column of matrix m
(*mc m c) ; multiply column c with each column of matrix m
(/mc m c) ; divide column c with each column of matrix m
```

#### Row/Column Operations
```lisp
(nth-col-min col-idx mat)
(nth-col-max col-idx mat)

(nth-row-min row-idx mat)
(nth-row-max row-idx mat)
```

```lisp
(sum-rows mat)
(sum-cols mat)
```
