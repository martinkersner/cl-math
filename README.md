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
