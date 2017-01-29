;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/01/29

(asdf:defsystem cl-math
  :name    "cl-math"
  :version "0.0.1"
  :author  "Martin Kersner, <m.kersner@gmail.com>"
  :long-description "Mathematical library for Common Lisp"
  :serial t
  :components ((:file "package")
               (:file "math")
               (:file "random")
               (:file "list")
               (:file "matrix")

               (:module unit-tests
                  :components ((:file "unit-test")
                               (:file "unit-test-list")
                               (:file "unit-test-math")
                               (:file "unit-test-matrix")))
               )
  )
