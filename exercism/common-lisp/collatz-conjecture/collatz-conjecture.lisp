(defpackage :collatz-conjecture
  (:use :cl)
  (:export :collatz))

(in-package :collatz-conjecture)

(defun collatz (n &optional (k 0))
  (cond
    ((< n 1) nil)
    ((= n 1) k)
    ((evenp n) (collatz (floor n 2) (1+ k)))
    (:otherwise (collatz (+ 1 (* n 3)) (1+ k)))
    )
  )
