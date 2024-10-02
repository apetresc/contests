(defpackage :darts
  (:use :cl)
  (:export :score))

(in-package :darts)

(defun distance (x y) (sqrt (+ (* x x) (* y y))))

(defun score (x y)
  (let ((d (distance x y)))
    (cond
      ((<= d 1) 10)
      ((<= d 5) 5)
      ((<= d 10) 1)
      (:otherwise 0)
    )))
