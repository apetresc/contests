(defpackage :difference-of-squares
  (:use :cl)
  (:export :sum-of-squares
           :square-of-sum
           :difference))

(in-package :difference-of-squares)

(defun square-of-sum (n)
  "Calculates the square of the sum for a given number."
  (expt (/ (* n (1+ n)) 2) 2))

(defun sum-of-squares (n)
  "Calculates the sum of squares for a given number."
  (/ (* n (1+ n) (1+ (* 2 n))) 6))

(defun difference (n)
  "Finds the diff. between the square of the sum and the sum of the squares."
  ; (- (square-of-sum n) (sum-of-squares n)))
  ; Actually, we can save a few operations by simplifying the algebra on paper first
  (/ (* n (1+ n) (- n 1) (+ 2 (* 3 n))) 12))
