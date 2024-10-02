(defpackage :perfect-numbers
  (:use :cl)
  (:export :range :primes :classify))

(in-package :perfect-numbers)

(defun range (min max &optional (step 1))
  (when (<= min max)
    (cons min (range (+ min step) max step)))
  )

(defun primes (n &optional (sieve '()))
  "Generates a list of all prime numbers less than or equal to n"
  sieve
  )

(defun classify (n))
