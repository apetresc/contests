(defpackage :all-your-base
  (:use :cl)
  (:export :rebase))

(in-package :all-your-base)

(defun from-base (list-digits base)
  (if (null list-digits) 0
    (+
      (* (car list-digits) (expt base (- (length list-digits) 1)))
      (from-base (cdr list-digits) base)
    )))

(defun to-base (n base &optional (acc '()))
  (if (= n 0) (if (null acc) '(0) acc)
    (to-base (floor n base) base (cons (mod n base) acc))))

(defun rebase (list-digits in-base out-base)
  (cond
    ((or (< in-base 2) (< out-base 2)) 'nil)
    ((some #'identity (map 'list (lambda (x) (or (< x 0) (>= x in-base))) list-digits)) 'nil)
    ((null list-digits) '(0))
    ((to-base (from-base list-digits in-base) out-base))))
