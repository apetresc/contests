(defpackage :leap
  (:use :cl)
  (:export :leap-year-p))
(in-package :leap)

(defun leap-year-p (year)
  (cond
    ((zerop (mod year 400)) T)
    ((zerop (mod year 100)) NIL)
    ((zerop (mod year 4)) T)
    (:otherwise NIL)))
