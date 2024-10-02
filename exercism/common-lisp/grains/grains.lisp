(defpackage :grains
  (:use :cl)
  (:export :square :total))
(in-package :grains)

(defun square (n) (expt 2 (- n 1)))

(defun total () (- (expt 2 64) 1))
