(defpackage :lillys-lasagna
  (:use :cl)
  (:export :expected-time-in-oven
           :remaining-minutes-in-oven
           :preparation-time-in-minutes
           :elapsed-time-in-minutes))

(in-package :lillys-lasagna)

(defun expected-time-in-oven () "How many minutes the lasagna should be in the oven" 337)

(defun remaining-minutes-in-oven (actual-time-in-oven)
  "How many minutes the lasagna still has to remain in the oven"
  (- (expected-time-in-oven) actual-time-in-oven))

(defun preparation-time-in-minutes (num-layers)
  "How many minutes spent preparing the lasagna"
  (* 19 num-layers))

(defun elapsed-time-in-minutes (num-layers actual-time-in-oven)
  "How many minutes spent cooking the lasagna in total"
  (+ (preparation-time-in-minutes num-layers) actual-time-in-oven))
