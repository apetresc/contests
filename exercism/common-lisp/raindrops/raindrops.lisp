(defpackage :raindrops
  (:use :cl)
  (:export :convert))

(in-package :raindrops)

(defun convert (n)
  "Converts a number to a string of raindrop sounds."
  (let ((result
    (concatenate 'string
      (when (zerop (mod n 3)) "Pling")
      (when (zerop (mod n 5)) "Plang")
      (when (zerop (mod n 7)) "Plong"))))
    (if (string= "" result) (write-to-string n) result)
  ))
