(defpackage :pizza-pi
  (:use :cl)
  (:export :dough-calculator :pizzas-per-cube
           :size-from-sauce :fair-share-p))

(in-package :pizza-pi)

(defun dough-calculator (pizzas diameter)
  (round (* pizzas
    (+
      200
      (/ (* 45 pi diameter) 20))
  )))

(defun size-from-sauce (sauce)
  (sqrt (/
    (* 40 sauce)
    (* 3 pi))))

(defun pizzas-per-cube (cube-size diameter)
  (floor (/
    (* 2 cube-size cube-size cube-size)
    (* 3 pi diameter diameter))))

(defun fair-share-p (pizzas friends)
  (= 0 (mod (* 8 pizzas) friends)))
