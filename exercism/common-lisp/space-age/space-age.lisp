(defpackage :space-age
  (:use :cl)
  (:export :on-mercury
           :on-venus
           :on-earth
           :on-mars
           :on-jupiter
           :on-saturn
           :on-uranus
           :on-neptune))

(in-package :space-age)

(defconstant +earth-year-in-seconds+ 31557600 "Duration of Earth year in seconds.")

(defun on-planet (orbital-period seconds)
  (/ (/ seconds +earth-year-in-seconds+) orbital-period))

(defun on-mercury (seconds) (on-planet 0.2408467 seconds))
(defun on-venus (seconds) (on-planet 0.61519726 seconds))
(defun on-earth (seconds) (on-planet 1.0 seconds))
(defun on-mars (seconds) (on-planet 1.8808158 seconds))
(defun on-jupiter (seconds) (on-planet 11.862615 seconds))
(defun on-saturn (seconds) (on-planet 29.447498 seconds))
(defun on-uranus (seconds) (on-planet 84.016846 seconds))
(defun on-neptune (seconds) (on-planet 164.79132 seconds))
