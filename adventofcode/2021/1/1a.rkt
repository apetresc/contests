#!/usr/bin/env racket
#lang racket

(define (diff-list l1 l2)
  (if (empty? l2)
    '()
    (cons (- (car l2) (car l1)) (diff-list (cdr l1) (cdr l2)))))

(define (rolling-windows l)
  (if (<= (length l) 3)
    (list l)
    (cons (take l 3) (rolling-windows (cdr l)))))
  
(define (sonar-sweep depths)
  (apply + (map (lambda (x) (if (> x 0) 1 0)) (diff-list depths (cdr depths)))))

(define (parse in)
  (let ((line (read-line in 'any)))
    (if (eof-object? line)
      '()
      (cons (string->number line) (parse in)))))

(let ((input (parse (current-input-port))))
  (sonar-sweep input)
  (sonar-sweep (map (lambda (x) (apply + x)) (rolling-windows input))))
