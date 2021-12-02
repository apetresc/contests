#!/usr/bin/env racket
#lang racket

(struct posn (x y) #:transparent)
(define (posn-add p1 p2)
  (posn (+ (posn-x p1) (posn-x p2))
        (+ (posn-y p1) (posn-y p2))))

(define (parse-line l)
  (cond
    ((string-prefix? l "forward ")
     (posn (string->number (substring l 8)) 0))
    ((string-prefix? l "down ")
     (posn 0 (string->number (substring l 5))))
    ((string-prefix? l "up ")
     (posn 0 (- (string->number (substring l 3)))))))

(define (parse in (pos (posn 0 0)))
  (let ((line (read-line in 'any)))
    (if (eof-object? line)
      (posn 0 0)
      (posn-add (parse-line line) (parse in pos)))))

(parse (current-input-port))
