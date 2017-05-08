#lang racket

; Iterative process of multiplying two integers 
; in terms of adding, doubling and halving
; in a logarithmic number of steps.

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (mult a b)
  (define (iter s a b)
    (cond ((= b 1) (+ s a))
          ((even? b) (iter s (double a) (halve b)))
          (else (iter (+ s a) a (- b 1)))))
  
  (iter 0 a b))