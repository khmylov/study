#lang racket

; Recursive process of multiplying two integers
; in terms of adding, doubling and halving
; in a logarithmic number of steps.

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (mult a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (mult (double a) (halve b)))
        (else (+ a (mult a (- b 1))))))