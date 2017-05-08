#lang racket

(define (cube x) (* x x x))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.0001))

(define (improve y x)
  (/ (+ (/ x (* y y)) (* 2 y)) 3))

(define (cube-root x)
  (define (cube-root-iter guess x)
    (if (good-enough? guess x)
        guess
        (cube-root-iter (improve guess x) x)))
  
  (cube-root-iter 1 x))