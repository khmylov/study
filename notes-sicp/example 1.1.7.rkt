#lang racket

(define (sqrt1 x)
  (define (average x y)
    (/ (+ x y) 2))
  
  (define (square x) (* x x))
  
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  
  (define (improve guess)
    (average (/ x guess) guess))
  
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  
  (sqrt-iter 1))