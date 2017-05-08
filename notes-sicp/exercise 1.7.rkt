#lang racket

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (good-enough? guess previous-guess)
  (< (abs (- guess previous-guess)) (* guess 0.001)))

(define (sqrt1 x)

  (define (improve guess)
    (average (/ x guess) guess))
  
  (define (sqrt-iter guess previous-guess)
    (if (good-enough? guess previous-guess)
        guess
        (sqrt-iter (improve guess) guess)))
  
  (sqrt-iter x (* x 2)))