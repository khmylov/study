#lang racket

; Iterative exponentiation process in a logarithmic number of steps.

(define (square x) (* x x))

(define (fast-expt b n)
  (define (iter a b n)
    (cond ((= n 1) (* a b))
          ((odd? n) (iter (* a b) b (- n 1)))
          (else (iter a (square b) (/ n 2)))))
  
  (if (= n 0) 1
      (iter 1 b n)))