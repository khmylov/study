#lang racket

(define (square x) (* x x))
(define (divides? x y) (= (remainder x y) 0))

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? n test-divisor) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  
  (find-divisor n 2))

(define (prime? n) (= (smallest-divisor n) n))

(define (first-prime lower-bound)
  (cond ((even? lower-bound) (first-prime (+ 1 lower-bound)))
        ((prime? lower-bound) lower-bound)
        (else (first-prime (+ 2 lower-bound)))))

(define (find-primes lower-bound count)
  (define (iter n lower-bound)
    (define (handle-prime p)
      (display p) (newline)
      (iter (+ 1 n) (+ 2 p)))
    
    (cond ((< n count) (handle-prime (first-prime lower-bound)))))
  
  (iter 0 lower-bound))
    
  