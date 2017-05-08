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

; Fermat's Little Theorem
; a^n is congruent to a modulo n, when n is a prime number, and 0 < a < n,
; which effectively means that (remainder (expt a n) n) equals a;
; The following code implements so called "Fermat test"

; We could use the usual (remainder (expt x y) y) function here,
; but for the studying purposes we are going to implement the logarithmic process similar to fast-exp.
(define (expmod base exp m)
  (cond ((= 0 exp) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test? n)
  (define (iter a)
    (= a (expmod a n n)))
  (iter (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= 0 times) true)
        ((fermat-test? n) (fast-prime? n (- times 1)))
        (else false)))