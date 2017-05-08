#lang racket

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (define (iter a b c current-step)
    (if (= current-step n)
        c
        (iter b c (+ c (* 2 b) (* 3 a)) (+ current-step 1))))
  
  (if (< n 3)
      n
      (iter 0 1 2 2)))