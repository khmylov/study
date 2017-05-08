#lang racket

(define (same-parity n . xs)
  (define (iter acc xs)
    (if (null? xs)
        acc
        (let ((head (car xs)))
          (let ((new-acc 
                 (if (eq? (even? n) (even? head)) 
                     (append acc (list head)) 
                     acc)))
            (iter new-acc (cdr xs))))))
  
  (iter (list n) xs))