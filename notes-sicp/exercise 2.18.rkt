#lang racket

; Reversing a list

; #1 - using delayed procedure chain

(define (reverse-delayed xs)
  (define (iter append-tail tail)
    (if (null? tail) 
        (append-tail null)
        (iter (lambda (ys) (cons (car tail) (append-tail ys))) (cdr tail))))
  
  (iter (lambda (tail) null) xs))

; #2 - using append procedure

(define (reverse-append xs)
  (define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))
  
  (if (null? xs)
      null
      (append (reverse-append (cdr xs)) (list (car xs)))))

; #3 - using accumulated list
(define (reverse xs)
  (define (iter xs acc)
    (if (null? xs)
        acc
        (iter (cdr xs) (cons (car xs) acc))))
  
  (iter xs null))
    
    