#lang racket

(define (triangle row column)
  (cond ((< row 3) 1)
        ((or (= column 1) (= column row)) 1)
        (else (+ (triangle (- row 1) (- column 1)) (triangle (- row 1) column)))))