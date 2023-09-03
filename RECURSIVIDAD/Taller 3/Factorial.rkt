#lang racket
(define (factor n)
  (cond
    [(= n 0) 1]
    [(= n 1) 1]
    [else
     (* n (factor (- n 1)))]
))

(factor(read))