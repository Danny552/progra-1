#lang racket
(define n (read))
(define (convertidor n)
  (if(= (quotient n 8) 0)
     (begin
       (set! n (remainder n 8))
       (display n)
       (convertidor (quotient n 8))
       )
     (begin
       (set! n (quotient n 8))
       (convertidor n)
       )
     )
  )
(convertidor n)