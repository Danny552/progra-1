#lang racket
(define n (read))
(define n1 n)
(define e (read))
(define c 1)
(define (potencia n c)
  (cond
    [(= e 0)(display 1)]
    [(= e 1)(display n)]
    (else
       (if(= c e)
          (display n)
          (begin
            (set! n (* n n1))
            (set! c (+ c 1))
            (potencia n c)
            )
          )
       )
     )
  )
(potencia n c)