#lang racket
(define c 0)
(define (potencia n e)
  (define n1 2)
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
(define e 0)
(define a 2)
(define n (read))
(define (sucesion n)
  (if (= e n)
      (potencia a e)
      (begin
        (potencia a e)
        (display " + ")
        (set! e (+ 1 e))
        (sucesion n)
        )
      )
  )
(sucesion n)