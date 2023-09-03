#lang racket
(define (division dividendo divisor acumula)
   (if (<= dividendo 0)
    (display acumula)
    (division (- dividendo divisor) divisor (+ 1 acumula))
    )
  )
(division (read) (read) 0)