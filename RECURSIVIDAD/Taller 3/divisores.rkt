#lang racket
(define c 1)
(define (divisores n c)
  (if (= n 0)
      (display 0)
      (if (= c n)
          (display n)
          (if( integer? (/ n c))
             (begin
               (display c)
               (display ", ")
               (set! c (+ c 1))
               (divisores n c)
               )
             (begin
               (set! c (+ c 1))
               (divisores n c)
               )
             )
          )
      )
  )
(define (interfaz)
  (displayln "Programa para calcular los divisores de un número:")
  (display "Inserte un número: ")
  (define n (read))
  (display "Los divisores son: ")
  (divisores n c)
  )
(interfaz)