#lang racket
(define (multi2 lista contador lista-final)
  (if(= (length lista) contador)
     (begin
       (display "The list result is: ")
       (display lista-final)
       )
     (if(integer? (/ contador 2))
        (begin
          (set! lista-final (append lista-final (list (* (list-ref lista contador) 2))))
          (multi2 lista (+ contador 1) lista-final)
          )
        (begin
          (set! lista-final (append lista-final (list (list-ref lista contador))))
          (multi2 lista (+ contador 1) lista-final)
          )
        )
     )
  )
(display "Insert the list you want to double uneven positions: ")
(define lista (read))
(multi2 lista 0 '())