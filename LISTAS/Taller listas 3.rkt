#lang racket
(define (prom list cont suma)
  (if(= cont (length list))
     (begin
       (display "The list's average is: ")
       (display (exact->inexact (/ suma cont)))
       )
     (begin
       (set! suma (+ suma (list-ref list cont)))
       (prom list (+ cont 1) suma)
       )
     )
  )
(display "Insert the list you want to average: ")
(define list (read))
(prom list 0 0)