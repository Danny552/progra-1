#lang racket
(define (saypingo)
(display "Pingo ")
(sleep 0)
(saypingo)
  )
(saypingo)