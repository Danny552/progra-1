#lang racket
(define (num-cifras-rec n)
  (if (< n 10)
      1
      (+ 1 (num-cifras-rec (quotient n 10)))))

(num-cifras-rec (read))