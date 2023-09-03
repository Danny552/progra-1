#lang racket
(define (imprimir-no-primos)
  (for ([i (in-range 1 101)]
        #:unless (es-primo? i))
    (display i)
    (display " ")))

(define (es-primo? n)
  (or (= n 2)
      (and (> n 2)
           (not (even? n))
           (let loop ((i 3))
             (cond ((> (* i i) n) #t)
                   ((zero? (modulo n i)) #f)
                   (else (loop (+ i 2))))))))

(imprimir-no-primos)
