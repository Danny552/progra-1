#lang racket
(define (es-primo? n)
  (define (prueba i)
    (cond ((< i 2) #f)
          ((= i n) #t)
          ((zero? (remainder n i)) #f)
          (else (prueba (+ i 1)))))
  (prueba 2))

(define (mostrar-p n)
  (define (prueba-num num primos)
    (cond ((= (length primos) n) primos)
          ((es-primo? num)
           (prueba-num (+ num 1) (cons num primos)))
          (else (prueba-num (+ num 1) primos))))
  (reverse (prueba-num 2 '())))

(define n (read))
(mostrar-p n)