#lang racket
  (define c0 0)
  (define c1 0)
  (define c2 0)
  (define c3 0)
  (define c4 0)
  (define c5 0)
  (define c6 0)
  (define c7 0)
  (define c8 0)
  (define c9 0)
  (define n 0)
  (define n1 0)
  (define n2 0)
  (define n3 0)
  (define n4 0)
(define (separar)
  (set! n1 (quotient n 1000))
  (set! n2 (-(quotient n 100)(* n1 10)))
  (set! n3 (-(quotient n 10)(+ (* n2 10)(* n1 100))))
  (set! n4 (-(quotient n 1)(+ (* n2 100)(* n1 1000)(* n3 10))))
  )
(define (num1 n)
  (cond
    [(= n 0)(set! c0 (+ c0 1))]
    [(= n 1)(set! c1 (+ c1 1))]
    [(= n 2)(set! c2 (+ c2 1))]
    [(= n 3)(set! c3 (+ c3 1))]
    [(= n 4)(set! c4 (+ c4 1))]
    [(= n 5)(set! c5 (+ c5 1))]
    [(= n 6)(set! c6 (+ c6 1))]
    [(= n 7)(set! c7 (+ c7 1))]
    [(= n 8)(set! c8 (+ c8 1))]
    [(= n 9)(set! c9 (+ c9 1))]
    )
  )
(define (eval)
  (if(and(>= n 1000)(<= n 9999))
     (funPrin)
     (begin(displayln "ERROR")
           )
     )
  )
(define (interfaz)
(displayln "Inserte un número de cuatro cifras")
(set! n (read))
  (eval)
  )
  (define (funPrin)
    (separar)
    (num1 n1)
    (num1 n2)
    (num1 n3)
    (num1 n4)
    (display "La cantidad de ceros es: ")(displayln c0)
    (display "La cantidad de unos es: ")(displayln c1)
    (display "La cantidad de dos es: ")(displayln c2)
    (display "La cantidad de tres es: ")(displayln c3)
    (display "La cantidad de cuatros es: ")(displayln c4)
    (display "La cantidad de cincos es: ")(displayln c5)
    (display "La cantidad de seis es: ")(displayln c6)
    (display "La cantidad de sietes es: ")(displayln c7)
    (display "La cantidad de ochos es: ")(displayln c8)
    (display "La cantidad de nueves es: ")(displayln c9)
    )
  (interfaz)