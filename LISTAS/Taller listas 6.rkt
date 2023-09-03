#lang racket

(define (ordenar-mayor-a-menor lista)
  (cond
    ((null? lista) '())
    (else
     (cons (maximo lista 0 (length lista)) (ordenar-mayor-a-menor (remover-maximo lista (maximo lista 0 (length lista)))))
     )
    )
  )

(define (maximo lista indice longitud)
  (cond
    ((= indice (- longitud 1)) (list-ref lista indice))
    ((> (list-ref lista indice) (maximo lista (+ indice 1) longitud)) (list-ref lista indice))
    (else (maximo lista (+ indice 1) longitud))
    )
  )

(define (remover-maximo lista maximo)
  (cond
    ((null? lista) '())
    ((equal? (list-ref lista 0) maximo) (rest lista))
    (else (cons (list-ref lista 0) (remover-maximo (rest lista) maximo)))
    )
  )

(define (guardar-digitos numero)
  (define (guardar-digitos-aux numero digitos)
    (if (zero? numero)
        digitos
        (guardar-digitos-aux (quotient numero 10)
                            (cons (remainder numero 10) digitos))))
  
  (guardar-digitos-aux numero '())
  )

(display "Insert the number you want to make a list of and organize it: ")
(define numero (read))
(define digitos (guardar-digitos numero))
(display "The ordenated number an list is: ")
(display (ordenar-mayor-a-menor digitos))
