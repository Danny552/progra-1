#lang racket

(define (dec-a-oct n)
  (cond ((< n 8) (number->string n))
        (else (string-append (dec-a-oct (quotient n 8)) 
                             (number->string (modulo n 8))))))

; Pedir al usuario el número
(define num (read))

; Convertir a base 8 y mostrar
(displayln (dec-a-oct num))
