#lang racket
(define (interfaz)
  (display "Digite la cadena de texto a la que desea sacar el recuento de caracteres en cada palabra en un vector: \n")
  (define cadena (read-line))
  (define vector (hacer-vector cadena 0 1))
  (display "La cantidad de caracteres que tiene cada palabra respectivamente es: \n")
  (cuenta-caracteres cadena vector 0 0 0)
)
(define (hacer-vector cadena contador numero-espacios)
  (if(= contador (string-length cadena))
     (make-vector numero-espacios 0)
     (if(char=? (string-ref cadena contador) #\space)
        (hacer-vector cadena (+ contador 1) (+ numero-espacios 1))
        (hacer-vector cadena (+ contador 1) numero-espacios)
        )
     )
  )
(define (cuenta-caracteres cadena vector contador numero-espacios numero-car)
  (if(= contador (string-length cadena))
     (begin
       (vector-set! vector numero-espacios numero-car)
       (display vector)
       )
     (if(char=? (string-ref cadena contador) #\space)
        (begin
          (vector-set! vector numero-espacios numero-car)
          (cuenta-caracteres cadena vector (+ contador 1) (+ numero-espacios 1) 0)
          )
        (cuenta-caracteres cadena vector (+ contador 1) numero-espacios (+ numero-car 1))
        )
     )
)
(interfaz)