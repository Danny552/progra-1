#lang racket
(define (gone)
  (define x 0)

  (display "Programa que muestra los primeros n números primos \n")
  (newline)

  (display "Ingrese n: \n")
  (set! x (read))
  (newline)

  (imprimo (- x (- x 1)) 0 x)

  )

(define (primo n divi)
  [cond
    ((= n 1) #f) 
    ((= divi 1) #t) 
    ((= (remainder n divi) 0) #f) 
    (else (primo n (- divi 1)))
                   ]
                  )

(define (imprimo n cont b)
  [cond
    ((= cont b) (displayln "Fin")) 
    ((primo n (- n 1)) 
     {begin
       (printf "~a, " n)
       (imprimo (+ n 1) (+ cont 1) b)
                          }
                         )
    (else (imprimo (+ n 1) cont b)) 
                                ]
                               )
 
(gone)