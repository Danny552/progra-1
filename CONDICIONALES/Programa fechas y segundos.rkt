#lang racket
(define (programa1)
  (define(anioNuevo)
    (set! d 1)(set! m 1)(set! a (+ a 1)); por si hay año nuevo
    )
  (define (mesNuevo); por el cambio de mes
    (set! d 1)
    (set! m (+ m 1))
    )
  (define d 0)
  (define m 0) 
  (define a 0)
  (define b 0)
  (display "Inserte el día: ")
  (set! d (read))
  (display "Inserte el mes: ")
  (set! m (read))
  (display "Inserte el año: ")
  (set! a (read))
  (if(and(=(remainder a 4)0)(or(not(=(remainder a 100)0))(=(remainder a 400)0))); fórmula año biciesto
     (set! b 1)
     (set! b 0)
     )
  (cond
    [(and(= m 2)(= b 0)(= d 28)); febrero no biciesto
     (mesNuevo)]
    [(and(= m 2)(= b 1)(= d 29)); febrero biciesto
     (mesNuevo)]
    [(and(or(= m 1)(= m 3)(= m 5)(= m 7)(= m 8)(= m 10))(= d 31)); meses de 31
     (mesNuevo)]
    [(and(or(= m 4)(= m 6)(= m 9)(= m 11))(= d 30)); meses de 30
     (mesNuevo)]
    [(and(= m 12)(= d 31)); diciembre
     (anioNuevo)]
    [(and(<= d 31)(>= d 1)(<= m 12)(>= m 1)(>= a 0)) (set! d (+ d 1))]
    )
  (display "Mañana será: ")(display d)(display "/")(display m)(display "/")(display a); display de la fecha, (sigo mirando para colocar un error si algo no es válido)
  )
(define(programa2)
  (define s 0)
  (define m 0)
  (define h 0)
  (display "Inserte las horas (en formato 24 horas): ")
  (set! h (read))
  (display "Inserte los minutos: ")
  (set! m (read))
  (display "Inserte los segundos: ")
  (set! s (read))
  (cond
    [(and(= h 23)(= m 59)(= s 59)); si se cumple un ciclo completo
     (set! h 0)(set! m 0)(set! s 0)]
    [(and(= m 59)(= s 59)); cuando se completa una hora
     (set! h (+ h 1))(set! s 0)(set! m 0)]
    [(= s 59); cuando se completa un minuto
     (set! m (+ m 1))(set! s 0)]
    (else(set! s (+ s 1))); si el segundo no determina otras cosas
    )
  (display "La hora es: ")(display h)(display":")(display m)(display ":")(display s)
  )
  (define e 0)
(displayln "Seleccione el programa para correr:")
(displayln "Para el programa que calcula una fecha a partir de otra ingrese 1")
(displayln "Para el programa que calcula la hora en el siguiente segundo ingrese 2")
(set! e (read))
(cond
  [(= e 1)(programa1)]
  [(= e 2)(programa2)]
  (else(display "¡no es una opción válida!"))
  )