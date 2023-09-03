#lang racket
(define (interfaz)
  (define u 0)

  (display "\n\nSeleccione el programa para iniciar\n\n")
  (display " - 1)Diagnóstico equipo.\n")
  (display " - 2)Calculadora de notas\n")
  (display " - 3)Lector de libros.\n")
  (display " - 4)Reproductor de música.\n")
  (display " - 5)Cuántas solucienes tiene la ecuación cuadrática.\n")
  (display " - 6)Randomizador de acciones.\n")

  (set! u (read))

  (cond
    [(= u 1)(ej1)]
    [(= u 2)(ej2)]
    [(= u 3)(ej3)]
    [(= u 4)(ej4)]
    [(= u 5)(ej5)]
    [(= u 6)(ej6)]
    ))
(define(retorno)
    (define u 0)
  (display "\n\n¿Quiere volver a la función anterior?\n")
    (display "1)Si. 2)No.")
  (set! u (read))
  (if(= u 1)
     (interfaz)
     (display "\n\nFin del programa")
     )
  )
;____________________________________________________________________________________
  (define (ej1)
    (define p 0)
    (displayln "DIAGNÓSTICO DE ORDENADOR\n")
    (displayln "Seleccione el problema que tiene: ")
    (displayln "1)El computador no enciende")
    (displayln "2)El computador se bloquea después de 10 minutos")
    (displayln "3)El computador se bloquea cuando abro varias aplicaciones")
    (set! p (read))
    (cond
      [(= p 1)
       (display "Revise conexión")]; si el dato dado es 1
      [(= p 2)
       (display "Vacunar equipo")]; si el dato dado es 2
      [(= p 3)
       (display "Aumentar capacidad de memoria")]; si el dato dado es 3
      (else
       (display "Lo lamento, esta no es una de las opciones"); si ninguna es correcta
       )
      )
(retorno)
    )
;_____________________________________________________________________________________
(define(ej2)
  (define n 0)
  (displayln "CALCULADORA DE NOTAS\n")
  (displayln "Ingrese la nota del estudiante: ")
  (set! n (read))
  (cond
    [(>= n 5)
     (displayln "Excelente")(display (* n 50))]; Notas más altas
    [(and(< n 5)(>= n 3))
     (displayln "Bueno")(display (* n 20))]; nota media
    [(< n 3)
     (displayln "Mal")(display (* n 10))]; notas más bajas
    )
  (retorno)
  )
;_____________________________________________________________________________________
(define(ej3)
  (define l 0)
  (displayln "PROGRAMA PARA LEER LIBROS\n")
  (displayln "Seleccione el libro para leer:")
  (displayln "1)El Capital")
  (displayln "2)El Código Da Vinci")
  (displayln "3)Harry Potter and the Half Blood Prince")
  (displayln "4)Cien años de soledad")
  (displayln "5)La odisea")
  (set! l (read))
  (cond
    [(= l 1)
     (display "Reproducir El Capital")]; si el dato dado es 1
    [(= l 2)
     (display "Reproducir El Código Da Vinci")]; si el dato dado es 2
    [(= l 3)
     (display "Reproducir Harry Potter and the Half Blood Prince")]; si el dato dado es 3
    [(= l 4)
     (display "Reproducir Cien Años de Soledad")]; si el dado dato es 4
    [(= l 5)
     (display "Reproducir La Odisea")]; si el dato dado es 5
    (else
     (display "Lo lamento, esta no es una de las opciones"); si ninguna es correcta
     )
    )
  (retorno)
  )
;_________________________________________________________________________________________
(define(ej4)
  (define c 0)
  (displayln "PROGRAMA PARA REPRODUCIR MÚSICA\n")
  (displayln "Seleccione el género para escuchar:")
  (displayln "1)RAP")
  (displayln "2)HEAVY METAL")
  (displayln "3)ROCK")
  (displayln "4)REGGAETON")
  (displayln "5)SALSA")
  (displayln "6)VALLEANTO")
  (set! c (read))
  (cond
    [(= c 1)
     (display "C:/Musica/RAP")]; si el dato dado es 1
    [(= c 2)
     (display "C:/Musica/HEAVY METAL")]; si el dato dado es 2
    [(= c 3)
     (display "C:/Musica/ROCK")]; si el dato dado es 3
    [(= c 4)
     (display "C:/Musica/REGGAETON")]; si el dado dato es 4
    [(= c 5)
     (display "C:/Musica/SALSA")]; si el dato dado es 5
    [(= c 6)
     (display "C:/Musica/VALLENATO")]; si el dato dado es 6
    (else
     (display "Lo lamento, esta no es una de las opciones"); si ninguna es correcta
     )
    )
  (retorno)
  )
;_______________________________________________________________________________________
(define(ej5)
  (define a 0)
  (define b 0)
  (define c 0)
  (define(cuantos a b c)
    (cond
      [(>(expt b 2)(* 4 a c))
       (display "Dos soluciones")]
      [(= (expt b 2)(* 4 a c))
       (display "Una solución")]
      [(< (expt b 2)(* 4 a c))
       (display "Sin solución")]
      [(= a 0)
       (display "Es una función degenerada, por ende no se consideran soluciones")]
      ))
  (display "PROGRAMA PARA CALCULAR SOLUCIONES DE UNA ECUACIÓN CUADRÁTICA\n\n")
  (display "Inserte a, b y c, teniendo en cuenta la fórmula cuadrática la cual es: ax^2+bx+c\n")
  (set!  a (read))(set!  b (read))(set!  c (read))
  (cuantos a b c)
  (retorno)
  )
;________________________________________________________________________________________________
(define(ej6)
  (define c 0)
(displayln "PROGRAMA PARA REALIZAR ACCIONES\n")
(displayln "OPCIONES POSIBLES:")
(displayln "1)JUEGA RESIDENT EVIL")
(displayln "2)¡PONTE A DORMIR YA!")
(displayln "3)JUEGA WARCRAFT")
(displayln "4)DEDÍCATE SOLO A BAILAR")
(displayln "5)ESCUCHA MÚSICA")
(displayln "6)VE AL CINE")
(displayln "7)COMPRA 10 CERVEZAS Y ÉCHATE A VER FÚTBOL 5 HORAS")
(displayln "LO QUE TIENES QUE HACER AHORA ES: ")
(set! c (random 1 8))
(cond
  [(= c 1)
   (display "JUEGA RESIDENT EVIL")]; si el dato dado es 1
  [(= c 2)
   (display "¡PONTE A DORMIR YA!")]; si el dato dado es 2
  [(= c 3)
   (display "JUEGA WARCRAFT")]; si el dato dado es 3
  [(= c 4)
   (display "DEDÍCATE SOLO A BAILAR")]; si el dado dato es 4
  [(= c 5)
   (display "ESCUCHA MÚSICA")]; si el dato dado es 5
  [(= c 6)
   (display "VE AL CINE")]; si el dato dado es 6
  [(= c 7)
   (display "COMPRA 10 CERVEZAS Y ÉCHATE A VER FÚTBOL 5 HORAS")]; si el dato dado es 7
  (else
   (display "Lo lamento, esta no es una de las opciones"); si ninguna es correcta
   )
  )
  (retorno)
  )
(displayln "Taller 3-Condicionales\n\n\n")
(interfaz)