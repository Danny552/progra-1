#lang racket
(define(retorno)
    (define u 0)
  (display "\n\n¿Quiere volver a la función anterior?\n")
    (display "1)Si. 2)No.")
  (set! u (read))
  (if(= u 1)
     (interfaz-principal)
     (display "\n\nFin del programa")
     )
  )
(define (interfaz-principal)
  (define ej 0)
  (display "Bienvenido al ejercicio 3 del tema de recursividad: elija el punto que desee observar: \n\n")
  (display "1) Base octadecimal\n2)Primera serie\n3)Divisores\n4)Primeros N primos\n5)Números del 1 al 100 no primos\n6)Potencia\n7)Secuencia 2\n8)Pirámide inversa\n9)Moño\n10)Factorial\n11)Multiplicación\n12)División\n13)Cantidad de cifras de N\n")
  (set! ej (read))
  (cond
    [(= ej 1)(ej1)]
    [(= ej 2)(ej2)]
    [(= ej 3)(ej3)]
    [(= ej 4)(ej4)]
    [(= ej 5)(ej5)]
    [(= ej 6)(ej6)]
    [(= ej 7)(ej7)]
    [(= ej 8)(ej8)]
    [(= ej 9)(ej9)]
    [(= ej 10)(ej10)]
    [(= ej 11)(ej11)]
    [(= ej 12)(ej12)]
    [(= ej 13)(ej13)]
    (else (display "ERROR")
          )
    )
  )
;_________________________________________________________________________________________________________________________________________________________________________________________
(define (ej1)
  (define (dec-a-oct n)
    (cond ((< n 8) (number->string n))
          (else (string-append (dec-a-oct (quotient n 8)) 
                               (number->string (modulo n 8))))))
  
  (display "Inserte el número en base 10")
  (define num (read))
  
  ; Convertir a base 8 y mostrar
  (displayln (dec-a-oct num))
  (retorno)
  )
;________________________________________________________________________________________________________________________________________________________________________________________
(define (ej2)
  (define num 1)
(define c 0)
(define den 0)
(displayln "Inserte x en la sucesión: (1/x) – (3/(x-2)) – (5/(x-4)) – (7/(x-6)) – (9/(x-8)) - (11/(x-10))...")
(define x (read))
(display "Inserte la cantidad de repeticiones de la sucesión que desee (tenga en cuenta que el primer valor es 0): ")
(define n (read))
(define (secuenc num den x n c)
  (if(= n c)
     (display (/ num (- x den)))
     (begin
       (display (/ num (- x den)))
       (display ", ")
       (set! num (+ num 2))
       (set! den (+ den 2))
       (set! c (+ c 1))
       (secuenc num den x n c)
       )
     )
  )
(secuenc num den x n c)
  (retorno)
  )
;_________________________________________________________________________________________________________________________________________________________________________________________
(define (ej3)
  (define c 1)
(define (divisores n c)
  (if (= n 0)
      (display 0)
      (if (= c n)
          (display n)
          (if( integer? (/ n c))
             (begin
               (display c)
               (display ", ")
               (set! c (+ c 1))
               (divisores n c)
               )
             (begin
               (set! c (+ c 1))
               (divisores n c)
               )
             )
          )
      )
  )
(define (interfaz)
  (displayln "Programa para calcular los divisores de un número:")
  (display "Inserte un número: ")
  (define n (read))
  (display "Los divisores son: ")
  (divisores n c)
  )
  (interfaz)
  (retorno)
  )
;_______________________________________________________________________________________________________________________________________________________________________________________
(define (ej4)
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
    (reverse (prueba-num 2 '()))
    )
  (display "Inserte la cantidad de n primos que desee ver: ")
  (define n (read))
  (mostrar-p n)
  (retorno)
  )
;________________________________________________________________________________________________________________________________________________________________________________________
(define (ej5)
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
                   (else (loop (+ i 2)))
                   )
             )
           )
      )
  )
  (display "los primeros 100 números no primos son: ")
(imprimir-no-primos)
  (retorno)
)
;_________________________________________________________________________________________________________________________________________________________________________________________
(define (ej6)
  (display "Inserte la base: ")
  (define n (read))
(define n1 n)
  (display "Inserte el exponente: ")
(define e (read))
(define c 1)
(define (potencia n c)
  (cond
    [(= e 0)(display 1)]
    [(= e 1)(display n)]
    (else
       (if(= c e)
          (display n)
          (begin
            (set! n (* n n1))
            (set! c (+ c 1))
            (potencia n c)
            )
          )
       )
     )
  )
  (potencia n c)
  (retorno)
  )
;_______________________________________________________________________________________________________________________________________________________________________________________
(define (ej7)
(define c 0)
(define (potencia n e)
  (define n1 2)
  (cond
    [(= e 0)(display 1)]
    [(= e 1)(display n)]
    (else
       (if(= c e)
          (display n)
          (begin
            (set! n (* n n1))
            (set! c (+ c 1))
            (potencia n c)
            )
          )
       )
     )
  )
(define e 0)
(define a 2)
  (display "Inserte la cantidad de sucesiones: ")
(define n (read))
(define (sucesion n)
  (if (= e n)
      (potencia a e)
      (begin
        (potencia a e)
        (display " + ")
        (set! e (+ 1 e))
        (sucesion n)
        )
      )
  )
(sucesion n) ; No está del todo correcto, no entiendo por qué siempre n me vale 2
  (retorno)
  )
;________________________________________________________________________________________________________________________________________________________________________________________
(define (ej8)
  (define (ImprP cantidadP)
  (if (= cantidadP 0)
      (void)
      (begin
        (printf "P ")
        (ImprP (- cantidadP 1))
        )
      )
  )

;Imprimir espacio
(define (ImprSpace cantidadSpace)
  (if (= 0 cantidadSpace)
      (void)
      (begin
        (printf " ")
        (ImprSpace (- cantidadSpace 1))
        )
      )
  )

;Función principal
  (display "Inserte cantidad de P en la primera fila: ")
  (define filasE (read))

(define (PiramideInversa filas)
  (if (= filas 0)
      (void)
      (begin
        (ImprSpace (- filasE (- filas 1)))
        (ImprP filas)
        (newline)
        (PiramideInversa (- filas 1))
        )
      )
  )

(PiramideInversa filasE)
  (retorno)
  )
;_________________________________________________________________________________________________________________________________________________________________________________________
(define (ej9)
  ;Imprimir P
(define (ImprP cantidadP)
  (if (= cantidadP 0)
      (void)
      (begin
        (printf "P ")
        (ImprP (- cantidadP 1))
        )
      )
  )

;Imprimir espacio
(define (ImprSpace cantidadSpace)
  (if (= 0 cantidadSpace)
      (void)
      (begin
        (printf " ")
        (ImprSpace (- cantidadSpace 1))
        )
      )
  )

;Función Auxiliar Inversa
  (display "Inserte la cantidad de P en la primera y última fila: ")
(define filasE (read))

(define (PiramideInversa filas)
  (if (= filas 0)
      (void)
      (begin
        (ImprSpace (- filasE (- filas 1)))
        (ImprP filas)
        (newline)
        (PiramideInversa (- filas 1))
        )
      )
  )

;Función Auxiliar Derecha

(define (PiramideNormal filas)
  (if (= filas 0)
      (void)
      (begin
        (ImprSpace filas)
        (ImprP (- filasE (- filas 1)))
        (newline)
        (PiramideNormal (- filas 1))
        )
      )
  )

;función principal

(define (MoñoCompleto filas)
  (if (= filas 0)
      (void)
      (begin
        (PiramideInversa filas)
        (PiramideNormal filas)
        )
      )
  )

(MoñoCompleto filasE)
  (retorno)
  )
;_________________________________________________________________________________________________________________________________________________________________________________________
(define (ej10)
  (define (factor n)
  (cond
    [(= n 0) 1]
    [(= n 1) 1]
    [else
     (* n (factor (- n 1)))]
))
(display "Inserte el número al que desea sacar factorial: ")
(factor(read))
  (retorno)
  )
;________________________________________________________________________________________________________________________________________________________________________________________
(define (ej11)
  (define (Sumar num1 num2 numMul)
  (if (= num2 1)
      (void)
      (begin
        (printf "\n~a" (+ num1 numMul))
        (Sumar (+ num1 numMul) (- num2 1) numMul)
        )
      )
  )
(printf "los numeros que ingrese se multiplicarán\n")
(printf "\nPor favor ingrese un numero: ")
(define num1 (read))
(printf "\nPor favor ingrese otro numero: ")
(define num2 (read))
(define numMul num1)
(Sumar num1 num2 numMul)
  (retorno)
  )
;_________________________________________________________________________________________________________________________________________________________________________________________
(define (ej12)
  (define (division dividendo divisor acumula)
   (if (<= dividendo 0)
    (display acumula)
    (division (- dividendo divisor) divisor (+ 1 acumula))
    )
  )
  (display "Inserte en dividendo y luego el divisor: ")
(division (read) (read) 0)
  (retorno)
  )
;____________________________________________________________________________________________________________________________________________________________________________________
(define (ej13)
  (define (num-cifras-rec n)
  (if (< n 10)
      1
      (+ 1 (num-cifras-rec (quotient n 10)))
      )
    )
  (display "Inserte el número al cual quiera saber el número de cifras: ")
(display(num-cifras-rec (read)))
  (retorno)
  )
;_____________________________________________________________________________________________________________________________________________________________________________________
(interfaz-principal)