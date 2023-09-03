#lang racket
(define (contar-caracteres frase)
  (define caracteres (make-vector (string-length frase) #f))
  (define repeticiones (make-vector (string-length frase) 0))
  (define num-caracteres 0)
  
  ; Función recursiva para buscar un carácter en el vector de caracteres
  (define (buscar-caracter caracter indice)
    (cond
      [(>= indice num-caracteres) #f]
      [(char=? caracter (vector-ref caracteres indice)) #t]
      [else (buscar-caracter caracter (+ indice 1))]))

  ; Función recursiva para actualizar el vector de repeticiones
  (define (actualizar-repeticiones caracter indice)
    (cond
      [(>= indice num-caracteres) #f]
      [(char=? caracter (vector-ref caracteres indice))
       (begin
         (vector-set! repeticiones indice (+ (vector-ref repeticiones indice) 1))
         #t)]
      [else (actualizar-repeticiones caracter (+ indice 1))]))

  ; Función recursiva para recorrer los caracteres de la frase
  (define (recorrer-caracteres indice)
    (cond
      [(>= indice (string-length frase)) #f]
      [else
       (define caracter (string-ref frase indice))
       (define encontrado? (buscar-caracter caracter 0))
       (if (not encontrado?)
           (begin
             (vector-set! caracteres num-caracteres caracter)
             (vector-set! repeticiones num-caracteres 1)
             (set! num-caracteres (+ num-caracteres 1)))
           (actualizar-repeticiones caracter 0))
       (recorrer-caracteres (+ indice 1))]))

  ; Llamada inicial a la función recursiva
  (recorrer-caracteres 0)

  ; Imprimir el vector de caracteres
  (displayln "Vector de caracteres:")
  (define (imprimir-caracteres indice)
    (cond
      [(>= indice num-caracteres) #f]
      [else
       (display (vector-ref caracteres indice))
       (imprimir-caracteres (+ indice 1))]))
  (imprimir-caracteres 0)
  (newline)

  ; Imprimir el vector de repeticiones
  (displayln "Vector de número de repeticiones:")
  (define (imprimir-repeticiones indice)
    (cond
      [(>= indice num-caracteres) #f]
      [else
       (display (vector-ref repeticiones indice))
       (imprimir-repeticiones (+ indice 1))]))
  (imprimir-repeticiones 0)
  (newline))

; Solicitar frase al usuario
(display "Ingresa una frase: ")
(define frase (read-line))

; Llamar a la función para contar los caracteres
(contar-caracteres frase)
