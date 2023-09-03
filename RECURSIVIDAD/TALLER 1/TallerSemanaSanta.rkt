#lang racket
; tres puntos en un programa
(define (retorno)
  (printf "¿Quiere volver al menú anterior?\n1) Sí, 2) No\n")
  (define volver (read))
  (cond
    [(= volver 1)(interfaz-principal)]
    [(= volver 2)(printf "¡Gracias por ver!")]
    (else
     (printf "Esa no era una opción pero igual terminaré el programa")
     )
    )
  )
(define (interfaz-principal)
  (printf "Bienvenid@ al taller de semana santa\nPor favor elija el punto del taller que desee ver:\n1) Torres de Hanoi\n2) Acertijo Monty Hall\n3) Conejos de fibonacci\n")
  (define ej (read))
  (cond
    [(= ej 1)(ej1)]
    [(= ej 2)(ej2)]
    [(= ej 3)(ej3)]
    (else
     (printf "Esa no es una opción\n")
     (retorno)
     )
    )
  )
;_________________________________________________________________________________________________________________________________________________________________________________________
(define (ej1)
  (define (hanoi n A C puente)
    (cond ((= n 1) (displayln (string-append "Mover disco 1 de " A " a " C)))
        (else (begin
                (hanoi (- n 1) A puente C)
                (displayln (string-append "Mover disco " (number->string n) " de " A " a " C))
                (hanoi (- n 1) puente C A)))))
  
  
  (displayln "Solución para n discos:")
  (hanoi (read) "A" "C" "B")
  (retorno)
  )
;_________________________________________________________________________________________________________________________________________________________________________________________
(define (ej2)
  (define (monty-hall opc)
    (define puertas '(1 2 3))
    (define prem (random 1 4))
    (define monty-opc
      (let ((cabras (remove prem puertas))
            (opc-no-elegida (remove opc puertas)))
        (remove* cabras opc-no-elegida)))
    (define nueva-opc
      (let ((opc-no-elegida (remove monty-opc puertas)))
        (car (remove opc opc-no-elegida))))
    (display "Cambiaste a la opción: ")(displayln nueva-opc)
    (display "El premio estaba en la: ")(displayln prem)
    (if (= prem nueva-opc)
        (displayln "¡Felicidades te lo ganaste!")
        (displayln "Lástima, será la próxima")
        )
    (retorno)
    )
  (define(interfaz)
    (display "Elige la puerta: 1, 2 o 3: ")
    (define opc(read))
    (if (and (>= opc 1)(<= opc 3))
        (monty-hall opc)
        (display "Esa no es una puerta válida")
        )
    )
  (interfaz)
  (retorno)
  )
; en este programa, siempre se cambia la puerta elgida, sin importar
;_______________________________________________________________________________________________________________________________________________________________________________________
(define (ej3)
  (printf "Porfavor seleccione el numero de meses: \n")
  (define meses (read))
  (define (fibonacci a b contador)
    (if (= contador meses)
        (printf "~a \n" a)
        (fibonacci b (+ a b) (+ 1 contador))
        )
    )
  (cond
    [(= meses 0)(display "0")]
    [(= meses 1)(display "0")]
    )
  (fibonacci 1 1 1)
  (retorno)
  )
(interfaz-principal)