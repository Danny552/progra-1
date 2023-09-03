#lang racket
(define (hanoi n source dest temp)
  (cond ((= n 1) (displayln (string-append "Mover disco 1 de " source " a " dest)))
        (else (begin
                (hanoi (- n 1) source temp dest)
                (displayln (string-append "Mover disco " (number->string n) " de " source " a " dest))
                (hanoi (- n 1) temp dest source)))))


(displayln "Solución para n discos:")
(hanoi (read) "A" "C" "B")