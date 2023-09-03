#lang racket
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