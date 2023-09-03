#lang racket
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