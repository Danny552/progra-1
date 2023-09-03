#lang racket
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