#lang racket
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