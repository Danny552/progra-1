#lang racket
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