#lang racket
(define (ingreso-datos lista)
  (display "___________________________________________")
  (displayln "\nInserte los siguientes datos de la persona:")
  (display "Indentificación: ")
  (define identificacion (read))
  (display "Nombre: ")
  (read-line)
  (define nombre (read-line))
  (display "Apellidos: ")
  (define apellidos (read-line))
  (display "Teléfono fijo: ")
  (define tel-f (read))
  (display "Celular: ")
  (define cel (read))
  (display "Edad: ")
  (define edad (read))
  (set! lista (append lista (list(list identificacion nombre apellidos tel-f cel edad))))
  (displayln "\n\n¡Registrado con éxito!")
  (interfaz-prin lista)
  )
;________________________________________________________________________________________________________________________________________________
(define (ingreso-posicion lista)
  (displayln "_____________________________________________")
  (displayln "Inserte los siguientes datos de la persona:")
  (display "Indentificación: ")
  (define identificacion (read))
  (display "Nombre: ")
  (read-line)
  (define nombre (read-line))
  (display "Apellidos: ")
  (define apellidos (read-line))
  (display "Teléfono fijo: ")
  (define tel-f (read))
  (display "Celular: ")
  (define cel (read))
  (display "Edad: ")
  (define edad (read))
  (display "Posición: ")
  (define posicion (- (read) 1))
  (guardar-en-posicion (list(list identificacion nombre apellidos tel-f cel edad)) posicion lista)
  )


 (define (guardar-en-posicion datos posicion lista)
  (if (or (null? lista) (> posicion (length lista)))
      (display "¡La posición indicada está fuera de rango!")
      (set! lista (append (take lista posicion) datos (drop lista posicion))))
   (interfaz-prin lista)
   )
;________________________________________________________________________________________________________________________________________________
(define (pedir-id lista)
  (display "Inserte la identificación que desee buscar: ")
  (define id (read))
  (buscar-id lista id 0)
  )


(define (buscar-id lista id contador)
  (if (or (>= contador (length lista)) (null? lista))
      (begin
        (displayln "Datos no encontrados")
        (interfaz-prin lista)
        )
  (if (= id (list-ref (list-ref lista contador) 0))
      (begin
        (display "Los datos encontrados fueron: ")
        (display (list-ref (list-ref lista contador) 1))
        (display " ")
        (display (list-ref (list-ref lista contador) 2))
        (display " ")
        (display (list-ref (list-ref lista contador) 3))
        (display " ")
        (display (list-ref (list-ref lista contador) 4))
        (newline)
        (interfaz-prin lista)
        )
      (buscar-id lista id (+ contador 1))
      )
  )
  )
;________________________________________________________________________________________________________________________________________________
(define (pedir-tel lista) ;pide cualquiera de los dos teléfonos
  (display "Inserte el número de teléfono que desee buscar, celular o fijo: ")
  (define tel (read))
  (buscar-tel lista tel 0)
  )


(define (buscar-tel lista tel contador) ;Busca si el valor dado está en cualquiera de las dos posiciones
  (if (or (> contador (length lista)) (null? lista))
      (begin
        (displayln "Datos no encontrados")
        (interfaz-prin lista)
        )
  (if (or (= tel (list-ref (list-ref lista contador) 3)) (= tel (list-ref (list-ref lista contador) 4)))
      (begin
        (display "Los datos encontrados fueron: ")
        (display (list-ref (list-ref lista contador) 0))
        (display " ")
        (display (list-ref (list-ref lista contador) 1))
        (display " ")
        (display (list-ref (list-ref lista contador) 2))
        (newline)
        (interfaz-prin lista)
        )
      (buscar-tel lista tel (+ contador 1))
      )
  )
  )
;________________________________________________________________________________________________________________________________________________
(define (insertar-rango lista) ;Le pide el rango al usuario
  (displayln "Inserte desde qué edad desea buscar: ")
  (define base (read))
  (display "Inserte hasta qué límite de edad desea buscar: ")
  (define lim (read))
  (if (> base lim)
      (begin
        (display "El límite es menor que la base de la busqueda, error")
        (interfaz-prin lista)
        )
      (filtro-rango lista base lim 0)
      )
  )


(define (filtro-rango lista base lim contador); Filtra los valores en el rango
  (if (>= contador (length lista))
      (interfaz-prin lista)
      (if (and (> (list-ref (list-ref lista contador) 5) base)(< (list-ref (list-ref lista contador) 5) lim))
          (begin
            (display (list-ref (list-ref lista contador) 0))
            (display " ")
            (display (list-ref (list-ref lista contador) 1))
            (display " ")
            (display (list-ref (list-ref lista contador) 2))
            (newline)
            (filtro-rango lista base lim (+ contador 1))
            )
          (filtro-rango lista base lim (+ contador 1))
          )
      )
  )
;________________________________________________________________________________________________________________________________________________
(define (eliminar-posicion lista) ;Va directamente a la posición
  (define opc 0)
  (display "Inserte la posición que desee eliminar: ")
  (define posicion (- (read) 1) )
  (if (or (null? lista) (>= posicion (length lista)))
      (begin
        (displayln "La posición no existe en la lista")
        (interfaz-prin lista)
        )
      (begin
        (displayln "Estos son los datos que va a eliminar: ")
        (displayln (list-ref lista posicion))
        (displayln "¿Desea continuar?")
        (displayln "1) Si \n2) No")
        (set! opc (read))
        (if (= opc 1)
            (begin
              (if (= posicion 0)
                  (begin
                    (set! lista (cdr lista))
                    (display "Posición eliminada con éxito")
                    (interfaz-prin lista)
                    )
                  (begin
                    (set! lista (append (take lista  posicion)(drop lista (+ posicion 1))))
                    (display "Posición eliminada con éxito")
                    (interfaz-prin lista)
                    )
                  )
              )
            (if(= opc 2)
               (begin
                 (displayln "Volviendo al menú principal")
                 (interfaz-prin lista)
                 )
               (begin
                 (display "La opción no es válida, volviendo al menú principal...")
                 (interfaz-prin lista)
                 )
               )
            )
        )
      )
  )
;________________________________________________________________________________________________________________________________________________
(define (pedir-borrar-id lista) ;Le pide id al usuario
  (display "Inserte la identificación que desee eliminar: ")
  (define id (read))
  (borrar-id lista id 0)
  )


(define (borrar-id lista id contador) ;Avanza hasta encontrar la id
  (define opc 0)
  (if (or (>= contador (length lista)) (null? lista))
      (begin
        (displayln "Datos no encontrados")
        (interfaz-prin lista)
        )
  (if (= id (list-ref (list-ref lista contador) 0))
      (begin
        (displayln "Los datos encontrados fueron: ")
        (displayln (list-ref lista contador))
        (displayln "¿Desea eliminarlos?")
        (displayln "1) Si  \n2) No")
        (set! opc (read))
        (if (= opc 1)
            (begin
              (set! lista (append (take lista contador)(drop lista (+ contador 1))))
              (display "Datos eliminados con éxito")
              (interfaz-prin lista)
              )
            (if (= opc 2)
                (begin
                  (display "Voliviendo al menú anterior")
                  (interfaz-prin lista)
                )
                (begin
                  (display "La opción no está en la lista")
                  (interfaz-prin lista)
                  )
                )
            )
        )
      (borrar-id lista id (+ contador 1))
      )
  )
  )
;________________________________________________________________________________________________________________________________________________
(define (mostrar-cosas lista) ;Mostrar parte superior de la tabla
  (displayln "________________________________________________________________________")
  (displayln "| Identificación | Nombre | Apellido | Tel. Fijo | Tel. Celular | Edad |")
  (mostrar lista 0)
  )


(define (mostrar lista contador) ;Mostrar elementos de la tabla
  (displayln "________________________________________________________________________")
  (if (= contador (length lista))
      (interfaz-prin lista)
      (begin
        (display (list-ref (list-ref lista contador) 0))
        (display " | ")
        (display (list-ref (list-ref lista contador) 1))
        (display " | ")
        (display (list-ref (list-ref lista contador) 2))
        (display " | ")
        (display (list-ref (list-ref lista contador) 3))
        (display " | ")
        (display (list-ref (list-ref lista contador) 4))
        (display " | ")
        (displayln (list-ref (list-ref lista contador) 5))
        (mostrar lista (+ contador 1))
        )
      )
  )
;________________________________________________________________________________________________________________________________________________
;________________________________________________________________________________________________________________________________________________
(define (interfaz-prin lista); Función principal
  (display "Lista de listas \n\n")
  (displayln "¿Qué desea realizar?\n")
  (displayln "1) Ingresar los datos de una persona")
  (displayln "2) Ingresar datos en una posición especificada")
  (displayln "3) Buscar por identificación")
  (displayln "4) Buscar por número de teléfono")
  (displayln "5) Filtrar por rango de edad")
  (displayln "6) Elliminar datos por posición")
  (displayln "7) Eliminar datos por identificación")
  (displayln "8) Mostrar lista completa")
  (displayln "9) Salir")
  (define opc (read))
  (cond
    [(= opc 1)(ingreso-datos lista)]
    [(= opc 2)(ingreso-posicion lista)]
    [(= opc 3)(pedir-id lista)]
    [(= opc 4)(pedir-tel lista)]
    [(= opc 5)(insertar-rango lista)]
    [(= opc 6)(eliminar-posicion lista)]
    [(= opc 7)(pedir-borrar-id lista)]
    [(= opc 8)(mostrar-cosas lista)]
    [(= opc 9)(display "¡Gracias por usar el programa!")]
    (else (display "Esa no es una opción, vuelva a intentarlo\n\n")
          (interfaz-prin lista))
    )
  )
;________________________________________________________________________________________________________________________________________________
(interfaz-prin '()) ; Inicio de función principal