#lang racket
  (define (creav x b) ; este apartado son funciones ya trabajadas en clase en las cuales se realiza un proceso recursivo para crear un vector con otros vectorees
  (define m (make-vector x 0))
  (cream m 0 b)
  )
(define (cream v p b); es la primera parte de la creación del vector
  (if (< p (vector-length v))
      (begin
        (vector-set! v p (make-vector b 0))
        (cream v (+ p 1) b)
      )
      (void)
   )
 )
(define (inicio)
  (display "Inserte la cantidad de productos que desee registrar: ")
  (define x (read))
  (define base-de-datos (make-vector x 0))
  (cream base-de-datos 0 8) ; se tiene el 8 ya que todos los "semivectores" de la red o matriz tienen 8 elementos y estos no pueden cambiarse.
  (define numero-producto 0)
  (interfaz base-de-datos numero-producto)
  )
(define (interfaz base numero-producto) ;la interfaz general de todo, la cual se va a llamar a si misma múltiples veces durante la ejecución del programa.
  (displayln "\nBienvenido a la base de datos de nuestra librería ")
  (newline)
  (displayln "Indique lo que desee realizar")
  (display "1) Registrar un producto\n2) Actualizar datos de un libro existente (requiere la identificación del producto)\n")
  (display "3) Lista de datos de libros de dicho tipo \n4) Lista de libros de precio mayor al que indique\n5) Lista general de los libros registrados\n0) Salir\n")
  (define opción (read))
  (condicional-opciones opción base numero-producto) ; Añadí esta función la cual es un condicional para la selección de opción
  )
  (define (condicional-opciones opción base numero-producto); aquí está el condicional que mencioné antes
  (cond
    [(= opción 1)(registrar base numero-producto)]
    [(= opción 2)(display "Ingrese el código del libro que desea modificar: ")(actualizar-datos base (read) 0 numero-producto)]
    [(= opción 3)(lista-tipos base numero-producto)]
    [(= opción 4)(lista-precios base numero-producto)]
    [(= opción 5)(mostrar-base base 0 0 numero-producto)]
    [(= opción 0)(display "Gracias por usar nuestros servicios")]
    (else(display "vuelva a intentarlo, la opción no se encuentra en la lista. \n"); Usé la función de condicional, meramente por si el usuario digita mal una opción...
         (set! opción (read))
         (condicional-opciones opción base numero-producto)); Aquí solo llamo el condicional con el nuevo dato que le pregunto al usuario, así no vuelve a mostrar toda la interfaz...
    )                                                       ; anteriormente mostrada
    ); todo esto es el condicional para el menú, cada opción representa una función auxiliar para realizar dicha acción
(define (registrar base numero-producto) ;registrar se refiere a la primera opción de la lista, donde se permite al usuario regustrar un producto a su gusto
  (define llena-volver 0)
  (if (< numero-producto (vector-length base))
      (begin
        (display "Inserte el código del producto: ")
        (vector-set! (vector-ref base numero-producto) 0 (read))
        (displayln "Inserte el género del libro:")
        (displayln "1) Acción")
        (displayln "2) Romance")
        (displayln "3) Misterio")
        (displayln "4) Autoayuda")
        (displayln "5) Académico")
        (vector-set! (vector-ref base numero-producto) 1 (read))
        (display "Inserte el nombre del libro: ")
        (read-line)
        (vector-set! (vector-ref base numero-producto) 2 (read-line))
        (display "Inserte la fecha máxima de disponibilidad (dd/mm/aaaa): ")
        (vector-set! (vector-ref base numero-producto) 3 (read-line))
        (display "Inserte el número de páginas: ")
        (vector-set! (vector-ref base numero-producto) 4 (read-line))
        (display "Inserte la cantidad de libros disponibles: ")
        (vector-set! (vector-ref base numero-producto) 5 (read))
        (display "Indique el costo del libro: ")
        (vector-set! (vector-ref base numero-producto) 6 (read))
        (vector-set! (vector-ref base numero-producto) 7 (* (vector-ref(vector-ref base numero-producto) 5)(vector-ref(vector-ref base numero-producto) 6)))
        (set! numero-producto (+ numero-producto 1))
        (interfaz base numero-producto)
        ) ; en sí aquí termina el código para registrar un valor, más sin embargo queda un condicional if para que verifique si la base de datos está llena o no
      (begin
        (displayln "¡La base de datos está llena!")
        (displayln "¿Quiere volver al menú anterior? (responder negativamente cerrará el programa)")
        (display "1) Si \n2) No")        (set! llena-volver (read))
        (cond
          [(= llena-volver 1)(interfaz base numero-producto)]
          [(= llena-volver 2)(display "Gracias por usar nuestros servicios")]
          )
        )
      )
  )
(define (actualizar-datos base código contador numero-producto)
  (define opcn 0)
 (if (> numero-producto 0)
     (begin
       (if (< contador (vector-length base))
           (begin
             (if (= código (vector-ref (vector-ref base contador) 0))
                 (begin
                   (display "Actualmente está modificando el producto: ")
                   (displayln (vector-ref (vector-ref base contador) 2))
                   (displayln "¿Desea continuar editando?")
                   (display "1) Si 2) No")
                   (set! opcn (read))
                   (if (= opcn 1)
                       (begin
                         (display "Inserte la fecha máxima de disponibilidad (dd/mm/aaaa): ")
                         (read-line)
                         (vector-set! (vector-ref base contador) 3 (read-line))
                         (display "Indique el costo del libro: ")
                         (vector-set! (vector-ref base contador) 6 (read))
                         (displayln "¿Desea modificar otro producto?")
                         (displayln "1) Si 2) Volver al menú anterior")
                         (set! opcn (read))
                         (cond
                           [(= opcn 1)(display "Inserte el código del producto")(actualizar-datos base (read) 0 numero-producto)]
                           [(= opcn 2)(interfaz base numero-producto)]
                           (else
                            (display "Esa no era una opción, volviendo al menú anterior")
                            (interfaz base numero-producto)
                            )
                           )
                         )
                       (begin
                         (displayln "¿Desea modificar otro producto?")
                         (displayln "1) Si 2) Volver al menú anterior")
                         (set! opcn (read))
                         (cond
                           [(= opcn 1)(display "Inserte el código del producto")(actualizar-datos base (read) 0 numero-producto)]
                           [(= opcn 2)(interfaz base numero-producto)]
                           (else
                            (display "Esa no era una opción, volviendo al menú anterior")
                            (interfaz base numero-producto)
                            )
                           )
                         )
                       )
                   )
                 (actualizar-datos base código (+ contador 1) numero-producto)
                 )
             )
           (begin
             (displayln "¡El producto no existe en la base de datos!")
             (displayln "¿Desea modificar otro producto?")
             (displayln "1) Si 2) Volver al menú anterior")
             (set! opcn (read))
             (cond
               [(= opcn 1)(display "Inserte el código del producto")(actualizar-datos base (read) 0 numero-producto)]
               [(= opcn 2)(interfaz base numero-producto)]
               (else
                (display "Esa no era una opción, volviendo al menú anterior")
                (interfaz base numero-producto)
                )
               )
             )
           )
       )
     (display "No hay libros registrados...")
     )
  )
(define (lista-tipos base numero-producto)
  (displayln "Inserte el género del libro que desea filtrar:")
  (displayln "1) Acción")
  (displayln "2) Romance")
  (displayln "3) Misterio")
  (displayln "4) Autoayuda")
  (displayln "5) Académico")
  (define tipo (read))
  (imprimir-lista-tipos base 0 0 tipo numero-producto)
  )
(define (imprimir-lista-tipos base posición contador tipo numero-producto)
  (define opc 0)
  (if (< posición (vector-length base))
      (if (< contador 8)
          (begin
            (if (= tipo (vector-ref (vector-ref base posición) 1))
                (begin
                  (display (vector-ref (vector-ref base posición) contador))
                  (display " | ")
                  (imprimir-lista-tipos base posición (+ contador 1) tipo numero-producto)
                  )
                (begin
                  (newline) 
                  (imprimir-lista-tipos base (+ posición 1) 0 tipo numero-producto)
                  )
                )
            )
          (begin
            (newline)
            (imprimir-lista-tipos base (+ posición 1) 0 tipo numero-producto)
            )
          )
      (begin
        (displayln "¿Desea volver al menú anterior?")
        (displayln "1) Si 2) No")
        (set! opc (read))
        (cond
          [(= opc 1)(interfaz base numero-producto)]
          [(= opc 2)(display "Gracias por usar nuestros servicios")]
          )
        )
      )
  )
(define (lista-precios base numero-producto)
  (display "Inserte el precio del libro que desea filtrar: ")
  (define precio (read))
  (imprimir-lista-precios base 0 0 precio numero-producto)
  )
(define (imprimir-lista-precios base posición contador precio numero-producto)
  (define opc 0)
  (if (< posición (vector-length base))
      (if (< contador 8)
          (begin
            (if (<= precio (vector-ref (vector-ref base posición) 6))
                (begin
                  (display (vector-ref (vector-ref base posición) contador))
                  (display " | ")
                  (imprimir-lista-precios base posición (+ contador 1) precio numero-producto)
                  )
                (begin
                  (newline) 
                  (imprimir-lista-precios base (+ posición 1) 0 precio numero-producto)
                  )
                )
            )
          (begin
            (newline)
            (imprimir-lista-precios base (+ posición 1) 0 precio numero-producto)
            )
          )
      (begin
        (displayln "¿Desea volver al menú anterior?")
        (displayln "1) Si 2) No")
        (set! opc (read))
        (cond
          [(= opc 1)(interfaz base numero-producto)]
          [(= opc 2)(display "Gracias por usar nuestros servicios")]
          )
        )
      )
  )
(define (mostrar-base base posición contador numero-producto); función ya trabajada en clase, la cual es un proceso recursivo para mostrar de manera ordenada los datos de un vector
  (define opc 0)
  (if (< posición (vector-length base))
    (if (< contador (vector-length (vector-ref base posición)))
        (begin
          (display (vector-ref (vector-ref base posición) contador))
          (display " | ")
          (mostrar-base base posición (+ contador 1) numero-producto)
        )
        (begin
          (newline) 
          (mostrar-base base (+ posición 1) 0 numero-producto)
        )
     )
    (begin
      (displayln "¿Desea volver al menú anterior?")
      (displayln "1) Si 2) No")
      (set! opc (read))
      (cond
        [(= opc 1)(interfaz base numero-producto)]
        [(= opc 2)(display "Gracias por usar nuestros servicios")]
        )
      )
    )
  )
  (inicio)