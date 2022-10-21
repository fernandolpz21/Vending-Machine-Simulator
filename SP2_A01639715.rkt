#|

Evidencia #2: Implementación de un simulador de una máquina expendedora (primera parte)

Fernando López Gómez | A01639715

23/10/2022

|#


#lang racket


;--------------------------------------------------------------------------------------------------
;************** LEEMOS Y CERRAMOS LOS ARCHIVOS ****************
(define transacciones(read (open-input-file "transacciones.txt")))
(define monedas (read (open-input-file "monedas.txt")))
(define productos (read (open-input-file "productos.txt")))
(close-input-port (open-input-file "transacciones.txt"))
(close-input-port (open-input-file "monedas.txt"))
(close-input-port (open-input-file "productos.txt"))






;--------------------------------------------------------------------------------------------------
;************** UPDATE FUNCTIONS ****************

(define (update-stock producto lista-productos)
  ;Encontramos el producto que se vendió
  ;y le restamos 1 al stock
  (cond
    [(null? lista-productos) '()]
    [(equal? producto (caar lista-productos))
    (append (list (list(caar lista-productos) (cadar lista-productos) (- (caddar lista-productos) 1)))
            (update-stock producto (cdr lista-productos)))]
    [else (append (list(car lista-productos)) (update-stock producto (cdr lista-productos)))]
  )
)

;Las monedas ingresadas son una lista ordenada de las monedas que se utilizaron
(define (add-coins monedas monedas-ingresadas)
  (cond
    [(null? monedas) '()] 
    ;Si no hay monedas ingresadas o no coinciden con el valor buscado, pasa a la siguiente moneda
    [(or (null? monedas-ingresadas) (not(equal? (car monedas-ingresadas) (caar monedas))))
     (append (list(car monedas)) (add-coins (cdr monedas) monedas-ingresadas))
        ]
    ;Agrega 1 a la cantidad de monedas actuales y corta la lista de monedas ingresadas
    ;hasta que dejen de repetirse los valores
    [else (append (list(cons (caar monedas)
                             (+(cdar monedas)(count monedas-ingresadas (car monedas-ingresadas)))))
                (add-coins (cdr monedas) (cut-list monedas-ingresadas
                                                   (count monedas-ingresadas (car monedas-ingresadas)))))]
    )
  )




(define (update-coins cambio monedas)
  (cond
    [(null? monedas) '()]
    ; Si es que si puede haber cambio con esa moneda y si hay
    ; monedas disponibles
    ;(quotient cambio (caar monedas)) --> Número de billetes posibles 
    [(and (not(equal? (quotient cambio (caar monedas)) 0))
          (>= (cdar monedas) (quotient cambio (caar monedas))))
    (append (list(cons (caar monedas) (- (cdar monedas) (quotient cambio (caar monedas)))))
            (update-coins (- cambio (* (quotient cambio (caar monedas)) (caar monedas))) (cdr monedas)))]

    ;Si es que no hay monedas suficientes o la moneda excede al cambio
    ;continúa con la siguiente moneda
    [else (append (list(car monedas)) (update-coins cambio (cdr monedas)))]
    )
  
  )


  ;********************* LIST RELATED FUNCTIONS ******************************
(define (find-product-price product list)
  ;Regresa el precio de un producto determinado
  (if (null? list)
      '()
      (if (equal? product (caar list))
          (cadar list)
          (find-product-price product (cdr list))
          )
      )
  )

(define (find-product-stock product list)
  ;Regresa el precio de un producto determinado
  (if (null? list)
      '()
      (if (equal? product (caar list))
          (caddar list)
          (find-product-stock product (cdr list))
          )
      )
  )

;Contar elementos dentro de una lista plana
(define (count list target)
  (if (null? list)
      0
      (if (equal?(car list) target)
          (+ 1 (count (cdr list) target))
          (count (cdr list) target)
          )
      )
  ) 

;Cortar n elementos de una lista
(define (cut-list list spaces)
  (if (equal? spaces 1)
      (cdr list)
      (cut-list (cdr list) (- spaces 1))
      )
  )

  ;******************* PROCESS STATE FUNCTIONS *******************
  
(define (success producto precio-producto monedas-ingresadas transacciones-nueva)
  

        
  ;Despliegue de información de la transacción
  (display "TRANSACCIÓN EXITOSA\n")
  (display "Producto: ")
  (display producto)
  (display "\n")
  (display "Precio: ")
  (display precio-producto)
  (display "\n")
  (display "Monto ingresado: ")
  (display (apply + monedas-ingresadas))
  (display "\n")
  (display "Cambio: ")
  (display (-(apply + monedas-ingresadas)  precio-producto))
  (display "\n")
  (display "------------------")
  (display "\n")


   #| 
 
  Para poder actualizar productos y monedas, mandamos llamar a la función update-stock,
    que regresa una lista con los productos actualizados

   Para el caso de las monedas, se utiliza la función update-coins, la cual
    tiene implicita la función add-coins, que se encarga de actualizar las monedas
    con la cantidad que ingresó el usuario.
 Una vez recibido esta nueva lista, genera otra lista que resta las monedas necesarias
 para proporcionarle el cambio al usuario.
  |#

  ;Leemos la siguiente transacción
  (leerTransacciones (cdr transacciones-nueva)
                     (update-stock producto productos)
                     (update-coins (-(apply + monedas-ingresadas)  precio-producto);cambio
                       (add-coins monedas (sort monedas-ingresadas >))); monedas
                     )
  )

;---------------------------
  
(define (monedas-aceptadas? monedas-ingresadas monedas-disponibles)
  (if (not(null? monedas-ingresadas)) 
      (if (not (equal? (member (car monedas-ingresadas) monedas-disponibles) #f))
          (monedas-aceptadas? (cdr monedas-ingresadas) monedas-disponibles)
          #f); Si member retorna false la moneda no es válida
      #t) ;si llega al final sin salirse retorna true
  )

     
  
;---------------------------
(define (evalua transacciones productos monedas)
  ;Evaluamos si la transacción es posible
  (cond
    ;Si alguna moneda no es aceptada
    [(not(monedas-aceptadas? (cdar transacciones) (map car monedas)))
         (display "ERROR: No se ha aceptado alguna de las monedas\n")]
    ;Si la suma de las monedas ingresadas no es suficiente
    [(< (apply + (cdar transacciones)) (find-product-price (caar transacciones) productos))
        (display "ERROR: No se han ingresado monedas suficientes\n")]
    ; Si el stock está vacío
    [(equal? (find-product-stock (caar transacciones) productos) 0) 
        (display "ERROR: Producto no disponible\n")]
    [else (success (caar transacciones);Producto
                   (find-product-price (caar transacciones) productos);Precio
                   (cdar transacciones);Monedas ingresadas
                   transacciones);lista de transacciones
          ])

  #|
  !!Se están repitiendo mucho.. tienes que sacar esta instrucción de aquí
  (leerTransacciones (cdr transacciones)
                     productos
                     monedas
                     )
 |#

 )

;---------------------------
(define (terminar-procesos productos monedas)

  ;Mostramos los datos finales al usuario
  (display "\n")
  (display "----------- FIN DE PROCESOS ---------- \n")
  (display "GANANCIA OBTENIDA: ")
  (display "YOUR_FUNCTION_HERE");(- (apply + (map cdr monedas-finales)) monedas-iniciales));NO FUNCIONA
  (display "\n")
  (display "PRODUCTOS CON POCO INVENTARIO: ")
  (display (map car (filter (lambda (x) (<= (caddr x) 2)) productos)))
  (display "\n")
  (display "MONEDAS CON MUCHO INVENTARIO: ")
  (display (map car (filter (lambda (x) (>= (cdr x) 35)) monedas)))
  (display "\n")
  (display "MONEDAS CON POCO INVENTARIO: ")
  (display (map car (filter (lambda (x) (<= (cdr x) 5)) monedas)))

  ;Modificamos los archivos con los valores que resultaron
  (define archivo-productos (open-output-file "productos.txt" #:exists `replace))
  (write productos archivo-productos)
  (close-output-port archivo-productos)
  
  (define archivo-monedas (open-output-file "monedas.txt" #:exists `replace))
  (write monedas archivo-monedas)
  (close-output-port archivo-monedas)
  
  
)

;----------------------------------
(define (leerTransacciones transacciones-nueva productos-nuevos monedas-nuevas)
  (if (not(null? transacciones-nueva)) 
      (evalua transacciones-nueva productos-nuevos monedas-nuevas)
      (terminar-procesos productos-nuevos monedas-nuevas)
   )
 )


;--------------------------------------------------------------------------------------------------
; *************** PROGRAMA PRINCIPAL *********************************
(define (main)
  (display "----- SISTEMA DE EVALUACIÓN DE TRANSACCIONES -----\n")
  
  (leerTransacciones transacciones productos monedas)
  (close-input-port(open-input-file "transacciones.txt"))
  (close-input-port(open-input-file "monedas.txt"))

 )



(main)