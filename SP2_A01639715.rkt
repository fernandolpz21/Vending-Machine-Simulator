#|

Evidencia #2: Implementación de un simulador de una máquina expendedora (primera parte)

Fernando López Gómez | A01639715

23/10/2022

|#


#lang racket

;--------------------------------------------------------------------------------------------------
;********* DEFINIMOS CONSTANTES *************

(define transacciones (open-input-file "transacciones.txt"))


;--------------------------------------------------------------------------------------------------
;************** UPDATE FUNCTIONS ****************

(define (update-stock producto lista)
  ;Encontramos el producto que se vendió
  ;y le restamos 1 al stock
  (cond
    [(null? lista) '()]
    [(equal? producto (caar lista))
    (append (list (list(caar lista) (cadar lista) (- (caddar lista) 1)))
            (update-stock producto (cdr lista)))]
    [else (append (list(car lista)) (update-stock producto (cdr lista)))]
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


  ;********************* LIST MUTATORS FUNCTIONS ******************************
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

;Contar elementos dentro de una lista plana ordenada
(define (count list target)
  (if (null? list)
      0
      (if (equal?(car list) target)
          (+ 1 (count (cdr list) target))
          (count (cdr list) target)
          )
      )
  )

;Cortar elementos de una lista
(define (cut-list list spaces)
  (if (equal? spaces 1)
      (cdr list)
      (cut-list (cdr list) (- spaces 1))
      )
  )
                   
      
  
 

  ;******************* PROCESS STATE FUNCTIONS *******************
  
(define (success producto precio-producto monedas-ingresadas lista-productos)
  ;Despliega información de la transacción y
  ;actualiza los datos de los productos en el archivo

  ;Abrimos el archivo
  (define archivo-productos (open-output-file "productos.txt" #:exists `replace))
  (define arch-monedas-in (open-input-file "monedas.txt"))
  (define monedas (read arch-monedas-in))

  
  ;Escribimos sobre él
  (write (update-stock producto lista-productos) archivo-productos)
  ;Cerramos y actualizamos
  (close-output-port archivo-productos)

  (define arch-monedas-out (open-output-file "monedas.txt" #:exists `replace))
  
  
  ;Para poder usar las monedas que ingresó a manera de cambio, debemos actualizar la lista de
 ; las monedas disponibles antes, por lo que pasamos como parámetro a la función add-coins, que
  ;regresa un arreglo actualizado con las monedas que agregó el usuario

  (write (update-coins (-(apply + monedas-ingresadas)  precio-producto);cambio
                       (add-coins monedas (sort monedas-ingresadas >)));monedas
         arch-monedas-out)


  
  (close-output-port arch-monedas-out)
  ;(close-input-port arch-monedas-in)
  

  ;UI
  (display "TRANSACCIÓN EXITOSA\n")
  (display "Cambio: ")
  (display (-(apply + monedas-ingresadas)  precio-producto))
  (display "\n")
  (display "------------------")
  (display "\n")
  )

;---------------------------
  
(define (transacción-exitosa? producto precio-producto monedas-ingresadas lista-productos)
  ;Si el precio del producto es mayor o igual a la suma de las monedas
  ;involucradas en la transacción
  (if (>= (apply + monedas-ingresadas) precio-producto)
      (success producto precio-producto monedas-ingresadas lista-productos)
      (display "ERROR: No se han ingresado monedas suficientes")
      )
  )
  
;---------------------------
(define (evalua arch transacción)
  ;Para cada transacción leemos el archivo
  ;de productos para en caso de que este se haya actualizado
  (define productos (open-input-file "productos.txt"))
  (define listaProductos (read productos))
  ;Evaluamos si la transacción es posible
  (transacción-exitosa? (car transacción)
                        (find-product-price (car transacción) listaProductos)
                        (cdr transacción)
                        listaProductos
                        )

  ;Cerramos el archivo de productos para que se actualice
  (close-input-port productos)
  ; Evaluamos la siguiente transacción
  (leerTransacción arch (read arch))
 )

;---------------------------
(define (terminar-procesos arch)
  (display "\n")
  (display "----------- FIN DE PROCESOS ---------- \n")
  (display "GANANCIA OBTENIDA: ")
  (display "\n")
  (display "PRODUCTOS CON POCO INVENTARIO:")
  (display "\n")
  (display "MONEDAS CON MUCHO INVENTARIO")
  (display "\n")
  (display "MONEDAS CON POCO INVENTARIO:")
  
  (close-input-port arch)
  )
  ;---------------------------
(define (leerTransacción arch last-read)
  ;Leemos cada transacción.
  (if (not(eof-object? last-read))
      (evalua arch last-read)
      (terminar-procesos arch)
   )
 )


;--------------------------------------------------------------------------------------------------
; *************** PROGRAMA PRINCIPAL *********************************
(define (main)
  (display "----- SISTEMA DE EVALUACIÓN DE TRANSACCIONES -----\n")

  (leerTransacción transacciones (read transacciones))

 )



(main)