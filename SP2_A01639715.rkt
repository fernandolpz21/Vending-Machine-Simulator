#|

Evidencia #2: Implementación de un simulador de una máquina expendedora (primera parte)

Fernando López Gómez | A01639715

23/10/2022

|#


#lang racket

;--------------------------------------------------------------------------------------------------
;********* DEFINIMOS CONSTANTES *************

;(define productos (read (open-input-file "productos.txt")))
;(define archivo-productos (open-output-file "productos.txt" #:exists `replace))

(define archivo-monedas (open-input-file "monedas.txt"))
(define monedas (read archivo-monedas))
(define transacciones (open-input-file "transacciones.txt"))


;--------------------------------------------------------------------------------------------------
;************** UDATE FUNCTIONS ****************

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

(define (update-coins cambio)
  (define archivo-monedas (open-input-file "monedas.txt"))
  (define monedas (read archivo-monedas))
  ;(map car monedas) ;Tipos de monedas
  (close-input-port archivo-monedas)
  

  )


  ;********************* FIND IN LIST FUNCTIONS ******************************
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


  ;******************* PROCESS STATE FUNCTIONS *******************
  
(define (success producto precio-producto monedas-ingresadas lista-productos)
  ;Despliega información de la transacción y
  ;actualiza los datos de los productos en el archivo

  ;Abrimos el archivo
  (define archivo-productos (open-output-file "productos.txt" #:exists `replace))
  ;Escribimos sobre él
  (write (update-stock producto lista-productos) archivo-productos)
  ;Cerramos y actualizamos
  (close-output-port archivo-productos)

  ;(write (update-coins cambio) archivo-monedas)
  

  ;UI
  (display "TRANSACCIÓN EXITOSA\n")
  (display "Cambio: ")
  (display (- precio-producto(apply + monedas-ingresadas)))
  (display "\n")
  )

;---------------------------
  
(define (transacción-exitosa? producto precio-producto monedas-ingresadas lista-productos)
  ;Si el precio del producto es mayor o igual a la suma de las monedas
  ;involucradas en la transacción
  (if (>= precio-producto(apply + monedas-ingresadas))
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
  (display "------ FIN DE PROCESOS ------ \n")
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