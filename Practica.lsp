;------------------------------------------------------------------------------
;                              PRÁCTICA LISP
; Alumnos:
;           Nadal Llabrés Belmar
;           Andreu López Cortés
; Curso:
;           2019/2020
; Asignatura: 
;           21721 - Lenguajes de Programación
;
;------------------------------------------------------------------------------

;******************************************************************************
;*          DECLARACIONES INICIALES E INTRODUCCIÓN DE PRODUCTOS               *
;******************************************************************************

;------------------------------------------------------------------------------
; Declaración de constantes
;------------------------------------------------------------------------------
(defconstant IMAGE_AREA_START_X 440)
(defconstant IMAGE_AREA_START_Y 175)
(defconstant IMAGE_AREA_SIZE 200)

(defconstant PRODUCT_AREA_START_X 5)
(defconstant PRODUCT_AREA_START_Y 175)
(defconstant PRODUCT_AREA_LONG_X 430)
(defconstant PRODUCT_AREA_LONG_Y 155)

(defconstant LOGO "images/LogoPractica.bmp")

(defconstant START_ORDER_MESSAGE "[] INICIAR PEDIDO (S/N):")
(defconstant ORDER_NUMBER_MESSAGE "[] INDICAR NUMERO DE PEDIDO:")
(defconstant PRODUCT_NUMBER_MESSAGE "[] NUMERO DE PRODUCTO:")
(defconstant CONTINUE_ORDER_MESSAGE "[] CONTINUAR PEDIDO (S/N):")

;------------------------------------------------------------------------------
; Introducción de los productos junto a su precio. Lista de listas de productos.
;------------------------------------------------------------------------------
(set 'products '(("UBoost 20" 179.95)
                ("Zoom 20" 129.95)
                ("Pegasus 37" 119.95)
                ("Vaporfly X" 247.95)
                ("Pegasus Tu" 179.95)
                ("Zoom Fly 3" 159.95)
                ("Saucony Gu" 275.95)
                ("Fujitrabuco" 109.95)
                ("Venture" 105.95)
                ("GT-20 Trail" 139.95)
                ("Terrex Ultra" 179.95)
                ("Terrex Agra" 149.95)
                ("Adidas SL20" 115.95)
                ("Nimbus 22" 179.95)
                ("Levitate 3" 169.95)
                ("Fresh Foam" 169.95)
                ("Joyride Dual" 129.95)
                ("Vomero 15" 149.95)
                ("Glycerin 18" 119.95)
                ("GlideR Pro" 159.95))
)

;Lista que se usará posteriormente para almacenar los productos que el usuario
;haya elegido. 
(set 'selectedProducts '())

;******************************************************************************
;*                         FUNCIONES DEL PROGRAMA                             *
;******************************************************************************

;------------------------------------------------------------------------------
; Función que inicia todo el programa.
;------------------------------------------------------------------------------
(defun inicio()
    (initGuiElements)
    (menu)
    (goodbyeMessage)
)

;------------------------------------------------------------------------------
; Inicializa los elementos gráficos. 
;------------------------------------------------------------------------------
(defun initGuiElements()
    (cls)
    (printProductList)
    (initializeOrderNumberWindow)
    (printHeader)
    (initializeChosedProductsWindow)
    (initializeComunicationWindow)
)

;------------------------------------------------------------------------------
; Encargada de gestionar el menú y recolectar los datos del usuario. 
;------------------------------------------------------------------------------
(defun menu ()
    ;Bucle principal
    (loop

        ;Reset de valores
        (setq totalOrderPrice 0)
        (resetCart)
        (eraseChosedProductsWindow)
        (initializeTotalWindow)
        (initializeImageArea)

        (setq logoPrinted t)
        (writeInComunicationWindow START_ORDER_MESSAGE)
        (if (not (string= "S" (princ-to-string (read)))) (return))

        (writeInComunicationWindow ORDER_NUMBER_MESSAGE)
        (setq orderNumber (read))
        (printOrderNumber orderNumber)

        ; Segundo bucle para la elección de productos. 
        (loop
            (if (not logoPrinted) (initializeImageArea))
            (writeInComunicationWindow PRODUCT_NUMBER_MESSAGE)
            (setq productNumber (read))
            (printProductImage productNumber)
            (setq logoPrinted nil)

            (setq productName (getProductName productNumber))   
            (setq productPrice (getProductPrice productNumber))

            (writeInComunicationWindow (concatenate 'string "UNIDADES DE " productName ":"))
            (setq productQuantity (read))

            (setq confirmation t)
            (writeInComunicationWindow (concatenate 'string (princ-to-string productQuantity) " DE " productName " (S/N):"))
            (if (not (string= "S" (princ-to-string (read)))) (setq confirmation nil))

            (if confirmation (progn
                (addToCart productName productQuantity productPrice)
                (updateTotal productPrice productQuantity)
            ))

            (writeInComunicationWindow CONTINUE_ORDER_MESSAGE)
                (if (not (string= "S" (princ-to-string (read)))) (progn 
                (generateBill orderNumber)
                (color 0 255 0)
                (writeInComunicationWindow "Factura generada!")
                (resetCart)
                (color 0 0 0)
                (return)
            ))
        )
    )
)

;------------------------------------------------------------------------------
; Genera la factura con el formato: pedidoX.txt 
; Donde X es el número de pedido. 
;------------------------------------------------------------------------------
(defun generateBill (orderNumber)
    (setq file (open (concatenate 'string "pedido" (princ-to-string orderNumber) ".txt") :direction :output))

    ;Primera línea.
    (princ (concatenate 'string "PEDIDO " (princ-to-string orderNumber)) file)
	(write-char #\newline file)

    ;Segunda línea.
    (princ "\t PRODUCTOS \t UNIDADES \t IMPORTE" file)
	(write-char #\newline file)

    (dolist (i selectedProducts)

        ;Quitar los corchetes iniciales i finales. 
        (setq i (string-trim "[]" i))

        (setq startIndex 0)
        (setq endIndex 1)

        ;Iterar por el producto para separar nombre, unidades y precio. 
        (dotimes (j (length i))

            ;Coger primer char
            (setq char (subseq i startIndex endIndex))

            (if (string-equal char "/") (princ "\t\t" file) (princ char file))
            
            (setq startIndex (+ startIndex 1)) 
            (setq endIndex (+ endIndex 1)) 
        
        )
        ;Nueva línea para el siguiente producto. 
    	(write-char #\newline file)    
    )

    ;Última línea.
    (princ (concatenate 'string "TOTAL PEDIDO\t" (princ-to-string totalOrderPrice) " euros") file)
)

;------------------------------------------------------------------------------
; Extiende un string por la parte trasera con la cantidad indicada en size 
; y el carácter que se utilizará para extender en char.
;------------------------------------------------------------------------------
(defun extendStringBehind (str size char)
    (setq size (- size (length str)))
    (dotimes (i size)
        (setq str (concatenate 'string str char))    
    )
    (return-from extendStringBehind str)
)

;------------------------------------------------------------------------------
; Extiende un string por la parte delantera con la cantidad indicada en size 
; y el carácter que se utilizará para extender en char.
;------------------------------------------------------------------------------
(defun extendStringFront (str size char)
    (setq size (- size (length str)))
    (dotimes (i size)
        (setq str (concatenate 'string char str))    
    )
    (return-from extendStringFront str)
)

;------------------------------------------------------------------------------
; Elimina todos los productos del carrito de compra. 
;------------------------------------------------------------------------------
(defun resetCart()
    (setq selectedProducts ())
)

;------------------------------------------------------------------------------
; Resetea la ventana de productos elegidos para que se pueda generar un nuevo
; pedido. 
;------------------------------------------------------------------------------
(defun eraseChosedProductsWindow ()
    (setq nline 16)

    (dotimes (i 6)
        (eraseText nline 1 79)
        (setq nline (+ nline 1))    
    )
)

;------------------------------------------------------------------------------
; Se añade un producto al carrito. Los datos son: 
; - Nombre del producto. 
; - Cantidad. 
; - Precio de una unidad del producto. 
;------------------------------------------------------------------------------
(defun addToCart (productName productQuantity productPrice)
    ;Extener string para que todos tengan el mismo tamaño.
    (setq productName (extendStringBehind productName 12 " "))

    (setq element (concatenate 'string "[" productName " /" (princ-to-string productQuantity) "/ " (princ-to-string (* productQuantity productPrice)) "]"))
    (setq selectedProducts (cons element selectedProducts))
    (printCart)
)

;------------------------------------------------------------------------------
; Imprime los productos del carrito en pantalla para que el usuario tenga 
; feedback de lo que tiene actualmente pedido. 
;------------------------------------------------------------------------------
(defun printCart ()

    ;Inicialización de variables
    (setq nline 16)
    (setq ncolumn -25)
    (setq nproduct 0)

    ;Se revierte la lista para que se muestren en orden de inserción
    (setq auxSelectedProducts selectedProducts)
    (setq auxSelectedProducts (reverse auxSelectedProducts))

    (dolist (i auxSelectedProducts nproduct)

        ;Cuando se llega a la mitad de los productos (6), se cambia a la columna
        ;29 y se vuelve empezar en la fila 16
        (cond ((= (mod nproduct 6) 0)
            (setq ncolumn (+ ncolumn 26))
            (setq nline 16))
        )

        (setq line i)

        ;Mostrar en pantalla.
        (write nline ncolumn line)

        ;Incrementar variables.
        (setq nline (+ nline 1))
        (setq nproduct (+ nproduct 1))
    )
)

;------------------------------------------------------------------------------
; Devuelve el precio de un producto pasando por parámetro su referencia (número
; de producto). 
;------------------------------------------------------------------------------
(defun getProductPrice (productNumber)

    (setq products2 products)
    (setq counter 0)

    (dolist (i products2 productNumber)
        (if (= counter productNumber) (return-from getProductPrice (car (cdr i))))
        (setq counter  (+ counter 1))
        (setq products2 (cdr products2))
    )
)

;------------------------------------------------------------------------------
; Devuelve el nombre de un producto pasando por parámetro su referencia (número
; de producto). 
;------------------------------------------------------------------------------
(defun getProductName (productNumber)

    (setq products2 products)
    (setq counter 0)

    (dolist (i products2 productNumber)
        ;Progn permite poner más de una sentencia en la clausula "then" del if (en este caso no es necesario).
        (if (= counter productNumber) (return-from getProductName (princ-to-string (car i))))
        (setq counter  (+ counter 1))
        (setq products2 (cdr products2))
    )
)

;------------------------------------------------------------------------------
; Actualiza el nuevo total después de haber cambios en el carrito. 
;------------------------------------------------------------------------------
(defun updateTotal (productPrice productQuantity)
    (setq totalOrderPrice (+ (* productQuantity productPrice) totalOrderPrice))
    (printTotal totalOrderPrice)
)

;------------------------------------------------------------------------------
; Imprime el total en euros del contenido del carrito en la caja inferior 
; derecha. 
;------------------------------------------------------------------------------
(defun printTotal (total)
    (printWord  "TOTAL" 330 10 1)

    ;Añadir ceros por delante. 
    (setq total (extendStringFront (princ-to-string total) 9 "0"))

    (printWord total 440 10 1)
)

;------------------------------------------------------------------------------
; Imprime la imagen de un producto mediante el paso de su referencia. 
;------------------------------------------------------------------------------
(defun printProductImage (productNumber)
    (printImage (concatenate 'string "images/products/" (princ-to-string productNumber) ".bmp") IMAGE_AREA_START_X IMAGE_AREA_START_Y IMAGE_AREA_SIZE)
)

;------------------------------------------------------------------------------
; Escribe en la ventana de comunicación del usuario el texto indicado por 
; parámetro. Si había texto ya escrito, lo elimina.  
;------------------------------------------------------------------------------
(defun writeInComunicationWindow (message)
    (eraseText 23 1 40)
    (write 23 1 message)
    (goto-xy (+ (length message) 2) 23)
)

;------------------------------------------------------------------------------
; Imprime en el área correspondiente el número de pedido que ha introducido el 
; usuario. 
;------------------------------------------------------------------------------
(defun printOrderNumber (number)
    (setq pedido (concatenate 'string "PEDIDO " (princ-to-string number)))
    
    ;Añadir espacios para imprimir con cuadros blancos. (30 carácteres total).
    (setq pedido (extendStringBehind pedido 30 " "))

    (printWord pedido 8 145 1)
)

;------------------------------------------------------------------------------
; Mensaje de despedida cuando se cierra el programa. 
;------------------------------------------------------------------------------
(defun goodbyeMessage ()
    (cls)
    (color 0 0 255)
    (princ "Gracias por utilizar la app! Hasta pronto!\n\nNadal Llabres Belmar \nAndreu Lopez Cortes")
    (color 0 0 0)
)

;------------------------------------------------------------------------------
; Dibuja el área donde se muestran los productos disponibles.
;------------------------------------------------------------------------------
(defun printProductList () 
    (rectangle PRODUCT_AREA_START_X PRODUCT_AREA_START_Y PRODUCT_AREA_LONG_X PRODUCT_AREA_LONG_Y)
    (printProducts)
)

;------------------------------------------------------------------------------
; Mostrar los productos disponibles en el áerea correspondiente. 
;------------------------------------------------------------------------------
(defun printProducts ()

    ;Inicialización de variables
    (setq nline 3)
    (setq ncolumn 1)
    (setq nproduct 0)

    (dolist (i products nproduct)

        ;Cuando se llega a la mitad de los productos (10), se cambia a la columna
        ;29 y se vuelve empezar en la fila 3
        (cond ((= nproduct 10)
            (setq ncolumn 29)
            (setq nline 3))
        )

        ;Concatenar Número de producto + Nombre del producto + tabulación.
        (setq line (concatenate 'string (princ-to-string nproduct) ". " (car i) "\t"))

        ;Concatenar precio
        (setq line (concatenate 'string line (princ-to-string (car (cdr i)))))

        ;Mostrar en pantalla.
        (write nline ncolumn line)

        ;Incrementar variables.
        (setq nline (+ nline 1))
        (setq nproduct (+ nproduct 1))
    )
)

;------------------------------------------------------------------------------
; Inicializa la ventana para interaccionar con el usuario. 
;------------------------------------------------------------------------------
(defun initializeComunicationWindow ()
    (rectangle 5 5 315 30) 
)

;------------------------------------------------------------------------------
; Inicializa la ventana donde se muestra el total del pedido. 
;------------------------------------------------------------------------------
(defun initializeTotalWindow ()
    (rectangle 325 5 310 30)
    (fillAreaColor 0 0 0 638 5 325 35)
)

;------------------------------------------------------------------------------
; Inicializa la ventana donde se muestra el número de pedido. 
;------------------------------------------------------------------------------
(defun initializeOrderNumberWindow ()
    (rectangle 5 140 633 30)
    (fillAreaColor 0 0 0 5 140 638 170)
)

;------------------------------------------------------------------------------
; Imprime la cabecera del programa.  
;------------------------------------------------------------------------------
(defun printHeader () 
    (rectangle 5 335 430 39)
    (fillAreaColor 0 0 0 5 335 435 469)
    (printWord "PRODUCTOS" 90 345 10)
)

;------------------------------------------------------------------------------
; Inicializa la ventana donde se muestran los productos del carrito. 
;------------------------------------------------------------------------------
(defun initializeChosedProductsWindow ()
    (rectangle 5 40 633 95)
)

;------------------------------------------------------------------------------
; Dibuja el área donde se muestran las imágenes de los productos.
;------------------------------------------------------------------------------
(defun initializeImageArea ()
    (printImage LOGO IMAGE_AREA_START_X IMAGE_AREA_START_Y IMAGE_AREA_SIZE)
)

;******************************************************************************
;*                          FUNCIONES DE APOYO                                *
;******************************************************************************

;------------------------------------------------------------------------------
; Dibuja un rectángulo en la posición x, y de tamaño dimx y dimy.
;------------------------------------------------------------------------------
(defun rectangle (x y dimx dimy)
	(move x y)
	(draw x (+ y dimy) (+ x dimx) (+ y dimy) (+ x dimx) y x y)
)

;------------------------------------------------------------------------------
; Imprime una imagen cuadrada en la posición x y de la ventana. El último
; parámetro indica la dimensión de la misma. 
;------------------------------------------------------------------------------
(defun printImage (imagen x y dimension)
	(setq fichero (open imagen :direction :input 
	:element-type 'unsigned-byte))

    ;Lectura de la cabecera del archivo .bmp (54 bytes)
    (dotimes (i 54)
        (read-byte fichero nil)
    )

	(setq pixel 1)
	(setq R 0 G 0 B 0)
	(setq x1 x)
	(move x1 y)
	(loop 
		(setq B (read-byte fichero nil))
		(setq G (read-byte fichero nil))
		(setq R (read-byte fichero nil))
		(if (null B) (return ()) )
		(if (null G) (return ()) )
		(if (null R) (return ()) )
		(color R G B)
		(draw (+ 1 x1) y)
		(setq pixel  (+ pixel  1))
		(setq x1 (+ x1 1))
		(cond  ((> pixel  dimension) (setq pixel  1) (setq x1 x) (setq y (+ y 1)) ) )
		(move x1 y)
	) 
	(color 0 0 0)	
        (close fichero)
)

;------------------------------------------------------------------------------
; Escribe un texto en la línea y columna indicada de la ventana. 
;------------------------------------------------------------------------------
(defun write (linea columna TEXTO)
	(goto-xy columna linea)
	(princ TEXTO)
)

;------------------------------------------------------------------------------
; Rellena un área específica del color elegido. 
;------------------------------------------------------------------------------
(defun fillAreaColor (r g b x1 y1 x2 y2)
	(color r g b)
	(dotimes (i (- y2 y1))
		(move x1 (+ i y1))
		(draw x2 (+ i y1))
	)
	(color 0 0 0)
)

;------------------------------------------------------------------------------
; Imprime una letra en la posición x y.
;------------------------------------------------------------------------------
(defun printLetter (letra x y)
    (if (string-equal letra " ") (setq letra ""))
    (printImage (concatenate 'string "images/charactersAndDigits/" letra "_NB.bmp") x y 20)
)

;------------------------------------------------------------------------------
; Imprime una palabra en la posición x y con el espaciado indicado. 
;------------------------------------------------------------------------------
(defun printWord (palabra x y espaciado)
    (dotimes (i (length palabra))
	(printLetter (string (aref palabra i)) x y)
	(setq x (+ 20 x espaciado))
    )
)

;------------------------------------------------------------------------------
; Elimina el texto a partir de la línea y columna especificada y la cantidadad
; de columnas. 
;------------------------------------------------------------------------------
(defun eraseText (linea columna numcolumnas)
	(goto-xy columna linea)
	(dotimes (i numcolumnas)
		(princ " ")
		(goto-xy (+ i columna) linea)
	)
)