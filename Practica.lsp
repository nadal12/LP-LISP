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

;------------------------------------------------------------------------------
; Función que inicia todo el programa.
;------------------------------------------------------------------------------
(defun inicio()
    (cls)
    (printProductList)
    (initializeTotalWindow)
    (initializeOrderNumberWindow)
    (printHeader)
    (initializeImageArea)
    (initializeComunicationWindow)
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
    (write 3 1 "01. UltraBoost 20      189,99")
    (write 4 1 "02. Asics Nim. 22      169,99")
    (write 5 1 "03. NB Fresh Foam      199,99")
    (write 6 1 "04. UltraBoost 20      189,99")
    (write 7 1 "05. UltraBoost 20      189,99")
    (write 8 1 "06. UltraBoost 20      189,99")
    (write 9 1 "07. UltraBoost 20      189,99")
    (write 10 1 "08. UltraBoost 20      189,99")
    (write 11 1 "09. UltraBoost 20      189,99")
    (write 12 1 "10. UltraBoost 20      189,99")

    (write 3 31 "11. Product      189,99")
    (write 4 31 "12. Product      169,99")
    (write 5 31 "13. Product      199,99")
    (write 6 31 "14. Product      189,99")
    (write 7 31 "15. Product      189,99")
    (write 8 31 "16. Product      189,99")
    (write 9 31 "17. Product      189,99")
    (write 10 31 "18. Product      189,99")
    (write 11 31 "19. Product      189,99")
    (write 12 31 "20. Product      189,99")
)

(defun initializeComunicationWindow ()
    (rectangle 5 5 315 30) 
    (write 23 1 "[] INICIAR PEDIDO (S/N):")
	(goto-xy 26 23)
    (setq product (read)) 
)

(defun initializeTotalWindow ()
    (rectangle 325 5 310 30)
    (fillAreaColor 0 0 0 635 5 325 35)
)

(defun initializeOrderNumberWindow ()
    (rectangle 5 140 630 30)
    (fillAreaColor 0 0 0 5 140 635 170)
)

(defun printHeader () 
    (rectangle 5 335 430 39)
    (fillAreaColor 0 0 0 5 335 435 469)
    (printWord "PRODUCTOS" 90 345 10)
)

;------------------------------------------------------------------------------
; Dibuja el área donde se muestran los productos disponibles.
;------------------------------------------------------------------------------
(defun initializeImageArea ()
    (printImage "images/LogoPractica.bmp" IMAGE_AREA_X IMAGE_AREA_Y IMAGE_AREA_SIZE)
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

(defun printImage (imagen x y dimension)
	(setq fichero (open imagen :direction :input 
	:element-type 'unsigned-byte))
	(setq pixel -17)
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

;visualiza el texto dado en la (linea,columna)dada de la pantalla en modo texto
(defun write (linea columna TEXTO)
	(goto-xy columna linea)
	(princ TEXTO)
)

(defun fillAreaColor (r g b x1 y1 x2 y2)
	(color r g b)
	(paralelepipedo x1 y1 x2 y2)
	(dotimes (i (- y2 y1))
		(move x1 (+ i y1))
		(draw x2 (+ i y1))
	)
	(color 0 0 0)
)

(defun paralelepipedo (x1 y1 x2 y2)
	(move x1 y1)
	(draw x1 y2 x2 y2 x2 y1 x1 y1)
)

(defun printLetter (letra x y)
    (printImage (concatenate 'string "images/" letra "_NB.bmp") x y 20)
)

(defun printWord (palabra x y espaciado)
    (dotimes (i (length palabra))
	(printLetter (string (aref palabra i)) x y)
	(setq x (+ 20 x espaciado))
    )
)