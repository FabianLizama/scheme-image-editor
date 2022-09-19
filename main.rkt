#lang scheme

(require "funciones.rkt")

; Se definen los TDAs necesarios para el programa

; TDA pixbit-d - constructor
; Esta función construye un pixel de tipo pixbit-d que está representado con una lista que tiene:
;     el tipo de pixel (representado con un número), las coordenadas del pixel, el bit del pixel y su profundidad.
; Dominio: x (int) X y (int) X bit [0|1] X depth (int)
; Reccorrido: pixbit-d
(define (pixbit-d x y bit depth) (list 0 x y bit depth))

; TDA pixrgb-d - constructor
; Esta función construye un pixel de tipo pixrgb-d que está representado con una lista que tiene:
;     el tipo de pixel (representado con un número), las coordenadas del pixel, el color del pixel y su profundidad.
; Dominio: x (int) X y (int) X r (C) X g (C) X b (C) X depth (int) "Con C con valores entre 0 y 255"
; Reccorrido: pixrgb-d
(define (pixrgb-d x y r g b depth) (list 1 x y r g b depth))

; TDA pixhex-d - constructor
; Esta función construye un pixel de tipo pixhex-d que está representado con una lista que tiene:
;     el tipo de pixel (representado con un número), las coordenadas del pixel, el color del pixel y su profundidad.
; Dominio: x (int) X y (int) X hex (String) X depth (int)
; Reccorrido: pixhex-d
(define (pixhex-d x y hex depth) (list 2 x y hex depth))

; Funciones

(define (get-bit pix) (list-ref pix 3))

; Esta función calcula el largo de una lista
; Dominio: l (list) Recorrido: largo (int)
(define len length)

; Función que permite determinar si un pixel es de tipo pixbit-d
; Dominio: [pixbit-d | pixrgb-d | pixhex-d] Recorrido: boolean
(define (pixbit-d? pix)
  (if (= (car pix) 0) #t #f))

; Función que permite determinar si un pixel es de tipo pixrgb-d
; Dominio: [pixbit-d | pixrgb-d | pixhex-d] Recorrido: boolean
(define (pixrgb-d? pix)
  (if (= (car pix) 1) #t #f))

; Función que permite determinar si un pixel es de tipo pixhex-d
; Dominio: [pixbit-d | pixrgb-d | pixhex-d] Recorrido: boolean
(define (pixhex-d? pix)
  (if (= (car pix) 2) #t #f))

; Función de selección x en común para pixbit-d pixrgb-d y pixhex-d
; Dominio: [pixbit-d | pixrgb-d | pixhex-d]
; Recorrido: x (int)
(define get-x cadr)

; Función de selección x en común para pixbit-d pixrgb-d y pixhex-d
; Dominio: [pixbit-d | pixrgb-d | pixhex-d]
; Recorrido: y (int)
(define get-y caddr)

(define (get-d pix)
  (list-ref pix (- (len pix) 1)))

(define (get-hex pix)
  (list-ref pix 3))

(define (get-r pix) (list-ref pix 3))
(define (get-g pix) (list-ref pix 4))
(define (get-b pix) (list-ref pix 5))


; Funciones principales

; El TDA image está representado por una lista que tiene: el color más frecuente inicializado en -1, el ancho, el alto y la lista de pixeles
; TDA image - constructor: Función constructora de imágenes con bitmaps o pixmaps que incluye información de profundidad de cada pixel.
; Dominio: Width (int) X Height (int) X [pixbit-d | pixrgb-d | pixhex-d]
; Recorrido: image

(define (image w h . l) (list -1 w h l))

(define (image-list color w h l) (list color w h l))

; Función que selecciona el ancho de una imagen
; Dominio: image Recorrido: Width (int)
(define (get-w pic) (list-ref pic 1))

; Función que selecciona el alto de una imagen
; Dominio: image Recorrido: Height (int)
(define (get-h pic) (list-ref pic 2))

; Función que selecciona la lista de pixeles de una imagen
; Dominio: image Recorrido: pixels (list)
(define (get-pixlist pic) (list-ref pic 3))

; TDA image - bitmap?
; Función que permite determinar si la imagen corresponde a un bitmap-d
; Dominio: image Recorrido: boolean
(define (bitmap? pic) (pixbit-d? (car (list-ref pic 3))))

; TDA image - pixmap?
; Función que permite determinar si la imagen corresponde a un pixmap-d
; Dominio: image Recorrido: boolean
(define (pixmap? pic) (pixrgb-d? (car (get-pixlist pic))))

; TDA image - hexmap?
; Función que permite determinar si la imagen corresponde a un hexmap-d
; Dominio: image Recorrido: boolean
(define (hexmap? pic) (pixhex-d? (car (get-pixlist pic))))

; TDA image - compressed?
; Función que determina si una imagen está comprimida
; Dominio: image Recorrido: boolean
(define (compressed? pic)
  (cond ((= (* (get-w pic) (get-h pic)) (len (get-pixlist pic))) #f)
        (else #t)))

; TDA image - flipH
; Función que permite invertir una imágen horizontalmente
; Se encuentra la función matemática para invertir un pixel horizontalmente (|x - (ancho-1)|)
(define (flipH pic)
  (image (get-w pic) (get-h pic) ; Se define el mismo largo y ancho de la imagen original
         (recur-flip flipH-pix (get-pixlist pic) (get-w pic)))) ; Invierte la coordenada "x" de cada pixel de la lista de pixeles

; Esta función modifica el valor y de un pixel
; Dominio: y (int) X [pixbit-d | pixrgb-d | pixhex-d]
; Recorrido: [pixbit-d | pixrgb-d | pixhex-d]
(define (mod-y y pix)
  (cond ((= 0 (car pix)) (pixbit-d (get-x pix) y (list-ref pix 3) (list-ref pix 4)))
        ((= 1 (car pix)) (pixrgb-d (get-x pix) y (list-ref pix 3) (list-ref pix 4) (list-ref pix 5) (list-ref pix 6)))
        ((= 2 (car pix)) (pixhex-d (get-x pix) y (list-ref pix 3) (list-ref pix 4)))))

(define (mod-x x pix)
  (cond ((= 0 (car pix)) (pixbit-d x (get-y pix) (list-ref pix 3) (list-ref pix 4)))
        ((= 1 (car pix)) (pixrgb-d x (get-y pix) (list-ref pix 3) (list-ref pix 4) (list-ref pix 5) (list-ref pix 6)))
        ((= 2 (car pix)) (pixhex-d x (get-y pix) (list-ref pix 3) (list-ref pix 4)))))

; Esta función recursiva está diseñada para aplicar la función flipH-pix a una lista de pixeles, aplica la recursión de cola
; Dominio: fx ([flipH-pix | flipV-pix]) X pixels (list) X width (int)
; Recorrido: pixels (list)
(define (recur-flip fx pixels w)
  (if (null? pixels) null
      (cons (fx (car pixels) w) (recur-flip fx (cdr pixels) w))))


; Esta función ajusta la posición de un pixel otorgandole la coordenada "x" correspondiente después de invertirlo horizontalmente
; Dominio: pix ([pixbit-d | pixrgb-d | pixhex-d]) X width (int)
; Recorrido: pix ([pixbit-d | pixrgb-d | pixhex-d])
(define (flipH-pix pix w)
  (mod-x (abs (- (get-x pix) (- w 1))) pix))



; TDA image - flipV
; Función que permite invertir una imágen verticalmente
; Se encuentra la función matemática para invertir un pixel verticalmente (|x - (ancho-1)|)
(define (flipV pic)
  (image (get-w pic) (get-h pic) ; Se define el mismo largo y ancho de la imagen original
         (recur-flip flipV-pix (get-pixlist pic) (get-w pic)))) ; Invierte la coordenada "x" de cada pixel de la lista de pixeles


; Esta función ajusta la posición de un pixel otorgandole la coordenada "x" correspondiente después de invertirlo verticalmente
; Dominio: pix ([pixbit-d | pixrgb-d | pixhex-d]) X width (int)
; Recorrido: pix ([pixbit-d | pixrgb-d | pixhex-d])
(define (flipV-pix pix w)
  (mod-y (abs (- (get-y pix) (- w 1))) pix))


; TDA image - crop: Recortar una imagen a partir de un cuadrante
; Dominio: image X x1 (int) X y1 (int) X x2 (int) X y2 (int)
; Recorrido: image
(define (crop pic x1 y1 x2 y2)
  (image-list (if (compressed? pic) (car pic) -1) (+ (- x2 x1) 1) (+ (- y2 y1) 1) (map (lambda (pix) (mod-y (- (get-y pix) y1) (mod-x (- (get-x pix) x1) pix))) (myfilter condi (get-pixlist pic) x1 y1 x2 y2))))

(define (myfilter condition list x1 y1 x2 y2)
  (if (null? list) null
      (if (condi (car list) x1 y1 x2 y2)
          (cons (car list) (myfilter condi (cdr list) x1 y1 x2 y2))
          (myfilter condi (cdr list) x1 y1 x2 y2)
          )))

(define (condi pix x1 y1 x2 y2)
  (if (and (>= (get-x pix) x1) (<= (get-x pix) x2) (>= (get-y pix) y1) (<= (get-y pix) y2)) #t #f))
         


(define (imgRGB->imgHex pic)
  (image (get-w pic) (get-h pic) (map pixrgb->pixhex (get-pixlist pic))))

(define (pixrgb->pixhex pix)
  (list 2 (get-x pix) (get-y pix) (string-append "#" (int->hex (get-r pix)) (int->hex (get-g pix)) (int->hex (get-b pix))) (get-d pix)))

(define (int->hex x)
  (string-append
   (cond ((< (quotient x 16) 10) (number->string (quotient x 16)))
         ((= 10 (quotient x 16)) "A")
         ((= 11 (quotient x 16)) "B")
         ((= 12 (quotient x 16)) "C")
         ((= 13 (quotient x 16)) "D")
         ((= 14 (quotient x 16)) "E")
         ((= 15 (quotient x 16)) "F"))
   (cond ((< (remainder x 16) 10) (number->string (remainder x 16)))
         ((= 10 (remainder x 16)) "A")
         ((= 11 (remainder x 16)) "B")
         ((= 12 (remainder x 16)) "C")
         ((= 13 (remainder x 16)) "D")
         ((= 14 (remainder x 16)) "E")
         ((= 15 (remainder x 16)) "F"))))

; Esta función obtiene el código hexadecimal de cualquier pixel
(define (pix->hex pix)
  (cond((pixbit-d? pix) (if (zero? (get-bit pix)) "#000000" "#FFFFFF"))
        ((pixhex-d? pix) (get-hex pix))
        ((pixrgb-d? pix) (get-hex (pixrgb->pixhex pix)))))

; TDA image - histogram
; Retorna una lista que tiene de primer elemento el número de pixeles totales,
; y luego pares que están conformados por el código del color y la cantidad de pixeles que poseen ese color
; Dominio: image    Recorrido: [n (int) X ["#XXXXXX" (string) . x (int)] (pair)] (list)  
(define (histogram pic)
  (recur-hist (get-pixlist pic) (list 0) (* (get-w pic) (get-h pic))))

; Función recursiva de cola que aplica la función count-pix a toda una lista de pixeles
(define (recur-hist pixels histlist n)
  (if (= (car histlist) n) histlist
      (recur-hist (cdr pixels) (count-pix (car pixels) histlist) n)))


; Función que cuenta un pixel y lo agrega al histograma entregado
(define (count-pix pix histlist)
  (if (null? (findHex histlist (pix->hex pix)))
      (append (append (list (+ (car histlist) 1)) (cdr histlist)) (list(cons (pix->hex pix) 1)))
      (append
       (append (list (+ (car histlist) 1)) (cdr (remove (car (findHex histlist (pix->hex pix))) histlist))
       (list (cons (caar (findHex histlist (pix->hex pix))) (+ 1 (cdar (findHex histlist (pix->hex pix))))))))))

; Función que encuentra el par que posee el color entregado
(define (findHex histlist hexcode)
  (filter (compareHex hexcode) (cdr histlist)))

; Función que compara que si dos códigos hexadecimales son iguales
(define compareHex(lambda (hex1)
                     (lambda (elem)
                       (if (string=? hex1 (car elem)) #t #f))))


; Función Rotate90
; Función que rota en 90° un pixel
; Recursión de cola que va rotando los anillos de pixeles exteriores
; Dominio: image (pic)
; Recorrido: image

; Llamada recursiva
(define rotate90 (lambda (pic) (recur-rotate90 pic pic '() (get-pixlist pic))))


; Función recursiva principal
(define recur-rotate90 (lambda
                         (root-image mod-image readypixs remainpixs)
                         (cond ((null? remainpixs) (image-list -1 (get-y root-image) (get-x root-image) readypixs))
                             ((= 1 (len remainpixs)) (image-list -1 (get-y root-image) (get-x root-image) (append readypixs remainpixs))) ; Condiciones de borde (si la imagen es par o impar)
                             ((null? readypixs) (recur-rotate90
                                                 root-image
                                                 (crop mod-image 1 1 (- (get-x mod-image) 1) (- (get-y mod-image) 1))
                                                 (map (pix90 (get-w mod-image) (get-h mod-image)) (filter (ring? (get-w mod-image) (get-h mod-image)) remainpixs))
                                                 (filter (notring? (get-w mod-image) (get-h mod-image)) remainpixs)))
                             
                             (else (recur-rotate90     ; La recursión funciona con una lista de pixeles rotados y una lista de pixeles por rotar, esta para cuando ya no quedan pixeles por rotar
                                                 root-image
                                                 (crop mod-image 1 1 (- (get-x mod-image) 1) (- (get-y mod-image) 1))
                                                 (append readypixs (map pixsum1 (map (pix90 (get-w mod-image) (get-h mod-image)) (filter (ring? (get-w mod-image) (get-h mod-image)) remainpixs))))
                                                 (map pixsum1 (filter (notring? (get-w mod-image) (get-h mod-image)) remainpixs)))))))


; Función que suma 1 a los pixeles para recuperar las coordenadas originales después de aplicar el crop
(define pixsum1 (lambda (pix) (mod-x (+ 1 (get-x pix)) (mod-y (+ 1 (get-y pix)) pix))))

; Función que rota el pixel en 90 grados dependiendo del ancho y largo de la imagen
(define pix90 (lambda (w h)
                (lambda (pix)
                  (cond ((= (get-x pix) 0) (mod-x (- (- h 1) (get-y pix)) (mod-y 0 pix)))
                        ((= (get-x pix) (- w 1)) (mod-x (- (- h 1) (get-y pix)) (mod-y (- w 1) pix)))
                        ((= (get-y pix) 0) (mod-x (- h 1) (mod-y (get-x pix) pix)))
                        ((= (get-y pix) (- h 1)) (mod-x 0 (mod-y (get-x pix) pix)))))))

; Función que comprueba si un pixel es parte del anillo exterior de la imagen
(define ring? (lambda (w h)
                (lambda (pix)
                (if (or (= (get-x pix) 0) (= (get-y pix) 0) (= (get-x pix) (- w 1)) (= (get-y pix) (- h 1))) #t #f))))

; Función que comprueba si un pixel no es parte del anillo exterior de la imagen
(define notring? (lambda (w h)
                (lambda (pix)
                (if (or (= (get-x pix) 0) (= (get-y pix) 0) (= (get-x pix) (- w 1)) (= (get-y pix) (- h 1))) #f #t))))


; TDA image - compress
; Función que comprime una imágen eliminando aquellos pixeles con el color más frecuente.
; La imagen comprimida resultante solo se puede manipular con las otras funciones una vez que haya sido descomprimida con la función decompress.
; Dominio: image     Recorrido: image

(define compress (lambda (pic) (image-list (car (max-h pic)) (get-w pic) (get-h pic) (filter (diff-color? (car (max-h pic))) (get-pixlist pic)))))


; Función que encuentra el color más frecuente de una imagen
; Dominio: image
; Recorrido: string X int
(define max-h (lambda (pic)
               (define max-rec (lambda (hist max)
                                (if (null? hist)
                                    max
                                    (if (> (cdar hist) (cdr max))
                                        (max-rec (cdr hist) (car hist))
                                        (max-rec (cdr hist) max)))))
                (max-rec (cdr (histogram pic)) (cadr (histogram pic)))))

; Función que compara un color y un pixel para ver si son colores distintos
; Dominio: string X [pixbit-d | pixrgb-d | pixhex-d]
; Recorrido: bool
(define diff-color? (lambda (color)
                     (lambda (pix)
                       (if (equal? (pix->hex pix) color) #f #t))))

; TDA image - edit
; Permite aplicar funciones especiales a las imágenes. Por ejemplo, para modificar colores en alguno de los canales, pasar a blanco y negro, etc.
; Dominio: f X image
; Recorrido: image

(define edit (lambda (f pic)
               (image-list -1 (get-w pic) (get-h pic) (map f (get-pixlist pic)))))

; TDA image - invertColorBit
; Función que permite obtener el valor del bit opuesto
; Dominio: pixbit-d
; Recorrido: pixbit-d

(define invertColorBit (lambda (pix)
                         (if (= 0 (get-bit pix))
                             (pixbit-d (get-x pix) (get-y pix) 1 (get-d pix))
                             (pixbit-d (get-x pix) (get-y pix) 0 (get-d pix)))))

; TDA image - invertColorRGB
; Función que permite obtener el color simétricamente
; opuesto en cada canal dentro de un pixel.
; Dominio: pixrgb-d   Rrcorrido: pixrgb-d

(define invertColorRGB (lambda (pix)
                         (pixrgb-d (get-x pix) (get-y pix)
                                   (- 255 (get-r pix))
                                   (- 255 (get-g pix))
                                   (- 255 (get-b pix))
                                   (get-d pix))))

; TDA image - adjustChannel
; Función que permite ajustar cualquier canal de una imagen con pixeles pixrgb-d, incluido el canal de profundidad d.
; Se asume que la función que modificará el canal produce valores dentro del rango válido.
; Dominio: f1 X f2 X f3 X pixrgb-d
; Recorrido: pixrgb-d

(define adjustChannel (lambda (f1 f2 f3) ;getR setR incCh
                        (lambda (pix)
                          (pixrgb-d (get-x pix) (get-y pix) )))


; ejemplos
(define pix (pixbit-d 4 3 1 1))
(define pixh (pixhex-d 0 0 "#FFFFFF" 1))
(define ejimageb (image 2 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 1 0 0 30) (pixbit-d 1 1 0 30)))
(define ejimager (image 3 3
                        (pixrgb-d 0 0 255 0 0 0) (pixrgb-d 0 1 255 0 0 0) (pixrgb-d 0 2 0 0 255 0)
                        (pixrgb-d 1 0 255 0 0 1) (pixrgb-d 1 1 255 0 0 1) (pixrgb-d 1 2 0 0 255 1)
                        (pixrgb-d 2 0 255 0 0 2) (pixrgb-d 2 1 255 0 0 2) (pixrgb-d 2 2 0 0 255 2)))

(define ejimageh (image 1 1 (pixhex-d 1 1 "#FF0000" 1)))