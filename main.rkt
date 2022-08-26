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

; Esta función calcula el largo de una lista
; Dominio: l (list) Recorrido: largo (int)
(define (len l) (length l))

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
(define (get-x pix)
  (cadr pix))

; Función de selección x en común para pixbit-d pixrgb-d y pixhex-d
; Dominio: [pixbit-d | pixrgb-d | pixhex-d]
; Recorrido: y (int)
(define (get-y pix)
  (caddr pix))



; Funciones principales

; El TDA image está representado por una lista que tiene: el color más frecuente inicializado en -1, el ancho, el alto y la lista de pixeles
; TDA image - constructor: Función constructora de imágenes con bitmaps o pixmaps que incluye información de profundidad de cada pixel.
; Dominio: Width (int) X Height (int) X [pixbit-d | pixrgb-d | pixhex-d]
; Recorrido: image

(define (image w h . l) (list -1 w h l))

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
(define (pixmap? pic) (pixrgb-d? (car (list-ref pic 3))))

; TDA image - hexmap?
; Función que permite determinar si la imagen corresponde a un hexmap-d
; Dominio: image Recorrido: boolean
(define (hexmap? pic) (pixhex-d? (car (list-ref pic 3))))

; TDA image - compressed?
; Función que determina si una imagen está comprimida
; Dominio: image Recorrido: boolean
(define (compressed? pic)
  (cond ((= (* (get-w pic) (get-h pic)) (len (get-pixlist pic))) #f)
        (else #t)))

; TDA image - flipH
; Función que permite invertir una imágen horizontalmente
; Se encuentra la función matemática para invertir un pixel horizontalmente (|y - (ancho-1)|)
(define (flipH pic)
  (image (get-w pic) (get-h pic) ; Se define el mismo largo y ancho de la imagen original
         (mod-y-map mod-y        ; Se modifican las coordenadas "y" de la lista de pixeles de la imagen
                    (fH-pix-map flipH-pix (map get-y (get-pixlist pic)) (get-w pic)) ; Se calculan las coordenadas "y" invertidas horizontalmente
                    (get-pixlist pic)))) ; Obtiene la lista de pixeles para realizar las tareas de las dos lineas anteriores

; Esta función calcula el valor que debería tener el "y" de un pixel al rotarlo horizontalmente
; Dominio: y (int) X width (int)
; Recorrido: y (int)
(define (flipH-pix y w)
  (abs (- y (- w 1))))

; Esta función recursiva está diseñada para aplicar la función flipH-pix a una lista de coordenadas "y" de una lista de pixeles
; Dominio: fx (flipH-pix) X lista (list) X width (int)
; Recorrido: lista (list) -> "lista de coordenadas 'y' recalculadas"
(define (fH-pix-map fx l w)
  (if (null? l) null
      (cons (fx (car l) w) (fH-pix-map fx (cdr l) w))))


; Esta función modifica el valor y de un pixel
; Dominio: y (int) X [pixbit-d | pixrgb-d | pixhex-d]
; Recorrido: [pixbit-d | pixrgb-d | pixhex-d]
(define (mod-y y pix)
  (cond ((= 0 (car pix)) (pixbit-d (get-x pix) y (list-ref pix 3) (list-ref pix 4)))
        ((= 1 (car pix)) (pixrgb-d (get-x pix) y (list-ref pix 3) (list-ref pix 4) (list-ref pix 5) (list-ref pix 6)))
        ((= 2 (car pix)) (pixhex-d (get-x pix) y (list-ref pix 3) (list-ref pix 4)))))

; Esta función recursiva está diseñada para aplicar la función mod-y a una lista de pixeles pertenecientes a una imagen
; Dominio: fx (mod-y) X lista (list) X pixeles (list)   " 'lista' contiene una lista de las coordenadas 'y' recalculadas"
; Recorrido: [pixbit-d | pixrgb-d | pixhex-d] (list)    "lista de pixeles invertidos horizontalmente"
(define (mod-y-map fx l pixels)
  (if (null? l) null
      (cons (fx (car l) (car pixels)) (mod-y-map fx (cdr l) (cdr pixels)))))







; ejemplos
(define pix (pixbit-d 4 3 1 1))
(define ejimageb (image 2 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 1 0 0 30) (pixbit-d 1 1 0 30)))
(define ejimager (image 3 3
                        (pixrgb-d 0 0 255 0 0 0) (pixrgb-d 0 1 0 0 0 0) (pixrgb-d 0 2 0 0 255 0)
                        (pixrgb-d 1 0 255 0 0 1) (pixrgb-d 1 1 0 0 0 1) (pixrgb-d 1 2 0 0 255 1)
                        (pixrgb-d 2 0 255 0 0 2) (pixrgb-d 1 1 0 0 0 2) (pixrgb-d 2 2 0 0 255 2)))

(define ejimageh (image 1 1 (pixhex-d 1 1 "#FF0000" 1)))