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
  (car (cdr pix)))

; Función de selección x en común para pixbit-d pixrgb-d y pixhex-d
; Dominio: [pixbit-d | pixrgb-d | pixhex-d]
; Recorrido: y (int)
(define (get-y pix)
  (car (cdr (cdr pix))))


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
(define (get-h pic) (car (list-ref pic 2)))

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

; ejemplos
(define pix (pixbit-d 4 3 1 1))
(define ejimageb (image 2 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 1 0 0 30)))
(define ejimager (image 1 1 (pixrgb-d 1 1 1 1 1 1)))
(define ejimageh (image 1 1 (pixhex-d 1 1 "#FF0000" 1)))