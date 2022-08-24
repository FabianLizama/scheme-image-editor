#lang scheme

(require "funciones.rkt")

; Se definen los TDAs necesarios para el programa

; TDA pixbit-d corresponde a un pixel de tipo bit con sus coordenadas y profundidad
(define-struct pixbit-d (x y bit depth))

; TDA pixrgb-d corresponde a un pixel de tipo rgb con sus coordenadas y profundidad
(define-struct pixrgb-d (x y r g b depth))

; TDA pixhex-d corresponde a un pixel de tipo hexadecimal con sus coordenadas y profundidad
(define-struct pixhex-d (x y hex depth))

; TDA picture corresponde a una imagen con su largo ancho y lista de pixeles
(define-struct picture (width height pixels))

; Funciones

; Función de selección x en común para pixbit-d pixrgb-d y pixhex-d
(define (get-x pix)
  (cond ((pixbit-d? pix) (pixbit-d-x pix))
        ((pixrgb-d? pix) (pixrgb-d-x pix))
        ((pixhex-d? pix) (pixhex-d-x pix))))

; Función de selección y  en común para pixbit-d pixrgb-d y pixhex-d
(define (get-y pix)
  (cond ((pixbit-d? pix) (pixbit-d-y pix))
        ((pixrgb-d? pix) (pixrgb-d-y pix))
        ((pixhex-d? pix) (pixhex-d-y pix))))

; Funciones principales
; TDA image - constructor
(define (image x y . l)
  (make-picture x y l))

; TDA image - bitmap?
(define (bitmap? pic)
  (pixbit-d? (car(picture-pixels pic))))

; TDA image - pixmap?
(define (pixmap? pic)
  (pixrgb-d? (car(picture-pixels pic))))

; TDA image - hexmap?
(define (hexmap? pic)
  (pixhex-d? (car(picture-pixels pic))))



; ejemplos
(define pix (pixbit-d 4 3 2 1))
(define ejimageb (image 2 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 1 0 0 30) (pixbit-d 1 1 1 4)))
(define ejimager (image 1 1 (pixrgb-d 1 1 1 1 1 1)))
(define ejimageh (image 1 1 (pixhex-d 1 1 "#FF0000" 1)))