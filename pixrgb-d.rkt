#lang scheme
(provide (all-defined-out))
(require "pixbit-d.rkt")

; TDA pixrgb-d

; Constructor - pixrgb-d
; Esta función construye un pixel de tipo pixrgb-d que está representado con una lista que tiene:
;     el tipo de píxel (representado con un número), las coordenadas del pixel, el color del pixel y su profundidad.
; Dominio: int X int X int [C] X int [C] X int [C] X int "Con C con valores entre 0 y 255"
; Reccorrido: pixrgb-d
(define pixrgb-d (lambda (x y r g b depth) (list 1 x y r g b depth)))

; Selector - getR
; Función de selección del canal r de un pixrgb-d
; Dominio: pixrgb-d        Recorrido: int
(define getR (lambda (pix) (list-ref pix 3)))

; Selector - getG
; Función de selección del canal g de un pixrgb-d
; Dominio: pixrgb-d        Recorrido: int
(define getG (lambda (pix) (list-ref pix 4)))

; Selector - getB
; Función de selección del canal b de un pixrgb-d
; Dominio: pixrgb-d        Recorrido: int
(define getB (lambda (pix) (list-ref pix 5)))

; Pertenencia - pixhex-d?
; Función que permite determinar si un pixel es de tipo pixrgb-d
; Dominio: [pixbit-d | pixrgb-d | pixhex-d]        Recorrido: boolean
(define pixrgb-d? (lambda (pix)
                    (if (= (car pix) 1) #t #f)))

; Modificador - setR
; Cambia el canal r de un pixrgb-d
; Dominio: int [C] X pixrgb-d "Con C con valores entre 0 y 1"
; Recorrido: pixrgb-d
(define setR (lambda (r pix) (pixrgb-d (getX pix) (getY pix) r (getG pix) (getB pix) (getD pix))))

