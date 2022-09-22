#lang scheme
(provide (all-defined-out))
(require "pixbit-d.rkt")

; TDA pixhex-d

; Constructor - pixhex-d
; Esta función construye un píxel de tipo pixhex-d que está representado con una lista que tiene:
;     el tipo de pixel (representado con un número), las coordenadas del pixel, el color del pixel y su profundidad.
; Dominio: int X int X string X int
; Reccorrido: pixhex-d
(define pixhex-d (lambda (x y hex depth) (list 2 x y hex depth)))

; Selector - getHex
; Función de selección del canal hex de un pixhex-d
; Dominio: pixhex-d       Recorrido: string
(define getHex (lambda (pix)
                 (list-ref pix 3)))

; Pertenencia - pixhex-d?
; Función que permite determinar si un píxel es de tipo pixhex-d
; Dominio: [pixbit-d | pixrgb-d | pixhex-d]        Recorrido: boolean
(define pixhex-d? (lambda (pix)
                    (if (= (car pix) 2) #t #f)))

