#lang scheme
(provide (all-defined-out))
(require "pixbit-d_21081166_LizamaGonzález.rkt")

; TDA pixhex-d

; Constructor - pixhex-d
; Esta función construye un píxel de tipo pixhex-d que está representado con una lista que tiene:
;     el tipo de pixel (representado con un número), las coordenadas del pixel, el color del pixel y su profundidad.
; Dominio: int X int X string X int
; Reccorrido: pixhex-d
(define pixhex-d (lambda (x y hex depth) (list 2 x y hex depth)))

; Selector - getXh
; Función de selección x en para un pixhex-d
; Dominio: pixhex-d
; Recorrido: int
(define getXh cadr)

; Selector - getYh
; Función de selección y de un pixrgb-d
; Dominio: pixrgb-d
; Recorrido: int
(define getYh caddr)

; Selector - getDh
; Función de selección del canal D para un pixhex-d
; Dominio: pixhex-d
; Recorrido: int
(define getDh (lambda (pix)
  (list-ref pix (- (length pix) 1))))


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

; Función pixhex->string
; Transforma un pixhex-d a string
; Dominio: pixhex-d        Recorrido: string
(define pixhex->string (lambda (pix) (getHex pix)))