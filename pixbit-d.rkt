#lang scheme
(provide (all-defined-out))

; Selector - getX
; Función de selección x en común para pixbit-d pixrgb-d y pixhex-d
; Dominio: [pixbit-d | pixrgb-d | pixhex-d]
; Recorrido: int
(define getX cadr)

; Selector - getY 
; Función de selección y en común para pixbit-d pixrgb-d y pixhex-d
; Dominio: [pixbit-d | pixrgb-d | pixhex-d]
; Recorrido: int
(define getY caddr)

; Selector - getD
; Función de selección y en común para pixbit-d pixrgb-d y pixhex-d
; Dominio: [pixbit-d | pixrgb-d | pixhex-d]
; Recorrido: int
(define getD (lambda (pix)
  (list-ref pix (- (length pix) 1))))

; TDA pixbit-d

; Constructor - pixbit-d
; Esta función construye un pixel de tipo pixbit-d que está representado con una lista que tiene:
;     El tipo de píxel (representado con un número), las coordenadas del pixel, el bit del pixel y su profundidad.
; Dominio: int X int X int [0|1] X int
; Reccorrido: pixbit-d
(define pixbit-d (lambda (x y bit depth) (list 0 x y bit depth)))


; Selector - getBit
; Función selectora del canal "bit" del tda pixbit-d
; Dominio: pixbit-d        Recorrido: int [0|1]
(define getBit (lambda (pix) (list-ref pix 3)))

; Pertenencia - pixbit-d?
; Función que permite determinar si un pixel es de tipo pixbit-d
; Dominio: [pixbit-d | pixrgb-d | pixhex-d]        Recorrido: boolean
(define pixbit-d? (lambda (pix) (if (= (car pix) 0) #t #f)))







; Otras - invertColorBit
; Función que permite invierte el valor del bit de un pixbit-d
; Dominio: pixbit-d        Recorrido: pixbit-d

(define invertColorBit (lambda (pix)
                         (if (= 0 (getBit pix))
                             (pixbit-d (getX pix) (getY pix) 1 (getD pix))
                             (pixbit-d (getX pix) (getY pix) 0 (getD pix)))))

; Otras - pixbit->string
; Transforma un pixbit-d a string
; Dominio: pixbit-d        Recorrido: string
(define pixbit->string (lambda (pix) (number->string (getBit pix))))




