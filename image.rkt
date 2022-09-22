#lang scheme
(provide (all-defined-out))

; TDA image
; El TDA image está representado por una lista que tiene:
;     el color más frecuente inicializado en -1 (este cambia cuando se aplica la funciónc compress), el ancho, el alto y la lista de pixeles


; Constructor - image-list
; Función constructora secundaria necesaria para la implementación de otras funciones
; Dominio: [int|string] X int x int x list
; Recorrido: image
(define (image-list color w h l) (list color w h l))

; Selector - getColor
; Función que obtiene el color más frecuente de una image en hexadecimal si ya ha sido comprimida, si no entrega un -1
; Dominio: image        Recorrido: [int|string]
(define getColor car)

; Selector - getW
; Función que selecciona el ancho de una imagen
; Dominio: image        Recorrido: int
(define getW (lambda (pic) (list-ref pic 1)))

; Selector - getH
; Función que selecciona el alto de una imagen
; Dominio: image       Recorrido: int
(define getH (lambda (pic) (list-ref pic 2)))

; Selector - get-pixlist
; Función que selecciona la lista de pixeles de una imagen
; Dominio: image        Recorrido: list
(define get-pixlist (lambda (pic) (list-ref pic 3)))

