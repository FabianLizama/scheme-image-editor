#lang scheme
(provide (all-defined-out))

; TDA pixrgb-d

; Constructor - pixrgb-d
; Esta función construye un pixel de tipo pixrgb-d que está representado con una lista que tiene:
;     el tipo de píxel (representado con un número), las coordenadas del pixel, el color del pixel y su profundidad.
; Dominio: int X int X int [C] X int [C] X int [C] X int "Con C con valores entre 0 y 255"
; Reccorrido: pixrgb-d
(define pixrgb-d (lambda (x y r g b depth) (list 1 x y r g b depth)))

; Selector - getXr
; Función de selección x en para un pixrgb-d
; Dominio: pixrgb-d
; Recorrido: int
(define getXr cadr)

; Selector - getYr
; Función de selección y de un pixrgb-d
; Dominio: pixrgb-d
; Recorrido: int
(define getYr caddr)

; Selector - getDr
; Función de selección del canal d de un pixrgb-d
; Dominio: pixrgb-d
; Recorrido: int
(define getDr (lambda (pix)
  (list-ref pix (- (length pix) 1))))

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

; Pertenencia - pixrgb-d?
; Función que permite determinar si un pixel es de tipo pixrgb-d
; Dominio: [pixbit-d | pixrgb-d | pixhex-d]        Recorrido: boolean
(define pixrgb-d? (lambda (pix)
                    (if (= (car pix) 1) #t #f)))

; Modificador - setR
; Cambia el canal r de un pixrgb-d
; Dominio: int [C] X pixrgb-d "Con C con valores entre 0 y 255"
; Recorrido: pixrgb-d
(define setR (lambda (r pix) (pixrgb-d (getXr pix) (getYr pix) r (getG pix) (getB pix) (getDr pix))))

; Modificador - setG
; Cambia el canal g de un pixrgb-d
; Dominio: int [C] X pixrgb-d "Con C con valores entre 0 y 255"
; Recorrido: pixrgb-d
(define setG (lambda (g pix) (pixrgb-d (getXr pix) (getYr pix) (getR pix) g (getB pix) (getDr pix))))

; Modificador - setB
; Cambia el canal r de un pixrgb-d
; Dominio: int [C] X pixrgb-d "Con C con valores entre 0 y 255"
; Recorrido: pixrgb-d
(define setB (lambda (b pix) (pixrgb-d (getXr pix) (getYr pix) (getR pix) (getG pix) b (getDr pix))))


; Modificador - pixrgb->pixhex
; Función que transforma un pixrgb-d a un pixhex-d
; Dominio: pixrgb-d        Recorrido: pixhex-d
(define pixrgb->pixhex (lambda
                         (pix) ; Argumento
                         (define int->hex (lambda (x)
                                            (let ([quo (quotient x 16)]
                                                  [rem (remainder x 16)])
                                              ; Función auxiliar que transforma un número entero a hexadecimal
                                              (string-append      ; Dominio: int        Recorrido: string
                                               (cond ((< quo 10) (number->string (quotient x 16)))
                                                     ((= 10 quo) "A")
                                                     ((= 11 quo) "B")
                                                     ((= 12 quo) "C")
                                                     ((= 13 quo) "D")
                                                     ((= 14 quo) "E")
                                                     ((= 15 quo) "F"))
                                               (cond ((< rem 10) (number->string (remainder x 16)))
                                                     ((= 10 rem) "A")
                                                     ((= 11 rem) "B")
                                                     ((= 12 rem) "C")
                                                     ((= 13 rem) "D")
                                                     ((= 14 rem) "E")
                                                     ((= 15 rem) "F"))))))
                           (list 2 (getXr pix) (getYr pix) (string-append "#" (int->hex (getR pix)) (int->hex (getG pix)) (int->hex (getB pix))) (getDr pix))))

; Modificador - adjustChannel
; Función que permite ajustar cualquier canal de un pixel pixrgb-d, incluido el canal de profundidad d.
; Puede aplicarse a una imagen con la función edit
; Se asume que la función que modificará el canal produce valores dentro del rango válido.
; Dominio: f1 X f2 X f3 X pixrgb-d
; Recorrido: pixrgb-d

(define adjustChannel (lambda (sel mod f) ;getR setR incCh
                        (lambda (pix)
                          (mod (f (sel pix)) pix))))

; Modificador - invertColorRGB
; Función que permite obtener el color simétricamente
; opuesto en cada canal dentro de un pixel.
; Dominio: pixrgb-d   Rrcorrido: pixrgb-d

(define invertColorRGB (lambda (pix)
                         (pixrgb-d (getXr pix) (getYr pix)
                                   (- 255 (getR pix))
                                   (- 255 (getG pix))
                                   (- 255 (getB pix))
                                   (getDr pix))))

; Modificador - incCh
; Incrementa un canal rgb en uno
; Dominio: int    Recorrido: int
(define incCh (lambda (color)
                       (if (< color 255) (+ color 1) color)))


; Modificador - redCh
; Recude un canal rgb en uno
; Dominio: int    Recorrido: int
(define redCh (lambda (color)
                       (if (> color 0) (- color 1) color)))

; Otras - pixrgb->string
; Transforma un pixrgb-d a string
; Dominio: pixrgb-d        Recorrido: string
(define pixrgb->string (lambda (pix)
                         (define getHex (lambda (pix)
                                          (list-ref pix 3)))
                         (getHex (pixrgb->pixhex pix))))