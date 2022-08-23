#lang scheme

(require "funciones.rkt")

(define-struct pixbit-d (x y bit depth))
        

(define-struct pixrgb-d (x y r g b depth))

(define-struct pixhex-d (x y hex depth))

(define-struct picture (width length pixels))

;funciones

(define (get-x pix)
  (cond ((pixbit-d? pix) (pixbit-d-x pix))
        ((pixrgb-d? pix) (pixrgb-d-x pix))
        ((pixhex-d? pix) (pixhex-d-x pix))))

(define (get-y pix)
  (cond ((pixbit-d? pix) (pixbit-d-y pix))
        ((pixrgb-d? pix) (pixrgb-d-y pix))
        ((pixhex-d? pix) (pixhex-d-y pix))))

(define (makelist . args) args)



(define (image x y . l)
  (make-picture x y (makelist l)))

; ejemplos
(define pix (pixbit-d 4 3 2 1))

;(define example (picture 2 2 (list pix pix)))

;(pixbit-d-x (car (picture-pixels example)))

;(image 2 2 (list pix pix))