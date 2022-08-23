#lang scheme

(provide suma)
(provide resta)

(define (suma x y)
  (+ x y))
(define (resta x y)
  (- x y))

(define (abs x)
  (cond (> x 0) (x)
        (< x 0) (* -1 x)
        (else (0))))