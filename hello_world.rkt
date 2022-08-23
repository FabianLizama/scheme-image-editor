#lang scheme
(define cuad
  (lambda (x)
    (* x x)))

(cuad 5)

(define abs
  (lambda (x)
    (cond ((>= x 0) x)
          (else (* -1 x)))))
(abs (cuad -5))
(abs 7)
"Hola"
#\a
1/4
1.3e27
#f
#t
'(a b c d)
car '(a b c d)
cdr '(a b c d)