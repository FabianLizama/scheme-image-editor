#lang scheme
(require "funciones.rkt")
(require "pixbit-d.rkt")
(require "pixrgb-d.rkt")
(require "pixhex-d.rkt")
(require "image.rkt")

;Funciones en común


; TDA image - constructor
; Función constructora de imágenes de tipo bitmap, pixmap o hexmap que incluye información de profundidad de cada pixel.
; Dominio: int X int X [pixbit-d | pixrgb-d | pixhex-d]
; Recorrido: image
(define image (lambda (w h . l) (list -1 w h l)))

; TDA image - bitmap?
; Función que permite determinar si la imagen corresponde a un bitmap-d
; Dominio: image        Recorrido: boolean
(define bitmap? (lambda (pic) (pixbit-d? (car (get-pixlist pic)))))

; TDA image - pixmap?
; Función que permite determinar si la imagen corresponde a un pixmap-d
; Dominio: image Recorrido: boolean
(define (pixmap? pic) (pixrgb-d? (car (get-pixlist pic))))

; TDA image - hexmap?
; Función que permite determinar si la imagen corresponde a un hexmap-d
; Dominio: image Recorrido: boolean
(define (hexmap? pic) (pixhex-d? (car (get-pixlist pic))))

; TDA image - compressed?
; Función que determina si una imagen está comprimida
; Dominio: image Recorrido: boolean
(define (compressed? pic)
  (cond ((= (* (getW pic) (getH pic)) (length (get-pixlist pic))) #f)
        (else #t)))

; TDA image - flipH
; Función que permite invertir una imágen horizontalmente
; Se encuentra la función matemática para invertir un pixel horizontalmente (|x - (ancho-1)|)
(define (flipH pic)
  (image (getW pic) (getH pic) ; Se define el mismo largo y ancho de la imagen original
         (recur-flip flipH-pix (get-pixlist pic) (getW pic)))) ; Invierte la coordenada "x" de cada pixel de la lista de pixeles

; Modificador - setY
; Esta función modifica el valor y de un pixel
; Dominio: int X [pixbit-d | pixrgb-d | pixhex-d]
; Recorrido: [pixbit-d | pixrgb-d | pixhex-d]
(define setY (lambda (y pix)
               (cond ((pixbit-d? pix) (pixbit-d (getX pix) y (getBit pix) (getD pix)))
                     ((pixrgb-d? pix) (pixrgb-d (getX pix) y (getR pix) (getG pix) (getB pix) (getD pix)))
                     ((pixhex-d? pix) (pixhex-d (getX pix) y (getHex pix) (getD pix))))))
; Modificador - setX
; Esta función modifica el valor x de un pixel
; Dominio: int X [pixbit-d | pixrgb-d | pixhex-d]
; Recorrido: [pixbit-d | pixrgb-d | pixhex-d]
(define setY (lambda (y pix)
               (cond ((pixbit-d? pix) (pixbit-d x (getY pix) (getBit pix) (getD pix)))
                     ((pixrgb-d? pix) (pixrgb-d x (getY pix) (getR pix) (getG pix) (getB pix) (getD pix)))
                     ((pixhex-d? pix) (pixhex-d x (getY pix) (getHex pix) (getD pix))))))

; Esta función recursiva está diseñada para aplicar la función flipH-pix a una lista de pixeles, aplica la recursión de cola
; Dominio: fx ([flipH-pix | flipV-pix]) X pixels (list) X width (int)
; Recorrido: pixels (list)
(define (recur-flip fx pixels w)
  (if (null? pixels) null
      (cons (fx (car pixels) w) (recur-flip fx (cdr pixels) w))))


; Esta función ajusta la posición de un pixel otorgandole la coordenada "x" correspondiente después de invertirlo horizontalmente
; Dominio: pix ([pixbit-d | pixrgb-d | pixhex-d]) X width (int)
; Recorrido: pix ([pixbit-d | pixrgb-d | pixhex-d])
(define (flipH-pix pix w)
  (setX (abs (- (getX pix) (- w 1))) pix))



; TDA image - flipV
; Función que permite invertir una imágen verticalmente
; Se encuentra la función matemática para invertir un pixel verticalmente (|x - (ancho-1)|)
(define (flipV pic)
  (image (getW pic) (getH pic) ; Se define el mismo largo y ancho de la imagen original
         (recur-flip flipV-pix (get-pixlist pic) (getW pic)))) ; Invierte la coordenada "x" de cada pixel de la lista de pixeles


; Esta función ajusta la posición de un pixel otorgandole la coordenada "x" correspondiente después de invertirlo verticalmente
; Dominio: pix ([pixbit-d | pixrgb-d | pixhex-d]) X width (int)
; Recorrido: pix ([pixbit-d | pixrgb-d | pixhex-d])
(define (flipV-pix pix w)
  (setY (abs (- (getY pix) (- w 1))) pix))


; TDA image - crop: Recortar una imagen a partir de un cuadrante
; Dominio: image X x1 (int) X y1 (int) X x2 (int) X y2 (int)
; Recorrido: image
(define (crop pic x1 y1 x2 y2)
  (image-list (if (compressed? pic) (car pic) -1) (+ (- x2 x1) 1) (+ (- y2 y1) 1) (map (lambda (pix) (setY (- (getY pix) y1) (setX (- (getX pix) x1) pix))) (myfilter condi (get-pixlist pic) x1 y1 x2 y2))))

(define (myfilter condition list x1 y1 x2 y2)
  (if (null? list) null
      (if (condi (car list) x1 y1 x2 y2)
          (cons (car list) (myfilter condi (cdr list) x1 y1 x2 y2))
          (myfilter condi (cdr list) x1 y1 x2 y2)
          )))

(define (condi pix x1 y1 x2 y2)
  (if (and (>= (getX pix) x1) (<= (getX pix) x2) (>= (getY pix) y1) (<= (getY pix) y2)) #t #f))
         


(define (imgRGB->imgHex pic)
  (image (getW pic) (getH pic) (map pixrgb->pixhex (get-pixlist pic))))

(define (pixrgb->pixhex pix)
  (list 2 (getX pix) (getY pix) (string-append "#" (int->hex (getR pix)) (int->hex (getG pix)) (int->hex (getB pix))) (getD pix)))

(define (int->hex x)
  (string-append
   (cond ((< (quotient x 16) 10) (number->string (quotient x 16)))
         ((= 10 (quotient x 16)) "A")
         ((= 11 (quotient x 16)) "B")
         ((= 12 (quotient x 16)) "C")
         ((= 13 (quotient x 16)) "D")
         ((= 14 (quotient x 16)) "E")
         ((= 15 (quotient x 16)) "F"))
   (cond ((< (remainder x 16) 10) (number->string (remainder x 16)))
         ((= 10 (remainder x 16)) "A")
         ((= 11 (remainder x 16)) "B")
         ((= 12 (remainder x 16)) "C")
         ((= 13 (remainder x 16)) "D")
         ((= 14 (remainder x 16)) "E")
         ((= 15 (remainder x 16)) "F"))))


; Esta función obtiene el código hexadecimal de cualquier pixel
(define (pix->hex pix)
  (cond((pixbit-d? pix) (if (zero? (getBit pix)) "#000000" "#FFFFFF"))
        ((pixhex-d? pix) (getHex pix))
        ((pixrgb-d? pix) (getHex (pixrgb->pixhex pix)))))

; TDA image - histogram
; Retorna una lista que tiene de primer elemento el número de pixeles totales,
; y luego pares que están conformados por el código del color y la cantidad de pixeles que poseen ese color
; Dominio: image    Recorrido: [n (int) X ["#XXXXXX" (string) . x (int)] (pair)] (list)  
(define (histogram pic)
  (recur-hist (get-pixlist pic) (list 0) (* (getW pic) (getH pic))))

; Función recursiva de cola que aplica la función count-pix a toda una lista de pixeles
(define (recur-hist pixels histlist n)
  (if (= (car histlist) n) histlist
      (recur-hist (cdr pixels) (count-pix (car pixels) histlist) n)))


; Función que cuenta un pixel y lo agrega al histograma entregado
(define (count-pix pix histlist)
  (if (null? (findHex histlist (pix->hex pix)))
      (append (append (list (+ (car histlist) 1)) (cdr histlist)) (list(cons (pix->hex pix) 1)))
      (append
       (append (list (+ (car histlist) 1)) (cdr (remove (car (findHex histlist (pix->hex pix))) histlist))
       (list (cons (caar (findHex histlist (pix->hex pix))) (+ 1 (cdar (findHex histlist (pix->hex pix))))))))))

; Función que encuentra el par que posee el color entregado
(define (findHex histlist hexcode)
  (filter (compareHex hexcode) (cdr histlist)))

; Función que compara que si dos códigos hexadecimales son iguales
(define compareHex(lambda (hex1)
                     (lambda (elem)
                       (if (string=? hex1 (car elem)) #t #f))))


; Función Rotate90
; Función que rota en 90° un pixel
; Recursión de cola que va rotando los anillos de pixeles exteriores
; Dominio: image (pic)
; Recorrido: image

; Llamada recursiva
(define rotate90 (lambda (pic) (recur-rotate90 pic pic '() (get-pixlist pic))))


; Función recursiva principal
(define recur-rotate90 (lambda
                         (root-image mod-image readypixs remainpixs)
                         (cond ((null? remainpixs) (image-list -1 (getY root-image) (getX root-image) readypixs))
                             ((= 1 (length remainpixs)) (image-list -1 (getY root-image) (getX root-image) (append readypixs remainpixs))) ; Condiciones de borde (si la imagen es par o impar)
                             ((null? readypixs) (recur-rotate90
                                                 root-image
                                                 (crop mod-image 1 1 (- (getX mod-image) 1) (- (getY mod-image) 1))
                                                 (map (pix90 (getW mod-image) (getH mod-image)) (filter (ring? (getW mod-image) (getH mod-image)) remainpixs))
                                                 (filter (notring? (getW mod-image) (getH mod-image)) remainpixs)))
                             
                             (else (recur-rotate90     ; La recursión funciona con una lista de pixeles rotados y una lista de pixeles por rotar, esta para cuando ya no quedan pixeles por rotar
                                                 root-image
                                                 (crop mod-image 1 1 (- (getX mod-image) 1) (- (getY mod-image) 1))
                                                 (append readypixs (map pixsum1 (map (pix90 (getW mod-image) (getH mod-image)) (filter (ring? (getW mod-image) (getH mod-image)) remainpixs))))
                                                 (map pixsum1 (filter (notring? (getW mod-image) (getH mod-image)) remainpixs)))))))


; Función que suma 1 a los pixeles para recuperar las coordenadas originales después de aplicar el crop
(define pixsum1 (lambda (pix) (setX (+ 1 (getX pix)) (setY (+ 1 (getY pix)) pix))))

; Función que rota el pixel en 90 grados dependiendo del ancho y largo de la imagen
(define pix90 (lambda (w h)
                (lambda (pix)
                  (cond ((= (getX pix) 0) (setX (- (- h 1) (getY pix)) (setY 0 pix)))
                        ((= (getX pix) (- w 1)) (setX (- (- h 1) (getY pix)) (setY (- w 1) pix)))
                        ((= (getX pix) 0) (setX (- h 1) (setY (getX pix) pix)))
                        ((= (getX pix) (- h 1)) (setX 0 (setY (getX pix) pix)))))))

; Función que comprueba si un pixel es parte del anillo exterior de la imagen
(define ring? (lambda (w h)
                (lambda (pix)
                (if (or (= (getX pix) 0) (= (getY pix) 0) (= (getX pix) (- w 1)) (= (getY pix) (- h 1))) #t #f))))

; Función que comprueba si un pixel no es parte del anillo exterior de la imagen
(define notring? (lambda (w h)
                (lambda (pix)
                (if (or (= (getX pix) 0) (= (getY pix) 0) (= (getX pix) (- w 1)) (= (getY pix) (- h 1))) #f #t))))


; TDA image - compress
; Función que comprime una imágen eliminando aquellos pixeles con el color más frecuente.
; La imagen comprimida resultante solo se puede manipular con las otras funciones una vez que haya sido descomprimida con la función decompress.
; Dominio: image     Recorrido: image

(define compress (lambda (pic) (image-list (car (max-h pic)) (getW pic) (getH pic) (filter (diff-color? (car (max-h pic))) (get-pixlist pic)))))


; Función que encuentra el color más frecuente de una imagen
; Dominio: image
; Recorrido: string X int
(define max-h (lambda (pic)
               (define max-rec (lambda (hist max)
                                (if (null? hist)
                                    max
                                    (if (> (cdar hist) (cdr max))
                                        (max-rec (cdr hist) (car hist))
                                        (max-rec (cdr hist) max)))))
                (max-rec (cdr (histogram pic)) (cadr (histogram pic)))))

; Función que compara un color y un pixel para ver si son colores distintos
; Dominio: string X [pixbit-d | pixrgb-d | pixhex-d]
; Recorrido: bool
(define diff-color? (lambda (color)
                     (lambda (pix)
                       (if (equal? (pix->hex pix) color) #f #t))))

; TDA image - edit
; Permite aplicar funciones especiales a las imágenes. Por ejemplo, para modificar colores en alguno de los canales, pasar a blanco y negro, etc.
; Dominio: f X image
; Recorrido: image

(define edit (lambda (f pic)
               (image-list -1 (getW pic) (getH pic) (map f (get-pixlist pic)))))


; TDA image - invertColorRGB
; Función que permite obtener el color simétricamente
; opuesto en cada canal dentro de un pixel.
; Dominio: pixrgb-d   Rrcorrido: pixrgb-d

(define invertColorRGB (lambda (pix)
                         (pixrgb-d (getX pix) (getY pix)
                                   (- 255 (getR pix))
                                   (- 255 (getG pix))
                                   (- 255 (getB pix))
                                   (getD pix))))

; TDA image - adjustChannel
; Función que permite ajustar cualquier canal de una imagen con pixeles pixrgb-d, incluido el canal de profundidad d.
; Se asume que la función que modificará el canal produce valores dentro del rango válido.
; Dominio: f1 X f2 X f3 X pixrgb-d
; Recorrido: pixrgb-d

(define adjustChannel (lambda (sel mod f) ;getR setR incCh
                        (lambda (pix)
                          (mod (f (sel pix)) pix))))

; Función incCh incrementa un canal rgb en uno
; Dominio: int    Recorrido: int
(define incCh (lambda (color)
                       (if (< color 255) (+ color 1) color)))


; Función redCh recude un canal rgb en uno
; Dominio: int    Recorrido: int
(define redCh (lambda (color)
                       (if (> color 0) (- color 1) color)))


; TDA image - image->string
; Función que transforma una imagen a una representación string.
; La transformación depende de si la imagen es bitmap-d, hexmap-d o pixmap-d, para lo cual se pasa la función de transformación

(define image->string (lambda (pic f)
                        ; Función string-recur es una función recursiva de cola que añade a un string vacío los pixeles en orden
                        ; Dominio: image X string X int X int X int X int        Recorrido: string
                        (define string-recur (lambda
                                                 (pic str x y w h)
                                                       ; Si ya se recorrió toda la imagen se entrega el string resultante
                                                 (cond ((= y h) str)
                                                       ; Si ya se recorrió una fila se agrega el salto de línea, se reinicia la coordenada "x", y finalmente se aumenta la "y"
                                                       ((= x w) (string-recur pic (string-append str "\n") 0 (+ y 1) w h))
                                                       ; Si no se realiza la recursión de cola aumentando la coordenada "x" y manteniendo la "y"
                                                       (else (string-recur pic
                                                                   ; El string de la recursión filtra los pixeles que tengan la coordenada "x" e "y" actuales y concatena con el string acumulado
                                                                   (if (null? (filter ((lambda (x y w h)
                                                                                                    (lambda (pix)
                                                                                                      (if (and (= (getX pix) x) (= (getY pix) y)) #t #f))) x y w h) (get-pixlist pic)))
                                                                       str
                                                                       (string-append str (f (car (filter ((lambda (x y w h)
                                                                                                    (lambda (pix)
                                                                                                      (if (and (= (getX pix) x) (= (getY pix) y)) #t #f))) x y w h) (get-pixlist pic)))) "\t"))
                                                                   (+ x 1) y w h)))))
                        ; La llamada recursiva inicial parte con un string vacío y en las coordenadas (0, 0)
                        (string-recur pic "" 0 0 (getW pic) (getH pic))))



; Función pixrgb->string
; Transforma un pixrgb-d a string
; Dominio: pixrgb-d        Recorrido: string
(define pixrgb->string (lambda (pix) (getHex (pixrgb->pixhex pix))))

; Función pixhex->string
; Transforma un pixhex-d a string
; Dominio: pixhex-d        Recorrido: string
(define pixhex->string (lambda (pix) (getHex pix)))


; TDA image - depthLayers
; Función que permite separar una imagen en capas en base a la profundidad en que se sitúan los pixeles.
; El resultado consiste en una lista de imagenes donde cada una agrupa los pixeles que se sitúan en el mismo nivel de profuncidad.
; Además, en las imágenes resultantes se sustituyen los pixeles que se encuentran en otro nivel de profundidad por pixeles blancos.
; Dominio: image        Recorrido: image list
(define depthLayers (lambda (pic)
                      ; Función recursiva de cola que va creando la lista de imagenes por profundidad
                      (define crop-layers (lambda (pic-list d pixels)
                                            ; Condición de freno, cuando no queden pixeles por agrupar retorna la lita de imagenes
                                            (cond ((null? pixels) pic-list)
                                                  ; Si no hay pixeles de la profundidad "d" se llama a la función aumentando "d" en 1
                                                  ((null? (filter (lambda (pix) (if (= d (get-d pix)) #t #f)) pixels)) (crop-layers pic-list (+ d 1) pixels))
                                                  ; Si existen pixeles en la profundidad "d" se crea una imagen con todos ellos y se llenan los espacios con pixeles blancos
                                                  (else (crop-layers (cons (white-fill (image-list -1 (get-w pic) (get-h pic) (filter (lambda (pix) (if (= d (get-d pix)) #t #f)) pixels))) pic-list) (+ d 1) (remove* (filter (lambda (pix) (if (= d (get-d pix)) #t #f)) pixels) pixels))))))
                      (crop-layers '() 0 (get-pixlist pic))))


; Función recursiva de cola que rellena los pixeles faltantes de una imagen con el color blanco
; Dominio: image        Recorrido: image
(define white-fill (lambda (pic)
                     ; La función recorre las coordenadas x e y buscando pixeles faltantes
                     (define recur-fill (lambda (pic x y w h)
                                         (cond ((= y h) pic)
                                               ((= x w) (recur-fill pic 0 (+ y 1) w h))
                                               ((not (null? (filter (lambda (pix) (if (and (= (get-x pix) x) (= (get-y pix) y)) #t #f)) (get-pixlist pic)))) (recur-fill pic (+ x 1) y w h))
                                               ; Si no existe el pixel en las coordenadas "x" e "y" se genera un pixel blanco
                                               (else (recur-fill (image-list -1 w h (cons (cond ((bitmap? pic) (pixbit-d x y 1 (get-d (car (get-pixlist pic)))))
                                                                                                ((pixmap? pic) (pixrgb-d x y 255 255 255 (get-d (car (get-pixlist pic)))))
                                                                                                ((hexmap? pic) (pixhex-d x y "#FFFFFF" (get-d (car (get-pixlist pic))))))
                                                                                                 (get-pixlist pic))) (+ x 1) y w h)))))
                     (recur-fill pic 0 0 (get-w pic) (get-h pic))))


; TDA image - decompress
; Función que permite descomprimir una imagen comprimida.
; Para esto se toma como referencia el color más frecuente a fin de reconstruir todos los pixeles que fueron eliminados en la compresión
; Dominio: image        Recorrido: image
(define decompress (lambda (pic)
                     (color-fill pic (getColor pic))))

; Función recursiva de cola que rellena los pixeles faltantes de una imagen con el color entregado
(define color-fill (lambda (pic color)
                     ; La función recorre las coordenadas x e y buscando pixeles faltantes
                     (define recur-fill (lambda (pic x y w h)
                                         (cond ((= y h) pic)
                                               ((= x w) (recur-fill pic 0 (+ y 1) w h))
                                               ((not (null? (filter (lambda (pix) (if (and (= (get-x pix) x) (= (get-y pix) y)) #t #f)) (get-pixlist pic)))) (recur-fill pic (+ x 1) y w h))
                                               ; Si no existe el pixel en las coordenadas "x" e "y" se genera un pixel blanco
                                               (else (recur-fill (image-list -1 w h (cons (if (null? (get-pixlist pic)) (pixhex-d x y color -1) ; Si la imagen no posee ni un solo pixel, se genera un hexmap por defecto
                                                                                              (cond ((bitmap? pic) (pixbit-d x y (if (equal? color "#000000") 0 1) -1))
                                                                                                    ((pixmap? pic) (pixrgb-d x y (hex->r color) (hex->g color) (hex->b color) -1))
                                                                                                    ((hexmap? pic) (pixhex-d x y color -1))))
                                                                                                 (get-pixlist pic))) (+ x 1) y w h)))))
                     (recur-fill pic 0 0 (get-w pic) (get-h pic))))


; Función que transforma un color hexadecimal a un entero con la información del canal R
; Dominio: string        Recorrido: int
(define hex->r (lambda (hex)
                 (let ([a (substring hex 1 2)]
                       [b (substring hex 2 3)])
                 (+ (* (if (string->number a)
                           (string->number a)
                           (cond ((equal? a "A") 10)
                                 ((equal? a "B") 11)
                                 ((equal? a "C") 12)
                                 ((equal? a "D") 13)
                                 ((equal? a "E") 14)
                                 ((equal? a "F") 15))) 16)
                    (if (string->number b)
                           (string->number b)
                           (cond ((equal? b "A") 10)
                                 ((equal? b "B") 11)
                                 ((equal? b "C") 12)
                                 ((equal? b "D") 13)
                                 ((equal? b "E") 14)
                                 ((equal? b "F") 15)))))))

; Función que transforma un color hexadecimal a un entero con la información del canal G
; Dominio: string        Recorrido: int
(define hex->g (lambda (hex)
                 (let ([a (substring hex 3 4)]
                       [b (substring hex 4 5)])
                 (+ (* (if (string->number a)
                           (string->number a)
                           (cond ((equal? a "A") 10)
                                 ((equal? a "B") 11)
                                 ((equal? a "C") 12)
                                 ((equal? a "D") 13)
                                 ((equal? a "E") 14)
                                 ((equal? a "F") 15))) 16)
                    (if (string->number b)
                           (string->number b)
                           (cond ((equal? b "A") 10)
                                 ((equal? b "B") 11)
                                 ((equal? b "C") 12)
                                 ((equal? b "D") 13)
                                 ((equal? b "E") 14)
                                 ((equal? b "F") 15)))))))

; Función que transforma un color hexadecimal a un entero con la información del canal B
; Dominio: string        Recorrido: int
(define hex->b (lambda (hex)
                 (let ([a (substring hex 5 6)]
                       [b (substring hex 6 7)])
                 (+ (* (if (string->number a)
                           (string->number a)
                           (cond ((equal? a "A") 10)
                                 ((equal? a "B") 11)
                                 ((equal? a "C") 12)
                                 ((equal? a "D") 13)
                                 ((equal? a "E") 14)
                                 ((equal? a "F") 15))) 16)
                    (if (string->number b)
                           (string->number b)
                           (cond ((equal? b "A") 10)
                                 ((equal? b "B") 11)
                                 ((equal? b "C") 12)
                                 ((equal? b "D") 13)
                                 ((equal? b "E") 14)
                                 ((equal? b "F") 15)))))))




  






; ejemplos
(define pix (pixbit-d 4 3 1 1))
(define pixh (pixhex-d 0 0 "#FFFFFF" 1))
(define ejimageb (image 2 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 1 0 0 30) (pixbit-d 1 1 0 30)))
(define ejimager (image 3 3
                        (pixrgb-d 0 0 1 0 0 0) (pixrgb-d 1 0 2 0 0 0) (pixrgb-d 2 0 3 0 0 0)
                        (pixrgb-d 0 1 4 0 0 2) (pixrgb-d 1 1 5 0 0 2) (pixrgb-d 2 1 6 0 0 2)
                        (pixrgb-d 0 2 7 0 0 3) (pixrgb-d 1 2 8 0 0 3) (pixrgb-d 2 2 9 0 0 3)))

(define ejimageh (image 4 4 (pixhex-d 0 0 "#FF0000" 1) (pixhex-d 1 0 "#FF0000" 1) (pixhex-d 2 0 "#FF0000" 1) (pixhex-d 3 0 "#FF0000" 1)
                            (pixhex-d 0 1 "#FF0000" 1) (pixhex-d 1 1 "#FF0000" 1) (pixhex-d 2 1 "#FF0000" 1) (pixhex-d 3 1 "#FF0000" 1)
                            (pixhex-d 0 2 "#FF0000" 1) (pixhex-d 1 2 "#FF0000" 1) (pixhex-d 2 2 "#FF0000" 1) (pixhex-d 3 2 "#FF0000" 1)
                            (pixhex-d 0 3 "#FF0000" 1) (pixhex-d 1 3 "#FF0000" 1) (pixhex-d 2 3 "#FF0000" 1) (pixhex-d 3 3 "#FF0000" 1)))
