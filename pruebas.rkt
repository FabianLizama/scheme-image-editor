#lang scheme
(require "pixbit-d.rkt")
(require "pixrgb-d.rkt")
(require "pixhex-d.rkt")
(require "image.rkt")


; Script de pruebas
(display "-----------------------------------------------------------------\n")
(display "1. Ejemplos función image (Constructor)\n\n")

;img1
;Creación de una imagen de 2 x 2 del tipo pixmap
(define img1 (image 2 2
                  (pixrgb-d 0 0 255 0 0 10)
                  (pixrgb-d 0 1 0 255 0 20)
                  (pixrgb-d 1 0 0 0 255 10)
                  (pixrgb-d 1 1 255 255 255  1)
 ))

;img2
;Creación de una imagen de 2 x 2 del tipo bitmap
(define img2 (image 2 2
                  (pixbit-d 0 0 0 10)
                  (pixbit-d 0 1 1 20)
                  (pixbit-d 1 0 1 10)
                  (pixbit-d 1 1 0 255))
 )

(define img3 (imgRGB->imgHex img1))


;imprimir una representación string de la imagen 1
(display "img1")
(display (image->string img1 pixrgb->string))
(display "\n")

;output:
; #FF0000 #00FF00
; #0000FF #FFFFFF

;imprimir una representación string de la imagen 2
(display "img2")
(display (image->string img2 pixbit->string))
(display "\n")

;output:
;0 1
;1 0

(display "Creación de una imagen pixmap 3x3")
(display (image->string (image 3 3 (pixrgb-d 0 0 255 0 0 0) (pixrgb-d 1 0 0 255 0 0) (pixrgb-d 2 0 0 0 255 0)
                                                 (pixrgb-d 0 1 255 0 0 1) (pixrgb-d 1 1 0 255 0 1) (pixrgb-d 2 1 0 0 255 1)
                                                 (pixrgb-d 0 2 255 0 0 2) (pixrgb-d 1 2 0 255 0 1) (pixrgb-d 2 2 0 0 255 2)) pixrgb->string))
(display "\n")

(display "Creación de una imagen hexmap 1x1")
(display (image->string (image 1 1 (pixhex-d 0 0 "#A46C00" 0)) pixhex->string))
(display "\n")

(display "Creación de una imagen bitmap 1x1")
(display (image->string (image 1 1 (pixbit-d 0 0 1 0)) pixbit->string))
(display "\n")

(display "-----------------------------------------------------------------\n")
(display "2. Ejemplos función bitmap? (Pertenencia)\n\n")

(bitmap? img1) ; la respuesta debería ser #f
(bitmap? img2)  ; la respuesta debería ser #t
(bitmap? img3)  ; la respuesta debería ser #f
(bitmap? (image 3 3 (pixrgb-d 0 0 255 0 0 0) (pixrgb-d 1 0 0 255 0 0) (pixrgb-d 2 0 0 0 255 0)
                                                 (pixrgb-d 0 1 255 0 0 1) (pixrgb-d 1 1 0 255 0 1) (pixrgb-d 2 1 0 0 255 1)
                                                 (pixrgb-d 0 2 255 0 0 2) (pixrgb-d 1 2 0 255 0 1) (pixrgb-d 2 2 0 0 255 2))) ; la respuesta debería ser #f
(bitmap? (image 1 1 (pixbit-d 0 0 1 0))) ; la respuesta debería ser #t
(bitmap? (image 2 2 (pixhex-d 0 0 "#AABBCC" 0) (pixhex-d 1 0 "#FFFFFF" 2) (pixhex-d 0 1 "#000000" 5) (pixhex-d 1 1 "#0A4C6B" 0))) ; la respuesta debería ser #f


(display "-----------------------------------------------------------------\n")
(display "3. Ejemplos función pixmap? (Pertenencia)\n\n")
(pixmap? img1) ; la respuesta debería ser #t
(pixmap? img2)  ; la respuesta debería ser #f
(pixmap? img3)  ; la respuesta debería ser #f
(pixmap? (image 3 3 (pixrgb-d 0 0 255 0 0 0) (pixrgb-d 1 0 0 255 0 0) (pixrgb-d 2 0 0 0 255 0)
                                                 (pixrgb-d 0 1 255 0 0 1) (pixrgb-d 1 1 0 255 0 1) (pixrgb-d 2 1 0 0 255 1)
                                                 (pixrgb-d 0 2 255 0 0 2) (pixrgb-d 1 2 0 255 0 1) (pixrgb-d 2 2 0 0 255 2))) ; la respuesta debería ser #t
(pixmap? (image 1 1 (pixbit-d 0 0 1 0))) ; la respuesta debería ser #f
(pixmap? (image 2 2 (pixhex-d 0 0 "#AABBCC" 0) (pixhex-d 1 0 "#FFFFFF" 2) (pixhex-d 0 1 "#000000" 5) (pixhex-d 1 1 "#0A4C6B" 0))) ; la respuesta debería ser #f

(display "-----------------------------------------------------------------\n")
(display "4. Ejemplos función hexmap? (Pertenencia)\n\n")
(hexmap? img1) ; la respuesta debería ser #f
(hexmap? img2)  ; la respuesta debería ser #f
(hexmap? img3)  ; la respuesta debería ser #t
(hexmap? (image 3 3 (pixrgb-d 0 0 255 0 0 0) (pixrgb-d 1 0 0 255 0 0) (pixrgb-d 2 0 0 0 255 0)
                                                 (pixrgb-d 0 1 255 0 0 1) (pixrgb-d 1 1 0 255 0 1) (pixrgb-d 2 1 0 0 255 1)
                                                 (pixrgb-d 0 2 255 0 0 2) (pixrgb-d 1 2 0 255 0 1) (pixrgb-d 2 2 0 0 255 2))) ; la respuesta debería ser #f
(hexmap? (image 1 1 (pixbit-d 0 0 1 0))) ; la respuesta debería ser #f
(hexmap? (image 2 2 (pixhex-d 0 0 "#AABBCC" 0) (pixhex-d 1 0 "#FFFFFF" 2) (pixhex-d 0 1 "#000000" 5) (pixhex-d 1 1 "#0A4C6B" 0))) ; la respuesta debería ser #t

(display "-----------------------------------------------------------------\n")
(display "5. Ejemplos función compressed? (Pertenencia)\n\n")
(compressed? img1) ; la respuesta debería ser #f
(compressed? img2) ; la respuesta debería ser #f
(compressed? img3) ; la respuesta debería ser #f
(compressed? (compress (image 3 3 (pixrgb-d 0 0 255 0 0 0) (pixrgb-d 1 0 0 255 0 0) (pixrgb-d 2 0 0 0 255 0)
                                                 (pixrgb-d 0 1 255 0 0 1) (pixrgb-d 1 1 0 255 0 1) (pixrgb-d 2 1 0 0 255 1)
                                                 (pixrgb-d 0 2 255 0 0 2) (pixrgb-d 1 2 0 255 0 1) (pixrgb-d 2 2 0 0 255 2)))) ; la respuesta debería ser #t
(compressed? (image 3 3 (pixrgb-d 0 0 255 0 0 0) (pixrgb-d 1 0 0 255 0 0) (pixrgb-d 2 0 0 0 255 0)
                                                 (pixrgb-d 0 1 255 0 0 1) (pixrgb-d 1 1 0 255 0 1) (pixrgb-d 2 1 0 0 255 1)
                                                 (pixrgb-d 0 2 255 0 0 2) (pixrgb-d 1 2 0 255 0 1) (pixrgb-d 2 2 0 0 255 2)))  ; la respuesta debería ser #f
(compressed? (compress (image 1 1 (pixbit-d 0 0 1 0)))) ; la respuesta debería ser #t

(display "-----------------------------------------------------------------\n")
(display "6. Ejemplos función flipH (Modificador)\n\n")
(display "img1 original")
(display (image->string img1 pixrgb->string))
(display "\n")
(display "img1 invertida horizontalmente")
(display (image->string (flipH img1) pixrgb->string))
(display "\n")
(display "img2 original")
(display (image->string img2 pixbit->string))
(display "\n")
(display "img2 invertida horizontalmente")
(display (image->string (flipH img2) pixbit->string))
(display "\n")
(display "img3 original")
(display (image->string img3 pixhex->string))
(display "\n")
(display "img3 invertida horizontalmente")
(display (image->string (flipH img3) pixhex->string))
(display "\n")
(display "Imagen 3x3 invertida horizontalmente")
(display (image->string (flipH (image 3 3 (pixrgb-d 0 0 255 0 0 0) (pixrgb-d 1 0 0 255 0 0) (pixrgb-d 2 0 0 0 255 0)
                                                 (pixrgb-d 0 1 255 0 0 1) (pixrgb-d 1 1 0 255 0 1) (pixrgb-d 2 1 0 0 255 1)
                                                 (pixrgb-d 0 2 255 0 0 2) (pixrgb-d 1 2 0 255 0 1) (pixrgb-d 2 2 0 0 255 2))) pixrgb->string))
(display "\n")
(display "Imagen 1x1 invertida horizontalmente")
(display (image->string (flipH (image 1 1 (pixbit-d 0 0 1 0))) pixbit->string))


(display "-----------------------------------------------------------------\n")
(display "7. Ejemplos función flipV (Modificador)\n\n")
(display "img1 original")
(display (image->string img1 pixrgb->string))
(display "\n")
(display "img1 invertida verticalmente")
(display (image->string (flipV img1) pixrgb->string))
(display "\n")
(display "img2 original")
(display (image->string img2 pixbit->string))
(display "\n")
(display "img2 invertida verticalmente")
(display (image->string (flipV img2) pixbit->string))
(display "\n")
(display "img3 original")
(display (image->string img3 pixhex->string))
(display "\n")
(display "img3 invertida verticalmente")
(display (image->string (flipV img3) pixhex->string))
(display "\n")
(display "Imagen 3x3 invertida verticalmente")
(display (image->string (flipV (image 3 3 (pixrgb-d 0 0 255 0 0 0) (pixrgb-d 1 0 0 255 0 0) (pixrgb-d 2 0 0 0 255 0)
                                                 (pixrgb-d 0 1 255 0 0 1) (pixrgb-d 1 1 0 255 0 1) (pixrgb-d 2 1 0 0 255 1)
                                                 (pixrgb-d 0 2 255 0 0 2) (pixrgb-d 1 2 0 255 0 1) (pixrgb-d 2 2 0 0 255 2))) pixrgb->string))
(display "\n")
(display "Imagen 1x1 invertida verticalmente")
(display (image->string (flipV (image 1 1 (pixbit-d 0 0 1 0))) pixbit->string))


(display "-----------------------------------------------------------------\n")
(display "8. Ejemplos función crop (Modificador)\n\n")
(define img4 (crop img1 0 0 0 0)) ; debería retornar una imágen con un pixel
(define img5 (crop img2 0 0 0 1)) ; debería retornar una imágen con dos pixeles
(define img6 (crop img1 0 1 1 1)) ; debería retornar una imágen con dos pixeles
(define img7 (crop img2 0 0 1 1)) ; debería retornar la misma imagen

(display "img 4 crop de img1 debería retornar una imágen con un pixel")
(display (image->string img4 pixrgb->string))
(display "\n")
(display "img 5 crop de img2 debería retornar una imágen con dos pixeles")
(display (image->string img5 pixbit->string))
(display "\n")
(display "img 6 crop de img1 debería retornar una imágen con dos pixeles")
(display (image->string img6 pixrgb->string))
(display "\n")
(display "img 7 crop de img2 debería retornar debería retornar la misma imagen")
(display (image->string img7 pixbit->string))
(display "\n")
(display "Crop de imagen 3x3 debería retornar una imagen con 4 pixeles")
(display (image->string (crop (image 3 3 (pixrgb-d 0 0 255 0 0 0) (pixrgb-d 1 0 0 255 0 0) (pixrgb-d 2 0 0 0 255 0)
                                                 (pixrgb-d 0 1 255 0 0 1) (pixrgb-d 1 1 0 255 0 1) (pixrgb-d 2 1 0 0 255 1)
                                                 (pixrgb-d 0 2 255 0 0 2) (pixrgb-d 1 2 0 255 0 1) (pixrgb-d 2 2 0 0 255 2)) 0 0 1 1) pixrgb->string)) ; debería retornar una imagen con 4 pixeles
(display "\n")
(display "Crop de imagen 1x1 debería retornar la misma imagen")
(display (image->string (crop (image 1 1 (pixbit-d 0 0 1 0)) 0 0 0 0) pixbit->string)) ; debería retornar la misma imagen
(display "\n")
(display "Crop de imagen 2x2 debería retornar una imagen de 1x1")
(display (image->string (crop (image 2 2 (pixhex-d 0 0 "#AABBCC" 0) (pixhex-d 1 0 "#FFFFFF" 2) (pixhex-d 0 1 "#000000" 5) (pixhex-d 1 1 "#0A4C6B" 0)) 1 1 1 1) pixhex->string)) ; debería retornar una imagen de 1x1

(display "-----------------------------------------------------------------\n")
(display "9. Ejemplos función imgRGB->imgHex (Modificador)\n\n")
(display "img1 transformada a hexmap\n")
(imgRGB->imgHex img1)
(display "img4 transformada a hexmap\n")
(imgRGB->imgHex img4)
(display "imagen 3x3 transformada a hexmap\n")
(imgRGB->imgHex (image 3 3 (pixrgb-d 0 0 255 0 0 0) (pixrgb-d 1 0 0 255 0 0) (pixrgb-d 2 0 0 0 255 0)
                                                 (pixrgb-d 0 1 255 0 0 1) (pixrgb-d 1 1 0 255 0 1) (pixrgb-d 2 1 0 0 255 1)
                                                 (pixrgb-d 0 2 255 0 0 2) (pixrgb-d 1 2 0 255 0 1) (pixrgb-d 2 2 0 0 255 2)))

(display "-----------------------------------------------------------------\n")
(display "10. Ejemplos función histogram (Otros)\n\n")
(display "El histograma es una lista de pares, cada par tiene como primer elemento el color que representa y como segundo elemento la frecuencia con que este color aparece en la imagen\n")
(display "Histograma de img1\n")
(histogram img1)
(display "Histograma de img2\n")
(histogram img2)
(display "Histograma de img3\n")
(histogram img3)
(display "Histograma de img4\n")
(histogram img4)
(display "Histograma de img5\n")
(histogram img5)
(display "Histograma de img6\n")
(histogram img6)
(display "Histograma de img7\n")
(histogram img7)
(display "Histograma imagen 3x3\n")
(histogram (image 3 3 (pixrgb-d 0 0 255 0 0 0) (pixrgb-d 1 0 0 255 0 0) (pixrgb-d 2 0 0 0 255 0)
                                                 (pixrgb-d 0 1 255 0 0 1) (pixrgb-d 1 1 0 255 0 1) (pixrgb-d 2 1 0 0 255 1)
                                                 (pixrgb-d 0 2 255 0 0 2) (pixrgb-d 1 2 0 255 0 1) (pixrgb-d 2 2 0 0 255 2)))
(display "Histograma imagen 1x1\n")
(histogram (image 1 1 (pixbit-d 0 0 1 0)))
(display "Histograma imagen 2x2\n")
(histogram (image 2 2 (pixhex-d 0 0 "#AABBCC" 0) (pixhex-d 1 0 "#FFFFFF" 2) (pixhex-d 0 1 "#000000" 5) (pixhex-d 1 1 "#0A4C6B" 0)))

(display "-----------------------------------------------------------------\n")
(display "11. Ejemplos función rotate90 (Modificador)\n\n")
(define img18 (rotate90 img1))
(display "img18 corresponde a img1 rotada 90°")
(display (image->string img18 pixrgb->string))
(display "\n")
(define img19 (rotate90 img2))
(display "img19 corresponde a img2 rotada 90°")
(display (image->string img19 pixbit->string))
(display "\n")
(define img20 (rotate90 img3))
(display "img20 corresponde a img3 rotada 90°")
(display (image->string img20 pixhex->string))
(display "\n")
(define img21 (rotate90 img4))
(display "img21 corresponde a img4 rotada 90°")
(display (image->string img21 pixrgb->string))
(display "\n")
(define img22 (rotate90 img5))
(display "img22 corresponde a img5 rotada 90°")
(display (image->string img22 pixbit->string))
(display "\n")
(define img23 (rotate90 img6))
(display "img23 corresponde a img6 rotada 90°")
(display (image->string img23 pixrgb->string))
(display "\n")
(define img24 (rotate90 img7))
(display "img24 corresponde a img7 rotada 90°")
(display (image->string img24 pixbit->string))
(display "\n")

(display "-----------------------------------------------------------------\n")
(display "12. Ejemplos función compress (Modificador)\n\n")
(define img8 (compress img1))
(display "img8 corresponde a img2 comprimida\n")
img8
(define img9 (compress img2))
(display "img9 corresponde a img3 comprimida\n")
img9
(define img10 (compress img3))
(display "img10 corresponde a img4 comprimida\n")
img10
(define img11 (compress img4))
(display "img11 corresponde a img5 comprimida\n")
img11
(define img12 (compress img5))
(display "img12 corresponde a img6 comprimida\n")
img12
(define img13 (compress img6))
(display "img13 corresponde a img7 comprimida\n")
img13
(define img14 (compress img7))
(display "img13 corresponde a img7 comprimida\n")
img14
(display "Imagen 3x3 comprimida\n")
(compress (image 3 3 (pixrgb-d 0 0 255 0 0 0) (pixrgb-d 1 0 0 255 0 0) (pixrgb-d 2 0 0 0 255 0)
                                                 (pixrgb-d 0 1 255 0 0 1) (pixrgb-d 1 1 0 255 0 1) (pixrgb-d 2 1 0 0 255 1)
                                                 (pixrgb-d 0 2 255 0 0 2) (pixrgb-d 1 2 0 255 0 1) (pixrgb-d 2 2 0 0 255 2)))
(display "Imagen 1x1 comprimida\n")
(compress (image 1 1 (pixbit-d 0 0 1 0)))
(display "Imagen 2x2 comprimida\n")
(compress (image 2 2 (pixhex-d 0 0 "#AABBCC" 0) (pixhex-d 1 0 "#FFFFFF" 2) (pixhex-d 0 1 "#000000" 5) (pixhex-d 1 1 "#0A4C6B" 0)))

(display "Se comprueba mediante la función compressed? si las imagenes anteriores están comprimidas:\n")
(compressed? img8)  ; la respuesta debería ser #t
(compressed? img9)  ; la respuesta debería ser #t
(compressed? img10)  ; la respuesta debería ser #t
(compressed? img11)  ; la respuesta debería ser #t
(compressed? img12)  ; la respuesta debería ser #t
(compressed? img13)  ; la respuesta debería ser #t
(compressed? img14)  ; la respuesta debería ser #t

(display "-----------------------------------------------------------------\n")
(display "13 14 y 15. Ejemplos función edit aplicando invertColorBit e invertColorRGB (Modificador)\n\n")

(display "img 15 corresponde a img2 con los bits invertidos")
(define img15 (edit invertColorBit img2))
(display (image->string img15 pixbit->string))
(display "Imagen 1x1 con los bits invertidos")
(display (image->string (edit invertColorBit (image 1 1 (pixbit-d 0 0 1 0))) pixbit->string))
(display "Imagen 2x2 con los bits invertidos")
(display (image->string (edit invertColorBit (image 2 2 (pixbit-d 0 0 0 0) (pixbit-d 1 0 0 0) (pixbit-d 0 1 1 0) (pixbit-d 1 1 0 0))) pixbit->string))
(display "Imagen 1x2 con los bits invertidos")
(display (image->string (edit invertColorBit (image 1 2 (pixbit-d 0 0 1 0) (pixbit-d 0 1 0 0))) pixbit->string))

(display "img 16 corresponde a img1 con los canales RGB invertidos")
(define img16 (edit invertColorRGB img1))
(display "Imagen 3x3 con los canales RGB invertidos")
(display (image->string (edit invertColorRGB(image 3 3 (pixrgb-d 0 0 255 0 0 0) (pixrgb-d 1 0 0 255 0 0) (pixrgb-d 2 0 0 0 255 0)
                                                 (pixrgb-d 0 1 255 0 0 1) (pixrgb-d 1 1 0 255 0 1) (pixrgb-d 2 1 0 0 255 1)
                                                 (pixrgb-d 0 2 255 0 0 2) (pixrgb-d 1 2 0 255 0 1) (pixrgb-d 2 2 0 0 255 2))) pixrgb->string))
(display "Imagen 2x2 con los canales RGB invertidos")
(display (image->string (edit invertColorRGB  (image 2 2 (pixrgb-d  0 0 10 10 10 10) (pixrgb-d  0 1 20 20 20 20) (pixrgb-d 1 0 30 30 30 30) (pixrgb-d 1 1 40 40 40 40))) pixrgb->string))
(display "Imagen 1x1 con los canales RGB invertidos")
(display (image->string (edit invertColorRGB (image 1 1 (pixrgb-d 0 0 153 184 173 0))) pixrgb->string))


(display "-----------------------------------------------------------------\n")
(display "16. Ejemplos función adjustChannel (Modificador)\n\n")
(display "Imagen 33 corresponde a img1 con el canal R incrementado en 1")
(define img33 (edit (adjustChannel getR setR incCh) img1))
(display (image->string img33 pixrgb->string))
(display "Imagen 34 corresponde a img1 con el canal G incrementado en 1")
(define img34 (edit (adjustChannel getG setG incCh) img1))
(display (image->string img34 pixrgb->string))
(display "Imagen 35 corresponde a img1 con el canal B incrementado en 1")
(define img35 (edit (adjustChannel getB setB incCh) img1))
(display (image->string img35 pixrgb->string))

(display "img1 con el canal R reducido en 1")
(display (image->string (edit (adjustChannel getR setR redCh) img1) pixrgb->string))
(display "img1 con el canal G reducido en 1")
(display (image->string (edit (adjustChannel getG setG redCh) img1) pixrgb->string))
(display "img1 con el canal B reducido en 1")
(display (image->string (edit (adjustChannel getB setB redCh) img1) pixrgb->string))

(display "-----------------------------------------------------------------\n")
(display "17. Ejemplos función image->string (Otros)\n\n")
;imágenes no comprimidas
(display "img1")
(display (image->string img1 pixrgb->string))
(display "img2")
(display (image->string img2 pixbit->string))
(display "img3")
(display (image->string img3 pixhex->string))
(display "img4")
(display (image->string img4 pixrgb->string))
(display "img5")
(display (image->string img5 pixbit->string))
(display "img6")
(display (image->string img6 pixrgb->string))
(display "img7")
(display (image->string img7 pixbit->string))
(display "Imagen pixmap 3x3")
(display (image->string (image 3 3 (pixrgb-d 0 0 255 0 0 0) (pixrgb-d 1 0 0 255 0 0) (pixrgb-d 2 0 0 0 255 0)
                                                 (pixrgb-d 0 1 255 0 0 1) (pixrgb-d 1 1 0 255 0 1) (pixrgb-d 2 1 0 0 255 1)
                                                 (pixrgb-d 0 2 255 0 0 2) (pixrgb-d 1 2 0 255 0 1) (pixrgb-d 2 2 0 0 255 2)) pixrgb->string))
(display "Imagen hexmap 2x2")
(display (image->string (image 2 2 (pixhex-d 0 0 "#AABBCC" 0) (pixhex-d 1 0 "#FFFFFF" 2) (pixhex-d 0 1 "#000000" 5) (pixhex-d 1 1 "#0A4C6B" 0)) pixhex->string))
(display "Imagen bitmap 1x1")
(display (image->string (image 1 1 (pixbit-d 0 0 1 0)) pixbit->string))


;imagenes comprimidas, podrían internamente descomprimirlas para convertir a string ;(opcional)
(display "Las siguientes imágenes no se descomprimieron, se imprimen raro por consola porque las funciones están diseñadas para funcionar con imágenes descomprimidas")
(display (image->string img8 pixrgb->string))
(display (image->string img9 pixbit->string))
(display (image->string img10 pixhex->string)) 
(display (image->string img11 pixrgb->string))
(display  (image->string img12 pixbit->string))
(display (image->string img13 pixrgb->string))
(display (image->string img14 pixbit->string))

;imágenes no comprimidas
(display "img15")
(display (image->string img15 pixbit->string))
(display "img16")
(display (image->string img16 pixrgb->string))
;(display (image->string img17 pixrgb->string)) (se comenta este ejemplo porque la img17 no fue definida anteriormente)
(display "img18")
(display (image->string img18 pixrgb->string))
(display "img19")
(display (image->string img19 pixbit->string))
(display "img20")
(display (image->string img20 pixhex->string))
(display "img21")
(display (image->string img21 pixrgb->string))
(display "img22")
(display (image->string img22 pixbit->string))
(display "img23")
(display (image->string img23 pixrgb->string))
(display "img24")
(display (image->string img24 pixbit->string))

(display "-----------------------------------------------------------------\n")
(display "18. Ejemplos función depthLayers (Otros)\n\n")
(display "img1 separada por profundidades\n")
(depthLayers img1)
(display "img2 separada por profundidades\n")
(depthLayers img2)
(display "img3 separada por profundidades\n")
(depthLayers img3)
(display "img4 separada por profundidades\n")
(depthLayers img4)
(display "img5 separada por profundidades\n")
(depthLayers img5)
(display "img6 separada por profundidades\n")
(depthLayers img6)
(display "img7 separada por profundidades\n")
(depthLayers img7)
(display "Imagen 2x2 separada por profundidades\n")
(depthLayers (image 2 2 (pixrgb-d  0 0 10 10 10 10) (pixrgb-d  0 1 20 20 20 20) (pixrgb-d 1 0 30 30 30 30) (pixrgb-d 1 1 40 40 40 40)))
(display "Imagen 3x3 separada por profundidades\n")
(depthLayers (image 3 3 (pixrgb-d 0 0 255 0 0 0) (pixrgb-d 1 0 0 255 0 0) (pixrgb-d 2 0 0 0 255 0)
                                                 (pixrgb-d 0 1 255 0 0 1) (pixrgb-d 1 1 0 255 0 1) (pixrgb-d 2 1 0 0 255 1)
                                                 (pixrgb-d 0 2 255 0 0 2) (pixrgb-d 1 2 0 255 0 1) (pixrgb-d 2 2 0 0 255 2)))
(display "Imagen 2x2 separada por profundidades\n")
(depthLayers (image 2 2 (pixhex-d 0 0 "#AABBCC" 0) (pixhex-d 1 0 "#FFFFFF" 2) (pixhex-d 0 1 "#000000" 5) (pixhex-d 1 1 "#0A4C6B" 0)))

(display "-----------------------------------------------------------------\n")
(display "19. Ejemplos función decompress (Modificador)\n\n")
(define img25 (decompress img8))
(display "img25 corresponde a la descompresión de la img8")
(display (image->string img25 pixrgb->string))
(define img26 (decompress img9))
(display "img26 corresponde a la descompresión de la img9")
(display (image->string img26 pixbit->string))
(define img27 (decompress img10))
(display "img27 corresponde a la descompresión de la img10")
(display (image->string img27 pixhex->string))
(define img29 (decompress img11))
(display "img29 corresponde a la descompresión de la img11")
(display (image->string img29 pixhex->string))
(define img30 (decompress img12))
(display "img30 corresponde a la descompresión de la img12")
(display (image->string img30 pixbit->string))
(define img31 (decompress img13))
(display "img31 corresponde a la descompresión de la img13")
(display (image->string img31 pixrgb->string))
(define img32 (decompress img14))
(display "img32 corresponde a la descompresión de la img14")
(display (image->string img32 pixbit->string))
(display "Descompresión de la compresión de imagen 3x3")
(display (image->string (decompress (compress (image 3 3 (pixrgb-d 0 0 255 0 0 0) (pixrgb-d 1 0 0 255 0 0) (pixrgb-d 2 0 0 0 255 0)
                                                 (pixrgb-d 0 1 255 0 0 1) (pixrgb-d 1 1 0 255 0 1) (pixrgb-d 2 1 0 0 255 1)
                                                 (pixrgb-d 0 2 255 0 0 2) (pixrgb-d 1 2 0 255 0 1) (pixrgb-d 2 2 0 0 255 2)))) pixrgb->string))
(display "Descompresión de la compresión de imagen 2x2")
(display (image->string (decompress (compress (image 2 2 (pixrgb-d  0 0 10 10 10 10) (pixrgb-d  0 1 20 20 20 20) (pixrgb-d 1 0 30 30 30 30) (pixrgb-d 1 1 40 40 40 40)))) pixrgb->string))
(display "Descompresión de la compresión de imagen 2x2")
(display (image->string (decompress (compress (image 2 2 (pixhex-d 0 0 "#AABBCC" 0) (pixhex-d 1 0 "#FFFFFF" 2) (pixhex-d 0 1 "#000000" 5) (pixhex-d 1 1 "#0A4C6B" 0)))) pixhex->string))

;las siguientes comparaciones deberían arrojar #t
(display "Según enunciado las siguientes comparaciones deberían arrojar #t pero esto no debería ser así pues se permitió perder la información de profundidad de los pixeles al momento de la compresión\n")
(equal? img25 img1)
(equal? img26 img2)
(equal? img27 img3)
;(equal? img28 img4) Se comenta esta linea porque la img28 nunca se definió
(equal? img29 img5)
(equal? img30 img6)
(equal? img31 img7)
(display "las siguientes comparaciones deberían arrojar #f\n")
;las siguientes comparaciones deberían arrojar #f
(equal? img25 img2)
(equal? img26 img1)
