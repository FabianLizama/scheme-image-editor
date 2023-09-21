# scheme-image-editor

## Introducción

Editor abstracto de imágenes en 3 dimensiones.

El objetivo de este proyecto es proporcionar una serie de funciones que permiten realizar diversas operaciones en imágenes abstractas a un nivel muy bajo, sin una interfaz gráfica, y utilizando programación funcional.

## Características
- **3D:** Las imágenes cuentan con largo, ancho y profundidad.
- **Compatibilidad de Formatos:** Admite diferentes formatos de imágenes, incluyendo bitmap, pixmap y hexmap, lo que permite trabajar con una variedad de tipos de imágenes abstractas.
- **Operaciones de Transformación:** Puedes realizar transformaciones en las imágenes, como voltear horizontalmente (`flipH`) y verticalmente (`flipV`), recortar (`crop`) y rotar 90 grados (`rotate90`).
- **Manipulación de Colores:** Las funciones de este editor permiten invertir colores a nivel de bits (`invertColorBit`), invertir colores RGB (`invertColorRGB`) y ajustar canales de color (`adjustChannel`).
- **Compresión y Descompresión:** Puedes comprimir (`compress`) y descomprimir (`decompress`) imágenes según sea necesario, aunque ten en cuenta que se puede perder información de profundidad durante este proceso.
- **Representación Flexible:** La función `image->string` te permite convertir una imagen en una representación de cadena, lo que facilita el almacenamiento y la visualización.

## Tecnologías
- [Scheme](https://www.scheme.org/): Lenguaje de programación
- [Dr. Racket](https://racket-lang.org/): Editor de código

## Instalación

```shell
git clone https://github.com/FabianLizama/scheme-image-editor.git
```


## Uso
Se pueden crear píxeles e imágenes para su posterior manipulación.
Para creación de píxeles se utilizará el ejemplo de un pixel blanco en las coordenadas `x: 2`, `y: 2` y `d: 10`.
- Pixbit:
```scheme
;(pixbit-d x y bit depth)
(pixbit-d 2 2 1 10)
```

- Pixrgb:
```scheme
;(pixrgb-d x y r g b depth)
(pixrgb-d 2 2 255 255 255 10)
```

- Pixhex:
```scheme
;(pixhex-d x y hexcode depth) ejemplo:
(pixhex-d 2 2 ffffff 10)
```

- Image:
```scheme
;(image width height pixel-1 pixel-2 ... pixel-n) ejemplo:
(image 2 2
        (pixbit-d 0 0 0 10)
        (pixbit-d 0 1 1 20)
        (pixbit-d 1 0 1 10)
        (pixbit-d 1 1 0 255))
```

Para ver todas las funciones y su uso revisar el código respectivamente documentado.
