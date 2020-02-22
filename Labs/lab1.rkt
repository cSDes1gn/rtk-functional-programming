#lang racket
(require 2htdp/image)

;;Notes
(define CM-PER-INCH 2.54)

(define a-red-square (rectangle 100 100 "solid" "red"))
(define a-blue-circle (circle 50 "solid" "blue"))

(define outlined-circle (circle 50 "outline" "blue"))
(define outlined-square (rectangle 100 100 "outline" "red"))

(define row-of-squares (beside (rectangle 50 50 "solid" "red")
                               (rectangle 50 50 "solid" "blue")
                               (rectangle 50 50 "solid" "green")))

(define column-of-squares (above (rectangle 50 50 "solid" "red")
                                 (rectangle 50 50 "solid" "blue")
                                 (rectangle 50 50 "solid" "green")))

(define nested-squares (overlay (rectangle 12.5 12.5 "solid" "black")
                                (rectangle 25 25 "solid" "green")
                                (rectangle 37.5 37.5 "solid" "blue")
                                (rectangle 50 50 "solid" "red")))

(define rotated-square (rotate 45 nested-squares))

(define canada-flag (overlay/offset (rectangle 25 50 "solid" "red")
                                    -75 0
                                    (rectangle 25 50 "solid" "red")))