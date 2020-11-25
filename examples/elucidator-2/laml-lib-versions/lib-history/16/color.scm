; The LAML library and programs written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999  Kurt Normark.
; 
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


;;;; A library which contains the basis of handling colors in HTML.
;;;; In particular it can convert decimal color numbers to a hexidecial representation. 
;;;; The library is used to convert rgb color lists, such as (list 255 255 255) to a string, such as "#ffffff".<p>
;;;; The library also contains a set of ad hoc color constants.

;;; Number converting. 

(define (number-in-base n base)
 ;; Return the decimal number n in base. n should be positive. Returns a string.
 (if (= n 0) "0"
  (let ((ciffer-list (reverse (ciffers-in-base n base))))
     (ciffer-output ciffer-list))))

(define (ciffers-in-base n base)
  (if (= n 0)
      '()
      (let ((rem (modulo n base))
            (newn (quotient n base)))
        (cons rem (ciffers-in-base newn base)))))

(define (ciffer-output ciffer-list)
 (apply string-append
    (map ciffer-translation ciffer-list)))

(define (ciffer-translation c)
  (cond ((<= c 9) (number->string c))
        ((and (> c 9) (< c 33)) (make-string 1 (integer->char (+ c 87))))
        (t "?")))

;;; Rgb color functions. 

;; Return an 'Internet list' encoding the color (list r g b). 
(define (rgb r g b)
  (list (number-in-base r 16) (number-in-base g 16) (number-in-base b 16)))

(define (pad-to-length2 str)
  (if (< (string-length str) 2)
      (string-append "0" str)
      str))

;; Return an 'Internet' string encoding the color (list r g b). 
(define (rgb-string r g b)
  (let* ((lst3 (rgb r g b))
         (lst3-a (map pad-to-length2 lst3)))
    (apply string-append (cons "#" lst3-a))))

;; Returns the color encoding of (list r g b) given three numbers a parameters.
;; The function rgb-color is an alias for rgb-string.
;; .form (rgb-color r g b)
;; .parameter r The red color (a number between 0 and 255)
;; .parameter g The green color (a number between 0 and 255)
;; .parameter b The blue color (a number between 0 and 255)
(define rgb-color rgb-string)

;; A variant of rgb-string, in which the colors are passed in one list.
(define (rgb-string-list color-list)
  (rgb-string (car color-list) (cadr color-list) (caddr color-list)))

;; Returns the color encoding of (list r g b) given a list of three color numbers as parameter.
;; The function rgb-color-list is an alias for rgb-string-list.
;; .form (rgb-color-list color-triple-list)
;; .parameter color-triple-list A list of three numbers - red, green, and blue.
(define rgb-color-list rgb-string-list)


; The hexidecimal ciffer, represented as a character, is translated to
; a number between 0 and 15. Both lower case and upper case letters between a and f (A and F)
; can be used to represent the high ciffers.
(define (hex-ciffer->decimal-ciffer x)
  (let ((n (char->integer x)))
    (cond ((and (>= n 48) (<= n 57)) (- n 48))
          ((and (>= n 97) (<= n 102)) (- n 87)) 
          ((and (>= n 65) (<= n 70)) (- n 55))
          (error (string-append "hex-ciffer->decimal-ciffer: The ciffer " (as-string x) " is not a hexadecimal ciffer")))))

;;; Color construction and constants. 

;; Make and return the rgb-list representation of the color with r red, g green, and b blue. 
(define (make-color r g b)
  (list r g b))

;;; Color constants.

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define red (make-color 255 0 0))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define dark-red (make-color 210 0 0))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define green (make-color 0 255 0))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define green1 (make-color 202 240 179))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define green2 (make-color 182 248 197))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define blue (make-color 0 0 255))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define white (make-color 255 255 255))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define black (make-color 0 0 0))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define yellow (make-color 255 255 0))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define purple (make-color 255 0 255))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define light-blue (make-color 0 255 255))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define blue1 (make-color 170 241 249))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define blue2 (make-color 204 255 255))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define blue3 (make-color 198 203 253))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define orange (make-color 211 90 18))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define dark-yellow (make-color 228 211 5))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define grey1 (make-color 145 145 145))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define grey2 (make-color 210 210 210))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define brown (make-color 166 71 0))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define maroon   (make-color 128 0 0))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define grey     (make-color 128 128 128))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define silver   (make-color 192 192 192))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define tetal    (make-color 0 128 128))

;; A color constant. A color is represented as a list of integers of length three (rgb).

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define aqua     (make-color 0 255 255))

;; A color constant. A color is represented as a list of integers of length three (rgb).

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define lime     (make-color 0 255 0))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define olive    (make-color 128 128 0))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define navy     (make-color 0 0 128))

;; A color constant. A color is represented as a list of integers of length three (rgb).
(define fuchsia  (make-color 255 0 255))
