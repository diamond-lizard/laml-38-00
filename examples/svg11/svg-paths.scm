;;; Scheme path functions.
;;; Functions for definition of SVG paths

;; Move start of path to x y - absolute
(define (am-p x y path)
  (string-append "M " (as-string x) " " (as-string y) " " path))

(define (e-p) "")

; absolute line path to x,y
(define (al-p x y path) (p-exp "L" path x y))

; relative line path to x,y
(define (rl-p x y path) (p-exp "l" path x y))

(define (ah-p x path) (p-exp "H" path x))

(define (rh-p x path) (p-exp "h" path x))

(define (av-p y path) (p-exp "V" path y))

(define (rv-p y path) (p-exp "v" path y))

(define (rm-p x y path) (p-exp "m" path x y))

(define (am-p x y path) (p-exp "M" path x y))

(define (z-p) "Z")

(define (e-p) "")

(define (p-exp letter path . coordinates)
  (string-append 
    letter " " 
    (list-to-string (map as-string coordinates) " ") 
    " " path))


(define concat-p string-append)

(define (box . four-numbers)
 (list-to-string (map as-string four-numbers) " "))