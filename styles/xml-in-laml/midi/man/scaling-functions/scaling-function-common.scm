(laml-style "simple-svg11")
(laml-style "xml-in-laml/midi/midi")
(load (string-append laml-dir "styles/xml-in-laml/midi/midi-laml-processing-lib.scm"))
(lib-load "svg-extensions.scm")

(define x-of (lambda (x) (exact->inexact (car x))))
(define y-of (lambda (x) (exact->inexact (cdr x))))

(define (drawing-points resolution y-axis-min y-axis-max sf)
 (map (transform-to-graphics-coordinate-sytem y-axis-min y-axis-max) 
  (map (lambda (x) (cons x (sf x)))
   (map (lambda (x) (/ x resolution)) (number-interval 0 resolution)))))


; x between 0 and 1
; y is real
(define (transform-to-graphics-coordinate-sytem y-axis-min y-axis-max)
  (lambda (x-y-pair)
   (let* ((x (car x-y-pair))
          (y (cdr x-y-pair))
          (x-out (+ 40 (* x 500)))
          (x-axis-level (if (= y-axis-min 0) ; on y-axis
                            500
                            (* 500 (/ y-axis-max (- y-axis-max y-axis-min)))))
          (y-out (- 500 (* (/ (- y y-axis-min) (- y-axis-max y-axis-min)) 500)))
         )
     (cons x-out y-out))))
         
(define (point-list-to-path p-lst)
  (let ((first-point (car p-lst)))
    (am-p (x-of first-point) (y-of first-point)
          (point-list-to-path-rest (cdr p-lst)))))

(define (point-list-to-path-rest p-lst)
  (cond ((null? p-lst) (e-p))
        (else (let ((point (car p-lst)))
                 (al-p (x-of point) (y-of point)
                    (point-list-to-path-rest (cdr p-lst)))))))

; y-axis-max >= 1
; y-axis-min <= 0
(define (make-svg-drawing sf resolution y-axis-min y-axis-max)
 (let ((x-axis-level (exact->inexact (if (= y-axis-min 0)  ; on y-axis
                                         500
                                         (* 500 (/ y-axis-max (- y-axis-max y-axis-min)))))))
  (write-html '(raw prolog)
   (svg 'width 540 'height 540
     (g 
       ; y axix:
       (line 'x1 40 'y1 540 'x2 40 'y1 0 'stroke "black" 'stroke-width "1px")
          ; arrow
          (line 'x1 30 'y1 10 'x2 40 'y2 0  'stroke "black" 'stroke-width "1px")
          (line 'x1 50 'y1 10 'x2 40 'y2 0  'stroke "black" 'stroke-width "1px")

          ; unit texts:
          (text  'font-family "times-roman" 'font-size 15 'stroke "blue" 'color "blue" 'fill "blue"
                 'x 10 'y 15 y-axis-max)

          ; unit texts:
          (text  'font-family "times-roman" 'font-size 15 'stroke "blue" 'color "blue" 'fill "blue"
                 'x 10 'y 510 y-axis-min)

       ; x axix:
       (line 'x1 0 'y1 x-axis-level 'x2 540 'y2 x-axis-level 'stroke "black" 'stroke-width "1px")
          ; arrow
          (line 'x1 530 'y1 (- x-axis-level 10) 'x2 540 'y2 x-axis-level 'stroke "black" 'stroke-width "1px")
          (line 'x1 530 'y1 (+ x-axis-level 10) 'x2 540 'y2 x-axis-level 'stroke "black" 'stroke-width "1px")

          (text  'font-family "times-roman" 'font-size 15 'stroke "blue" 'color "blue" 'fill "blue"
                 'x 530 'y (+ x-axis-level 25) 1)


       (path 'd (point-list-to-path (drawing-points resolution y-axis-min y-axis-max sf)) 'stroke "red" 'stroke-width "2px" 'fill "none")
     ))
   (string-append (source-filename-without-extension) "." "svg")
  )))