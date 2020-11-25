;;;; .title SchemeDoc Demo
;;;; .author Kurt Normark
;;;; .affiliation Aalborg University, Denmark
;;;; This is a brief example of a Scheme 
;;;; program with multi-semicolon SchemeDoc comments.

; This comment is not extracted.

;;; Factorials. 
;;; .section-id fac-stuff
;;; This section demonstrates a plain function.

;; The factorial function. Also known as n!
;; .parameter n An integer
;; .pre-condition n >= 0
;; .returns n * (n-1) * ... * 1
(define (fac n)
 (fac-iter 1 n 1))

(define (fac-iter i n res)
 (cond ((= i n) (* n res))
       ((< i n) (fac-iter (+ i 1) n (* res i)))))

;;; List selection functions.
;;; .section-id list-stuff
;;; This section demonstrates two aliased functions.


;;; Moved Higher order functions.

;; Filter lst by pred.
(define (filter pred lst)
 (reverse ;@a
  (filter-help pred lst '())))

; The function that does the real filtering.
; A tail recursive function
(define (filter-help pred lst res)
  (cond ((null? lst) res)  
        ((pred (car lst)) 
           (filter-help pred (cdr lst)  (cons (car lst) res))) 
        (else 
           (filter-help pred (cdr lst)  res))))