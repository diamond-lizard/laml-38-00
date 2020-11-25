;;; Generation of list selector functions. Just a sample library scheme for the time example.
;;; As an alternative to using car, cadr etc. we provide for generation of more general list selector functions.

;; Returns a function, which selects element number n in a list.
;; The second parameter, which is optional, is used for error message purposes.
;; In general, this parameter should be a string corresponding to the name of the selector function.
;; If the second parameter is given, we check whether the list is long enough for selection.
;; If not, we give a decent error message. We recommend use of the second parameter in order to
;; avoid meaningless error messages.
;; The first element is number 1.
;; (make-selector-function 1) corresponds to car, (make-selector-function 2) corresponds to cadr, etc.
(define (make-selector-function n . selector-name)
 (let ((selector-name-1 (if (null? selector-name) #f (car selector-name))))
   (if selector-name-1
       (lambda (lst) 
         (let ((lgt (length lst)))
            (if (> n lgt)
                (display-error (string-append "The selector function " (as-string selector-name-1) ": " 
                               "The list "  (as-string lst) " is is too short for selection. "
                               "It must have at least " (as-string n) " elements."
                               ))
                (list-ref lst (- n 1)))))
       (lambda (lst) (list-ref lst (- n 1))))))

;; Return the first element of a list
(define first car)

;; Return the second element of a list
(define second cadr)

;; Return the third element of a list
(define third caddr)

;; Return the fourth element of a list
(define fourth cadddr)

;; Return the fifth element of a list
(define fifth (make-selector-function 5))

;; Return the sixth element of a list
(define sixth (make-selector-function 6))

;; Return the seventh element of a list
(define seventh (make-selector-function 7))

;; Return the eighth element of a list
(define eighth (make-selector-function 8))

;; Return the nineth element of a list
(define nineth (make-selector-function 9))