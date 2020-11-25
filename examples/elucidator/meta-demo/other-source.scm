;; Return a list of pairs of elements from lst1 and lst2.
;; In other words, return an association list with keys from lst1 and values from lst2.
;; The list is as long as the shortest of lst1 and lst2.
(define (pair-up lst1 lst2)
  (pair-up-1 lst1 lst2 '()))

(define (pair-up-1 lst1 lst2 res)
  (cond ((or (null? lst1) (null? lst2)) (reverse res))
        (else (pair-up-1 (cdr lst1) (cdr lst2) (cons (cons (car lst1) (car lst2)) res)))))
 

(define key-list '(a b c d))
(define val-list '(1 2 3 4))

; ::aref-assignment::
; definition and assignment of an-a-list.
(define an-a-list '())
(set! an-a-list (pair-up key-list val-lst))

