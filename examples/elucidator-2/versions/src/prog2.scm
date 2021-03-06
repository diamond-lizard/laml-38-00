;; Zip the two lists lst1 and lst2 by means of z.
(define (zip-lists z lst1 lst2)
  (if (null? lst1)
      '()
      (cons 
        (z (car lst1) (car lst2))
        (zip z (cdr lst1) (cdr lst2)))))

;; Compose a list of functions to a single function.
;; Each function in the list takes a single parameter.
;; Handles the typical case of two functions manually to achieve
;; better efficiency.
;; .precondition f-list is a proper list of length ast least one.
(define (compose-functions . f-list)
  (cond ((= 1 (length f-list)) (car f-list)) ; @b
        ((= 2 (length f-list)) ; @c
	 (let ((f (car f-list)) 
	       (g (cadr f-list)))
	   (lambda (x) (f (g x)))))
        (else  (lambda (x)     ; @d
		 ((car f-list)
		  ((apply compose-functions (cdr f-list)) x))))))





; A helping function of generate-leq.
; Return the position of e in lst. First is 1
; compare with el-eq?
; if e is not member of lst return (+ 1 (length lst))
(define (list-index e lst el-eq?)
 (cond ((null? lst) 1)
       ((el-eq? (car lst) e) 1)
       (else (+ 1 (list-index e (cdr lst) el-eq?)))))  
