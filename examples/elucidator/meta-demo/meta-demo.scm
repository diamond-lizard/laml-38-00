;;;; A sample source file


;; The fak function, n!
(define (fak n)
  (if (= n 0)   ; @a
      1         ; @b
      (* n (fak (- n 1))) ; @c
  )
)

;; Return the list of the first n elements of lst.
;; If n < (length lst) just return lst
(define (list-prefix lst n)
  (if (< (length lst) n)
      lst
      (list-prefix-1 lst n)))

(define (list-prefix-1 lst n)
  (if (= n 0)
      '()
      (cons (car lst) (list-prefix-1 (cdr lst) (- n 1)))))


