
; transforms leno-css-styled to leno-css-class
(define contr1 (map (lambda (x) (list 'leno-css-class (second x) (third x)))
     (file-read "css-clauses" 1)))

; transforms leno-css-styled-extra to leno-css-class
(define contr2 (map (lambda (x) (list 'leno-css-class (second x) (third x) (fourth x)))
     (file-read "css-clauses" 2)))

(define contr3 (file-read "css-clauses" 3))

(define all (append contr1 contr2 contr3))

(define (rem-dup x)
  (remove-duplicates-by-predicate
     x
    (lambda (a b)
      (equal? (eval-cur-env x) (eval-cur-env y)))))

(define all-2 (define (rem-dup lst)
  (remove-duplicates-by-predicate
    lst
    (lambda (a b)
      (equal? (eval-cur-env a) (eval-cur-env b))))))

(define all-3 
  (sort-list all-2 
    (lambda (x y)
     (or
      (string<? (as-string (cadr (second x))) (as-string (cadr (second y))))
      (and (equal? (as-string (cadr (second x))) (as-string (cadr (second y))))
           (and (= (length x) 2) (= (length y) 2)))
      (and (equal? (as-string (cadr (second x))) (as-string (cadr (second y))))
           (and (> (length x) 2) (> (length y) 2))
           (string<? (as-string (cadr (third x))) (as-string (cadr (third y)))))))))
      
      
           

(define all-4
  (sort-list all-3
    (lambda (x y)
      (if (and (>= (length x) 3) (>= (length y) 3))
          (string<=? (as-string (cadr (third x))) (as-string (cadr (third y))))
          #t))))
