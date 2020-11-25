;   (leno-themes-front-matters,  begin-themes, theme*, end-themes)

; The practical use of this predicate is very limited, because other errors will occur before
; this predicate eventually fails.
; In fact, it is not necessary to use the top level leno form at all.
(define (leno-leno1-checker? contents)
 (let ((contents-1 (if (list? contents) (filter (negate white-space-related?) contents) contents)))
   (cond ((not (list? contents-1))	; #f - no contents probably
	  (xml-add-problem! 
            (xml-enrich-error-message "The  leno  instance must at least have leno-front-matters, begin-notes, end-notes" contents-1)))

         ((and (list? contents-1) (null? contents-1))
          (xml-add-problem! 
            (xml-enrich-error-message "The  leno  instance must at least have leno-front-matters, begin-notes, end-notes" contents-1))) 

	 ((and (list? contents-1) (not (equal? (ast-element-name (first contents-1)) "leno-front-matters")))
           (xml-add-problem! 
            (xml-enrich-error-message "The  leno  instance must begin with a leno-front-matters element" contents-1)))

	 ((and (list? contents-1) (>= (length contents-1) 2) (not (equal? (ast-element-name (second contents-1)) "begin-notes")))
           (xml-add-problem! 
            (xml-enrich-error-message "The  leno  instance must have a begin-notes element after the leno-front-matters element" contents-1)))

	 ((and (list? contents-1)  (not (equal? (ast-element-name (last contents-1)) "end-notes")))
           (xml-add-problem! 
            (xml-enrich-error-message "The  leno  instance must have an end-notes element as the last element" contents-1)))

         ((and (list? contents-1) (>= (length contents-1) 3) (not (all-ast-member-of? (list "note-page") (cddr (butlast contents-1)))))
           (xml-add-problem! 
            (xml-enrich-error-message "The  leno  instance can only have note-page elements in between begin-notes and end-notes" contents-1)))

	 (else #t))))