; "(leno-front-matters, begin-notes, note-page*, end-notes)" 
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




;"((title|section-title),
;   (text | point | items | source-program | image | concepts | example | opposing | 
;    comment | syntax | tabular | note-text | slide-text | cross-references | exercise | 
;    index-words | image-series | slide-image | show-and-speak | lecturer-photos | 
;    lecturer-photos-and-logo | eluciate | splice-page-with | splice-page-without | 
;    side-track svg-image meta-text theme-text)*)"
(define (note-page-leno1-checker? contents)
 (let ((contents-1 (if (list? contents) (filter (negate white-space-related?) contents) contents)))
   (cond ((not (list? contents-1))	; #f - no contents probably
	  (xml-add-problem! 
            (xml-enrich-error-message "The  note-page  instance must at least have a title or section-title element" contents-1)))

         ((and (list? contents-1) (null? contents-1))
          (xml-add-problem! 
            (xml-enrich-error-message "The  note-page  instance must at least have a title or section-title element" contents-1))) 

	 ((and (list? contents-1) 
               (not (or (equal? (ast-element-name (first contents-1)) "title") 
                        (equal? (ast-element-name (first contents-1)) "section-title"))))
           (xml-add-problem! 
            (xml-enrich-error-message "The  note-page  instance must begin with either a title or a section-title" contents-1)))

	 ((and (list? contents-1) (equal? (ast-element-name (first contents-1)) "title"))
           (let* ((legal-el-names 
		   (list "text" "point" "items" "source-program" "image" "concepts" "example" "opposing" "comment"
                         "syntax" "tabular" "note-text" "slide-text" "cross-references" "exercise" "index-words"
                         "image-series" "slide-image" "show-and-speak" "lecturer-photos" "lecturer-photos-and-logo" 
                         "elucidate" "splice-page-with" "splice-page-without" "side-track" "slide-space" "quiz" 
                         "quotation" "synopsis" "applet-program" "concept-list" "svg-image" "flash-image" "meta-text" "theme-text"))
		 (immediate-constituents-ok (apply zero-or-more legal-el-names))
                )
             (if (not (immediate-constituents-ok (cdr contents-1))) 
                 (xml-add-problem! 
		  (xml-enrich-error-message 
                    (string-append "The element " (as-string xml-problem-element) " of a  note-page  is illegal")
                    contents-1))
                 #t)))

	 ((and (list? contents-1) (equal? (ast-element-name (first contents-1)) "section-title"))
           (let* ((legal-el-names 
		   (list "show-and-speak" "lecturer-photos" "lecturer-photos-and-logo" "splice-page-with"
                         "splice-page-without" "comment" "index-words" "quiz" "theme-text"))
		 (immediate-constituents-ok (apply zero-or-more legal-el-names))
                )
             (if (not (immediate-constituents-ok (cdr contents-1))) 
                 (xml-add-problem! 
		  (xml-enrich-error-message 
                    (string-append "The element " (as-string xml-problem-element) " of a  note-page  is illegal")
                    contents-1))
                 #t)))

	 (else #f))))

; "(row-widths, row*, annotation?)"
(define (tabular-leno1-checker? contents)
  (let ((contents-1 (if (list? contents) (filter (negate white-space-related?) contents) contents)))
   (cond ((not (list? contents-1))	; #f - no contents probably
	  (xml-add-problem! 
            (xml-enrich-error-message "The  tabular element cannot be empty: must at least have a row-widths element" contents-1)))

         ((and (list? contents-1) (null? contents-1))
          (xml-add-problem! 
            (xml-enrich-error-message "The  tabular element cannot be empty: must at least have a row-widths element" contents-1))) 

	 ((and (list? contents-1) (>= (length contents-1) 1) (not (equal? (ast-element-name (first contents-1)) "row-widths")))
           (xml-add-problem! 
            (xml-enrich-error-message "The first element of a  tabular  element must be a row-widths element" contents-1)))

	 ((and (list? contents-1) (> (length contents-1) 1) (equal? (ast-element-name (first contents-1)) "row-widths"))
           (if (equal? (ast-element-name (last contents-1)) "annotation")
               (if (not (all-ast-member-of? (list "row") (butlast (cdr contents-1))))
                   (xml-add-problem! 
		    (xml-enrich-error-message "The  tabular  element can only have row elements between row-widths and annotation" contents-1))
                   #t)
               (if (not (all-ast-member-of? (list "row") (cdr contents-1)))
                   (xml-add-problem! 
		    (xml-enrich-error-message "The  tabular  element can only have row elements after row-widths" contents-1))
                   #t)))

	 (else #f))))


(define (applet-program-leno1-checker? contents)
  #t)