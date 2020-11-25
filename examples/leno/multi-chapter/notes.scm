(define note-abstract
  "This is just a very simple, quick and dirty example that shows how to make
   a lecture of two quick and dirty sections")


; The directory in which this file is located.
; Must be ended with a slash
(define note-source-directory (startup-directory))

(define notes-title "Sample notes")  
 
(define current-lecture 2) 

(define lecture-sections                              
  (list
    (list  "ch1"   "Chapter one title")
    (list  "ch2"   "Chapter two title")
   )
)
                                                      
(define (meta-tag-clauses)
   (list (list 'HTTP-EQUIV "Content-Type" 'content "text/html; charset = ISO-8859-1")  
         (list 'name "Generator" 'content "laml")
         (list 'name "description" 
                     'lang (meta-language language-preference)
                     'content "LENO Multi chapter example")
   )
)


(define course-home-url "../../../../index.html")    

(define author-home-url "http://www.cs.auc.dk/~normark/")    

(define note-download-url "")

(define (cross-reference-location-hints location-list)  
  "")


; --------------------------------------------------------------------------------------------------------

; Here it is possible to define useful scheme functions,
; for instance functions which abbreviate URL's to outside
; material. Such functions are highly recommended.
