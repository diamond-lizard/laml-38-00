(load (in-startup-directory "txt-mirror.scm"))

(define (p->text p-list length add-attribute?)
 (let ((count-attr
         (if add-attribute? 
             (list 'count (as-string (- length 1)))
             '())))
  (cond ((= length 0) '())
        ((= length 1) 
         (text count-attr (ast-text (car p-list))))
        ((= length 2)
         (let ((p1 (car p-list))
               (p2 (cadr p-list)))
           (text count-attr (ast-text p1) "and"
                 (text (ast-text p2)))))
        ((> length 2)
         (text count-attr
           (ast-text (car p-list)) _ ","
             (p->text (cdr p-list) (- length 1) #f)))
        (else 
         (laml-error "unexpected length" length)))))


(define (purchase! ast)
 (let* ((p-clauses (find-asts ast "p"))
        (p-count (length p-clauses))
       )
  (write-html '(pp) (p->text p-clauses p-count #t)
   (in-startup-directory
    (string-append (source-filename-without-extension)
                    "." "xml")))))