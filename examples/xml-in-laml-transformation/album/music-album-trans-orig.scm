(lib-load "xml-in-laml/mirrors/xhtml10-transitional-mirror.scm")

(define text-contents aggregated-ast-cdata-contents) 

(define html:title (xhtml10-transitional 'title))  

(define (album! album-ast)
 (let* ((title-ast (ast-subtree album-ast "title"))
        (artist-ast (ast-subtree album-ast "artist"))
        (recording-date-ast (ast-subtree album-ast "recordingdate"))

        (html-doc
         (html
          (head (html:title (text-contents artist-ast)_":"
                       (text-contents title-ast)))
          (body 'bgcolor (rgb-color-encoding white)
            (center (h1 (text-contents title-ast)))
            (h2 "Notes")
            (p (traverse-and-collect-first-from-ast album-ast 
                  (ast-of-type? 'element-name "notes") 
                  (lambda (notes)
                   (laml-source-prepare
                    (transform-ast-list (ast-subtrees notes)
                      (list (ast-of-type? 'element-name "trackref")
                            (lambda (tf) (em (text-contents tf))))
                      (list (ast-of-type? 'element-name "albumref") 
                            (lambda (af) 
			      (a 'href (get-prop 'link (ast-attributes af))
                                 (text-contents af)))))))))
            (table 'border "1"
             (tr (td "Album title") (td (text-contents title-ast)))
             (tr (td "Artist") (td (text-contents artist-ast)))
             (tr (td "Recording date")
                 (td (if recording-date-ast 
                        (get-prop 'date (ast-attributes recording-date-ast))
                        "-")))
             (tr (td 'valign "top" "Catalog numbers")
                 (td (ol (traverse-and-collect-all-from-ast
                           album-ast
                           (ast-of-type? 'element-name "catalogno") 
                           (lambda (cn)
                            (let ((format (defaulted-get-prop 'format (ast-attributes cn) #f)))
                              (li (get-prop 'label (ast-attributes cn)) 
                                  (get-prop 'number (ast-attributes cn))
                                  (if format (list "(" _ format _ ")") '()))))))))
            ))))
        )
   (write-html '(pp prolog) html-doc)))