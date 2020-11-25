(lib-load
 "xml-in-laml/mirrors/xhtml10-transitional-mirror.scm")
(define html:title (xhtml10-transitional 'title))  

(define (album! album-ast)
 (let* ((title-ast (ast-subtree album-ast "title"))
        (artist-ast (ast-subtree album-ast "artist"))
        (dt-ast (ast-subtree album-ast "recordingdate"))
        (notes-ast (ast-subtree album-ast "recordingdate")))
  (write-html '(pp prolog)
   (html
    (head (html:title (ast-text artist-ast)_":"
                      (ast-text title-ast)))
    (body 'bgcolor (rgb-color-encoding white)
     (center (h1 (ast-text title-ast)))
     (h2 "Notes")
     (p (transform-ast-list (ast-subtrees notes-ast)
         (list (ast-of-type? 'element-name "trackref")
               (lambda (tf) (em (ast-text tf))))
         (list (ast-of-type? 'element-name "albumref") 
               (lambda (af) 
                (a 'href (ast-attribute af 'link)
                         (ast-text af))))))
     (table 'border "1"
      (tr (td "Album title") (td (ast-text title-ast)))
      (tr (td "Artist") (td (ast-text artist-ast)))
      (tr (td "Recording date")
          (td (if dt-ast
                  (ast-attribute dt-ast 'date)
                  "-")))
      (tr (td 'valign "top" "Catalog numbers")
          (td 
           (ol 
            (traverse-and-collect-all-from-ast
             album-ast
             (ast-of-type? 'element-name "catalogno") 
             (lambda (cn)
              (let ((fmt (ast-attribute cn 'format #f)))
               (li (ast-attribute cn 'label) 
                   (ast-attribute cn 'number)
                   (if fmt (list "("_ fmt _")") '())))))
           )
          ) )))))))