(lib-load
 "xml-in-laml/mirrors/xhtml10-transitional-mirror.scm")
(define html:title (xhtml10-transitional 'title))  

(define (album! album-ast)
 (let* ((title-ast (ast-subtree album-ast "title"))
        (artist-ast (ast-subtree album-ast "artist"))
        (dt-ast (ast-subtree album-ast "recordingdate"))
        (notes-ast (ast-subtree album-ast "notes")))
  (write-html '(pp prolog)
   (html
    (head (html:title  (ast-text artist-ast)_":"
                       (ast-text title-ast)))
    (body 'bgcolor (rgb-color-encoding white)
     (center (h1 (ast-text title-ast)))
     (h2 "Notes")
     (present-notes notes-ast)
     (present-summary album-ast title-ast artist-ast dt-ast)
)))))

(define (present-notes notes-ast)
  (p (laml-source-prepare
      (transform-ast-list (ast-subtrees notes-ast)
       (list (ast-of-type? 'element-name "trackref")
             (lambda (tf) (em (ast-text tf))))
       (list (ast-of-type? 'element-name "albumref")
             mklink)))))

(define (present-summary album-ast title-ast artist-ast dt-ast)
 (let* ((catalogno-asts (find-asts album-ast "catalogno"))
        (numbers (number-interval 1 (length catalogno-asts))))
  (table 'border "1"
   (tr (td "Album title") (td (ast-text title-ast)))
   (tr (td "Artist") (td (ast-text artist-ast)))
   (tr (td "Recording date")
       (td (if dt-ast (ast-attribute dt-ast 'date) "-")))
   (tr (td 'valign "top" "Catalog numbers")
       (td (ul (map catno catalogno-asts numbers)))))))

(define (catno cat-ast n)
 (let ((fmt (ast-attribute cat-ast 'format #f)))
  (li (as-string n) _ "." (ast-attribute cat-ast 'label)
      (ast-attribute cat-ast 'number)
      (if fmt (list "("_ fmt _")") '()))))

(define (mklink aref-ast) 
  (a 'href (ast-attribute aref-ast 'link)
     (ast-text aref-ast)))