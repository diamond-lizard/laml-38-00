; The LAML bikes style, including the transformaton to HTML.
; For tutorial and demo purposes.

(define bikes-mirror-dir 
  (string-append laml-dir
    "lib/xml-in-laml/mirrors/tutorials-and-demos/"))


; Load the share XML-in-LAML library
(lib-load "xml-in-laml/xml-in-laml.scm")

; ::xhtml-loading::
; XHTML miror loading
(lib-load "xml-in-laml/mirrors/xhtml10-transitional-mirror.scm")


; Define the action procedure
(define (bikes! ast)
 (let* ((bike-table-list  ; @a
          (traverse-and-collect-all-from-ast ast 
             (ast-of-type? 'element-name "bike") bike-table))
        (table-with-border (xml-modify-element table 'border "1")) ; @b
       )
  (write-html '(pp prolog)
   (html 
     (head (title "Bike Report"))
     (body
       (map table-with-border bike-table-list))))))


(define (bike-table bike-ast)
 (let ((frame-number 
          (traverse-and-collect-first-from-ast
            bike-ast (ast-of-type? 'element-name "frame")
            (attribute-getter 'frame-number)))
       (wheel-sizes 
          (traverse-and-collect-all-from-ast
            bike-ast (ast-of-type? 'element-name "wheel")
            (attribute-getter 'size)))
       (insurance-approved-list
          (traverse-and-collect-all-from-ast
            bike-ast (ast-of-type? 'element-name "lock")
            (compose as-boolean (attribute-getter 'insurance-approved))))
      )
  (list
    (tr (td "Frame number") 
        (td frame-number))
    (tr (td "Wheel sizes")
        (td (list-to-string wheel-sizes ", ")))
    (tr (td "Approved locks")
        (td (if (accumulate-right and-fn #t insurance-approved-list) "yes" "no"))))))


; Load the language-specific mirror and validation stuff
(load (string-append bikes-mirror-dir "bike-management-mirror.scm"))


        







