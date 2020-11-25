; Top level file of XML-in-LAML Leno Trail. This is the file to load via laml-style.
; Depends on styles/lecture-notes/lecture-notes-kernel.scm for part of the real work.

; The current directory:
(define leno-trail-software-dir (string-append laml-dir "styles/xml-in-laml/lecture-notes-trail/"))

; The LENO software directory
(define leno-software-directory (string-append laml-dir "styles/lecture-notes/"))


; Mirror loading:
(lib-load "html4.0-loose/basis.scm")
(lib-load "html4.0-loose/surface.scm")
(lib-load "html4.0-loose/convenience.scm")

(lib-load "color.scm")
(lib-load "time.scm")
(lib-load "file-read.scm")

; ---------------------------------------------------------------------------------------------------
; Global settings - required by lecture-notes-kernel.scm

(define note-source-directory (startup-directory))
(define page-name (source-filename-without-extension))
(define current-lecture 1) ; redefined later
(define lecture-number #f)
(define front-index? #f)

(define (cross-reference-location-hints location-list)  ; No location hints 
  "")

(define (meta-tag-clauses)
   (list (list 'http-equiv "Content-Type" 'content "text/html; charset = ISO-8859-1")  
         (list 'name "Generator" 'content "LAML")
         (list 'name "description" 
                     'lang (meta-language language-preference)
                     'content "Trail of LENO Lecture")
   )
)

; ---------------------------------------------------------------------------------------------------
; Global trail data

(define trail-selection-list '())
(define the-trail-id #f)    ; assigned in leno-trail-front-matters!

; ---------------------------------------------------------------------------------------------------
; Definition of action procedures - 
; which must be known before Leno specific XML-in-LAML mirror loading.


(define (leno-trail-front-matters! front-ast)
  (let* ((global-define (lambda (symbol expr) (eval-cur-env (list 'define symbol expr))))

         (boolean-value-of 
          (lambda (x) (cond ((equal? (downcase-string x) "true") #t)
                            ((equal? (downcase-string x) "false") #f)
                            (else (laml-error "Unknown boolean val: " x)))))

         (content-aggregation (lambda (ast) (aggregated-contents (ast-subtrees ast))))

         (front-matters-attributes (propertylist-to-alist (ast-attributes front-ast)))

         (trail-id      (get 'trail-id front-matters-attributes))
	 (scheme-prefix (defaulted-get 'scheme-prefix front-matters-attributes #f))
	 (scheme-suffix (defaulted-get 'scheme-suffix front-matters-attributes #f))
         (course-home-url-attr-val (get 'course-home-url front-matters-attributes))
         (language-attr (defaulted-get 'language front-matters-attributes "english"))

         (front-title-txt
              (traverse-and-collect-first-from-ast
                front-ast (ast-of-type? 'element-name "front-title")
                content-aggregation))

         (front-author-txt
              (traverse-and-collect-first-from-ast
                front-ast (ast-of-type? 'element-name "front-author")
                content-aggregation))

         (front-affiliation-txt
              (traverse-and-collect-first-from-ast
                front-ast (ast-of-type? 'element-name "front-affiliation")
                content-aggregation))

         (front-abstract-txt
              (traverse-and-collect-first-from-ast
                front-ast (ast-of-type? 'element-name "front-abstract")
                content-aggregation))
        )

    (set! the-trail-id trail-id)

    ; PREFIX LOADING
    ; if there is a scheme prefix, it must define the variables lecture-sections, notes-title, note-abstract, and current-lecture
    (if scheme-prefix
        (begin
             (load (string-append note-source-directory scheme-prefix)) 
        )
        (begin  ; earlier just   (global-define 'lecture-sections '())
             (global-define 'lecture-sections `(list (list ,page-name ,title-constituent)))
             (global-define 'notes-title front-title-txt)  ; notes-title identical with title-constituent = lecture-title
             (global-define 'note-abstract "")               ; no note-abstract needed for the trail
             (global-define 'current-lecture 1)
           )
    )

    ; MAJOR SOFTWARE LOADING
    (load (string-append leno-software-directory "lecture-notes-kernel.scm")) 

    ; SUFFIX HANDLING
    (global-define 'lecture-title front-title-txt)
    (global-define 'lecture-author-info `(list ,front-author-txt ,front-affiliation-txt))
    (global-define 'lecture-abstract front-abstract-txt)
    (global-define 'course-home-url course-home-url-attr-val)
    (if language-attr (set! language-preference (as-symbol language-attr)))

    ; SUFFIX LOADING
    (if scheme-suffix
        (load (string-append note-source-directory scheme-suffix)))
  )
)


(define (begin-trail! ast) 
 'empty)

(define make-trail-selection-record list)
(define lecture-id-of-trail-selection-record (make-selector-function 1 "lecture-id-of-trail-selection-record"))
(define page-id-of-trail-selection-record (make-selector-function 2 "page-id-of-trail-selection-record"))

(define (page-select! select-ast)
 (let* (
        (select-attributes-plist (ast-attributes select-ast))
        (lecture-id (get-prop 'lecture-id select-attributes-plist))
        (page-id    (get-prop 'page-id    select-attributes-plist))
       )
   (set! trail-selection-list (cons (make-trail-selection-record (as-symbol lecture-id) (as-symbol page-id)) trail-selection-list))
))


(define (end-trail! ast)   
  (apply note-trail   ; note-trail is a kernel function - it does the real work of the trail construction
   (cons 
     the-trail-id
     (map 
      (lambda (trail-selection-record)
        (slide-select    ; slide-select is a kernel function
          (lecture-id-of-trail-selection-record trail-selection-record)
          (page-id-of-trail-selection-record trail-selection-record)))
      (reverse trail-selection-list)))))
 


(lib-load "xml-in-laml/xml-in-laml.scm")
(load (string-append leno-trail-software-dir "mirror/leno-trail-1-mirror.scm"))


  
; ---------------------------------------------------------------------
; Helping functions:

(define (aggregated-contents contents-list)
  (aggregated-contents-1 contents-list ""))

(define (aggregated-contents-1 contents-list res)
  (cond ((null? contents-list) res)
        ((cdata? (car contents-list)) 
           (aggregated-contents-1 (cdr contents-list) (string-append res (car contents-list))))
        ((forced-white-space? (car contents-list)) (aggregated-contents-1 (cdr contents-list) (string-append res " ")))
        ((element-structure? (car contents-list)) ; pertaining to HTML elements in LENO XML elements
           (aggregated-contents-1 (cdr contents-list) (string-append res (html-render (car contents-list)))))))

; ---------------------------------------------------------------------

(end-laml-loading)