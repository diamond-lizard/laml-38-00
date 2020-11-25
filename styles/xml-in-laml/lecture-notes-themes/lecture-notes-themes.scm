; Top level file of XML-in-LAML Leno Themes. This is the file to load via laml-style.
; Depends on styles/lecture-notes/lecture-notes-themes.scm for the real work.

; The current directory:
(define leno-themes-software-dir (string-append laml-dir "styles/xml-in-laml/lecture-notes-themes/"))

; The LENO software directory
(define leno-software-directory (string-append laml-dir "styles/lecture-notes/"))


; Mirror loading:
(lib-load "html4.0-loose/basis.scm")
(lib-load "html4.0-loose/surface.scm")
(lib-load "html4.0-loose/convenience.scm")

(define note-source-directory (startup-directory))

; --------------------------------------------------------------------
; Definition of action procedures - 
; which must be known before Leno specific XML-in-LAML mirror loading.


(define (leno-themes-front-matters! ast)
  (let* ((global-define (lambda (symbol expr) (eval (list 'define symbol expr) (interaction-environment))))

         (boolean-value-of 
          (lambda (x) (cond ((equal? (downcase-string x) "true") #t)
                            ((equal? (downcase-string x) "false") #f)
                            (else (laml-error "Unknown boolean val: " x)))))

         (front-matters-attributes (propertylist-to-alist (ast-attributes ast)))
         (author-mode-attr (as-boolean (defaulted-get 'author-mode front-matters-attributes #f)))
         (presentation-medium-attr (as-symbol (defaulted-get 'presentation-medium front-matters-attributes "web")))

	 (scheme-prefix (defaulted-get 'scheme-prefix front-matters-attributes #f))
	 (scheme-suffix (defaulted-get 'scheme-suffix front-matters-attributes #f))
         (language (as-symbol (defaulted-get 'language front-matters-attributes "english")))

         (slide-view (as-boolean (defaulted-get 'slide-view front-matters-attributes #t)))
         (annotated-slide-view (as-boolean (defaulted-get 'annotated-slide-view front-matters-attributes #f)))
         (aggregated-view (as-boolean (defaulted-get 'aggregated-view front-matters-attributes #f)))

         (show-and-speak-attr (defaulted-get 'show-and-speak front-matters-attributes #f))
         (sound-source-attr (defaulted-get 'sound-source front-matters-attributes #f))
         (speak-url-prefix-attr (defaulted-get 'speak-url-prefix front-matters-attributes #f))
         (speak-file-prefix-attr (defaulted-get 'speak-file-prefix front-matters-attributes #f))
         (make-print-page-attr (defaulted-get 'make-print-page front-matters-attributes #f))
         (make-cross-reference-section-attr (as-boolean (defaulted-get 'make-cross-reference-section front-matters-attributes #t)))

         (treat-svg-images-as-attr (defaulted-get 'treat-svg-images-as front-matters-attributes #f))

         (exercise-linking-attr (defaulted-get 'exercise-linking front-matters-attributes #t))
         (program-linking-attr (defaulted-get 'program-linking front-matters-attributes #t))

         (news-flash-string-attr (defaulted-get 'news-flash-string front-matters-attributes ""))
         (news-flash-level-attr (defaulted-get 'news-flash-level front-matters-attributes "0"))
         (news-flash-url-attr (defaulted-get 'news-flash-url front-matters-attributes #f))

         (course-home-url-attr (defaulted-get 'course-home-url front-matters-attributes #f))
         (author-home-url-attr (defaulted-get 'author-home-url front-matters-attributes #f))
         (note-download-url-attr (defaulted-get 'note-download-url front-matters-attributes #f))

         (use-doc-comments-attr (defaulted-get 'use-doc-comments front-matters-attributes #f))
         (doc-comment-prefix-attr (defaulted-get 'doc-comment-prefix front-matters-attributes #f))
  
         (program-text-font-size-attr (defaulted-get 'program-text-font-size front-matters-attributes "80%"))

        )

    ; PREFIX LOADING
    ; if there is a scheme prefix, it must define the variables lecture-sections, notes-title, note-abstract, and current-lecture
    (if scheme-prefix
        (load (string-append note-source-directory scheme-prefix)) ; should define lecture-sections and notes-title
        (laml-error "You must use a scheme prefix file. Add a scheme-prefix to the leno-themes-front-matters")
    )


    ; MAJOR SOFTWARE LOADING
    (load (string-append leno-software-directory "lecture-notes-themes.scm")) ; loads also lecture-notes-kernel.scm

    (set! themes-author-mode author-mode-attr)
    (set! themes-presentation-medium presentation-medium-attr)

    (set! make-slide-view? slide-view)
    (set! make-annotated-slide-view? annotated-slide-view)
    (set! make-aggregated-view? aggregated-view)

    ; No need to support keyboard navigation when producing paper edition
    (if (eq? 'paper themes-presentation-medium)
        (set! java-scripting #f))


    (if show-and-speak-attr 
	   (begin
	     (if show-and-speak-attr (set! show-and-speak? (boolean-value-of show-and-speak-attr)))
	     (if sound-source-attr (set! sound-source (as-symbol sound-source-attr)))
	     (if speak-url-prefix-attr (set! speak-url-prefix speak-url-prefix-attr))
	     (if speak-file-prefix-attr (set! speak-file-prefix speak-file-prefix-attr))
	     )
	   )

    (if make-print-page-attr
           (set! make-print-page? (boolean-value-of make-print-page-attr)))

    (set! make-cross-reference-section?  make-cross-reference-section-attr)

    (set! language-preference language)

    (if treat-svg-images-as-attr   ; assigns -kernel variable:
        (set! treat-svg-as (as-symbol treat-svg-images-as-attr))
        (set! treat-svg-as 'svg))

    (set! do-exercise-linking-in-themes? (as-boolean exercise-linking-attr))
    (set! do-program-linking-in-themes? (as-boolean program-linking-attr))

    (if news-flash-string-attr (set! news-flash-string news-flash-string-attr))             ; setting kernel variables
    (if news-flash-level-attr (set! news-flash-level (as-number news-flash-level-attr)))
    (if news-flash-url-attr (set! news-flash-url news-flash-url-attr))

    (set! course-home-url course-home-url-attr)
    (set! author-home-url author-home-url-attr)
    (if note-download-url-attr (set! note-download-url note-download-url-attr))

    ; SUFFIX LOADING
    (if scheme-suffix
        (load (string-append note-source-directory scheme-suffix)))

    (if use-doc-comments-attr
        (set! extract-and-present-end-of-line-doc-comments (as-boolean use-doc-comments-attr)))

    (if doc-comment-prefix-attr
        (set! end-of-line-doc-comment-prefix (as-string doc-comment-prefix-attr)))

    (set! program-text-font-size program-text-font-size-attr)  ; a text string, such as "80%"
    
  )) 


(define (begin-themes! ast) 
 (do-begin-themes))

(define (theme! theme-ast)
 (let* ((theme-children (filter (negate white-space-related?) (ast-subtrees theme-ast)))
        (old-theme-children (map leno-theme-children-new-to-old theme-children))
        (theme-attributes-alist (propertylist-to-alist (ast-attributes theme-ast)))
        (theme-id (get 'id theme-attributes-alist))
       )
   (apply do-theme (cons 'id (cons theme-id old-theme-children)))
))


(define (end-themes! ast)   
 (do-end-themes))

(define (leno-themes! ast)
 ; (set! the-theme-ast ast)
 'nothing
)


(lib-load "xml-in-laml/xml-in-laml.scm")
(load (string-append leno-themes-software-dir "mirror/leno-themes-1-mirror.scm"))


; ---------------------------------------------------------------------------------
; AST to old element conversion:

(define (leno-theme-children-new-to-old ast)
 (cond ((equal? "leno-element" (ast-element-name ast)) (leno-element-new-to-old ast))
       ((equal? "theme-text" (ast-element-name ast)) (leno-theme-text-new-to-old ast))
       ((equal? "theme-index-table" (ast-element-name ast)) (leno-theme-index-new-to-old ast))
       ((equal? "theme-side-box" (ast-element-name ast)) (leno-theme-side-box-new-to-old ast))
       (else (laml-error "leno-theme-children-new-to-old: Unknown element type:" (ast-element-name ast)))))

(define (leno-element-new-to-old leno-element-ast)
  (make-element-structure 
    (as-symbol (ast-element-name leno-element-ast))
    '()                                                ; The leno-element is empty
    (propertylist-to-alist (ast-attributes leno-element-ast))))

(define (leno-theme-text-new-to-old leno-theme-text-ast)
 (let ((contents-list (ast-subtrees leno-theme-text-ast)))
  (make-element-structure 
    (as-symbol (ast-element-name leno-theme-text-ast))
    (list (aggregated-contents contents-list))
    (propertylist-to-alist (ast-attributes leno-theme-text-ast)))))

(define (leno-theme-side-box-new-to-old leno-theme-side-box-ast)
 (let ((contents-list (ast-subtrees leno-theme-side-box-ast)))  
  (make-element-structure 
    (as-symbol (ast-element-name leno-theme-side-box-ast))
    (list (aggregated-contents contents-list))
    (propertylist-to-alist (ast-attributes leno-theme-side-box-ast)))))

(define (leno-theme-index-new-to-old leno-theme-index-ast)
 (let ((contents-list (ast-subtrees leno-theme-index-ast)))  ; supposed to be empty
  (make-element-structure 
    (as-symbol (ast-element-name leno-theme-index-ast))
    (list (aggregated-contents contents-list))
    (propertylist-to-alist (ast-attributes leno-theme-index-ast)))))




(define (aggregated-contents contents-list)
  (aggregated-contents-1 contents-list ""))

(define (aggregated-contents-1 contents-list res)
  (cond ((null? contents-list) res)
        ((cdata? (car contents-list)) 
           (aggregated-contents-1 (cdr contents-list) (string-append res (car contents-list))))
        ((forced-white-space? (car contents-list)) (aggregated-contents-1 (cdr contents-list) (string-append res " ")))
        ((element-structure? (car contents-list)) ; pertaining to HTML elements in LENO XML elements  --- ????
           (aggregated-contents-1 (cdr contents-list) (string-append res (html-render (car contents-list)))))))


  
; ---------------------------------------------------------------------

(end-laml-loading)
