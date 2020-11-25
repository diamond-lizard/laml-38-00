; Top level file of XML-in-LAML Leno. This is the file to load via laml-style.

; A Leno style which makes it easier and cleaner
; to use the XML-in-LAML Leno version. This style
; takes care of style loading from the lecture-notes
; directory, and it implements all the front matters stuff.

; ---------------------------------------------------------------------------------------------------
; The directory in which the LENO Scheme software resides:
(define leno-xml-in-laml-software-directory (string-append laml-dir "styles/xml-in-laml/lecture-notes/"))
(define (leno-xml-in-laml-software-dir-file . x) (string-append leno-xml-in-laml-software-directory (list-to-string x "")))

(define leno-software-directory (string-append laml-dir "styles/lecture-notes/"))
(define leno-xml-in-laml-software-directory (string-append laml-dir "styles/xml-in-laml/lecture-notes/"))
(define (leno-software-dir-file . x) (string-append leno-software-directory (list-to-string x "")))


;  ---------------------------------------------------------------------------------------------------
; LOADING:

(lib-load "xml-in-laml/xml-in-laml.scm")

(set! xml-check-language-overlap? #f)

; The name of the XML-in-LAML SVG language used in LENO. A symbol. Currently either svg10 or svg11.
(define svg-language 'svg11)

(load (string-append laml-dir "lib/xml-in-laml/mirrors/" (as-string svg-language) "-" "mirror" "." "scm"))
(set-xml-accept-only-string-valued-attributes-in svg-language #f) 

(if (and laml-load-variation (eq? laml-load-variation 'no-html-mirror))
    (begin #f) ; load no html mirror stuff
    (begin
      (lib-load "html4.0-loose/basis.scm")
      (lib-load "html4.0-loose/surface.scm")
      (lib-load "html4.0-loose/convenience.scm")))

; ---------------------------------------------------------------------------------------------------
; Welcome

(define (leno-welcome)
 (display-message 
  (string-append 
    "Welcome to LENO - LAML " laml-version " - "
    "featuring the validating XML-in-LAML LENO mirror" )))


(leno-welcome)

; --------------------------------------------------------------------
; Definition of action procedures - 
; which must be known before Leno specific XML-in-LAML mirror loading.

; The mirror function leno-front-matters call the action procedure leno-front-matters!, which in turn calls do-leno-front-matters:
; The function leno-front-matters returns the ast.
(define (leno-front-matters! ast)
 (do-leno-front-matters ast))

(define (note-page! ast)
 (do-note-page ast))

(define (begin-notes! ast) 
 (do-begin-notes))

(define (end-notes! ast)   
 (do-end-notes))
  
; ---------------------------------------------------------------------

(load (leno-xml-in-laml-software-dir-file "mirror/leno1-mirror.scm"))

; -----------------------------------------------------------------------------
; Currently only supports relatively simple single chapter materials.
; Fixed variables due this this limiation.
(define note-source-directory (startup-directory))

(define page-name (source-filename-without-extension))

; which lecture - relevant for the author if there are more chapters
(define current-lecture 1)

; Do not show lecture numbers - there is only one
(define lecture-number #f)

(define (cross-reference-location-hints location-list)  ; No location hints 
  "")

(define (meta-tag-clauses)
   (list (list 'http-equiv "Content-Type" 'content "text/html; charset = ISO-8859-1")  
         (list 'name "Generator" 'content "LAML")
         (list 'name "description" 
               'lang (meta-language language-preference)
               'content note-contents-description)
   )
)

(define front-index? #f)  ; No front index; not needed because there is only one chaper.

(define note-abstract "")

; ---------------------------------------------------------------------------

; A variable in which we capture the front matters stuff - intended for xml reverse enginering
(define leno-front-matters-element #f)

(define (do-leno-front-matters element-ast)
  (let ((contents-list (new-element-contents element-ast))
        (attr-list (new-element-attributes element-ast))
       )

   (set! leno-front-matters-element element-ast)
   (let ((get-constituent 
          (lambda (tag-name) 
            (let ((res (find-in-list (lambda (y) (and (element-structure? y) (eq? (new-element-tag y) tag-name))) contents-list)))
              (if res (aggregated-contents (new-element-contents res)) #f))))

	 (get-attribute
          (lambda (attr-name)
            (defaulted-get attr-name attr-list #f)))
 
	 (global-define (lambda (symbol expr) (eval-cur-env (list 'define symbol expr))))

	 (boolean-value-of 
          (lambda (x) (cond ((equal? (downcase-string x) "true") #t)
                            ((equal? (downcase-string x) "false") #f)
                            (else (laml-error "Unknown boolean val: " x)))))
        
	 )


     (let* ((title-constituent (get-constituent 'front-title))
	    (subtitle-constituent (get-constituent 'front-subtitle))
	    (author-constituent (get-constituent 'front-author))
	    (affiliation-constituent (get-constituent 'front-affiliation)) ; perhaps support more of these
	    (abstract-constituent (get-constituent 'front-abstract))

	    (scheme-prefix (get-attribute 'scheme-prefix))
	    (scheme-suffix (get-attribute 'scheme-suffix))

	    (course-home-url-attr (get-attribute 'course-home-url))
	    (author-home-url-attr (get-attribute 'author-home-url))
	    (note-download-url-attr (get-attribute 'note-download-url))

	    (note-contents-description-attr (get-attribute 'note-contents-description))
	    (slide-header-attr (get-attribute 'slide-header))
	    (trail-of-lecture-attr (get-attribute 'trail-of-lecture))
	    (language-attr (get-attribute 'language))
	    (show-and-speak-attr (get-attribute 'show-and-speak))
            (default-showing-time-attr (get-attribute 'default-showing-time))
            (additional-showing-time-attr (get-attribute 'additional-showing-time))
            (sound-source-attr (get-attribute 'sound-source))
            (speak-url-prefix-attr (get-attribute 'speak-url-prefix))
            (speak-file-prefix-attr (get-attribute 'speak-file-prefix))
            (show-and-speak-author-mode-attr (get-attribute 'show-and-speak-author-mode))
	    (exercise-model-attr (get-attribute 'exercise-model))
	    (mouse-advancement-attr (get-attribute 'mouse-advancement))
	    (word-index-attr (get-attribute 'word-index))
;	    (apply-css-styling-attr (get-attribute 'apply-css-styling))
            (css-prestylesheet-attr (get-attribute 'css-prestylesheet))
            (css-stylesheet-attr (get-attribute 'css-stylesheet))
            (css-stylesheet-copying-attr (get-attribute 'css-stylesheet-copying))
	    (logo-url-attr (get-attribute 'logo-url))   
	    (news-flash-string-attr (get-attribute 'news-flash-string))
            (news-flash-level-attr (get-attribute 'news-flash-level))
            (news-flash-url-attr (get-attribute 'news-flash-url))
	    (quiz-support-attr (get-attribute 'quiz-support))
	    (lecture-note-verbosity-level (get-attribute 'verbosity-level))
            (lecture-type-attr (get-attribute 'lecture-type))
            (process-all-lectures-attr (get-attribute 'process-all-lectures))
            (clean-html-directory-attr (get-attribute 'clean-html-directory))
            (theme-source-attr (get-attribute 'theme-source))
            (theme-auto-process-attr (get-attribute 'theme-auto-process))
            (trail-source-attr (get-attribute 'trail-source))
            (make-print-page-attr (get-attribute 'make-print-page))

            (slide-view-attr (get-attribute 'slide-view))
            (annotated-slide-view-attr (get-attribute 'annotated-slide-view))
            (aggregated-view-attr (get-attribute 'aggregated-view))
            (theme-view-attr (get-attribute 'theme-view))
            (primary-view-attr (get-attribute 'primary-view))
            (source-destination-delta-attr (get-attribute 'source-destination-delta))
            (html-pdf-delta-attr (get-attribute 'html-pdf-delta))

            (source-program-index-attr (get-attribute 'source-program-index))
            (source-index-page-attr (get-attribute 'source-index-page))

            (treat-svg-images-as-attr (get-attribute 'treat-svg-images-as))

            (word-index-type-attr (get-attribute 'word-index-type))
            (presentation-medium-attr (get-attribute 'presentation-medium))

            (use-doc-comments-attr (get-attribute 'use-doc-comments))
            (doc-comment-prefix-attr (get-attribute 'doc-comment-prefix))

            (use-note-page-importance-attr (get-attribute 'use-note-page-importance))
            (pdf-version-as-of-attr (get-attribute 'pdf-version-as-of)) 
  	   )

       ; PREFIX LOADING

       ; if there is a scheme prefix, it must define the variables lecture-sections, notes-title, note-abstract, and current-lecture
       (if scheme-prefix
           (load (string-append note-source-directory scheme-prefix)) ; should define lecture-sections, 
           (begin
             (global-define 'lecture-sections `(list (list ,page-name ,title-constituent)))
             (global-define 'notes-title title-constituent)  ; notes-title identical with title-constituent = lecture-title
             (global-define 'note-abstract "")               ; no note-abstract needed for single lecture material
             (global-define 'current-lecture 1)
           )  
       )

       
;      (global-define 'note-contents-description note-contents-description-attr)  ; moved to suffix loading part
       (global-define 'course-home-url course-home-url-attr)
       (global-define 'author-home-url author-home-url-attr)
       (global-define 'note-download-url (if note-download-url-attr note-download-url-attr ""))

  
       (laml-style "xml-in-laml/lecture-notes/lecture-notes-transition")


       ; SUFFIX LOADING
 
       (global-define 'note-contents-description note-contents-description-attr)

       (if scheme-suffix
	   (load (string-append note-source-directory scheme-suffix)))

       (set! front-index? (>= (length lecture-sections) 1))    ; thus, in reality always make the front index (change August 16, 2007).

       (if (= (length lecture-sections) 1)              ; a single lecture collection 
           (set! notes-subtitle subtitle-constituent))


       (if slide-header-attr (set! slide-header? (as-symbol slide-header-attr)))
       (if trail-of-lecture-attr (set! trail-of-lecture? (boolean-value-of trail-of-lecture-attr)))
       (if language-attr (set! language-preference (as-symbol language-attr)))

       (if show-and-speak-attr 
	   (begin
	     (if show-and-speak-attr (set! show-and-speak? (boolean-value-of show-and-speak-attr)))
	     (if default-showing-time-attr (set! default-showing-time (as-number default-showing-time-attr)))
	     (if additional-showing-time-attr (set! additional-showing-time (as-number additional-showing-time-attr)))
	     (if sound-source-attr (set! sound-source (as-symbol sound-source-attr)))
	     (if speak-url-prefix-attr (set! speak-url-prefix speak-url-prefix-attr))
	     (if speak-file-prefix-attr (set! speak-file-prefix speak-file-prefix-attr))
             (if show-and-speak-author-mode-attr (set! show-and-speak-recording? (boolean-value-of show-and-speak-author-mode-attr)))
	     )
	   )
 
  
       (set! lecture-title title-constituent)

       (if (> (length lecture-sections) 1)
           (set! lecture-number (calculate-lecture-number page-name lecture-sections))
           (set! lecture-number #f))

       (set! lecture-author-info (list author-constituent affiliation-constituent))   ; Earlier:   (copyright-owner author-constituent)
       (set! lecture-abstract abstract-constituent)    


       (if exercise-model-attr (exercise-model (as-symbol exercise-model-attr)))
       (if mouse-advancement-attr (set! mouse-advancement (as-symbol mouse-advancement-attr)))
       (if word-index-attr (if (boolean-value-of word-index-attr) (include-word-index) (exclude-word-index)))

       (if (and (or css-stylesheet-attr css-prestylesheet-attr) (not (blank-string? css-stylesheet-attr)))
	   (begin
	     (set! apply-css-styling? #t)
             (set! the-css-prestylesheet-name (as-string css-prestylesheet-attr))
             (set! the-css-stylesheet-name (as-string css-stylesheet-attr))
             (set! css-stylesheet-do-copying 
                   (if css-stylesheet-copying-attr
                       (boolean-value-of css-stylesheet-copying-attr)
                       #t))
           )
	   (begin
	     (set! apply-css-styling? #f)
             (set! the-css-prestylesheet-name #f)
             (set! the-css-stylesheet-name #f)
             (set! css-stylesheet-do-copying #t)
           )
       )
           
            
       (if quiz-support-attr (set! quiz-support? (boolean-value-of quiz-support-attr)))
       (if news-flash-string-attr (set! news-flash-string news-flash-string-attr))
       (if news-flash-level-attr (set! news-flash-level (as-number news-flash-level-attr)))
       (if news-flash-url-attr (set! news-flash-url news-flash-url-attr))
       (if logo-url-attr (set! logo-url logo-url-attr))
       (if (and lecture-note-verbosity-level (numeric-string? lecture-note-verbosity-level))
	   (set! lecture-note-verbose-mode (as-number lecture-note-verbosity-level)))
       (if lecture-type-attr 
           (if (equal? lecture-type-attr "side-track") (set! lecture-type 'side-track)))
       (if process-all-lectures-attr
           (set! do-process-all-lectures? (boolean-value-of process-all-lectures-attr)))
       (if clean-html-directory-attr
           (set! do-clean-html-directory? (boolean-value-of clean-html-directory-attr)))
       (if theme-source-attr
           (set! theme-source-mode (as-symbol theme-source-attr)))
       (if theme-auto-process-attr
           (set! theme-auto-process-mode (boolean-value-of theme-auto-process-attr)))
       (if trail-source-attr
           (set! trail-source-mode (as-symbol trail-source-attr)))

       ; views
       (if slide-view-attr
           (set! make-slide-view? (boolean-value-of slide-view-attr)))
       (if annotated-slide-view-attr
           (set! make-annotated-slide-view? (boolean-value-of annotated-slide-view-attr)))
       (if aggregated-view-attr
           (set! make-aggregated-view? (boolean-value-of aggregated-view-attr)))
       (if theme-view-attr
           (set! themes? (boolean-value-of theme-view-attr)))
       (if primary-view-attr
           (set! the-primary-view (as-symbol primary-view-attr)))

       (if make-print-page-attr
           (set! make-print-page? (boolean-value-of make-print-page-attr)))
       (if source-destination-delta-attr
           (set! source-destination-delta-path-fragment source-destination-delta-attr)
           (set! source-destination-delta-path-fragment "html/"))
       (if html-pdf-delta-attr
           (set! html-destination-pdf-path-fragment html-pdf-delta-attr)
           (set! html-destination-pdf-path-fragment "../pdf/"))

       (if source-program-index-attr
           (set! source-program-index? (boolean-value-of source-program-index-attr))
           (set! source-program-index? #t))

       (if source-index-page-attr
           (set! source-level-index-page? (boolean-value-of source-index-page-attr))
           (set! source-level-index-page? #t))

       (if treat-svg-images-as-attr
           (set! treat-svg-as (as-symbol treat-svg-images-as-attr))
           (set! treat-svg-as 'svg))

       (if word-index-type-attr
           (set! word-index-type (as-symbol word-index-type-attr))
           (set! word-index-type 'slide))

       (if presentation-medium-attr
           (set! presentation-medium-of-theme (as-symbol presentation-medium-attr))
           (set! presentation-medium-of-theme 'web))

       (if use-doc-comments-attr
           (set! extract-and-present-end-of-line-doc-comments (boolean-value-of use-doc-comments-attr)))

       (if doc-comment-prefix-attr
           (set! end-of-line-doc-comment-prefix (as-string doc-comment-prefix-attr)))

       (if use-note-page-importance-attr
           (set! showing-importance? (boolean-value-of use-note-page-importance-attr )))

       (set! pdf-version-as-of pdf-version-as-of-attr)



     )
   )
  )
)

; Find and return the number of lecture-id relative to lecture-sections
; Non-registered lectures get number 0.
(define (calculate-lecture-number lecture-id lecture-sections)
 (let* ((lecture-id-string (as-string lecture-id))
        (lecture-id-list
         (map 
          (lambda (x) 
           (cond ((list? x) (as-string (car x)))
                 ((string? x) x)
                 ((symbol? x) (as-string x))
                 (else (laml-error "calculate-lecture-number: Cannot calculate lecture number"))))
          lecture-sections))
        (index (index-in-list-by-predicate lecture-id-list lecture-id-string equal?))
       )
   (if index
       (+ 1 index)
       0 ; (laml-error "calculate-lecture-number: Cannot find current lecture-id in lecture-sections - problems finding lecture-number")
   )
  )
)
  

; ---------------------------------------------------------------------------------------------------------------

; Low level element contructor.
; Make a normalized element structure (list). Name is the name of the element (either a string or symbol).
; contents is a list of contents forms, each on normalized form.
; attributes is an association list of attributes (key value pairs).
(define (make-element-structure name contents attributes)
  (make-ast (as-string name) contents (alist-to-propertylist attributes) 'double 'leno1)
  ; make-ast is an xml-in-laml constructor.
  ; earlier: (list 'element name contents attributes)
  )

; Is x an element structure.
(define element-structure? ast?)

;(define (element-structure? x)
;  (and (list? x) (= (length x) 4) (eq? (car x) 'element)))

; Element selector functions 
(define (new-element-tag ast)
 (as-symbol (ast-element-name ast)))

(define new-element-contents ast-subtrees)

(define (new-element-attributes ast)
  (propertylist-to-alist (ast-attributes ast)))


(define (aggregated-contents contents-list)
  (aggregated-contents-1 contents-list ""))

(define (aggregated-contents-1 contents-list res)
  (cond ((null? contents-list) res)
        ((cdata? (car contents-list)) 
           (aggregated-contents-1 (cdr contents-list) (string-append res (car contents-list))))
        ((forced-white-space? (car contents-list)) (aggregated-contents-1 (cdr contents-list) (string-append res " ")))
        ((element-structure? (car contents-list)) ; pertaining to HTML elements in LENO XML elements
           (aggregated-contents-1 (cdr contents-list) (string-append res (html-render (car contents-list)))))))



; -----------------------------------------------------------------------------
; The XML-in-LAML parameter handling - inspired by the HTML4.0 surface mirror.

(define (sort-element-parameters parameters tag-name)
 (sort-element-parameters-1 parameters parameters '() '() tag-name))

(define (sort-element-parameters-1 original-parameters parameters contents attributes tag-name)
 (letrec ((strip-initial-explicit-spaces 
	   (lambda (cl) (if (and (pair? cl) (eq? (car cl) explicit-space)) (strip-initial-explicit-spaces (cdr cl)) cl)))
          (ast? element-structure?)
          (cdata? string?)
          (contents-data? (lambda (x) (or (cdata? x) (ast? x))))
	  (list-non-ast? (lambda (x) (and (list? x) (not (ast? x)))))
         )
  (cond ((null? parameters) (cons (reverse (strip-initial-explicit-spaces contents)) (reverse attributes)))

        ; CONTENTS DATA -  no space after
        ((and (contents-data? (car parameters)) (not (null? (cdr parameters)))
              (boolean? (cadr parameters)) (not (cadr parameters)))              
                 (sort-element-parameters-1 original-parameters (cddr parameters) (cons (car parameters) contents) attributes tag-name))

        ; CONTENTS DATA -  space after
        ((and (contents-data? (car parameters)) (not (null? (cdr parameters)))
              (boolean? (cadr parameters)) (cadr parameters))                    ; space after
                 (sort-element-parameters-1 original-parameters
                                        (cddr parameters) (cons explicit-space (cons (car parameters) contents)) attributes tag-name))

        ; CONTENTS DATA -  space after
        ((contents-data? (car parameters))   ; space after
                 (sort-element-parameters-1 original-parameters 
                                        (cdr parameters) (cons explicit-space (cons (car parameters) contents)) attributes tag-name))


        ; LIST - no space after
        ((and (list-non-ast? (car parameters)) (not (null? (cdr parameters))) (boolean? (cadr parameters)) (not (cadr parameters)))
           (let* ((rec-res (sort-element-parameters-1 original-parameters (car parameters) '() '() tag-name))
                  (rec-contents (car rec-res))
		  (rec-attributes (cdr rec-res)) ; reversed
                 )  
              (sort-element-parameters-1 original-parameters
                                        (cddr parameters) 
                                        (append (reverse rec-contents) contents) (append (reverse rec-attributes) attributes) tag-name)))

        ; LIST - space after
        ((and (list-non-ast? (car parameters)) (not (null? (cdr parameters))) (boolean? (cadr parameters))  (cadr parameters))
           (let* ((rec-res (sort-element-parameters-1 original-parameters (car parameters) '() '() tag-name))
                  (rec-contents (car rec-res))
		  (rec-attributes (cdr rec-res)) ; reversed
                 )  
              (sort-element-parameters-1 original-parameters
                                        (cddr parameters) 
                                        (append (list explicit-space) (reverse rec-contents) contents) 
                                        (append (reverse rec-attributes) attributes) tag-name)))

        ; LIST - space after
        ((and (list-non-ast? (car parameters)))
           (let* ((rec-res (sort-element-parameters-1 original-parameters (car parameters) '() '() tag-name))
                  (rec-contents (car rec-res))
		  (rec-attributes (cdr rec-res)) ; reversed
                 ) 
              (sort-element-parameters-1 original-parameters
                                        (cdr parameters) 
                                        (append (list explicit-space)
                                                (reverse rec-contents)
                                                contents)
                                        (append (reverse rec-attributes) attributes) tag-name)))

        ; SYMBOL - attribute name 
        ((and (symbol? (car parameters)) (not (null? (cdr parameters))) (not (boolean? (cadr parameters))))
           (let ((attr-name (car parameters))
                 (attr-val (cadr parameters)))
             (sort-element-parameters-1 original-parameters (cddr parameters) contents (cons attr-val (cons attr-name attributes)) tag-name)))

        ; SYMBOL - error
        ((symbol? (car parameters)) (sort-error (string-append "Attributes of the " tag-name
                                    " tag must be of the form 'symbol \"value\". Only the symbol " (as-string (car parameters))
                                    " appears in last attribute") original-parameters))

        (else (sort-error (string-append "Error in an LAML " tag-name " tag.") original-parameters))
  )))

(define (sort-error message parameters)
 (let* ((max-str-lgt 80)  ; symbolic constant definition
        (parameters-1 (as-quoted-string parameters))
        (parameters-2 (if (> (string-length parameters-1) max-str-lgt) 
                        (string-append (substring parameters-1 0 (- max-str-lgt 1)) "...")
                        parameters-1)))
  (error
    (string-append
      message (as-string #\newline)
      "  FORM CONTENTS AND ATTR.: " parameters-2 (as-string #\newline)))))

(define (forced-white-space? x)
 (and (boolean? x) x))

(define white-space-related? boolean?)

(define cdata? string?)



; ---------------------------------------------------------------------------------------------------------------
; This procedure is called by tool procedure leno-xml in laml.scm.
; Handling of element structure from Leno-XML: The XML interface to LENO.
; Extracts front matters stuff and the note page elements, and processes these.
; Assume as a precondition that the structure of the parsed XML document is valid.
(define (leno-xml-process element-str)
 (let* ((leno-subelements (new-element-contents element-str))
        (front-matter-element 
          (find-in-list (lambda (el) (eq? 'leno-front-matters (new-element-tag el))) leno-subelements))
        (front-matter-list (make-front-matter-list front-matter-element))
        (note-page-list 
          (filter (lambda (el) (eq? 'note-page (new-element-tag el))) leno-subelements))
       )
   (apply leno-front-matters front-matter-list)
   (begin-notes)
   (for-each 
     proces-note-page-element 
     note-page-list)
   (end-notes)))

(define default-importance 0)

(define (proces-note-page-element note-page-element)
  (let* ((note-page-id (as-symbol (get 'id (new-element-attributes note-page-element))))
         (note-page-importance (defaulted-get 'importance (as-number (new-element-attributes note-page-element)) default-importance)) 
	 (new-style-elements (filter (compose not forced-white-space?) (new-element-contents note-page-element)))
	 (new-style-attributes (new-element-attributes note-page-element)) ; not used
	 (old-style-elements (map new-style-element-to-old-style-element new-style-elements))
	)
    (set! new-note-page-list (cons note-page-element new-note-page-list))
    (apply original-note-page (cons note-page-importance (cons note-page-id old-style-elements)))))


(define (make-front-matter-list front-matter-element)
  (let ((contents-list (new-element-contents front-matter-element))
        (attr-list (new-element-attributes front-matter-element)))
   (append
    contents-list
    (alist-to-propertylist attr-list))))

(end-laml-loading)