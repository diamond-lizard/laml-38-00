; Top level file of the original Leno system. This is the file to load via laml-style.

; This is the original LENO interface - the ad hoc Scheme interface.
; Besides a tiny surface level it loads lecture-notes-kernel.scm
; which contains the bulk amount of LENO software.

; -----------------------------------------------------------------------------
; Loading of libaries:

; general.scm is loaded by laml.scm
(lib-load "cgi.scm")
(lib-load "encode-decode.scm")

(if (and laml-load-variation (eq? laml-load-variation 'no-html-mirror))
    (begin #f) ; load no html mirror stuff
    (begin
      (lib-load "html4.0-loose/basis.scm")
      (lib-load "html4.0-loose/surface.scm")
      (lib-load "html4.0-loose/convenience.scm")))


(lib-load "color.scm")
(lib-load "time.scm")
(lib-load "file-read.scm")

; ---------------------------------------------------------------------------------------------------
; Welcome

(define (leno-welcome)
 (display-message 
  (string-append 
    "Welcome to LENO - LAML " laml-version " - "
    "featuring the exact HTML4.0 loose mirror." )))


(leno-welcome)


; ---------------------------------------------------------------------------------------------------
; The directory in which the LENO Scheme software resides:
(define leno-software-directory (string-append laml-dir "styles/lecture-notes/"))

(define (leno-software-dir-file . x) (string-append leno-software-directory (list-to-string x "")))

; ---------------------------------------------------------------------------------------------------
; Old style surface layer:

(define (note-page id . elements)
  (apply original-note-page (cons id elements)))

; original-note-page is located in lecture-notes-kernel, because the new interface falls back on it.

; ---------------------------------------------------------------------------------------------------
; Leno clause function definitions:

(define (title . t) (make-element-optional-id 'title t))
(define (point . p) (make-element-optional-id 'point p))
(define (point . p) (make-element-optional-id 'point p))
(define (items . i) (make-element-optional-id 'items i))
(define (item . i) (make-element-optional-id 'item i))
(define (text . i) (make-element-optional-id 'text i))
(define (program . p) (make-element-optional-id 'program p))
(define (source-program . p) (make-element-optional-id 'source-program p))
(define (image . p) (make-element-optional-id 'image p))
(define (concepts . p) (make-element-optional-id 'concepts p))  ; now use concept-list
(define (example . p) (make-element-optional-id 'example p))
(define (opposing . p) (make-element-optional-id 'opposing p))
(define (comment . p) (make-element-optional-id 'comment p))
(define (syntax . p) (make-element-optional-id 'syntax p))
(define (slide-space . p) (make-element-optional-id 'slide-space p))
(define (tabular . p) (make-element-optional-id 'tabular p))
(define (note-text . p) (make-element-optional-id 'note-text p))
(define (slide-text . p) (make-element-optional-id 'slide-text p))
(define (cross-references . p) (make-element-optional-id 'cross-references p))
(define (exercise . p) (make-element-required-id 'exercise p))
(define (formulation . p) (make-element-optional-id 'formulation p))
(define (solution . p) (make-element-optional-id 'solution p))
(define (long-slide . p) (make-element-optional-id 'long-slide p))
(define (index-words . p) (make-element-optional-id 'index-words p))
(define (image-series . p) (make-element-optional-id 'image-series p))
(define (section-title . p) (make-element-optional-id 'section-title p))
(define (slide-image . p) (make-element-optional-id 'slide-image p))
(define (concept-list . p) (make-element-optional-id 'concept-list p))
(define (concept . p) (make-element-optional-id 'concept p))
(define (applet-program . p) (make-element-optional-id 'applet-program p))
(define (synopsis . p) (make-element-optional-id 'synopsis p))
(define (synopsis-item . p) (make-element-optional-id 'synopsis-item p))
(define (quotation . p) (make-element-optional-id 'quotation p))
(define (quiz . p) (make-element-optional-id 'quiz p))
(define (question . p) (make-element-optional-id 'question p))
(define (answers . p) (make-element-optional-id 'answers p))
(define (answer . p) (make-element-optional-id 'answer p))
(define (show-and-speak . p) (make-element-optional-id 'show-and-speak p))
(define (slide-part . p) (make-old-style-element 'slide-part p))              ; without possibility of id
(define (program-part . p) (make-old-style-element 'program-part p))
(define (exercise-part . p) (make-old-style-element 'exercise-part p))
(define (image-series-part . p) (make-old-style-element 'image-series-part p))
(define (lecturer-photos . p) (make-element-optional-id 'lecturer-photos p))
(define (lecturer-photos-and-logo . p) (make-element-optional-id 'lecturer-photos-and-logo p))
(define (elucidate . p) (make-element-optional-id 'elucidate p))
(define (elucidator-program-fragment . p) (make-element-optional-id 'elucidator-program-fragment p))
(define (splice-page-with . p) (make-element-optional-id 'splice-page-with p))
(define (splice-page-without . p) (make-element-optional-id 'splice-page-without p))
(define (side-track . p) (make-element-optional-id 'side-track p))
(define (svg-image . p) (make-element-optional-id 'svg-image p))


; (define (informal-reference title reference)
;   ; both strings
;   (list 'informal-reference title reference (list 'informal-reference)))
; 
; (define (internet-reference title url . location-hints)
;  (let ((lh (if (not (null? location-hints)) (car location-hints) '())))
;   (list 'internet-reference title url (cons 'internet-reference lh))))
; 
; (define (note-reference title lecture-id page-id . location-hints)
;  (let ((lh (if (not (null? location-hints)) (car location-hints) '())))
;   (list 'note-reference title lecture-id page-id (cons 'note-reference lh))))

(define (informal-reference . p) (make-element-optional-id 'informal-reference p))
(define (internet-reference . p) (make-element-optional-id 'internet-reference p))
(define (note-reference . p) (make-element-optional-id 'note-reference p))
(define (bibtex-reference . p)  (make-element-required-id 'bibtex-reference p))

; ---------------------------------------------------------------------------------------------------
; Element construction:

; Make an element in which the first element of rest is allowed to by an id symbol.
(define (make-element-optional-id tag rest)
  (cond ((null? rest) (make-element tag #f '() '()))
        ((symbol? (car rest)) (make-element tag (car rest) (cdr rest) '()))  
        ((not (symbol? (car rest))) (make-element tag #f rest '()))))

; Make an element in which the first element of rest is required to by an id symbol.
; The element will duplicate the id to both the id field and the first element of the contents.
(define (make-element-required-id tag rest)
  (cond ((null? rest) (error (string-append "An element with required id is not allowed to have empty rest: " (as-string tag))))
        ((symbol? (car rest)) (make-element tag (car rest) rest '()))
        (else (error (string-append "An element with required must have an id symbol as first constituent of rest: " (as-string tag))))))

(define make-old-style-element cons)

; Make an element. 
; Use with accessors element-tag, element-id, element-contents, 
; and element-attributes in lecture-notes-kernel.scm
(define (make-element tag id contents attributes)
  (list tag id contents attributes))

; ------------------------------------------------------------------

(load (leno-software-dir-file "lecture-notes-kernel.scm"))

(define begin-notes do-begin-notes)
(define end-notes do-end-notes)




