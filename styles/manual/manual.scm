; LAML Manual page style.
; The old and original frontend (ad hoc Scheme functions) with a new XHTML backend.
; The original LAML manual facility, using the old ad hoc HTML libraries, is
; available in original-manual.scm of the current directory.
;
; Loads manual-kernel.scm at the rear end of the source file.

; COPYRIGHT (c)  Kurt N�rmark, Department of Computer Science, Aalborg University, Denmark. 2003.
; normark@cs.auc.dk,  http://www.cs.auc.dk/~normark/

; Warning: If schemdoc.scm is loaded after manual.scm, and if manual facility is used
; thereafter, problems occur. There is a conflict, yet unknown, between manual.scm and schemedoc.scm

; ---------------------------------------------------------------------------------------------------
; LOADING

; if conservative-xhtml-loading do not unnecessarily reload xhtml10-transitional.
(if (eq? laml-load-variation 'conservative-xhtml-loading)
    (if (not (memq 'xhtml10-transitional (languages-in-use)))
        (lib-load "xml-in-laml/mirrors/xhtml10-transitional-mirror.scm"))   
    (begin
     (lib-load "xml-in-laml/xml-in-laml.scm")
     (lib-load "xml-in-laml/mirrors/xhtml10-transitional-mirror.scm"))
)

(lib-load "xhtml1.0-convenience.scm")

(lib-load "color.scm")
(lib-load "time.scm")
(lib-load "file-read.scm")

; ---------------------------------------------------------------------------------------------------
; Constants

; The full path the directory that hosts this scheme file.
(define manual-software-directory (string-append laml-dir "styles/manual/"))
(define manual-source-directory (startup-directory))

; ---------------------------------------------------------------------------------------------------

(define (set-manual-name name)
  (set! manual-name name))

(define (set-manual-author . author-list)
  (set! the-manual-author author-list))

(define (set-manual-abstract abstract)
  (set! the-manual-abstract abstract))

(define (set-manual-title title)
  (set! the-manual-title title))

(define (set-home-url url)
  (set! home-url url))

(define (set-manual-master-index url)
 (set! manual-master-index url))


; ---------------------------------------------------------------------------------------------------
;  TOP LEVEL FUNCTION

; Define a manual page with a variety of different elements
(define (manual-page id . elements)
  (set! manual-page-list 
        (cons (cons (list 'kind "manual-page") elements) manual-page-list))
)

;; Define a manual section with title and body (plain text)
(define (manual-section . elements)
  (set! manual-page-list
        (cons (cons (list 'kind "manual-section") elements) manual-page-list)))


; ---------------------------------------------------------------------------------------------------
; TAG GENERATION

(define (make-tag tagid)
  (lambda parameters (cons tagid parameters)))


(define title (make-tag 'title))
(define form (make-tag 'form))
(define pre-condition (make-tag 'pre-condition))
(define description (make-tag 'description))
(define parameters (make-tag 'parameters))
(define parameter (make-tag 'parameter))
(define example (make-tag 'example))
(define cross-references (make-tag 'cross-references))
(define reference (make-tag 'reference))
(define internal-references (make-tag 'internal-references))
(define comment (make-tag 'comment))
(define misc (make-tag 'misc))
(define returns (make-tag 'returns))
(define section-title (make-tag 'section-title))
(define section-body (make-tag 'section-body))

; XML-in-LAML relevant entries:
(define xml-in-laml-example-form (make-tag 'xml-in-laml-example-form))
(define content-model (make-tag 'content-model))
(define attributes (make-tag 'attributes))
(define attribute (make-tag 'attribute))

(define attribute-descriptions (make-tag 'attribute-descriptions))
(define attribute-description  (make-tag 'attribute-description ))

(load (string-append manual-software-directory "manual-kernel.scm"))
(load (string-append laml-dir "tools/dtd-parser/element-attribute-access.scm"))





