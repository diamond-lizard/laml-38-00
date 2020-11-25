; This file will be read after the LENO software is loaded.
; Here you can redefine a number of LENO variables.

(define end-mark "; end note-page")

(define (man-entry clause-name)
  (string-append "../../lecture-notes.html" "#" clause-name))

; (define bibtex-files (list "../../../../../examples/bibtex/sample.lsp"))

(define (cross-reference-location-hints hint)
  (cond ((and (list? hint) (>= (length hint) 2) (eq? (car hint) 'internet-reference))
           (string-append " " (as-string (second hint))))
        (else "")))

; Return an abosolute URL to some meta lecture page
(define (course-absolute-url suffix)
  (string-append "http://www.cs.auc.dk/~normark/scheme/" "styles/xml-in-laml/lecture-notes/man/meta-example/" suffix))

(define lecturer-photo-list (list "kn1.gif" "kn2.gif"))
           
; Set to true when we record sound fragments:
(define show-and-speak-recording? #f)  