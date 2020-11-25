; Top level file for Leno trails system. This is the file to load via laml-style when making trails.

; -----------------------------------------------------------------------------
; Loading of libaries:

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
    "Welcome to LENO trails - LAML " laml-version " - "
    "featuring the exact HTML4.0 loose mirror." )))


(leno-welcome)

; ---------------------------------------------------------------------------------------------------
; The directory in which the LENO Scheme software resides:
(define leno-software-directory (string-append laml-dir "styles/lecture-notes/"))

(define (leno-software-dir-file . x) (string-append leno-software-directory (list-to-string x "")))

(define note-source-directory (startup-directory))

(define page-name (source-filename-without-extension))

; which lecture - relevant for the author if there are more chapters
(define current-lecture 1)

; Do not show lecture numbers - there is only one
(define lecture-number #f)

(define (cross-reference-location-hints location-list)  ; No location hints 
  "")

(define note-contents-description "A trail of LENO material")

(define (meta-tag-clauses)
   (list (list 'http-equiv "Content-Type" 'content "text/html; charset = ISO-8859-1")  
         (list 'name "Generator" 'content "LAML")
         (list 'name "description" 
                     'lang (meta-language language-preference)
                     'content note-contents-description)
   )
)

(define front-index? #f)  

(define note-abstract "??")

(define course-home-url "???")



; ---------------------------------------------------------------------------------------------------

(define (front-title . t) (cons 'front-title t)
(define (front-author . a) (cons 'front-author a))
(define (front-affiliation . a) (cons 'front-affiliation a))
(define (front-abstract . a) (cons 'front-abstract a))

(define (leno-trail-front-matters . properties)
 (let ((get-constituent 
          (lambda (tag-name) 
            (let ((res (find-in-list (lambda (y) (eq? (car y) tag-name)) contents-list)))
              (if res res "???"))))

	 (global-define (lambda (symbol expr) (eval-cur-env (list 'define symbol expr))))
      )

  (let ((scheme-prefix-file (defaulted-get-prop 'scheme-prefix properties #f))
        (scheme-suffix-file (defaulted-get-prop 'scheme-suffix properties #f))
        (the-title (get-constituent 'front-title))
        (the-author (get-constituent 'front-author))
        (the-affiliation (get-constituent 'front-affiliation))
        (the-front-abstract (get-constituent 'front-abstract))
       )
   (load (in-startup-directory scheme-prefix-file))
   ; ...
   (load (leno-software-dir-file "lecture-notes-kernel.scm"))
   (load (in-startup-directory scheme-suffix-file)))))



(define (begin-trail) 'empty)
(define (end-trail) 'empty)




