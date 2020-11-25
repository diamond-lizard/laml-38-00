; The Quali-Quiz tool. CGI program

; Url parameters:
; quiz-filename: the full path to the quiz file which contains all data about the quiz.
; activation-mode: present or present-and-check.

; ----------------------------------------------------------------------------------------
; LOADING: 

(lib-load "cgi.scm")
(lib-load "encode-decode.scm")
(lib-load "html4.0-loose/basis.scm")
(lib-load "html4.0-loose/surface.scm")
(lib-load "html4.0-loose/convenience.scm")
(lib-load "color.scm")
(lib-load "time.scm")
(lib-load "file-read.scm")

(load (string-append quiz-software-dir "quiz-lib.scm"))



;; ----------------------------------------------------------------------------------------
; CGI TESTING AND IMAGE ACCESS STUFF.

(define cgi-testing #f)
(define test-file "tttt.html")

(define image-file-access 'net)
(set-image-file-path! 'net)

; ----------------------------------------------------------------------------------------
; URL PARAMETERS:

(define url-pars (extract-url-parameters ""))
 
; The name of the file which holds the quiz data. Full path.
(define quiz-filename (get 'quiz-filename url-pars))
 
; The URL which gives the language
(define language-used (defaulted-get 'language url-pars "danish"))

(define activation-mode (as-symbol (defaulted-get 'activation-mode url-pars "present")))

; ----------------------------------------------------------------------------------------
; ANNOTATION SUPPORT:

(define annotator-url-prefix "http://www.cs.auc.dk/~normark/cgi-bin/annotator/")

; The absolute path to the Annotator CGI directory.
; Only used if annotator-support? is #t.
(define annotator-cgi-directory "/user/normark/.public_html/cgi-bin/annotator/")

(define (annotator-url annotator-cgi-program key-list val-list)
  (string-append annotator-url-prefix annotator-cgi-program "?" (make-url-parameters key-list val-list)))

; Return a full path to an annotator cgi-bin directory file
(define (annotator-cgi-file final-path)  
  (string-append annotator-cgi-directory final-path))

(define annotator-id-separator-char #\$)

; Encode id-list to a dollar (or whatever) separated string suitable for URL parameter passing purposes
(define (encode-annotation-ids id-list)
  (list-to-string id-list (as-string annotator-id-separator-char)))

; Return the list of existing annotations (a-list format) of the given category
; Corresponds roughly to the function annotation-catalogue-list in annotator-lib.scm (modulo the filtering with non-categorial).
(define (list-of-submitted-annotations . decoded-id-list)
  (let ((all-data-path (annotator-cgi-file (string-append "data/" (list-to-string decoded-id-list "/") "/" "all.dat"))))
   (letrec ((non-categorial string?))
    (if (file-exists? all-data-path)
        (filter non-categorial (file-read all-data-path))
        '()))))

; ----------------------------------------------------------------------------------------
; INFORMATION DERIVED FROM THE URL PARAMETERS. ACTIONS CAUSED BY URL PARAMETERS.

(define quiz-file
    (if (file-exists? quiz-filename)
        (file-read quiz-filename)
        '()))

(define quiz-identification (quiz-id-of-quiz-file quiz-file))
(define quiz-title (quiz-title-of-quiz-file quiz-file))
(define quiz-list (quiz-list-of-quiz-file quiz-file))


(cond ((equal? language-used "danish") (set! language-preference 'danish))
      ((equal? language-used "english") (set! language-preference 'english))
      (else (error "Unknown language preference, as given in the URL parameter")))

; ----------------------------------------------------------------------------------------
; HANDLING OF FORM-INPUT:

(define cur-time (current-time))

(define form-a-list 
  (if (eq? activation-mode 'present-and-check)
      (map symbolize-key 
         (time-extend 
            (extract-form-input "")
            cur-time))
      '()))

; ----------------------------------------------------------------------------------------


(write-page
   "Quali-Quiz"
   (con

     (form-1 (quiz-url "show-quiz.cgi" 
                       (list 'quiz-filename 'activation-mode)
                       (list  quiz-filename 'present-and-check))

      (con 

       (font-1 2 black (b "QUIZ:")) (br)
       (font-1 6 quiz-title-color quiz-title) (p)
  
       (present-quiz quiz-list form-a-list)

       (p) (hr) (p)

       (submit (text-choice "Check dine svar" "Check your answers")) (p)

        (a-tag 
          (annotator-url "annotator.cgi"
 			(list 'annotation-id 'detail-level 'activation-mode 'language 'informative-title 'back-url)
 			(list (encode-annotation-ids (list "oop-quiz" (as-string quiz-identification)))
 			      "flat" "present-only" language-preference quiz-title
 			      (quiz-url "show-quiz.cgi" (list 'quiz-filename 'activation-mode) (list  quiz-filename 'present))
 			))
          (text-choice "Indsend eller læs kommentarer til denne quiz" "Submit or read comments to this quiz")
        )

      )

     )

     (font-size 2 quiz-footer)
  )
  quiz-background-color black blue blue

)

