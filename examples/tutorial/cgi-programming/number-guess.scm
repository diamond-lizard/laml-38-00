;;;; A CGI demonstration program in Scheme with use of the LAML cgi library 
;;;; and HTML mirror libraries.


; ::laml-loading::
; LAML loading
 
(define laml-dir "/pack/laml/") ; @g
(load (string-append laml-dir "laml.scm"))


; ::initial-constants::
; Initial constants and functions

(define number-guess-url-prefix
  "http://www.cs.auc.dk/~normark/cgi-bin/number-guess/")

; Return an URL to a number guessing program 
; with url parameters
(define (number-guess-url program-name par-name-list 
         par-value-list)
 (string-append 
   number-guess-url-prefix
   program-name
   "?"
   (make-url-parameters par-name-list par-value-list)))



; ::additional-loading:: 
; Additional loading

(lib-load "cgi.scm") ;@a
(lib-load "encode-decode.scm") ;@b

; HTML mirror loading
(lib-load "html4.01-transitional-validating/basis.scm")
(lib-load "html4.01-transitional-validating/surface.scm")
(lib-load "html4.01-transitional-validating/convenience.scm")

; Other loadings
(lib-load "color.scm")
(lib-load "time.scm")

; ::other-settings::
; Setting testing conditions and access time

(define cgi-testing #f)
(define cur-time (current-time))

; ::url-parameters::
; Defining URL parameters

(define url-pars (extract-url-parameters))
(define language-preference 
    (as-symbol (defaulted-get 'language url-pars 'english)))
(define mode 
  (as-symbol (defaulted-get 'mode url-pars 'init)))

; -----------------------------------------------------------------------------

; A common part of both the init and the play page:
(define (guess-part secret-number)
 (con
  (p)
  (text-choice "Dit gæt: " "Your guess: ") 
  (horizontal-space 2) (text-line 'players-guess 3 "") ;@f
  (hidden-line 'secret-number (as-string secret-number)) ;@g
  (horizontal-space 6) (submit (text-choice "Gæt" "Guess")) ;@h
))

; The game init body
(define (game-init)
 (let ((secret-number (random 100)))  ;@a
  (body
   (form-1  ;@b
    (number-guess-url "number-guess.cgi"
       (list 'language 'mode) 
       (list language-preference "play")) 
    (con
     (font-1 5 red 
	     (text-choice 
	      "Velkommen til 'Gæt et tal'" 
	      "Welcome to 'guess a number'")) 

     (p) (hr) (p)

     (text-choice 
      "Gæt et tal mellem 0 og 100" 
      "Guess a number between 0 and 100")

     (guess-part secret-number) ;@c

     (p) (hr))))))

; The play body
(define (game-play)
 (let* ((form-a-list (map symbolize-key (extract-form-input))) ;@i
        (secret-number (as-number (get 'secret-number form-a-list)))
        (players-guess (as-number (get 'players-guess form-a-list)))
       )
  (body
   (form-1 ;@j
    (number-guess-url 
      "number-guess.cgi"
      (list 'language 'mode) (list language-preference "play"))
    (cond  ;@k
     ((> players-guess secret-number)
      (con
       (text-choice "Dit tal er for stort" "Your guess is too large")
       (guess-part secret-number) ;@c
       ))
     ((< players-guess secret-number)
      (con
       (text-choice "Dit tal er for lille" "Your guess is too small")
       (guess-part secret-number))) ;@d
     ((= players-guess secret-number)
      (con
       (font-color red (text-choice "Du ramte plet" "You got it")) (p)
       (a-tag ;@e
	(number-guess-url 
	 "number-guess.cgi"
	 (list 'language 'mode) (list language-preference "init"))
	(text-choice "Nyt spil" "New game")))))))))


; ::page-write::
; Writing the game page
(write-page
  (text-choice "Gæt et tal" "Guess a number")
  (cond ((eq? mode 'init) (game-init)) ;@b
        ((eq? mode 'play) (game-play)) ;@a
        (else (game-error)))
  white black blue blue)


(end)

