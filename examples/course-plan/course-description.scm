; Course description file
; The file must be loaded just after the style course-plan.

; Adjust as lecture pages get ready
(define last-lecture-number 3)

(define language-preference 'english)

(apply-course-plan-color-scheme course-plan-color-scheme-3)

(define course-title "Functional Programming in Scheme")

(define teacher-name "Kurt Nørmark")

(define course-dir (string-append laml-dir "examples/course-plan/"))

; The directory in which the indiviaul lecture descriptions are located.
(define course-plan-source-directory (string-append course-dir "lectures/"))

(define course-url-prefix "http://www.cs.auc.dk/~normark/???/")

; Return formatted links (an HTML string) which is presented at the bottom of a course-page
(define (standard-links)
 (letrec ((between-links (lambda () (con (space 5) "|" (space 5)))))
  (font-size 2
   (center
    (con (a-tag "http://www.cs.auc.dk/~normark/laml/" "The LAML home page") (br)
         (a-tag "http://www.cs.indiana.edu/scheme-repository/home.html" "The Scheme Repository") (br)
         (a-tag "http://www.cs.auc.dk/~normark/" "Kurt Nørmark"))))))

; Returns a list of links to be presented in the left most navigation frame of the course home page
(define (general-overview-links)
  '())

(define brief-course-title "SFP")

(define course-semester "Fall 1999")

(define brief-course-semester "F99")

(define normal-course-room "R123")


; ---------------------------------------------------------------------------------------------------------------
; TIMING ISSUES:

(define course-start (time-encode 2000 1 3 8 15 0))

; A list of lecture starting times. A lecture starting time is the time where either the exercises or the plenum session
; starts, depending on the model we follow.

; (define time-list (periodic course-start 5 (* 14 seconds-in-a-day)))

(define time-list 
  (list 
    course-start
   (time-encode 2000 2 4 8 15 0)
   (time-encode 2000 2 11 8 15 0)
   (time-encode 2000 2 18 8 15 0)
   (time-encode 2000 2 25 8 15 0)))


; ---------------------------------------------------------------------------------------------------------------
; CENTRAL COURSE DESCRIPTION:

(define lecture-description-list
     (let* (
       (ex-lgt (* 60 105))     ; exercise lengh
       (pause-lgt (* 60 15))   ; pause between exercises and plenum
       (for-lgt (* 60 105))    ; plenum length (talk).
       (ex-inf (list 0 ex-lgt))
       (pl-inf (list (+ ex-lgt pause-lgt) for-lgt)))

  (list
;         LECTURE-ID       SUBJECT-ID         EXERCISE-INFO            PLENUM-INFO                                  ROOM
    (list 'fp-intr         'fp-intr           ex-inf                   pl-inf                                       normal-course-room)
    (list 'functions       'functions         ex-inf                   pl-inf                                       normal-course-room)
    (list 'lists           'lists             ex-inf                   pl-inf                                       normal-course-room)
    (list 'higher-order    'higher-order      ex-inf                   pl-inf                                       normal-course-room)
    (list 'eval            'eval              ex-inf                   pl-inf                                       normal-course-room)
  )))

; ---------------------------------------------------------------------------------------------------
; Actual exercises take place in this lecture's lecture-slot
(define (actual-exercise-interval this-lecture-rec next-lecture-rec)
  (lecture-exercise-interval this-lecture-rec))


; ---------------------------------------------------------------------------------------------------------------
; SUBJECT ISSUES:

(define subject-list
 (list

   (list 'fp-intr  "Introduction to functional programming"
         "In this lecture we introduce functional programming, especially as a contrast to plain imperative programming")

   (list 'functions  "The function concept"
         "The concept of functions is central. Here we take a close look at this")

   (list 'lists  "The list concept"
         "Lists are classical data structures in functional programming. Here we introduce the original Lisp list concept ")

   (list 'higher-order  "Higher order functions"
         "Higher order functions accept functions are parameters, and they allow us to return a function as the result")

   (list 'eval  "Evaluation order"
         "Evaluation order is one of the more advanced aspects in this course")

   ))
