; ---------------------------------------------------------------------------------------------------
; QUIZ FUNCTIONS AND CONSTANTS

(define quiz-background-color white)

(define quiz-footer (string-append "Quali Quiz (c) er designet og programmeret af Kurt Nørmark med brug af "
                                   (a-tag "http://www.cs.auc.dk/~normark/laml/" (font-color black "LAML")) " teknologi."))

(define (quiz-url quiz-program key-list val-list)
  (string-append quiz-url-prefix quiz-program "?" (make-url-parameters key-list val-list)))

; ---------------------------------------------------------------------------------------------------
; COLORS:

(define quiz-title-color (make-color 151 0 0))
(define quiz-correct-color (make-color 0 200 0))
(define quiz-reasonable-color (make-color 0 168 84))
(define quiz-error-color (make-color 255 0 0))
(define quiz-neutral-color (make-color 129 129 129))
(define quiz-not-good-color (make-color 255 108 108))


; ---------------------------------------------------------------------------------------------------
; QUIZ SELECTOR FUNCTIONS

(define quiz-id (make-selector-function 1 "quiz-id"))
(define quiz-question (make-selector-function 2 "quiz-question"))
(define quiz-background-url (make-selector-function 3 "quiz-url"))
(define quiz-answers (make-selector-function 4 "quiz-answers"))

; selectors of a single answer:
(define quiz-answer-choice (make-selector-function 1 "quiz-answer-choice"))
(define quiz-answer-correcness (compose as-number (make-selector-function 2 "quiz-answer-selection")))
(define quiz-answer-clarification (make-selector-function 3 "quiz-answer-clarification"))

; selectors on the quiz file structure:
(define quiz-id-of-quiz-file (make-selector-function 1 "quiz-id-of-quiz-file"))
(define quiz-title-of-quiz-file (make-selector-function 2 "quiz-title-of-quiz-file"))
(define quiz-list-of-quiz-file (make-selector-function 3 "quiz-list-of-quiz-file"))

; ---------------------------------------------------------------------------------------------------

; QUIZ CONSTRUCTORS:

(define (make-quiz-entry id quest backgr-url answers)
  (list id quest backgr-url answers))

(define (make-answer-entry answer correctness clarification)
  (list answer correctness clarification))

; The constructor of the quiz file format.
; The result of this function is written to a file
(define (make-quiz-file quiz-id quiz-title quiz-entry-list)
  (list (as-symbol quiz-id) quiz-title quiz-entry-list))


; ---------------------------------------------------------------------------------------------------
; Quiz presentation.

(define (present-quiz quiz-list form-a-list)
  (list-to-string
     (map2 (lambda (quiz-entry n) (present-question quiz-entry n form-a-list)) quiz-list (number-interval 1 (length quiz-list)))
     (p)))

(define (present-question quiz-entry n form-a-list)
 (let* ((quest (quiz-question quiz-entry))
        (id (quiz-id quiz-entry))
        (answer-list (quiz-answers quiz-entry))
        (selected-answer-list (selected-answers form-a-list id))
        (back-url (quiz-background-url quiz-entry))
       )
  (con 
    (font-1 4 quiz-title-color (b (con (as-string n) ". " quest))) 

    ; background url
    (if (not (null? selected-answer-list))
        (con (br) (a-tag back-url (font-1 2 black "Baggrundsmateriale om dette spørgsmål")))
        "")

    (p)
    (indent-pixels 10
      (presentation-answer-table 
        (map2 (lambda (answer-entry m)
                (let ((checked? (memv m selected-answer-list)))
                  (present-possible-answer answer-entry (quiz-id quiz-entry) m (if checked? #t #f))))
              answer-list (number-interval 1 (length answer-list))))))))

; Present answer 
(define (present-possible-answer answer-entry quiz-id n checked?)
 (letrec ((uid (lambda (qid n) (string-append (as-string qid) "-" (as-string n)))))
  (presentation-answer-entry
     (checkbox (uid quiz-id n) checked?)
     (con
       (quiz-answer-choice answer-entry)
       (if checked?
           (con 
             (br)
             (b (how-correct (quiz-answer-correcness answer-entry))) (br)
             (em (quiz-answer-clarification answer-entry)))
           "")))))

(define (how-correct percent-number)
  (cond ((= 0 percent-number) (font-color quiz-error-color "Svaret er forkert"))
        ((and (> percent-number 0) (< percent-number 25)) (font-color quiz-not-good-color "Svaret er overvejende forkert"))
        ((and (>= percent-number 25) (<= percent-number 75)) (font-color quiz-neutral-color "Svaret er hverken rigtig eller forkert"))
        ((and (and (> percent-number 75) (< percent-number 100)))  (font-color quiz-reasonable-color "Svaret er rimeligt"))
        ((= 100 percent-number) (font-color quiz-correct-color "Svaret er korrekt"))))

(define presentation-answer-entry list)

(define (presentation-answer-table list-of-lists)
  (table-1 0
    (list 30 "*")
    (make-list 2 white)
    list-of-lists))

; ---------------------------------------------------------------------------------------------------
; FUNCTIONS FOR EXTRACTING SELECTED (CHECKED) ANSWERS FROM A FORM-A-LIST:

; Return a list of numbers corresponding to the selected answers to question question-id in the quiz.
(define (selected-answers form-a-list question-id)
 (map extract-answer-number-from-symbol
   (filter (matching-id? question-id) (map (compose as-string car) form-a-list))))

; Returns a predicate which determines whether question-id matches the prefix of a selection-key.
(define (matching-id? question-id)
  (lambda (selection-key)
    (let* ((question-id-str (string-append (as-string question-id) "-"))
           (lgt (string-length question-id-str))
           (selection-key-lgt (string-length selection-key))
          )
      (equal? (downcase-string (substring selection-key 0 (min lgt selection-key-lgt))) (downcase-string question-id-str)))))

; assume that symbol is of the form x-n, where n is a number.
; Return n (a number)
(define (extract-answer-number-from-symbol selection-key)
 (let* ((str (as-string selection-key))
        (lgt (string-length str))
        (pos (find-in-string-from-end str #\-)))
  (if pos
      (as-number (substring str (+ 1 pos) lgt))
      (error (string-append "Malformed selection key: " (as-string selection-key))))))


; ---------------------------------------------------------------------------------------------------











