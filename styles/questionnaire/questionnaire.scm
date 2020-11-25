;;;; This is a LAML questionnaire style. It is based on the html4.0-loose libraries.
;;;; The main function is questionnaire. Besides this function and its helping functions,
;;;; a collection of useful quetionnaire functionality is collected in this style; Some
;;;; of these are used in the accompanying cgi programs.
;;;; A submitted questionnaire can be self-contained if you want; It means
;;;; that the questions and possible answers are submitted too via hidden lines.
;;;; The cgi programs rely on self contained submissions.
;;;; If a questions is identified with 'ID, the field 'ID$question' denotes the formulation
;;;; of the question. In case of multi-choice-question, single-choice-question, and rating-question
;;;; the questions themselves are submitted via hidden input lines with the answering
;;;; possibilities submitted as 'ID$answers' in a particular encoding.<p>
;;;;
;;;; The cgi programs in cgi-support (a subdirectory of the containing directory - not part of the LAML distribution)
;;;; holds a number of accompanying cgi programs which can accept and display submissions.
;;;; If you are interested in this part of the work, please contact normark@cs.auc.dk.<p>
;;;;
;;;; Important: Avoid the character '$' in both questions, answers, and ids.<p>
;;;;
;;;; Please consult the <a href="../../../examples/questionnaire/index.html">example questionnaire</a> for further information and insight.
;;;; .title Manual of the LAML questionnaire facility

(lib-load "cgi.scm")

(lib-load "html4.0-loose/basis.scm")
(lib-load "html4.0-loose/surface.scm")
(lib-load "html4.0-loose/convenience.scm")

; (lib-load "html.scm")
; (lib-load "html-v1.scm")

(lib-load "color.scm")
(lib-load "time.scm")
(lib-load "file-read.scm")

;;; Front matters.
;;; The functions and variables in this section are front matters to the real questionnaire.

;; Hidden answers. In some situations we might want (in an adhoc fashion) to
;; hide certain answers. The answers to the questions ids in hidden-ids are not shown. Intial value is the empty list.
(define hidden-ids '())

(define laml-source-file-name-without-extension (source-filename-without-extension scheme-system))
(define target-extension "html")

;; Is the submitted questionnaire self contained?
(define questionnaire-self-contained #t)

;; The questionnaire color scheme. 
;; A list of four colors: background, foreground, link, visited links colors.
(define questionnaire-color-scheme (list green1 black blue blue))

;; The font size of a questionnaire
(define questionnaire-font-size 3)

;; The width of an identification question field (in terms of number of characters)
(define identification-question-width 60)

;; The width of an free-style question field (in terms of number of characters)
(define free-style-question-width 60)

;; The height of an free-style question field (in terms of number of characters)
(define free-style-question-height 60)

;; Question emphasis function. This function is applied on any question text.
(define question-emphasis-function (lambda (q) (b (font-1 3 red q))))

; color scheme selectors (general)
(define background-of-color-scheme car)
(define foreground-of-color-scheme cadr)
(define link-of-color-scheme caddr)
(define vlink-of-color-scheme cadddr)

(define question-separator (vertical-space 1))

(define col1-width 200)
(define col2-width (if (<= questionnaire-font-size 2) 400 500))
(define sep-width 18)

; a list in which all id's are collected. Collected imperatively.
(define all-id-list '())

; a list of all question types. Collected imperatively.
(define all-type-list '())

; return a 'self identification'.
(define (self-id id type question . answers)
 (let ((answers-1 (if (null? answers) #f (car answers))))
  (set! all-id-list (cons id all-id-list))
  (set! all-type-list (cons type all-type-list))
  (con 
    (if questionnaire-self-contained
        (hidden-line (string-append (as-string id) "$" "question") question)
        "")
    (if (and questionnaire-self-contained answers-1)
        (hidden-line (string-append (as-string id) "$" "answers") (list-to-string answers-1 "$"))
        "")

  )
 )
)



;;; Questionnaire forms.
;;; In this section you will find the top-level questionnaire form together with possible subforms.

; laml version of questionnaire function
; a plain version of the same function is questionnaire-1

;; Write a questionnaire to a file.
;; The title is the title of the questionnaire.
;; receiving-program-url is the URL of the CGI program which processes the answers.
;; questionnaire-id is a symbol which identifies this particular questionnaire.
;; The question-list is a number of forms: identification-question, free-style-question, multi-choice-question, rating-question.
(define (questionnaire title receiving-program-url questionnaire-id . question-list)
 (let ((file-name  (string-append laml-source-file-name-without-extension "." target-extension)))
  (write-text-file
   (con 
    (apply questionnaire-1
            (append
              (list title receiving-program-url questionnaire-id)
              question-list))
    (p) (hr) (p) (font-size 2 (credits "Spørgeskema-systemet" "The questionnaire system"
                                       "http://www.cs.auc.dk/~normark/scheme/styles/questionnaire/man/questionnaire.html")))
    file-name)))

(define (questionnaire-1 title receiving-program-url questionnaire-id . question-list)
 (apply page 
   (append
     (list title
           (con (h 1 title)
                (form-1 receiving-program-url
                  (string-append
                   ; questionnaire id:
                   (if questionnaire-self-contained   
                      (hidden-line "questionnaire-id" (as-string questionnaire-id))
                      "")

                   ; all questions
                   (string-merge
                       question-list
                       (make-list (- (length question-list) 1) question-separator))

                   ; all question field names (ids):
                   (if questionnaire-self-contained
                      (hidden-line "all-ids" (list-to-string (map as-string (reverse all-id-list)) " "))
                      "")

                   ; all question types:
                   (if questionnaire-self-contained
                      (hidden-line "all-types" (list-to-string (map as-string (reverse all-type-list)) " "))
                      "")

                   question-separator
                   (submit (text-choice "Indsend skema" "Submit questionnaire")))

    )))
     questionnaire-color-scheme)))

(define (questionnaire-slice question form)
 (table-1
   0
   (list col1-width sep-width col2-width) 
   (make-list 3 (background-of-color-scheme questionnaire-color-scheme))
   (list
    (list (question-emphasis-function (font-size questionnaire-font-size question)) ""
	  form))))

;; Ask a question which requires a one line answer.
;; The answer to this question is meant to identify a particular answer relative to other answers.
;; Thus, typically ask for the name of the submitter in an identification question.
;; Id is the identification of this question (a symbol).
;; .internal-references "Alternative form" "free-style-question" 
(define (identification-question id question)
 (con
  (self-id id 'identification question)
  (questionnaire-slice question (text-line (as-string id) identification-question-width ""))
  
 )
)

;; Ask a question which requires a multi-line answer.
;; The id is the identification of this question (a symbol).
(define (free-style-question id question)
 (con
  (self-id id 'free-style question)
  (questionnaire-slice question (textarea-1 (as-string id) 
                       free-style-question-height free-style-question-width ""))
 )
)

;; Ask a multi-choice question.
;; Possibilities is a list of possible answers.
;; One or more answers can be selected.
;; The id is the identification of this question (a symbol).
(define (multi-choice-question id question possibilities)
 (con
  (self-id id 'multi-choice question possibilities)
  (questionnaire-slice question (choice-list-sequence possibilities id))
 )
)

(define (choice-list-sequence possibilities id)
 (let ((answer-numbers (number-interval 1 (length possibilities))))
    (string-merge
       (map2
         (lambda (possibility n)
            (con 
             (table-3 0
              (list 20 10 (- col2-width 35))
              (list
               (list
                (checkbox (string-append (as-string id) "-" (as-string n)))
                ""
                (font-size questionnaire-font-size possibility))))))
         possibilities answer-numbers)
       (make-list (- (length possibilities) 1) (br)))))


;; Ask a rating question.
;; The question can be answered by selecting exactly one of answers presenting in the rating-list.
;; Gives entry ("id" . "n"), where n is the entry selected.
;; The first is number 1.
;; The id is the identification of this question (a symbol).
;; Rating-list is a list of strings.
(define (rating-question id question rating-list)
 (con
  (self-id id 'rating question rating-list)
  (questionnaire-slice question (rating-list-sequence rating-list id))
 )
)

(define (rating-list-sequence rating-list id)
  (let ((rating-numbers (number-interval 1 (length rating-list))))
    (string-merge
       (map2 
         (lambda (rating-text rating-numb)
            (con (radio-button rating-numb id) (space 2) (font-size questionnaire-font-size rating-text)))
         rating-list rating-numbers)
       (make-list (- (length rating-list) 1) (br)))))
                     

;; As a question which can be answered by choosing one answer among a selection of possible answers.
;; Same form as rating question. However, the ordering of the answers is not important for single-choice-question.
;; .form (single-choice-question id question possible-answers)
(define single-choice-question rating-question)

;; Define the feedback message, which appears when the questionnaire form is submitted.
;; Do NOT use the characters '{', '}', and '£' in the message. These
;; characters are used for encoding purposes of embedded HTML anchor fragments - ad hoc stuff.
(define (feedback-message message)
  (hidden-line "questionnaire-feedback-message" (html-protect-adhoc message)))
  

; ---------------------------------------------------------------------------------------------------------------
;;; CGI relevant stuff.
;;; In sub-directory cgi-support of the directory holding the questionnaire.scm file, there is a number
;;; of CGI Scheme programs, which accepts answers and display the results of the questioning It is up to
;;; the administrator of the questionnaire facility (the LAML installer) to arrange that actual
;;; cgi programs are set up in an appropriate cgi-bin directory. These actual CGI programs can just load
;;; the Scheme programs in cgi-support.<p>
;;; The CGI stuff is not part of the LAML distribution.

;; show txt in a colored answer box
(define (answer-box txt)
 (color-frame-width (rebreak-string (font-size questionnaire-font-size txt)) (make-color 255 255 128) 900))

;; Return a symbol assembled from constituents
(define (syn-con . constituents)
  (as-symbol (list-to-string (map as-string constituents) "")))


;; Return a formatted answer with id and type.
(define (format-answer answer-record id type)
  (cond ((eq? type 'identification) (format-identification-answer id answer-record))
        ((eq? type 'free-style) (format-free-style-answer id answer-record))
        ((eq? type 'multi-choice) (format-multi-choice-answer id answer-record))
        ((eq? type 'rating) (format-rating-answer id answer-record))
        (else "???")))

(define (format-identification-answer id rec)
 (let* ((quest (defaulted-get (syn-con id "$" "question") rec "???"))
        (answ  (defaulted-get id rec "???"))
        (answ-1 (if (blank-string? answ) #f answ))
       )
    (questionnaire-slice
      quest
      (answer-box 
       (if (not (memq id hidden-ids))
        (if answ-1
            (pre answ-1)
            (i (text-choice  "Intet svar"  "No answer")))
       (i (text-choice  "Skjult svar" "Answer hidden"))) ))))

(define (format-free-style-answer id rec)
 (let* ((quest (defaulted-get (syn-con id "$" "question") rec "???"))
        (answ  (defaulted-get id rec "???"))
        (answ-1 (if (blank-string? answ) #f answ))
       )
    (questionnaire-slice
      quest
      (answer-box 
       (if (not (memq id hidden-ids))
        (if answ-1
            (pre answ-1)
            (i (text-choice  "Intet svar"  "No answer")))

        (i (text-choice  "Skjult svar" "Answer hidden"))) ))))

(define (format-multi-choice-answer id rec)
 (let* ((quest (defaulted-get (syn-con id "$" "question") rec "???"))
        (possible-answers (string-to-list (defaulted-get (syn-con id "$" "answers") rec "???") (list #\$)))
        (number-of-answers (length possible-answers))
        (checked-answers (map (lambda (n) (defaulted-get (syn-con id "$" n) rec #f)) (number-interval 1 number-of-answers)))
       )
  (questionnaire-slice
   quest
   (if (not (memq id hidden-ids))
    (answer-box 
     (con 
      (list-to-string
       (map2
         (lambda (answer checked)
           (if checked answer ""))
         possible-answers
         checked-answers) (p))
     (br))) 
    (i (text-choice  "Skjult svar" "Answer hidden"))) )))

(define (format-rating-answer id rec)
 (let* ((quest (defaulted-get (syn-con id "$" "question") rec "???"))
        (answer-number-selected (defaulted-get id rec #f))
        (possible-answers (string-to-list (defaulted-get (syn-con id "$" "answers") rec "???") (list #\$)))
       )
  (questionnaire-slice
   quest
   (if (not (memq id hidden-ids))
    (answer-box
     (if answer-number-selected
         (let* ((n (as-number answer-number-selected))
                (answer (list-ref possible-answers (- n 1))))
            answer)
         (i (text-choice  "Intet svar"  "No answer"))))
    (i (text-choice  "Skjult svar" "Answer hidden"))))))

;; Present the answer in answer-record
(define (show-an-answer answer-record)
 (let* ((answer-date (defaulted-get 'date answer-record "???"))
        (answer-time (defaulted-get 'time answer-record "???"))
        (all-ids (map as-symbol (as-list (get 'all-ids answer-record ))))
        (all-types (map as-symbol (as-list (get 'all-types answer-record))))
        (quest-id (get 'questionnaire-id answer-record)))
  (con answer-date ", " answer-time (p)
    (list-to-string
      (map2 (lambda (id type) (format-answer answer-record id type)) all-ids all-types)
      (p)))))



; Substitute angel brackets with curly brackets. Double quote characters are translated to dollars.
(define (html-protect-adhoc str)
 (transliterate
  (transliterate
   (transliterate
     str #\> (as-string #\}) ) 
    #\< (as-string #\{))
  #\" (as-string #\$)))

