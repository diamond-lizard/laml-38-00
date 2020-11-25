;;;; This is a simple demo LAML document style, written for the WWW 2002 paper 
;;;; 'Programmatic WWW authoring using Scheme and LAML'

; LAML Library loading
(lib-load "file-read.scm")
(lib-load "html4.01-transitional-validating/basis.scm")
(lib-load "html4.01-transitional-validating/surface.scm")
(lib-load "html4.01-transitional-validating/convenience.scm")
(lib-load "time.scm")
(lib-load "color.scm")

; Return a function which tags some information with tag-symbol
(define (tag-information tag-symbol)
  (lambda information (cons tag-symbol information)))

; Tag function generation
(define quiz-entry (tag-information 'quiz-entry))
(define question-formulation (tag-information 'question-formulation)) 
(define answers (tag-information 'answers))
(define answer (tag-information 'answer))
(define answer-formulation (tag-information 'answer-formulation))
(define answer-correctness (tag-information 'answer-correctness))
(define answer-clarification (tag-information 'answer-clarification))

;;; Quiz entry selectors
(define question-of-entry (make-selector-function 2))
(define answers-of-entry (make-selector-function 3))

;;; Question selector
(define formulation-of-question (make-selector-function 2))

;;; Answer list selector
(define answer-list-of-answers (make-selector-function 2))

;;; Answer selectors
(define answer-formulation-of 
  (compose second (make-selector-function 2)))

(define answer-correctness-of
   (compose second (make-selector-function 3)))

(define answer-clarification-of
   (compose second (make-selector-function 4)))

; Form the outer structure of a HTML page
(define (html-page ttl body-form)
 (html 
  (head  (title ttl))
  (body 
    body-form)))

; Render and write the quiz list q-lst to a HTML file.
(define (quiz q-lst)
 (let ((n (length q-lst)))
   (write-html '(raw prolog)
     (html-page 
      "Quiz Example"
      (con  
       (map present-quiz-entry q-lst (number-interval 1 n))
       (p))))))


; Present a single quiz entry qe, which is assigned to the number n.
(define (present-quiz-entry qe n)
 (let* ((question 
         (formulation-of-question (question-of-entry qe)))
        (answers
          (answer-list-of-answers (answers-of-entry qe)))
        (m (length answers)))
  (con (font-color red (b question)) (br)
       (map
           (lambda (a m) (present-answer a n m))
           answers (number-interval 1 m))
       (br))))

; Present a single answer a in quiz entry n.
; This answer is assigned to the number m.
(define (present-answer a n m)
 (let ((formulation (answer-formulation-of a))
       (answer-id (make-id n m)))
   (con (checkbox answer-id #f) 
        (horizontal-space 1)
        formulation (br))))

; Make an internal answer identification string based on two numbers.
(define (make-id n m)
  (string-append "a" "-" (as-string n) "-" (as-string m)))
        
; Has quiz-entry only correct or incorrect answering possibilities
(define (black-or-white-quiz-entry? quiz-entry)
  (let ((answer-list 
          (answer-list-of-answers (answers-of-entry quiz-entry)))
        (partial-correct-answer?
          (lambda (answer)
            (let ((n (answer-correctness-of answer)))
              (and (> n 0) (< n 100)))))
       )
    (null?
      (filter partial-correct-answer? answer-list))))

; Make a quiz entry from a list lst
(define (make-quiz-entry-from-list lst)
  (let* ((question (first lst))
         (a-lst (second lst)))
    (quiz-entry
       (question-formulation question)
       (answers (map make-answer-from-list a-lst)))))
       
; Make an answer entry from a list lst
(define (make-answer-from-list lst)
  (let ((fo (first lst))
        (co (second lst))
        (cl (third lst)))
    (answer 
     (answer-formulation fo)
     (answer-correctness co)
     (answer-clarification cl)))) 












