;;;; This small library contains a function which is able to make up a string,
;;;; such that it obtains an approximate, maximum linelength.
;;;; Lines which are indented one or more characters are NOT indented.
;;;; The only function of external relevance is the function called rebreak-string.<p>
;;;; This library requires the general library in order to work.
;;;; .title Reference Manual of String Make-up Facility

; May 2, 99: We have identified three variables which may depend on end of line conventions:
; eol-string-output, eol-char-input, drop-13.

;; The line length limit.
;; If a line exceeds linebreak-limit, break the line as soon as possible.
(define linebreak-limit 70) 

(define CR-char (as-string (as-char 13)))         
(define LF-char (as-string (as-char 10)))

; THESE VARIABLES MAY BE SYSTEM DEPENDENT:

(define eol-string-output (as-string (as-char 10)))   ; the end of line character string in the output
(define eol-char-input LF-char)                       ; the end of line character in the input
(define drop-13 #t)                                    ; if true we drop character 13 (Control M) entirely when reading input

; END OF SYSTEM DEPENDENT VARIABLES.


(define CR-13 CR-char) ; just for convenience

(define DOUBLE-CR (string-append eol-string-output eol-string-output))
(define CR-SPACE (string-append eol-string-output (as-string #\space)))


(define state-list '()) ; for debugging purposes
(define debugging-rebreak #f)

;; Make up the string str, and return a variant of it which
;; is rebroken to have a certain, approximate maximum linelength. 
;; Lines that start with one or more spaces are not rebroken.
;; The line length is determined by the variable linebreak-limit.
(define (rebreak-string str)
 (let* ((strlgt (string-length str))
        (res-str (make-string (* 2 strlgt) #\space)))
  (set! state-list '())
  (rebreak-string-1 str 0 strlgt res-str 0 0 0 'leftbound-line)))

(define (rebreak-string-1 instr inptr inlength outstr outptr out-linelength total-out-line-length current-state)
  (if (= inptr inlength)
      (substring outstr 0 total-out-line-length)
      (let* ((inch (string-ref instr inptr))
             (trans-res (rebreak-transition current-state inch out-linelength))
             (next-state (car trans-res))
             (new-out-linelength (if (or (eq? next-state 'start-of-line) (eq? next-state 'just-broken)) 0 (+ 1 out-linelength)))
             (toput (as-string (cdr trans-res)))
            )
       (if debugging-rebreak
            (set! state-list (cons (cons (as-string inch) next-state) state-list)))
       (copy-string-into! outstr outptr toput)   ; Before (??):      (put-into-string! outstr outptr toput)
       (rebreak-string-1 instr (+ 1 inptr) inlength outstr (+ outptr (string-length toput))
                         new-out-linelength (+ total-out-line-length (string-length toput))  next-state)
  )))



; In this version, we only rebreak lines which are leftbound:
; 
; states 
;   leftbound-line: we are reading a line which started at the left margin
;   leftbound-pending-break: we are reading a line which started at the left margin. Rebreak it as soon as possible
;   indented-line:  we are reading a line which was indented
;   cr-encountered:  we have encountered a CR, but the line may be continued
;   start-of-line: we are at the start of an output line

(define (rebreak-transition in-state ch linelength)
 (let ((char (as-string ch))
       (CR (as-string eol-char-input)))
   (cond 
         ((and (symbol? in-state) (eq? in-state 'leftbound-line))
            (cond ((and drop-13 (equal? char CR-13))                      (cons 'leftbound-line ""))
                  ((and (< linelength linebreak-limit) (equal? char CR))  (cons 'cr-encountered ""))
                  ((< linelength linebreak-limit)                         (cons 'leftbound-line char))
                  ((and (>= linelength linebreak-limit) (equal? char CR)) (cons 'cr-encountered ""))
                  ((>= linelength linebreak-limit)                        (cons 'leftbound-pending-break char))
                  (else                                                   (error "rebreak-transition error 1"))))


         ((and (symbol? in-state) (eq? in-state 'leftbound-pending-break))
            (cond ((and drop-13 (equal? char CR-13))                      (cons 'leftbound-pending-break ""))
                  ((equal? char " ")                                      (cons 'just-broken eol-string-output))
                  ((equal? char CR)                                       (cons 'start-of-line eol-string-output))  
                  (else                                                   (cons 'leftbound-pending-break  char))))

         ((and (symbol? in-state) (eq? in-state 'just-broken))
            (cond ((equal? char " ")                                      (cons 'just-broken ""))
                  (else                                                   (cons 'leftbound-line char))))

         ((and (symbol? in-state) (eq? in-state 'indented-line))
            (cond ((and drop-13 (equal? char CR-13))                      (cons 'indented-line ""))
                  ((equal? char CR)                                       (cons 'start-of-line eol-string-output))
                  (else                                                   (cons 'indented-line  char))))


         ((and (symbol? in-state) (eq? in-state 'start-of-line))
            (cond ((and drop-13 (equal? char CR-13))                      (cons 'start-of-line ""))
                  ((equal? char " ")                                      (cons 'indented-line char))
                  ((equal? char CR)                                       (cons 'leftbound-line eol-string-output))
                  (else                                                   (cons 'leftbound-line  char))))

         ((and (symbol? in-state) (eq? in-state 'cr-encountered))
            (cond ((and drop-13 (equal? char CR-13))                      (cons 'cr-encountered ""))
                  ((equal? char CR)                                       (cons 'start-of-line DOUBLE-CR))
                  ((equal? char " ")                                      (cons 'start-of-line CR-SPACE))
                  (else                                                   (cons 'leftbound-line (string-append " " char)))))



         (else                                                            (error "rebreak-transition error 2"))

  )))
    





