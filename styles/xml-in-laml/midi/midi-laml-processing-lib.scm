;;;; .title       Midi Function Library
;;;; .author      Kurt NÅ¯rmark
;;;; .affiliation Department of Computer Science, Aalborg University, Denmark
;;;; This is a library of common MIDI manipulation functions. 
;;;; It must be loaded together with the <a href= "midi-mirror.html">MIDI LAML mirror library</a>.
;;;; .laml-resource                 true
;;;; .css-prestylesheet             compact
;;;; .css-stylesheet                argentina
;;;; .css-stylesheet-copying        true
;;;; .scheme-source-linking         true 
;;;; .source-destination-delta      

; .schemedoc-dependencies  "man/midi-mirror.manlsp" "man/midi-laml-processing-lib.manlsp"
; schemedoc-dependencies  "man/midi-mirror.manlsp" 
; .source-file-dependencies "midi.scm" 


; ---------------------------------------------------------------------------------------------------------------
; Mirror parameter setting:

(set-xml-accept-only-string-valued-attributes-in 'midi #f)

; ---------------------------------------------------------------------------------------------------------------
; Issue a fatal error if deltaTime attributes is present in message-list. 
; operation is a parameter passed for error message purposes. 
(define (ensure-all-abstime-in! operation message-list)
  (let ((delta-time-messages 
           (traverse-and-collect-all-from-ast 
              message-list
              (lambda (x) (and (ast? x) (equal? (ast-element-name x) "NoteOn") (ast-attribute x 'deltaTime #f)))
              id-1)))
    (if (> (length delta-time-messages) 0)
        (laml-error "Only absTime mode is supported by" operation))))


; Issue a fatal error if absTime attributes is present in message-list. 
; operation is a parameter passed for error message purposes. 
(define (ensure-all-deltatime-in! operation message-list)
  (let ((delta-time-messages 
           (traverse-and-collect-all-from-ast 
              message-list
              (lambda (x) (and (ast? x) (equal? (ast-element-name x) "NoteOn") (ast-attribute x 'absTime #f)))
              id-1)))
    (if (> (length delta-time-messages) 0)
        (laml-error "Only deltaTime mode is supported by" operation))))


; ---------------------------------------------------------------------------------------------------------------
; Assume absTime:

(define (fuzzy-drums  message-list)
   (map fuzzy-drum-1 
        message-list 
        (append (cdr message-list) (list #f))
        (cons #f (butlast message-list))))

(define (fuzzy-drum-1 this-mes next-mes prev-mes)
  (cond ((drum-message? this-mes)
            (fuzzy-drum-message this-mes next-mes prev-mes))
        (else this-mes)))

(define (fuzzy-drum-message this-mes next-mes prev-mes)
 (let ((window-size (if (and this-mes next-mes prev-mes)
                        (max (- (as-number (ast-attribute next-mes 'absTime)) (as-number (ast-attribute this-mes 'absTime)))
                             (- (as-number (ast-attribute this-mes 'absTime)) (as-number (ast-attribute prev-mes 'absTime))))
                        0)))
  (cond 
        ((delete-message? this-mes) '())
        ((change-message-velocity? this-mes)
           (copy-ast-mutate-attributes this-mes 'velocity (as-int-string (between 0 127 (+ (as-number (ast-attribute this-mes 'velocity)) (delta-velocity))))))
        ((move-message? this-mes)
           (copy-ast-mutate-attributes this-mes 'absTime (as-int-string (+ (as-number (ast-attribute this-mes 'absTime)) (delta-move window-size)))))
        (else this-mes))))

(define (drum-message? mes)
  (or (equal? "9" (ast-attribute mes 'channel))
      (equal? "10" (ast-attribute mes 'channel))))


(define delete-frequency 5)
(define move-frequency 5)
(define velocity-change-frequency 20)
(define velocity-max-change 40)


(define (delete-message? mes)
  (let ((r (random 100)))
    (< r delete-frequency)))

     

(define move-r 0)

(define (move-message? mes)
 (let ((r (random 100)))
   (if (< r move-frequency)
       (begin (set! move-r r) #t)
       (begin (set! move-r 0) #f))))

(define (delta-move window-size)
    (if (even? move-r)
        (- (to-int (* (/ move-r 100) window-size)))
        (+ (to-int (* (/ move-r 100) window-size)))))
          


(define vel-r 0)

(define (change-message-velocity? mes)
 (let ((r (random 100)))
   (if (< r velocity-change-frequency)
       (begin (set! vel-r r) #t)
       (begin (set! vel-r 0) #f))))

(define (delta-velocity)
  (to-int 
    (if (even? vel-r)
        (- (* (/ vel-r 100) velocity-max-change))
        (+ (* (/ vel-r 100) velocity-max-change)))))


; ---------------------------------------------------------------------------------------------------------------

;;; .section-id midi-mes-predicates
;;; Midi message predicates.

;; Is x a NoteOn midi message?
;; .parameter x Not constrained.
(define (NoteOn? x)
  (and (ast? x) (equal? (ast-element-name x) "NoteOn")))

;; A higher-order function that returns a NoteOn predicate that only accepts NoteOn midi messages that belong to channels.
;; .parameter channels A list of channel numbers (elements are integers between 1 and 16).
;; .returns A NoteOn predicate that accepts NoteOn midi messages in the the given channels.
(define (NoteOnCh? channels)
  (lambda (x)
    (if (NoteOn? x)
        (let ((ch (ast-attribute x 'channel #f)))
           (if ch
               (member (as-number ch) channels)
               #f))
        #f)))



;; Is x a Meta midi message of type?
;; .form (Meta? x [type])
;; .parameter x Any data, but typically some midi AST.
;; .parameter type A meta type attribute (an integer). If not provided, the type does not matter.
(define (Meta? x . optional-parameter-list)
  (let ((type-1 (optional-parameter 1 optional-parameter-list "*")))
    (and (ast? x) (equal? (ast-element-name x) "Meta")
         (if (equal? type-1 "*")
             #t
             (= type-1 (as-number (ast-attribute x 'type #f)))))))

;; Is x a ProgramChange midi message in channel?
;; .form (ProgramChange? x [channel])
;; .parameter x Any data, but typically some midi AST.
;; .parameter channel A channel (an integer or string). If not provided, the channel does not matter.
;; .misc Will never give a fatal error. The function returns #f in cases of problems. 
(define (ProgramChange? x . optional-parameter-list)
  (let ((channel (optional-parameter 1 optional-parameter-list #t)))
    (and (ast? x)
         (cond ((and (boolean? channel) channel)
                  (equal? (ast-element-name x) "ProgramChange"))
               ((and (number? channel) (>= channel 1) (<= channel 16))
                  (and (equal? (ast-element-name x) "ProgramChange")
                       (= channel (as-number (ast-attribute x 'channel)))))
               ((and (string? channel) (>= (as-number channel) 1) (<= (as-number channel) 16))
                  (and (equal? (ast-element-name x) "ProgramChange")
                       (equal? channel (as-number (ast-attribute x 'channel)))))
               (else #f)))))

;; Is x a SysEx midi message?
;; .form (SysEx? x [sys-ex-hex-string])
;; .parameter x Any data, but typically some midi AST.
;; .parameter sys-ex-hex-string A hex string, such as "08 43 10 4C 08 0C 00 01 F7". If provided, it must match the textual contents of the SysEx message\
;;                              (just compared via equal?). 
(define (SysEx? x . optional-parameter-list)
 (let ((sys-ex-hex-string (optional-parameter 1 optional-parameter-list #f)))
   (if (ast? x)
       (cond ((not sys-ex-hex-string) (equal? (ast-element-name x) "SysEx"))
             (sys-ex-hex-string (and (equal? (ast-element-name x) "SysEx") (equal? (ast-text x) sys-ex-hex-string)))
             (else #f))
       #f)))

;; Is x a ControlChange midi message in channel.
;; .form (ControlChange? x  [control channel])
;; .parameter x Any data, but typically some midi AST.
;; .parameter control A control number (an integer or string or boolean #t). If not provided, or if #t, the control number does not matter.
;; .parameter channel A channel (an integer or string). If not provided, the channel does not matter.
;; .misc Will never give a fatal error. The function returns #f in cases of problems. 
(define (ControlChange? x . optional-parameter-list)
  (let* ((control (optional-parameter 1 optional-parameter-list #t))
         (channel (optional-parameter 2 optional-parameter-list #t))
         (control-nr (if (and (boolean? control) control) #t (as-number control)))
         (channel-nr (if (and (boolean? channel) channel) #t (as-number channel))))
    (and (ast? x)
         (cond ((and (boolean? control) control  (boolean? channel) channel)
                  (equal? (ast-element-name x) "ControlChange"))
               ((and (boolean? control) control  (number? channel-nr))
                  (and (equal? (ast-element-name x) "ControlChange") (= (as-number (ast-attribute x 'channel)) channel-nr)))
               ((and (number? control-nr)  (boolean? channel) channel)
                  (and (equal? (ast-element-name x) "ControlChange") (= (as-number (ast-attribute x 'control)) control-nr)))
               ((and (number? control-nr) (number? channel-nr))
                  (and (equal? (ast-element-name x) "ControlChange") 
                       (= (as-number (ast-attribute x 'control)) control-nr)  (= (as-number (ast-attribute x 'channel)) channel-nr)))
               (else #f)))))

;; Is x a PitchBendChange midi message in channel.
;; .form (PitchBendChange? x [channel])
;; .parameter x Any data, but typically some midi AST.
;; .parameter channel A channel (an integer or string in the range of 1 .. 16). If not provided, the channel does not matter.
;; .misc Will never give a fatal error. The function returns #f in cases of problems. 
(define (PitchBendChange? x . optional-parameter-list)
  (let* ((channel (optional-parameter 1 optional-parameter-list #f)))
    (if channel
        (and (ast? x) (equal? (ast-element-name x) "PitchBendChange") (= (as-number channel) (as-number (ast-attribute x 'channel -1))))   ; -1: forces a number out of ast-attribute which compare with a legal channel always will give a false result.
        (and (ast? x) (equal? (ast-element-name x) "PitchBendChange")))))

;; Is x a NoteOn drum midi message?
;; In the context of this predicate, a drum midi message uses channel 9 or channel 10.
(define (drum-NoteOn? x)
  (and (NoteOn? x) (or (equal? (ast-attribute x 'channel) "9") (equal? (ast-attribute x 'channel) "10"))))

;; Is x a midi null event AST?
;; In fact, this predicate catches all Meta type 1 events (because the textual contents of midi null events have been given free).
;; .internal-references "Generator" "midi-null-event"
(define (midi-null-event-message? x)
  (and (ast? x) (equal? (ast-element-name x) "Meta") 
       (equal? (ast-attribute x 'type) "1")))

;; Is x a midi message ast with a channel attribute?
;; Returns #f is x is not an AST.
(define (channel-message? x)
  (if (ast? x)
      (let ((ch (ast-attribute x 'channel #f)))
         (if ch #t #f))
      #f))

;; Is x a midi message ast without a channel attribute?
;; Returns #f is x is not an AST.
(define (non-channel-message? x)
  (if (ast? x)
      (let ((ch (ast-attribute x 'channel #f)))
         (if ch #f #t))
      #f))


             

; ---------------------------------------------------------------------------------------------------------------

;;; .section-id midi-mes-accessors
;;; Midi message accessor.
;;; Convenient accessor of Midi Asts. Can be used instead of the general purpose accessors of LAML asts. 

;; Return the attribute value of the attribute named attribute-name of mes. 
;; Numeric attributes are automatically returned as numbers.
;; If mes is a non-ast, or an ast without an attribute named attribute-name, return #f.
;; .parameter attribute-name The name of the attribute. A symbol. (A string also works).
;; .parameter mes A midi message ast. (Any other value my also be passed, in which case the function just returns #f).
;; .returns The attribute value of mes, converted to a number for the following attribute-names: channel, note, velocity, duration, value, number, pressure, and strum-length.
;; .pre-condition True
;; .misc Can always be called. The precondition is as weak as possible.
(define (midi attribute-name mes)
  (let ((attribute-name-symbol (as-symbol attribute-name)))
    (if (ast? mes)
	(let ((attr-val (ast-attribute mes attribute-name-symbol #f)))
	  (if (and attr-val
		   (member attribute-name-symbol '(deltaTime absTime channel note velocity duration value number pressure strum-length control type)))
	      (as-number attr-val)
	      attr-val))
	#f)))
      

; ---------------------------------------------------------------------------------------------------------------

;;; .section-id midi-list-fn-gp
;;; Message List functions - General Purpose.
;;; This section and the following sections contain function that can be applied on lists of midi messages.
;;; The functions in this section are general purpose.
;;; As such, this is the important 'bread and butter' functions of this library.
;;; Most functions come in two flavors. The main function, f, can be applied in this way (f m1 m2 ... mk) on arbitrary many midi messages m1 ... mk.
;;; The other flavor, always named f-1, is applied as (f-1 (list m1 m2 ... mk)).
;;; In many cases there will be a few positional and required parameters before the first message.
;;; Thus, if there are two such required parameters p1 and p2, the calling forms are (f p1 p2 m1 m2 ... mk) and (f-1 p1 p2 (list m1 m2 ... mk)) respectively.
;;; Notice that the f-1 flavor of the functions are not explicitly documented below. If necessary, consult the Scheme source file to see f-1 via the provided links under 'See also'.




;; Merge the current selection with another a list of messages.
;; Both the current selection and the other message list must consist of deltaTime messages (and only deltaTime messages).
;; The sibling function (delta-merge-two-lists message-list-1 message-list-2) accepts two lists.
;; .form (delta-merge other-message-list . messages)
;; .parameter other-message-list A list of deltaTime messages.
;; .pre-condition There are deltaTime messages (and only deltaTime messages) in both involved message lists.
;; .returns a list of merged messages that preserves the timing of both input lists.
;; .misc It is trivial to do the merging if one of the inputs contain absTime messages.\
;;       This is because, per design, deltaTime messages can be inserted in between absTime messages.
(define delta-merge
 (xml-in-laml-positional-abstraction 1 0
   (lambda (other-message-list contents attributes)
    (delta-merge-two-lists contents other-message-list))))

;; Merge two list of deltaTimed notes.
(define (delta-merge-two-lists message-list-1 message-list-2)
  (delta-merge-two-lists-1 message-list-1 0 message-list-2 0 '()))

;; Merge an arbitrary number of lists, each of which contains deltaTimed notes.
(define (delta-merge-lists . list-of-message-lists)
  (if (null? list-of-message-lists)
      '()
      (let ((first-list (first list-of-message-lists))
            (rest-list-of-message-lists (cdr list-of-message-lists)))
        (delta-merge-two-lists first-list (apply delta-merge-lists rest-list-of-message-lists))))) 

; Tail recursive implementation.
; Only one of  subtraction-1 and  subtraction-2 is non-zero (positive) at a given time.
(define (delta-merge-two-lists-1 message-list-1 subtraction-1 message-list-2 subtraction-2 res)
  ; (display-message (length message-list-1) subtraction-1 (length message-list-2) subtraction-2 (length res) (if (not (null? res)) (ast-attribute (car res) 'deltaTime) #f))
  (cond ((and (null? message-list-1)   ; both message lists empty
              (null? message-list-2)) (reverse res))
        
         ((null? message-list-1)       ; message-list-1 empty. Adjust deltaTime of first element of message-list-2
                                (append (reverse res) 
                                        (let* ((ast (car message-list-2))
                                               (delta-time (as-number (ast-attribute ast 'deltaTime)))
                                               (effective-delta-time (- delta-time subtraction-2)))
                                          (cons (copy-ast-mutate-attributes ast 'deltaTime effective-delta-time) (cdr message-list-2)))))

        ((null? message-list-2)       ; message-list-2 empty. Adjust deltaTime of first element of message-list-1
                                (append (reverse res) 
                                        (let* ((ast (car message-list-1))
                                               (delta-time (as-number (ast-attribute ast 'deltaTime)))
                                               (effective-delta-time (- delta-time subtraction-1)))
                                          (cons (copy-ast-mutate-attributes ast 'deltaTime effective-delta-time) (cdr message-list-1)))))

        ((not (ast? (car message-list-1))) (delta-merge-two-lists-1 (cdr message-list-1) subtraction-1 message-list-2 subtraction-2 res))
        ((not (ast? (car message-list-2))) (delta-merge-two-lists-1 message-list-1 subtraction-1 (cdr message-list-2) subtraction-2 res))

        (else                         ; do proper merging
              (let* ((ast-1 (car message-list-1))
                     (ast-2 (car message-list-2))
                     (delta-time-1 (as-number (ast-attribute ast-1 'deltaTime)))
                     (delta-time-2 (as-number (ast-attribute ast-2 'deltaTime)))
                     (effective-delta-time-1 (- delta-time-1 subtraction-1))
                     (effective-delta-time-2 (- delta-time-2 subtraction-2)))
                 (if (< effective-delta-time-1 effective-delta-time-2)
                     (delta-merge-two-lists-1 (cdr message-list-1) 0 message-list-2 (+ subtraction-2 effective-delta-time-1)  
                                              (cons (copy-ast-mutate-attributes ast-1 'deltaTime effective-delta-time-1) res))
                     (delta-merge-two-lists-1 message-list-1 (+ subtraction-1 effective-delta-time-2) (cdr message-list-2) 0  
                                              (cons (copy-ast-mutate-attributes ast-2 'deltaTime effective-delta-time-2) res))
                 )))))


;; Merge the current selection with another a list of messages.
;; Both the current selection and the other message list must consist of absTime messages (and only absTime messages).
;; The sibling function (abs-merge-two-lists message-list-1 message-list-2) accepts two lists.
;; .form (abs-merge other-message-list . messages)
;; .parameter other-message-list A list of absTime messages.
;; .pre-condition There are absTime messages (and only absTime messages) in both involved message lists.
;; .returns A list of merged messages (absTime)
;; .misc It is trivial to do the merging if one of the inputs contain absTime messages.\
;;       deltaTime merging, as implemented in delta-merge, is a little tricky - but doable.\
;;       absTime merging, as implemented in this function, is easier to deal with.
(define abs-merge
 (xml-in-laml-positional-abstraction 1 0
   (lambda (other-message-list contents attributes)
    (abs-merge-two-lists contents other-message-list))))

(define (abs-merge-two-lists message-list-1 message-list-2)
  (abs-merge-two-lists-1 message-list-1 message-list-2 '()))  

(define (abs-merge-two-lists-1 message-list-1 message-list-2 res)
  (cond ((and (null? message-list-1)   ; both message lists empty
              (null? message-list-2)) (reverse res))
        
        ((null? message-list-1) (append (reverse res) message-list-2))  ; one of the message lists is empty
        ((null? message-list-2) (append (reverse res) message-list-1)) 

        ((not (ast? (car message-list-1))) (abs-merge-two-lists-1 (cdr message-list-1) message-list-2 res))  ; drop non-AST element
        ((not (ast? (car message-list-2))) (abs-merge-two-lists-1 message-list-1 (cdr message-list-2) res))  ; ditto

        (else                         ; do proper merging - none of the message lists are empty
              (let* ((ast-1 (car message-list-1))
                     (ast-2 (car message-list-2))
                     (abs-time-1 (as-number (ast-attribute ast-1 'absTime)))
                     (abs-time-2 (as-number (ast-attribute ast-2 'absTime))))
                 (if (<= abs-time-1 abs-time-2)
                     (abs-merge-two-lists-1 (cdr message-list-1) message-list-2  
                                              (cons (car message-list-1) res))
                     (abs-merge-two-lists-1 message-list-1 (cdr message-list-2)  
                                              (cons (car message-list-2) res))
                 )))))

;; Reverses the absTimes of messages. 
;; The absTime of the first resulting message is taken from the first of messages etc.
;; .form (abs-time-reverse . messages)
;; .parameter messages A list of midi messages (such as NoteOn messages).
;; .pre-condition There are absTime messages (and only absTime messages) in both involved message lists.
;; .mode absTime mode
(define abs-time-reverse
 (xml-in-laml-positional-abstraction 0 0
   (lambda (contents attributes)
     (abs-time-reverse-1 contents))))

(define (abs-time-reverse-1 messages)
  (let ((rev-messages (reverse messages)))
    (map (lambda (m mr)
            (copy-ast-mutate-attributes m 'absTime (midi 'absTime mr)))
         messages rev-messages)))



;; Merge the delta time messages in delta-message-list with messages.
;; The messages in delta-message-list consist of deltaTime messages (and only deltaTime messages).
;; The messages in messages consist of absTime messages (and only absTime messages).
;; The list consisting of messages must not be empty.
;; Returns a list of pure absTime messages.
;; .form (delta-abs-merge delta-message-list . messages)
(define delta-abs-merge 
 (xml-in-laml-positional-abstraction 1 0
   (lambda (delta-message-list contents attributes)
    (delta-abs-merge-two-lists delta-message-list contents))))

(define (delta-abs-merge-two-lists delta-message-list abs-message-list)
  (let* ((first-abs-time (as-number (ast-attribute (first abs-message-list) 'absTime)))
         (delta-to-abs-message-list (delta-time-list-to-abs-time-list delta-message-list first-abs-time)))
    (abs-merge-two-lists delta-to-abs-message-list abs-message-list)))

(define (delta-time-list-to-abs-time-list delta-message-list first-abs-time)
  (if (null? delta-message-list)
      '()
      (let* ((first-delta-mes (first delta-message-list))
             (next-abs-time (+ first-abs-time (as-number (ast-attribute first-delta-mes 'deltaTime)))))
        (cons (single-message-ast-delta-to-abs-time first-delta-mes next-abs-time)
              (delta-time-list-to-abs-time-list (cdr delta-message-list) next-abs-time)))))


;; If filter-fn holds (returns true) on a given midi message, transform this message with transformation-fn.
;; If not, pass the midi message untransformed.
;; .form (transform-messages filter-fn transformation-fn . messages)
;; .parameter filter-fn A boolean midi-message function. Is always applied on an AST.
;; .parameter transformation-fn a single midi-message transformation function. Is always applied on an AST.
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .returns a list of transformed messages
(define transform-messages
 (xml-in-laml-positional-abstraction 2 0
   (lambda (filter-fn transformation-fn contents attributes)
    (transform-messages-1 filter-fn transformation-fn contents ))))


(define (transform-messages-1 filter-fn transformation-fn message-list)
 (map 
  (lambda (mes-ast) 
    (if (and (ast? mes-ast) (filter-fn mes-ast))
        (transformation-fn mes-ast)
        mes-ast))
  message-list))



;; Keep those messages in the message list that satisfy the predicate pred-fn.
;; Messages that do not satify the predicate are removed. As a consequence, this function is most likely to be useful in absTime mode.
;; .form (filter-messages pred-fn . messages)
;; .parameter pred-fn A boolean midi-message predicate function. Will applied on an AST. If pred-fn returns true on an-ast, keep it.
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .returns a list of messages
(define filter-messages
 (xml-in-laml-positional-abstraction 1 0
   (lambda (pred-fn contents attributes)
    (filter-messages-1 pred-fn contents ))))


(define (filter-messages-1 pred-fn message-list)
 (filter 
  (lambda (x) 
     (if (ast? x) 
         (pred-fn x)
         #t))
  message-list))

;; Filter those messages in the message list that satisfy the predicate pred-fn.
;; The residual messages that do not satisfy the predicate are transferred to a file abs-merge-file-path.
;; If the abs-merge-file-path parameter addresses an existing residual file, the compacted MIDI LAML messages in this file are merged with the residual messages of the current processing.
;; This function is useful for accumulating residual messages - step-wise zooming (where the residual messages represent are represented separately at any time).
;; The format used in the residual files is a list of either normal or compacted ASTs of the individual MIDI messages.
;; Compaction of a single MIDI LAML AST message is done with the function compact-midi-laml-ast.
;; This function is used internally by the MIDI LAML zoom facility. It only applies to pure absTime mode. 
;; Problems arise if any deltaTime messages are encountered.
;; .form (filter-messages-keep-residual-and-accumulate! pred-fn abs-target-file-path abs-merge-file-path . messages)
;; .mode absTime mode
;; .internal-references "Similar to" "filter-messages-keep-residual-and-reprocess!"
;; .parameter pred-fn A boolean midi-message predicate function. Will applied on an AST. If pred-fn returns true on an-ast, keep it.
;; .parameter abs-target-file-path The absolute path to a file where the residual MIDI messages are stored. If this file exists, it is deleted before new contents is written to it.
;; .parameter abs-merge-file-path The absolute path to an existing file with residual MIDI messages, of #f. A #f value, or a non-existing file causes no merging to take place.
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .returns a list of messages
(define filter-messages-keep-residual-and-accumulate!
 (xml-in-laml-positional-abstraction 3 0
   (lambda (pred-fn abs-target-file-path abs-merge-file-path contents attributes)
    (filter-messages-keep-residual-and-accumulate-1! pred-fn abs-target-file-path abs-merge-file-path contents))))

(define (filter-messages-keep-residual-and-accumulate-1! pred-fn abs-target-file-path abs-merge-file-path message-list)
 (filter-messages-keep-residual-and-accumulate-2 pred-fn abs-target-file-path abs-merge-file-path message-list '() '()))

(define (filter-messages-keep-residual-and-accumulate-2 pred-fn abs-target-file-path abs-merge-file-path message-list result-list residual-list)
  (cond ((null? message-list)
           (let ((merge-list (if (and abs-merge-file-path (file-exists? abs-merge-file-path))
                                 (file-read abs-merge-file-path)
                                 '())))
            (file-write (append merge-list (map compact-midi-laml-ast (reverse residual-list))) abs-target-file-path)  ; save reversed residual-list
            (reverse result-list)                               ; return list of messages on which the predicate holds
           )
        )
        ((and (ast? (car message-list)) (pred-fn (car message-list))) 
           (filter-messages-keep-residual-and-accumulate-2 pred-fn abs-target-file-path abs-merge-file-path (cdr message-list) (cons (car message-list) result-list) residual-list))
        ((and (ast? (car message-list)) (not (pred-fn (car message-list)))) 
           (filter-messages-keep-residual-and-accumulate-2 pred-fn abs-target-file-path abs-merge-file-path (cdr message-list) result-list (cons (car message-list) residual-list)))
        (else 
           (filter-messages-keep-residual-and-accumulate-2 pred-fn abs-target-file-path abs-merge-file-path (cdr message-list) result-list residual-list))))


;; Filter those messages in the message list that satisfy the predicate pred-fn.
;; The residual messages that do not satisfy the predicate are transferred to a file abs-merge-file-path.
;; If the abs-merge-file-path parameter addresses an existing residual file, the compacted MIDI LAML messages in this file are merged with messages before processing.
;; This function is useful for reprocessing residual messages - zooming out and then rezooming.
;; The format used in the files is a list of either normal or compacted ASTs of the individual MIDI messages.
;; Compaction of a single MIDI LAML AST message is done with the function compact-midi-laml-ast.
;; This function is used internally by the MIDI LAML zoom facility. It only applies to absTime mode.
;; .form (filter-messages-keep-residual-and-reprocess! pred-fn abs-target-file-path abs-merge-file-path . messages)
;; .mode absTime mode
;; .internal-references "Similar to" "filter-messages-keep-residual-and-accumulate!"
;; .parameter pred-fn A boolean midi-message predicate function. Will applied on an AST. If pred-fn returns true on an-ast, keep it.
;; .parameter abs-target-file-path The absolute path to a file where the residual MIDI messages are stored. If this file exists, it is deleted before new contents is written to it.
;; .parameter abs-merge-file-path The absolute path to an existing file with residual MIDI messages, of #f. A #f value, or a non-existing file causes no merging to take place.
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .returns a list of messages
(define filter-messages-keep-residual-and-reprocess!
 (xml-in-laml-positional-abstraction 3 0
   (lambda (pred-fn abs-target-file-path abs-merge-file-path contents attributes)
    (filter-messages-keep-residual-and-reprocess-1! pred-fn abs-target-file-path abs-merge-file-path contents))))

(define (filter-messages-keep-residual-and-reprocess-1! pred-fn abs-target-file-path abs-merge-file-path message-list)
 (let ((merge-list (if (and abs-merge-file-path (file-exists? abs-merge-file-path))
                       (map uncompact-midi-laml-entry (file-read abs-merge-file-path))
                      '())))
   (filter-messages-keep-residual-and-reprocess-2 pred-fn abs-target-file-path (abs-merge-two-lists message-list merge-list) '() '())))

(define (filter-messages-keep-residual-and-reprocess-2 pred-fn abs-target-file-path message-list result-list residual-list)
  (cond ((null? message-list)
            (file-write (map compact-midi-laml-ast (reverse residual-list)) abs-target-file-path)  ; save reversed residual-list
            (reverse result-list)                               ; return list of messages on which the predicate holds
        )
        ((and (ast? (car message-list)) (pred-fn (car message-list))) 
           (filter-messages-keep-residual-and-reprocess-2 pred-fn abs-target-file-path (cdr message-list) (cons (car message-list) result-list) residual-list))
        ((and (ast? (car message-list)) (not (pred-fn (car message-list)))) 
           (filter-messages-keep-residual-and-reprocess-2 pred-fn abs-target-file-path (cdr message-list) result-list (cons (car message-list) residual-list)))
        (else 
           (filter-messages-keep-residual-and-reprocess-2 pred-fn abs-target-file-path (cdr message-list) result-list residual-list))))


;; Transform the value of attribute-name with with a given numeric transformation function.
;; Only messages that satisfy the ast predicate ast-pred are affected.
;; Non-affected messages, or messages that to not have an attribtute named attribute-name, are passed without modification.
;; .form (transform-attribute  ast-predicate attribute-name trasformation-fn . messages)
;; .parameter ast-predicate  A predicate, guarantied to be applied on an AST (midi-message).
;; .parameter-name  attribute-name The name of an attribute. A symbol.
;; .parameter-name transformation-fn A function of one numeric value which returns a numeric value. \
;;                                   The input of transformation-fn is the numeric value of the value name attribute-name.\
;;                                   The output of the transformation-fn becomes the new value of attribute-name.\
;;                                   Conversion of the returned value to an integer string is done in the calling context of the transformation function.
;; .parameter messages A list of midi messages
;; .internal-references "Specialized function" "scale-attribute-by-factor"
;; .internal-references "Pitch-bend scaling function" "pitch-bend-scale"
;; .misc The function-generator pitch-bend-scale produces functions that can be used as transformation functions\
;;       in case we transform the value attribute of PitchBendChange midi messages.
(define transform-attribute
  (xml-in-laml-positional-abstraction
   3 0
   (lambda (ast-predicate attribute-name transformation-fn contents attributes)
     (transform-attribute-1 ast-predicate attribute-name transformation-fn contents))))

(define (transform-attribute-1 ast-predicate attribute-name transformation-fn message-list)
  (map 
   (lambda (x) 
     (if (and (ast? x) (ast-predicate x))
         (let* ((mes-ast x)  ; just alias
                (attribute-value (ast-attribute mes-ast attribute-name #f))
               )
           (if attribute-value
               (copy-ast-mutate-attributes mes-ast attribute-name (as-int-string (transformation-fn (as-number attribute-value))))
               mes-ast))
         x))
   message-list))


;; Scale the value of attribute-name with factor.
;; Only messages that satisfy the ast predicate ast-pred are affected.
;; Non-affected messages, or messages that to not have an attribtute named attribute-name, are passed without modification.
;; .form (scale-attribute-by-factor ast-predicate attribute-name factor . messages)
;; .parameter ast-predicate  A predicate, guarantied to be applied on an AST (midi-message)
;; .parameter-name  attribute-name The name of an attribute. A symbol.
;; .parameter-name factor A real number.
;; .parameter messages A list of midi messages
;; .internal-references "Generalized function" "transform-attribute"
;; .internal-references "Context-dependent scaling function" "scale-attribute"
;; .misc Notice that this function is a specialization of the more general function transform-attribute.
(define scale-attribute-by-factor 
  (xml-in-laml-positional-abstraction
   3 0
   (lambda (ast-predicate attribute-name factor contents attributes)
     (scale-attribute-by-factor-1 ast-predicate attribute-name factor contents))))

(define (scale-attribute-by-factor-1 ast-predicate attribute-name factor messages)
  (transform-attribute-1 ast-predicate attribute-name (lambda (value) (* factor value)) messages))   

;; Randomize the values of the attributes that belong to attribute-name of messages in channel-list.
;; Only messages that satisfy the AST predidate pred are affected.
;; Each relevant attribute value is multiplied by a random number drawn from the interval [lower-number - upper-number].
;; Round the result of the multiplication to an integer, and enforce the result to be 
;; within the interval [min-attribute-value - max-attribute-value].
;; .form (randomize-attribute pred attribute-name channel-list lower-number upper-number min-attribute-value max-attribute-value . messages)
;; .parameter pred An AST predicate.
;; .parameter attribute-name The name of an attribute. A symbol
;; .parameter channel-list A list of channels. A list of integers in the interval 1..16.
;; .parameter lower-number The lowest possible scale factor. A real number.
;; .parameter upper-number The highest possible scale factor. A real number.
;; .parameter min-attribute-value The minimum value of the attribute after scaling, or #f if no such minimum applies.
;; .parameter max-attribute-value The maxium value of the attribute after scaling, or  #f if no such maximum applies.
;; .parameter messages A list of midi messages (such as NoteOn messages).
(define randomize-attribute
  (xml-in-laml-positional-abstraction 7 0
    (lambda (pred attribute-name channel-list lower-number upper-number min-attribute-value max-attribute-value  cont attr)
       (randomize-attribute-1 pred attribute-name channel-list lower-number upper-number min-attribute-value max-attribute-value cont))))

(define (randomize-attribute-1 pred attribute-name channel-list lower-number upper-number min-attribute-value max-attribute-value message-list)
 (map 
  (lambda (mes-ast) 
    (if (and (ast? mes-ast) (pred mes-ast))
        (let ((channel (ast-attribute mes-ast 'channel #f)))
          (if (and channel (memv (as-number channel) channel-list)) 
              (let ((attr-value (as-number (ast-attribute mes-ast attribute-name #f)))
                    (random-number (random-real-number-between lower-number upper-number))
                   )  
                (if attr-value
                    (copy-ast-mutate-attributes mes-ast attribute-name 
                        (if (and min-attribute-value max-attribute-value)
                            (between min-attribute-value max-attribute-value (to-int (* random-number attr-value)))
                            (to-int (* random-number attr-value))))
                    mes-ast))
              mes-ast))
         mes-ast))
  message-list))

(define random-real-number-between
  (let ((seed (- (power 2 31) 1)))
    (random-seed seed)
    (lambda (a b)
     (let* ((lgt (- b a))
            (r (exact->inexact (/ (random seed) seed))) ; between 0 and 1
            (rl (* r lgt))
            (rla (+ a rl)))
       rla))))

; Is message-list an absTime sequence.
; Please notice that the choice is made from the first message in the list
(define (abs-time-sequence? message-list)
  (cond ((null? message-list) #f)
        ((ast? (first message-list))
           (has-ast-attribute? (car message-list) 'absTime))
        (else (abs-time-sequence? (cdr message-list)))))

; Is message-list an deltaTime sequence.
; Please notice that the choice is made from the first message in the list
(define (delta-time-sequence? message-list)
  (cond ((null? message-list) #f)
        ((ast? (first message-list))
           (has-ast-attribute? (car message-list) 'deltaTime))
        (else (delta-time-sequence? (cdr message-list)))))

(define (assert-abs-time messages)
  (if (not (abs-time-sequence? messages))
      (laml-error "In this context, you must use absTime sequences.")))

(define (assert-delta-time messages)
  (if (not (delta-time-sequence? messages))
      (laml-error "In this context, you must use deltaTime sequences.")))


;; Replicate the events (in all channels) in message-list n times. 
;; Most useful in deltaTime mode.
;; Is typically used to play n verses of a song.
;; .form (replicate n . messages)
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .parameter n The number of replications. Must be a non-negative integer.
(define replicate
 (xml-in-laml-positional-abstraction 1 0
  (lambda (n cont attr)
   (replicate-1 n cont))))

(define (replicate-1 n message-list)
  (cond ((= n 0) '())
        (else (append message-list (replicate-1 (- n 1) message-list)))))

;; Scale an attribute with use of a scaling function.
;; Scale the value of the attribute attribute-name in all NoteOn messages in messages.
;; Non-NoteOn events (and NoteOn events which does not have an attribute named attribute-name) are not affected.
;; The scaling is done by multiplying each attribute value with a factor returned by the scaling function.
;; The factor depends on the position of the NoteOn message relative to the contextual NoteOn messages. 
;; scaling-function is a real-valued function (lambda (x) ...) where x belongs to the interval [0,1].    
;; .form (scale-attribute attribute-name scaling-function . messages)
;; .parameter attribute-name The name of the attribute to scale. A string or symbol.
;; .parameter scaling-function A function from [0,1] to Real.
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .reference "Examples" "Midi LAML examples" "../examples/attribute-scaling/index.html"
;; .internal-references "Non-context dependent scaling" "scale-attribute-by-factor"
;; .internal-references "Scaling per channel" "scale-attribute-of-channel"
;; .internal-references "Scaling functions" "scaling-function-generation"
(define scale-attribute  
 (xml-in-laml-positional-abstraction 2 0
   (lambda (attribute-name scaling-function contents attributes)
    (scale-attribute-1 attribute-name scaling-function  contents ))))

(define (scale-attribute-1 attribute-name f contents)
 (let* ((attr-name (as-symbol attribute-name))
        (noteon-contents (filter (lambda (x) (and (ast? x) (equal? "NoteOn" (ast-element-name x)))) contents))
        (number-of-noteon-messages (length noteon-contents))
        (number-list (consequtive-numbering-by-predicate NoteOn? contents 1 0)) ; progressing integer for each NoteOn message. 
                                                                                ; 0 for non-NoteOn messages. 
       )
   (map 
    (lambda (mes-ast i) 
      (if (and (ast? mes-ast) (equal? "NoteOn" (ast-element-name mes-ast)))
          (let ((attr-value (ast-attribute mes-ast attr-name #f)))
            (if attr-value
                (let* ((attr-val-number (as-number attr-value))
                       (max-i number-of-noteon-messages) ; a convenient alias
                       (scaled-attr-value (* attr-val-number (f (/ i max-i))))
                      )
                  (copy-ast-mutate-attributes mes-ast
                     (as-symbol attr-name) 
                     (as-int-string scaled-attr-value)))
                mes-ast))
          mes-ast)
    )
    contents
    number-list)))



;; Scale an attribute in a given channel with use of a scaling function.
;; Scale the value of the attribute attribute-name in all NoteOn messages in messages.
;; Non-NoteOn events (and NoteOn events which does not have an attribute named attribute-name as well
;; as NoteOn messages in other channels) are not affected.
;; The scaling is done by multiplying each attribute value with a factor returned by the scaling function.
;; The factor depends on the position of the NoteOn message relative to the contextual NoteOn messages. 
;; scaling-function is a real-valued function (lambda (x) ...) where x belongs to the interval [0,1].    
;; .form (scale-attribute-of-channel ch attribute-name scaling-function . messages)
;; .parameter ch The channel number. An integer between 1 and 16.
;; .parameter attribute-name The name of the attribute to scale. A string or symbol.
;; .parameter scaling-function A function from [0,1] to Real.
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .reference "Examples" "Midi LAML examples" "../examples/attribute-scaling/index.html"
;; .internal-references "Similar non-channel function" "scale-attribute"
;; .internal-references "Non-context dependent scaling" "scale-attribute-by-factor"
;; .internal-references "Scaling functions" "scaling-function-generation"
(define scale-attribute-of-channel  
 (xml-in-laml-positional-abstraction 3 0
   (lambda (ch attribute-name scaling-function contents attributes)
    (scale-attribute-of-channel-1 ch attribute-name scaling-function  contents ))))

(define (scale-attribute-of-channel-1 ch attribute-name f contents)
 (let* ((attr-name (as-symbol attribute-name))
        (noteon-contents (filter (lambda (x) (and (ast? x) (equal? "NoteOn" (ast-element-name x)) (= ch (as-number (ast-attribute x 'channel))))) contents))
        (number-of-noteon-messages (length noteon-contents))
        (number-list (consequtive-numbering-by-predicate                                                     ; progressing integer for each NoteOn message. 
                        (lambda (x) (and (NoteOn? x) (= ch (as-number (ast-attribute x 'channel)))))                                   
                        contents 1 0))
       )
   (map 
    (lambda (mes-ast i) 
      (if (and (ast? mes-ast) (equal? "NoteOn" (ast-element-name mes-ast)))
          (let ((attr-value (ast-attribute mes-ast attr-name #f)))
            (if (and attr-value (= ch (as-number (ast-attribute mes-ast 'channel))))
                (let* ((attr-val-number (as-number attr-value))
                       (max-i number-of-noteon-messages) ; a conveninent alias
                       (scaled-attr-value (* attr-val-number (f (/ i max-i))))
                      )
                  (copy-ast-mutate-attributes mes-ast
                     (as-symbol attr-name) 
                     (as-int-string scaled-attr-value)))
                mes-ast))
          mes-ast)
    )
    contents
    number-list)))

;; Scale an attribute with use of a scaling function on selected NoteOn message in given channels, determined by note-value-predicate.
;; Scale the value of the attribute attribute-name in all NoteOn messages in messages that satisfy the note-value-predicate.
;; Only NoteOn messages whose channels belong to ch-list are affected.
;; The note-value-predicate is applied on the note attribute.
;; Non-NoteOn events (as well as NoteOn events which does not have an attribute named attribute-name and NoteOn events which does not fullfil the precidate) are not affected.
;; The scaling is done by multiplying each attribute value with a factor returned by the scaling function.
;; If the attribute called domaining is relative-position, the factor depends on the position of the selected NoteOn message relative to the contextual NoteOn messages which satisfy the predicate.
;; If the attribute domaining is abs-time-domain-scaling, the factor depends on the absTime value of the selected NoteOn message.
;; It will lead to a fatal error if abs-time-domain-scaling is used in deltaTime mode.
;; scaling-function is a real-valued function (lambda (x) ...) where x belongs to the interval [0,1].    
;; .form (scale-attribute-by-predicate ch-list attribute-name scaling-function note-value-predicate . messages)
;; .parameter attribute-name The name of the attribute to scale. A string or symbol.
;; .parameter ch-list A list of channels. Each channel in the list must be an integer between 1 and 16.
;; .parameter scaling-function A function from [0,1] to Real.
;; .parameter note-value-predicate A predicate applied on the note attribute of the NoteOn message.
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .attribute domaining  implied  Determines the values in the domain of f. Either relative-position or abs-time-domain-scaling. Defaults to relative-position. A string.
;; .internal-references "scaling without note predicate" "scale-attribute"
;; .internal-references "scaling without note predicate" "scale-attribute-of-channel"
(define scale-attribute-by-predicate
 (xml-in-laml-positional-abstraction 4 0
   (lambda (ch-list attribute-name scaling-function note-value-predicate contents attributes)
    (let ((domaining (defaulted-get-prop 'domaining attributes 'relative-position))) 
      (scale-attribute-by-predicate-1 ch-list attribute-name scaling-function note-value-predicate (as-symbol domaining) (filter ast? contents))))))

(define (scale-attribute-by-predicate-1 ch-list attribute-name f note-value-predicate domaining contents)

 (if (and (delta-time-sequence? contents) (eq? domaining 'abs-time-domain-scaling))
     (laml-error "abs-time-domain-scaling can only be sued in pure absTime mode"))

 (let* ((attr-name (as-symbol attribute-name))
        (noteon-contents (filter (lambda (x) (and (ast? x) (equal? "NoteOn" (ast-element-name x)) (note-value-predicate (as-number (ast-attribute x 'note))))) contents))
        (number-of-noteon-messages (length noteon-contents))
        (number-list (consequtive-numbering-by-predicate
                         (lambda (x) (NoteOn? x) (note-value-predicate (as-number (ast-attribute x 'note))))
                         contents 1 0)) ; progressing integer for each NoteOn message which satisfy the predicate.
                                        ; 0 for non-NoteOn messages etc.
        (t-start (if (eq? domaining 'abs-time-domain-scaling)
                     (as-number (ast-attribute (first contents) 'absTime))
                     0)) ; dummy
        (t-end   (if (eq? domaining 'abs-time-domain-scaling)
                     (as-number (ast-attribute (last contents) 'absTime))
                     0))
       )
   (map 
    (lambda (mes-ast i) 
      (if (and (ast? mes-ast) (equal? "NoteOn" (ast-element-name mes-ast)) (note-value-predicate (as-number (ast-attribute mes-ast 'note))))  ; implies that i is not zero
          (let ((attr-value (ast-attribute mes-ast attr-name #f)))
            (if (and attr-value (member (as-number (ast-attribute mes-ast 'channel)) ch-list))
                (let* ((attr-val-number (as-number attr-value))
                       (max-i number-of-noteon-messages) ; a convenient alias
                       (scaled-attr-value 
                           (cond ((eq? domaining 'abs-time-domain-scaling) 
                                   (let ((t-cur (as-number (ast-attribute mes-ast 'absTime))))
                                     (* attr-val-number (f (/ (- t-cur t-start) (- t-end t-start))))) 
                                 )
                                 ((eq? domaining 'relative-position)
                                   (* attr-val-number (f (/ i max-i)))
                                 )
                                 (else (laml-error "scale-attribute-by-predicate-1: Unknown domaining" domaining))
                           ))
                      )
                  (copy-ast-mutate-attributes mes-ast
                     (as-symbol attr-name) 
                     (as-int-string scaled-attr-value)))
                mes-ast))
          mes-ast)
    )
    contents
    number-list)))


; Return a consequtive numbering of those elements of lst that satisfy the predicate pred.
; Numbering starts with first-val
; Those elements that do not satisfy pred are given the value missing-value in the resulting list.
; .parameter pred A predicate which must be applicable on all elements of lst
; .parameter lst A list of elements
; .parameter first-val An arbitray integer value. Typically 0 or 1.
; .parameter missing-val An arbitrary value which is given for those elements in lst that do not satisfy pred.
; .returns A list of the same length as lst, of consequtive numbering of those elements of lst that satisfy pred.
(define (consequtive-numbering-by-predicate pred lst first-val missing-value)
  (consequtive-numbering-by-predicate-1 pred lst 1 missing-value '()))

(define (consequtive-numbering-by-predicate-1 pred lst first-val missing-value res)
  (cond ((null? lst) (reverse res))
        ((pred (car lst)) (consequtive-numbering-by-predicate-1 pred (cdr lst) (+ first-val 1) missing-value (cons first-val res)))
        (else             (consequtive-numbering-by-predicate-1 pred (cdr lst) first-val missing-value (cons missing-value res)))))

;; Enforce that the attribute value of of attribute-name in all forms with name form-name will have the value attribute-value.
;; .form (enforce-attribute-value form-name attribute-name attribute-value . messages)
;; .parameter form-name The name of forms affected by this function. A string or symbol.
;; .parameter attribute-name The name of the attribute affected by this function. A string or symbol.
;; .parameter attribute-value The enforced attribute value. Automatically converted to a string.
;; .parameter messages A list of midi messages.
(define enforce-attribute-value
 (xml-in-laml-positional-abstraction 3 0 
   (lambda (form-name attribute-name attribute-value contents attributes)
      (enforce-attribute-value-1 form-name attribute-name attribute-value contents))))

(define (enforce-attribute-value-1 form-name attribute-name attribute-value message-list)
  (map (lambda (x)
         (if (and (ast? x) (equal? (ast-element-name x) (as-string form-name)))
             (copy-ast-mutate-attributes x (as-symbol attribute-name) (as-string attribute-value))
             x) 
       )
       message-list)
)







;; Insert four beats in channel ch (such as channel 10, the drum channel) just before the first midi message in messages
;; and just before the end-of-track Meta event.
;; The insert-leading-and-trailing-beats form is typically supposed to be inserted just before bar 1:0:0 and ended just after Meta type 47 (end of track). 
;; The messages in message list, and the meta event appropriately, are automatically time-displaced by this function.
;; The ppqn and the time signature n:m can be passed as attributes, but all have sensible default values.
;; .form (insert-leading-and-trailing-beats ch . messages)
;; .pre-condition The end of track event Meta is the last event in messages (explicitly checked).
;; .mode absTime mode
;; .parameter ch The channel in which to generate the four beats. An integer between 1 and 16.
;; .parameter messages A list of midi messages.
;; .attribute ppqn  implied  Pulses per quarter note - defaults to 192
;; .attribute n  implied  The first number of the time signature - defaults to 4.
;; .attribute m  implied  The last number of the time signature - defaults to 4. Not used yet.
;; .misc Used for synchronization of sound recordings from two (or more) MIDI keyboards.
(define insert-leading-and-trailing-beats 
 (xml-in-laml-positional-abstraction 1 0
   (lambda (ch contents attributes)
    (let ((ppqn (defaulted-get-prop 'ppqn attributes #f))
          (n    (defaulted-get-prop 'n attributes #f))
          (m    (defaulted-get-prop 'm attributes #f)))
     (insert-leading-and-trailing-beats-1 
        ch 
        (if ppqn (as-number ppqn) 1920)
        (if n (as-number n) 4)
        (if m (as-number n) 4)
        contents)))))

; ppqn: typically 1920 - when recorded at Tyros.
; n:m: time signature. Typically n = 4 and m = 4. m is not used.
(define (insert-leading-and-trailing-beats-1 ch ppqn n m messages)
  (let* ((messages-only-ast (filter ast? messages))
         (end-of-track-message (last messages-only-ast))
         (before-end-of-track-messages (butlast messages-only-ast)))
    (if (not (and (equal? (ast-element-name end-of-track-message) "Meta") (= 47 (midi 'type end-of-track-message))))
        (laml-error "insert-leading-and-trailing-beats-1 ch messages: Last message is not a Meta end of track message."))

    (let* ((first-mes-abs-time (midi 'absTime (first before-end-of-track-messages)))
           (end-of-track-time (midi 'absTime end-of-track-message))
           (last-bar (quotient end-of-track-time (* ppqn n)))
           (trailing-insert-abs-time (* (+ last-bar 1) (* ppqn n)))
           (end-of-track-abs-time (+ trailing-insert-abs-time (* ppqn n 2))))
      (append
        (map (lambda (at)
               (NoteOn 'absTime at 'channel ch 'note "37" 'velocity "127" 'duration "100")
               )
             (list first-mes-abs-time (+ first-mes-abs-time (* 1 ppqn)) (+ first-mes-abs-time (* 2 ppqn)) (+ first-mes-abs-time (* 3 ppqn))))
        (time-displace (* 2 n ppqn) before-end-of-track-messages)
        (map (lambda (at)
               (NoteOn 'absTime at 'channel ch 'note "37" 'velocity "127" 'duration "100")
               )
             (list trailing-insert-abs-time (+ trailing-insert-abs-time (* 1 ppqn)) (+ trailing-insert-abs-time (* 2 ppqn)) (+ trailing-insert-abs-time (* 3 ppqn))))
        (list 
          (Meta 'absTime end-of-track-abs-time 'info "End of track" 'type "47" ""))))))


;; Thin out messages based on their absolute timing.
;; For messages in channel-list, keep only those those message whose absTime attribute satisfy abs-time-pred.
;; Keep messages in channels which do not belong to channel-list, and keep messages without a channel.
;; This function is (implemented as) a specialized application of filter-messages.
;; The higher-order function keep-beat generates useful predicates that can be used as actual parameters to abs-time-pred
;; .form (thin-out-messages-abs-time channel-list abs-time-pred . messages) 
;; .mode pure absTime
;; .parameter channel-list A list of channel numbers (an integer list).
;; .parameter abs-time-pred A boolean function (lambda (abs-time) ...). Keep those message who's absTime attribute satsify the predicate. abs-time is an integer.
;; .reference "Examples" "Midi LAML examples" "../examples/thin-out/index.html"
;; .internal-references "Predicate generator" "keep-beat" 
;; .internal-references "More general function" "filter-messages" 
;; .internal-references "By bar variant" "thin-out-section-by-bar"
(define thin-out-messages-abs-time
 (xml-in-laml-positional-abstraction 2 0
   (lambda (channel-list abs-time-pred contents attributes) 
     (thin-out-messages-abs-time-1 channel-list abs-time-pred contents))))

(define (thin-out-messages-abs-time-1 channel-list abs-time-pred message-list)
  (filter-messages-1
    (lambda (mes-ast)
      (let ((ch (ast-attribute mes-ast 'channel #f))
            (delta-time? (ast-attribute mes-ast 'deltaTime #f))   ; for error reporting purposes only.
           )
        (if delta-time? (laml-error "thin-out-message-abs-time: Encountered a deltaTime message. Can only be applied in pure absTime mode."))
        (if ch
            (if (member (as-number ch) channel-list)
                (abs-time-pred (as-number (ast-attribute mes-ast 'absTime)))
                #t)
            #t)))
    message-list))


;; Thin out messages based on their absolute timing, where the absolute timing is calculated from start-time and the deltaTime attributes.
;; For messages in channel-list, keep only those those message whose calculated absolute time satisfy abs-time-pred.
;; Keep messages in channels which do not belong to channel-list, and keep messages without a channel.
;; This function is (implemented as) a specialized application of filter-messages.
;; The higher-order function keep-beat generates useful predicates that can be used as actual parameters to abs-time-pred
;; .form (thin-out-messages-delta-time channel-list abs-time-pred start-time . messages) 
;; .mode deltaTime
;; .parameter channel-list A list of channel numbers (an integer list).
;; .parameter abs-time-pred A boolean function (lambda (abs-time) ...). Keep those message who's absTime attribute satsify the predicate. abs-time is an integer.
;; .parameter start-time The start-time which is used as the basis for calculating absolute times. Typically 0, but no default is available.
;; .reference "Examples" "Midi LAML examples" "../examples/thin-out/index.html"
;; .internal-references "Predicate generator" "keep-beat" 
;; .internal-references "More general function" "filter-messages" 
;; .internal-references "By bar variant" "thin-out-section-by-bar"
(define thin-out-messages-delta-time
 (xml-in-laml-positional-abstraction 3 0
   (lambda (channel-list abs-time-pred start-time contents attributes) 
     (thin-out-messages-delta-time-1 channel-list abs-time-pred start-time contents))))

(define (thin-out-messages-delta-time-1 channel-list abs-time-pred start-time message-list)
  (thin-out-messages-delta-time-2 channel-list abs-time-pred start-time 0 message-list '()))

(define (thin-out-messages-delta-time-2 channel-list abs-time-pred previous-abs-time accumulated-deltas  message-list result-list)
  (cond ((null? message-list) (reverse result-list))
        ((ast? (car message-list))
           (let* ((mes-ast (car message-list))
                  (ch (ast-attribute mes-ast 'channel #f))
                  (abs-time? (ast-attribute mes-ast 'absTime #f)))
             (if abs-time? (laml-error "thin-out-message-delta-time: Encountered an absTime message. Can only be applied in deltaTime mode."))
             (let* ((delta-time (as-number (ast-attribute mes-ast 'deltaTime)))
                    (new-abs-time (+ previous-abs-time delta-time))
                   )
               (if (and ch 
                        (member (as-number ch) channel-list) 
                        (abs-time-pred new-abs-time)
                   )
                   (let ((delta-modifier-mes-ast (copy-ast-mutate-attributes mes-ast 'deltaTime (+ delta-time accumulated-deltas))))
                     (thin-out-messages-delta-time-2 channel-list abs-time-pred new-abs-time 0 (cdr message-list) (cons delta-modifier-mes-ast result-list)))
                   (thin-out-messages-delta-time-2 channel-list abs-time-pred new-abs-time (+ accumulated-deltas delta-time) (cdr message-list) result-list)))))
        (else (thin-out-messages-delta-time-2 channel-list abs-time-pred previous-abs-time accumulated-deltas (cdr message-list) result-list))))


;; Generates a useful absTime predicate for thin-out-messages-abs-time.
;; The absolute time is displaced with displacement (displacement is subtracted from the absolute time) before fed to the predicate.
;; Returns a function that keeps all messages for 1/n notes, where n is 1, 2, 4, 8, ... (a power of 2, power >= 1).
;; .form (keep-beat n [displacement ppqn])
;; .parameter n defines which notes to keep.
;; .parameter displacement Allows identification of notes that are systematically displace. Defaults to 0.
;; .parameter ppqn Pulse per quarter notes. Defaults to 1920.
;; .returns An abs-time predicate (lambda (abs-time) ...)
;; .misc (keep-beat 4) returns a predicate that keeps all quarter notes.
;; .internal-references "Use context" "thin-out-messages-abs-time" "thin-out-section-by-bar" 
(define (keep-beat n . optional-parameter-list)
  (let ((displacement (optional-parameter 1 optional-parameter-list 0))
        (ppqn         (optional-parameter 2 optional-parameter-list 1920))
       )
    (lambda (abs-time)
        (= (remainder 
               (- abs-time displacement)
               (to-int (* ppqn (expt 2 (- 2 (round (log2 n))))))    ; if n is 4, then (- 2 (log2 n)) is 0.
                                                           ; Thus  (expt 2 (- 2 (log2 n))) is 1.  expt is the usual power function - std scheme.
            )
            0))))

(define (log2 x)
  (* (/ 1 (log 2)) (log x)))


;; Use the given channel as a marker channel.
;; Convert NoteOn messages in the given channel to markers with consequtive numbering such as "M-1", "M-2", etc.
;; The note attribute is used as the level of the marker.
;; C is level 0, D is level 1, E is level 2, etc. We only use white keys for leveling.
;; Eliminate all other ControlChange and ProgramChange messages relating to the channel.
;; .form (marker-channel channel marker-letter . messages)
;; .parameter channel A channel number in the interval 1..16.
;; .parameter marker-letter A string of length one containg the desired marking letter.
;; .parameter messages A list of midi messages (such as NoteOn messages). Cannot be empty.
(define marker-channel 
 (xml-in-laml-positional-abstraction 2 0
   (lambda (channel marker-letter contents attributes)
    (if (ast? marker-letter) (laml-error "Be sure to supply maker-letter as second argument to marker-channel"))  ; backward compatibility error message
    (eliminate-program-change-1 channel
     (eliminate-control-change-1 channel #t
      (marker-channel-1 channel marker-letter contents ))))))

; Assume as a precondition that message-list holds at least one message.
(define (marker-channel-1 channel marker-letter message-list)
   (let ((numbering (consequtive-numbering-by-predicate
                        (lambda (x) (and (NoteOn? x) (= channel (as-number (ast-attribute x 'channel))))) 
                        message-list 1 0))
        )
     (append
       (list
        ((treat-marking channel marker-letter) (car message-list) (car numbering))
        (Meta 'deltaTime "0"                                 ; initial marker - after first (possible) absTime event
              'type "6"
              (string-append marker-letter "-" "0" " " "**"))
       )
       (map2 (treat-marking channel marker-letter)
             (cdr message-list)
             (cdr numbering))) 
   )
)

; marker transformation of mes as number n
(define (treat-marking channel marker-letter) 
  (lambda (mes n)
    (if (and (NoteOn? mes) (= channel (as-number (ast-attribute mes 'channel))))
        (let ((abs-time (ast-attribute mes 'absTime #f))
              (delta-time (ast-attribute mes 'deltaTime #f)))
          (Meta (if abs-time 'absTime 'deltaTime) (time-of-message mes)
                'type "6"
                (string-append marker-letter "-" (as-string n) " " (star-marking-of (marker-level-of-note-on mes)))))
        mes)
    ))


; Mapping from notes (non-octave) to marker levels.
; The black (#/b) keys count as the white key to the left of them (instead of errors).
(define (marker-level-of-note-on noteon-ast)
  (let* ((note-attr (as-number (ast-attribute noteon-ast 'note)))
        (level-number (remainder note-attr 12)))
    (cond ((= level-number 0) 0)   ; C
          ((= level-number 1) 0)   ; C#
          ((= level-number 2) 1)   ; D
          ((= level-number 3) 1)   ; D#
          ((= level-number 4) 2)   ; E
          ((= level-number 5) 3)   ; F
          ((= level-number 6) 3)   ; F#
          ((= level-number 7) 4)   ; G
          ((= level-number 8) 4)   ; G#
          ((= level-number 9) 5)   ; A
          ((= level-number 10) 5)  ; A#
          ((= level-number 11) 6)  ; H
    )))

(define (star-marking-of level)
  (make-string level #\*))

;; Insert markers in messages - in between sections separated by least silent-ticks of silence.
;; Also insert an initial marker in front of the messages.
;; Currently, this function works only in absTime mode.
;; .form (marker-silence silence-ticks marker-letter . messages)
;; .parameter silence-ticks A number of ticks (a non-negative integer).
;; .parameter marker-letter A string of length one containg the desired marking letter.
;; .parameter messages A list of midi messages.
;; .mode absTime
(define marker-silence
 (xml-in-laml-positional-abstraction 2 0
   (lambda (silence-ticks marker-letter contents attributes)
     (marker-silence-1 silence-ticks marker-letter contents ))))

(define (marker-silence-1 silence-ticks marker-letter messages)
 (let ((next-marker-number 0)
       (first-mes (car messages))
       (in-between-messages (butlast (cdr messages)))
       (last-mes (last (cdr messages)))                  ; expected to be end of track message
      )

  (assert-abs-time messages)

  ; It causes weird problems if we insert a marker after the end of track message. Therefore
  ; last-mes is taken out before map-bites is called.
  (append (list (Meta 'absTime (time-of-message first-mes)
                              'type "6"
                              (string-append marker-letter "-" "0" " ")))
          (map-n-bites ; ealier: (take-message-bite-until-silence silence-ticks)
                       (bite-while-element-with-accumulation
			  (lambda (mes sound-frontier-time)
			    (not (and (> (midi 'absTime mes) sound-frontier-time) (> (- (midi 'absTime mes) sound-frontier-time) silence-ticks))))
			  (lambda (sound-frontier-time NoteOnMes)
			    (max sound-frontier-time (+ (midi 'absTime NoteOnMes) (midi 'duration NoteOnMes))))
			  0 
			  (lambda (x) (not (NoteOn? x))))
                       (lambda (midi-messages-bite next-marker-number)
			 (let ((last-mes (last midi-messages-bite)))
			   (append midi-messages-bite 
				   (list 
				    (Meta 'absTime (time-of-message last-mes)
					  'type "6"
					  (string-append marker-letter "-" (as-string next-marker-number) " "))))))
                     in-between-messages)
          (list last-mes))))


; NOT USED.
; A bite function. Takes a bite until an integer of silence of at least silence-ticks has been identified.
; Relies on pure absTime in the list of messages.
(define (take-message-bite-until-silence silence-ticks)
  (lambda (messages . rest)
     (take-message-bite-until-silence-1 silence-ticks messages #f '())))

; if sound-frontier-time is boolean #f, initiate the biting.
(define (take-message-bite-until-silence-1 silence-ticks messages sound-frontier-time res-messages)
  (if (null? messages)
      (reverse res-messages)
      (let ((mes (car messages)))
         (cond ((and (boolean? sound-frontier-time) (not sound-frontier-time) (NoteOn? mes))                                                    ; get started
                  (take-message-bite-until-silence-1 silence-ticks (cdr messages) (+ (midi 'absTime mes) (midi 'duration mes))  (cons mes res-messages)))
                ((and (boolean? sound-frontier-time) (not sound-frontier-time) (not (NoteOn? mes)))                                             ; get started
                   (take-message-bite-until-silence-1 silence-ticks (cdr messages) #f (cons mes res-messages))) 

               ((and (NoteOn? mes) (> (midi 'absTime mes) sound-frontier-time) (> (- (midi 'absTime mes) sound-frontier-time) silence-ticks))   ; large enough gap found
                  (reverse res-messages))

               ((NoteOn? mes) (take-message-bite-until-silence-1 silence-ticks (cdr messages)                                                   ; adjust sound-frontier-time 
                                      (max sound-frontier-time (+ (midi 'absTime mes) (midi 'duration mes)))
                                      (cons mes res-messages)))
               (else                                                                                                                            ; iterate
                  (take-message-bite-until-silence-1 silence-ticks (cdr messages) sound-frontier-time (cons mes res-messages)))))))


;; Identify chords, and markup sections with a given chord by start and end markers.
;; Also insert an initial marker in front of the messages.
;; Currently, this function works only in absTime mode.
;; .form (markup-chords channel marker-letter . messages)
;; .parameter channel A channel, between 1 and 16.
;; .parameter marker-letter A single letter, a string of length 1, used as the marker name.
;; .parameter messages A list of midi messages.
;; .mode absTime
;; .misc Obsolete. Used map-chords instead together with the function chord-marker.
(define markup-chords 
 (xml-in-laml-positional-abstraction 2 0
   (lambda (channel marker-letter contents attributes)
     (markup-chords-1 channel marker-letter contents))))

(define (markup-chords-1 channel marker-letter messages)
 (let ((first-mes (car messages))
       (in-between-messages (butlast (cdr messages)))
       (last-mes (last (cdr messages)))                  ; expected to be end of track message
       (normalized-note-val (lambda (noteon-mes) (remainder (midi 'note noteon-mes) 12)))
      )
  (assert-abs-time messages)

  (append (list (Meta 'absTime (time-of-message first-mes)
                              'type "6"
                              (string-append marker-letter "-" "0" " ")))
          (step-and-map-n-bites 
                       (bite-while-element-with-accumulation
                          (lambda (mes prev-chords)
                           (let ((chord-candidate-list (append prev-chords (list (normalized-note-val mes)))))
                            (if (< (length chord-candidate-list) 3)
                                #t
                                (chord-match? (normalize-chord-list chord-candidate-list)))))
                          (lambda (chord-candidate-list mes)
                             (append chord-candidate-list (list (normalized-note-val mes))))
                          '() 
                          (lambda (x) (not (and (NoteOn? x) (= channel (midi 'channel x)))))
                       )
                       (lambda (bite)
                          (let ((chord-list
                                    (map (lambda (no) (normalized-note-val no))
                                         (filter (lambda (x) (and (NoteOn? x) (= channel (midi 'channel x)))) bite))))
                            (if (chord-match? (normalize-chord-list chord-list))
                                (length bite)
                                -1)))
                       (lambda (bite i)
                         (let ((first-mes (first bite))
                               (last-mes (last bite))
                               (normalized-chord-list
                                    (normalize-chord-list
                                      (map (lambda (no) (normalized-note-val no))
                                         (filter (lambda (x) (and (NoteOn? x) (= channel (midi 'channel x)))) bite)))))
                           (append 
                                   (list (Meta 'absTime (time-of-message first-mes)
                                               'type "6" 
                                                (string-append marker-letter "-" (as-string (- (* i 2) 1)) 
                                                                "  " "#" (as-string channel) ": " 
                                                                "Start of chord: " 
                                                                (chord-name-of-normalized-note-list normalized-chord-list))))
                                   bite 
                                   (list (Meta 'absTime (time-of-message last-mes)
                                               'type "6" 
                                                (string-append marker-letter "-" (as-string (* i 2))
                                                                "  " "#" (as-string channel) ": " "End of chord"))))))
                      in-between-messages
          )
          (list last-mes))))

;; Map the function f on chords in a given channel.
;; Currently, this function works only in absTime mode.
;; .form (map-chords channel max-time-diff f . messages)
;; .parameter channel A channel, between 1 and 16.
;; .parameter max-time-diff The maximal number of ticks between chord note candidates. An integer.
;; .parameter f A function.   f: message-list channel bite-number chord-formula chord-name -> message-list.
;; .parameter messages A list of midi messages.
;; .internal-references "Useful f" "chord-marker"  
;; .mode absTime
;; .misc This function is a generalization of markup-chords, which we developed before this function has been provided.
(define map-chords 
 (xml-in-laml-positional-abstraction 3 0
   (lambda (channel max-time-diff f contents attributes)
     (map-chords-1 channel max-time-diff f contents))))

(define (map-chords-1 channel max-time-diff f messages)
 (let ((normalized-note-val (lambda (noteon-mes) (remainder (midi 'note noteon-mes) 12)))
       (relevant-message? (lambda (x) (and (NoteOn? x) (= channel (midi 'channel x))))) 
      )
  (assert-abs-time messages)

  (step-and-map-n-bites 
     (bite-while-element-with-accumulation   ; accumulates time of previous absTime note
      (lambda (mes prev-time)                ; keep going while notes are dense
          (if prev-time
              (if (< (- (time-of-message mes) prev-time) max-time-diff)
                  #t
                  #f)
               #t))
      (lambda (time mes)
        (time-of-message mes))
      #f
      (negate relevant-message?)
     )
     (lambda (bite)
       (let ((chord-list
              (map (lambda (no) (normalized-note-val no))
                   (filter relevant-message? bite))))
         (if (chord-match? (normalize-chord-list chord-list))
             (length bite)
             -1)))
     (lambda (bite n)
       (let ((normalized-chord-list
              (normalize-chord-list
               (map (lambda (no) (normalized-note-val no))
                    (filter relevant-message? bite)))))
         (f bite channel n normalized-chord-list (chord-name-of-normalized-note-list normalized-chord-list))))

   messages)))

(define (map-chords-1-old channel f messages)
 (let ((normalized-note-val (lambda (noteon-mes) (remainder (midi 'note noteon-mes) 12)))
       (relevant-message? (lambda (x) (and (NoteOn? x) (= channel (midi 'channel x))))) 
      )
  (assert-abs-time messages)

  (step-and-map-n-bites 
     (bite-while-element-with-accumulation
      (lambda (mes prev-chords)
        (let ((chord-candidate-list (append prev-chords (list (normalized-note-val mes)))))
          (if (< (length chord-candidate-list) 3)
              #t
              (chord-match? (normalize-chord-list chord-candidate-list)))))
      (lambda (chord-candidate-list mes)
        (append chord-candidate-list (list (normalized-note-val mes))))
      '()
      (negate relevant-message?)
     )
     (lambda (bite)
       (let ((chord-list
              (map (lambda (no) (normalized-note-val no))
                   (filter relevant-message? bite))))
         (if (chord-match? (normalize-chord-list chord-list))
             (length bite)
             -1)))
     (lambda (bite n)
       (let ((normalized-chord-list
              (normalize-chord-list
               (map (lambda (no) (normalized-note-val no))
                    (filter relevant-message? bite)))))
         (f bite channel n normalized-chord-list (chord-name-of-normalized-note-list normalized-chord-list))))

   messages)))

;; A suitable function f for map-chords, which mark the beginning and ends of chords with MIDI markers.
;; .internal-references "Use context" "map-chords"
(define (chord-marker bite channel n chord-formula chord-name) 
 (let ((first-mes (first bite))
       (last-mes (last bite))
       (marker-letter "C")
       )
   (append 
    (list (midi-marker-abs-time 
           (+ (time-of-message first-mes) 1)
           (string-append
            "#" (as-string channel) ": " 
            "Start of chord: " chord-name)
           (- (* n 2) 1)
           marker-letter)
          )
    bite 
    (list (midi-marker-abs-time
           (- (time-of-message last-mes) 1)
           (string-append
            "#" (as-string channel) ": "
            "End of chord")
           (* n 2)
           marker-letter)))))
 

;; Map the function f over bites of messages that are monotone in the sustain pedal relative to a given channel.
;; Being monotone means increasing, decreasing, or constant (constant will probably never happen in this context).
;; Increasing and decreasing bites do NOT necessarily alternate - two increasing bites may occur next to each other, for instance.
;; Appart from lists (or list tails) with zero or only one sustain message, a bite passed to f will always have at least two elements.
;; .form (map-sustain-intervals channel f . messages)
;; .parameter channel   A channel, between 1 and 16.
;; .parameter f         A function of signature message-list int direction -> message-list.\
;;                      The integer parameter is the (one-based) bite number. \
;;                      The direction (a symbol) is either increasing, decreasing, or constant depending on the elements in messages.
;; .parameter messages  A list of midi messages.
(define map-sustain-intervals
 (xml-in-laml-positional-abstraction 2 0
   (lambda (channel f contents attributes)
     (map-sustain-intervals-1 channel f contents))))

(define (map-sustain-intervals-1 channel f messages)
 (let ((cc-val-comparator (make-comparator 
                            (lambda (cc1 cc2) (< (midi 'value cc1) (midi 'value cc2)))
                            (lambda (cc1 cc2) (> (midi 'value cc1) (midi 'value cc2)))))
       (noice-fn (lambda (x) (not (ControlChange? x 64 channel)))))
  (map-n-bites 
    (bite-while-monotone 
       cc-val-comparator
       noice-fn)
    (lambda (messages bite-number)    ; earlier, in a simpler version, just f.   It is very helpful for f to get direct acccess to whether the bite is increasing, decreasing or constant.
             (f messages bite-number 
                (cond ((increasing-list-with-noice? cc-val-comparator noice-fn messages) 'increasing)
                      ((decreasing-list-with-noice? cc-val-comparator noice-fn messages) 'decreasing)
                      (else 'constant))))
    messages)))

;; Map the function f on each (non-empty) bar of messages.
;; Works in both absTime mode and in deltaTime mode (but sligthly better in absTime mode due to more accurate bar numbering).
;; As of now, the deltaTime version gives wrong results!
;; .form (map-bars f ppqn time-signature . messages)
;; .parameter f A function applied on each non-emtpy bar bite of messages. f: MIDI message list, bar-number, bar-start-tick, bar-end-tick -> MIDI message list. The bar number is one-based.
;; .parameter time-signature  A list of length 2, such as (3 4) for 3:4 and (6 8) for 6:8. 
;; .parameter ppqn The number of pulses per quarter note. A non-negative integer.
;; .parameter messages  A list of midi messages.
;; .misc The function can be applied at any location in an absTimed MIDI sequence, but it should be applied at the start of a deltaTimed sequence in order for the bars to be correct.\\
;;       In deltaTime mode, the bar numbers stem for the bit numbers which may give different bar numbers than in absTime mode.
(define map-bars
 (xml-in-laml-positional-abstraction 3 0
   (lambda (f ppqn time-signature contents attributes)
     (map-bars-1 f ppqn time-signature contents))))

(define (map-bars-1 f ppqn time-signature messages)
 (let* ((num (first time-signature))
        (denom (second time-signature))
        (pulses-per-whole-note (* 4 ppqn))
        (ticks-per-bar (to-int (* pulses-per-whole-note (/ (exact->inexact num) (exact->inexact denom)))))
       )
 (if (null? messages)
     '()
     (let* ((first-mes (car messages))
            (delta-time? (delta-time-message? first-mes))
            (abs-time?   (abs-time-message? first-mes)))
       (cond (abs-time?
               (map-bites 
                  (lambda (lst . rest)
                    (let* ((start-time-first-mes (midi 'absTime (first lst)))
                           (bar-number (quotient start-time-first-mes ticks-per-bar))  ; zero based
                           (bar-start-time (* bar-number ticks-per-bar))
                           (bar-end-time (+ bar-start-time ticks-per-bar))
                          )
                      ((bite-while-element (lambda (mes) (< (midi 'absTime mes) bar-end-time)) 'sentinel "first") lst)))
                  (lambda (bite)
                     (let* ((start-time-first-mes (midi 'absTime (first bite)))   ; As a service - recalculate these informations in the bite transformation function 
                            (bar-number (quotient start-time-first-mes ticks-per-bar))
                            (bar-start-time (* bar-number ticks-per-bar))
                            (bar-end-time (+ bar-start-time ticks-per-bar))
                           )
                       (f bite (+ bar-number 1) bar-start-time (- bar-end-time 1))))
                  messages)
             )
             (delta-time?   ; Assume that messages (the parameter to map-bars-1) start at absolute time 0   ; NOT CORRECT YET!!!!
                            ; Der sker en tidsforskydning nÅÂr der starter en ny bite. Dette Å¯delÅÊgger gradvist timingen af bars fuldstÅÊndigt!!!!
               (map-n-bites 
                  (lambda (lst n)  ; n is one-based
                    (let* ((bar-start-time (* (- n 1) ticks-per-bar))
                           (bar-end-time (+ bar-start-time ticks-per-bar))
                          )
                      ((bite-while-element-with-accumulation
                          (lambda (mes absTime) (< (+ absTime (midi 'deltaTime mes)) bar-end-time))
                          (lambda (absTime mes) (+ absTime (midi 'deltaTime mes)))
                          bar-start-time  ; Dette er ikke et korrekt start punkt for nÅÊste bid...
                       )
                       lst)))
                  (lambda (bite n)
                     (let* ((bar-number (- n 1))
                            (bar-start-time (* bar-number ticks-per-bar))
                            (bar-end-time (+ bar-start-time ticks-per-bar))
                           )
                       (f bite (+ bar-number 1) bar-start-time (- bar-end-time 1))))
                  messages)
             )
             (else (laml-error "map-bars-abs-time-1: Time problem. Should not happen")))))))


; just playing: map with noice
(define (n-map f lst noice?)
  (map (lambda (el) (if (noice? el) el (f el))) lst))


; A comprehensive mapping of chord names to a list of notes in the chords.
; The notes in the chord are ordered, and all in the interval 0..11. The list is read when needed the first time,
; and cached for subsequent use.
; May come from a file, such as data/extended-chord-list.lsp.
; Can also be calculated from a C-rooted chord list, via use of the function generate-complete-chord-list.
(define chord-map #f)

; Chord map selectors
(define (chord-name-of chord-entry) (car chord-entry))  
(define (chord-notes-of chord-entry) (cdr chord-entry))  

; Do cache the chord info list
(define (do-ensure-chord-map)
  (if chord-map 
      'do-nothing
      (set! chord-map
           (generate-complete-chord-list basic-chord-list)   ; earlier:  (file-read (string-append midi-software-dir "data/" "extended-chord-list.lsp"))
      )))

(define (normalize-chord-list chord-notes)
  (sort-list (remove-duplicates (map (lambda (note-val) (remainder note-val 12)) chord-notes))
             <=))

(define (chord-match? normalized-note-list)
  (do-ensure-chord-map)
  (find-in-list
     (lambda (chord-entry) 
       (equal? normalized-note-list (chord-notes-of chord-entry)))
     chord-map))

(define (chord-name-of-normalized-note-list normalized-note-list)
  (do-ensure-chord-map)
  (let ((search-res (find-in-list
                     (lambda (chord-entry) 
                       (equal? normalized-note-list (chord-notes-of chord-entry)))
                     chord-map)))
    (if search-res
        (chord-name-of search-res)
        (list-to-string normalized-note-list ","))))


; A chord formula list for the root of C. A possible input to the function generate-complete-chord-list below.
(define basic-chord-list
   '(("major" 0 4 7) ("6" 0 4 7 9) ("7" 0 4 7 10) ("M7" 0 4 7 11) ("Aug" 0 4 8) ("maj 9" 0 2 4 7)
     ("minor" 0 3 7) ("min6" 0 3 7 9) ("min7" 0 3 7 10)
     ("dim" 0 3 6) ("dim 6" 0 3 6 9) ("dim 7" 0 3 6 9) ("min 9" 0 2 3 7) ("sus4" 0 5 7) ("sus2" 0 2 7)))

; An more complete - and a much more complex - chord formula list for the root of C. A possible input to the function generate-complete-chord-list below.
(define extended-chord-list
   '(("major" 0 4 7) ("maj 6" 0 4 7 9) ("maj 7" 0 4 7 11) ("maj 9" 0 2 4 7 11) ("maj 11" 0 2 4 5 7 11) ("maj 13" 0 2 4 7 9 11) ("maj b5" 0 4 6) ("maj 7b5" 0 4 6 11) ("maj 9b5" 0 2 4 6 11) ("maj 11b5" 0 2 4 5 6 11) 
     ("maj 13 b5" 0 2 4 6 9 11) ("Aug" 0 4 8) ("maj 7#5" 0 4 8 11) ("maj 9 #5" 0 2 4 8 11) ("maj 11 #5" 0 2 4 5 8 11) ("maj 13 #5" 0 2 4 8 9 11) ("maj/9" 0 2 4 7) ("maj 6/9" 0 2 4 7 9) ("maj 7/6" 0 4 7 9 11) 
     ("maj 7/11" 0 4 5 7 11) ("maj 11/13" 0 2 4 5 7 9 11) ("maj 7b9" 0 1 4 7 11) ("maj 11b9" 0 1 4 5 7 11) ("maj 7#9" 0 3 4 7 11) ("maj 11 #9" 0 3 4 5 7 11) ("maj 9#11" 0 2 4 6 7 11) ("maj 7b5#9" 0 3 4 6 11) 
     ("minor" 0 3 7) ("min6" 0 3 7 9) ("min7" 0 3 7 10) ("min9" 0 2 3 7 10) ("min 11" 0 2 3 5 7 10) ("min 13" 0 2 3 7 9 10) ("diminished" 0 3 6) ("dim 6" 0 3 6 9) ("dim 7" 0 3 6 9) ("minor Major 7" 0 3 7 11)
     ("min Maj 9" 0 2 3 7 11) ("min Maj 11" 0 2 3 5 7 11) ("min 7 b5 - half dim" 0 3 6 10) ("min9 b5" 0 2 3 6 10) ("min 11b5" 0 2 3 5 6 10) ("min 13 b 5" 0 2 3 6 9 10) ("min 7#5" 0 3 8 10) 
     ("min 9#5" 0 2 3 8 10) ("min 11#5" 0 2 3 5 8 10) ("min/9" 0 2 3 7) ("min 6/9" 0 2 3 7 9) ("min 7/6" 0 3 7 9 10) ("min 7/11" 0 3 5 7 10) ("min 7b 9" 0 1 3 7 10) ("min 7#9" 0 3 3 7 10) ("Dominant 7" 0 4 7 10)
     ("Dom 9" 0 2 4 7 10) ("Dom 11" 0 2 4 5 7 10) ("Dom 13" 0 2 4 7 9 10) ("Dom 7b5" 0 4 6 10) ("Dom 9b5" 0 2 4 6 10) ("Dom 11b5" 0 2 4 5 6 10) ("Dom 7#5" 0 4 8 10) ("Dom 9#5" 0 2 4 8 10) ("Dom 7/6" 0 4 7 9 10)
     ("Dom 7/11" 0 4 5 7 10) ("Dom 7b9" 0 1 4 7 10) ("Dom 7#9" 0 3 4 7 10) ("Suspended 4" 0 5 7) ("Sus 6" 0 5 7 9) ("Sus 7" 0 5 7 11) ("Sus 9" 0 2 5 7 11) ("Sus 7b5" 0 5 6 11) ("Sus 7#5" 0 5 8 11)
     ("Sus/9" 0 2 5 7) ("Sus 6/9" 0 2 5 7 9) ("Maj/4" 0 4 5 7) ("maj 6/4" 0 4 5 7 9) ("maj 7/4" 0 4 5 7 11) ("maj/9/4" 0 2 4 5 7) ("min/4" 0 3 5 7) ("min 6/4" 0 3 5 7 9) ("min 7/4" 0 3 5 7 10) ("dim Sus" 0 5 6)
     ("dim 6 Sus" 0 5 6 9) ("dim 7 Sus" 0 5 6 9) ("aug Sus" 0 5 8) ("Dom 7 Sus" 0 5 7 10) ("Dom 7/4" 0 4 5 7 11) ("Aug/4" 0 4 5 8)))

;; Generate a complete list of chords from a basic chord list (a list of c chords, like the basic-chord-list).
;; Applies either flat og sharp naming style.
;; .form (generate-complete-chord-list basic-chord-list [chord-naming-style])
;; .parameter basic-chord-list a list of chord entries of the form (name-without-root . notes), where notes is 0-based.
;; .parameter chord-naming-style. Either sharp og flat (a symbol). Defaults value is sharp.
(define (generate-complete-chord-list basic-chord-list . optional-parameter-list)
 (let ((chord-naming-style (optional-parameter 1 optional-parameter-list 'sharp)))
  (let* ((displacements (number-interval 0 11))
         (roots-sharp (list "C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))
         (roots-flat  (list "C" "Db" "D" "Eb" "E" "F" "Gb" "G" "Ab" "A" "Bb" "B"))
         (roots (if (eq? chord-naming-style 'sharp) roots-sharp roots-flat))
         (transpose-chord-formula 
           (lambda (formula displacement)
              (normalize-chord-list (map (lambda (formula-entry) (+ formula-entry displacement)) formula))))
       )
    (flatten
     (map (lambda (displ root)
            (map 
              (lambda (chord-formula) 
                (let ((chord-name (chord-name-of chord-formula))
                      (chord-formula (chord-notes-of chord-formula)))
                   (cons (string-append root " " chord-name)
                         (transpose-chord-formula chord-formula displ))
                )
              ) 
              basic-chord-list
            )
          )
          displacements
          roots
      )))))


;; Apply a function on section of messages separate by a pause.
;; A pause is a section of at least pause-ticks ticks among messages that satisfy the relevance-predicate.
;; .form (map-paused-sections f silence-ticks relevant . messages)
;; .parameter f A function applied on the bite number and identified paused sections.   f: int x list of MIDI messages -> list of MIDI messages.
;; .parameter pause-ticks A number of ticks (a non-negative integer).
;; .parameter relevance-predicate An AST predicate function. Non-relevant messages is considered as noice to the undelying bite mapping function. Non-NoteOn messages are irrelevant, independent of the body of relevance-predicate.
;; .parameter messages A list of midi messages.
;; .mode absTime
;; .misc This function is a generalization of marker-silence, which inserts markers between paused sections. 
(define map-paused-sections
 (xml-in-laml-positional-abstraction 3 0
   (lambda (f pause-ticks relevance-predicate contents attributes)
     (map-paused-sections-1 f pause-ticks relevance-predicate contents))))

(define (map-paused-sections-1 f pause-ticks relevance-predicate messages)
 (assert-abs-time messages)

 (map-n-bites 
   (bite-while-element-with-accumulation
      (lambda (mes sound-frontier-time)  ; pred
         (not (and (> (midi 'absTime mes) sound-frontier-time) (> (- (midi 'absTime mes) sound-frontier-time) pause-ticks))))
      (lambda (sound-frontier-time NoteOnMes)  ; accumulator
         (max sound-frontier-time (+ (midi 'absTime NoteOnMes) (midi 'duration NoteOnMes))))
      0  ; init-val
      (lambda (x) (and (ast? x) (or (not (relevance-predicate x)) (not (NoteOn? x)))))  ; noise function
   )
   (lambda (midi-messages-bite n)
     (f n midi-messages-bite))
    messages))


;; Repeat messages n times.
;; .parameter n The number of times to repeat messages. An integer.
;; .parameter messages A list of midi messages (such as NoteOn messages). May also be a nested list of such messages according to LAML conventions.
;; .mode deltaTime
;; .form (repeat-messages n . messages)
;; .internal-references "Variant with period length" "repeat-messages"
(define repeat-messages
 (xml-in-laml-positional-abstraction 1 0
   (lambda (n contents attributes)  
     (repeat-messages-1 n contents))))

(define (repeat-messages-1 n flat-message-list)
  (if (= n 0)
      '()
      (append flat-message-list (repeat-messages-1 (- n 1) flat-message-list))))


;; Repeat messages n times, and enforce that each repetition has length of at least min-period-length.
;; (If the length of message-list is less than min-period-length, an appropriate deltaTime message is appended to each repetition).
;; .form (repeat-messages-enforce-periode-length n min-period-length . messages)
;; .parameter n The number of times to repeat messages. An integer.
;; .parameter min-period-length The mini period length, measured in pulses (an integer).
;; .parameter messages A list of midi messages (such as NoteOn messages). May also be a nested list of such messages according to LAML conventions.
;; .mode deltaTime
;; .internal-references "More basic variant" "repeat-messages"
(define repeat-messages-enforce-periode-length
 (xml-in-laml-positional-abstraction 2 0
   (lambda (n min-period-length contents attributes)  
     (repeat-messages-enforce-periode-length-1 n min-period-length contents))))

(define (repeat-messages-enforce-periode-length-1 n min-period-length flat-message-list)
  (if (= n 0)
      '()
      (append (enforce-minimum-message-length min-period-length flat-message-list)
              (repeat-messages-enforce-periode-length-1 (- n 1) min-period-length flat-message-list))))



;; .form (surround-by-delta-note-list delta-time-note-list . messages)
;; Return messages surrounded (both initially and in the end) by a list of delta timed notes.
;; This can, for instance, be used for initial and final drum beats (like a metronome) applied for synchronization purposes.
;; .parameter messages A list of midi messages.
;; .parameter delta-time-note-list A list of deltaTimed notes.
;; .internal-references "Useful note generator" "regular-beats"
;; .misc Synchronization beat hint: Surround from meassure number 1 to the end of the track (including the end of track meta event).\
;;       Afterwards, move the meta event to the new ending.
(define surround-by-delta-time-note-list
 (xml-in-laml-positional-abstraction 1 0 
   (lambda (delta-time-note-list contents attributes)
      (let ((lgt (length-of-delta-time-midi-list delta-time-note-list)))
        (list delta-time-note-list
              (time-displace lgt contents)
              delta-time-note-list)))))


;; A neutral and trivial message list function. Just pass the message-list through the form.
;; .form (pass-through . messages).
(define pass-through
 (xml-in-laml-abstraction 
   (lambda (contents attributes)
     contents)))

;; Apply sublist-pred to successive sublist bites of message-list. The prefix bites of the sublist are constructed by the function prefix-bite.
;; The sublist bites are passed to sublist-pred, and those which are selected by the predicated are transformed by
;; the transformation function, and spliced into the resulting list.
;; Sublists (elements) which are not selected by the predicate are passed non-transformed.
;; .misc This function is - more or less - a direct application of step-and-map-n-bites. As such it is probably too difficult to apply.
;; .form (map-midi-sections prefix-bite sublist-pred sublist-trans . message-list)
;; .parameter prefix-bite A function which selects a prefix of the list, for examination and possible transformation.\
;;                        Signature: List, int -> List.
;; .parameter sublist-pred An examination function from sublist to integer.\
;;                         Positive integer serves as boolean true. Negative integers serve as boolean false.\
;;                         A positive result p meaning that p elements of sublist is selected for transformation;\
;;                         A negative result n meaning that the start of the next examined bite is (- n) elements ahead.
;; .parameter sublist-tranf A transformation function on sublists, called on a bite if sublist-pred returns a\
;;                          positive integer on the bite. Signature List, int -> List. The integer is the bite number (1 based).
;; .parameter message-list A list of midi messages.
(define map-midi-sections 
 (xml-in-laml-positional-abstraction 3 0 
   (lambda (prefix-bite sublist-pred sublist-trans contents attributes)
     (map-midi-sections-1 prefix-bite sublist-pred sublist-trans contents))))

(define (map-midi-sections-1 prefix-bite sublist-pred sublist-trans message-list)
  (step-and-map-n-bites prefix-bite sublist-pred sublist-trans message-list))


;;; .section-id midi-list-fn-time
;;; Message List functions - Time related

;; Stretch the time and duration of message-list with a factor. Affects all messages (not only noteOn messages).
;; Works in both absTime and deltaTime mode.
;; .form (time-stretch factor . messages)
;; .mode Both deltaTime and absTime. Also in absTime mode with interleaving deltaTime events.
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .parameter factor A factor with wich to stretch the timing of the messages (a real number). A factor 1.0 is neutral with respect to timing.
;; .reference "Examples" "Midi LAML examples" "../examples/time-stretch/index.html"
(define time-stretch
 (xml-in-laml-positional-abstraction
  1 0
  (lambda (factor cont attr)
   (time-stretch-1 factor cont))))

(define (time-stretch-1 factor message-list)
 (map 
  (lambda (mes-ast) 
    (if (ast? mes-ast)
        (let ((delta-time (ast-attribute mes-ast 'deltaTime #f))
              (abs-time (ast-attribute mes-ast 'absTime #f))
              (dur (ast-attribute mes-ast 'duration 0))
             )
          (cond (delta-time 
                 (copy-ast-mutate-attributes mes-ast
                                            'deltaTime (as-int-string (* (as-number delta-time) factor))
                                            'duration  (as-int-string (* (as-number dur) factor)) ))
                (abs-time 
                 (copy-ast-mutate-attributes mes-ast
                                            'absTime (as-int-string (* (as-number abs-time) factor))
                                            'duration  (as-int-string (* (as-number dur) factor)) ))

                (else (laml-error "Can only time stretch in deltaTime and absTime mode")) ))
        mes-ast))
  message-list))

;; Adapt the time of the midi messages to new-length time units in total.
;; The durations of the individual messages are adapted proportionally. 
;; Implemented by use of time-stretch.
;; Can be used in both absTime mode and deltaTime mode, but it is most obvious to use this function in deltaTime mode.
;; .form (time-adapt-to new-length . messages)
;; .parameter messages A list of midi messages
;; .parameter new-length The desired length of messages including the first message\
;;  to and including the last message (but excluding the sucessor's delta). An integer.
(define time-adapt-to
 (xml-in-laml-positional-abstraction
  1 0
  (lambda (new-length cont attr)
   (time-adapt-to-1 new-length cont))))

(define (time-adapt-to-1 new-length message-list)
  (let* ((old-length (total-length-of-message-list message-list))
         (factor (/ new-length old-length)))
    (time-stretch-1 factor message-list)))     


;; Time displace with amount (adds displacement to the absolute time of the event). Affects all channels as well as system messages. 
;; Displacement can be positive or negative. 
;; This function works both in absTime mode and deltaTime mode, including absTime mode where there appears deltaTime contributions.
;; It is a requirement, however, that the first message-ast in messages reveals the time mode.
;; .form (time-displace displacement . messages)
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .parameter displacement The displacement of messages in time units, as described by the pulsesPerQuarterNote attribute of MidiHeader.  Can be positive or negative. 
;; .internal-references "Similar function" "time-displace-channels"
(define time-displace
 (xml-in-laml-positional-abstraction
  1 0
  (lambda (amount cont attr)
   (time-displace-1 amount cont))))

(define (time-displace-1 amount message-list)
 (cond ((null? message-list) '())
       ((abs-time-message? (first message-list))
          (map 
           (lambda (mes-ast) 
             (if (ast? mes-ast)
                 (let ((abs-time (ast-attribute mes-ast 'absTime #f))
                       (delta-time (ast-attribute mes-ast 'deltaTime #f))
                      )
                   (cond (abs-time 
                          (copy-ast-mutate-attributes mes-ast
                                                      'absTime (as-int-string (+ (as-number abs-time) amount))))
                         (delta-time
                          mes-ast)                
                         (else (laml-error "time-displace: Problems!"))))
                 mes-ast))
           message-list))
       ((delta-time-message? (first message-list))
          (let* ((first-mes (first message-list))
                 (first-delta-time (ast-attribute first-mes 'deltaTime #f))
                 (rest-messages (cdr message-list)))
             (cons
               (copy-ast-mutate-attributes first-mes 'deltaTime (as-int-string (+ (as-number first-delta-time) amount)))
               rest-messages)))
       (else (laml-error "time-displace-1: First message must reveal time mode."))))


;; As time-displace, but only affecting channels in channel-list
;; .form (time-displace-channels channel-list displacement . messages)
;; .mode absTime mode - with possible interleaved deltaTime elements
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .parameter displacement The displacement of messages in time units, as described by the pulsesPerQuarterNote attribute of MidiHeader.  Can be positive or negative. 
;; .parameter channel-list A list of channels. (A list of integers in the interval 1..16).
;; .internal-references "Similar function" "time-displace"
(define time-displace-channels
 (xml-in-laml-positional-abstraction
  2 0
  (lambda (channel-list amount cont attr)
   (time-displace-channels-1 channel-list  amount cont))))

(define (time-displace-channels-1 ch-list amount message-list)
 (map 
  (lambda (mes-ast) 
    (if (ast? mes-ast)
        (let ((channel (ast-attribute mes-ast 'channel #f)))
          (if channel
              (let ((channel-num (as-number channel)))
                 (if (member channel-num ch-list)
                     (let ((abs-time (ast-attribute mes-ast 'absTime #f)) 
                           (delta-time (ast-attribute mes-ast 'deltaTime #f)))
                       (cond (abs-time 
                               (copy-ast-mutate-attributes mes-ast
                                                           'absTime (as-int-string (+ (as-number abs-time) amount))))
                             (delta-time
                               mes-ast)
                             (else (laml-error "time-displace-channels: Problems!"))))
                     mes-ast))
              mes-ast))
        mes-ast))
   message-list))

;; As time-displace-channels, but with a scaling of the amount of displacement.
;; .form (time-displace-channels-with-scaling channel-list displacement scaling-fn . messages)
;; .mode absTime mode.
;; .parameter channel-list A list of channels. (A list of integers in the interval 1..16).
;; .parameter displacement The basis amount of displacement of messages in time units, as described by the pulsesPerQuarterNote attribute of MidiHeader.\
;;                         Can be positive or negative. Scaled with scaling-fn before addition.
;; .parameter scaling-fn A scaling function.
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .internal-references "Similar function" "time-displace-channels"
(define time-displace-channels-with-scaling
 (xml-in-laml-positional-abstraction
  3 0
  (lambda (channel-list amount scaling-fn cont attr)
   (time-displace-channels-with-scaling-1 channel-list amount scaling-fn cont))))

(define (time-displace-channels-with-scaling-1 ch-list amount scaling-fn message-list)
 (let* ((number-list (consequtive-numbering-by-predicate (ast-with-channel-pred ch-list) message-list 1 0)) ; progressing integer for each NoteOn message. 
        (number-list-count (length (filter (lambda (x) (> x 0)) number-list)))
        (max-n number-list-count) ; alias
       )
   (map 
    (lambda (mes-ast n) 
      (if (ast? mes-ast)
          (let ((channel (ast-attribute mes-ast 'channel #f)))
            (if channel
                (let ((channel-num (as-number channel)))
                  (if (member channel-num ch-list)
                      (let ((abs-time (ast-attribute mes-ast 'absTime #f)) 
                            (delta-time (ast-attribute mes-ast 'deltaTime #f))
                            (scaled-amount (* amount (scaling-fn (/ n max-n))))
                           )
                        (if (= n 0) (laml-error "time-displace-channels-with-scaling-1: Should not happen"))
                        (cond (abs-time 
                               (copy-ast-mutate-attributes mes-ast
                                                           'absTime (as-int-string (+ (as-number abs-time) scaled-amount))))
                              (delta-time
                                (laml-error "time-displace-channels-with-scaling-1: Only supports absTime"))

                              (else (laml-error "time-displace-channels: Problems!"))))
                      mes-ast))
                mes-ast))
          mes-ast))
    message-list
    number-list)))

; Generates a predicate which asserts that x is a message ast belonging to a given channel list.
(define (ast-with-channel-pred ch-list)
  (lambda (x)
    (and (ast? x) 
         (let ((ch (ast-attribute x 'channel #f)))
           (and ch (member (as-number ch) ch-list))))))




;; Quantize channel to q. Works in both absTime mode and deltaTime mode.
;; Affects all messages with channel attributes.
;; In deltaTime mode the first deltaTime is taken to be 0 (independent of its actual value).
;; deltaTime mode is somewhat experimental.
;; .form (quantize channel q pulses-per-quarter-note . messages)
;; .channel A channel number between 1 and 16. An integer.
;; .parameter q  One of the integer values 1, 2, 4, 8, 16, 32, 64 and 128. As an example, 4 means a quarter note quantification.
;; .parameter pulses-per-quarter-note Must (redundantly) be similar to the  pulsesPerQuarterNote attribute of MidiHeader.  
;; .parameter messages A list of midi messages (such as NoteOn messages)
(define quantize
 (xml-in-laml-positional-abstraction
  3 0
  (lambda (channel q pulses-per-quarter-note cont attr)
    (cond ((abs-time-sequence? cont) (quantize-abs-timing channel q pulses-per-quarter-note cont))
          ((delta-time-sequence? cont) 
              (abs-time-message-list-to-delta-timing 
                (quantize-abs-timing
                   channel q pulses-per-quarter-note (delta-time-message-list-to-abs-timing cont 0))
                0)
          )
          (else (laml-error "quantize: Problems determining absTime or deltaTime mode of sequence"))))))



(define (quantize-abs-timing c q pulses-per-quarter-note message-list)
 (map 
  (lambda (mes-ast) 
    (if (and (ast? mes-ast) )   ;  earlier:  (and ... (equal? "NoteOn" (ast-element-name mes-ast)))
        (let ((channel (ast-attribute mes-ast 'channel #f)))
          (if (and channel (= (as-number channel) c))
              (let ((abs-time (ast-attribute mes-ast 'absTime #f)))
                (if (not abs-time)
                    (laml-error "Can only quantize in absTime mode"))
                (let ((time-interval (time-interval-of-note q pulses-per-quarter-note)))
                  (copy-ast-mutate-attributes mes-ast 
                                             'absTime (quantize-int (as-number abs-time) time-interval))))
               mes-ast))
         mes-ast))
  message-list))

;; Quantize a number of channels to q. Works in both absTime mode and in deltaTime mode.
;; Affects all messages with channel attributes.
;; In deltaTime mode the first deltaTime is taken to be 0 (independent of its actual value).
;; deltaTime mode is somewhat experimental.
;; .form (quantize channel-list q pulses-per-quarter-note . messages)
;; .channel-list A list of channel number between 1 and 16. A list.
;; .parameter q  One of the integer values 1, 2, 4, 8, 16, 32, 64 and 128. As an example, 4 means a quarter note quantification.
;; .parameter pulses-per-quarter-note Must (redundantly) be similar to the  pulsesPerQuarterNote attribute of MidiHeader.  
;; .parameter messages A list of midi messages (such as NoteOn messages)
(define quantize-channels
 (xml-in-laml-positional-abstraction
  3 0
  (lambda (channel-list q pulses-per-quarter-note cont attr)
    (cond ((abs-time-sequence? cont) (quantize-channels-abs-timing channel-list q pulses-per-quarter-note cont))
          ((delta-time-sequence? cont) 
              (abs-time-message-list-to-delta-timing 
                (quantize-channels-abs-timing
                   channel-list q pulses-per-quarter-note (delta-time-message-list-to-abs-timing cont 0))
                0)
          )
          (else (laml-error "quantize-channels: Problems determining absTime or deltaTime mode of sequence"))))))

(define (quantize-channels-abs-timing c-lst q pulses-per-quarter-note message-list)
 (map 
  (lambda (mes-ast) 
    (if (and (ast? mes-ast))  ;  earlier:  (and ... (equal? "NoteOn" (ast-element-name mes-ast)))
        (let ((channel (ast-attribute mes-ast 'channel #f)))
          (if (and channel (member (as-number channel) c-lst))
              (let ((abs-time (ast-attribute mes-ast 'absTime #f)))
                (if (not abs-time)
                    (laml-error "Can only quantize in absTime mode"))
                (let ((time-interval (time-interval-of-note q pulses-per-quarter-note)))
                  (copy-ast-mutate-attributes mes-ast 
                                             'absTime (quantize-int (as-number abs-time) time-interval))))
               mes-ast))
         mes-ast))
  message-list))

;; Distribute all NoteOn in the given channel evenly.
;; Does only work in absTime mode.
;; .form (distribute-even channel . messages)
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .parameter channel A channel number in the interval 1..16.
(define distribute-even
 (xml-in-laml-positional-abstraction
  1 0
  (lambda (channel cont attr)
    (distribute-even-1 channel cont))))

(define (distribute-even-1 channel message-list)
  (ensure-all-abstime-in! "distribute-even" message-list)
  (let* ((relevante-note-on-list 
           (filter (lambda (x)
                     (and (ast? x) (equal? "NoteOn" (ast-element-name x)) (= (as-number (ast-attribute x 'channel #f)) channel)))
                   message-list))
         (number-of-relevant-notes (length relevante-note-on-list ))
        )
    (if (>= number-of-relevant-notes 3)
        (let* ((abs-time-first (as-number (ast-attribute (first relevante-note-on-list) 'absTime #f)))
               (abs-time-last (as-number (ast-attribute (last relevante-note-on-list) 'absTime #f)))
               (distance (/ (- abs-time-last abs-time-first) (- number-of-relevant-notes 1)))
              )
          (distribute-even-2 channel message-list distance 0 abs-time-first) )
        message-list)))

(define (distribute-even-2 channel message-list distance i start-time)
  (cond ((null? message-list) '())
        ((and (ast? (car message-list)) (equal? "NoteOn" (ast-element-name (car message-list))) (= (as-number (ast-attribute (car message-list) 'channel #f)) channel))
           (cons (copy-ast-mutate-attributes (car message-list) 'absTime  (as-int-string (to-int (+ (* i distance) start-time))))
                 (distribute-even-2 channel (cdr message-list) distance (+ i 1) start-time)))
        (else (cons (car message-list) (distribute-even-2 channel (cdr message-list) distance i start-time)))))




; Adjust i to the nearest number (factor * n) for some integer n.
; Assume i is possitive
(define (quantize-int i factor)
  (let* ((half-factor (/ factor 2))
         (rem  (remainder i factor))
         (quot (quotient i factor))
         (grid-point (* quot factor))
        )
    (if (<= rem half-factor)
        grid-point
        (+ grid-point factor))))


(define (time-interval-of-note note-value pulses-per-quarter-note)
  (cond ((= 1 note-value) (* 4 pulses-per-quarter-note))   ; hel node
        ((= 2 note-value) (* 2 pulses-per-quarter-note))   ; halv node
        ((= 4 note-value) pulses-per-quarter-note)         ; fjerdedels node
        ((= 8 note-value) (/ pulses-per-quarter-note 2))   ; ottendedels node
        ((= 16 note-value) (/ pulses-per-quarter-note 4))
        ((= 32 note-value) (/ pulses-per-quarter-note 8))
        ((= 64 note-value) (/ pulses-per-quarter-note 16))
        ((= 128 note-value) (/ pulses-per-quarter-note 32))
        (else (laml-error "time-interval-of-note: note-value must be a power of 2 in between 1 and 128"))))



; Return a list of one note - the note in between note-ast-1 note-ast-2, or the empty list if note-ast-1 and note-ast-2 are too close
(define (calculate-note-in-between note-ast-1 note-ast-2)
  (let ((note-val-1 (as-number (ast-attribute note-ast-1 'note)))
        (note-val-2 (as-number (ast-attribute note-ast-2 'note)))
        (abs-time-1 (as-number (ast-attribute note-ast-1 'absTime #f)))
        (abs-time-2 (as-number (ast-attribute note-ast-2 'absTime #f)))
        (channel-1 (as-number (ast-attribute note-ast-1 'channel)))
        (channel-2 (as-number (ast-attribute note-ast-2 'channel)))
        (velocity-1 (as-number (ast-attribute note-ast-1 'velocity)))
        (velocity-2 (as-number (ast-attribute note-ast-2 'velocity)))
        (duration-1 (as-number (ast-attribute note-ast-1 'duration)))
        (duration-2 (as-number (ast-attribute note-ast-2 'duration)))
        )
    (if (or (not abs-time-1) (not abs-time-2) )
        (laml-error "The function interpolate can only be used with asbTime"))
    (if (> (abs (- note-val-1 note-val-2)) 1)  
      (list (NoteOn 'absTime (as-int-string (to-int (+ abs-time-1 (/ (- abs-time-2 abs-time-1) 2))))
                    'channel channel-1
                    'note (as-int-string (to-int (+ note-val-1 (/ (- note-val-2 note-val-1) 2))))
                    'velocity velocity-1
                    'duration (as-int-string (to-int (/ duration-1 2)))))
      '())))

;; Transform consecutive midi NoteOn messages (two or more) that belong to channels, and which occur at the same time.
;; The tranformer can do whatever calculation necessary on these 'same time messages'. The transformer must return a list of messages (maybe empty).
;; All other messages are not affected.
;; .form (same-time-transform channels transformer . messages)
;; .parameter channels A list of channel numbers in the interval 1..16.
;; .parameter transformer A function from a list of at least two NoteOn messages to a list of messages.\
;;  It is guarantied that this function is called with at least two messages, which both are NoteOn messages in absTime mode.\
;;  All messages passed to transformer are guarantied to take place at the same time.\
;;  The order or the messages passed to transformer is unknown.\
;;  The transformer must return a list of midi message asts (maybe an empty list), typically (but necessarily) NoteOn messages.
;; .parameter messages A list of midi messages.
;; .mode absTime 
;; .misc You can use this function to repair multiple track which must play in parallel.\
;;       In a typical scenario you have manually corrected note values in one of the channels, and you now wish to enforce another channel to introduce the samme note corrections.
;; .reference "Example" "Midi LAML examples" "../examples/same-time-transform/index.html"
(define same-time-transform
 (xml-in-laml-positional-abstraction 2 0 
   (lambda (channels transformer contents attributes)
      (same-time-transform-1 channels transformer contents))))

; Assume as a precondition that message-list is sorted by absTime.
(define (same-time-transform-1 channels transformer message-list)
  (same-time-transform-2 channels transformer message-list '() '()))

(define (same-time-transform-2 channels transformer message-list same-time-lst result-lst)
  (if (null? message-list)
      (reverse (append same-time-lst result-lst))
      (let* ((mes (first message-list))
             (absTime? (ast-attribute mes 'absTime #f))
             (same-mes (if (not (null? same-time-lst)) (first same-time-lst) #f))
             (same-mes-ch (if same-mes (ast-attribute same-mes 'channel #f) #f))
            )
        (if (not absTime?) (laml-error "Same time transformation must occur in pure abs-time mode."))
        (cond  
                ; adding to same-time-lst:
                ((and same-mes   ; same-time-lst non-empty
                     (ast? mes)
                     (equal? (ast-attribute same-mes 'absTime) (ast-attribute mes 'absTime))  ; same time                          
                     same-mes-ch (member (as-number same-mes-ch) channels))
                  (same-time-transform-2 channels transformer (cdr message-list) (cons mes same-time-lst) result-lst))

                ; transforming non-singleton same-time-lst. Start new same-time-lst.
                ((and (ast? mes) (>= (length same-time-lst) 2))
                   (same-time-transform-2 channels transformer (cdr message-list) (list mes)
                                          (append (maybe-transformer transformer (filter (NoteOnCh? channels) same-time-lst)) (filter (negate (NoteOnCh? channels)) same-time-lst) result-lst)))

                ; ditto - do not start new same-time-lst.
                ((and (not (ast? mes)) (>= (length same-time-lst) 2))
                   (same-time-transform-2 channels transformer (cdr message-list) '() 
                                         (append (maybe-transformer transformer (filter (NoteOnCh? channels) same-time-lst)) (filter (negate (NoteOnCh? channels)) same-time-lst) result-lst)))

                ;; singleton same-time-lst. Do not transform.
                ((and (ast? mes) same-mes (< (length same-time-lst) 2))                                                                  
                   (same-time-transform-2 channels transformer (cdr message-list) (list mes) (append same-time-lst result-lst)))

                ;; ditto - do not start new same-time-lst. 
                ((and (not (ast? mes)) same-mes (< (length same-time-lst) 2))                                                                  
                   (same-time-transform-2 channels transformer (cdr message-list) '() (append same-time-lst result-lst)))

                ;; put mes in empty same-time-lst
                ((ast? mes) 
                   (same-time-transform-2 channels transformer (cdr message-list) (list mes) result-lst))

                ;; drop mes - not ast
                (else 
                   (same-time-transform-2 channels transformer (cdr message-list) same-time-lst result-lst))))))

(define (maybe-transformer transformer lst)
  (if (>= (length lst) 2)
      (transformer lst)
      lst))

;; Cut all messages in channel-list that starts after abs-cut-time, and (if necessary) adjust the duration of messages in channel-list that start before abs-cut-time such that they end at abs-cut-time or before.
;; Messages not in channel-list are not affected at all.
;; .form (cut-at-time abs-cut-time channel-list . messages)
;; .parameter channel-list A list of channel numbers in the interval 1..16 or the boolean value #t in the meaning of all channels between 1 and 16.  An integer list or boolean #t.
;; .parameter abs-cut-time The absolute time at which to cut. A non-negative integer. 
;; .returns The remaining messages and the adjusted messages in channel-list, together with all messages not in channel-list. 
;; .mode absTime 
(define cut-at-time
 (xml-in-laml-positional-abstraction
  2 0
  (lambda (abs-cut-time channel-list cont attr)
   (cut-at-time-1 abs-cut-time channel-list cont))))

(define (cut-at-time-1 abs-cut-time channel-list-0 message-list)
 (ensure-all-abstime-in! "cut-at-time-1"  message-list)
 (let ((channel-list (if (and (boolean? channel-list-0) channel-list-0) (number-interval 1 16) channel-list-0)))
   (cut-at-time-2 abs-cut-time channel-list message-list '())))

(define (cut-at-time-2 abs-cut-time channel-list message-list res-lst)
  (if (null? message-list)
      (reverse res-lst)
      (let* ((m (car message-list))
             (m-in-relevant-channel (and (channel-message? m) (member (midi 'channel m) channel-list))))
        (cond ((and m-in-relevant-channel (not (NoteOn? m)) (> (midi 'absTime m) abs-cut-time))                                                             ; Non-NoteOn that starts after cut time are deleted
                 (cut-at-time-2 abs-cut-time channel-list (cdr message-list) res-lst))
              ((and m-in-relevant-channel (NoteOn? m) (> (midi 'absTime m) abs-cut-time))                                                                   ; NoteOn messages that starts after cut time are deleted in the same way
                 (cut-at-time-2 abs-cut-time channel-list (cdr message-list) res-lst))
              ((and m-in-relevant-channel (NoteOn? m) (<= (midi 'absTime m) abs-cut-time) (> (+ (midi 'absTime m) (midi 'duration m)) abs-cut-time))        ; Note messages that 'cross' the cut-time are given shorter duration 
                 (cut-at-time-2 abs-cut-time channel-list (cdr message-list)
                               (cons (copy-ast-mutate-attributes m 'duration (- abs-cut-time (midi 'absTime m))) res-lst)))
              (else (cut-at-time-2 abs-cut-time channel-list (cdr message-list) (cons m res-lst)))                                                          ; Keep all other messages
         ))))


;;; .section-id midi-list-fn-note
;;; Message List functions - Note related

;; Change octave on channel ch with n. 
;; Works in both absTime and deltaTime mode.
;; .form (octave channel n . messages)
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .parameter n Relative octave number.  n can be positive or negative. The amount 0 is neutral.
(define octave
 (xml-in-laml-positional-abstraction
  2 0
  (lambda (ch n cont attr)
    (octave-1 ch n cont))))

(define (octave-1 c n message-list)
 (map 
  (lambda (mes-ast) 
    (if (and (ast? mes-ast) (or (equal? "NoteOn" (ast-element-name mes-ast)) (equal? "NoteOff" (ast-element-name mes-ast))))
        (let ((channel (ast-attribute mes-ast 'channel #f)))
          (if (and channel (= (as-number channel) c))
              (let* ((old-note (as-number (ast-attribute mes-ast 'note)))
                     (new-note (+ old-note (* 12 n))))
                (if (or (> new-note 127) (< new-note 0))
                    (laml-error "Octave underflow or overflow" (ast-attribute mes-ast 'info) new-note))
                (copy-ast-mutate-attributes mes-ast 'note new-note))
              mes-ast))
        mes-ast))
  message-list))




;; Put an interpolation note in between every note on the given channel.
;; Works only in absTime mode.
;; .form (interpolate channel . messages)
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .parameter channel A channel number in the interval 1..16.
(define interpolate
 (xml-in-laml-positional-abstraction
  1 0
  (lambda (channel cont attr)
    (interpolate-1 channel cont #f))))

(define (interpolate-1 ch message-list prev-ast)
  (cond ((and (null? message-list) prev-ast) (list prev-ast))
        ((and (null? message-list) (not prev-ast)) '())

        ((and (ast? (car message-list))
              (equal? "NoteOn" (ast-element-name (car message-list)))
              (= (as-number (ast-attribute (car message-list) 'channel #f)) ch))
            (if (not prev-ast)   ; only in beginning
                (interpolate-1 ch (cdr message-list) (car message-list))  ; now the first prev-ast is in place
                (let* ((first prev-ast)
                       (second (car message-list))
                       (note-in-between-list (calculate-note-in-between first second))
                      )
                  (cons 
                     (cons first note-in-between-list)
                     (interpolate-1 ch (cdr message-list) second)
                  ))))

        (else (cons (car message-list)
                    (interpolate-1 ch (cdr message-list) prev-ast))) ))

;; Transpose all channels with amount. 
;; amount can be negative, 0 (for no transposition), or positive.
;; Works for both deltaTime and absTime.
;; .form (transpose amount . messages)
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .parameter amount A relative number of half note values (positive or negative). The amount 0 is neutral.
(define transpose
 (xml-in-laml-positional-abstraction 1 0
   (lambda (amount contents attributes)
     (transpose-1 amount contents))))

(define (transpose-1 amount message-list)
 (map 
  (lambda (mes-ast) 
    (if (and (ast? mes-ast) (or (equal? "NoteOn" (ast-element-name mes-ast)) (equal? "NoteOff" (ast-element-name mes-ast))))
        (let* ((old-note (as-number (ast-attribute mes-ast 'note)))
               (new-note (+ old-note amount)))
          (copy-ast-mutate-attributes mes-ast 'note new-note))
        mes-ast))
  message-list))


;; Transpose channels in channel-list with amount. 
;; amount can be negative, 0 (for no transposition), or positive.
;; Works for both deltaTime and absTime.
;; .form (transpose-channels channel-list amount . messages)
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .parameter amount A relative number of half note values (positive or negative). The amount 0 is neutral.
;; .parameter channel-list A list of channels. (A list of integers in the interval 1..16).
(define transpose-channels
 (xml-in-laml-positional-abstraction 2 0
   (lambda (channel-list amount contents attributes)
     (transpose-channels-1 channel-list amount contents))))

(define (transpose-channels-1 ch-list amount message-list)
 (map 
  (lambda (mes-ast) 
    (if (and (ast? mes-ast) (or (equal? "NoteOn" (ast-element-name mes-ast)) (equal? "NoteOff" (ast-element-name mes-ast))))
        (let ((channel (ast-attribute mes-ast 'channel #f)))
          (if channel
              (if (member (as-number channel) ch-list)
                  (let* ((old-note (as-number (ast-attribute mes-ast 'note)))
                         (new-note (+ old-note amount)))
                    (copy-ast-mutate-attributes mes-ast 'note new-note))
                  mes-ast)
              mes-ast))
        mes-ast))
  message-list))

;; Map the notes in channel-list to other notes. Notes in other channels are NOT affected.
;; Notes in affected channels which are not mentioned in the note-map are removed.
;; The note-map has the form as for example ((f c) (a e) (c g)).
;; The note f is mapped to note c, etc. 
;; Each element in the list is a mapping-entry of source-note and target-note symbols.
;; The possible note symbols are c, c#, d, d#, e, f, f#, g, g#, a, a#, b (or h). Lower and upper case symbols both work. 
;; The octave of the source notes is respected (although it is not controllable if the note 'goes up' or 'goes down').
;; .form (map-notes-in-channels note-map channels-list . messages)
;; .parameter note-map A list of note pairs (each pair a list of symbols, of length 2).
;; .parameter channel-list A list of channels. Elements belong to the interval 1..16.
;; .parameter messages A list of midi messages.
;; .misc Intended to be used for corrections of the automatic accompaniment, played on several channels.
(define map-notes-in-channels 
 (xml-in-laml-positional-abstraction 2 0 
   (lambda (note-map channel-list contents attributes)
      (map-notes-in-channels-1 note-map channel-list contents))))

(define (map-notes-in-channels-1 note-map channel-list message-list)
  (let ((note-map-numbers (map (lambda (map-entry) 
                                 (let ((source (car map-entry))
                                       (dest (cadr map-entry)))
                                   (list (number-of-note-name (upcase-string (as-string source))) (number-of-note-name (upcase-string (as-string dest))))))
                               note-map)))
    (letrec ((note-mapper (lambda (source-note-value) ; a midi note-value, such as 64. Mapped to the appropriate midi note value, or #f is it is not mapped.
                            (let ((map-res (find-in-list (lambda (entry) (= (first entry) (remainder source-note-value 12))) note-map-numbers)))
                              (if map-res (+ (* (quotient source-note-value 12) 12) (second map-res)) #f)))))
      (map-notes-in-channels-2 note-mapper channel-list message-list '())
    )
  )
)

; iterative processing:
(define (map-notes-in-channels-2 note-mapper channel-list message-list res-lst)
  (cond ((null? message-list) (reverse res-lst))
        (else (let ((message (car message-list)))
                 (if (ast? message)
                     (if (NoteOn? message)
                         (if (member (as-number (ast-attribute message 'channel)) channel-list)
                             (let* ((target-note-value (note-mapper (as-number (ast-attribute message 'note)))))
                               (if target-note-value
                                   (map-notes-in-channels-2 note-mapper channel-list (cdr message-list) 
                                         (cons (copy-ast-mutate-attributes message 'note target-note-value) res-lst))  ; map note value!!
                                   (map-notes-in-channels-2 note-mapper channel-list (cdr message-list)
                                         (cons (copy-ast-mutate-attributes message 'velocity "1") res-lst))  ; exclude message - muted (cannot use value 0 - WHY???).
                               )
                             )
                             (map-notes-in-channels-2 note-mapper channel-list (cdr message-list) (cons message res-lst)))  ; unchanged - other channel
                         (map-notes-in-channels-2 note-mapper channel-list (cdr message-list) (cons message res-lst))) ;  unchanged - non-NoteOn
                     (map-notes-in-channels-2 note-mapper channel-list (cdr message-list) res-lst) ; drop: non-ast
                  )
               )
         )
  )
)

;;; .section-id midi-list-fn-vel
;;; Message List functions - Velocity related


;; Fade out linearly all channels in message-list.
;; Works in both absTime and deltaTime mode.
;; .form (fade-out . messages)
;; .mode Both deltaTime and absTime
;; .parameter messages A list of midi messages (such as NoteOn messages)
(define (fade-out . message-list)
  (fade-out-1 message-list))

(define (fade-out-1 message-list)
  (let ((lgt (length message-list)))
    (map 
      (lambda (mes-ast i) 
        (if (equal? (ast-element-name mes-ast) "NoteOn")
            (copy-ast-mutate-attributes mes-ast
               'velocity (fade-velocity lgt i (as-number (ast-attribute mes-ast 'velocity))))
            mes-ast))
      message-list
      (number-interval 1 lgt))))

;; Fade out linearly channelels in channel-list in message-list.
;; Works in both absTime and deltaTime mode.
;; .form (fade-out . messages)
;; .mode Both deltaTime and absTime
;; .parameter channel-list A list of channels. (A list of integers in the interval 1..16).
;; .parameter messages A list of midi messages (such as NoteOn messages)
(define (fade-out-channels channel-list . message-list)
  (fade-out-channel-1 channel-list message-list))

(define (fade-out-channel-1 channel-list message-list)
  (let ((lgt (length message-list)))
    (map 
      (lambda (mes-ast i) 
        (let ((channel (ast-attribute mes-ast 'channel #f)))
          (if channel
              (let ((channel-num (as-number channel)))
                 (if (member channel-num channel-list)
                     (if (equal? (ast-element-name mes-ast) "NoteOn")
                         (copy-ast-mutate-attributes mes-ast
                                                     'velocity (fade-velocity lgt i (as-number (ast-attribute mes-ast 'velocity))))
                         mes-ast)  ; Not NoteOn
                     mes-ast)) ; Not right channel
               mes-ast))) ; Not channel message
      message-list
      (number-interval 1 lgt))))

(define (fade-velocity n i input-velocity)
  (as-int-string (between 0 127 (to-int (/ (* input-velocity (- n i)) n)))))

          



;; Add amount to velocity of a given channel.
;; If the volicity exceeds the limits (0..127) it is enforced to the lower/upper limit.
;; Works in both absTime and deltaTime mode.
;; .form (add-to-velocity channel amount . messages)
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .parameter amount An amount to add to the velocity. Positive or negative.
(define add-to-velocity
 (xml-in-laml-positional-abstraction
  2 0
  (lambda (channel amount cont attr)
   (add-to-velocity-1 channel amount cont))))

(define (add-to-velocity-1 channel amount message-list)
 (map 
  (lambda (mes-ast) 
    (if (and (ast? mes-ast) (equal? (ast-element-name mes-ast) "NoteOn") (= (as-number (ast-attribute mes-ast 'channel)) channel))
        (let ((velocity (ast-attribute mes-ast 'velocity #f)))
          (cond (velocity
                 (copy-ast-mutate-attributes mes-ast
                                            'velocity (as-int-string (between 0 127 (+ (as-number velocity) amount)))))
                (else (laml-error "Cannot find velocity of NoteOn message. Should not happen"))))
        mes-ast))
  message-list))


;;; .section-id midi-list-fn-dur
;;; Message List functions - Duration related

;; Tie all notes in channel together, in order to eliminate breaks.
;; This affects the duration of all notes in the given channel, such that one note lasts until the beginning of the next.
;; More precisely, the duration becomes the maximum of the existing duration and the absTime difference between this note and the next (in the given channel). 
;; Non-channel notes and notes in other channels are not affected by this function.
;; Works only in absTime mode (not checked).
;; .form (eliminate-breaks channel . messages)
;; .parameter channel. A channel between 1 and 16. An integer.
;; .parameter messages A list of midi messages.
;; .internal-references "More advanced variant" "legato-in-channel"
;; .mode absTime
(define eliminate-breaks 
 (xml-in-laml-positional-abstraction 1 0 
   (lambda (channel contents attributes)
    (eliminate-breaks-1 channel contents))))

(define (eliminate-breaks-1 channel messages)
  (let* ((channel-messages (filter (lambda (x) (and (ast? x) (NoteOn? x) (= channel (midi 'channel x)))) messages))
         (abs-time-list (map (lambda (noteon-ast) (midi 'absTime noteon-ast)) channel-messages))
         (abs-time-diff-list (map2 (lambda (at1 at2) (- at1 at2)) (cdr abs-time-list) abs-time-list)))
   (adjust-duration channel messages abs-time-diff-list '())))

; Adjust duration of messages in the given channel to the max of the existing duration and the calculated absTime diffes in abs-time-diff-list
; Iterative.
(define (adjust-duration channel messages abs-time-diff-list res-messages)
  (cond ((null? messages) (reverse res-messages))
        ((not (ast? (car messages))) (adjust-duration channel (cdr messages) abs-time-diff-list res-messages)) ; just skip
        ((not (has-ast-attribute? (car messages) 'channel)) (adjust-duration channel (cdr messages) abs-time-diff-list (cons (car messages) res-messages))) ; non-channel mes
        ((and (NoteOn? (car messages)) (= channel (midi 'channel (car messages))) (not (null? abs-time-diff-list)))  ; channel NoteOn message
                 (adjust-duration channel 
                                  (cdr messages)
                                  (cdr abs-time-diff-list)
                                  (cons (copy-ast-mutate-attributes (car messages) 'duration (max (midi 'duration (car messages)) (car abs-time-diff-list)))
                                        res-messages)))
        (else (adjust-duration channel (cdr messages) abs-time-diff-list (cons (car messages) res-messages)))))


;; Tie the notes in channel together - legato. Affects the duration attribute of notes in channel. Notes in other channels, and non-NoteOn messages, are not affected.
;; Works in both time modes, but it is slightly more general in abs time mode than in delta time mode.
;; .form (legato-in-channel channel . messages)
;; .parameter channel A channel number. An integer between 1 and 16.
;; .parameter messages A list of midi messages.
;; .attribute time-slack-delta implied The allowed time slack for notes in channel to be considered as activated at the same time,\
;;                                     without being tied together mutually.\
;;                                     Defaults to 20. A number of time ticks. A non-negative integer.
;; .attribute strict implied If strict, the duration between notes in channel becomes the time difference between them. Time slack still applies, however.\
;;                           If non-strict, the time may be longer (if the duration is already longer).\
;;                           A boolean (true or false).
;; .attribute offset implied A number added to the duration in addition to the calculated duration. Can be used to enforce more overlapping, or less overlapping. A positive or negative integer.
;; .attribute max-tie-span implied If provided, gives the maximum span (in terms of abs time difference) that we attempt to tie.\
;;                                 The duration is only affected if it is less than the value of this attribute. A number of time ticks. If provided, a non-negative integer.\
;;                                 If not provided, the max tie span is in principle infinitely large.\
;;                                 This attribute must be #f in delta time mode.
;; .internal-references "Simpler variant" "eliminate-breaks"
;; .misc This function takes care never to introduce negative duration (which causes severe confusion).
(define legato-in-channel
 (xml-in-laml-positional-abstraction 1 0 
   (lambda (channel contents attributes)
     (let ((time-slack-delta (as-number (defaulted-get-prop 'time-slack-delta attributes 20)))
           (strict (as-boolean (defaulted-get-prop 'strict attributes #f)))
           (offset (as-number (defaulted-get-prop 'offset attributes 0)))
           (max-tie-span (defaulted-get-prop 'max-tie-span attributes #f))
          )
      (legato-in-channel-1 channel time-slack-delta strict max-tie-span offset contents)))))

(define (legato-in-channel-1 channel time-slack-delta strict max-tie-span offset message-list)
 (let ((message-list-1 (filter ast? message-list)))
  (cond ((abs-time-sequence? message-list-1)
           (legato-in-channel-abs-time channel time-slack-delta strict max-tie-span offset message-list-1 '() 0 '()))
        ((delta-time-sequence? message-list-1)
           (if max-tie-span (laml-error "legato-in-channel-1: max-timespan cannot be used in delta time mode"))
           (legato-in-channel-delta-time channel time-slack-delta strict offset message-list-1 '() 0 '() 0))
        (else (laml-error "legato-in-channel-1: Either pure absTime or pure deltaTime mode is expected")))))

; t0 is the absolute time (relative to the place of application) of the first NoteOn message that belongs to channel in message-list
; tcur is the current absolute time, as accumulated during traversal. tcur is new in the delta time version.
(define (legato-in-channel-delta-time channel time-slack-delta strict offset message-list pending-message-list t0 res-list tcur)
    (cond ((null? message-list)
             (append (reverse res-list) (reverse pending-message-list)))
          (else (let* ((first-mes (first message-list))
                       (time-first (+ tcur (as-number (ast-attribute first-mes 'deltaTime)))))
                  (display-message (length pending-message-list) "note = " (midi 'note first-mes) "time-first = " time-first "t0 = " t0 "tcur = " tcur)
                  (cond 

                        ; getting started: going through messages irrelevant for legato in channel.
                        ((and (null? pending-message-list)
                              (or (not (NoteOn? first-mes))
                                  (and (NoteOn? first-mes) (not (= channel (as-number (ast-attribute first-mes 'channel)))))))
                          (legato-in-channel-delta-time channel time-slack-delta strict offset (cdr message-list) '() 0 (cons first-mes res-list) 
                                                        (+ tcur (as-number (ast-attribute first-mes 'deltaTime)))))

                        ; locating a Noteon in channel. Serves as the first reference point in pending-message-list.
                        ((and (null? pending-message-list)
                                     (and (NoteOn? first-mes) (= channel (as-number (ast-attribute first-mes 'channel)))))
                          (legato-in-channel-delta-time channel time-slack-delta strict offset (cdr message-list) (list first-mes) 
                                                     time-first res-list (+ tcur (as-number (ast-attribute first-mes 'deltaTime)))))

                        ; locating messages to pass to pending messages. Either nearby NoteOns in samme channel, or irrelevant messages.
                        ((and (not (null? pending-message-list))
                              (or (not (NoteOn? first-mes))
                                  (and (NoteOn? first-mes) (not (= channel (as-number (ast-attribute first-mes 'channel)))))
                                  (and (NoteOn? first-mes) (= channel (as-number (ast-attribute first-mes 'channel))) (<= (- time-first t0) time-slack-delta))))
                         (legato-in-channel-delta-time channel time-slack-delta strict offset (cdr message-list) (cons first-mes pending-message-list) t0 res-list 
                                                       (+ tcur (as-number (ast-attribute first-mes 'deltaTime)))))

                        ; Locating a more distant NoteOn messages in channel. Now add the pending messages up to first-mes. And continue recursively.
                        (else
                         (legato-in-channel-delta-time channel time-slack-delta strict offset (cdr message-list) (list first-mes)
                                                     time-first
                                                     (let* ((note-to-modify (last pending-message-list))
                                                            (note-now-modified 
                                                              (copy-ast-mutate-attributes
                                                                note-to-modify 'duration (cond (strict (max 0 (+ offset (- time-first t0))))
                                                                                               (else   (max 0 (+ offset (max (as-number (ast-attribute note-to-modify 'duration))
                                                                                                                             (- time-first t0))))))))
                                                            (other-pendings-messages (butlast pending-message-list)))
                                                       (append (append other-pendings-messages (list note-now-modified)) res-list)) 
                                                      (+ tcur (as-number (ast-attribute first-mes 'deltaTime))))) )))))


(define (legato-in-channel-abs-time channel time-slack-delta strict max-tie-span offset message-list pending-message-list t0 res-list)
    (cond ((null? message-list)
             (append (reverse res-list) (reverse pending-message-list)))
          (else (let* ((first-mes (first message-list))
                       (time-first (as-number (ast-attribute first-mes 'absTime))))
                  (display-message (length pending-message-list) " " (midi 'note first-mes))
                  (cond 

                        ; getting started: going through messages irrelevant for legato in channel.
                        ((and (null? pending-message-list)
                              (or (not (NoteOn? first-mes))
                                  (and (NoteOn? first-mes) (not (= channel (as-number (ast-attribute first-mes 'channel)))))))
                          (legato-in-channel-abs-time channel time-slack-delta strict max-tie-span offset (cdr message-list) '() 0 (cons first-mes res-list)))

                        ; locating a Noteon in channel. Serves as first reference point in pending-message-list.
                        ((and (null? pending-message-list)
                                     (and (NoteOn? first-mes) (= channel (as-number (ast-attribute first-mes 'channel)))))
                          (legato-in-channel-abs-time channel time-slack-delta strict max-tie-span offset (cdr message-list) (list first-mes) 
                                                     (as-number (ast-attribute first-mes 'absTime)) res-list))

                        ; locating messages to pass to pending messages. Either close NoteOns in samme channel or irrelevant messages.
                        ((and (not (null? pending-message-list))
                              (or (not (NoteOn? first-mes))
                                  (and (NoteOn? first-mes) (not (= channel (as-number (ast-attribute first-mes 'channel)))))
                                  (and (NoteOn? first-mes) (= channel (as-number (ast-attribute first-mes 'channel))) (<= (- time-first t0) time-slack-delta))))
                         (legato-in-channel-abs-time channel time-slack-delta strict max-tie-span offset (cdr message-list) (cons first-mes pending-message-list) t0 res-list))

                        ; Locating a more distant NoteOn messages in channel. Now add pending messages up to first-mes.
                        (else
                         (legato-in-channel-abs-time channel time-slack-delta strict max-tie-span offset (cdr message-list) (list first-mes)
                                                     (as-number (ast-attribute first-mes 'absTime))
                                                     (append 
                                                      (map (lambda (pending-mes)
                                                             (if (and (NoteOn? pending-mes) (= channel (as-number (ast-attribute pending-mes 'channel))))
                                                                 (if (or (not max-tie-span)
                                                                         (and max-tie-span
                                                                              (<= (- (as-number (ast-attribute first-mes 'absTime))
										     (as-number (ast-attribute pending-mes 'absTime)))
										  (as-number max-tie-span))))
                                                                     (copy-ast-mutate-attributes pending-mes 
									    'duration 
                                                                                (cond (strict (max 0 (+ offset (- (as-number (ast-attribute first-mes 'absTime))
                                                                                                                  (as-number (ast-attribute pending-mes 'absTime))))))
                                                                                      (else   (max 0 (+ offset (max (as-number (ast-attribute pending-mes 'duration))
                                                                                                                    (- (as-number (ast-attribute first-mes 'absTime))
                                                                                                                       (as-number (ast-attribute pending-mes 'absTime)))))))))
                                                                 pending-mes)
                                                              pending-mes))
                                                           pending-message-list)
                                                      res-list))))))))


;;; .section-id midi-list-fn-others
;;; Message List functions - Control Related and Misc

;; Gradually pan a given channel from pan-from to pan-to. Works for both deltaTime and absTime.
;; Inserts one ControlChange message for each relevant NoteOn message.
;; The limits of pan-from and pan-to are 0..127. If exceeded, automatic cut off to min/max value is provided.
;; .form (pan-flow channel pan-from pan-to . messages)
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .parameter channel A channel number in the interval 1..16.
;; .parameter pan-from The initial pan value. 64 is neutral, 0 is hard left, and 127 is hard right.
;; .parameter pan-to The final pan value. 64 is neutral, 0 is hard left, and 127 is hard right.
(define pan-flow 
 (xml-in-laml-positional-abstraction 3 0
   (lambda (channel pan-from pan-to contents attributes)
     (pan-flow-1 channel pan-from pan-to contents))))

(define (pan-flow-1 ch pan-from pan-to message-list)
  (let* ((pan-diff (abs (- pan-to pan-from)))
         (number-of-note-ons 
            (length (filter (lambda (x) (and (ast? x) (equal? "NoteOn" (ast-element-name x)) (= ch (as-number (ast-attribute x 'channel -1))))) message-list)))
         (pan-step (if (> number-of-note-ons 1)
                       (if (< pan-from pan-to)
                           (/ pan-diff (- number-of-note-ons 1))
                           (- (/ pan-diff (- number-of-note-ons 1))))
                       #f))
        )
    (if pan-step
        (pan-flow-2 ch pan-from pan-to pan-step 0 message-list)
        message-list)))

(define (pan-flow-2 ch pan-from pan-to pan-step i message-list)
  (cond ((null? message-list) '())
        ((and (ast? (car message-list)) (equal? "NoteOn" (ast-element-name (car message-list))))
           (let* ((mes-ast (car message-list))
                  (channel (ast-attribute mes-ast 'channel #f)))
             (if (and channel (= (as-number channel) ch))
                 (cons 
                   (list 
                      (ControlChange 'deltaTime "0" 'channel ch 'control "10" 'value (as-int-string (between 0 127 (to-int (+ pan-from (* i pan-step))))))
                       mes-ast)
                   (pan-flow-2 ch pan-from pan-to pan-step (+ i 1) (cdr message-list)))
                 (cons mes-ast (pan-flow-2 ch pan-from pan-to pan-step i (cdr message-list))))))
        (else 
          (cons (car message-list) (pan-flow-2 ch pan-from pan-to pan-step i (cdr message-list))))))

;; Gradually pan a given channel from pan-level towards left, then to the right and back to pan-level.
;; Works for both deltaTime and absTime.
;; Inserts one ControlChange message for each relevant NoteOn message.
;; .form (pan-left-right channel pan-level . messages)
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .parameter channel A channel number in the interval 1..16.
;; .parameter pan-level The initial pan level value. 64 is neutral, 0 is hard left, and 127 is hard right.
(define pan-left-right 
 (xml-in-laml-positional-abstraction 2 0
   (lambda (channel pan-level contents attributes)
     (pan-left-right-1 channel pan-level contents))))

(define (pan-left-right-1 ch pan-level message-list)
 (letrec (
          (arc-pan (lambda (pan-value)     ; pan-value between 0 and 128. Return an "arccos value"...
                      (acos (/ (- pan-value 64) 64))))

          (pan-fn (lambda (x)
                    (+ 64 (* 64 (cos x)))))

          (interval-of-reals (lambda (lower upper step)
                               (if (< lower upper)
                                   (cons lower (interval-of-reals (+ lower step) upper step))
                                   '())))

          (interval-of-n-reals (lambda (lower upper n)
                                 (let ((step (/ (- upper lower) n)))
                                   (interval-of-reals lower upper step))))

          (PI 3.14159)
         )
  (let* ((number-of-note-ons 
            (length (filter (lambda (x) (and (ast? x) (equal? "NoteOn" (ast-element-name x)) (= ch (as-number (ast-attribute x 'channel -1))))) message-list)))
         (ap (arc-pan pan-level))   ; (pan-fn ap) = pan-level
         (pan-value-list (map (lambda (r) (pan-fn r)) (interval-of-n-reals ap (+ ap (* 2 PI)) number-of-note-ons)))   ; a list of values in the interval 0 .. 128
         (pan-message-list 
            (map (lambda (pan-val) 
                    (ControlChange 'deltaTime "0" 'channel ch 'control "10" 'value (as-int-string (between 0 127 (to-int pan-val)))))
                 pan-value-list))
        )
    (pan-left-right-2 ch pan-level pan-message-list message-list))))


(define (pan-left-right-2 ch pan-level pan-message-list message-list)
  (cond ((null? message-list) (list 
                                 (ControlChange 'deltaTime "0" 'channel ch 'control "10" 'value pan-level)))
        ((and (ast? (car message-list)) (equal? "NoteOn" (ast-element-name (car message-list))))
           (let* ((mes-ast (car message-list))
                  (channel (ast-attribute mes-ast 'channel #f)))
             (if (and channel (= (as-number channel) ch))
                 (cons 
                   (list 
                      (if (not (null? pan-message-list)) (car pan-message-list) '())
                       mes-ast)
                   (pan-left-right-2 ch pan-level (cdr pan-message-list) (cdr message-list)))
                 (cons mes-ast (pan-left-right-2 ch pan-level pan-message-list (cdr message-list))))))
        (else 
          (cons (car message-list) (pan-left-right-2 ch pan-level pan-message-list (cdr message-list))))))


;; Gradually change the channel volume a given channel from channel-volume-from to channel-volume-to. Works for both deltaTime and absTime.
;; The limits of channel-volume-from and channel-volume-to are 0..127. If exceeded, automatic cut off to min/max value is provided.
;; .form (channel-volume-flow channel channel-volume-from channel-volume-to . messages)
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .parameter channel A channel number in the interval 1..16.
;; .parameter channel-volume-from The initial channel-volume value. An integer between 0 and 127.
;; .parameter channel-volume-to The final channel-volume value. An integer between 0 and 127.
(define channel-volume-flow 
 (xml-in-laml-positional-abstraction 3 0
   (lambda (channel channel-volume-from channel-volume-to contents attributes)
     (channel-volume-flow-1 channel channel-volume-from channel-volume-to contents))))

(define (channel-volume-flow-1 ch channel-volume-from channel-volume-to message-list)
  (let* ((channel-volume-diff (abs (- channel-volume-to channel-volume-from)))
         (number-of-note-ons 
            (length (filter (lambda (x) (and (ast? x) (equal? "NoteOn" (ast-element-name x)) (= ch (as-number (ast-attribute x 'channel -1))))) message-list)))
         (channel-volume-step (if (> number-of-note-ons 1)
                       (if (< channel-volume-from channel-volume-to)
                           (/ channel-volume-diff (- number-of-note-ons 1))
                           (- (/ channel-volume-diff (- number-of-note-ons 1))))
                       #f))
        )
    (if channel-volume-step
        (channel-volume-flow-2 ch channel-volume-from channel-volume-to channel-volume-step 0 message-list)
        message-list)))

(define (channel-volume-flow-2 ch channel-volume-from channel-volume-to channel-volume-step i message-list)
  (cond ((null? message-list) '())
        ((and (ast? (car message-list)) (equal? "NoteOn" (ast-element-name (car message-list))))
           (let* ((mes-ast (car message-list))
                  (channel (ast-attribute mes-ast 'channel #f)))
             (if (and channel (= (as-number channel) ch))
                 (cons 
                   (list 
                      (ControlChange 'deltaTime "0" 'channel ch 'control "7" 'value (as-int-string (between 0 127 (to-int (+ channel-volume-from (* i channel-volume-step))))))
                       mes-ast)
                   (channel-volume-flow-2 ch channel-volume-from channel-volume-to channel-volume-step (+ i 1) (cdr message-list)))
                 (cons mes-ast (channel-volume-flow-2 ch channel-volume-from channel-volume-to channel-volume-step i (cdr message-list))))))
        (else 
          (cons (car message-list) (channel-volume-flow-2 ch channel-volume-from channel-volume-to channel-volume-step i (cdr message-list))))))

;; Eliminate all sustain ControlChange messages on the given channel in message-list.
;; .form (no-sustain channel . message-list)
;; .parameter channel A channel number in the interval 1..16, or #t as a wildchard for any channel.
;; .parameter message-list A list of midi messages (such as NoteOn messages)
(define no-sustain
 (xml-in-laml-positional-abstraction 1 0
   (lambda (channel contents attributes)
    (no-sustain-1 channel contents))))

(define (no-sustain-1 ch message-list)
 (eliminate-control-change-1 ch 64 message-list))


;; Eliminate ControlChange messages in a given channel and with a given control number.
;; Works in both deltaTime and absTime mode.
;; .form (eliminate-control-change channel control . messages)
;; .parameter channel A channel number in the interval 1..16. The value #t serves as a wilchard for any channel.
;; .parameter control A control number in the interval 0 ..127. The value #t serves as a wildchard for any control number.
;; .parameter messages A list of midi messages.
(define eliminate-control-change
 (xml-in-laml-positional-abstraction 2 0
   (lambda (channel control contents attributes)
      (eliminate-control-change-1 channel control contents))))

(define (eliminate-control-change-1 ch cntrl message-list)
 (eliminate-midi-null-events
  (map 
   (lambda (mes-ast) 
     (if (and (ast? mes-ast)
              (equal? (ast-element-name mes-ast) "ControlChange")
              (if (and (boolean? cntrl) cntrl) #t (= cntrl (as-number (ast-attribute mes-ast 'control))))
              (if (and (boolean? ch) ch) #t (= ch (as-number (ast-attribute mes-ast 'channel))))
         )
         (let ((abs-time (ast-attribute mes-ast 'absTime #f))
               (delta-time (ast-attribute mes-ast 'deltaTime #f)))
           (cond (abs-time
                  (midi-null-event-abs-time (ast-attribute mes-ast 'absTime)))
                 (delta-time
                  (midi-null-event-delta-time (ast-attribute mes-ast 'deltaTime)))
                 (else (laml-error "eliminate-control-change-1: Not absTime and not deltaTime. Should not happen")))) 
         mes-ast)
     )
   message-list)))


;; Eliminate ChannelKeyPressure messages in a given channel.
;; Works in both deltaTime and absTime mode.
;; .form (eliminate-channel-key-pressure channel . messages)
;; .parameter channel A channel number in the interval 1..16. The value #t serves as a wilchard for any channel.
;; .parameter messages A list of midi messages.
(define eliminate-channel-key-pressure
 (xml-in-laml-positional-abstraction 1 0
   (lambda (channel contents attributes)
      (eliminate-channel-key-pressure-1 channel contents))))

(define (eliminate-channel-key-pressure-1 ch message-list)
 (eliminate-midi-null-events
  (map 
   (lambda (mes-ast) 
     (if (and (ast? mes-ast)
              (equal? (ast-element-name mes-ast) "ChannelKeyPressure")
              (if (and (boolean? ch) ch) #t (= ch (as-number (ast-attribute mes-ast 'channel))))
         )
         (let ((abs-time (ast-attribute mes-ast 'absTime #f))
               (delta-time (ast-attribute mes-ast 'deltaTime #f)))
           (cond (abs-time
                  (midi-null-event-abs-time (ast-attribute mes-ast 'absTime)))
                 (delta-time
                  (midi-null-event-delta-time (ast-attribute mes-ast 'deltaTime)))
                 (else (laml-error "eliminate-channel-key-pressure-1: Not absTime and not deltaTime. Should not happen")))) 
         mes-ast)
     )
   message-list)))

;; Eliminate ProgramChange message in a given channel.
;; Works in both deltaTime and absTime mode.
;; .form (eliminate-program-change channel . messages)
;; .parameter channel A channel number in the interval 1..16. The value #t serves as a wilchard for any channel.
;; .parameter messages A list of midi messages.
(define eliminate-program-change
 (xml-in-laml-positional-abstraction 1 0
   (lambda (channel contents attributes)
      (eliminate-program-change-1 channel contents))))

(define (eliminate-program-change-1 ch message-list)
 (eliminate-midi-null-events
  (map 
   (lambda (mes-ast) 
     (if (and (ast? mes-ast)
              (equal? (ast-element-name mes-ast) "ProgramChange")
              (if (and (boolean? ch) ch) #t (= ch (as-number (ast-attribute mes-ast 'channel))))
         )
         (let ((abs-time (ast-attribute mes-ast 'absTime #f))
               (delta-time (ast-attribute mes-ast 'deltaTime #f)))
           (cond (abs-time
                  (midi-null-event-abs-time (ast-attribute mes-ast 'absTime)))
                 (delta-time
                  (midi-null-event-delta-time (ast-attribute mes-ast 'deltaTime)))
                 (else (laml-error "eliminate-program-change-1: Not absTime and not deltaTime. Should not happen")))) 
         mes-ast)
     )
   message-list)))


;; Eliminate PitchBench message in a given channel.
;; Works in both deltaTime and absTime mode.
;; .form (eliminate-pitch-bend channel . messages)
;; .parameter channel A channel number in the interval 1..16. The value #t serves as a wilchard for any channel.
;; .parameter messages A list of midi messages.
(define eliminate-pitch-bend
 (xml-in-laml-positional-abstraction 1 0
   (lambda (channel contents attributes)
      (eliminate-pitch-bend-1 channel contents))))

(define (eliminate-pitch-bend-1 ch message-list)
 (eliminate-midi-null-events
  (map 
   (lambda (mes-ast) 
     (if (and (ast? mes-ast)
              (equal? (ast-element-name mes-ast) "PitchBendChange")
              (if (and (boolean? ch) ch) #t (= ch (as-number (ast-attribute mes-ast 'channel))))
         )
         (let ((abs-time (ast-attribute mes-ast 'absTime #f))
               (delta-time (ast-attribute mes-ast 'deltaTime #f)))
           (cond (abs-time
                  (midi-null-event-abs-time (ast-attribute mes-ast 'absTime)))
                 (delta-time
                  (midi-null-event-delta-time (ast-attribute mes-ast 'deltaTime)))
                 (else (laml-error "eliminate-pitch-bend-1: Not absTime and not deltaTime. Should not happen")))) 
         mes-ast)
     )
   message-list)))

;; Revoice all ProgramChange events in the messages list according to revoice-map.
;; In order to revoice, two appropropriate control change messages must occur before a program change message.
;; If this is not the case, revoicing to general midi (GM) takes place.  
;; .form (revoice revoice-map . messages)
;; .parameter revoice-map A list of revoice entries. A revoice entry is a list of length two of the form ((msb-from lsb-from pc-from info) (msb-to lsb-to pc-to info))\
;;                        where msb-from, lsb-from, pc-from, msb-to, lsb-to, and pc-to are integers. info is an informal optional string (the name of the voice).
;; .attribute revoice-file implied  The name of a text file, to which a template of a revoice map is written. \
;;                               The templates are generated from the program change information in the source file (not from revoice-map).\
;;                               The generated template is a useful (but incomplete) starting point of revoice-map.\
;;                               If the attribute is not supplied, no info file is generated.
;; .parameter messages A list of midi messages.
(define revoice
 (xml-in-laml-positional-abstraction 1 0 
   (lambda (revoice-map contents attributes)
       (let ((revoice-file (defaulted-get-prop 'revoice-file attributes #f))) 
         (revoice-1 revoice-map contents revoice-file)))))

(define (revoice-1 revoice-map contents revoice-file)
  (let ((contents-1 (msb-lsb-to-internal-pc-attributes contents '(#t . 0) '(#t . 0) '() )))

     (if revoice-file
         (let ((file-path (in-startup-directory revoice-file)))
           (if (file-exists? file-path) (delete-file file-path))
           (write-text-file
             (extract-revoice-file-info contents-1)
             file-path)))

     (revoice-2 revoice-map contents-1)))

(define (extract-revoice-file-info contents)
 (string-append "(" CR
   (list-to-string
    (filter (lambda (x) x)
            (map
             (lambda (ast)
               (if (ProgramChange? ast)
                   (if (and (ast-internal-attribute ast 'msb) (ast-internal-attribute ast 'lsb))
                       (let* ((msb (ast-internal-attribute ast 'msb))
                              (lsb (ast-internal-attribute ast 'lsb))
                              (pc (midi 'number ast)))
                         (single-msb-lsb-pc-info-string msb lsb pc))
                       #f)
                   #f))
             contents)) CR)
   CR ")"))

(define (single-msb-lsb-pc-info-string msb lsb pc)
  (string-append
    " (list " " (list " (as-string msb) " " (as-string lsb) " " (as-string pc) " " (string-it (as-string (find-voice-info-string msb lsb pc))) ")" CR
       "        (list  )  "
    ")"))

; Traverse messages in contents, and put internal attributes on ProgramChange messages that reflect the msb and lsb of preceding ControlChange messages.
; The meaning of previous-msb previous-lsb corresponds to the global variables previous-msb-control-channel-value and
; previous-lsb-control-channel-value in midi.scm
; Decorate program change messages in contents with internal lsb and msb attributes.
; Return a list of ProgramChange ASTs decorated with interal attributes for msb and lsb information (control change info).
(define (msb-lsb-to-internal-pc-attributes contents previous-msb previous-lsb out-contents)
  (cond ((null? contents) (reverse out-contents))
        ((not (ast? (car contents))) (msb-lsb-to-internal-pc-attributes (cdr contents) previous-msb previous-lsb out-contents))
        ((ControlChange? (car contents) 0)  ; LSB
            (msb-lsb-to-internal-pc-attributes (cdr contents)
                                               (cons (midi 'channel (car contents)) (midi 'value (car contents)))
                                               previous-lsb 
                                               out-contents))   ; remove from midi stream
        ((ControlChange? (car contents) 32)  ; MSB
            (msb-lsb-to-internal-pc-attributes (cdr contents)
                                               previous-msb
                                               (cons (midi 'channel (car contents)) (midi 'value (car contents)))
                                               out-contents))   ; remove from midi stream
        ((ProgramChange? (car contents))
            (let* ((element (car contents)) ; alias
                   (channel (midi 'channel element)))
              (if #t  ; (and (not (boolean? (first previous-msb))) (not (boolean? (first previous-lsb)))
                      ;     (= channel (first previous-msb)) (= channel (first previous-lsb)))
                  (let ((new-element
                          (make-ast (ast-element-name element) (ast-subtrees element) (ast-attributes element) (ast-kind element) (ast-language element) 
                                  (list 'msb (cdr previous-msb) 'lsb (cdr previous-lsb)))))
                     (msb-lsb-to-internal-pc-attributes (cdr contents) '(#t . 0) '(#t . 0) (cons new-element out-contents)))
                  (msb-lsb-to-internal-pc-attributes (cdr contents) previous-msb previous-lsb (cons element out-contents)))))
        (else (msb-lsb-to-internal-pc-attributes (cdr contents) previous-msb previous-lsb (cons (car contents) out-contents))))) 

; Carry out the revoicing on the a contents list in which program change messages are decorated with internal msb and lsb attributes.
(define (revoice-2 revoice-map contents)
  (map
    (lambda (ast)
       (if (ProgramChange? ast)
           (if (and (ast-internal-attribute ast 'msb) (ast-internal-attribute ast 'lsb))
               (let* ((msb (ast-internal-attribute ast 'msb))
                      (lsb (ast-internal-attribute ast 'lsb))
                      (pc (midi 'number ast))
                      (ch (midi 'channel ast))
                      (res (lookup-revoice-map revoice-map msb lsb pc)))
                 (if res
                     (apply voice (cons ch res))
                     (voice ch 0 0 pc)   ; Revocing to GM
                 ))
               (let ((pc (midi 'number ast))
                     (ch (midi 'channel ast)))
                (voice ch 0 0 pc)))
           ast)
    )
    contents))

; A revoice map is a list mapping entries.
; A mapping entry is of the form  ((msb-from lsb-from pc-from) (msb-to lsb-to pc-to)) where all constituents are integers
; Return the list (msb-to lsb-to pc-to) if msb, lsb and pc match (msb-from lsb-from pc-from). If not, return #f.
(define (lookup-revoice-map revoice-map msb lsb pc)
  (let ((res (find-in-list 
               (lambda (e) 
                 (let ((from (car e)))
                    (and (= msb (first from)) (= lsb (second from)) (= pc (third from)))))
               revoice-map)))
    (if res
        (cadr res)
        #f)))


;; Eliminate ProgramChange messages and ControlChange messages that select voice banks (both the MSB and LSB variants).
;; The elimination takes place by replacing the ProgramChange message with a Meta message which contain channel, MSB, LSB and program-number.
;; In that way, it will be possible to provide for the reverse operation (not yet programmed).
;; If the attribute info-file is provided, a text file with voice information is generated. The information is targeted at the Motif XS.
;; Use the text file for convenient input of the voices in the Motif XS mixer.
;; .form (eliminate-voice-messages . messages)
;; .attribute info-file implied  The name of a text file, to which a voice information is written. If not provided, no voice information is generated.
;; .parameter messages A list of midi messages.
;; .internal-references "more radical function" "clean-for-motif"
(define eliminate-voice-messages
 (xml-in-laml-positional-abstraction 0 0 
   (lambda (contents attributes)
     (let ((info-file (defaulted-get-prop 'info-file attributes #f))) 
       (eliminate-voice-messages-1 info-file contents))
 )))

(define (eliminate-voice-messages-1 info-file contents)
  (let ((contents-1 (msb-lsb-to-internal-pc-attributes contents '(#t . 0) '(#t . 0) '() )))

     (if info-file
         (let ((file-path (in-startup-directory info-file)))
           (if (file-exists? file-path) (delete-file file-path))
           (write-text-file
             (make-voices-info-string contents-1)
             file-path)))

     (map (lambda (ast) 
            (cond ((ProgramChange? ast)
                      (cond ((midi 'absTime ast)
                              (midi-comment-abs-time (midi 'absTime ast) 
                                (string-append "Was ProgramChange" " "
                                               (ast-attribute ast 'channel) " " (as-string (ast-internal-attribute ast 'msb "?")) " "
                                               (as-string (ast-internal-attribute ast 'lsb "?")) " " (ast-attribute ast 'number))))
                            ((midi 'deltaTime ast)
                              (midi-comment-delta-time (midi 'deltaTime ast) 
                                (string-append "Was ProgramChange" " "
                                               (ast-attribute ast 'channel) " " (as-string (ast-internal-attribute ast 'msb "?")) " "
                                               (as-string (ast-internal-attribute ast 'lsb "?")) " " (ast-attribute ast 'number))))
                            (else (laml-error "eliminate-voice-messages-1 (A): Should not happen"))))

                  ((SysEx? ast "05 7E 7F 09 01 F7")
                      (cond ((midi 'absTime ast)
                              (midi-comment-abs-time (midi 'absTime ast) 
                                (string-append "Was SysEx" " "
                                               (ast-text ast))))
                            ((midi 'deltaTime ast)
                              (midi-comment-delta-time (midi 'deltaTime ast) 
                                (string-append "Was SysEx" " "
                                               (ast-text ast))))
                            (else (laml-error "eliminate-voice-messages-1 (B): Should not happen"))))
                           
                  (else ast)))
          contents-1)
  )
)

(define (make-voices-info-string messages)
  (list-to-string
   (map (lambda (lst) (apply make-single-voice-info lst))
        (sort-list
         (filter pair?
                 (map
                  (lambda (ast)
                    (if (ProgramChange? ast)
                        (if (and (ast-internal-attribute ast 'msb) (ast-internal-attribute ast 'lsb))
                            (let* ((ch (midi 'channel ast))
                                   (msb (ast-internal-attribute ast 'msb))
                                   (lsb (ast-internal-attribute ast 'lsb))
                                   (pc (midi 'number ast)))
                              (list ch msb lsb pc))
                            #f)
                        #f))
                  messages))
         (lambda (lst1 lst2) (<= (first lst1) (first lst2))))) ; sort according to channel
   CR))


(define (make-single-voice-info ch msb lsb pc)
 (let ((voice-name (find-voice-info-string msb lsb pc)))
  (string-append
    "Channel: " (as-string ch) CR
    (if voice-name voice-name "???") CR
    "MSB LSB PC: " (as-string msb) " " (as-string lsb) " " (as-string pc) " " CR
    (xs-instrument-how-to msb lsb pc) CR)))

(define (xs-instrument-how-to msb lsb pc)
  (let ((bank (cond ((and (= msb 63) (= lsb 0))  "PRE1")      ; 128
                    ((and (= msb 63) (= lsb 1))  "PRE2")                    
                    ((and (= msb 63) (= lsb 2))  "PRE3")                    
                    ((and (= msb 63) (= lsb 3))  "PRE4")                    
                    ((and (= msb 63) (= lsb 4))  "PRE5")                    
                    ((and (= msb 63) (= lsb 5))  "PRE6")                    
                    ((and (= msb 63) (= lsb 6))  "PRE7")                    
                    ((and (= msb 63) (= lsb 7))  "PRE8")                    
                    ((and (= msb 63) (= lsb 8))  "USR1")                    
                    ((and (= msb 63) (= lsb 9))  "USR2")                    
                    ((and (= msb 63) (= lsb 10)) "USR3")
                    ((and (= msb 0) (= lsb 0))   "GM")        ; 128
                    ((and (= msb 63) (= lsb 32)) "PRE DRUM")  ; 64
                    ((and (= msb 63) (= lsb 40)) "USR DRUM")  ; 32
                    ((and (= msb 127) (= lsb 0)) "GM Drum")   ; 1
                    (else "??")))
        (number (+ (remainder pc 16) 1))
        (letter-number (quotient pc 16)))
   (string-append bank " " (as-string (as-char (+ 65 letter-number))) (as-string number))))

;; Eliminate voice message (by use of the function eliminate-voice-messages) and remove all ControlChange messages (apart from Pedal Sustain).
;; Also remove SysEx messages which pertain to Yamaha Tyros (43**4C).
;; .form (clean-for-motif . messages)
;; .attribute info-file implied  The name of a text file, to which a voice information is written. If not provided, no voice information is generated.
;; .internal-references "called function" "eliminate-voice-messages"
(define clean-for-motif
 (xml-in-laml-positional-abstraction 0 0 
   (lambda (contents attributes)
     (let ((info-file (defaulted-get-prop 'info-file attributes #f)))
      (clean-for-motif-1 info-file contents)))))

(define (clean-for-motif-1 info-file contents)
  (let ((contents-1 (eliminate-voice-messages-1 info-file contents)))
   (filter 
    (lambda (x)
       (cond ((not (ast? x)) #f)
             ((and (ControlChange? x) (not (member (midi 'control x) (list 64 84)))) #f)
             ((and (SysEx? x) (equal? (substring (ast-text x) 3 5) "43") (equal? (substring (ast-text x) 9 11) "4C")) #f)
             (else #t)))
    contents-1)))

;; Rinse the prefix of messages for noise messages (Meta tempo, ProgramControl, certain ControlChanges, and PitchBendChange)
;; If include-voice-messages? is true, also include voice related ProgramChange and ControlChange messages in the prefix.
;; The first NoteOn message is set to start at time time-start (per time displacement done by this function).
;; Normalize the time attribute of messages before the first NoteOn message, and time displace the rest.
;; Rinse the NoteOn messages for tempo messages, ControlChange type 0 and 32, and ProgramChange messages.
;; Setting of the tempo is supposed to be handled in the context of this function. 
;; Supports both absTime and deltaTime mode.
;; .form (clean-for-sectional-playing time-mode time-start include-voice-messages? . messages)
;; .parameter time-mode Either delta-time or abs-time (symbol or string).
;; .parameter time-start The start time of the first NoteOn message. A number of ticks (non-negative integer).
;; .parameter include-voice-messages? A boolean value that controls if voice related messages are included.
;; .misc This function is used automatically when a delimited section is played from the Emacs MIDI LAML environment, hereby ensuring\\
;;       that playing starts immediately, and that it is not disturbed by inappropriate control messages.
(define clean-for-sectional-playing 
 (xml-in-laml-positional-abstraction 3 0 
   (lambda (time-mode time-start include-voice-messages? contents attributes)
     (clean-for-sectional-playing-1 time-mode time-start include-voice-messages? contents))))

(define (clean-for-sectional-playing-1 time-mode-0 time-start include-voice-messages? messages)
 (let ((time-mode (as-symbol time-mode-0))) 
  (let* ((suffix-starting-with-note-on
          (find-tail-in-list (lambda (mes-ast)
                               (and (ast? mes-ast) (NoteOn? mes-ast)))
                             messages))
         (prefix-before-starting-note-on
          (find-but-tail-in-list (lambda (mes-ast)
                                   (and (ast? mes-ast) (NoteOn? mes-ast)))
                                 messages))
         (cleaned-prefix ; does not work correctly in deltaTime mode! 
          (if include-voice-messages?
              (let* ((voice-messages (filter-messages-1 (lambda (mes-ast)
                                                          (or (ProgramChange? mes-ast) (ControlChange? mes-ast 0) (ControlChange? mes-ast 32)))
                                                        prefix-before-starting-note-on))
                     )
                (map (lambda (voice-mes-ast)
                         (if (eq? time-mode 'abs-time)
                             (copy-ast-mutate-attributes voice-mes-ast 'absTime "0")  ; enforce absTime "0"
                             (copy-ast-mutate-attributes voice-mes-ast 'deltaTime "0")))
                       voice-messages))
              '()))
        )
    (if (null? suffix-starting-with-note-on)
        cleaned-prefix
        (let ((first-time (time-of-message (first suffix-starting-with-note-on))))
          (append
           cleaned-prefix
           (time-displace-1 (- (- first-time time-start)) 
                            (cond ((eq? time-mode 'abs-time)
                                     (filter-messages 
                                      (lambda (mes-ast)
                                        (not (or (Meta? mes-ast 81) (ProgramChange? mes-ast) (ControlChange? mes-ast 0) (ControlChange? mes-ast 32) (PitchBendChange? mes-ast))))
                                      suffix-starting-with-note-on))
                                  ((eq? time-mode 'delta-time)
                                     (transform-messages 
                                      (lambda (mes-ast)
                                        (or (Meta? mes-ast 81) (ProgramChange? mes-ast) (ControlChange? mes-ast 0) (ControlChange? mes-ast 32) (PitchBendChange? mes-ast)))
                                      (lambda (mes-ast) (midi-null-event-delta-time (as-number (ast-attribute mes-ast 'deltaTime))))
                                      suffix-starting-with-note-on))
                                  (else (laml-error "clean-for-sectional-playing-1: Should not happen."))))))))))

;; Scale the tempo of the messages enclosed by this function.
;; This is done by inserting a tempo change list in front of messages, as produced by make-tempo-change-list.
;; The inserted tempo change list is adapted to the duration of messages.
;; If the attribute last-tempo is not provided, the messages after the enclosed message list continues in the end tempo obtained by the scaling.
;; .form (tempo-scale base-tempo scaling-fn . messages)
;; .parameter base-tempo The base tempo, which is scaled throughout the of generated Meta tempo events. Normally the tempo of the song. A real number.
;; .parameter scale-fn The tempo scaling function: [0,1] -> Positive Real Number.
;; .attribute n implied The number of Meta tempo messages to produce. At least one. Defaults to the duration of messages divided by 960 (somewhat arbitrary).
;; .attribute last-tempo implied If provided, the last Meta tempo event is forced to be last-tempo. A real number. 
;; .internal-references "uses" "make-tempo-change-list"
;; .mode absTime 
(define tempo-scale
 (xml-in-laml-positional-abstraction 2 0 
   (lambda (base-tempo scale-fn contents attributes)
     (let* ((duration (total-length-of-message-list contents))
            (n (defaulted-get-prop 'n attributes (quotient duration 960)))
            (last-tempo (defaulted-get-prop 'last-tempo attributes #f)))
      (tempo-scale-1 n base-tempo scale-fn last-tempo contents)))))

; The signature of tempo-scale-1 has been changed a little on December 16, 2010 (eliminated duration parameter - recalculated in tempo-scale-1):
(define (tempo-scale-1 n base-tempo scale-fn last-tempo contents)
  (assert-abs-time contents)
  (let ((duration (total-length-of-message-list contents))  ; recalculated of convenience, need not be passed as parameter
       )
    (delta-abs-merge-two-lists 
     (if last-tempo
         (make-tempo-change-list n duration base-tempo scale-fn (as-number last-tempo))
         (make-tempo-change-list n duration base-tempo scale-fn))
     contents)))

;; Insert a volume change list in front of messages, as produced by make-volume-change-list.
;; This controls the volumne of the messages of channel ch enclosed by this function 
;; The volumne change list is adapted to the duration of messages.
;; If the attribute end-volume is not provide, the messages after the enclosed message list continues at the volumne obtained by the scaling.
;; .form (volume-scale ch start-volume scaling-fn . messages)
;; .parameter start-volumne The start-volumne which is scaled throughout the of generated ControlChange events. 
;; .parameter scale-fn The volumne scaling function: [0,1] -> Positive Real Number.
;; .attribute n implied The number of ControlChange messages to produce. At least one. Defaults to the duration of messages divided by 960 (somewhat arbitrary).
;; .attribute end-volumne implied If provided, the last ControlChange event is forced to prescribe end-volume.
;; .internal-references "uses" "make-volume-change-list"
;; .mode absTime 
(define volume-scale
 (xml-in-laml-positional-abstraction 3 0 
   (lambda (ch start-volume scale-fn contents attributes)
     (let* ((duration (total-length-of-message-list contents))
            (n (defaulted-get-prop 'n attributes (quotient duration 960)))
            (end-volume (defaulted-get-prop 'end-volume attributes #f)))
      (volume-scale-1 ch n duration start-volume scale-fn end-volume contents)))))

; (define (volume-scale-1 ch n duration start-volume scale-fn end-volume contents)
;   (list 
;     (if end-volume
;         (make-volume-change-list ch n duration start-volume scale-fn (as-number end-volume))
;         (make-volume-change-list ch n duration start-volume scale-fn))
;     contents)
; )

(define (volume-scale-1 ch n duration start-volume scale-fn end-volume contents)
  (get-rid-of-delta-times
   (delta-abs-merge-two-lists 
    (if end-volume
        (make-volume-change-list ch n duration start-volume scale-fn (as-number end-volume))
        (make-volume-change-list ch n duration start-volume scale-fn))
    (map ast-copy contents)   ; conservative: copy perhaps not necessary?
   )
   0)
)

;; Apply volume-scale multiple times as specified by volume-scale-list.
;; An entry in volume-scale-list is of the form (channel start-vol scaling-fn [end-vol]).
;; .form (volume-scale-multi-channel volume-scale-list . messages)
;; .internal-references "applied function" "volume-scale"
(define volume-scale-multi-channel
 (xml-in-laml-positional-abstraction 1 0 
   (lambda (volume-scale-list contents attributes)
     (let* ((duration (total-length-of-message-list contents))
            (n (quotient duration 960)))
      (volume-scale-multi-channel-1 n duration volume-scale-list contents)))))

(define (volume-scale-multi-channel-1 n duration volume-scale-list contents)
  (if (null? volume-scale-list)
      contents
      (let* ((vol-scale-entry (car volume-scale-list))
             (ch (first vol-scale-entry))
             (start-vol (second vol-scale-entry))
             (scaling-fn (third vol-scale-entry))
             (end-vol (if (>= (length vol-scale-entry) 4) (fourth vol-scale-entry) #f)))
        (volume-scale-multi-channel-1 n duration (cdr volume-scale-list) (volume-scale-1 ch n duration start-vol scaling-fn end-vol contents)))))

;; Insert lyric elements from lyric-syllable-list in the empty Meta lyric in messages.
;; .form (insert-lyric lyric-syllable-list . messages)
;; .parameter lyric-syllable-list A list of syllables (a list of strings/symbols).
;; .parameter messages A list of midi messages.
;; .misc First insert empty lyric meta mesages before all appropriate NoteOn midi messages. Then apply this function.
(define insert-lyric
 (xml-in-laml-positional-abstraction 1 0 
   (lambda (lyric-syllable-list contents attributes)
    (insert-lyric-1 lyric-syllable-list contents))))


(define (insert-lyric-1 lyric-syllable-list messages)
  (insert-lyric-2 lyric-syllable-list messages '())
)

(define (insert-lyric-2 lyric-syllable-list messages res-messages)
  (cond ((null? messages) (reverse res-messages))
        (else (let ((mes (first messages)))
                (cond ((not (ast? mes))
                         (insert-lyric-2 lyric-syllable-list (cdr messages) res-messages))

                      ((and (Meta? mes 5)  ; empty lyric meta message
                            (empty-string? (ast-text mes))
                            (not (null? lyric-syllable-list)))
                         (insert-lyric-2 (cdr lyric-syllable-list) (cdr messages)
                                         (cons (Meta 'deltaTime (midi 'deltaTime mes) 'type 5
                                                     (as-string (car lyric-syllable-list))) res-messages)))

                      (else 
                         (insert-lyric-2 lyric-syllable-list (cdr messages) (cons mes res-messages))))))))


;;; .section-id smf-functions
;;; Standard Midi File functions.
;;; Functions which should be applied on a StandardMidiFile form.

;; Convert smf-ast (a StandardMidiFile) to target-ppqn. Does not affect the format of the midi laml file.
;; Works both for format 0 and format 1 midi laml files. Also works for both absTime mode and deltaTime mode.
;; .parameter target-ppqn The desired target ppqn (an integer such as 480, 960, 1920).
;; .parameter smf-ast An AST with root element name StandardMidiFile.
;; .parameter context-mode Either top-level or nested (a symbol). Only in top-level context-mode action routines are executed.
;; .pre-condition smf-ast must have element name StandardMidiFile.
(define (smf-to-ppqn target-ppqn context-mode smf-ast)
  (let* ((header (ast-subtree smf-ast "MidiHeader"))
         (source-ppqn (as-number (ast-attribute header 'pulsesPerQuarterNote)))
         (format      (as-number (ast-attribute header 'format)))
         (mode        (ast-attribute header 'mode))
         (ct          (ast-attribute header 'counterTransposition))
         (no-of-tracks (as-number (ast-attribute header 'numberOfTracks)))
         (track-list (filter (lambda (x) (and (ast? x) (equal? (ast-element-name x) "MidiTrack"))) (ast-subtrees smf-ast)))
        )
    (StandardMidiFile (if (eq? context-mode 'nested) (list 'internal:run-action-procedure "false") (list))
      (MidiHeader 'format format 'numberOfTracks no-of-tracks 'pulsesPerQuarterNote target-ppqn 'mode mode 'counterTransposition ct)
      (map 
       (lambda (track-ast)
         (MidiTrack
           (time-stretch-1 (/ target-ppqn source-ppqn) (ast-subtrees track-ast)))
       ) 
       track-list))))

;; Convert the StandardMidiFile to format 0 by merging all tracks.
;; If smf-ast is a format 1 midi laml file, merge all tracks.
;; If smf-ast is already a format 0 midi laml file, this funciton is the identity function.
;; Works in both absTime mode and deltaTime mode.
;; This function takes care that there is only one end-of-track Meta event, located (* ppqn-distance-to-end-of-track ppqn) 
;; time units from last message in the resulting track.
;; .form (smf-to-format-0 context-mode smf-ast [ppqn-distance-to-end-of-track])
;; .parameter smf-ast A StandardMidiFile ast.
;; .parameter context-mode Either top-level or nested (a symbol). Only in top-level context-mode action routines are executed.
;; .parameter ppqn-distance-to-end-of-track The number of ppqn intervals between the last event and the end-of-track Meta event. Defaults to 8.
(define (smf-to-format-0 context-mode smf-ast . optional-parameters)
  (let* ((ppqn-distance-to-end-of-track (optional-parameter 1 optional-parameters 8))
         (header (ast-subtree smf-ast "MidiHeader"))
         (ppqn (as-number (ast-attribute header 'pulsesPerQuarterNote)))
         (format      (as-number (ast-attribute header 'format)))
         (mode        (ast-attribute header 'mode))
         (ct          (ast-attribute header 'counterTransposition))
         (no-of-tracks (as-number (ast-attribute header 'numberOfTracks))) ; not used
         (track-list (filter (lambda (x) (and (ast? x) (equal? (ast-element-name x) "MidiTrack"))) (ast-subtrees smf-ast)))
         (no-end-of-track (lambda (message-list) (filter (lambda (x) (not (and (ast? x) (Meta? x 47)))) message-list)))
        )

   (if (= format 0)
       smf-ast  ; the identity function
       (cond ((equal? mode "absTime")
               (StandardMidiFile (if (eq? context-mode 'nested) (list 'internal:run-action-procedure "false") (list))
                (MidiHeader 'format 0 'numberOfTracks 1 'pulsesPerQuarterNote ppqn 'mode "absTime" 'counterTransposition ct)
                (MidiTrack
                 (accumulate-right abs-merge-two-lists '() (map (compose no-end-of-track ast-subtrees) track-list))
                 (Meta 'deltaTime (* ppqn-distance-to-end-of-track ppqn) 'type "47" ""))))
             ((equal? mode "deltaTime")
               (StandardMidiFile (if (eq? context-mode 'nested) (list 'internal:run-action-procedure "false") (list))
                (MidiHeader 'format 0 'numberOfTracks 1 'pulsesPerQuarterNote ppqn 'mode "deltaTime" 'counterTransposition ct)
                (MidiTrack
                 (accumulate-right delta-merge-two-lists '() (map (compose no-end-of-track ast-subtrees) track-list))
                 (Meta 'deltaTime (* ppqn-distance-to-end-of-track ppqn) 'type "47" ""))))))))

;; Convert a 'brainstorm recording' to a Motif XS format 0 midi file of ppqn 480.
;; Just a convenient composition of smf-to-format-0 and smf-to-ppqn with a top-level application (enabeling action procedures).
;; .internal-references "Convenience variant" "rtm"
(define (recording-to-motif smf-ast)
  (smf-to-format-0 'top-level (smf-to-ppqn 480 'nested smf-ast)))

(define (recording-to-motif-nested smf-ast)
  (smf-to-format-0 'nested (smf-to-ppqn 480 'nested smf-ast)))

; An function used internally by the step-recording facility.
; It transforms the smf ast to mofif format (a single track, 480 ppqn).
; Quantification q is either 32, 16, 4, 2, or 1 corresponding to 1/q't note.
; Assume deltaTiming.
; NOT USED
(define (as-step-recording-on-motif-nested-fixed-timing smf-ast quantification-str)
 (let ((quantification (as-number quantification-str)))
  (let ((ticks (cond ((= quantification 1) (* 4 480))
                     ((= quantification 2) (* 2 480))
                     ((= quantification 4) 480)
                     ((= quantification 8) 240)
                     ((= quantification 16) 120)
                     ((= quantification 32) 60)
                     (else (laml-error "as-step-recording-on-motif-nested: Unknown quantification:" quantification)))))
   (apply-to-tracks-nested 
      (lambda mes-lst
         (map (lambda (ast)
                (if (NoteOn? ast)
                    (copy-ast-mutate-attributes ast 'deltaTime ticks 'duration ticks)
                    ast)
              )
              mes-lst))  ; earlier: (filter-messages NoteOn? mes-lst)
      ()
      (recording-to-motif-nested smf-ast)))))

; An function used internally by the step-recording facility.
; Assume step recording takes place in channel 1.
; The function transforms the smf ast to mofif format (a single track, 480 ppqn) and it quantizes the notes to 1/16th, 1/8th, 1/4th, 1/2th, or 1/1th notes (durations 120, 240, 480, 960, 1920 resp)
; after having applied legato
; Pauses are restricted to 1/4, 1/2 or 1/1.
; Recording happens at 100 BPM = 100 quarter notes / 60 sek = 100*480 ticks / 60 s = 800 ticks pr. sec with PPQN = 480.
; Assume deltaTiming in this function.
(define (as-step-recording-on-motif-nested-variable-timing smf-ast)
  (apply-to-tracks-nested 
   (lambda mes-lst
     (quantize-timing-in-a-step-recording
      (map (lambda (ast)
             (if (NoteOn? ast)
                 (quantize-duration-of-note-step-recording ast)
                 ast)
             )
           (legato-in-channel-1 1 10 #t #f 0 mes-lst))))  ; (legato-in-channel-1 1 10 #t #f 0 mes-lst)
   ()
   (recording-to-motif-nested smf-ast)))  

; Force duration to either 120, 240, 480, 960 or 1920 ticks.
; The forced duration comes from the recorded duration.
(define (quantize-duration-of-note-step-recording note-ast)
  (let* ((recorded-duration (midi 'duration note-ast))
         (forced-duration (cond ((<= recorded-duration 300) 120)     ; 1/16th at 480 PPQN
                                ((<= recorded-duration 600) 240)     ; 1/8th at 480 PPQN
                                ((<= recorded-duration 1200) 480)    ; 1/4th at 480 PPQN
                                ((<= recorded-duration 2400) 960)    ; 1/2th at 480 PPQN
                                (else                        1920))) ; 1/1th at 480 PPQN
        )
    (copy-ast-mutate-attributes note-ast 'duration forced-duration)))

; Set the deltaTime based on the duration of the previous note.
; No pauses are introduced.
(define (quantize-timing-in-a-step-recording event-ast-lst)
  (quantize-timing-step-recording-1 event-ast-lst #f '())
)

(define (quantize-timing-step-recording-1 event-ast-lst prev-duration res-lst)
  (if (null? event-ast-lst)
      (reverse res-lst)
      (let ((mes (car event-ast-lst)))
        (cond ((and (NoteOn? mes) (not prev-duration))
                  (quantize-timing-step-recording-1 (cdr event-ast-lst) (midi 'duration mes) (cons (copy-ast-mutate-attributes mes 'deltaTime 480) res-lst)))
              ((and (NoteOn? mes) prev-duration)
                  (quantize-timing-step-recording-1 (cdr event-ast-lst) (midi 'duration mes) (cons (copy-ast-mutate-attributes mes 'deltaTime prev-duration) res-lst)))
              ((ast? mes)
                  (quantize-timing-step-recording-1 (cdr event-ast-lst) prev-duration (cons mes res-lst)))
              (else 
                  (quantize-timing-step-recording-1 (cdr event-ast-lst) prev-duration res-lst))))))


;; A convenience variant of the function recording-to-motif which, in addition, eliminates all ChannelKeyPressure messages.
;; Most important, however, this function also inserts markers by means of the function marker-silence (with a constant 2000 silence-ticks). 
;; .internal-references "More basic variant" "recording-to-motif"
(define (rtm smf-ast)
 (apply-to-tracks-top-level eliminate-channel-key-pressure (list #t)
  (apply-to-tracks-nested marker-silence (list 2000)
    (recording-to-motif-nested smf-ast))))

;; Convert a 'brainstorm recording' to a Tyros format 0 midi file of ppqn 1920.
;; Just a convenient composition of smf-to-format-0 and smf-to-ppqn.
(define (recording-to-tyros smf-ast)
  (smf-to-format-0 'top-level (smf-to-ppqn 1920 'nested smf-ast)))

;; Apply f on the messages of all tracks in ast (a StandardMidiFile or MidiTrack AST).
;; In case the ast is a StandardMidiFile, it is considered to be nested into additional transformations, thus suppressing the execution of action routines.
;; Reconstructs the context around the track and return an AST corresponding to the third parameter.
;; .parameter ast A StandardMidiFile ast (format 0 or 1), or a MidiTrack ast.
;; .parameter f A function of which the last parameter is a rest parameter of messages\
;;              (a function generated by xml-in-laml-positional-abstraction or a similar function).
;; .parameter f-parameter-list Parameters prior to the messages. Passed to f just before the messages.
(define (apply-to-tracks-nested f f-parameter-list ast)
  (apply-to-tracks f f-parameter-list ast 'nested)
)

;; Apply f on the messages of all tracks in ast (a StandardMidiFile or MidiTrack AST).
;; In case the ast is a StandardMidiFile, it is considered to be at top-level, thus enabeling the execution of action routines.
;; Reconstructs the context around the track and return an AST corresponding to the third parameter.
;; .parameter ast A StandardMidiFile ast (format 0 or 1), or a MidiTrack ast.
;; .parameter f A function of which the last parameter is a rest parameter of messages\
;;              (a function generated by xml-in-laml-positional-abstraction or a similar function).
;; .parameter f-parameter-list Parameters prior to the messages. Passed to f just before the messages.
(define (apply-to-tracks-top-level f f-parameter-list ast)
  (apply-to-tracks f f-parameter-list ast 'top-level)
)

; In this function, mode is either top-level or nested (a symbol).
(define (apply-to-tracks f f-parameter-list ast context-mode)
  (cond 
         ; ast is format 0 StandardMidiFile
         ((and (equal? "StandardMidiFile" (ast-element-name ast)) (equal? "0" (ast-attribute (ast-subtree ast "MidiHeader") 'format)))
            (let* ((track-ast (ast-subtree ast "MidiTrack" 1))
                   (header-ast (ast-subtree ast "MidiHeader"))
                   (track-messages (ast-subtrees track-ast)))
              (StandardMidiFile (if (eq? context-mode 'nested) (list 'internal:run-action-procedure "false") (list)) 
               header-ast
               (MidiTrack
                (apply f (append f-parameter-list track-messages))))))

         ; ast is format 1 StandardMidiFile
         ((and (equal? "StandardMidiFile" (ast-element-name ast)) (equal? "1" (ast-attribute (ast-subtree ast "MidiHeader") 'format)))
            (let* ((header-ast (ast-subtree ast "MidiHeader"))
                   (track-ast-list (find-asts ast "MidiTrack"))
                  )
              (StandardMidiFile (if (eq? context-mode 'nested) (list 'internal:run-action-procedure "false") (list))
               header-ast
               (map (lambda (track-ast)
                      (MidiTrack
                        (apply f (append f-parameter-list (ast-subtrees track-ast)))))
                    track-ast-list))))

         ; ast is MidiTrack
         ((equal? "MidiTrack" (ast-element-name ast))
            (let ((track-messages (ast-subtrees ast)))
               (MidiTrack
                (apply f (append f-parameter-list track-messages)))))

 
         (else (laml-error "Must be called on a StandardMidiFile or MidiTrack ast"))))
   

;;; .section-id scaling-function-generation
;;; Generation of Scaling Functions.
;;; Scaling functions are used for smooth scaling of attributes such as deltaTime, duration, and velocity.
;;; A scaling function has the simple signature [0,1] -> Real.
;;; The functions in this section generate such scaling functions.
;;; The next section contains concrete scaling functions.
;;; The most useful - and the most versatile - generator is make-scale-function-by-xy-points.

;; Generates a positive, continuous scaling function which is constant one 'in the middle' and a second degree polynomial 'in the ends'.
;; Given 0 <= d < 0.5: Generate and return a function f for which (f d) = 1 and (f (- 1 d)) = 1. 
;; For d < x < (- 1 d): f(x) = 1.
;; For 0 < x < d: f(x) is polynomial or constant (depending on shape-start)
;; For (- 1 d) < x < 1: f(x) is polynomial or constant (depending on shape-end)
;; shape-start and shape-end is either the symbol up, flat or down.
;; .parameter shape-start One of the symbols up, flat, or down.
;; .parameter shape-end One of the symbols up, flat, or down.
;; .parameter c The factor of the second degree term of the polynomial.
;; .parameter d An x-value between 0 and 0.5. The generated function is constant 1 in the interval [d, 1-d].
;; .returns A positive, real-valued function (lambda (x) ...) where x is in the interval [0,1].
(define (make-scale-fn-pol-one-pol shape-start shape-end c d)
  (letrec ((f-up (lambda (x) (+ 1 (* c (- x d) (- x (- 1 d))))))
           (f-down (lambda (x) (max (- 1 (* c (- x d) (- x (- 1 d)))) 0)))
           (f-flat (lambda (x) 1))
          )
     (lambda (x)
        (cond ((< x 0) (error "the input is not supposed to be negative"))
              ((< x d) ((cond ((eq? shape-start 'up) f-up)
                              ((eq? shape-start 'flat) f-flat)
                              ((eq? shape-start 'down) f-down)
                              (else (laml-error "shape-start must be one of the symbols up, flat, or down"))) x))
              ((< x (- 1 d)) 1)
              ((<= x 1) ((cond ((eq? shape-end 'up) f-up)
                              ((eq? shape-end 'flat) f-flat)
                              ((eq? shape-end 'down) f-down)
                              (else (laml-error "shape-end must be one of the symbols up, flat, or down"))) x))
              (else (error "the input is not supposed to larger than one"))))))

;; A generalization of f-identity-symmetric which uses separate parameters of the second degree polynomials
;; at the start of the interval and at the end of the interval.
;; .parameter shape-start One of the symbols up, flat, or down.
;; .parameter shape-end One of the symbols up, flat, or down.
;; .parameter cs The factor of the second degree term of the polynomial used in the interval [0,ds].
;; .parameter ce The factor of the second degree term of the polynomial used in the interval [1-de, 1].
;; .parameter ds An x-value between 0 and 1. The generated function is constant 1 in the interval [ds, 1-de].
;; .parameter de An x-value between 0 and 1. The generated function is constant 1 in the interval [ds, 1-de].
;; .returns A positive, real-valued function (lambda (x) ...) where x is in the interval [0,1].
(define (make-scale-fn-pol-one-pol-general shape-start shape-end cs ds ce de)
  (letrec ((f-up-s   (lambda (x) (+ 1 (* cs (- x ds) (- x (- 1 ds))))))
           (f-down-s (lambda (x) (max (- 1 (* cs (- x ds) (- x (- 1 ds)))) 0)))
           (f-up-e   (lambda (x) (+ 1 (* ce (- x de) (- x (- 1 de))))))
           (f-down-e (lambda (x) (max (- 1 (* ce (- x de) (- x (- 1 de)))) 0)))
           (f-flat (lambda (x) 1))
          )
     (lambda (x)
        (cond ((< x 0) (error "the input is not supposed to be negative"))
              ((< x ds) ((cond ((eq? shape-start 'up) f-up-s)
                               ((eq? shape-start 'flat) f-flat)
                               ((eq? shape-start 'down) f-down-s)
                               (else (laml-error "shape-start must be one of the symbols up, flat, or down"))) x))
              ((< x (- 1 de)) 1)
              ((<= x 1) ((cond ((eq? shape-end 'up) f-up-e)
                               ((eq? shape-end 'flat) f-flat)
                               ((eq? shape-end 'down) f-down-e)
                               (else (laml-error "shape-end must be one of the symbols up, flat, or down"))) x))
              (else (error "the input is not supposed to larger than one"))))))


;; Return a scaling function composed by linear functions on subintervals of [0,1].
;; The functions from-percent-points and from-permille-points are helpful for creation of the xy-list parameter.
;; .parameter xy-list a list of (x,y) cons pairs which must contain at least (0, y0) and (1, y1) for some y0 and y1.
;; .returns A real-value function (lambda (x) ...) where x is in the interval [0,1]. Not necessarily positive valued.
;; .internal-references "Helper functions" "from-percent-points" "from-permille-points"
(define (make-scale-function-by-xy-points xy-list)
  (letrec ((x-of car)
           (y-of cdr)
          )
   (let ((sorted-xy-list (sort-list
                            xy-list
                            (lambda (p1 p2) (<= (x-of p1) (x-of p2))))))
    (lambda (x)   ; x in [0,1]
      (let* ((first-pair-0 (find-in-list (lambda (pair) (> (x-of pair) x)) sorted-xy-list))
             (first-pair (if (and (boolean? first-pair-0) (not first-pair-0))
                             (last sorted-xy-list)
                             first-pair-0))
             (second-pair (element-before first-pair sorted-xy-list id-1 equal?))
             (x1 (x-of first-pair))
             (y1 (y-of first-pair))
             (x2 (x-of second-pair))
             (y2 (y-of second-pair)))
       (+ y1 (* (/ (- y2 y1) (- x2 x1)) (- x x1))))))))


;; Multiply a scaling function sc my a factor, and return the new, scaled scaling function.
;; .parameter sf A real-value function.
;; .parameter factor A real-valued number.
;; .returns A modified (scaled) real-valued scaling function.
(define (multiply-scaling-function factor sf) 
  (lambda (x)
     (* factor (sf x))))

;; Add two scaling functions with each other
(define (add-two-scaling-functions sf1 sf2) 
  (lambda (x)
     (+ (sf1 x) (sf2 x) )))

;; Subtract two scaling functions with each other
(define (subtract-two-scaling-functions sf1 sf2) 
  (lambda (x)
     (- (sf1 x) (sf2 x) )))

;; Multiply two scaling functions with each other
(define (multiply-two-scaling-functions sf1 sf2) 
  (lambda (x)
     (* (sf1 x) (sf2 x) )))

;; Return a list of points useful for make-scale-function-by-xy-points.
;; Both x and y parameters are percentage values. They will therefore both be divided by 100.
;; .parameter point-lst A list of points. Each point is of the form (n m).
;; .returns A list of cons pairs of the form (n/100 . m/100) suitable for input to make-scale-function-by-xy-points.
;; .internal-references "Use context" "make-scale-function-by-xy-points"
(define (from-percent-points point-lst)
  (map (lambda (point)
         (cons (/ (first point) 100)
               (/ (second point) 100)))
       point-lst))

;; Return a list of points useful for make-scale-function-by-xy-points.
;; Both x and y parameters are permillage values. They will therefore both be divided by 1000.
;; .parameter point-lst A list of points. Each point is of the form (n m).
;; .returns A list of cons pairs of the form (n/1000 . m/1000) suitable for input to make-scale-function-by-xy-points.
;; .internal-references "Use context" "make-scale-function-by-xy-points"
(define (from-permille-points point-lst)
  (map (lambda (point)
         (cons (/ (first point) 1000)
               (/ (second point) 1000)))
       point-lst))


;;; .section-id scaling-functions
;;; Examples of Scaling Functions.
;;; This section contains concrete scaling functions, as generated by the functions in the previous section.
;;; These scaling functions may serve as inspiration when new scaling functions are needed. 
;;; As already mentioned above, a scaling function has the simple signature [0,1] -> Real.
;;; We link to SVG illustrations of the graphs of the scaling functions.
;;; When a new scaling function is developed it can be tried out in <a href = "scaling-functions/try.laml"> man/scaling-functions/try.laml </a>
;;; and visualized in <a href="scaling-functions/try.svg"> man/scaling-functions/try.svg </a>.
;;; Consult the Scheme source to access the source form of the scaling function.

(define pi 3.141592654)

;; A scaling function
;; .reference "Graphical illustration" "SVG" "scaling-functions/sf1.svg"
(define sf1
   (multiply-scaling-function 2.5
          (make-scale-function-by-xy-points
           (from-permille-points '((0 -150) (350 -110) (700 0) (760 50) (800 70) (850 60) (900 30) (1000 0)))
          )))

;; A scaling function
;; .reference "Graphical illustration" "SVG" "scaling-functions/sf2.svg"
(define sf2
   (multiply-scaling-function 1.0
          (make-scale-function-by-xy-points
           (from-permille-points '((0 -250) (500 -150) (750 -75) (800 -50) (900 -10) (1000 0)))
          )))

;; A scaling function
;; .reference "Graphical illustration" "SVG" "scaling-functions/sf3.svg"
(define sf3
   (multiply-scaling-function 1.8
          (make-scale-function-by-xy-points
           (from-permille-points '((0 0) (33 -50) (66 50)   (100 0) (133 -50) (166 50)  
                                   (200 0) (233 -50) (266 50)   (300 0) (333 -50) (366 50)  
                                   (400 0) (433 -40) (466 40)   (500 0) (533 -40) (566 40)  
                                   (600 0) (633 -30) (666 30)   (700 0) (733 -30) (766 30)  
                                   (800 0) (833 -25) (866 25)   (900 0) (933 -25) (966 25)   (1000 0)  ))
          )))

;; A scaling function
;; .reference "Graphical illustration" "SVG" "scaling-functions/sf4.svg"
(define sf4
   (make-scale-fn-pol-one-pol 'down 'down 4.5 0.35))


;; A scaling function
;; .reference "Graphical illustration" "SVG" "scaling-functions/sf5.svg"
(define sf5
   (make-scale-fn-pol-one-pol 'up 'down 5.5 0.25))


;; A scaling function
;; .reference "Graphical illustration" "SVG" "scaling-functions/sf6.svg"
(define sf6
  (multiply-two-scaling-functions 
      (make-scale-function-by-xy-points
         (from-percent-points '((0 100) (100 0))))
      (lambda (x) (sin (* x 15 pi)))
     ))



        

; ---------------------------------------------------------------------------------------------------------------
;;; .section-id midi-region-functions
;;; Midi region functions.
;;; This section contains function that establish regions around a list of midi messages.

;; Establish a context in which a smaller selection can be made.
;; Defined as a macro.
;; The context can be substituted by the selection inside it.
;; Underlying, a continuation named select is captured.
;; .form (midi-context continuation-name . messages)
;; .example (midi-context select ... (midi-region-do select ...) ...)
;; .parameter continuation-name The formal name of the continuation that controls the emitted MIDI messages.
;; .parameter messages A list of midi messages (such as NoteOn messages)
(define-syntax midi-context
  (syntax-rules ()
     ((midi-context select midi-message ...)
      (call-with-current-continuation
        (lambda (select)
           (list midi-message ...))))))

;; Marks a region of midi messages. Pass them through to contextual-continuation and, if in absTime mode, time displace them to time 0.
;; midi-region-do is used to select and play a selected part of a MIDI LAML file. 
;; .form (midi-region-do contextual-continuation . messages)
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .parameter contextual-continuation The continuation to which messages are passed. This is normally the continuation established by the midi-context form.
(define midi-region-do
 (xml-in-laml-positional-abstraction 1 0
   (lambda (contextual-continuation contents attributes)
     (let* ((ast-contents (filter ast? contents))
            (first-mes (if (not (null? ast-contents)) (first ast-contents) #f))
            (abs-time (if first-mes (ast-attribute first-mes 'absTime #f) #f)))
       (if abs-time  
           (contextual-continuation (time-displace (- (as-number abs-time)) contents))
           (contextual-continuation contents))))))

;; Marks a region of midi messages, with the purpose of adding additional structure to a MIDI LAML file (much like span and div in HTML). Pass them through to caller.
;; Mark the region in the binary midi file with midi-comments.
;; .form (midi-region . midi-messages)
;; .attribute name implied The name of the midi region
;; .attribute drop implied A boolean attribute which allows for elimination of the region.
;; .parameter messages A list of midi messages (such as NoteOn messages)
(define midi-region
 (xml-in-laml-abstraction
   (lambda (contents attributes)
     (let* ((drop (as-boolean (defaulted-get-prop 'drop attributes #f)))
            (name (defaulted-get-prop 'name attributes "")) 
            (sep (if (empty-string? name) "" ":"))
            (midi-comment-start (midi-comment (string-append "Midi region start" sep)  name ))
            (midi-comment-end (midi-comment (string-append "Midi region end" sep) name ))
           )
       (if (not drop)
           (list midi-comment-start contents midi-comment-end)
           '())))))

; ---------------------------------------------------------------------------------------------------------------
;;; .section-id sing-midi-abstractions
;;; Single midi message abstractions.
;;; Abstractions that generate a single, or a few midi messages.

;; The default midi null event info text.
(define midi-null-event-text "Midi null-event")

;; A deltaTime midi null event inserted at delta-time.
;; The midi null event is neutral with respect to the midi sound.
;; Implemented as a Meta event of type 1.
;; .form (midi-null-event delta-time [info-text])
;; .parameter delta-time The deltaTime of the midi-null-event. A non-negative integer.
;; .parameter info-text The text of the midi null event. Defaults to the value of the global variable midi-null-event-text.
;; .internal-references "Similar Predicate" "midi-null-event-message?"
;; .internal-references "Alias" "midi-null-event-delta-time"
;; .internal-references "Similar function" "midi-null-event-abs-time"
;; .misc Deprecated. Use midi-null-event-delta-time instead.
(define (midi-null-event delta-time . optional-parameter-list)
 (let ((info-text (optional-parameter 1 optional-parameter-list midi-null-event-text)))
  (Meta 'deltaTime delta-time 'type "1" info-text)))

;; A deltaTime midi null event inserted at delta-time.
;; The midi null event is neutral with respect to the midi sound.
;; Implemented as a Meta event of type 1.
;; .form (midi-null-event-delta-time delta-time [info-text])
;; .parameter delta-time The deltaTime of the midi-null-event. A non-negative integer.
;; .parameter info-text The text of the midi null event. Defaults to the value of the global variable midi-null-event-text.
;; .internal-references "Similar Predicate" "midi-null-event-message?"
;; .internal-references "Alias of" "midi-null-event"
(define midi-null-event-delta-time midi-null-event)

;; An absTime midi null event, inserted at abs-time.
;; The midi null event is neutral with respect to the midi sound.
;; Implemented as a Meta event of type 1.
;; .form (midi-null-event-abs-time abs-time [info-text])
;; .parameter abs-time The absTime of the midi-null-event. A non-negative integer.
;; .parameter info-text The text of the midi null event. Defaults to the value of the global variable midi-null-event-text.
(define (midi-null-event-abs-time abs-time . optional-parameter-list)
 (let ((info-text (optional-parameter 1 optional-parameter-list midi-null-event-text)))
  (Meta 'absTime abs-time 'type "1" info-text)))

;; Return a deltaTime meta event which can act as a comment in the midi file.
;; Inserted at deltaTime 0.
;; .form (midi-comment . text-strings)
;; .parameter text-strings Textual contents, in term of zero, one or several strings.
(define midi-comment
 (xml-in-laml-abstraction
  (lambda (contents attr)
    (Meta 'deltaTime "0" 'type 1 contents))))

;; Return ad absTime meta event which can act as a comment in the midi file.
;; .form (midi-comment-abs-time abs-time . text-strings)
;; .parameter abs-time The absolute-time in which to insert the midi comment.
;; .parameter text-strings Textual contents, in term of zero, one or several strings.
(define midi-comment-abs-time 
 (xml-in-laml-positional-abstraction 1 0 
  (lambda (abs-time contents attr)
    (Meta 'absTime abs-time 'type 1 contents))))

;; Return a deltaTime meta event which can act as a comment in the midi file.
;; .form (midi-comment-delta-time delta-time . text-strings)
;; .parameter delta-time The delta-time in which to insert the midi comment.
;; .parameter text-strings Textual contents, in term of zero, one or several strings.
(define midi-comment-delta-time 
 (xml-in-laml-positional-abstraction 1 0 
  (lambda (delta-time contents attr)
    (Meta 'deltaTime delta-time 'type 1 contents))))

;; Return a deltaTime meta event of type 6 - a marker event.
;; The marker text has the form: <marker-letter>-<maker-number> <marker-txt>.
;; The meta event gets deltaTime 0.
;; .form (midi-marker marker-txt [marker-number marker-letter])
;; .parameter marker-txt The text of the marker. (A string).
;; .parameter marker-number An integer marker number. Defaults to 0.
;; .parameter marker-letter A string of length one containing the marker letter. Defaults to "M".
(define (midi-marker marker-txt . optional-parameter-list)
 (let ((marker-number (optional-parameter 1 optional-parameter-list 0))
       (marker-letter (optional-parameter 2 optional-parameter-list "M"))
      )
  (Meta 'deltaTime "0" 'type "6" (string-append marker-letter "-" (as-string marker-number) " " marker-txt))))

;; Return an absTime meta event of type 6 - a marker event timed at abs-time.
;; The marker text has the form: <marker-letter>-<maker-number> <marker-txt>.
;; .form (midi-marker-abs-time abs-time marker-txt [marker-number marker-letter])
;; .parameter abs-time The absolute time of the MIDI marker (an integer og a text string). 
;; .parameter marker-txt The text of the marker. (A string).
;; .parameter marker-number An integer marker number. Defaults to 0.
;; .parameter marker-letter A string of length one containing the marker letter. Defaults to "M".
(define (midi-marker-abs-time abs-time marker-txt . optional-parameter-list)
 (let ((marker-number (optional-parameter 1 optional-parameter-list 0))
       (marker-letter (optional-parameter 2 optional-parameter-list "M"))
      )
  (Meta 'absTime (as-string abs-time) 'type "6" (string-append marker-letter "-" (as-string marker-number) " " marker-txt))))


;; Set the PAN of channel c to value. Returns a single MIDI ControlChange event.
;; Inserted at deltaTime 0.
;; .parameter c A channel number between 1 and 16.
;; .parameter value The pan value. 64 is neutral, 0 is hard left, and 127 is hard right.
(define (pan c value)
 (ControlChange 'deltaTime "0" 'channel c 'control "10" 'value value))

;; Set the volumen of channel c to value. Returns a single MIDI ControlChange event.
;; Inserted at deltaTime 0.
;; .parameter c A channel number between 1 and 16.
;; .parameter value The volumen value between 0 and 127.
(define (volume c value)
 (ControlChange 'deltaTime "0" 'channel c 'control "7" 'value value))

;; Set the level of reverb of channel c to value. Returns a single MIDI ControlChange event.
;; Inserted at deltaTime 0.
;; .parameter c A channel number between 1 and 16.
;; .parameter value The volumen value between 0 and 127.
(define (reverb c value)
 (ControlChange 'deltaTime "0" 'channel c 'control "91" 'value value))

;; Set the level of chorus of channel c to value. Returns a single MIDI ControlChange event.
;; Inserted at deltaTime 0.
;; .parameter c A channel number between 1 and 16.
;; .parameter value The volumen value between 0 and 127.
(define (chorus c value)
 (ControlChange 'deltaTime "0" 'channel c 'control "93" 'value value))


; Works in certain test, but probably not in general. Too simple and specific perhaps.
; Experimental and doubtful.
(define (dsp-variation-on)
 (list
   (SysEx 'deltaTime "0"  "08 43 10 4C 03 00 02 5B F7")
   (SysEx 'deltaTime "0"  "08 43 10 4C 03 00 03 05 F7")))

(define (dsp-variation-off)
 (list
   (SysEx 'deltaTime "0"  "08 43 10 4C 03 00 02 28 F7")
   (SysEx 'deltaTime "0"  "08 43 10 4C 03 00 03 04 F7")))

; Voice Convenience.
; (voice c msb lsb program-number)

;; Emit two channel change messages (msb/lsb) and a program change message.
;; You should look up msb/lsb bank number and program number in a midi reference sheet. 
;; The program number is according to the general midi specification.
;; All events are inserted at deltaTime 0.
;; .parameter channel A channel number (between 1 and 16)
;; .parameter msb Most significant byte of bank number. An integer between 0 and 127.
;; .parameter lsb Least significant byte of bank number. An integer between 0 and 127.
;; .parameter program-number. The Standard MIDI program number.
;; .internal-references "Alternative functions" "gm-voice" "voices-from-file" "voice-with-mix"
(define (voice channel msb lsb program-number)
  (list 
    (ControlChange 'deltaTime "0" 'channel channel 'control "0" 'value msb)
    (ControlChange 'deltaTime "0" 'channel channel 'control "32" 'value lsb)
    (ProgramChange 'deltaTime "0" 'channel channel 'number program-number)
  )
)

;; Emit two channel change messages (msb/lsb) and a program change message together with control messages for volume, pan, reverb and chorus.
;; All events are inserted at deltaTime 0.
;; .parameter channel A channel number (between 1 and 16)
;; .parameter msb Most significant byte of bank number. An integer between 0 and 127.
;; .parameter lsb Least significant byte of bank number. An integer between 0 and 127.
;; .parameter program-number. The Standard MIDI program number.
;; .parameter v. The amount of volume. An integer between 0 and 127.
;; .parameter p. The amount of pan. An integer between 0 and 127.
;; .parameter r. The amount of reverb. An integer between 0 and 127.
;; .parameter c. The amount of chorus. An integer between 0 and 127.
;; .internal-references "Simpler function" "voice"
(define (voice-with-mix channel msb lsb program-number v p r c)
  (list 
    (ControlChange 'deltaTime "0" 'channel channel 'control "0" 'value msb)
    (ControlChange 'deltaTime "0" 'channel channel 'control "32" 'value lsb)
    (ProgramChange 'deltaTime "0" 'channel channel 'number program-number)
    (volume channel v)
    (pan channel p)
    (reverb channel r)
    (chorus channel c)
  )
)


;; A specialized General Midi version of the voice function.
;; .parameter channel A channel number (between 1 and 16)
;; .parameter program. A program number (between 0 and 127) or a full/partial case-insensitive GM voice name.
;; .internal-references "More general function" "voice"
;; .internal-references "Alternative function" "voices-from-file"
(define (gm-voice channel program)
  (cond ((number? program)
           (voice channel 0 0 program))
        ((string? program)
           (let* ((gm-data-list (file-read (string-append midi-software-dir "data/general-midi-voices.dat")))
                  (res (find-in-list (lambda (entry) (equal? (downcase-string program)
                                                             (downcase-string (get-gm-voice-name entry))))
                                     gm-data-list)))
             (if res
                 (voice channel 0 0 (get-gm-voice-pc res))
                 (let ((res (find-in-list (lambda (entry) (substring? (downcase-string (get-gm-voice-name entry))
                                                                      (downcase-string program)))
                                          gm-data-list)))
                   (if res
                       (voice channel 0 0 (get-gm-voice-pc res))
                       (laml-error "gm-voice: Cannot make sense of program: " program))))))
        (else (laml-error "gm-voice: The second parameter must be a pc-number of a voice name (a string)"))))

;; Return a list of voice messages (ProgramChange and ControlChange of control type 0 and 32) for the voices requested in voice-file.
;; A voice file can be generated interactively from a so-called voice collection of a voice browser in the MIDI LAML environment.
;; A voice collection file has the extension 'voices'.
;; .form (voices-from-file [voice-file])
;; .returns A list of ASTs.
;; .parameter voice-file Either a voice file name (with or without 'voices' extension) in the current directory, \\
;;                       or a full path to a voice file (without extension).\\
;;                       Defaults to a voice file with the same proper name as the enclosing midi laml (midl) file.
;; .internal-references "More basic functions" "voice" "voice-with-mix" "gm-voice"
(define (voices-from-file . optional-parameter-list)
  (let* ((voice-file (optional-parameter 1 optional-parameter-list (source-filename-without-extension)))  ; (string-append (startup-directory) (source-filename-without-extension))
         (init-path (file-name-initial-path voice-file))
         (voice-file-with-extension (string-append (file-name-proper voice-file) "." "voices"))
         (file-path (string-append (if (empty-string? init-path) (startup-directory) init-path) voice-file-with-extension)) 
         (get-msb first) (get-lsb second) (get-pc third) (get-vol fifth) (get-pan sixth) (get-reverb seventh) (get-chorus eighth)  ; local accessors - only used here, so waste to make them global.
        ) 
    (if (file-exists? file-path)
        (let* ((voice-structure (file-read file-path)))
           (flatten 
            (filter (lambda (x) x)
                    (map2 (lambda (e ch)
                            (if (not (eq? e 'nil))
                                (if (= (length e) 4) 
                                       (voice ch (get-msb e) (get-lsb e) (get-pc e))
                                       (voice-with-mix ch (get-msb e) (get-lsb e) (get-pc e) (get-vol e) (get-pan e) (get-reverb e) (get-chorus e)))
                                #f))
                          (cdr voice-structure) ; first element is dummy
                          (number-interval 1 16) ; possible channel numbers
                          ))))
        (laml-error "voices-from-file: Non-exisisting voice file path: " file-path))))


;; Return a tempo Meta event, for tempo bpm, at deltaTime 0.
(define (tempo bpm)
  (Meta 'deltaTime "0" 'type "81" (tempo= bpm)))

;; Emit four control change messages which control the pitch-bend-range, in half tones and cents, for the given channel.
;; All events are inserted at deltaTime 0.
;; .form (pitch-bend-range channel range [cents])
;; .parameter channel A channel number (between 1 and 16)
;; .parameter range An integer between 1 and 12 (half tones). Typical range: 2.
;; .parameter cents An integer between 0 and 100, for fine adjustments. Defaults to 0.
;; .internal-references "Pitch bend constructor" "make-pitch-bend-change-list"
(define (pitch-bend-range channel range  . optional-parameter-list)
 (let ((cents (optional-parameter 1 optional-parameter-list 0)))
  (list
   ; Tell that the following data entry messages encoded Pitch Bend:
   (ControlChange 'deltaTime "0" 'channel channel 'control "101" 'value "0")
   (ControlChange 'deltaTime "0" 'channel channel 'control "100" 'value "0")  

   ; Sets the pitch bend semi-tone and fine tune adjustments
   (ControlChange 'deltaTime "0" 'channel channel 'control "6" 'value range)
   (ControlChange 'deltaTime "0" 'channel channel 'control "38" 'value cents)
  )))


;; Create a Yamaha Tyros Meta event (with deltaTiming, value 0) that encodes a given root and chord-type. The chord-type is optional, and it defaults to "M" (for major).
;; .form (chord-meta root [chord-type])
;; .parameter root The name of the root note. One of "C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B" (a string).
;; .parameter chord-type The name of a chord type, the default of which is "M".  One of "1+8" "1+5" "M" "6" "M7" "M7b5" "M7(#11)" "9" "M7_9" "6_9" "b5" "aug" "7aug" "M7aug" "m" "m6" "m7" "m7b5" "m(9)" "m7(9)" "m7(11)" "mM7b5" "mM7" "mM7(9)" "dim" "dim7" "7" "7sus4" "7(9)" "7(#11)" "7(13)" "7b5" "7(b9)" "7(b13)" "7(#9)" "sus4" "sus2" (a string).
;; .misc Please notice that upper/lower case is important in the chord-type parameter (but not in the root parameter).
;; .reference "Examples" "Midi LAML examples" "../examples/chord-meta/index.html"
(define (chord-meta root . optional-parameter-list)
  (let ((chord-type (optional-parameter 1 optional-parameter-list "M")))
    (let* ((chord-type-number (index-in-list-by-predicate chord-types chord-type (lambda (chord-types-row ct) (equal? (car chord-types-row) ct))))
           (root-number (calculate-root-number root))
           (chord-type-number-two-ciffer-hex-string (binary-to-hex-string (int10-to-binary chord-type-number 1)))
           (chord-root-number-two-ciffer-hex-string (binary-to-hex-string (int10-to-binary root-number 1)))  ; produces a hex string, such as "A3"
          )
      (Meta 'deltaTime "0" 'type "127" 
          (string-append "43 7B 01" " "
                         chord-root-number-two-ciffer-hex-string " " chord-type-number-two-ciffer-hex-string " "
                         chord-root-number-two-ciffer-hex-string " " chord-type-number-two-ciffer-hex-string)))))

; Root is a string of one or two characters, such as "C#"
; Return a number between 0 and 255 (really between 0 and 127)
(define (calculate-root-number root0)   ; tyros data list page 59, cr
  (let* ((root (upcase-string root0))
         (real-root (string-ref root 0))
         (root-variation (if (> (string-length root) 1) (string-ref root 1) #f))
         (part1-hex-ciffer 
            (cond ((not root-variation) 3)       ; natural
                  ((eqv? root-variation #\#) 4)    ; sharp
                  ((eqv? root-variation #\b) 2)    ; b           - not really supported
                  ((eqv? root-variation #\B) 2)    ; b           - not really supported
                  (else (laml-error "chord-meta -> calculate-root-number. Unknown root variation. Use only the empty or '#'" root))))
         (part2-hex-ciffer 
            (cond ((eqv? #\C real-root) 1)
                  ((eqv? #\D real-root) 2)
                  ((eqv? #\E real-root) 3)
                  ((eqv? #\F real-root) 4)
                  ((eqv? #\G real-root) 5)
                  ((eqv? #\A real-root) 6)
                  ((eqv? #\B real-root) 7)
                  ((eqv? #\H real-root) 7)
                  (else (laml-error "chord-meta -> calculate-root-number. Unknown root. Use only C D E F G A H B (or H for B)")))))
    (+ (* 16 part1-hex-ciffer) part2-hex-ciffer)))

;; Create a lyrics meta event with the given text. 
;; The lyrics meta event is inserted at deltaTime 0.
;; .parameter txt A lyrics contribution (a text string).
;; .misc Yamaha Tyros observation: Do not use the Danish Åÿ, Å¯, Å∆, and ÅÊ in the text. The Danish ÅÂ and Å≈ are OK.
(define (lyrics txt)
  (Meta 'deltaTime "0" 'type "5" txt))


; ---------------------------------------------------------------------------------------------------------------
; Chord playing.

;; Play a given chord progression as a (maybe long) number of NoteOn messages, mostly for chord demo purposes.
;; The progression is allways ended by the root note.
;; A list of deltaTime NoteOn messages are created. Each note has deltaTime time-delta.
;; Each played note will last duration  time units
;; There will be time-delta between notes in the played chord (meassured in basic type units, 1920 pr. quarter note on tyros).
;; .form (play-chord root chord-type start-octave number-of-octaves time-delta duration [channel velocity])
;; .parameter root The name of the root. One of "C" "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B". (A string).
;; .parameter chord-type The name of a chord type. One of "1+8" "1+5" "M" "6" "M7" "M7b5" "M7(#11)" "9" "M7_9" "6_9" "b5" "aug" "7aug" "M7aug" "m" "m6" "m7" "m7b5" "m(9)" "m7(9)" "m7(11)" "mM7b5" "mM7" "mM7(9)" "dim" "dim7" "7" "7sus4" "7(9)" "7(#11)" "7(13)" "7b5" "7(b9)" "7(b13)" "7(#9)" "sus4" "sus2". (A string).
;; .parameter start-octave an octave number. Octave number 3 contains the middle C (note 60) - according the yamaha convention. Within the interval [-2..8]. (An integer).
;; .parameter number-of-octaves The number of octaves the play the cord. A positive integer.
;; .parameter time-delta The delta-time of each generated NoteOn message.
;; .parameter duration The durationof each generated NoteOn message.
;; .parameter channel The channel of the generated NoteOn messages. Defaults to channel 1.
;; .parameter velocity The velocity of the generated NoteOn messages. Defaults to channel 80.
;; .reference "Examples" "Midi LAML examples" "../examples/play-chord/index.html"
(define (play-chord root chord-type start-octave number-of-octaves time-delta duration . optional-parameter-list)
 (let ((ch (optional-parameter 1 optional-parameter-list 1))
       (vel (optional-parameter 2 optional-parameter-list 80)))
  (let* ((absolute-repeated-root-chord-formula (chord-note-list root chord-type start-octave number-of-octaves))
         (time-delta-list (make-list (length absolute-repeated-root-chord-formula) time-delta))
        )
    (map
      (lambda (nn dt)
         (NoteOn 'deltaTime dt 'channel ch 'note (between 0 127 nn) 'velocity vel 'duration duration))
      absolute-repeated-root-chord-formula
      time-delta-list)
  )))

; Return a list of NoteOn messages ending at the note value note (an integer).
; A list of deltaTime NoteOn messages are created; Thus, the chord sequence will be relative to the message occurring before the sequence.
; Root is a name (string): C, C#, D, D#, E, F, F#, G, G#, A, A#, B.
; Chord-type is a chord type name, as appearing in the list chord-types.
; There will be  time-delta  between notes in the played chord (meassured in basic type units, 1920 pr. quarter note on tyros).
; Each played note will last  duration  time units
; Optional parameters allows for control of channel and velocity.
; .form (noteon-sequence-ending-at note lgt root chord-type time-delta duration [channel velocity])
; .misc lgt must be at least 2 (not a problem in practical life due to lengths of chords).
(define (noteon-sequence-ending-at note lgt root chord-type time-delta duration . optional-parameter-list)
 (let ((ch (optional-parameter 1 optional-parameter-list 1))
       (vel (optional-parameter 2 optional-parameter-list 80)))
   (let ((chord-lst (chord-note-list-ending-at note lgt root chord-type)))
     (if chord-lst
         (cons
           ; first NoteOn is moved back in time
           (NoteOn 'deltaTime (- (* time-delta (- (length chord-lst) 1))) 'channel ch 'note (first chord-lst) 'velocity vel 'duration duration)

           ; The rest are relative to the first
           (map 
            (lambda (note-val) 
                (NoteOn 'deltaTime time-delta 'channel ch 'note note-val 'velocity vel 'duration duration)
              )
            (cdr chord-lst))
         )
         '()))))


; Add strum (a sequence of chord adapted notes) to note-ast.
; If the chord does not match note-on-ast, return (a list of) note-on-ast. Else return a longer chord adapted list ending with note-on-ast.
; note-on-ast should be with absTime.
; The parameters lgt, root, chord-type, delta-time, duration, channel and velocity is as for noteon-sequence-ending-at.
(define (strum-one-note note-on-ast lgt root chord-type time-delta duration . optional-parameter-list)
  (let* ((ch (optional-parameter 1 optional-parameter-list (ast-attribute note-on-ast 'channel)))
         (vel (optional-parameter 2 optional-parameter-list (ast-attribute note-on-ast 'velocity)))
         (note (as-number (ast-attribute note-on-ast 'note)))
         (seq (noteon-sequence-ending-at note lgt root chord-type time-delta duration ch vel)))
     (if (not (null? seq)) 
         (append (list note-on-ast) (butlast seq))   ; note-on-ast must have absTime. Strum is relative to it, therefore note-on-ast should come first
         (list note-on-ast))))

;; Add strum (a sequence of chord adapted notes) to all matching notes in messages.
;; Similar to strum-2, but this version takes explicit, positional chord root, chord type and strum length parameters.
;; A given notes matches a chord and a root if the note is present in the chord sequence made by root and chord-type.
;; If the chord does not match a note, just return the note. Else return a longer chord adapted list ending with note-on-ast.
;; Should only be used on absTime (?).
;; The parameters lgt, root, chord-type, delta-time, duration, channel and velocity is as for noteon-sequence-ending-at.
;; .form (strum-1 length root chord-type . messages)
;; .parameter length The number notes produced in case of a match (a non-negative integer)
;; .parameter root The name of the root note. One of "C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B" (a string).
;; .parameter chord-type The name of a chord type. One of "1+8" "1+5" "M" "6" "M7" "M7b5" "M7(#11)" "9" "M7_9" "6_9" "b5" "aug" "7aug" "M7aug" "m" "m6" "m7" "m7b5" "m(9)" "m7(9)" "m7(11)" "mM7b5" "mM7" "mM7(9)" "dim" "dim7" "7" "7sus4" "7(9)" "7(#11)" "7(13)" "7b5" "7(b9)" "7(b13)" "7(#9)" "sus4" "sus2" (a string).
(define strum-1
 (xml-in-laml-positional-abstraction 3 0
   (lambda (lgt root chord-type contents attributes)
     (map (lambda (mes-ast)
            (if (and (ast? mes-ast) (equal? "NoteOn" (ast-element-name mes-ast)))
                (strum-one-note mes-ast lgt root chord-type 300 300)
                mes-ast))
          contents))))

;; Add strum (a sequence of chord adapted notes) to all matching notes in messages.
;; Similar to strum-1, but this version takes chord root, chord type and strum length from attributes of NoteOn.
;; The strum-length attribute defaults to 4 in the context of this function.
;; The chord-type defaults to major (\"M\"). This \"C\" and \"C#\" are legal chord attribute values of NoteOn elements. They are identical to \"CM\" and \"C#M\".
;; A given notes matches a chord and a root if the note is present in the chord sequence made by root and chord-type.
;; If the chord does not match a note, just return the note. Else return a longer chord adapted list ending with note-on-ast.
;; Should only be used on absTime (?).
;; The chord root, chord type, and strum length are taken from the chord and strum-length attributes of the NoteOn elements.
;; .form (strum-2 . messages)
(define strum-2
 (xml-in-laml-abstraction 
   (lambda (contents attributes)
     (map (lambda (mes-ast)
            (if (and (ast? mes-ast) (equal? "NoteOn" (ast-element-name mes-ast)))
                (let* ((lgt (as-number (ast-attribute mes-ast 'strum-length 4)))
                       (chord (ast-attribute mes-ast 'chord #f)))
                  (if chord
                      (let* ((root-chordtype (split-chord-to-root-and-type chord))
                             (root (car root-chordtype))
                             (chord-type (cdr root-chordtype)))
                        (strum-one-note mes-ast lgt root chord-type 300 300))
                      mes-ast))
                mes-ast))
          contents))))


;; Add strum (a sequence of chord adapted notes) to all matching notes in messages which belong to channel.
;; Similar to strum-1, but this version takes chord root, chord type from Meta events, maybe and typically generated from the accompaniment of Keyboard.
;; It may also take chord information from a chord attribute of NoteOn messages. 
;; As a distinctive feature of this version of strum, the chord information is carried through the messages.
;; The strum-length attribute defaults to 4 in the context of this function, but it can change if a NoteOn messages carries a strum-length attribute.
;; A given notes matches a chord and a root if the note is present in the chord sequence made by root and chord-type.
;; If the chord does not match a note, just return the note. Else return a longer chord adapted list ending with note-on-ast.
;; Should only be used on absTime (?).
;; .form (strum-3 channel . messages)
(define strum-3
 (xml-in-laml-positional-abstraction 1 0
   (lambda (ch contents attributes)
      (strum-3-internal ch contents #f #f 10))))

(define (strum-3-internal ch contents root chord-type strum-length)
  (cond ((null? contents) '())
        (else (let ((mes-ast (car contents)))
                 (cond ((and (ast? mes-ast) (equal? "NoteOn" (ast-element-name mes-ast)) (= ch (as-number (ast-attribute mes-ast 'channel))))
                          (let* ((lgt-new (as-number (ast-attribute mes-ast 'strum-length strum-length)))
                                 (chord-new (ast-attribute mes-ast 'chord #f)))
                            (if chord-new
                                (let* ((root-chordtype (split-chord-to-root-and-type chord-new))
                                       (root-new (car root-chordtype))
                                       (chord-type-new (cdr root-chordtype)))
                                    (append (strum-one-note mes-ast lgt-new root-new chord-type-new 300 300) 
                                            (strum-3-internal ch (cdr contents) root-new chord-type-new lgt-new)))
                                (if (and root chord-type)
                                    (append (strum-one-note mes-ast lgt-new root chord-type 300 300) 
                                             (strum-3-internal ch (cdr contents) root chord-type strum-length))
                                    (cons mes-ast (strum-3-internal ch (cdr contents) root chord-type strum-length))))))

                       ((meta-chord-ast? mes-ast)
                          (let* ((root-and-chordtype (meta-chord-root-and-chordtype mes-ast))
                                 (root-new (car root-and-chordtype))  ; a string, maybe with b instead of #
                                 (chord-type-new (cdr root-and-chordtype)))  ; a string
                            (cons mes-ast (strum-3-internal ch (cdr contents) root-new chord-type-new strum-length))))

                       (else (cons mes-ast (strum-3-internal ch (cdr contents) root chord-type strum-length))))))))


; Split chord to cons of two strings: chord root and chord-type.
; Typical input "C#M7". Output ( "C#" . "M7").
(define (split-chord-to-root-and-type chord)
  (let ((lgt (string-length chord)))
    (cond ((= lgt 1) (cons (substring chord 0 1) "M"))
          ((= lgt 2) 
             (if (eqv? (string-ref chord 1) #\#)
                 (cons (substring chord 0 2) "M")
                 (cons (substring chord 0 1) (substring chord 1 lgt))))
          ((>= lgt 2) 
             (if (eqv? (string-ref chord 1) #\#)
                 (cons (substring chord 0 2) (substring chord 2 lgt))
                 (cons (substring chord 0 1) (substring chord 1 lgt)))))))


;; Return a list of n deltaTimed PitchBendChange midi events for channel ch with a total duration.
;; The list returned is a flat list of deltaTime PitchBendChange events.
;; The value of the PitchBendChange events are scaled by a standard scaling function, scale-fn.
;; Scaling functions are functions from [0,1] -> Real.
;; Per convention of this function, the scaling value 0 corresponds to 8192 (pitch bend neutral).
;; Scaling value 1 corresponds to 16383, and scaling value -1 correponds to 0.
;; .form (make-pitch-bend-change-list ch n duration scale-fn [first-delta-time])
;; .parameter ch Channel number (between 1 and 16).
;; .parameter n The number of PitchBendChange messages to produce. Minimum value: 2
;; .parameter duration The total duration of the list of PitchBendChange messages. \
;;                     Only accurate if the first-delta-time is the default value.
;; .parameter scale-fn The scaling function: [0,1] -> [-1, 1].
;; .parameter first-delta-time The delta time of the first PichBendChange message. Defaults to the calculated deltaTime of all PitchBendChange messages.\
;;            It may, in some situations, be useful to use a negative value of this parameter to facilitate an earlier insertion point than the physical location\
;;            of the make-pitch-bend-change-list form.
;; .internal-references "Scaling functions" "scaling-function-generation"
;; .internal-references "Pitch bend calibration" "pitch-bend-range"
(define (make-pitch-bend-change-list ch n duration scale-fn . optional-parameters)
 (let ((first-delta-time (optional-parameter 1 optional-parameters (/ duration n))))
   (letrec ((make-function-domain-values ; produces length equidistant values between 0 and 1. actual is used for accumulation, and should initially be 0.
             (lambda (length increment actual)
               (if (= length 0)
                   '()
                   (cons actual (make-function-domain-values (- length 1) increment (+ actual increment))))))
            )
     (let ((pitch-value-fn (compose (lambda (r) (+ (* r 8192) 8192)) scale-fn)) ;     [0,1] -> pitch value
           (delta-dur (/ duration n))
           (function-unit-domain-values (make-function-domain-values n (/ 1 (- n 1)) 0))
          )
       (cons (let ((value (between 0 16383 (pitch-value-fn (car function-unit-domain-values)))))
                (PitchBendChange 'deltaTime (as-int-string first-delta-time) 'channel (as-string ch)
                              'value (as-int-string value)))
             (map
              (lambda (unit-domain-value)
                (let ((value (between 0 16383 (pitch-value-fn unit-domain-value))))
                  (PitchBendChange 'deltaTime (as-int-string delta-dur) 'channel (as-string ch) 'value (as-int-string value))))
              (cdr function-unit-domain-values)))))))

;; Return a list of n deltaTimed tempo Meta events with a total length of duration.
;; Scales the base tempo with scale-fn.
;; .form (make-tempo-change-list n duration base-tempo scale-fn [last-tempo])
;; .parameter n The number of Meta tempo messages to produce. At least one.
;; .parameter duration The total duration of the list of Meta tempo messages (in time ticks).
;; .parameter base-tempo The base tempo, which is scaled throughout the of generated Meta tempo events. A real number.
;; .parameter scale-fn The scaling function: [0,1] -> Positive Real Number.
;; .parameter last-tempo If provided, the last Meta tempo event is forced to be last-tempo. A real number.
;; .internal-references "Scaling functions" "scaling-function-generation"
(define (make-tempo-change-list n0 duration base-tempo scale-fn . optional-parameter-list)
 (let* ((n (- n0 1))
        (last-tempo (optional-parameter 1 optional-parameter-list #f)))
  (letrec ((make-function-domain-values ; produces length+1 equidistant values between 0 and 1. actual is used for accumulation, and should initially be 0.
            (lambda (length increment actual)
              (if (= length 0)
                  '()
                  (cons actual (make-function-domain-values (- length 1) increment (+ actual increment))))))
          )
    (if last-tempo  ; insert a forced tempo event as the last in the list of deltaTimed meta events
        (let ((function-unit-domain-values (make-function-domain-values (+ n 1) (/ 1 n) 0))
              (delta-dur (/ duration n0))
             )
           (append
             (map
               (lambda (unit-domain-value)
                 (Meta 'deltaTime (as-int-string delta-dur) 'type "81" (tempo= (* base-tempo (scale-fn unit-domain-value))))
                 )
               (butlast function-unit-domain-values))  ; notice butlast
             (list  ; the last forced tempo Meta event:
                (Meta 'deltaTime (as-int-string delta-dur) 'type "81" (tempo= last-tempo)))))

        (let ((function-unit-domain-values (make-function-domain-values (+ n 1) (/ 1 n) 0))
              (delta-dur (/ duration n0))
              )
          (map
           (lambda (unit-domain-value)
             (Meta 'deltaTime (as-int-string delta-dur) 'type "81" (tempo= (* base-tempo (scale-fn unit-domain-value))))
             )
           function-unit-domain-values))
    ))))

;; Return a list of n deltaTimed ControlChange type 7 'channel volume" events with a total length of duration.
;; Scales the start-volume with scale-fn.
;; .form (make-volume-change-list ch n duration start-volume scale-fn [end-volumne])
;; .parameter n The number of ControlChange messages to produce. At least one.
;; .parameter duration The total duration of the list of ControlChange messages (in time ticks).
;; .parameter start-volumne The start volume which is scaled throughout the of generated ControlChange list.
;; .parameter scale-fn The scaling function: [0,1] -> Positive Real Number.
;; .parameter end-volumne If provided, the end volumne value is forced to be end-volume.
;; .internal-references "Scaling functions" "scaling-function-generation"
(define (make-volume-change-list ch n0 duration start-volume scale-fn . optional-parameter-list)
  (let* ((n (- n0 1))
         (end-volume (optional-parameter 1 optional-parameter-list #f)))
    (letrec ((make-function-domain-values ; produces length equidistant values between 0 and 1. actual is used for accumulation, and should initially be 0.
              (lambda (length increment actual)
                (if (= length 0)
                    '()
                    (cons actual (make-function-domain-values (- length 1) increment (+ actual increment))))))
             )
      (if end-volume ; insert a forced end-volume as the last in the list of deltaTimed meta events
          (let ((function-unit-domain-values (make-function-domain-values (+ n 1) (/ 1 n) 0))
                (delta-dur (/ duration n0))
               )
            (append
             (map
              (lambda (unit-domain-value)
                (ControlChange 'deltaTime (as-int-string delta-dur) 'channel ch 'control "7" 
                               'value (as-int-string (between 0 127 (* start-volume (scale-fn unit-domain-value)))))
                )
              (butlast function-unit-domain-values)) ; notice butlast
             (list                 ; the last forced volumne control event:
              (ControlChange 'deltaTime (as-int-string delta-dur) 'channel ch 'control "7" 
                             'value (as-int-string (between 0 127 end-volume))))))

          (let ((function-unit-domain-values (make-function-domain-values (+ n 1) (/ 1 n) 0))
                (delta-dur (/ duration n0))
                )
            (map
             (lambda (unit-domain-value)
               (ControlChange 'deltaTime (as-int-string delta-dur) 'channel ch 'control "7"
                               'value (as-int-string (between 0 127 (* start-volume (scale-fn unit-domain-value)))))
               )
             function-unit-domain-values))
          ))))
  

; ---------------------------------------------------------------------------------------------------------------
;;; .section-id channel-repl-join-sel
;;; Channel replication, (copying), rechanneling, joining, and selection.

;; Replicate each message of a given channel in message-list. Allocate each replicated message in channel ch-to.
;; Existing messages in ch-to are not affected.
;; It may be useful to use target channels outside the interval [1..16] for temporary purposes. 
;; Each message belonging to channel ch-to is immediately replicated. (This is a contrast to the function replicate, which serves as "verse replication").
;; .form (replicate-channel ch-from ch-to . messages)
;; .parameter ch-from A channel (an integer between 1 and 16)
;; .parameter ch-to A channel (an integer between 1 and 16)
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .internal-references "More general functions" "replicate-by-predicate" "replicate-by-predicate-and-transformation"
(define replicate-channel
 (xml-in-laml-positional-abstraction 2 0
  (lambda (ch-from ch-to cont attr)
    (replicate-channel-1 ch-from ch-to cont))))

(define (replicate-channel-1 ch-from ch-to message-list)
 (map 
  (lambda (mes-ast) 
    (if (ast? mes-ast)
        (let ((channel (ast-attribute mes-ast 'channel #f)))
          (if channel
              (if (= ch-from (as-number channel))
                  (list 
                     mes-ast
                     (copy-ast-mutate-attributes mes-ast 'channel ch-to)
                  )
                  mes-ast)
              mes-ast))
         mes-ast))
  message-list))

;; Replicate each channel-message that satisfies the predicate. Allocate each replicated message to channel ch-to.
;; Existing messages in ch-to are not affected. 
;; Midi messages that do not satisfy the predicate are not affected.
;; Non-channel messages are not affected.
;; Works in both absTime and deltaTime mode.
;; .form (replicate-by-predicate predicate ch-to . messages)
;; .parameter predicate A midi message predicate. If true, replicate the message to channel ch-to.
;; .parameter ch-to A channel (an integer between 1 and 16)
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .internal-references "More general function" "replicate-by-predicate-and-transformation"
;; .internal-references "More basic function" "replicate-channel"
(define replicate-by-predicate
 (xml-in-laml-positional-abstraction 2 0
  (lambda (predicate ch-to cont attr)
    (replicate-by-predicate-1 predicate ch-to cont))))

(define (replicate-by-predicate-1 predicate ch-to message-list)
 (map 
  (lambda (mes-ast) 
    (if (ast? mes-ast)
        (let ((channel (ast-attribute mes-ast 'channel #f)))
          (if (and channel (predicate mes-ast))
              (list   ; the replication:
                mes-ast
                (copy-ast-mutate-attributes mes-ast 'channel ch-to)
              )
              mes-ast))
         mes-ast
     ))
  message-list))

;; A generalization of replicate-by-predicate, which passes the message to be replicated to a transformation function.
;; Replicates a channel message. The replications are returned by the transformation function when passed a channel message.
;; Only replicate those messages that satisfy the predicate.
;; Midi messages that do not satisfy the predicate are not affected.
;; Non-channel messages are not affected.
;; Works in both absTime and deltaTime mode.
;; .form (replicate-by-predicate predicate transformation . messages)
;; .parameter predicate A midi message predicate. If true, replicate the message to channel ch-to.
;; .parameter transformation A function that transforms a single midi message (passed by the predicate) to either a single AST, or a list of ASTs.
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .internal-references "More basis functions" "replicate-by-predicate" "replicate-channel"
(define replicate-by-predicate-and-transformation
 (xml-in-laml-positional-abstraction 2 0
  (lambda (predicate transf cont attr)
    (replicate-by-predicate-and-transformation-1 predicate transf cont))))

(define (replicate-by-predicate-and-transformation-1 predicate transf message-list)
 (map 
  (lambda (mes-ast) 
    (if (ast? mes-ast)
        (let ((channel (ast-attribute mes-ast 'channel #f)))
          (if (and channel (predicate mes-ast))
              (let ((the-transformation (transf mes-ast)))
                 (cond ((ast? the-transformation) (list mes-ast (transf mes-ast)))
                       ((list? the-transformation) (cons mes-ast the-transformation))
                       (else (laml-error "replicate-by-predicate-1: The transformation must return an AST, or a list of ASTs"))))
              mes-ast))
         mes-ast
     ))
  message-list))





;; Join (remove) messages in channel-list, and instead assign these to channel ch-to.
;; Existing messages in ch-to are not affected.
;; It is allowed to use source/target channels outside the interval [1..16], which are discared upon midi generation.
;; (If you use channels outside the interval [1..16] it may affect the timing of the remaining events.
;;  Therefore it is recommended to stay inside the interval [1.16]).
;; ch-to is allowed to be one of the channels in channel-list.
;; .form (join-channels channel-list ch-to . messages)
;; .parameter channel-list A list of channel numbers (an integer list).
;; .paramter  ch-to  A channel number (between 1 and 16).
;; .parameter messages A list of midi messages (such as NoteOn messages)
(define join-channels
 (xml-in-laml-positional-abstraction 2 0
  (lambda (ch-list ch-to cont attr)
    (join-channels-1 ch-list ch-to cont))))

(define (join-channels-1 ch-list ch-to message-list)
 (map 
  (lambda (mes-ast) 
    (if (ast? mes-ast)
        (let ((channel (ast-attribute mes-ast 'channel #f)))
          (if channel
              (if (member (as-number channel) ch-list)
                  (copy-ast-mutate-attributes mes-ast 'channel ch-to)
                  mes-ast)
              mes-ast))
         mes-ast))
  message-list))


;; Return only those messages from message-list which belong to the given channel.
;; Also include messages without a channel assigned to them.
;; Works in both absTime and deltaTime mode.
;; .form (select-channel channel . messages)
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .parameter channel A channel (an integer between 1 and 16)
(define select-channel
 (xml-in-laml-positional-abstraction 1 0
  (lambda (ch cont attr)
    (eliminate-midi-null-events (select-channel-1 ch cont)))))

(define (select-channel-1 c message-list)
 (map 
  (lambda (mes-ast) 
    (if (ast? mes-ast)
        (let ((channel (ast-attribute mes-ast 'channel #f)))
          (if channel
              (if (= c (as-number (ast-attribute mes-ast 'channel)))
                  mes-ast
                  (midi-null-event (ast-attribute mes-ast 'deltaTime 0)))
              mes-ast))
         mes-ast))
  message-list))

;; Delete those messages from message-list which belong to the given channel.
;; In this version the deleted messages are substituted by midi null events.
;; As a consequence, the function works in both absTime and deltaTime mode.
;; .form (delete-channel channel . messages)
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .parameter channel A channel (an integer between 1 and 16)
;; .internal-references "absTime only variant" "delete-channel-abstime"
(define delete-channel
 (xml-in-laml-positional-abstraction 1 0
  (lambda (ch cont attr)
    (delete-channel-1 ch cont))))

(define (delete-channel-1 c message-list)
 (map 
  (lambda (mes-ast) 
    (if (ast? mes-ast)
        (let ((channel (ast-attribute mes-ast 'channel #f)))
          (if channel
              (if (= c (as-number (ast-attribute mes-ast 'channel)))
                  (midi-null-event (ast-attribute mes-ast 'deltaTime 0))
                  mes-ast)
              mes-ast))
         mes-ast))
  message-list))

;; Delete those messages from message-list which belong to the given channel.
;; The messages disappear entirely.
;; .form (delete-channel-abs-time channel . messages)
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .parameter channel A channel (an integer between 1 and 16)
;; .internal-references "Mode independent version" "delete-channel"
;; .mode absTime 
(define delete-channel-abs-time
 (xml-in-laml-positional-abstraction 1 0
  (lambda (ch cont attr)
    (delete-channel-abs-time-1 ch cont))))

(define (delete-channel-abs-time-1 ch message-list)
  (delete-channel-abs-time-2 ch message-list '()))

(define (delete-channel-abs-time-2 ch message-list res)
  (if (null? message-list)
      (reverse res)
      (let ((mes-ast (car message-list)))
        (if (and (ast? mes-ast) (ast-attribute mes-ast 'channel #f) (= ch (as-number (ast-attribute mes-ast 'channel))))
            (delete-channel-abs-time-2 ch (cdr message-list) res)
            (delete-channel-abs-time-2 ch (cdr message-list) (cons mes-ast res))))))

;; Select (project) to only those channels of channel-list.
;; Also include messages without a channel assigned to them.
;; .form (select-channels channel-list . messages)
;; .parameter messages A list of midi messages (such as NoteOn messages) or #t in the meaning of all channels.
;; .parameter channel-list A list of channel numbers (an integer list).
(define select-channels
 (xml-in-laml-positional-abstraction 1 0
  (lambda (ch-list cont attr)
   (if (and (boolean? ch-list) ch-list)  ; channel-list is #t
       cont
       (eliminate-midi-null-events (select-channels-1 ch-list cont 0))))))

; (define (select-channels-1 c-list message-list)
;  (map
;   (lambda (mes-ast)
;     (if (ast? mes-ast) 
;         (let ((channel (ast-attribute mes-ast 'channel #f)))
;           (if channel
;               (if (member (as-number (ast-attribute mes-ast 'channel)) c-list)
;                   mes-ast 
;                   (midi-null-event (ast-attribute mes-ast 'deltaTime 0)))
;               #t))
;         mes-ast))
;   message-list))

; between-time is accumulated time between deltaTime events. Only used for deltaTime mode. Rather complicated. 
; Future: Split deltaTime and absTime version
(define (select-channels-1 c-list message-list between-time)
 (cond ((null? message-list)  '())
       (else (let ((mes-ast (first message-list)))
                (if (ast? mes-ast) 
                    (let ((channel (ast-attribute mes-ast 'channel #f)))
                      (if channel
                          (if (or (and (boolean? c-list) c-list) (member (as-number (ast-attribute mes-ast 'channel)) c-list))
                              (cons (if (delta-time-message? mes-ast)
                                        (copy-ast-mutate-attributes mes-ast 'deltaTime (+ (time-of-message mes-ast) between-time))
                                        mes-ast)
                                    (select-channels-1 c-list (cdr message-list) 0))
                              (if (delta-time-message? mes-ast)
                                  (cons (midi-null-event (+ (time-of-message mes-ast) between-time))   ; remove event from unwanted channel - keep delta time in null event
                                        (select-channels-1 c-list (cdr message-list) 0))    
                                  (select-channels-1 c-list (cdr message-list) (+ between-time (time-of-message mes-ast)))    ; remove event from unwanted channel
                              )
                          )
                          (cons   ; system and meta messages 
                             (if (delta-time-message? mes-ast)
                                 (copy-ast-mutate-attributes mes-ast 'deltaTime (+ (time-of-message mes-ast) between-time))
                                 mes-ast)
                             (select-channels-1 c-list (cdr message-list) 0))
                      )
                    )
                    (cons mes-ast (select-channels-1 c-list (cdr message-list) between-time)))))))

(define (time-of-message mes-ast)
  (let ((abs-time (ast-attribute mes-ast 'absTime #f))
        (delta-time (ast-attribute mes-ast 'deltaTime #f)))
    (cond  (abs-time (as-number abs-time))
           (delta-time (as-number delta-time))
           (else (laml-error "time-of-message: Message AST without deltaTime or absTime attribute")))))

(define (delta-time-message? mes-ast)
  (if (ast? mes-ast)
      (as-boolean (ast-attribute mes-ast 'deltaTime #f))
      #f))

(define (abs-time-message? mes-ast)
  (if (ast? mes-ast)
      (as-boolean (ast-attribute mes-ast 'absTime #f))
      #f))

;; Rechannel (change channel numbers) messages according to channel-map.
;; Channel not addressed in the channel-map are not affected. 
;; Non-channel messages are not affected.
;; .form (rechannel channel-map . messages)
;; .parameter messages A list of midi messages.
;; .parameter channel-map An association list of channel numbers that map a source channel to a target channel.\
;;            Example: ((3 . 1) (9 . 7) (10 . 8) (11 . 7) (12 . 2) (13 . 3)).
(define rechannel
 (xml-in-laml-positional-abstraction 1 0
  (lambda (ch-map cont attr)
    (rechannel-1 ch-map cont))))

(define (rechannel-1 ch-map message-list)
 (map 
  (lambda (mes-ast) 
    (if (ast? mes-ast)
        (let ((channel (ast-attribute mes-ast 'channel #f)))
          (if channel
              (if (assoc (as-number channel) ch-map)
                  (copy-ast-mutate-attributes mes-ast 'channel (cdr (assoc (as-number channel) ch-map)))
                  mes-ast)
              mes-ast))
         mes-ast))
  message-list))


;; Eliminate midi null events in all channels. Works in both absTime and deltaTime mode.
;; .form (eliminate-midi-null-events . messages)
;; .parameter messages A list of midi messages (such as NoteOn messages)
(define eliminate-midi-null-events
 (xml-in-laml-abstraction 
  (lambda (cont attr)
   (eliminate-midi-null-events-1 cont 0 0))))

; acc-delta is accumulated delta-time until a non-null event
; abs-time is the current absolute time.
(define (eliminate-midi-null-events-1 message-list abs-time acc-delta)
  (eliminate-events-1 midi-null-event-message? message-list abs-time acc-delta))

;; Eliminate events that matches the predicate. The elimination is done independent of channel. Works in both absTime and deltaTime mode.
;; .form (eliminate-events predicate . messages)
;; .parameter messages A list of midi messages (such as NoteOn messages)
;; .parameter predicate A predicate which is guarantied to be called on an AST.
(define eliminate-events
 (xml-in-laml-positional-abstraction 1 0
  (lambda (predicate cont attr)
   (eliminate-events-1 predicate cont 1 0))))

; acc-delta is accumulated delta-time until a non-null event
; abs-time is the current absolute time.
(define (eliminate-events-1 predicate message-list abs-time acc-delta)
   (cond ((null? message-list)  '())
         (else (let ((mes-ast (first message-list)))
                 (if (ast? mes-ast)
                     (cond ((and (abs-time-message? mes-ast) (predicate mes-ast))       ; just drop message
                              (let ((delta-time (- (time-of-message mes-ast) abs-time)))
                                (eliminate-events-1 predicate (cdr message-list) (time-of-message mes-ast) (+ delta-time acc-delta))))
                           ((and (abs-time-message? mes-ast) (not (predicate mes-ast)))
                              (cons mes-ast (eliminate-events-1 predicate (cdr message-list) (time-of-message mes-ast) 0)))
                           ((and (delta-time-message? mes-ast) (predicate mes-ast))
                              (let ((delta-time (time-of-message mes-ast)))
                               (eliminate-events-1 predicate (cdr message-list) (+ abs-time delta-time) (+ delta-time acc-delta))))
                           ((and (delta-time-message? mes-ast) (not (predicate mes-ast)))
                              (let ((delta-time (time-of-message mes-ast)))
                                (cons (copy-ast-mutate-attributes mes-ast 'deltaTime (+ acc-delta delta-time))
                                      (eliminate-events-1 predicate (cdr message-list) (+ abs-time delta-time) 0))))
                           (else (laml-error "eliminate-events-1: Should not happen.")))

                     (cons mes-ast (eliminate-events-1 predicate (cdr message-list) abs-time acc-delta)))))))

;; Split the NoteOn events in the given channel to two channels.
;; The NoteOn events in channel that satisfy the predicate will be located in channel-true, and those that do not will be located in channel-false.
;; Midi events in other channels, and non-channel events are not affected.
;; channel-true or channel-false are allowed to be the samme as channel.
;; Works in both deltaTime mode and absTime mode.
;; .form (split-channel-by-predicate channel predicate channel-true channel-false . messages)
;; .parameter channel The channel in which to split the NoteO messages.
;; .parameter predicate An AST predicate (guarantied to be called on a NoteOn AST).
;; .parameter channel-true The target channel of those NoteOn messages in channel for which the predicate holds.
;; .parameter channel-false The target channel of those NoteOn messages in channel for which the predicate does not hold.
;; .internal-references "Context-sensitive variant" "split-channel-by-contextual-predicate"
;; .misc Intended for splitting the left hand and right hand of a piano piece.
(define split-channel-by-predicate
 (xml-in-laml-positional-abstraction 4 0
  (lambda (channel predicate channel-true channel-false cont attr)
   (split-channel-by-predicate-1 channel predicate channel-true channel-false cont))))

(define (split-channel-by-predicate-1 channel predicate channel-true channel-false message-list)
   (split-channel-by-predicate-2 channel predicate channel-true channel-false message-list '())
)

(define (split-channel-by-predicate-2 channel predicate channel-true channel-false message-list res-list)
   (cond ((null? message-list)  (reverse res-list))
         (else (let ((mes-ast (first message-list)))
                 (if (ast? mes-ast)
                     (if (and (NoteOn? mes-ast) (= channel (midi 'channel mes-ast)))
                         (if (predicate mes-ast)
                             (split-channel-by-predicate-2 channel predicate channel-true channel-false (cdr message-list) 
                                  (cons (copy-ast-mutate-attributes mes-ast 'channel channel-true) res-list))
                             (split-channel-by-predicate-2 channel predicate channel-true channel-false (cdr message-list) 
                                  (cons (copy-ast-mutate-attributes mes-ast 'channel channel-false) res-list)))
                         (split-channel-by-predicate-2 channel predicate channel-true channel-false (cdr message-list) 
                                  (cons  mes-ast res-list)))
                     (split-channel-by-predicate-2 channel predicate channel-true channel-false (cdr message-list) res-list))))))

;; Split the NoteOn events in the given channel to two channels.
;; In contrast to the function split-channel-predicate, the contextual NoteOn events are passed to the predicate in addition to the actual NoteOn midi event.
;; The NoteOn events in channel that satisfy the predicate will be located in channel-true, and those that do not will be located in channel-false.
;; Midi events in other channels, and non-channel events are not affected by this function.
;; channel-true or channel-false are allowed to be the samme as channel.
;; .form (split-channel-by-contextual-predicate channel predicate channel-true channel-false . messages)
;; .parameter channel The channel in which to split the NoteO messages.
;; .parameter predicate A prediate of two parameters: (1) A NoteOn midi message, and (2) an additional NoteOn context list.
;; .parameter channel-true The target channel of those NoteOn messages in channel for which the predicate holds.
;; .parameter channel-false The target channel of those NoteOn messages in channel for which the predicate does not hold.
;; .attribute behind-context implied The number of time ticks of the context behind the current NoteOn message. A non-negative integer. Defaults to 240.
;; .attribute ahead-context implied The number of time ticks of the context ahead of the current NoteOn message. A non-negative integer. Defaults to 240.
;; .mode absTime 
;; .misc Intended for splitting the left hand and right hand of a piano piece.
;; .internal-references "Context-free variant" "split-channel-by-predicate".
(define split-channel-by-contextual-predicate
 (xml-in-laml-positional-abstraction 4 0
  (lambda (channel predicate channel-true channel-false cont attr)
   (let ((behind-context (as-number (defaulted-get-prop 'behind-context attr 240)))
         (ahead-context (as-number (defaulted-get-prop 'ahead-context attr 240))))
     (split-channel-by-contextual-predicate-1 channel predicate channel-true channel-false behind-context ahead-context cont)))))

(define (split-channel-by-contextual-predicate-1 channel predicate channel-true channel-false behind-context ahead-context message-list)
   (split-channel-by-contextual-predicate-2 channel predicate channel-true channel-false behind-context ahead-context message-list '() '())
)

(define (split-channel-by-contextual-predicate-2 channel predicate channel-true channel-false behind-context ahead-context message-list past-list res-list)
   (cond ((null? message-list)  (reverse res-list))
         (else (let ((mes-ast (first message-list)))
                 (if (ast? mes-ast)
                     (if (and (NoteOn? mes-ast) (= channel (midi 'channel mes-ast)))
                         (let* ((tm (as-number (ast-attribute mes-ast 'absTime)))
                                (past-list  (list-prefix-while past-list (lambda (n) (>= (midi 'absTime n) (- tm behind-context)))))
                                (ahead-list (list-prefix-while message-list (lambda (n) (<= (midi 'absTime n) (+ tm ahead-context))))))
                           (display-message (length past-list) (length ahead-list))
                           (if (predicate mes-ast (filter (NoteOnCh? (list channel)) (append (reverse past-list) ahead-list)))
                               (split-channel-by-contextual-predicate-2 channel predicate channel-true channel-false behind-context ahead-context (cdr message-list) 
                                                                        (cons mes-ast past-list) (cons (copy-ast-mutate-attributes mes-ast 'channel channel-true) res-list))
                               (split-channel-by-contextual-predicate-2 channel predicate channel-true channel-false behind-context ahead-context (cdr message-list) 
                                                                        (cons mes-ast past-list) (cons (copy-ast-mutate-attributes mes-ast 'channel channel-false) res-list))))
                         (split-channel-by-contextual-predicate-2 channel predicate channel-true channel-false behind-context ahead-context (cdr message-list) 
                                  (cons mes-ast past-list) (cons  mes-ast res-list)))
                     (split-channel-by-contextual-predicate-2 channel predicate channel-true channel-false behind-context ahead-context 
                                                              (cdr message-list) past-list res-list))))))


;;; .section-id bar-transformations
;;; Midi transformations on given bars.

;; Substitute sections of the midi message-list with sections from section-list.
;; The midi message-list must be in absTime mode and the replacements must be in deltaTime mode. (This is not checked).
;; The affected sections in the midi message-list are identified by bar.
;; Only messages in the given channels are substituted.
;; The inserted sections are replicated to fill the requested number of bars (but they are never divided).
;; .form (substitute-section-by-bar channels section-list . message-list)
;; .parameter channels A list of channels (a list of integers)
;; .parameter section-list A list of sections to be inserted.\
;;    Each section is itself a list, which has the form (bar-number number-of-bars replacement-list).
;; .parameter message-list A list of midi messages.
;; .internal-references "Similar abstime function" "substitute-section-by-time"
(define substitute-section-by-bar
 (xml-in-laml-positional-abstraction 2 0
  (lambda (channels section-list cont attr)
    (substitute-section-by-bar-1 channels section-list cont)))) 

(define (substitute-section-by-bar-1 channels section-list message-list)
  (let ((sorted-section-list
          (sort-list
             section-list
             (lambda (s1 s2) (<= (bar-number-of s1) (bar-number-of s2)))))
       )
    (let* ((section (first sorted-section-list))
           (start-end-repl (by-bar-advancement-substitution section))
          )
      (multi-substitution channels message-list
                         (first start-end-repl) (second start-end-repl) (third start-end-repl) sorted-section-list by-bar-advancement-substitution #f))))

; returns a list of start-point, end-point, and effective replacement-list of section - of replication and bar version.
; holds detailed knowledge of the structure of a section
(define (by-bar-advancement-substitution section)
  (let* ((units-per-bar (* global-ppqn (first global-signature)))
         (bar (bar-number-of section))
         (repl-start-point (* bar units-per-bar))
         (number-of-bars (bar-length-of section))
         (repl-length (* number-of-bars units-per-bar))
         (repl-end-point (+ repl-start-point repl-length))
         (replacement-lst (replicate-if-necessary (replacement-list-of section) (* number-of-bars units-per-bar)))
        )
    (list repl-start-point repl-end-point replacement-lst)))
  

; Invariant: repl-start-point, repl-end-point and replacement-lst correspond to first element of section-insertion-list.
; repl-start-point and repl-end-point is measured in (abs)time units.
; section-advancement-fn is a function that returns a list of start time, end time, and effective replacment (stretched or replicated or...)
; of a given entry of the replacement-list.
(define (multi-substitution channels ml repl-start-point repl-end-point replacement-lst section-insertion-list section-advancement-fn replacing?)
  (cond ((null? ml) '())

        ((and (not replacing?) (not (ast? (car ml)))) 
           (cons (car ml) (multi-substitution channels (cdr ml) repl-start-point repl-end-point replacement-lst section-insertion-list section-advancement-fn replacing?)))

        ((and (not replacing?) (ast? (car ml)))
           (let* ((mes (car ml))
                  (abs-time (as-number (ast-attribute mes 'absTime))))
             (if (and (>= abs-time repl-start-point) (< abs-time repl-end-point))  ; insert replacement-list and enter relacing mode
                 (cons
                   (cons
                    (if (member (as-number (ast-attribute mes 'channel)) channels)
                        (midi-null-event-abs-time (ast-attribute mes 'absTime))
                        mes)
                    replacement-lst   ; delta time events
                   )
                   (multi-substitution channels (cdr ml) repl-start-point repl-end-point replacement-lst section-insertion-list section-advancement-fn #t))
                 (cons mes (multi-substitution channels (cdr ml) repl-start-point repl-end-point replacement-lst section-insertion-list section-advancement-fn replacing?)))))

        ((and replacing? (not (ast? (car ml))))   ; replacing mode - replacement-lst has already been inserted or waiting for it to happen
           (cons (car ml) (multi-substitution channels (cdr ml) repl-start-point repl-end-point replacement-lst section-insertion-list section-advancement-fn replacing?)))

        ((and replacing? (ast? (car ml)))
           (let* ((mes (car ml))
                  (abs-time (as-number (ast-attribute mes 'absTime))))
             (if (<= abs-time repl-end-point)
                 (if (and (member (as-number (ast-attribute mes 'channel)) channels) (equal? (ast-element-name mes) "NoteOn"))
                     (multi-substitution channels (cdr ml) repl-start-point repl-end-point replacement-lst section-insertion-list section-advancement-fn replacing?)   ; removing event
                     (cons mes (multi-substitution channels (cdr ml) repl-start-point repl-end-point replacement-lst section-insertion-list section-advancement-fn replacing?)))

                 ; outside replacing interval - advance replacement-lst:
                 (if (not (null? (cdr section-insertion-list)))  
                     (let* ((next-section (second section-insertion-list))
                            (start-end-repl (section-advancement-fn next-section))
                           )
                       (cons mes (multi-substitution channels (cdr ml) 
                                 (first start-end-repl) (second start-end-repl) (third start-end-repl)
                                 (cdr section-insertion-list) section-advancement-fn #f))) 
                     (cons mes (multi-substitution channels (cdr ml) repl-start-point repl-end-point replacement-lst '() section-advancement-fn #f))))))

        (else (laml-error "multi-substitution: Should not happen"))))



; -----------------------------------------------------------------------------
(define (replicate-if-necessary delta-message-list source-length)
  (let ((mes-lst-lgt (length-of-delta-time-midi-list delta-message-list)))
    (replicate-if-necessary-1 delta-message-list source-length mes-lst-lgt)))

(define (replicate-if-necessary-1 delta-message-list source-length mes-lst-lgt)
  (if (<= mes-lst-lgt source-length)
      (cons delta-message-list (replicate-if-necessary-1 delta-message-list (- source-length mes-lst-lgt) mes-lst-lgt))
      '()))

; -----------------------------------------------------------------------------

(define (length-of-delta-time-midi-list message-list)
  (length-of-delta-time-midi-list-1 message-list 0))

(define (length-of-delta-time-midi-list-1 message-list res)
  (cond ((null? message-list) res)
        ((ast? (car message-list))
           (length-of-delta-time-midi-list-1 (cdr message-list) (+ res (as-number (ast-attribute (car message-list) 'deltaTime)))))
        (else (length-of-delta-time-midi-list-1 (cdr message-list) res))))
  

;; Thin out sections of the midi message-list by means of abs-time predicates.
;; The midi message-list must be in absTime mode.
;; The affected sections in the midi message-list are identified by bar.
;; Only messages in the given channels are thinned out.
;; .form (thin-out-section-by-bar channel section-list . message-list)
;; .parameter channels A list of channels (a list of integers)
;; .parameter section-list A list of sections to be thinned out.\
;;             Each section is itself a list, which has the form (bar-number number-of-bars abs-time-keep-predicate).
;; .parameter message-list A list of midi messages.
;; .reference "Examples" "Midi LAML examples" "../examples/thin-out/index.html"
;; .internal-references "Predicate generator" "keep-beat" 
;; .internal-references "Simpler variant" "thin-out-messages-abs-time"
(define thin-out-section-by-bar
 (xml-in-laml-positional-abstraction 2 0
  (lambda (channels section-list cont attr)
    (thin-out-section-by-bar-1 channels section-list cont)))) 

(define (thin-out-section-by-bar-1 channels section-list message-list)
  (let ((units-per-bar (* global-ppqn (first global-signature)))
        (sorted-section-list
          (sort-list
             section-list
             (lambda (s1 s2) (<= (bar-number-of s1) (bar-number-of s2)))))
       )
    (let* ((section (first sorted-section-list))
           (bar (bar-number-of section))
           (repl-start-point (* bar units-per-bar))
           (number-of-bars (bar-length-of section))
           (repl-length (* number-of-bars units-per-bar))
           (repl-end-point (+ repl-start-point repl-length))
           (keep-pred (keep-predicate-of section))
          )
      (multi-thin-out channels message-list repl-start-point repl-end-point keep-pred  sorted-section-list units-per-bar #f))))

; Invariant: repl-start-point, repl-end-point and keep-pred correspond to first element of section-list
(define (multi-thin-out channels ml repl-start-point repl-end-point keep-pred section-list upb thinning-out?)
  (cond ((null? ml) '())
        ((and (not thinning-out?) (not (ast? (car ml)))) 
           (cons (car ml) (multi-thin-out channels (cdr ml) repl-start-point repl-end-point keep-pred section-list upb thinning-out?)))
        ((and (not thinning-out?) (ast? (car ml)))
           (let* ((mes (car ml))
                  (abs-time (as-number (ast-attribute mes 'absTime)))
                  (delta-time? (ast-attribute mes 'deltaTime #f))     ; for error reporting purposes only.
                 )
             (if delta-time? 
                 (laml-error "thin-out-section-by-bar: Encountered a deltaTime message. Can only be applied in pure absTime mode."))
             (if (and (>= abs-time repl-start-point) (< abs-time repl-end-point))  ; entering thin-out zone
                 (let ((ch (ast-attribute mes 'channel #f))
                      )                
                   (if ch   ; channel message
                       (if (member (as-number ch) channels) ; channel in the channel list channels
                           (if (keep-pred abs-time)   ; retain this mes
                               (cons mes (multi-thin-out channels (cdr ml) repl-start-point repl-end-point keep-pred section-list upb #t))
                               (multi-thin-out channels (cdr ml) repl-start-point repl-end-point keep-pred section-list upb #t))
                       (cons mes (multi-thin-out channels (cdr ml) repl-start-point repl-end-point keep-pred section-list upb #t)))))
                 (cons mes (multi-thin-out channels (cdr ml) repl-start-point repl-end-point keep-pred section-list upb thinning-out?)))))

        ((and thinning-out? (not (ast? (car ml))))
           (cons (car ml) (multi-thin-out channels (cdr ml) repl-start-point repl-end-point keep-pred section-list upb thinning-out?)))

        ((and thinning-out? (ast? (car ml)))
           (let* ((mes (car ml))
                  (abs-time (as-number (ast-attribute mes 'absTime))))
             (if (<= abs-time repl-end-point)
                 (if (and (member (as-number (ast-attribute mes 'channel)) channels) (equal? (ast-element-name mes) "NoteOn"))
                     (if (keep-pred abs-time) ; retain this mes
                         (cons mes (multi-thin-out channels (cdr ml) repl-start-point repl-end-point keep-pred section-list upb #t))
                         (multi-thin-out channels (cdr ml) repl-start-point repl-end-point keep-pred section-list upb #t))
                     (cons mes (multi-thin-out channels (cdr ml) repl-start-point repl-end-point keep-pred section-list upb thinning-out?)))

                 ; now again outside thin-out zone:
                 (if (not (null? (cdr section-list)))  
                     (let* ((next-section (second section-list))
                            (next-bar (bar-number-of next-section))
                            (next-repl-start-point (* next-bar upb))
                            (next-number-of-bars (bar-length-of next-section))
                            (next-repl-length (* next-number-of-bars upb))
                            (next-repl-end-point (+ next-repl-start-point next-repl-length))
                            (next-keep-pred (keep-predicate-of next-section)))
                       (cons mes (multi-thin-out channels (cdr ml) next-repl-start-point next-repl-end-point next-keep-pred (cdr section-list) upb #f))) 
                     (cons mes (multi-thin-out channels (cdr ml) repl-start-point repl-end-point keep-pred '() upb #f))))))

        (else (laml-error "multi-thin-out: Should not happen"))))


;; Scale the velocity of NoteOn messages, in selected channels, in given bars.
;; The midi message-list must be in absTime mode (not checked).
;; The affected sections in the midi message-list are identified by bar.
;; Only messages in the given channels are affected
;; .form (scale-velocity-of-sections-by-bar channels section-list . message-list)
;; .parameter channels A list of channels (a list of integers)
;; .parameter section-list A list of sections in which to scale the velocity.\
;;             Each section is itself a list, which has the form (bar-number number-of-bars scaling-function).
;; .attribute min-velocity  implied  The smallest possible velocity. Defaults to 0. Serves as a lower limit cut off value.
;; .attribute max-velocity  implied  The largest possible velocity. Defaults to 127. Serves as an upper limit cut off value.
;; .parameter message-list A list of midi messages.
;; .reference "Examples" "Midi LAML examples" "../examples/velocity-scaling/index.html"
;; .internal-references "Similar time function" "scale-velocity-of-sections-by-time"
(define scale-velocity-of-sections-by-bar
 (xml-in-laml-positional-abstraction 2 0
  (lambda (channels section-list cont attr)
   (let ((min-vel (as-number (defaulted-get-prop 'min-velocity attr "0")))
         (max-vel (as-number (defaulted-get-prop 'max-velocity attr "127")))) 
    (scale-velocity-of-sections-by-bar-1 channels section-list min-vel max-vel cont)))))

(define (scale-velocity-of-sections-by-bar-1 channels section-list min-vel max-vel message-list)
  (let ((sorted-section-list
          (sort-list
             section-list
             (lambda (s1 s2) (<= (bar-number-of s1) (bar-number-of s2)))))
       )
    (let* ((section (first sorted-section-list))
           (start-end-sf (by-bar-advancement-velocity-scaling section))
          )
      (multi-scale-velocity channels message-list min-vel max-vel (first start-end-sf) (second start-end-sf) (third start-end-sf)
                            sorted-section-list by-bar-advancement-velocity-scaling #f 0 0))))

(define (by-bar-advancement-velocity-scaling section)
  (let* ((units-per-bar (* global-ppqn (first global-signature)))
         (bar (bar-number-of section))
         (repl-start-point (* bar units-per-bar))
         (number-of-bars (bar-length-of section))
         (repl-length (* number-of-bars units-per-bar))
         (repl-end-point (+ repl-start-point repl-length))
         (scaling-fu (scaling-function-of section))
        )
    (list repl-start-point repl-end-point scaling-fu)))

; Invariant: repl-start-point, repl-end-point and scaling-fu correspond to first element of section-list
; nss is the number of scaling steps, and i is the actual scaling step (progressing from 1 to nss). 
(define (multi-scale-velocity channels ml min-vel max-vel repl-start-point repl-end-point scaling-fu section-list by-bar-advancement-velocity-scaling scaling? nss i)
  (cond ((null? ml) '())
        ((and (not scaling?) (not (ast? (car ml))))
           (cons (car ml) (multi-scale-velocity channels (cdr ml) min-vel max-vel repl-start-point
                                                repl-end-point scaling-fu section-list by-bar-advancement-velocity-scaling scaling? nss i)))
        ((and (not scaling?) (ast? (car ml)))
           (display "X")
           (let* ((mes (car ml))
                  (abs-time (as-number (ast-attribute mes 'absTime)))
                  (delta-time? (ast-attribute mes 'deltaTime #f)) ; for error reporting purposes only.
                 )
             (if delta-time? 
                 (laml-error "scale-velocity-of-sections-by-bar: Encountered a deltaTime message."))          
             (if (and (>= abs-time repl-start-point) (< abs-time repl-end-point))  ; entering scaling zone
                 (let ((ch (ast-attribute mes 'channel #f))
                      )
                   (if ch   ; channel message
                       (let ((scaling-steps (find-number-of-scaling-steps-in ml channels repl-end-point)))
                         (if (member (as-number ch) channels) ; channel in the channel list channels
                             (cons (scale-message mes scaling-fu scaling-steps 1 min-vel max-vel)
                                   (multi-scale-velocity channels (cdr ml) min-vel max-vel repl-start-point
                                                         repl-end-point scaling-fu section-list by-bar-advancement-velocity-scaling #t scaling-steps 2))
                             (cons mes (multi-scale-velocity channels (cdr ml) min-vel max-vel repl-start-point
                                                             repl-end-point scaling-fu section-list by-bar-advancement-velocity-scaling #t scaling-steps 1)) 
                           ))
                       (let ((scaling-steps (find-number-of-scaling-steps-in ml channels repl-end-point)))
                         (cons mes (multi-scale-velocity channels (cdr ml) min-vel max-vel repl-start-point
                                                           repl-end-point scaling-fu section-list by-bar-advancement-velocity-scaling #t scaling-steps 1)))))
                 (cons mes (multi-scale-velocity channels (cdr ml) min-vel max-vel repl-start-point repl-end-point
                                                 scaling-fu section-list by-bar-advancement-velocity-scaling scaling? nss i)))))

        ((and scaling? (not (ast? (car ml))))
           (cons (car ml) (multi-scale-velocity channels (cdr ml) min-vel max-vel repl-start-point repl-end-point
                                                scaling-fu section-list by-bar-advancement-velocity-scaling scaling? nss i)))

        ((and scaling? (ast? (car ml)))
           (let* ((mes (car ml))
                  (abs-time (as-number (ast-attribute mes 'absTime))))
             (if (<= abs-time repl-end-point)
                 (begin
                    (display-message "W" nss i)
                 (if (and (member (as-number (ast-attribute mes 'channel)) channels) (equal? (ast-element-name mes) "NoteOn"))
                     (cons (scale-message mes scaling-fu nss i min-vel max-vel)
                           (multi-scale-velocity channels (cdr ml) min-vel max-vel repl-start-point
                                                 repl-end-point scaling-fu section-list by-bar-advancement-velocity-scaling #t nss (+ i 1)))
                     (cons mes (multi-scale-velocity channels (cdr ml) min-vel max-vel repl-start-point repl-end-point
                                                     scaling-fu section-list by-bar-advancement-velocity-scaling scaling? nss i))))

                 ; now again outside thin-out zone:
                 (begin
                    (display "V")

                 (if (not (null? (cdr section-list)))  
                     (let* ((next-section (second section-list))
                            (start-end-sf (by-bar-advancement-velocity-scaling next-section))
                           )
                       (cons mes (multi-scale-velocity channels (cdr ml) min-vel max-vel (first start-end-sf) (second start-end-sf) (third start-end-sf)
                                                       (cdr section-list) by-bar-advancement-velocity-scaling #f 0 0))) 
                     (cons mes (multi-scale-velocity channels (cdr ml) min-vel max-vel repl-start-point repl-end-point
                                                     scaling-fu '() by-bar-advancement-velocity-scaling #f 0 0)))))))

        (else (laml-error "multi-scale-velocity: Should not happen"))))

(define (find-number-of-scaling-steps-in message-list channels time-limit)
  (find-number-of-scaling-steps-in-1 message-list channels time-limit 0))

; Find the number of NoteOn messages in channels before time-limit
(define (find-number-of-scaling-steps-in-1 message-list channels time-limit count)
  (cond ((null? message-list) count)
        ((not (ast? (car message-list))) 
           (find-number-of-scaling-steps-in-1 (cdr message-list) channels time-limit count))
        ((> (as-number (ast-attribute (car message-list) 'absTime)) time-limit) count)
        ((and (equal? (ast-element-name (car message-list)) "NoteOn")
              (ast-attribute (car message-list) 'channel #f) 
              (member (as-number (ast-attribute (car message-list) 'channel)) channels))
           (find-number-of-scaling-steps-in-1 (cdr message-list) channels time-limit (+ count 1)))
        (else 
           (find-number-of-scaling-steps-in-1 (cdr message-list) channels time-limit count))))


(define (scale-message noteon-ast scaling-fu scaling-steps i min-vel max-vel)
  (display-message i scaling-steps)
  (let* ((old-velocity (as-number (ast-attribute noteon-ast 'velocity)))
         (new-velocity (between min-vel max-vel
                               (+ min-vel (* (- old-velocity min-vel) (scaling-fu (/ i scaling-steps)))))))
    (copy-ast-mutate-attributes noteon-ast 'velocity (as-int-string new-velocity))))



;; Envelope sections of the midi message-list with pieces from section-list.
;; The midi message-list must be in absTime mode and pieces from the section-list must be in deltaTime mode. (This is not checked).
;; The affected sections in the midi message-list are identified by bar.
;; .form (envelope-section-by-bar section-list . message-list)
;; .parameter section-list A list of sections to be inserted. \
;;    Each section is itself a list, which has the form (bar-number number-of-bars pre-envelope-list post-envelope-list).\
;;    The deltaTime messages in pre-envelope-list is inserted at the beginning of the section.\
;;    The deltaTime messages in post-envelope-list is inserted at the beginning of the section.\
;; .parameter message-list A list of midi messages.
(define envelope-sections-by-bar
 (xml-in-laml-positional-abstraction 1 0
  (lambda (section-list cont attr)
    (envelope-sections-by-bar-1 section-list cont)))) 

(define (envelope-sections-by-bar-1 section-list message-list)
  (let ((units-per-bar (* global-ppqn (first global-signature)))
        (sorted-section-list
          (sort-list
             section-list
             (lambda (s1 s2) (<= (bar-number-of s1) (bar-number-of s2)))))
       )
    (let* ((section (first sorted-section-list))
           (bar (bar-number-of section))
           (repl-start-point (* bar units-per-bar))
           (number-of-bars (bar-length-of section))
           (repl-length (* number-of-bars units-per-bar))
           (repl-end-point (+ repl-start-point repl-length))
           (pre-envelope-list (pre-envelope-of section))
           (post-envelope-list (post-envelope-of section))
           (post-envelope-list-length (total-length-of-message-list post-envelope-list))
          )
      (multi-enveloping message-list repl-start-point repl-end-point pre-envelope-list post-envelope-list post-envelope-list-length
                        sorted-section-list units-per-bar #f #f))))

; Invariant: repl-start-point, repl-end-point and replacement-lst correspond to first element of section-insertion-list
(define (multi-enveloping ml repl-start-point repl-end-point pre-envelope-list post-envelope-list
                          post-envelope-length  section-envelope-list upb enveloping? post-env-inserted?)
  (cond ((null? ml) '())
        ((and (not enveloping?) (not (ast? (car ml)))) 
           (cons (car ml) 
                 (multi-enveloping (cdr ml) repl-start-point repl-end-point pre-envelope-list post-envelope-list
                                   post-envelope-length  section-envelope-list upb enveloping? post-env-inserted? )))

        ((and (not enveloping?) (ast? (car ml)))
           (let* ((mes (car ml))
                  (abs-time (as-number (ast-attribute mes 'absTime))))
             (if (and (>= abs-time repl-start-point) (< abs-time repl-end-point))  ; insert the pre-envelope here
                 (append 
                    pre-envelope-list
                    (cons
                      mes
                      (multi-enveloping (cdr ml) repl-start-point repl-end-point pre-envelope-list post-envelope-list
                                        post-envelope-length  section-envelope-list upb #t #f)))
                 (cons mes (multi-enveloping (cdr ml) repl-start-point repl-end-point pre-envelope-list post-envelope-list
                                             post-envelope-length  section-envelope-list upb enveloping? post-env-inserted?)))))

        ((and enveloping? (not (ast? (car ml))))
           (cons (car ml) (multi-enveloping (cdr ml) repl-start-point repl-end-point pre-envelope-list
                                            post-envelope-list post-envelope-length  section-envelope-list upb enveloping? 
                                            post-env-inserted?)))

        ((and enveloping? (ast? (car ml)))
           (let* ((mes (car ml))
                  (abs-time (as-number (ast-attribute mes 'absTime))))
             (if (<= abs-time repl-end-point)
                 (if (and (>= abs-time (- repl-end-point post-envelope-length)) (not post-env-inserted?))
                     (append 
                        post-envelope-list
                        (cons
                         mes
                         (multi-enveloping (cdr ml) repl-start-point repl-end-point pre-envelope-list post-envelope-list
                                           post-envelope-length  section-envelope-list upb #t #t)))
                     (cons mes (multi-enveloping (cdr ml) repl-start-point repl-end-point pre-envelope-list 
                                                 post-envelope-list post-envelope-length  section-envelope-list upb enveloping? 
                                                 post-env-inserted?)) )

                 ; outside enveloping interval:
                 (if (not (null? (cdr section-envelope-list)))  
                     (let* ((next-section (second section-envelope-list))
                            (next-bar (bar-number-of next-section))
                            (next-repl-start-point (* next-bar upb))
                            (next-number-of-bars (bar-length-of next-section))
                            (next-repl-length (* next-number-of-bars upb))
                            (next-repl-end-point (+ next-repl-start-point next-repl-length))
                            (pre-envelope-list (pre-envelope-of next-section))
                            (post-envelope-list (post-envelope-of next-section))
                            (post-envelope-list-length (total-length-of-message-list post-envelope-list))
                           )
                       (cons mes 
                             (multi-enveloping (cdr ml) next-repl-start-point next-repl-end-point pre-envelope-list post-envelope-list
                                              post-envelope-length (cdr section-envelope-list) upb #f #f))) 
                     (cons mes (multi-enveloping (cdr ml) repl-start-point repl-end-point pre-envelope-list 
                                                 post-envelope-list post-envelope-length  '() upb #f #f))))))

        (else (laml-error "multi-enveloping: Should not happen"))))
  

; Selectors of section descriptions - kind of overloaded.
(define bar-number-of (make-selector-function 1 "bar-number-of"))
(define bar-length-of (make-selector-function 2 "bar-length-of"))
(define start-time-of (make-selector-function 1 "start-time-of"))
(define end-time-of (make-selector-function 2 "end-time-of"))
(define replacement-list-of (make-selector-function 3 "replacement-list-of"))
(define keep-predicate-of (make-selector-function 3 "keep-predicate-of"))
(define scaling-function-of (make-selector-function 3 "scaling-function-of"))
(define pre-envelope-of (make-selector-function 3 "pre-envelope-of"))
(define post-envelope-of (make-selector-function 4 "post-envelope-of"))


; ---------------------------------------------------------------------------------------------------------------

;;; .section-id 
;;; Midi transformations on sections.
;;; The functions in this section are similar to the functions in the previous sections.
;;; The main difference is that the functions in this section work on sections, identified 
;;; by absolute time ticks, not bar numbers. The function time-of-marker allows for identification 
;;; of sections by markers.

;; Substitute sections of the midi message-list with sections from section-list.
;; The midi message-list must be in absTime mode and the replacements must be in deltaTime mode. (This is not checked).
;; The affected sections in the midi message-list are identified by time.
;; Only messages in the given channels are substituted.
;; The inserted sections are stretched to fill the requested number of bars.
;; .form (substitute-section-by-time channels section-list . message-list)
;; .parameter channels A list of channels (a list of integers).
;; .parameter section-list A list of sections to be inserted.\
;;    Each section is itself a list, which has the form (time-start time-end replacement-list).
;; .parameter message-list A list of midi messages.
;; .internal-references "Similar bar function" "substitute-section-by-bar"
;; .misc Compared with substitute-section-by-bar this function identifies regions with absolute times, not bar number and bar length.
;;   In addition it stretches the replacement-list to fit the given time interval [start-time - end-time]. It does not replicate the replacement list.
(define substitute-section-by-time
 (xml-in-laml-positional-abstraction 2 0
  (lambda (channels section-list cont attr)
    (substitute-section-by-time-1 channels section-list cont)))) 

; Used by substitute-section-by-time-1 to access the message-list on which the substitution is being performed
(define contextual-message-list '())

(define (substitute-section-by-time-1 channels section-list message-list)
  (set! contextual-message-list message-list)
  (let ((units-per-bar (* global-ppqn (first global-signature)))
        (sorted-section-list
          (sort-list
             section-list
             (lambda (s1 s2) (<= (bar-number-of s1) (bar-number-of s2)))))
       )
    (let* ((section (first sorted-section-list))
           (start-end-repl (by-time-advancement-substitution section))
          )
      (multi-substitution channels message-list
                         (first start-end-repl) (second start-end-repl) (third start-end-repl)
                         sorted-section-list by-time-advancement-substitution #f))))

; Returns a list of start-point, end-point, and effective replacement-list of section - of stretching and given time version.
; holds detailed knowledge of the structure of a section
(define (by-time-advancement-substitution section)
  (let* ((repl-start-point (start-time-of section))
         (repl-end-point (end-time-of section))
         (replacement-lst (stretch-if-necessary (replacement-list-of section) (- repl-end-point repl-start-point)))
        )
    (list repl-start-point repl-end-point replacement-lst)))

(define (stretch-if-necessary delta-message-list to-length)
  (let* ((mes-lst-lgt (length-of-delta-time-midi-list delta-message-list))
         (stretch-factor (/ to-length mes-lst-lgt)))
    (time-stretch-1 stretch-factor delta-message-list)))


;; Scale the velocity of NoteOn messages, in selected channels, in given regions of time.
;; The midi message-list must be in absTime mode (not checked).
;; The affected sections in the midi message-list are identified by time intervals.
;; Only messages in the given channels are affected.
;; .form (scale-velocity-of-sections-by-time channels section-list . message-list)
;; .parameter channels A list of channels (a list of integers)
;; .parameter section-list A list of sections in which to scale the velocity.\
;;             Each section is itself a list, which has the form (start-time end-time scaling-function).
;; .attribute min-velocity  implied  The smallest possible velocity. Defaults to 0. Serves as a lower limit cut off value.
;; .attribute max-velocity  implied  The largest possible velocity. Defaults to 127. Serves as an upper limit cut off value.
;; .parameter message-list A list of midi messages.
;; .reference "Examples" "Midi LAML examples" "../examples/velocity-scaling/index.html"
;; .internal-references "Similar bar function" "scale-velocity-of-sections-by-bar"
(define scale-velocity-of-sections-by-time
 (xml-in-laml-positional-abstraction 2 0
  (lambda (channels section-list cont attr)
   (let ((min-vel (as-number (defaulted-get-prop 'min-velocity attr "0")))
         (max-vel (as-number (defaulted-get-prop 'max-velocity attr "127"))))
    (scale-velocity-of-sections-by-time-1 channels section-list min-vel max-vel cont)))))

(define (scale-velocity-of-sections-by-time-1 channels section-list min-vel max-vel message-list)
  (set! contextual-message-list message-list)
  (let ((sorted-section-list
          (sort-list
             section-list
             (lambda (s1 s2) (<= (bar-number-of s1) (bar-number-of s2)))))
       )
    (let* ((section (first sorted-section-list))
           (start-end-sf (by-time-advancement-velocity-scaling section))
          )
      (multi-scale-velocity channels message-list min-vel max-vel (first start-end-sf) (second start-end-sf) (third start-end-sf)
                            sorted-section-list by-time-advancement-velocity-scaling #f 0 0))))

(define (by-time-advancement-velocity-scaling section)
  (let* ((repl-start-point (start-time-of section))
         (repl-end-point (end-time-of section))
         (scaling-fu (scaling-function-of section))
        )
    (list repl-start-point repl-end-point scaling-fu)))


;; Find the absTime of marker with name marker-name, such as "M-1", in message-list.
;; message-list defaults to the value of the global variable contextual-message-list assigned by substitute-section-by-time.
;; The problem is, however, that contextual-message-list is not assigned before it typically is used. This problem has not been settled yet.
;; Assume, as a precondition, that message-list is in absTime mode.
;; .form (time-of-marker marker-name [message-list])
;; .parameter marker-name The name of a meta marker (a string). Example: "M-1".
;; .parameter message-list A list of midi messages. Defaults to the value of the global variable contextual-message-list.
;; .returns The absTime of the located meta midi message, of #f if the marker cannot be located in the message list. 
(define (time-of-marker marker-name . optional-parameter-list)
  (let ((message-lst (optional-parameter 1 optional-parameter-list contextual-message-list)))
    (let* ((marker-lgt (string-length marker-name))
           (res-mes (find-in-list 
                     (lambda (mes) 
                       (and (Meta? mes 6)
                            (let ((meta-txt (ast-text mes)))
                              (and (>= (string-length meta-txt) marker-lgt)
                                   (equal? (substring meta-txt 0 marker-lgt) marker-name)))))
                     message-lst)))
      (if res-mes
          (ast-attribute res-mes 'absTime)
          (laml-error "Cannot find marker" marker-name (length message-lst))))))


;;; .section-id guitar-beats
;;; Generation of note phrases.
;;; The functions in this section generate - or help generate - list of notes.
;;; The most sophisticated is the function beat, which - typically, but not necessarily - generates guitar beats.
;;; The instrument definition is outside the context of the beat function.
;;; The generated beats are affected by a large number of parameters.
;;; The function duration-to-next is a function which makes it possible to express the duration contextually.
;;; LAML technically, duration-to-next, is an attribute-returning delayed procedural content item function.

;; Return a list of n deltaTimed notes, of note value note-value, all in channel ch, with a mutual distance of distance.
;; .parameter n The number of NoteOn messages to be generated.
;; .parameter ch The channel of the NoteOn messages. An integer between 1 and 16.
;; .parameter note-value the value of the note attribute. An integer between 0 and 127.
;; .parameter duration The deltaTime of each NoteOn message.
(define (regular-beats n ch note-value distance)
  (map
    (lambda (n) (NoteOn 'deltaTime distance 'channel ch 'note note-value 'velocity 127 'duration 100))
    (number-interval 1 n)))

;; Return a guitar beat chord in channel ch with a number notes, with an enforced total length (duration) of total-length.
;; Direction is one of up or down (symbols or strings).
;; The notes and their deltaTimes is per default fixed to a C-Major chord of six notes, but it can be customized to an arbitrary sequence via the parameter time-note-list.
;; Each NoteOn sounds until it is activated later on in the contextual sequence, or with a duration of base-duration if it is last in the contextual sequence.
;; Transpose with the value of transpose.
;; Stretch with a factor of stretch (normally between 0 and 1).
;; The velocity of each NoteOn event is, in the starting point, base-velocity (scaling is pending).
;; The deltaTime of all but the first NoteOn event is base-delta-time (scaling is pending). The deltaTime of the first NoteOn is 0.
;; Scale velocities with velocity-scaling-fn.
;; Scale delta-times with delta-time-scaling-fn.
;; .form (beat direction stretch base-velocity total-length velocity-scaling-fn delta-time-scaling-fn [transposition ch base-duration time-note-list])
;; .parameter direction Either up or down (symbol or strings). If down, time-note-list is reversed.
;; .parameter stretch Stretch-factor (typically, but not necessarity, a real number between 0 and 1).
;; .parameter base-velocity The initial velocity before scaling with velocity-scaling-fn. An integer between 0 and 127.
;; .parameter total-length The enforced total-length of the beat. An non-negative integer.
;; .parameter velocity-scaling-fn A scaling function which is used to scale the base-velocity throughout the six notes.
;; .parameter delta-time-scaling-fn A scaling function which is used to scale the base-delta-time throughout the six notes.
;; .parameter transposition A transposition of the notes in the beat. A integer (positive or negative). Defaults to 0.
;; .parameter ch The channel in which to which the NoteOn in the beat belongs. Defaults to channel 1. An integer between 1 and 16.
;; .parameter base-duration The duration of all NoteOn which are not followed by a similar NoteOn in the same channel and with the same note value attribute. Defaults to 960.
;; .parameter time-note-list A list of deltaTime noteValue pairs which defines the basis beat of the chord.\ 
;;                           Each entry is of the form (deltaTime noteValue base-velocity) where deltaTime is non-negative integer and\
;;                           noteValue is a note number, or a symbol or string which can be transformed to a note number by the function note-name-to-note-number.\
;;                           base-velocity is optional, and if present it overrides the base-velocity (the third parameter of the function).
;;                           noteValue may also the symbol -, in which case the note is skipped (in the sense that the note is substituted by a mill-null-event).
;;                           time-note-list defaults to a C-major chord sequence of six notes. The first deltaTime is forced to be 0, independent of its given value.
;; .reference "Examples" "Midi LAML examples" "../examples/beat/index.html"
(define (beat direction stretch base-velocity total-length velocity-scaling-fn delta-time-scaling-fn . optional-parameter-list)
 (let ((transposition (optional-parameter 1 optional-parameter-list 0))
       (ch (optional-parameter 2 optional-parameter-list 1))
       (base-duration (optional-parameter 3 optional-parameter-list 960))
       (time-note-list (optional-parameter 4 optional-parameter-list '((240 C2) (240 E2) (240 G2) (240 B2) (240 C3) (240 E3))))
      )
  (transpose-channels (list ch) transposition
    (let* ((directional-time-note-list (if (eq? direction 'down) (reverse time-note-list) time-note-list))
           (notes
            (scale-attribute-1 'deltaTime delta-time-scaling-fn
             (scale-attribute-1 'velocity velocity-scaling-fn
              (time-stretch stretch
               (cons
                 (let* ((t 0)   ; First - forced zero deltaTime
                        (nv (second (first directional-time-note-list)))
                        (n-velocity (third-else (first directional-time-note-list) base-velocity))
                        (nn (cond ((eq? nv '-) #f) ((number? nv) nv) (else (note-name-to-note-number nv))))
                       )
                   (if (eq? nn #f)
                       (midi-null-event-delta-time t "Dropped note")
                       (NoteOn 'deltaTime t 'channel ch 'note nn 'velocity n-velocity (duration-to-next base-duration))))
                 (map                     ; Rest:
                  (lambda (t-nv)
                    (let* ((t (first t-nv))
                           (nv (second t-nv))
                           (n-velocity (third-else t-nv base-velocity))
                           (nn (cond ((eq? nv '-) #f) ((number? nv) nv) (else (note-name-to-note-number nv))))
                          )
                      (if (eq? nn #f)
                          (midi-null-event-delta-time t "Dropped note")
                          (NoteOn 'deltaTime t 'channel ch 'note nn 'velocity n-velocity (duration-to-next base-duration)))))
                  (cdr directional-time-note-list)))
              ))))
            (note-lgt (accumulate-right + 0 (map (lambda (ast) (as-number (ast-attribute ast 'deltaTime))) notes)))
           )
      (if (> note-lgt total-length) (laml-error "Stretched NoteOn sequence of length" note-lgt "does not fit in an interval of length" total-length))
      (list
        (midi-comment (if (eq? direction 'down) "Downwards:" "Upwards:"))
        notes
        (midi-null-event-delta-time (- total-length note-lgt) (string-append "Filling to total-length " (as-string total-length)))
        (midi-comment (if (eq? direction 'down) "End downwards." "End upwards."))
      )))))

(define (third-else lst default)
  (if (>= (length lst) 3) (third lst) default))


(define (add-together-delta-times-until ast-list stop-ast)
  (cond ((null? ast-list) 0)
        ((not (ast? (car ast-list))) (add-together-delta-times-until (cdr ast-list) stop-ast))
        ((eq? (car ast-list) stop-ast) (as-number (ast-attribute stop-ast 'deltaTime)) ) ; thus including deltaTime of stop-ast. Maybe not correct?
        (else (+ (as-number (ast-attribute (car ast-list) 'deltaTime)) (add-together-delta-times-until (cdr ast-list) stop-ast)))))

;; Return a functions, which (when called) calculates a duration attribute value pair (a list of two elements) of the deltaTime length 
;; from the hosting NoteOn to the next NoteOn with same channel and the same note value.
;; Serves as a delayed procedural content item of a NoteOn MIDI event.
;; .parameter default-duration The default duration, used in case the duration between neighbor NoteOn events cannot be found.
;; .returs A function, serving as a LAML delayed procedural content item, which accepts the root AST and the parent AST of the hosting element. 
(define (duration-to-next default-duration)
 (lambda (root-ast note-ast)
  (let* ((track (find-first-ast root-ast "MidiTrack"))
         (events-in-track (ast-subtrees track))
         (note-value (ast-attribute note-ast 'note -1))
         (channel (ast-attribute note-ast 'channel -1))
         (events-from-note-ast (find-tail-in-list (lambda (el) (eq? el note-ast)) events-in-track))     ; events after note-ast
         (events-after-note-ast (if (not (null? events-from-note-ast)) (cdr events-from-note-ast) '())) ; tail of
         (next-similar-note-ast (find-in-list 
                                  (lambda (n-ast)
                                    (and (NoteOn? n-ast) 
                                         (equal? note-value (ast-attribute n-ast 'note))
                                         (equal? channel (ast-attribute n-ast 'channel))
                                    ))
                                  events-after-note-ast))
         (dur (if next-similar-note-ast (add-together-delta-times-until events-after-note-ast next-similar-note-ast) default-duration))  
        )
    (list 'duration (max 0 dur)))))

; ----------------------------------------------------------------------------------------------------------------------------------------------------
; Mega voice maps:

; The functions in this section are used to encapsulate the details of a mega voice map.
; A mega voice map is a list of mega voice entries.
; A mega voice entry is a list of 
;   (mega-voice-section-name min-note max-note min-velocity max-melocity)
; A mega voice function, defined relative to a mega voice map, maps
;    note-name' section-name velocity'
; to
;    note-number velocity
; where note-name' is an extended note name, section-name is a name of mega voice section, and velocity' is a normal velocity in the interval [1 .. 127].
; note-name' examples:
;   C2   mapped by note-name-to-note-number
;   c2   mapped such that c0 is the minimum note in its section. 
; The given velocity is mapped to the interval which is characteristic of the mega voice section.

;; Generates a mega-voice-function from a given mega voice map.
;; A mega voice function maps a note-name (with special conventions), a mega voice section name, and a normal velocity 
;; to the note value and the mega-voice-specific velocity.
(define (generate-mega-voice-function mega-voice-map)
  (lambda (note-name section-name velocity)
    (let* ((note-name-str (as-string note-name))
           (relative-note-name? (member (as-number (string-ref note-name-str 0)) lower-case-interval))
           (min-note-mvm (min-note-of-mega-voice-map section-name mega-voice-map)) 
           (max-note-mvm (max-note-of-mega-voice-map section-name mega-voice-map)) 
           (min-vel-mvm (min-velocity-of-mega-voice-map section-name mega-voice-map)) 
           (max-vel-vmv (max-velocity-of-mega-voice-map section-name mega-voice-map))
          )
      (list
         (if relative-note-name?
             (mv-relative-to-absolute-note-number (- (note-name-to-note-number note-name) 24) min-note-mvm max-note-mvm) 
             (between min-note-mvm max-note-mvm (note-name-to-note-number note-name)))
         (mv-scale-velocity velocity min-vel-mvm max-vel-vmv)))))

; The interval of the lower case letters
(define lower-case-interval (number-interval 97 122))

(define (min-note-of-mega-voice-map section-name mega-voice-map)
  (let ((section (find-in-list (lambda (sec) (equal? (as-string section-name) (as-string (first sec)))) mega-voice-map)))
    (if section (second section) (laml-error "min-note-of-mega-voice-map: Unknown section" section-name))))

(define (max-note-of-mega-voice-map section-name mega-voice-map)
  (let ((section (find-in-list (lambda (sec) (equal? (as-string section-name) (as-string (first sec)))) mega-voice-map)))
    (if section (third section) (laml-error "max-note-of-mega-voice-map: Unknown section" section-name))))

(define (min-velocity-of-mega-voice-map section-name mega-voice-map)
  (let ((section (find-in-list (lambda (sec) (equal? (as-string section-name) (as-string (first sec)))) mega-voice-map)))
    (if section (fourth section) (laml-error "min-velocity-of-mega-voice-map: Unknown section" section-name))))

(define (max-velocity-of-mega-voice-map section-name mega-voice-map)
  (let ((section (find-in-list (lambda (sec) (equal? (as-string section-name) (as-string (first sec)))) mega-voice-map)))
    (if section (fifth section) (laml-error "max-velocity-of-mega-voice-map: Unknown section" section-name))))

; Scale velocity and displace it in the interval from min-vel to max-vel.
(define (mv-scale-velocity velocity min-vel max-vel)
  (to-int (+ min-vel (* (/ (- max-vel min-vel) 127) (- velocity 1)))))

; Displace rel-note-number to the interval from min-note to max-note.
(define (mv-relative-to-absolute-note-number rel-note-number min-note max-note)
  (let ((result (+ rel-note-number min-note)))
    (if (<= result max-note)
        result
        max-note)))

;; The mega voice map of a steel guitar (tyros 1).
(define steel-guitar-megavoice-map
  (list
    (list 'harmonics 0 95 121 127) (list 'slide 0 95 106 120) (list 'hammer 0 95 91 105) (list 'mute 0 95 76 90) (list 'dead 0 95 61 75) 
    (list 'open-hard 0 95 41 60) (list 'open-medium 0 95 21 40) (list 'open-soft 0 95 1 20)
    (list 'strum-noice 96 119 1 127) (list 'fret-noice 120 127 1 127)))

; ----------------------------------------------------------------------------------------------------------------------------------------------------

;;; .section-id style-splitting
;;; Style Splitting. The functions in this section split a style file in its midi path.
;;; In this context, a style file is a Yamaha Keyboard style file which control the automatic accompaniment.
;;; The first functions are older versions. The refined functions are more advanced.
;;; The refined functions are able to extract meta information about the midi contents of the pieces.
;;; Some levels of bulk processing is provided.

;; Split a given style file in midi pieces, and generate (up to) 15 different 
;; midi files of the parts. Place the midi pieces in a subdirectory of output-dir-path.
;; Only output channels in channel-selection (a list of integers).
;; .parameter style-file-path The full file path to a style file.
;; .parameter output-dir-path The full file path to a an existiing output directory.
;; .parameter mode Midi processing mode. A symbol. Must be deltaTime.
;; .parameter channel-selection A list of channel numbers, in the interval 1..16. A list of integers.
;; .internal-references "Bulk version" "split-and-process-all-styles"
;; .internal-references "Refined version" "split-and-process-style-one-channel-refined"
;; .misc Older version. Use the newer and refined variant. Does probably not work correctly any more.
(define (split-and-process-style style-file-path output-dir-path mode channel-selection)
  (let* ((target-dir (file-name-proper (file-name-proper style-file-path)))
         (midi-ast (midi-file-to-laml-ast style-file-path mode 0 #f #f))
         (midi-header (ast-subtree midi-ast "MidiHeader"))
         (track-ast (ast-subtree midi-ast "MidiTrack"))  ; format 0 - thus a single track
         (track-events (ast-subtrees track-ast))         ; all midi events in this track 

         (track-meta-divisions (filter meta-division-event? track-events))   ; a list of type 6 meta events
         (track-meta-division-names (map ast-text track-meta-divisions))     ; a list of division names, taken from type 6 meta events

         (init-stuff (midi-event-ast-subsequence track-events #t (third track-meta-division-names)))   ; initial stuff - to be in all sections.

         (section-list 
           (map
            (lambda (from to)
              (midi-event-ast-subsequence track-events from to))
            (cddr track-meta-division-names)
            (append (cdddr track-meta-division-names) (list #t))))

         (section-name-list (map no-spaces-in-string (cddr track-meta-division-names)))
         (end-of-track-event (Meta 'deltaTime "0" 'type "47" "")))

    (ensure-directory-existence! output-dir-path target-dir)  

    (for-each 
     (lambda (section section-name)
       (write-text-file
        (standard-midi-file-ast-to-bin
         (StandardMidiFile 'internal:run-action-procedure "false"
                           midi-header
                           (MidiTrack 
                            init-stuff
                            (select-channels channel-selection section)
                            end-of-track-event)))
        (string-append output-dir-path target-dir "/" section-name "." "mid")   
        )
       )
     section-list section-name-list)))

;; A bulk processing variant of split-and-process-style.
;; Spilt each style file in input-dir-path in midi pieces, and generate (up to) 15 different in output-dir-path. 
;; Apply mode (deltaTime or absTime) and select channels in channel-selection (a list of integers).
;; .parameter input-dir-path The absolute and complete path to an existing directory that contains style files.
;; .parameter output-dir-path The full file path to a an existiing output directory.
;; .parameter mode Midi processing mode. A symbol. Must be deltaTime.
;; .parameter channel-selection A list of channel numbers, in the interval 1..16. A list of integers.
;; .internal-references "Bulk version" "split-and-process-all-styles"
;; .internal-references "Refined version" "split-and-process-all-styles-refined"
;; .misc Older version. Use the newer and refined variant
(define (split-and-process-all-styles input-dir-path output-dir-path mode channel-selection)
 (let* ((file-list (directory-list input-dir-path))
        (style-file-list (filter (lambda (fn) (member (file-name-extension fn) (list "sty" "pst" "psc" "sst" "prs" "bcs"))) file-list)))
   (for-each (lambda (style-file)
               (display-message style-file)
               (split-and-process-style (string-append input-dir-path style-file) output-dir-path mode channel-selection)
               (display-message "")
             )
             style-file-list)))


;; A much refined version of split-and-process-style.
;; Split a given style file in midi pieces, and generate (up to) 15 different midi files of the parts.
;; Place the midi pieces in subdirectories of output-dir-path.
;; Apply mode (deltaTime or absTime) - a symbol - when parsing the style file.
;; Only output data in the given channel.
;; Do not store meta information. (If you want meta information stored, use split-and-process-style-one-channel-given-ast-refined instead of this function).
;; .parameter style-file-path The absolute and complete path to a style file.
;; .parameter output-dir-path The absolute path to a directory. All but last directory path must be existing.
;; .parameter mode Either absTime or deltaTime (a symbol). MUST be deltaTime
;; .parameter channel A single channel - an integer number between 1 and 16.
;; .internal-references "Bulk version" "split-and-process-all-styles"
(define (split-and-process-style-one-channel-refined style-file-path output-dir-path mode channel)
  (let ((midi-ast (midi-file-to-laml-ast style-file-path mode 0 #f #f)))  ; last #f: produce midi format 0 AST
   (split-and-process-style-one-channel-given-ast-refined #f style-file-path midi-ast output-dir-path mode channel)))


; Doing the real work, given the ast.
; If meta-file-path is not #f, store meta information in this path.
; Factored out of efficiency reasons when repeating the splitting many times for the same style.
(define (split-and-process-style-one-channel-given-ast-refined meta-file-path style-file-path midi-ast output-dir-path mode channel)
  (if (not (eq? mode 'deltaTime))
      (laml-error "split-and-process-style-one-channel-given-ast-refined: mode must be deltaTime" mode))
  
  ; Create last directory in output-dir-path if necessary:
  (if (not (directory-exists? output-dir-path))
      (let ((parent-output-dir (parent-directory output-dir-path))
            (last-dir (directory-leave-name output-dir-path))
           )
        (if (or (not parent-output-dir) (not last-dir))
            (laml-error "You should not work in the root directory"))
        (display-message "Creating" last-dir "in" parent-output-dir)
        (ensure-directory-existence! parent-output-dir last-dir)))


  (let* ((style-name-0 (file-name-proper (file-name-proper style-file-path)))
         (style-name (transliterate style-name-0 #\space "-"))
         (midi-header (ast-subtree midi-ast "MidiHeader"))
         (track-ast (ast-subtree midi-ast "MidiTrack"))  ; format 0 - thus a single track
         (track-events (ast-subtrees track-ast))         ; all midi events in this track 

         (track-meta-divisions (filter meta-division-event? track-events))   ; a list of type 6 meta events
         (track-meta-division-names (map ast-text track-meta-divisions))     ; a list of division names, taken from type 6 meta events

         (init-stuff (midi-event-ast-subsequence track-events #t (third track-meta-division-names)))   ; initial stuff - to be in all sections.

         (section-list 
           (map
            (lambda (from to)
              (midi-event-ast-subsequence track-events from to))
            (cddr track-meta-division-names)
            (append (cdddr track-meta-division-names) (list #t))))

         (section-name-list (map no-spaces-in-string (cddr track-meta-division-names)))
         (end-of-track-event (Meta 'deltaTime "0" 'type "47" "")))

    (ensure-directory-existence! output-dir-path style-name)  
    (ensure-directory-existence! (string-append output-dir-path style-name "/") (as-string channel))   

    (for-each 
     (lambda (section section-name)
      (let* ((init-events-for-selected-channel (select-channel channel init-stuff))
             (body-events-for-selected-channel (select-channel channel section))
             (target-file-path (string-append output-dir-path style-name "/" (as-string channel) "/" section-name "." "mid"))
            )
         (if (not (null? (filter (lambda (x) (and (ast? x) (equal? "NoteOn" (ast-element-name x)))) body-events-for-selected-channel)))
             (let ((meta-info (make-meta-info-about-style-part 
                                 style-name-0 section-name channel style-file-path target-file-path
                                 midi-header init-events-for-selected-channel body-events-for-selected-channel)))
               (if meta-file-path (add-meta-info-to-meta-base meta-file-path meta-info))
               (write-text-file ; There are relevant NoteOn events in the selected channels
                (standard-midi-file-ast-to-bin
                 (StandardMidiFile 'internal:run-action-procedure "false"
                                   midi-header
                                   (MidiTrack 
                                    init-events-for-selected-channel
                                    body-events-for-selected-channel
                                    end-of-track-event)))
                target-file-path
                ))
              'do-nothing)))
     section-list section-name-list)))

;; Split all channels in a style file into midi pieces. 
;; Save meta information about the midi pieces in meta-file-path.
;; If meta-file-path is #f, do not store meta information. Mode must be deltaTime.
;; .parameter meta-file-path The absolute and complete path to a meta file, or #f. An empty meta file is created automatically if it does not exist.
;; .parameter style-file-path The absolute and complete path to a style file.
;; .parameter output-dir-path The absolute path to a directory. All but last directory path must be existing.
;; .parameter mode Either absTime or deltaTime (a symbol). MUST be deltaTime
;; .internal-references "Directory bulk version" "split-and-process-all-styles-refined"
(define (split-and-process-style-refined meta-file-path style-file-path output-dir-path mode)
  (set! global-meta-info-list '())
  (split-and-process-style-refined-1 meta-file-path style-file-path output-dir-path mode))

; just without resetting global-meta-info-list
(define (split-and-process-style-refined-1 meta-file-path style-file-path output-dir-path mode)
  (let ((midi-ast (midi-file-to-laml-ast style-file-path mode 0 #f #f)))  ; last #f: produce midi format 0 AST
    (for-each (lambda (channel)  
                (split-and-process-style-one-channel-given-ast-refined meta-file-path style-file-path midi-ast output-dir-path mode channel))
              (number-interval 1 16))))


;; Split each style file in input-dir-path in midi pieces, and organize the midi pieces in output-dir-path.
;; Only the styles in input-dir-path - and not subdirectories - are processed.
;; Create the meta information in the file addressed by meta-file-path.
;; Create the last directory in output-dir-path if necessary.
;; A bulk processing variant of split-and-process-style-refined.
;; Apply mode (deltaTime or absTime) and select channels in channel-selection (a list of integers). Mode must be deltaTime.
;; .parameter meta-file-path The absolute and complete path to a meta file, or #f. An empty meta file is created automatically if it does not exist.
;; .parameter input-dir-path The absolute and complete path to an existing directory that contains style files.
;; .parameter output-dir-path The absolute path to a directory. All but the last directory path must be existing.
;; .parameter mode Either absTime or deltaTime (a symbol). MUST be deltaTime
;; .internal-references "Directory bulk version" "split-and-process-all-style-directory-refined"
(define (split-and-process-all-styles-refined meta-file-path input-dir-path output-dir-path mode)
  (set! global-meta-info-list '())
  (split-and-process-all-styles-refined-1 meta-file-path input-dir-path output-dir-path mode))  

; just without resetting global-meta-info-list
(define (split-and-process-all-styles-refined-1 meta-file-path input-dir-path output-dir-path mode)
 (let* ((file-list (directory-list input-dir-path))
        (style-file-list (filter (lambda (fn) (member (downcase-string (file-name-extension fn)) (list "sty" "pst" "psc" "sst" "prs" "bcs"))) file-list)))
   (for-each (lambda (style-file)
               (display-message style-file)
               (split-and-process-style-refined-1 meta-file-path (string-append input-dir-path style-file) output-dir-path mode)
               (display-message "")
             )
             style-file-list)))

;; Split each style file in each directory of input-dir-path in midi pieces, and organize the midi pieces in output-dir-path.
;; At the top level, the directory structure is replicated.
;; This is just another bluk version on top of split-and-process-all-styles-refined.
;; No recusive processing is involved. Only a directory of directories of style files.
;; Create the meta information in the file addressed by meta-file-path.
;; Create the last directory in output-dir-path if necessary.
;; Apply mode (deltaTime or absTime) and select channels in channel-selection (a list of integers). Mode must be deltaTime.
;; .parameter meta-file-path The absolute and complete path to a meta file, or #f. An empty meta file is created automatically if it does not exist.
;; .parameter input-dir-path The absolute and complete path to an existing directory that contains directories with style files.
;; .parameter output-dir-path The absolute path to a directory. The leave directory of output-dir-path is created if necessary. The parent part must exists.
;; .parameter mode Either absTime or deltaTime (a symbol). MUST be deltaTime
(define (split-and-process-all-style-directory-refined meta-file-path input-dir-path output-dir-path mode)
  (set! global-meta-info-list '())
  (split-and-process-all-style-directory-refined-1 meta-file-path input-dir-path output-dir-path mode)
)

; just without resetting global-meta-info-list
(define (split-and-process-all-style-directory-refined-1 meta-file-path input-dir-path output-dir-path mode)
 (let* ((directory-list (filter directory-exists? (map (lambda (subdir) (string-append input-dir-path subdir "/"))
                                                       (directory-list input-dir-path))))  ; only directories, full paths - after filtering
        (leave-output-dir (directory-leave-name output-dir-path))
        (output-parent-dir (parent-directory output-dir-path))
       )
   (ensure-directory-existence! output-parent-dir leave-output-dir)
   (for-each (lambda (dir)
               (let ((leave-dir (directory-leave-name dir)))
                 (display-message "***" dir)
                 (ensure-directory-existence! output-dir-path leave-dir)
                 (split-and-process-all-styles-refined-1 meta-file-path dir (string-append output-dir-path leave-dir "/") mode)
                 (display-message ""))
             )
             directory-list)

  (save-meta-info-on-file meta-file-path global-meta-info-list)
 )
)


; (define (fix-it) 
;   (let* ((mel (file-read "c:/users/kurt/Media/Tyros/Styles/style-meta-info.lsp"))
;          (nl (number-interval 1 (length mel))))
;     (file-write
;         (map (lambda (e n)
;                (cons n e))
;              mel nl)
;         "c:/users/kurt/Media/Tyros/Styles/style-meta-info-1.lsp")))

; ---------------------------------------------------------------------------------------------------------------
; Meta data of style pieces.

; Return a list of meta information of a given channel or a given section of a style file.
; channel is an integer
(define (make-meta-info-about-style-part  style-name section-name channel style-file-path target-file-path
               midi-header init-events-for-selected-channel body-events-for-selected-channel)
  (let* ((nil-if-false (lambda (x) (if (and (boolean? x) (not x)) 'nil x)))  ; #f -> nil
 
         (ppqn (as-number (ast-attribute midi-header 'pulsesPerQuarterNote)))
         (meta-time-signature-ast (find-in-list (lambda (x) (and (ast? x) (equal? "Meta" (ast-element-name x)) (equal? (ast-attribute x 'type #f) "88")))
                                                 init-events-for-selected-channel))
         (time-signature (if meta-time-signature-ast (time-signature-of-meta-type-88-ast meta-time-signature-ast) #f))
         (instrument-tuple (find-instrument-info-of channel init-events-for-selected-channel))
         (instrument-name (if instrument-tuple (find-tyros-voice (first instrument-tuple) (second instrument-tuple) (third instrument-tuple)) #f))
         (number-of-notes (length (filter (lambda (x) (and (ast? x) (equal? "NoteOn" (ast-element-name x)))) body-events-for-selected-channel)))
         (number-of-different-notes (count-number-of-different-notes body-events-for-selected-channel))
         (program-control-changes (program-control-change-info init-events-for-selected-channel body-events-for-selected-channel))
         (length-of-body   ; in pulses
             (accumulate-right + 0
                               (map (lambda (ast) (as-number (ast-attribute ast 'deltaTime)))
                                    (cdr  ; do not count first deltaTime
                                      (filter (lambda (x) (ast? x)) body-events-for-selected-channel)))))

        ; (bar-beat-clock    ; a list of 3 numbers
        ;   (if (and ppqn time-signature last-abs-time) (bar-beat-clock length-of-body ppqn (first time-signature) (second time-signature)) #f)) 

        )

  (list
    (nil-if-false time-signature)
    (ceiling (/ length-of-body ppqn))         ; approximate number of quater notes. Rounded up.
    number-of-notes
    (nil-if-false instrument-tuple)
    (nil-if-false instrument-name)
    (nil-if-false section-name)
    channel
    ppqn             ; in pulses
    length-of-body   ; in pulses
    (nil-if-false style-name)
    (nil-if-false (truncate-this-string "c:/users/kurt/Media/Tyros/Styles/midi/" style-file-path))    ; path to style file - the ultimate source of this inforamation
    (nil-if-false (truncate-this-string "c:/users/kurt/Media/Tyros/Styles/midi/MIDI-PIECES/All-pieces/" target-file-path))   ; path the midi file - the midi file with the essential result of this meta information
    number-of-different-notes
    program-control-changes
  )
 )
)

(define (count-number-of-different-notes midi-even-list)
  (let ((occ-count (make-vector 128 0)))   ; 128 elements, initial value 0
    ; register number of occurrences in vector
    (for-each 
       (lambda (x)
         (if (and (ast? x) (equal? "NoteOn" (ast-element-name x)))
             (let ((note (as-number (ast-attribute x 'note))))
               (vector-set! occ-count note (+ 1 (vector-ref occ-count note)))))  ; increment
       )
       midi-even-list)
    
    (accumulate-right + 0 (map (lambda (note) (if (> (vector-ref occ-count note) 0) 1 0)) (number-interval 0 127)))))

; Return a list of the number of Program change events, Expression controller events, and PitchBend controller events.
; A list of three integer numbers.
(define (program-control-change-info init-event-list body-event-list)
  (let* ((all-events (append  init-event-list body-event-list))

         (program-events (filter (lambda (x) (and (ast? x) (equal? "ProgramChange" (ast-element-name x)))) all-events))
         (control-change-expression-events
                         (filter (lambda (x) (and (ast? x) (equal? "ControlChange" (ast-element-name x)) (= 11 (as-number (ast-attribute x 'control))))) all-events))
         (pitch-bend-change-events
                         (filter (lambda (x) (and (ast? x) (equal? "PitchBendChange" (ast-element-name x)))) all-events))
        )
   (list (length program-events)
         (length control-change-expression-events)
         (length pitch-bend-change-events))))
    


; Find the msb, lsb, prog-number for a selected channel in a list of midi event ASTs that
; contains the releveant ControlChange and ProgramChange informations.
; Returns a list of three integers, of #f.
(define (find-instrument-info-of channel midi-events-for-selected-channel)
  (let* ((msb-ast (find-in-list (lambda (x) (and (ast? x) 
                                                 (equal? "ControlChange" (ast-element-name x))
                                                 (= (as-number (ast-attribute x 'channel)) channel)
                                                 (equal? (ast-attribute x 'control #f) "0")))
                               midi-events-for-selected-channel))
         (msb (if msb-ast (ast-attribute msb-ast 'value #f) #f))
         (lsb-ast (find-in-list (lambda (x) (and (ast? x) 
                                                 (equal? "ControlChange" (ast-element-name x))
                                                 (= (as-number (ast-attribute x 'channel)) channel)
                                                 (equal? (ast-attribute x 'control #f) "32")))
                               midi-events-for-selected-channel))
         (lsb (if lsb-ast (ast-attribute lsb-ast 'value #f) #f))
         (prog-number-ast (find-in-list (lambda (x) (and (ast? x) 
                                                 (equal? "ProgramChange" (ast-element-name x))
                                                 (= (as-number (ast-attribute x 'channel)) channel)))
                               midi-events-for-selected-channel))
         (prog-number (if prog-number-ast (ast-attribute prog-number-ast 'number #f) #f)))
    (if (and msb lsb prog-number)
        (list (as-number msb) (as-number lsb) (as-number prog-number))
        #f) ))

; The list where we - internally - accumulates meta info about midi pieces.
(define global-meta-info-list '())

; Add meta-info (a list) to the contents of meta-file-path (a full path to a file with a list).
(define (add-meta-info-to-meta-base meta-file-path meta-info)
  (set! global-meta-info-list (cons meta-info global-meta-info-list))

  ; For each 1000 new elements in global-meta-info-list, save it on meta-file-path.
  (if (= 0 (remainder (length global-meta-info-list) 1000))
      (begin
        (display "Saving meta info about midi-pieces... ")
        (save-meta-info-on-file meta-file-path global-meta-info-list)
        (display-message "  DONE")))
)

(define (save-meta-info-on-file meta-file-path meta-info-list)

  ; Create file in existing directory if necessary:
  (if (not (file-exists? meta-file-path))
      (let ((fnpe (file-name-proper-and-extension meta-file-path))
            (fnip (file-name-initial-path meta-file-path)))
        (display-message "Creating meta piece file" fnpe "in" fnip)
        (if (directory-exists? fnip)
            (file-write '() meta-file-path)
            (laml-error "Trying to make meta midi piece file in non-existing directory" fnip)))) 
  (file-write (reverse meta-info-list) meta-file-path)
)

; -----------------------------------------------------------------------------  
; Utility procedures:

(define (adapt-meta-info-file-to-relative-file-paths meta-info-path)
  (let ((meta-lst (file-read meta-info-path)))
    (file-write (map adapt-meta-entry-to-relative-file-paths meta-lst) meta-info-path)))

(define (adapt-meta-entry-to-relative-file-paths me)
  (list (list-ref me 0) (list-ref me 1) (list-ref me 2) (list-ref me 3) (list-ref me 4) (list-ref me 5) (list-ref me 6) (list-ref me 7) (list-ref me 8) (list-ref me 9)  
        (truncate-this-string "c:/users/kurt/Media/Tyros/Styles/midi/" (list-ref me 10))
        (truncate-this-string "c:/users/kurt/Media/Tyros/Styles/midi/MIDI-PIECES/All-pieces/" (list-ref me 11))
        (list-ref me 12) (list-ref me 13)))    ; when applied to remembered pieces: Add  (list-ref me 14)

(define (truncate-this-string str in-str)
 (let ((str-lgt (string-length str)))
  (if (equal? (substring in-str 0 str-lgt) str )
      (substring in-str str-lgt (string-length in-str))
      (laml-error "truncate-this-string: problems" str in-str))))

; End utility procedures
; -----------------------------------------------------------------------------  


; ---------------------------------------------------------------------------------------------------------------
; Abs time to delta time conversion and vice versa.
; Does only affect the deltaTime and absTime attributes. 
; All other attributes are left unchanged.

(define (abs-time-message-list-to-delta-timing message-ast-list previous-abs-time)
  (cond ((null? message-ast-list) '())
        ((ast? (car message-ast-list))
            (let* ((message-ast (car message-ast-list))
                   (this-abs-time (as-number (ast-attribute message-ast 'absTime)))
                   (new-delta-time (- this-abs-time previous-abs-time))
                   )
              (cons (sm-abs-to-delta-time message-ast new-delta-time)
                    (abs-time-message-list-to-delta-timing (cdr message-ast-list) this-abs-time))))
        (else (cons (car message-ast-list) (abs-time-message-list-to-delta-timing (cdr message-ast-list) previous-abs-time)))))

(define (sm-abs-to-delta-time ast delta-time)
  (make-ast (ast-element-name ast) 
            (ast-subtrees ast) 
            (append (list 'deltaTime 
                           (as-string delta-time))
                    (but-props (ast-attributes ast) (list 'absTime)))
            (ast-kind ast)
            (ast-language ast)
            (ast-internal-attributes ast)))



(define (delta-time-message-list-to-abs-timing message-ast-list start-time)
  (cond ((null? message-ast-list) '())
        ((ast? (car message-ast-list))
           (let* ((message-ast (car message-ast-list))
                  (delta-time (ast-attribute message-ast 'deltaTime))
                  (new-abs-time (+ start-time (as-number delta-time)))
                  )
             (cons (sm-delta-to-abs-time message-ast new-abs-time)
                   (delta-time-message-list-to-abs-timing (cdr message-ast-list) new-abs-time))))
        (else (cons (car message-ast-list) (delta-time-message-list-to-abs-timing (cdr message-ast-list) start-time)))))

(define (sm-delta-to-abs-time ast abs-time)
 (let ((existing-info (ast-attribute ast 'info "")))
  (make-ast (ast-element-name ast) 
            (ast-subtrees ast) 
            (append (list
                       'absTime (as-string abs-time) 
                    )
                    (but-props (ast-attributes ast) (list 'deltaTime 'info)))
            (ast-kind ast)
            (ast-language ast)
            (ast-internal-attributes ast))))

; ---------------------------------------------------------------------------------------------------------------
;;; Arpeggio splitting.

;; Split a midi file in which consecutive arpeggio patterns have been recorded.
;; The midi file is assumed to be a format 0 midi file (as recorded as a song on a motif XS).
;; I.e., the recording happens to be a MIDI format 0 file of PPQN 480.
;; It is assumed that each new arpeggio starts with a ControlChange number 0 message.
;; Deliver the patterns as individual midi files in target-dir (an existing directory).
;; .form (split-arpeggio-recording source-file-path start-number target-dir [expected-length])
;; .parameter source-file-path The full path to the midi file with the recording
;; .parameter start-number The initial arp number of the recording
;; .parameter target-dir-list A list of two full paths to the target directory and trimmed target dir, in which to store the resulting midi files.
;; .parameter expected-length The number of expected midi files. If provided, excution stops if the expectation is not correct.
;; .internal-references "Variant" "split-arpeggio-recording-via-pc-recording"
(define (split-arpeggio-recording source-file-path start-number target-dir-list . optional-parameter-list)
  (let* ((expected-length (optional-parameter 1 optional-parameter-list #f))
         (given-number-interval (optional-parameter 2 optional-parameter-list #f))
         (target-dir (first target-dir-list))
         (trimmed-target-dir (second target-dir-list))
         (meta-target-dir (third target-dir-list))
         (midi-ast (midi-file-to-laml-ast source-file-path 'absTime 0 #f))
         (midi-header (ast-subtree midi-ast "MidiHeader"))
         (track (ast-subtree midi-ast "MidiTrack"))
         (messages (ast-subtrees track))
         (sections (sublist-by-predicate messages (lambda (ast prev-ast n) (ControlChange? ast 0))))
         (sections-1 (cdr sections))
         (sections-2 (map (lambda (section) 
                             (let* ((first-mes (first section))
                                    (first-abs-time (midi 'absTime first-mes)))
                               (time-displace (- first-abs-time) section)))
                          sections-1))
         (count (length sections-1))
        )
    (if (and expected-length (not (= expected-length count)))
        (laml-error "Expected length: " expected-length "  Actual length: " count))
    (if (and given-number-interval (not (= expected-length (length given-number-interval))))
        (laml-error "The explicitly given list has length" (length given-number-interval) ". The expected length is" expected-length))


    (for-each 
       (lambda (section number)
          (let* ((arp-meta-data (get-arpeggio-meta-info number))

                 (a-length (arp-length arp-meta-data))
                 (a-time-sig-str (arp-time-sig arp-meta-data))
                 (a-time-sig-lst (parse-arp-time-signature a-time-sig-str))     ; to such as the list (4  4)
                 (nom (first a-time-sig-lst))
                 (denom (second a-time-sig-lst))                 

                 (target-file (string-append target-dir (as-string number) "." "mid"))
                 (trimmed-target-file (string-append trimmed-target-dir (as-string number) "." "mid"))
                 (total-length (total-length-of-message-list section))
                 (cc-and-pc-section (list-part 1 3 section))
                 (rest-section (cdr (cdr (cdr section))))
                 (first-abs-time (if (not (null? rest-section))
                                     (midi 'absTime (first rest-section))
                                     #f))
                 (time-displaced-rest-section 
                    (time-displace-1  (if first-abs-time (- 480 first-abs-time) 480)
                                      rest-section  
                                      ))
                 (trimmed-time-displaced-rest-section
                    (filter (lambda (event-ast)
                              (< (midi 'absTime event-ast) 
                                 (+ 480 (* a-length nom (cond ((= denom 4) 480) ((= denom 8) 240) (else (laml-error "unsupported time sig")))))))
                            time-displaced-rest-section)) 
                )
            (if (file-exists? target-file) (delete-file target-file)) 
            (if (file-exists? trimmed-target-file) (delete-file trimmed-target-file)) 
            (analyze-arpeggio-for-recording-control! number time-displaced-rest-section arp-meta-data)
            (analyze-arpeggio-and-write-results! meta-target-dir number cc-and-pc-section trimmed-time-displaced-rest-section time-displaced-rest-section arp-meta-data)

            (write-text-file                                     
                (standard-midi-file-ast-to-bin
                  (StandardMidiFile 'internal:run-action-procedure "false"
                                    midi-header
                                    (MidiTrack 
                                       (Meta 'absTime "0" 'type "81" (tempo= (arp-tempo arp-meta-data)))
                                       (midi-comment-abs-time 0 (string-append "Motif XS Arpeggio number " (as-string number)))
                                       cc-and-pc-section
                                       time-displaced-rest-section
                                       (Meta 'absTime (+ total-length 960) 'type "47" "")
                                    )))
                target-file)

            (write-text-file                                     ; Writes the trimmed section                                 
                (standard-midi-file-ast-to-bin
                  (StandardMidiFile 'internal:run-action-procedure "false"
                                    midi-header
                                    (MidiTrack 
                                       (Meta 'absTime "0" 'type "81" (tempo= (arp-tempo arp-meta-data)))
                                       (midi-comment-abs-time 0 (string-append "Motif XS Arpeggio number " (as-string number)))
                                       cc-and-pc-section
                                       trimmed-time-displaced-rest-section    
                                       (Meta 'absTime 
                                             (+ 480 (* a-length nom (cond ((= denom 4) 480) ((= denom 8) 240) (else (laml-error "unsupported time sig")))))
                                             'type "47" "")
                                    )))
                trimmed-target-file)
            )
       )
       sections-2
       (if given-number-interval given-number-interval (number-interval start-number (+ start-number count -1))))
  )
)

;; A variant of split-arpeggio-recording which takes a PC recording (as opposed to a Motif XS recording) as starting point.
;; The PC recording happens to be a MIDI format 1 file of PPQN 960.
;; .form (split-arpeggio-recording-via-pc-recording source-file-path start-number target-dir [expected-length])
;; .parameter source-file-path The full path to the midi file with the recording
;; .parameter start-number The initial arp number of the recording
;; .parameter target-dir-list A list of two full paths to the target directory and trimmed target dir, in which to store the resulting midi files.
;; .parameter expected-length The number of expected midi files. If provided, excution stops if the expectation is not correct.
;; .internal-references "Variant" "split-arpeggio-recording"
(define (split-arpeggio-recording-via-pc-recording source-file-path start-number target-dir-list . optional-parameter-list)
  (let* ((expected-length (optional-parameter 1 optional-parameter-list #f))
         (given-number-interval (optional-parameter 2 optional-parameter-list #f))
         (target-dir (first target-dir-list))
         (trimmed-target-dir (second target-dir-list))
         (meta-target-dir (third target-dir-list))
         (midi-ast (midi-file-to-laml-ast source-file-path 'absTime 0 #f))
         (midi-header (MidiHeader 'format "0" 'numberOfTracks "1" 'pulsesPerQuarterNote "480" 'mode "absTime" 'counterTransposition "0"))
         (track (ast-subtree midi-ast "MidiTrack" 2))  ; track 2 because the recording happens to be a format 1 midi file.
         (messages (ast-subtrees track))
         (sections (sublist-by-predicate messages (lambda (ast prev-ast n) (ControlChange? ast 0))))
         (sections-1 (cdr sections))  ; drop the first one
         (sections-2 (map (lambda (section) 
                             (let* ((first-mes (first section))
                                    (first-abs-time (midi 'absTime first-mes)))
                               (time-displace (- first-abs-time) section)))
                          sections-1))
         (count (length sections-1))
        )
    (if (and expected-length (not (= expected-length count)))
        (laml-error "Expected length: " expected-length "  Actual length: " count))
    (if (and given-number-interval (not (= expected-length (length given-number-interval))))
        (laml-error "The explicitly given list has length" (length given-number-interval) ". The expected length is" expected-length))

    (for-each 
       (lambda (section number)  ; section is assumed to start with to ControlChange messages and one ProgramChange message.
          (let* ((arp-meta-data (get-arpeggio-meta-info number))
                 (target-file (string-append target-dir (as-string number) "." "mid"))
                 (total-length (total-length-of-message-list section))
                 (stretched-section (time-stretch-1 0.5 section)) ; go from ppqn 960 to 480
                 (cc-and-pc-section (list-part 1 3 stretched-section))
                 (rest-section (cdr (cdr (cdr stretched-section))))
                 (first-abs-time (if (not (null? rest-section))
                                     (midi 'absTime (first rest-section))
                                     #f))
                )
            (if (file-exists? target-file) (delete-file target-file)) 
            (analyze-arpeggio-for-recording-control! number rest-section arp-meta-data)
            (write-text-file ; There are relevant NoteOn events in the selected channels
                (standard-midi-file-ast-to-bin
                  (StandardMidiFile 'internal:run-action-procedure "false"
                                    midi-header
                                    (MidiTrack 
                                       (Meta 'absTime "0" 'type "81" (tempo= (arp-tempo arp-meta-data)))
                                       (midi-comment-abs-time 0 (string-append "Motif XS Arpeggio number " (as-string number)))
                                       cc-and-pc-section
                                       (time-displace-1 (if first-abs-time (- 480 first-abs-time) 480)
                                         rest-section  
                                       )
                                       (Meta 'absTime (+ total-length 960) 'type "47" "")
                                    )))
                target-file)
            )
       )
       sections-2
       (if given-number-interval given-number-interval (number-interval start-number (+ start-number count -1)))
    )
  )
)

(define (analyze-arpeggio-for-recording-control! arp-number midi-event-list arp-meta-data)
  (let* ((midi-event-list-1 (filter ast? midi-event-list))
         (a-length (arp-length arp-meta-data))
         (a-time-sig-str (arp-time-sig arp-meta-data))
         (a-time-sig-lst (parse-arp-time-signature a-time-sig-str)) ; to such as (4  4)
         (nom (first a-time-sig-lst))
         (denom (second a-time-sig-lst))
         (last-abs-time (ast-attribute (last midi-event-list-1) 'absTime))
         (ppqn 480)
        )
   (cond ((= denom 4)
           (let ((required-length (* nom ppqn a-length))
                 (actual-length (- (as-number last-abs-time) 480)))
             (display-message
              (string-append
               (as-string arp-number) ": "
               (if (> required-length actual-length) "!!" "  ")
               "Required length: " a-time-sig-str " " (as-string required-length ) ".  "  "Actual length: " (as-string actual-length)))))
         ((= denom 8)
           (let ((required-length (* nom (/ ppqn 2) a-length))
                 (actual-length (- (as-number last-abs-time) 480)))
             (display-message
              (string-append
               (as-string arp-number) ": "
               (if (> required-length actual-length) "!!" "  ")
               "Required length: " "(" a-time-sig-str ")" " " (as-string required-length ) ".  "  "Actual length: " (as-string actual-length)))))
         (else (display-message "Non-fit")))))


; Format of arp analysis meta data:
; (long-enough msb-lsb-pc-voice-info-list number-of-notes number-of-different-notes pc-exprcc-pitchbend-list)

(define (analyze-arpeggio-and-write-results! target-dir arp-number cc-pc-events trimmed-midi-event-list untrimmed-midi-event-list arp-meta-data)
  (let* (
         (nil-if-false (lambda (x) (if (and (boolean? x) (not x)) 'nil 't)))
         (midi-event-list-1 (filter ast? trimmed-midi-event-list))
         (a-length (arp-length arp-meta-data))

         (a-time-sig-str (arp-time-sig arp-meta-data))
         (a-time-sig-lst (parse-arp-time-signature a-time-sig-str)) ; to such as (4  4)
         (nom (first a-time-sig-lst))
         (denom (second a-time-sig-lst))

         (last-abs-time (ast-attribute (last untrimmed-midi-event-list) 'absTime))
         (ppqn 480)

         (required-length (* nom ppqn a-length))
         (actual-length (- (as-number last-abs-time) 480))

         (msb-lsb-pc (find-instrument-info-of 1 cc-pc-events))  ; first parameter: channel: all arps are recorded to channel 1.
         (number-of-notes (length (filter (lambda (x) (and (ast? x) (equal? "NoteOn" (ast-element-name x)))) midi-event-list-1)))
         (number-of-different-notes (count-number-of-different-notes midi-event-list-1))
         (program-control-changes (program-control-change-info cc-pc-events midi-event-list-1))
        )
   (let ((result
           (list
                 (nil-if-false (<= required-length actual-length))
                 msb-lsb-pc
                 number-of-notes
                 number-of-different-notes
                 program-control-changes
           )
         )
         (target-file (string-append target-dir (as-string arp-number) "." "dat"))
        )
     (if (file-exists? target-file) (delete-file target-file))
     (file-write result target-file)
   )
 )
)

(define (parse-arp-time-signature time-sig-str)
  (map as-number (split-string-by-predicate time-sig-str (lambda (ch) (eqv? ch #\/)))))

(define cached-arpeggio-list #f)

(define (get-arpeggio-meta-info arp-number)
  (if cached-arpeggio-list 
      (list-ref cached-arpeggio-list (- arp-number 1))
      (let ((arp-info-list (file-read (string-append midi-software-dir "data/Motif-xs-arps.dat"))))
        (set! cached-arpeggio-list arp-info-list)
        (list-ref arp-info-list (- arp-number 1)))))

; Arp selectors

(define arp-main-cat (make-selector-function 1 "arp-main-cat"))
(define arp-sub-cat (make-selector-function 2 "arp-sub-cat"))
(define arp-number (make-selector-function 3 "arp-number"))
(define arp-role (make-selector-function 4 "arp-role"))
(define arp-name (make-selector-function 5 "arp-name"))
(define arp-generation (make-selector-function 6 "arp-generation"))
(define arp-time-sig (make-selector-function 7 "arp-time-sig"))
(define arp-length (make-selector-function 8 "arp-lenght"))
(define arp-tempo (make-selector-function 9 "arp-tempo"))
(define arp-accent? (make-selector-function 10 "arp-accent?"))
(define arp-random-sfx? (make-selector-function 11 "arp-random-sfx?"))
(define arp-voice (make-selector-function 12 "arp-voice"))
(define arp-voice-specific (make-selector-function 13 "arp-voice-specific"))


; ---------------------------------------------------------------------------------------------------------------
;;; .section-id aux-functions
;;; Auxiliary functions.
;;; Miscellaneous function that are not naturally contained in the categories from above.


;; A higher-order function that returns a numeric pitch-bend scaling function that takes into consideration that the mid-point pitch-bend value
;; is 8192 (and not 0 as in the simple case of scaling).
;; .returns A function suitable as transformation function in transform-attribute on the value attribute in PitchBendChange messages.
;; .internal-references "Possible use context" "transform-attribute"
(define (pitch-bend-scale factor)
  (let ((mid-value 8192))
    (lambda (value)
      (+  (* (- value mid-value) factor) mid-value))))

; Copy a list of midi ASTs.
; Useful if the same list of Midi ASTs are used several places in a song.
; The out list structure, the AST list structure and the attribute property list structures are copied.
; Strings are not copied.
; Not necessary any more (as of May 15, 2008).
(define (copy-midi-ast-list ast-lst)
  (map copy-midi-ast ast-lst))

(define (copy-midi-ast x)
  (if (ast? x)
      (make-ast
            (ast-element-name x) 
            (ast-subtrees x) 
            (copy-midi-property-list (ast-attributes x))
            (ast-kind x)
            (ast-language x)
            (ast-internal-attributes x))
      x))

(define (copy-midi-property-list plst)
  (if (null? plst)
      '()
      (cons (car plst) (cons (cadr plst) (copy-midi-property-list (cddr plst))))))

;; Calculate the total length of message-list.
;; Works in both absTime and deltaTime mode.
(define (total-length-of-message-list message-list)
 (let ((message-list-asts-only (filter ast? message-list)))
  (cond ((abs-time-sequence? message-list) 
           (let* ((first-message (first message-list-asts-only))
                  (last-message (last message-list-asts-only)))
             (- (time-of-message last-message) (time-of-message first-message))))
        ((delta-time-sequence? message-list)
           (accumulate-right + 0 (map (lambda (ast) (time-of-message ast)) message-list-asts-only)))
        (else (laml-error "total-length-of-message-list: Cannot determine time mode of message-list. Is the message-list maybe empty?")))))

;; Enforce that message-list will have a mini length of min-length.
;; Works only in deltaTime mode.
;; .mode deltaTime
(define (enforce-minimum-message-length min-length message-list)
  (let ((lgt (total-length-of-message-list message-list)))
    (if (< lgt min-length)
        (append 
           message-list 
           (list (midi-null-event-delta-time (- min-length lgt) (string-append "Enforcing of minium length"))))
        message-list)))

; ---------------------------------------------------------------------------------------------------------------
; Image file names

;; Return an icon name, which serve as the 'middle name' of a style file.
;; .parameter name A symbolic name of an icon. The currently supported names are: "penguin", "butterfly", "candle", "banana", "orange", "lighting".
(define (icon name)
  (cond ((equal? name "penguin") "S713")
        ((equal? name "butterfly") "S690")
        ((equal? name "candle") "S719")
        ((equal? name "banana") "S696")
        ((equal? name "orange") "S697")
        ((equal? name "lighting") "S718")
        (else "S713")))  

; ---------------------------------------------------------------------------------------------------------------

;; Returns the complement of note-str-list, relative to the possible note names.
;; .parameter note-str-list A comma-separated list of notes names belonging to C CS D Eb E F FS G GS A Bb and B.
;; .returns The comma-separated list of notes not in note-str-list.
(define (note-complement note-str-list)
 (let* ((note-list (string-to-list (transliterate note-str-list #\space "") (list #\,)))
        (complement-note-list
          (map (lambda (nn) (if (member nn note-list) #f nn)) note-name-list)))
   (list-to-string (filter (lambda (x) x) complement-note-list) ",")))

;; Returns the complement of chord-str-list, relative to the possible note names.
;; .parameter chord-str-list A comma-separated list of chord names belonging to maj maj6 maj7 maj7s11 maj9 maj7-9 maj6-9 aug min min6 min7 min7b5 min-9 min7-9 min7-11 min-maj7 min-maj7-9 dim dim7 7th 7sus4 7b5 7-9 7s11 7-13 7-b9 7-b13 7-s9 maj7aug 7aug 1-plus-8 1-plus-5 sus4 1-plus-2-plus-5 and cancel.
;; .returns The comma-separated list of chords not in chord-str-list.
(define (chord-complement chord-str-list)
 (let* ((chord-list (string-to-list (transliterate chord-str-list #\space "") (list #\,)))
        (complement-chord-list
          (map (lambda (cn) (if (member cn chord-list) #f cn)) chord-name-list)))
   (list-to-string (filter (lambda (x) x) complement-chord-list) ",")))


; ---------------------------------------------------------------------------------------------------------------
; Drum map - categorization of drum note values.

; Drum map vector, starting at index 13:
(define drum-map-vector
  #(latin-percussion latin-percussion others others others others others                                                        ; 13 -19
    others others others others others snare-drum snare-drum snare-drum snare-drum snare-drum                                   ; 20 - 29
    latin-high-pitch snare-drum others bass-drum snare-drum bass-drum bass-drum snare-drum snare-drum others                    ; 30 - 39
    snare-drum tom hi-hat tom hi-hat tom hi-hat tom tom crash-cymbal                                                            ; 40 - 49
    tom ride-cymbal cymbal ride-cymbal others cymbal others crash-cymbal others ride-cymbal                                     ; 50 - 59
    latin-percussion latin-percussion latin-percussion latin-percussion latin-percussion                                  ; 60 - 64
    latin-percussion latin-percussion latin-high-pitch latin-high-pitch latin-high-pitch                                  ; 66 - 69
    latin-high-pitch latin-high-pitch latin-high-pitch latin-high-pitch latin-high-pitch                                  ; 70 - 74
    latin-high-pitch latin-high-pitch latin-high-pitch latin-high-pitch latin-high-pitch                                  ; 75 - 79
    others others latin-high-pitch others others                                                                          ; 80 - 84
    others others others undefined undefined others others                                                                ; 85 - 91
   )
)

;; Return the drum category of note-value.
;; .parameter note-value An integer between 0 and 127.
;; .returns One of the symbols undefined, latin-percussion, others, snare-drum, latin-high-pitch, bass-drum, hi-hat, tom, crash-cymbal, ride-cymbal, cymbal, latin-high-pitch.
(define (drum-category-of-note-value note-value)
  (if (and (>= note-value 13) (<= note-value 91))
      (vector-ref drum-map-vector (- note-value 13))
      'undefined)) 

; ---------------------------------------------------------------------------------------------------------------

; A function used internally by a MIDL template, as used by MIDI LAML Emacs list support - for playing including the fixed part.
(define (get-fixed-part-from-to-abstime the-fixed-part-full-path first-abs-time last-abs-time)
  (let* ((fixed-part-ast-list (map uncompact-midi-laml-entry (file-read the-fixed-part-full-path))))
    (filter-messages-1 
      (lambda (m) (and (>= (midi 'absTime m) first-abs-time) (<= (midi 'absTime m) last-abs-time)))
      fixed-part-ast-list)
  )
)

; Returns the ending part of Emacs MIDI LAML sectional playing. Used in some of the MIDI LAML templates.
(define (end-sectional-playing delta-time)
  (list
     (midi-comment-delta-time delta-time "Ending part starts here")

     ; Reset sustain in all channels
     (map (lambda (ch) 
             (ControlChange 'deltaTime "0" 'channel ch 'control "64" 'value "0"))
          (number-interval 1 16))
   
     ; End of track message
     (Meta 'deltaTime "10" 'type "47" "")
  ) 
)



