; --------------------------------------------------------------------------------------------------------------------------------
; Loading section

(lib-load "xml-in-laml/xml-in-laml.scm")
(lib-load "time.scm")

(define (song-front-matters! ast)
  (do-song-front-matters ast))

(define (song-verses! ast)
  (do-song-verses ast))

(set! xml-check-language-overlap? #f)


(lib-load "xml-in-laml/mirrors/xhtml10-transitional-mirror.scm")
(define xhtml:em (xhtml10-transitional 'em))
(lib-load "xhtml1.0-convenience.scm")

; Loading chords XML-in-LAML stuff. 
; Overwrites a and em from the XHTML mirror.
(laml-style "xml-in-laml/chords/mirror/chords")

; Accept non-strings as attribute values in songs with chords:
(set-xml-accept-only-string-valued-attributes-in 'chords #f)

; ---------------------------------------------------------------------------------------------------------------
; Some global variables, used for non-local communication.

(define the-song-title "")
(define the-song-author "")
(define the-song-source "")
(define the-font-size 100)
(define the-indentation 0)
(define the-chord-color "black")
(define compact-lines-wanted #f)
(define rendering-mode 'normal)
(define do-show-chords #t)
(define the-name-of-h #f)

(define cs-or-db-choice #f)
(define ds-or-eb-choice #f)
(define fs-or-gb-choice #f)
(define gs-or-ab-choice #f)
(define as-or-hb-choice #f)

; shortcuts to the attributes above:
(define prefer-b (list 'cs-or-db "db" 'ds-or-eb "eb" 'fs-or-gb "gb" 'gs-or-ab "ab" 'as-or-hb "hb"))
(define prefer-s (list 'cs-or-db "cs" 'ds-or-eb "ds" 'fs-or-gb "fs" 'gs-or-ab "gs" 'as-or-hb "as"))

(define chords-url "http://www.cs.aau.dk/~normark/scheme/styles/xml-in-laml/chords/man/chords.html")

; ---------------------------------------------------------------------------------------------------------------

(define (do-song-front-matters ast)
  (let* ((song-title (find-first-ast ast "song-title")) 
         (title-str (if song-title (ast-text song-title) #f))
         (song-source (find-first-ast ast "song-source"))
         (source-str (if song-source (ast-text song-source) #f))
         (song-author (find-first-ast ast "song-author"))
         (author-str (if song-author (ast-text song-author) #f)) 

         (font-size (as-number (ast-attribute ast 'font-size 100)))
         (indentation (as-number (ast-attribute ast 'indentation 0)))
         (transposition-number (as-number (ast-attribute ast 'transposition 0)))
         (compact-lines? (as-boolean (ast-attribute ast 'compact-lines "false")))
         (chord-color (ast-attribute ast 'chord-color "black"))
         (h-name (upcase-string (ast-attribute ast 'h-name "h")))

         (cs-or-db (ast-attribute ast 'cs-or-db "cs"))
         (ds-or-eb (ast-attribute ast 'ds-or-eb "es"))
         (fs-or-gb (ast-attribute ast 'fs-or-gb "fs"))
         (gs-or-ab (ast-attribute ast 'gs-or-ab "gs"))
         (as-or-hb (ast-attribute ast 'as-or-hb "hb"))

         (the-rendering-mode (ast-attribute ast 'rendering-mode "normal"))
        )

   (set! the-song-title title-str)
   (set! the-song-author author-str)
   (set! the-song-source source-str)
   (set! the-font-size font-size)
   (set! the-indentation indentation)
   (set! transposition transposition-number)  ; a global variable
   (set! compact-lines-wanted compact-lines?)
   (set! rendering-mode (as-symbol the-rendering-mode))
   (set! the-chord-color chord-color)
   (set! the-name-of-h h-name)

   (set! cs-or-db-choice cs-or-db) 
   (set! ds-or-eb-choice ds-or-eb) 
   (set! fs-or-gb-choice fs-or-gb) 
   (set! gs-or-ab-choice gs-or-ab) 
   (set! as-or-hb-choice as-or-hb) 
))



(define (do-song-verses ast)
 (let ((xhtml:title (xhtml10-transitional 'title))
       (verses (find-asts ast "verse" verse-transform)))
   (write-html '(pp prolog)
     (html
      (head (xhtml:title the-song-title))
      (body 
       (cond ((and the-song-title the-song-author) 
               (left-right-banner "" (div 'css:text-align "right"  
                                       (span 'css:font-size (string-append (as-string the-font-size) "%")
                                             (b the-song-title) (br) (xhtml:em the-song-author))  (p))))
             (the-song-title 
               (left-right-banner "" (div 'css:text-align "right"  
                                       (span 'css:font-size (string-append (as-string the-font-size) "%")
                                             (b the-song-title)) (p))))
             (the-song-author
               (left-right-banner "" (div 'css:text-align "right"  
                                      (span 'css:font-size (string-append (as-string the-font-size) "%")
                                             (xhtml:em the-song-author)) (p))))
             (else '()))

       (indent-pixels 
         the-indentation
         (div 'css:font-size (string-append (as-string the-font-size) "%")
           verses))
 
       (left-right-banner ""
                          (span
                              (font 'size 1 'color "grey" (when-generated))
                              (font 'size 1 'color "grey" (span "by LAML" (a 'css:color "inherit" 'href chords-url "Chords")))
                              (if the-song-source (list (br) (font 'size 1 'color "grey" (b the-song-source)) (br)) "")))


                              

       ))
      (in-startup-directory (string-append (source-filename-without-extension) "." "html")))
  (end-laml)

  ))


(define (verse-transform verse-ast)
 (let ((show-chords? (as-boolean (ast-attribute verse-ast 'chords "true"))))
  (set! do-show-chords show-chords?)
  (let ((lines (find-asts verse-ast "line" line-transform))
        (n (ast-attribute verse-ast 'n #f))
       )
    (div (if show-chords? (list 'css:line-height "7ex") '())
         (if n (span 'css:margin-left "-3ex" (b (string-append (as-string n) ".")) (horizontal-space 1)) '()) 
         lines (p)))))

(define (line-transform line-ast)
 (span 
  (xhtml:em 
   (transform-ast-list
    (ast-subtrees line-ast)
    (list chord-ast? chord-transform) (list unison-ast? unison-transform)
   ) 
   (br))))

(define (unison-ast? x)
  (and (ast? x)
       (equal? "unison" (ast-element-name x))))

(define (unison-transform unison-ast)
 (let* ((phrase (ast-text unison-ast))
        (phrase1 (if (blank-string? phrase) #f phrase))
        (phrase-length (if (string? phrase) (string-length phrase) 0))
        (phrase-str (string-append "-" (as-string phrase-length) "ex"))
       )
  (span phrase
        (span 'css:position "relative" 'css:top "-1.2em" 'css:left phrase-str
              (b 'css:font-style "normal" 'css:color the-chord-color (xhtml:em "unis."))))
 )
)


(define (chord-ast? x)
  (and (ast? x)
       (member (ast-element-name x) (list "c"  "cs"  "d"  "eb"  "e"  "f"  "fs"  "g"  "gs"  "a"  "hb"  "h"))))

(define (chord-transform chord-ast)
  (let* ((phrase (ast-text chord-ast))
         (phrase1 (if (blank-string? phrase) #f phrase))
         (abstr (ast-attribute chord-ast 'x))
         (bass (ast-attribute chord-ast 'b))
         (optional (ast-attribute chord-ast 'o))
         (optional-1 (if optional (cond  ((equal? optional "t") #t) ((equal? optional "f") #f) (else #f)) #f))
        )
    (phrase-with-chord phrase1 (as-symbol (ast-element-name chord-ast)) abstr bass optional-1))) 

;---------------------------------------------------------------------------------------------------------------
; Transpose stuff:

(define wheel '(c cs d eb e f fs g gs a hb h))

(define minus-wheel (reverse wheel))

; Transponer song et antal steps. 
; Et positiv steps værdi transponerer med uret på hjule.
; En negativ steps værdi transponener mod uret.
; song er en liste af noder. En node kan være et symbol, eller en liste af 
; det primære navn og dets abstraktion: c eller (c) eller (c 7).
(define (transpose-song song steps)
  (map (transpose-node steps) song))

(define (transpose-node steps)
  (lambda (node) 
    (cond ((and (symbol? node) (or (eq? node 'x) (eq? node 'X))) 'X) 
          ((symbol? node) (transpose-primary-node node steps))
          ((list? node) (cons (transpose-primary-node (car node) steps) (cdr node)))
          (else (laml-error "transpose-node: Unknown node")))))

; Steps is in between -11 and 11
(define (transpose-primary-node node steps)
 (if (>= steps 0)
     (let* ((node-val (index-in-list-by-predicate wheel node eq?))
            (transposed-node-val (+ node-val steps))
            (transposed-node (cond ((and (>= transposed-node-val 0) (<= transposed-node-val 11)) 
                                      (list-ref wheel transposed-node-val))
                                   ((and (>= transposed-node-val 12) (<= transposed-node-val 23))
                                    (list-ref wheel (- transposed-node-val 12)))
                                   (else (laml-error "tranpose-primary-node: Problems (1)")))))
        transposed-node)
     (let* ((node-val (index-in-list-by-predicate minus-wheel node eq?))
            (transposed-node-val (+ node-val (- steps)))
            (transposed-node (cond ((and (>= transposed-node-val 0) (<= transposed-node-val 11)) 
                                      (list-ref minus-wheel transposed-node-val))
                                   ((and (>= transposed-node-val 12) (<= transposed-node-val 23))
                                    (list-ref minus-wheel (- transposed-node-val 12)))
                                   (else (laml-error "tranpose-primary-node: Problems (2)")))))
        transposed-node)))

; ---------------------------------------------------------------------------------------------------------------

; The number of half steps of transposition. A number between -11 and 11.
(define transposition 0)

; Handle a single chord, alone or surrounding phrase. The chord abstraction is also given as parameter.
; phrase is a pharase string
; chord is a symbol or string, as in the chord wheel.
; abstractin is any string to follow the chord, of #f if no abstractions apply.
; If optional (a boolean value) do not show the chord in simple rendering mode.
(define (phrase-with-chord phrase chord abstraction bass optional)
  (let* ((transposed-chord (transpose-primary-node (as-symbol chord) transposition))
         (transposed-bass (if bass (transpose-primary-node (as-symbol (downcase-string bass)) transposition) #f))
         (transposed-chord-with-abstraction (chord-presentation transposed-chord abstraction transposed-bass))
         (phrase-length (if (string? phrase) (string-length phrase) 0))
         (abstracted-chord-length 
             (+ (string-length (as-string (as-string transposed-chord)))
                (if abstraction (string-length (as-string abstraction)) 0)))
         (padding-constant (if compact-lines-wanted 0 1))  ; Better in IE: non-compact lines
         (padding (if (> abstracted-chord-length phrase-length)
                      1
                      (- (max (- abstracted-chord-length padding-constant) 0))))     
         (padding-str (string-append (as-string padding) "ex"))
         (phrase-str (string-append "-" (as-string phrase-length) "ex")))
    (if (or (not (eq? rendering-mode 'simple))  ; meaning: normal
            (and (eq? rendering-mode 'simple) (not optional)))   
        (if do-show-chords
            (span phrase
                  (span 'css:position "relative" 'css:top "-1.2em" 'css:left phrase-str
                        (b 'css:font-style "normal" 'css:color the-chord-color transposed-chord-with-abstraction))
                  (span 'css:margin-left padding-str) )
            phrase)
        phrase)))
        

; The textual chord presentation.
; chord is a symbol, as in the chord wheel
; abstraction is an arbitrary string, or #f if no abstractions apply.
; Return a span clause for the chord with its potential abstraction
(define (chord-presentation chord abstraction bass)
 (let ((abstraction-1 (if abstraction abstraction ""))
       (bass-1 (if bass (bass-presentation bass) "")) 
      )
  (cond ((eq? chord 'cs)
            (if (equal? cs-or-db-choice "cs")
                (span "C" _ (char-ref 9839) _ abstraction-1 _ bass-1)
                (span "D" _ (char-ref 9837) _ abstraction-1 _ bass-1)))
        ((eq? chord 'eb)
            (if (equal? ds-or-eb-choice "eb")
                (span "E" _ (char-ref 9837) _ abstraction-1 _ bass-1)
                (span "D" _ (char-ref 9839) _ abstraction-1 _ bass-1)))
        ((eq? chord 'fs)
            (if (equal? fs-or-gb-choice "fs")
                (span "F" _ (char-ref 9839) _ abstraction-1 _ bass-1)
                (span "G" _ (char-ref 9837) _ abstraction-1 _ bass-1)))
        ((eq? chord 'gs)
            (if (equal? gs-or-ab-choice "gs")
                (span "G" _ (char-ref 9839) _ abstraction-1 _ bass-1)
                (span "A" _ (char-ref 9837) _ abstraction-1 _ bass-1)))
        ((eq? chord 'h)
            (if (equal? the-name-of-h "H")
                (span "H" _ abstraction-1 _ bass-1)
                (span "B" _ abstraction-1 _ bass-1))
        )
        ((eq? chord 'hb)
            (if (equal? as-or-hb-choice "hb")
                (if (equal? the-name-of-h "H")
                    (span "H" _ (char-ref 9837) _ abstraction-1 _ bass-1)
                    (span "B" _ (char-ref 9837) _ abstraction-1 _ bass-1))
                (span "A" _ (char-ref 9839) _ abstraction-1 _ bass-1))
        )
        (else (span (upcase-string (as-string chord)) _ abstraction-1 _ bass-1 )))))


; The textual bass note presentation.
(define (bass-presentation bass-symbol)
  (cond ((eq? bass-symbol 'cs)
            (if (equal? cs-or-db-choice "cs")
                (span "/" _ "C" _ (char-ref 9839) )
                (span "/" _ "D" _ (char-ref 9837) )))
        ((eq? bass-symbol 'eb)
            (if (equal? ds-or-eb-choice "eb")
                (span "/" _ "E" _ (char-ref 9837))
                (span "/" _ "D" _ (char-ref 9839))))
        ((eq? bass-symbol 'fs)
            (if (equal? fs-or-gb-choice "fs")
                (span "/" _ "F" _ (char-ref 9839))
                (span "/" _ "G" _ (char-ref 9837))))
        ((eq? bass-symbol 'gs)
            (if (equal? gs-or-ab-choice "gs")
                (span "/" _ "G" _ (char-ref 9839))
                (span "/" _ "A" _ (char-ref 9837))))
        ((eq? bass-symbol 'h)
            (if (equal? the-name-of-h "H")
                (span "/" _ "H")
                (span "/" _ "B"))
        )
        ((eq? bass-symbol 'hb)
            (if (equal? as-or-hb-choice "hb")
                (if (equal? the-name-of-h "H")
                    (span "/" _ "H" _ (char-ref 9837))
                    (span "/" _ "B" _ (char-ref 9837)))
                (span "/" _ "A" _ (char-ref 9839)))
        )
        (else (span "/" _ (upcase-string (as-string bass-symbol)) ))))

; ---------------------------------------------------------------------------------------------------
; Secondary auxiliary chords, defined as ad hoc XML-in-LAML abstractions.
; These functions are not strictly necessary, but they are convenient for smooth definition of
; the most common chords with abstractions.

; Macro which is used to define auxiliary chords.
(define-syntax chord-variation   
 (syntax-rules ()
   ((chord-variation main-chord-name variation)
    (xml-in-laml-abstraction
    (lambda (cont attr)
      (let* ((abstr (defaulted-get-prop 'x attr #f))
             (bass  (defaulted-get-prop 'b attr #f))
             (optional  (defaulted-get-prop 'o attr #f))
             (optional-1 (if optional (cond  ((equal? optional "t") #t) ((equal? optional "f") #f) (else #f)) #f))
            )
        (main-chord-name cont 'x (if abstr (string-append variation abstr) variation) (if bass (list 'b bass) '()) (if optional-1 (list 'o "t") '()))))))))

; m - minors
(define cm (chord-variation c "m"))
(define csm (chord-variation cs "m"))
(define dm (chord-variation d "m"))
(define ebm (chord-variation eb "m"))
(define em  (chord-variation e "m"))    ; notice the overlap with XHTML em. XHTML em exists as xthml:em
(define fm (chord-variation f "m"))
(define fsm (chord-variation fs "m"))
(define gm (chord-variation g "m"))
(define gsm (chord-variation gs "m"))
(define am (chord-variation a "m"))
(define hbm (chord-variation hb "m"))
(define hm (chord-variation h "m"))

; 7
(define c7 (chord-variation c "7"))
(define cs7 (chord-variation cs "7"))
(define d7 (chord-variation d "7"))
(define eb7 (chord-variation eb "7"))
(define e7  (chord-variation e "7"))  
(define f7 (chord-variation f "7"))
(define fs7 (chord-variation fs "7"))
(define g7 (chord-variation g "7"))
(define gs7 (chord-variation gs "7"))
(define a7 (chord-variation a "7"))
(define hb7 (chord-variation hb "7"))
(define h7 (chord-variation h "7"))

; 6
(define c6 (chord-variation c "6"))
(define cs6 (chord-variation cs "6"))
(define d6 (chord-variation d "6"))
(define eb6 (chord-variation eb "6"))
(define e6  (chord-variation e "6"))  
(define f6 (chord-variation f "6"))
(define fs6 (chord-variation fs "6"))
(define g6 (chord-variation g "6"))
(define gs6 (chord-variation gs "6"))
(define a6 (chord-variation a "6"))
(define hb6 (chord-variation hb "6"))
(define h6 (chord-variation h "6"))

; m7
(define cm7 (chord-variation c "m7"))
(define csm7 (chord-variation cs "m7"))
(define dm7 (chord-variation d "m7"))
(define ebm7 (chord-variation eb "m7"))
(define em7  (chord-variation e "m7"))  
(define fm7 (chord-variation f "m7"))
(define fsm7 (chord-variation fs "m7"))
(define gm7 (chord-variation g "m7"))
(define gsm7 (chord-variation gs "m7"))
(define am7 (chord-variation a "m7"))
(define hbm7 (chord-variation hb "m7"))
(define hm7 (chord-variation h "m7"))

; extra + variations: translates:

(define db (chord-variation cs ""))
(define ds (chord-variation eb ""))
(define gb (chord-variation fs ""))
(define ab (chord-variation gs ""))
(define as (chord-variation hb ""))

; m
(define dbm (chord-variation cs "m"))
(define dsm (chord-variation eb "m"))
(define gbm (chord-variation fs "m"))
(define abm (chord-variation gs "m"))
(define asm (chord-variation hb "m"))

; 7
(define db7 (chord-variation cs "7"))
(define ds7 (chord-variation eb "7"))
(define gb7 (chord-variation fs "7"))
(define ab7 (chord-variation gs "7"))
(define as7 (chord-variation hb "7"))

; 6
(define db6 (chord-variation cs "6"))
(define ds6 (chord-variation eb "6"))
(define gb6 (chord-variation fs "6"))
(define ab6 (chord-variation gs "6"))
(define as6 (chord-variation hb "6"))

; m7
(define dbm7 (chord-variation cs "m7"))
(define dsm7 (chord-variation eb "m7"))
(define gbm7 (chord-variation fs "m7"))
(define abm7 (chord-variation gs "m7"))
(define asm7 (chord-variation hb "m7"))


(end-laml-loading)