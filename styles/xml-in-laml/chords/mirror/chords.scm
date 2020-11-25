; This file is generated by an LAML script based on the LAML tool tools/xml-in-laml/xml-in-laml.scm. DO NOT EDIT!

; lib/xml-in-laml/xml-in-laml.scm is not loaded here. 
; You must load it yourself prior to the loading of this file.

(define chords-xml-transliterate-character-data? #t)
(define chords-xml-char-transformation-table html-char-transformation-table)
(define chords-xml-non-transliteration-elements '())
(define chords-xml-preformatted-text-elements '())
(define chords-xml-pass-default-dtd-attributes? #f)
(define chords-xml-accept-only-string-valued-attributes? #t)
(define chords-xml-accept-extended-contents? #f)
(define chords-xml-document-type-declaration "")
(define chords-xml-represent-white-space? #t)
(define chords-xml-duplicated-attribute-handling 'keep-all)


; Empty temporary language map
(set! temp-language-map (quote ()))



;;; The validation procedures

(define (song-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0) (dfa (quote (finite-state-automaton 0 (3) #((0 b 1) (1 c 2) (1 d 3) (2 c 2) (2 d 3)) #((song-front-matters  . b) (song-verses  . c) (terminator$$  . d)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "song"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "song")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "song"))))

(define (song-front-matters-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("transposition" "CDATA" "#IMPLIED") ("indentation" "CDATA" "#IMPLIED") ("font-size" "CDATA" "#IMPLIED") ("compact-lines" ("true" "false") "false") ("rendering-mode" ("normal" "simple") "normal") ("chord-color" "CDATA" "black") ("h-name" ("h" "H" "B" "b") "H") ("cs-or-db" ("cs" "db") "cs") ("ds-or-eb" ("ds" "eb") "eb") ("fs-or-gb" ("fs" "gb") "fs") ("gs-or-ab" ("gs" "ab") "gs") ("as-or-hb" ("as" "hb") "hb")))) (req-n 0) (dfa (quote (finite-state-automaton 0 (4) #((0 b 1) (0 c 2) (0 d 3) (0 e 4) (1 c 2) (1 d 3) (1 e 4) (2 d 3) (2 e 4) (3 e 4)) #((song-author  . c) (song-source  . d) (song-title  . b) (terminator$$  . e)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "song-front-matters"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "song-front-matters")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "song-front-matters"))))

(define (song-verses-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0) (dfa (quote (finite-state-automaton 0 (2) #((0 b 1) (0 c 2) (1 b 1) (1 c 2)) #((terminator$$  . c) (verse  . b)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "song-verses"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "song-verses")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "song-verses"))))

(define (song-title-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "song-title"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "song-title")) (if xml-validate-contents? (validate-as-pcdata! contents "song-title"))))

(define (song-author-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "song-author"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "song-author")) (if xml-validate-contents? (validate-as-pcdata! contents "song-author"))))

(define (song-source-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "song-source"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "song-source")) (if xml-validate-contents? (validate-as-pcdata! contents "song-source"))))

(define (verse-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("n" "CDATA" "#IMPLIED") ("chords" ("true" "false") "true")))) (req-n 0) (dfa (quote (finite-state-automaton 0 (2) #((0 b 1) (0 c 2) (1 b 1) (1 c 2)) #((line  . b) (terminator$$  . c)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "verse"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "verse")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "verse"))))

(define (line-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "line"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "line")) (if xml-validate-contents? (validate-mixed-contents-by-simple-means! contents (quote (c cs d eb e f fs g gs a hb h unison)) "line"))))

(define (unison-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "unison"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "unison")) (if xml-validate-contents? (validate-as-pcdata! contents "unison"))))

(define (c-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("x" "CDATA" "#IMPLIED") ("b" ("c" "cs" "d" "eb" "e" "f" "fs" "g" "gs" "a" "hb" "h") "#IMPLIED") ("o" ("t" "f") "#IMPLIED")))) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "c"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "c")) (if xml-validate-contents? (validate-as-pcdata! contents "c"))))

(define (cs-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("x" "CDATA" "#IMPLIED") ("b" ("c" "cs" "d" "eb" "e" "f" "fs" "g" "gs" "a" "hb" "h") "#IMPLIED") ("o" ("t" "f") "#IMPLIED")))) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "cs"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "cs")) (if xml-validate-contents? (validate-as-pcdata! contents "cs"))))

(define (d-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("x" "CDATA" "#IMPLIED") ("b" ("c" "cs" "d" "eb" "e" "f" "fs" "g" "gs" "a" "hb" "h") "#IMPLIED") ("o" ("t" "f") "#IMPLIED")))) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "d"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "d")) (if xml-validate-contents? (validate-as-pcdata! contents "d"))))

(define (eb-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("x" "CDATA" "#IMPLIED") ("b" ("c" "cs" "d" "eb" "e" "f" "fs" "g" "gs" "a" "hb" "h") "#IMPLIED") ("o" ("t" "f") "#IMPLIED")))) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "eb"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "eb")) (if xml-validate-contents? (validate-as-pcdata! contents "eb"))))

(define (e-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("x" "CDATA" "#IMPLIED") ("b" ("c" "cs" "d" "eb" "e" "f" "fs" "g" "gs" "a" "hb" "h") "#IMPLIED") ("o" ("t" "f") "#IMPLIED")))) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "e"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "e")) (if xml-validate-contents? (validate-as-pcdata! contents "e"))))

(define (f-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("x" "CDATA" "#IMPLIED") ("b" ("c" "cs" "d" "eb" "e" "f" "fs" "g" "gs" "a" "hb" "h") "#IMPLIED") ("o" ("t" "f") "#IMPLIED")))) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "f"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "f")) (if xml-validate-contents? (validate-as-pcdata! contents "f"))))

(define (fs-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("x" "CDATA" "#IMPLIED") ("b" ("c" "cs" "d" "eb" "e" "f" "fs" "g" "gs" "a" "hb" "h") "#IMPLIED") ("o" ("t" "f") "#IMPLIED")))) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "fs"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "fs")) (if xml-validate-contents? (validate-as-pcdata! contents "fs"))))

(define (g-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("x" "CDATA" "#IMPLIED") ("b" ("c" "cs" "d" "eb" "e" "f" "fs" "g" "gs" "a" "hb" "h") "#IMPLIED") ("o" ("t" "f") "#IMPLIED")))) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "g"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "g")) (if xml-validate-contents? (validate-as-pcdata! contents "g"))))

(define (gs-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("x" "CDATA" "#IMPLIED") ("b" ("c" "cs" "d" "eb" "e" "f" "fs" "g" "gs" "a" "hb" "h") "#IMPLIED") ("o" ("t" "f") "#IMPLIED")))) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "gs"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "gs")) (if xml-validate-contents? (validate-as-pcdata! contents "gs"))))

(define (a-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("x" "CDATA" "#IMPLIED") ("b" ("c" "cs" "d" "eb" "e" "f" "fs" "g" "gs" "a" "hb" "h") "#IMPLIED") ("o" ("t" "f") "#IMPLIED")))) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "a"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "a")) (if xml-validate-contents? (validate-as-pcdata! contents "a"))))

(define (hb-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("x" "CDATA" "#IMPLIED") ("b" ("c" "cs" "d" "eb" "e" "f" "fs" "g" "gs" "a" "hb" "h") "#IMPLIED") ("o" ("t" "f") "#IMPLIED")))) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "hb"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "hb")) (if xml-validate-contents? (validate-as-pcdata! contents "hb"))))

(define (h-chords-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("x" "CDATA" "#IMPLIED") ("b" ("c" "cs" "d" "eb" "e" "f" "fs" "g" "gs" "a" "hb" "h") "#IMPLIED") ("o" ("t" "f") "#IMPLIED")))) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "h"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "h")) (if xml-validate-contents? (validate-as-pcdata! contents "h"))))

;;; Make and put XML mirror functions in the temporary language map:
(set! temp-mirror-function (generate-xml-mirror-function song-chords-laml-validate! "song" (quote ()) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "song" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function song-chords-laml-validate! "song" (quote ()) (quote double) (quote chords) #t #f))
(define song temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function song-front-matters-chords-laml-validate! "song-front-matters" (quote (compact-lines "false" rendering-mode "normal" chord-color "black" h-name "H" cs-or-db "cs" ds-or-eb "eb" fs-or-gb "fs" gs-or-ab "gs" as-or-hb "hb")) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "song-front-matters" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function song-front-matters-chords-laml-validate! "song-front-matters" (quote (compact-lines "false" rendering-mode "normal" chord-color "black" h-name "H" cs-or-db "cs" ds-or-eb "eb" fs-or-gb "fs" gs-or-ab "gs" as-or-hb "hb")) (quote double) (quote chords) #t #t))
(define song-front-matters temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function song-verses-chords-laml-validate! "song-verses" (quote ()) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "song-verses" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function song-verses-chords-laml-validate! "song-verses" (quote ()) (quote double) (quote chords) #t #t))
(define song-verses temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function song-title-chords-laml-validate! "song-title" (quote ()) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "song-title" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function song-title-chords-laml-validate! "song-title" (quote ()) (quote double) (quote chords) #t #f))
(define song-title temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function song-author-chords-laml-validate! "song-author" (quote ()) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "song-author" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function song-author-chords-laml-validate! "song-author" (quote ()) (quote double) (quote chords) #t #f))
(define song-author temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function song-source-chords-laml-validate! "song-source" (quote ()) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "song-source" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function song-source-chords-laml-validate! "song-source" (quote ()) (quote double) (quote chords) #t #f))
(define song-source temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function verse-chords-laml-validate! "verse" (quote (chords "true")) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "verse" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function verse-chords-laml-validate! "verse" (quote (chords "true")) (quote double) (quote chords) #t #f))
(define verse temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function line-chords-laml-validate! "line" (quote ()) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "line" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function line-chords-laml-validate! "line" (quote ()) (quote double) (quote chords) #t #f))
(define line temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function unison-chords-laml-validate! "unison" (quote ()) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "unison" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function unison-chords-laml-validate! "unison" (quote ()) (quote double) (quote chords) #t #f))
(define unison temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function c-chords-laml-validate! "c" (quote ()) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "c" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function c-chords-laml-validate! "c" (quote ()) (quote double) (quote chords) #t #f))
(define c temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function cs-chords-laml-validate! "cs" (quote ()) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "cs" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function cs-chords-laml-validate! "cs" (quote ()) (quote double) (quote chords) #t #f))
(define cs temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function d-chords-laml-validate! "d" (quote ()) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "d" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function d-chords-laml-validate! "d" (quote ()) (quote double) (quote chords) #t #f))
(define d temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function eb-chords-laml-validate! "eb" (quote ()) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "eb" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function eb-chords-laml-validate! "eb" (quote ()) (quote double) (quote chords) #t #f))
(define eb temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function e-chords-laml-validate! "e" (quote ()) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "e" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function e-chords-laml-validate! "e" (quote ()) (quote double) (quote chords) #t #f))
(define e temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function f-chords-laml-validate! "f" (quote ()) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "f" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function f-chords-laml-validate! "f" (quote ()) (quote double) (quote chords) #t #f))
(define f temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function fs-chords-laml-validate! "fs" (quote ()) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "fs" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function fs-chords-laml-validate! "fs" (quote ()) (quote double) (quote chords) #t #f))
(define fs temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function g-chords-laml-validate! "g" (quote ()) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "g" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function g-chords-laml-validate! "g" (quote ()) (quote double) (quote chords) #t #f))
(define g temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function gs-chords-laml-validate! "gs" (quote ()) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "gs" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function gs-chords-laml-validate! "gs" (quote ()) (quote double) (quote chords) #t #f))
(define gs temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function a-chords-laml-validate! "a" (quote ()) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "a" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function a-chords-laml-validate! "a" (quote ()) (quote double) (quote chords) #t #f))
(define a temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function hb-chords-laml-validate! "hb" (quote ()) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "hb" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function hb-chords-laml-validate! "hb" (quote ()) (quote double) (quote chords) #t #f))
(define hb temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function h-chords-laml-validate! "h" (quote ()) (quote double) (quote chords) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "h" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function h-chords-laml-validate! "h" (quote ()) (quote double) (quote chords) #t #f))
(define h temp-mirror-function)

; Register the name of the language:
(register-xml-in-laml-language (quote chords) temp-language-map)

; Define the language variable
(define chords (activator-via-language-map (quote chords)))

; Register the XML navigator of the language:
(register-xml-in-laml-navigator (quote chords) (quote (xml-navigator #((a #(a) #(b o x)) (c #(c) #(b o x)) (cs #(cs) #(b o x)) (d #(d) #(b o x)) (e #(e) #(b o x)) (eb #(eb) #(b o x)) (f #(f) #(b o x)) (fs #(fs) #(b o x)) (g #(g) #(b o x)) (gs #(gs) #(b o x)) (h #(h) #(b o x)) (hb #(hb) #(b o x)) (line #(a c cs d e eb f fs g gs h hb line unison) #(b o x)) (song #(a c cs d e eb f fs g gs h hb line song song-author song-front-matters song-source song-title song-verses unison verse) #(as-or-hb b chord-color chords compact-lines cs-or-db ds-or-eb font-size fs-or-gb gs-or-ab h-name indentation n o rendering-mode transposition x)) (song-author #(song-author) #()) (song-front-matters #(song-author song-front-matters song-source song-title) #(as-or-hb chord-color compact-lines cs-or-db ds-or-eb font-size fs-or-gb gs-or-ab h-name indentation rendering-mode transposition)) (song-source #(song-source) #()) (song-title #(song-title) #()) (song-verses #(a c cs d e eb f fs g gs h hb line song-verses unison verse) #(b chords n o x)) (unison #(unison) #()) (verse #(a c cs d e eb f fs g gs h hb line unison verse) #(b chords n o x))))))

; Register the validation procedures of the language
(register-xml-in-laml-validators (quote chords) (vector (list "a" a-chords-laml-validate!) (list "c" c-chords-laml-validate!) (list "cs" cs-chords-laml-validate!) (list "d" d-chords-laml-validate!) (list "e" e-chords-laml-validate!) (list "eb" eb-chords-laml-validate!) (list "f" f-chords-laml-validate!) (list "fs" fs-chords-laml-validate!) (list "g" g-chords-laml-validate!) (list "gs" gs-chords-laml-validate!) (list "h" h-chords-laml-validate!) (list "hb" hb-chords-laml-validate!) (list "line" line-chords-laml-validate!) (list "song" song-chords-laml-validate!) (list "song-author" song-author-chords-laml-validate!) (list "song-front-matters" song-front-matters-chords-laml-validate!) (list "song-source" song-source-chords-laml-validate!) (list "song-title" song-title-chords-laml-validate!) (list "song-verses" song-verses-chords-laml-validate!) (list "unison" unison-chords-laml-validate!) (list "verse" verse-chords-laml-validate!)))

; Register the content model map of the language.
; This makes the content model available for LAML at runtime.
(register-xml-in-laml-content-models (quote chords) (quote #(("a" (mixed-content pcdata)) ("c" (mixed-content pcdata)) ("cs" (mixed-content pcdata)) ("d" (mixed-content pcdata)) ("e" (mixed-content pcdata)) ("eb" (mixed-content pcdata)) ("f" (mixed-content pcdata)) ("fs" (mixed-content pcdata)) ("g" (mixed-content pcdata)) ("gs" (mixed-content pcdata)) ("h" (mixed-content pcdata)) ("hb" (mixed-content pcdata)) ("line" (mixed-content (choice pcdata c cs d eb e f fs g gs a hb h unison))) ("song" (element-content (seq one (name one song-front-matters) (name zero-or-more song-verses)))) ("song-author" (mixed-content pcdata)) ("song-front-matters" (element-content (seq one (name optional song-title) (name optional song-author) (name optional song-source)))) ("song-source" (mixed-content pcdata)) ("song-title" (mixed-content pcdata)) ("song-verses" (element-content (seq one (name zero-or-more verse)))) ("unison" (mixed-content pcdata)) ("verse" (element-content (seq zero-or-more (name one line)))))))

; Register the action procedure map of the language.
(register-xml-in-laml-action-procedures (quote chords) (vector (list "song-front-matters" (lambda (ast) (song-front-matters! ast))) (list "song-verses" (lambda (ast) (song-verses! ast)))))
