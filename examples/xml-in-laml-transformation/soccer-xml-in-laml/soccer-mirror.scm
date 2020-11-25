; This file is generated by an LAML script based on the LAML tool tools/xml-in-laml/xml-in-laml.scm. DO NOT EDIT!

; lib/xml-in-laml/xml-in-laml.scm is not loaded here. 
; You must load it yourself prior to the loading of this file.

(define soccer-xml-transliterate-character-data? #t)
(define soccer-xml-char-transformation-table html-char-transformation-table)
(define soccer-xml-non-transliteration-elements '())
(define soccer-xml-preformatted-text-elements '())
(define soccer-xml-pass-default-dtd-attributes? #f)
(define soccer-xml-accept-only-string-valued-attributes? #t)
(define soccer-xml-accept-extended-contents? #f)
(define soccer-xml-document-type-declaration "")
(define soccer-xml-represent-white-space? #f)
(define soccer-xml-duplicated-attribute-handling 'keep-all)


; Empty temporary language map
(set! temp-language-map (quote ()))



;;; The validation procedures

(define (results-soccer-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("group" "CDATA" "#REQUIRED")))) (req-n 1) (dfa (quote (finite-state-automaton 0 (2) #((0 b 1) (0 c 2) (1 b 1) (1 c 2)) #((match  . b) (terminator$$  . c)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "results"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "results")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "results"))))

(define (match-soccer-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0) (dfa (quote (finite-state-automaton 0 (4) #((0 b 1) (1 c 2) (2 c 3) (3 d 4)) #((date  . b) (team  . c) (terminator$$  . d)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "match"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "match")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "match"))))

(define (date-soccer-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "date"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "date")) (if xml-validate-contents? (validate-as-pcdata! contents "date"))))

(define (team-soccer-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("score" "CDATA" "#REQUIRED")))) (req-n 1)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "team"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "team")) (if xml-validate-contents? (validate-as-pcdata! contents "team"))))

;;; Make and put XML mirror functions in the temporary language map:
(set! temp-mirror-function (generate-xml-mirror-function results-soccer-laml-validate! "results" (quote ()) (quote double) (quote soccer) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "results" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function results-soccer-laml-validate! "results" (quote ()) (quote double) (quote soccer) #t #t))
(define results temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function match-soccer-laml-validate! "match" (quote ()) (quote double) (quote soccer) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "match" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function match-soccer-laml-validate! "match" (quote ()) (quote double) (quote soccer) #t #f))
(define match temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function date-soccer-laml-validate! "date" (quote ()) (quote double) (quote soccer) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "date" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function date-soccer-laml-validate! "date" (quote ()) (quote double) (quote soccer) #t #f))
(define date temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function team-soccer-laml-validate! "team" (quote ()) (quote double) (quote soccer) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "team" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function team-soccer-laml-validate! "team" (quote ()) (quote double) (quote soccer) #t #f))
(define team temp-mirror-function)

; Register the name of the language:
(register-xml-in-laml-language (quote soccer) temp-language-map)

; Define the language variable
(define soccer (activator-via-language-map (quote soccer)))

; Register the XML navigator of the language:
(register-xml-in-laml-navigator (quote soccer) (quote (xml-navigator #((date #(date) #()) (match #(date match team) #(score)) (results #(date match results team) #(group score)) (team #(team) #(score))))))

; Register the validation procedures of the language
(register-xml-in-laml-validators (quote soccer) (vector (list "date" date-soccer-laml-validate!) (list "match" match-soccer-laml-validate!) (list "results" results-soccer-laml-validate!) (list "team" team-soccer-laml-validate!)))

; Register the content model map of the language.
; This makes the content model available for LAML at runtime.
(register-xml-in-laml-content-models (quote soccer) (quote #(("date" (mixed-content pcdata)) ("match" (element-content (seq one (name one date) (name one team) (name one team)))) ("results" (element-content (seq zero-or-more (name one match)))) ("team" (mixed-content pcdata)))))

; Register the action procedure map of the language.
(register-xml-in-laml-action-procedures (quote soccer) (vector (list "results" (lambda (ast) (results! ast)))))

