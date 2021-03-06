; This file is generated by an LAML script based on the LAML tool tools/xml-in-laml/xml-in-laml.scm. DO NOT EDIT!

; lib/xml-in-laml/xml-in-laml.scm is not loaded here. 
; You must load it yourself prior to the loading of this file.

(define bike-management-xml-transliterate-character-data? #t)
(define bike-management-xml-char-transformation-table html-char-transformation-table)
(define bike-management-xml-non-transliteration-elements '())
(define bike-management-xml-preformatted-text-elements '())
(define bike-management-xml-pass-default-dtd-attributes? #f)
(define bike-management-xml-accept-only-string-valued-attributes? #t)
(define bike-management-xml-accept-extended-contents? #f)
(define bike-management-xml-document-type-declaration "")
(define bike-management-xml-represent-white-space? #t)
(define bike-management-xml-duplicated-attribute-handling 'keep-all)


; Empty temporary language map
(set! temp-language-map (quote ()))



;;; The validation procedures

(define (bikes-bike-management-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0) (dfa (quote (finite-state-automaton 0 (2) #((0 b 1) (0 c 2) (1 b 1) (1 c 2)) #((bike  . b) (terminator$$  . c)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "bikes"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "bikes")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "bikes"))))

(define (bike-bike-management-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("kind" ("mountain-bike, racer-bike, tourist-bike, other") "tourist-bike")))) (req-n 0) (dfa (quote (finite-state-automaton 0 (6) #((0 b 1) (1 c 2) (2 c 3) (2 d 4) (2 e 5) (2 f 6) (3 c 3) (3 d 4) (3 e 5) (3 f 6) (4 d 4) (4 e 5) (4 f 6) (5 e 5) (5 f 6)) #((brake  . d) (frame  . b) (lock  . e) (terminator$$  . f) (wheel  . c)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "bike"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "bike")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "bike"))))

(define (frame-bike-management-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("frame-number" "CDATA" "#REQUIRED")))) (req-n 1)) (xml-check-for-empty-contents! contents "frame") (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "frame"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "frame"))))

(define (wheel-bike-management-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("size" "CDATA" "#REQUIRED") ("tube-kind" "CDATA" "#IMPLIED")))) (req-n 1)) (xml-check-for-empty-contents! contents "wheel") (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "wheel"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "wheel"))))

(define (brake-bike-management-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("kind" "CDATA" "#IMPLIED") ("brand" "CDATA" "#IMPLIED")))) (req-n 0)) (xml-check-for-empty-contents! contents "brake") (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "brake"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "brake"))))

(define (lock-bike-management-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("insurance-approved" ("true" "false") "#REQUIRED") ("brand" "CDATA" "#IMPLIED")))) (req-n 1)) (xml-check-for-empty-contents! contents "lock") (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "lock"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "lock"))))

;;; Make and put XML mirror functions in the temporary language map:
(set! temp-mirror-function (generate-xml-mirror-function bikes-bike-management-laml-validate! "bikes" (quote ()) (quote double) (quote bike-management) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "bikes" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function bikes-bike-management-laml-validate! "bikes" (quote ()) (quote double) (quote bike-management) #t #t))
(define bikes temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function bike-bike-management-laml-validate! "bike" (quote (kind "tourist-bike")) (quote double) (quote bike-management) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "bike" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function bike-bike-management-laml-validate! "bike" (quote (kind "tourist-bike")) (quote double) (quote bike-management) #t #f))
(define bike temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function frame-bike-management-laml-validate! "frame" (quote ()) (quote single) (quote bike-management) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "frame" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function frame-bike-management-laml-validate! "frame" (quote ()) (quote single) (quote bike-management) #t #f))
(define frame temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function wheel-bike-management-laml-validate! "wheel" (quote ()) (quote single) (quote bike-management) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "wheel" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function wheel-bike-management-laml-validate! "wheel" (quote ()) (quote single) (quote bike-management) #t #f))
(define wheel temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function brake-bike-management-laml-validate! "brake" (quote ()) (quote single) (quote bike-management) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "brake" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function brake-bike-management-laml-validate! "brake" (quote ()) (quote single) (quote bike-management) #t #f))
(define brake temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function lock-bike-management-laml-validate! "lock" (quote ()) (quote single) (quote bike-management) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "lock" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function lock-bike-management-laml-validate! "lock" (quote ()) (quote single) (quote bike-management) #t #f))
(define lock temp-mirror-function)

; Register the name of the language:
(register-xml-in-laml-language (quote bike-management) temp-language-map)

; Define the language variable
(define bike-management (activator-via-language-map (quote bike-management)))

; Register the XML navigator of the language:
(register-xml-in-laml-navigator (quote bike-management) (quote (xml-navigator #((bike #(bike brake frame lock wheel) #(brand frame-number insurance-approved kind size tube-kind)) (bikes #(bike bikes brake frame lock wheel) #(brand frame-number insurance-approved kind size tube-kind)) (brake #(brake) #(brand kind)) (frame #(frame) #(frame-number)) (lock #(lock) #(brand insurance-approved)) (wheel #(wheel) #(size tube-kind))))))

; Register the validation procedures of the language
(register-xml-in-laml-validators (quote bike-management) (vector (list "bike" bike-bike-management-laml-validate!) (list "bikes" bikes-bike-management-laml-validate!) (list "brake" brake-bike-management-laml-validate!) (list "frame" frame-bike-management-laml-validate!) (list "lock" lock-bike-management-laml-validate!) (list "wheel" wheel-bike-management-laml-validate!)))

; Register the content model map of the language.
; This makes the content model available for LAML at runtime.
(register-xml-in-laml-content-models (quote bike-management) (quote #(("bike" (element-content (seq one (name one frame) (name one-or-more wheel) (name zero-or-more brake) (name zero-or-more lock)))) ("bikes" (element-content (seq zero-or-more (name one bike)))) ("brake" empty) ("frame" empty) ("lock" empty) ("wheel" empty))))

; Register the action procedure map of the language.
(register-xml-in-laml-action-procedures (quote bike-management) (vector (list "bikes" (lambda (ast) (bikes! ast)))))

