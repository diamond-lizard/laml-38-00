; This file is generated by an LAML script based on the LAML tool tools/xml-in-laml/xml-in-laml.scm. DO NOT EDIT!

; lib/xml-in-laml/xml-in-laml.scm is not loaded here. 
; You must load it your self prior to the loading of this file.

(define purchase-xml-transliterate-character-data? #t)
(define purchase-xml-char-transformation-table html-char-transformation-table)
(define purchase-xml-non-transliteration-elements '())
(define purchase-xml-preformatted-text-elements '())
(define purchase-xml-pass-default-dtd-attributes? #f)
(define purchase-xml-accept-only-string-valued-attributes? #t)
(define purchase-xml-document-type-declaration "")
(define purchase-xml-represent-white-space? #t)


; Empty temporary language map
(set! temp-language-map (quote ()))



;;; The validation procedures

(define (purchase-purchase-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0) (dfa (quote (finite-state-automaton 0 (2) #((0 b 1) (0 c 2) (1 b 1) (1 c 2)) #((p  . b) (terminator$$  . c)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "purchase"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "purchase")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "purchase"))))

(define (p-purchase-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "p"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "p")) (if xml-validate-contents? (validate-as-pcdata! contents "p"))))

;;; Make and put XML mirror functions in the temporary language map:
(set! temp-mirror-function (generate-xml-mirror-function purchase-purchase-laml-validate! "purchase" (quote ()) (quote double) (quote purchase) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "purchase" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function purchase-purchase-laml-validate! "purchase" (quote ()) (quote double) (quote purchase) #t purchase!))
(define purchase temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function p-purchase-laml-validate! "p" (quote ()) (quote double) (quote purchase) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "p" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function p-purchase-laml-validate! "p" (quote ()) (quote double) (quote purchase) #t #f))
(define p temp-mirror-function)

; Register the name of the language:
(register-xml-in-laml-language (quote purchase) temp-language-map)

; Define the language variable
(define purchase (activator-via-language-map (quote purchase)))

; Register the XML navigator of the language:
(register-xml-in-laml-navigator (quote purchase) (quote (xml-navigator #((p #(p) #()) (purchase #(p purchase) #())))))

