; This file is generated by an LAML script based on the LAML tool tools/xml-in-laml/xml-in-laml.scm. DO NOT EDIT!

; lib/xml-in-laml/xml-in-laml.scm is not loaded here. 
; You must load it yourself prior to the loading of this file.

(define course-plan-1-xml-transliterate-character-data? #t)
(define course-plan-1-xml-char-transformation-table html-char-transformation-table)
(define course-plan-1-xml-non-transliteration-elements '())
(define course-plan-1-xml-preformatted-text-elements '())
(define course-plan-1-xml-pass-default-dtd-attributes? #f)
(define course-plan-1-xml-accept-only-string-valued-attributes? #t)
(define course-plan-1-xml-accept-extended-contents? #f)
(define course-plan-1-xml-document-type-declaration "")
(define course-plan-1-xml-represent-white-space? #f)
(define course-plan-1-xml-duplicated-attribute-handling 'keep-all)


; Empty temporary language map
(set! temp-language-map (quote ()))



;;; The validation procedures

(define (course-plan-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0) (dfa (quote (finite-state-automaton 0 (4) #((0 b 1) (0 c 2) (1 c 2) (2 d 3) (3 e 4)) #((course-info  . c) (course-intro  . b) (lecture-plan-list  . d) (terminator$$  . e)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "course-plan"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "course-plan")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "course-plan"))))

(define (course-info-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("course-title" "CDATA" "#REQUIRED") ("brief-course-title" "CDATA" "#REQUIRED") ("course-semester" "CDATA" "#REQUIRED") ("brief-course-semester" "CDATA" "#REQUIRED") ("teacher-name" "CDATA" "#REQUIRED") ("author-home-url" "CDATA" "#REQUIRED") ("course-id" "Symbol" "IMPLIED") ("language-preference" ("english" "danish") "english") ("course-directory" "CDATA" "#IMPLIED") ("course-url-prefix" "CDATA" "#IMPLIED") ("make-overview-pages" ("true" "false") "false") ("color-scheme" ("purple-yellow" "red-yellow" "black-white" "blue-grey" "green-brown" "brownish-yellow" "orange-blue" "orange-brownish" "olive-yellow" "blue-white" "blue-creme") "green-brown") ("last-lecture-number" "CDATA" "#IMPLIED") ("relative-source-destination-path" "CDATA" "#IMPLIED") ("exercise-model" ("this-exercise-slot" "next-exercise-slot") "this-exercise-slot") ("floating-exercises" ("true" "false") "false") ("calendar-start-year" "CDATA" "#IMPLIED") ("calendar-start-month" "CDATA" "#IMPLIED") ("calendar-number-of-months" "CDATA" "#IMPLIED") ("url-of-external-calendar" "CDATA" "#IMPLIED") ("new-or-updated-n-days" "CDATA" "#IMPLIED") ("news-flash-string" "CDATA" "#IMPLIED") ("news-flash-url" "CDATA" "#IMPLIED") ("news-flash-level" "CDATA" "1") ("shortcut-icon" "CDATA" "#IMPLIED")))) (req-n 6) (dfa (quote (finite-state-automaton 0 (6) #((0 b 1) (0 c 2) (1 c 2) (2 d 3) (2 e 4) (2 f 5) (2 g 6) (3 e 4) (3 f 5) (3 g 6) (4 f 5) (4 g 6) (5 g 6)) #((bottom-links  . f) (index-links  . e) (lecture-list  . c) (subject-list  . d) (terminator$$  . g) (time-list  . b)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "course-info"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "course-info")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "course-info"))))

(define (time-list-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0) (dfa (quote (finite-state-automaton 0 (2) #((0 b 1) (0 c 2) (1 b 1) (1 c 2)) #((terminator$$  . c) (time  . b)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "time-list"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "time-list")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "time-list"))))

(define (time-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("year" "CDATA" "#REQUIRED") ("month" "CDATA" "#REQUIRED") ("day" "CDATA" "#REQUIRED") ("hour" "CDATA" "#REQUIRED") ("minute" "CDATA" "0") ("second" "CDATA" "0")))) (req-n 4)) (xml-check-for-empty-contents! contents "time") (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "time"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "time"))))

(define (lecture-list-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0) (dfa (quote (finite-state-automaton 0 (2) #((0 b 1) (0 c 2) (1 b 1) (1 c 2)) #((lecture  . b) (terminator$$  . c)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "lecture-list"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "lecture-list")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "lecture-list"))))

(define (lecture-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("lecture-id" "CDATA" "#REQUIRED") ("plenum-start" "CDATA" "#REQUIRED") ("plenum-length" "CDATA" "#REQUIRED") ("exercise-start" "CDATA" "#REQUIRED") ("exercise-length" "CDATA" "#REQUIRED") ("room" "CDATA" "#REQUIRED") ("subject-id" "CDATA" "#IMPLIED") ("title" "CDATA" "#IMPLIED") ("href" "CDATA" "#IMPLIED") ("new-as-of" "CDATA" "#IMPLIED") ("updated-as-of" "CDATA" "#IMPLIED")))) (req-n 6) (dfa (quote (finite-state-automaton 0 (3) #((0 b 1) (0 c 2) (0 d 3) (1 c 2) (1 d 3) (2 d 3)) #((description  . c) (terminator$$  . d) (time  . b)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "lecture"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "lecture")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "lecture"))))

(define (subject-list-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0) (dfa (quote (finite-state-automaton 0 (2) #((0 b 1) (0 c 2) (1 b 1) (1 c 2)) #((subject  . b) (terminator$$  . c)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "subject-list"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "subject-list")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "subject-list"))))

(define (subject-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("id" "CDATA" "#REQUIRED") ("title" "CDATA" "#REQUIRED") ("href" "CDATA" "#IMPLIED")))) (req-n 2) (dfa (quote (finite-state-automaton 0 (2) #((0 b 1) (1 c 2)) #((description  . b) (terminator$$  . c)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "subject"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "subject")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "subject"))))

(define (description-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "description"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "description")) (if xml-validate-contents? (validate-mixed-contents-by-simple-means! contents (quote (div span ol ul p)) "description"))))

(define (index-links-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0) (dfa (quote (finite-state-automaton 0 (2) #((0 b 1) (0 c 2) (1 b 1) (1 c 2)) #((link-entry  . b) (terminator$$  . c)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "index-links"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "index-links")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "index-links"))))

(define (bottom-links-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0) (dfa (quote (finite-state-automaton 0 (2) #((0 b 1) (0 c 2) (1 b 1) (1 c 2)) #((link-entry  . b) (terminator$$  . c)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "bottom-links"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "bottom-links")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "bottom-links"))))

(define (link-entry-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("href" "CDATA" "#REQUIRED") ("target" "CDATA" "main") ("new-as-of" "CDATA" "#IMPLIED") ("updated-as-of" "CDATA" "#IMPLIED")))) (req-n 1)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "link-entry"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "link-entry")) (if xml-validate-contents? (validate-as-pcdata! contents "link-entry"))))

(define (course-intro-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "course-intro"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "course-intro")) (if xml-validate-contents? (validate-mixed-contents-by-simple-means! contents (quote (div span ol ul p)) "course-intro"))))

(define (lecture-plan-list-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0) (dfa (quote (finite-state-automaton 0 (2) #((0 b 1) (0 c 2) (1 b 1) (1 c 2)) #((lecture-plan  . b) (terminator$$  . c)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "lecture-plan-list"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "lecture-plan-list")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "lecture-plan-list"))))

(define (lecture-plan-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("lecture-id" "CDATA" "#REQUIRED")))) (req-n 1) (dfa (quote (finite-state-automaton 0 (6) #((0 b 1) (0 c 2) (0 d 3) (0 e 4) (0 f 5) (0 g 6) (1 c 2) (1 d 3) (1 e 4) (1 f 5) (1 g 6) (2 d 3) (2 e 4) (2 f 5) (2 g 6) (3 e 4) (3 f 5) (3 g 6) (4 f 5) (4 g 6) (5 g 6)) #((exercises  . d) (literature  . b) (misc  . f) (reading-guide  . c) (references  . e) (terminator$$  . g)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "lecture-plan"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "lecture-plan")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "lecture-plan"))))

(define (literature-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "literature"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "literature")) (if xml-validate-contents? (validate-mixed-contents-by-simple-means! contents (quote (div span ol ul p)) "literature"))))

(define (reading-guide-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "reading-guide"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "reading-guide")) (if xml-validate-contents? (validate-mixed-contents-by-simple-means! contents (quote (div span ol ul p)) "reading-guide"))))

(define (exercises-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "exercises"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "exercises")) (if xml-validate-contents? (validate-mixed-contents-by-simple-means! contents (quote (div span ol ul p)) "exercises"))))

(define (references-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "references"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "references")) (if xml-validate-contents? (validate-mixed-contents-by-simple-means! contents (quote (div span ol ul p)) "references"))))

(define (misc-course-plan-1-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "misc"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "misc")) (if xml-validate-contents? (validate-mixed-contents-by-simple-means! contents (quote (div span ol ul p)) "misc"))))

;;; Make and put XML mirror functions in the temporary language map:
(set! temp-mirror-function (generate-xml-mirror-function course-plan-course-plan-1-laml-validate! "course-plan" (quote ()) (quote double) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "course-plan" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function course-plan-course-plan-1-laml-validate! "course-plan" (quote ()) (quote double) (quote course-plan-1) #t #t))
(define course-plan temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function course-info-course-plan-1-laml-validate! "course-info" (quote (course-id "IMPLIED" language-preference "english" make-overview-pages "false" color-scheme "green-brown" exercise-model "this-exercise-slot" floating-exercises "false" news-flash-level "1")) (quote double) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "course-info" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function course-info-course-plan-1-laml-validate! "course-info" (quote (course-id "IMPLIED" language-preference "english" make-overview-pages "false" color-scheme "green-brown" exercise-model "this-exercise-slot" floating-exercises "false" news-flash-level "1")) (quote double) (quote course-plan-1) #t #f))
(define course-info temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function time-list-course-plan-1-laml-validate! "time-list" (quote ()) (quote double) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "time-list" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function time-list-course-plan-1-laml-validate! "time-list" (quote ()) (quote double) (quote course-plan-1) #t #f))
(define time-list temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function time-course-plan-1-laml-validate! "time" (quote (minute "0" second "0")) (quote single) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "time" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function time-course-plan-1-laml-validate! "time" (quote (minute "0" second "0")) (quote single) (quote course-plan-1) #t #f))
(define time temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function lecture-list-course-plan-1-laml-validate! "lecture-list" (quote ()) (quote double) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "lecture-list" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function lecture-list-course-plan-1-laml-validate! "lecture-list" (quote ()) (quote double) (quote course-plan-1) #t #f))
(define lecture-list temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function lecture-course-plan-1-laml-validate! "lecture" (quote ()) (quote double) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "lecture" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function lecture-course-plan-1-laml-validate! "lecture" (quote ()) (quote double) (quote course-plan-1) #t #f))
(define lecture temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function subject-list-course-plan-1-laml-validate! "subject-list" (quote ()) (quote double) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "subject-list" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function subject-list-course-plan-1-laml-validate! "subject-list" (quote ()) (quote double) (quote course-plan-1) #t #f))
(define subject-list temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function subject-course-plan-1-laml-validate! "subject" (quote ()) (quote double) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "subject" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function subject-course-plan-1-laml-validate! "subject" (quote ()) (quote double) (quote course-plan-1) #t #f))
(define subject temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function description-course-plan-1-laml-validate! "description" (quote ()) (quote double) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "description" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function description-course-plan-1-laml-validate! "description" (quote ()) (quote double) (quote course-plan-1) #t #f))
(define description temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function index-links-course-plan-1-laml-validate! "index-links" (quote ()) (quote double) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "index-links" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function index-links-course-plan-1-laml-validate! "index-links" (quote ()) (quote double) (quote course-plan-1) #t #f))
(define index-links temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function bottom-links-course-plan-1-laml-validate! "bottom-links" (quote ()) (quote double) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "bottom-links" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function bottom-links-course-plan-1-laml-validate! "bottom-links" (quote ()) (quote double) (quote course-plan-1) #t #f))
(define bottom-links temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function link-entry-course-plan-1-laml-validate! "link-entry" (quote (target "main")) (quote double) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "link-entry" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function link-entry-course-plan-1-laml-validate! "link-entry" (quote (target "main")) (quote double) (quote course-plan-1) #t #f))
(define link-entry temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function course-intro-course-plan-1-laml-validate! "course-intro" (quote ()) (quote double) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "course-intro" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function course-intro-course-plan-1-laml-validate! "course-intro" (quote ()) (quote double) (quote course-plan-1) #t #f))
(define course-intro temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function lecture-plan-list-course-plan-1-laml-validate! "lecture-plan-list" (quote ()) (quote double) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "lecture-plan-list" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function lecture-plan-list-course-plan-1-laml-validate! "lecture-plan-list" (quote ()) (quote double) (quote course-plan-1) #t #f))
(define lecture-plan-list temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function lecture-plan-course-plan-1-laml-validate! "lecture-plan" (quote ()) (quote double) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "lecture-plan" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function lecture-plan-course-plan-1-laml-validate! "lecture-plan" (quote ()) (quote double) (quote course-plan-1) #t #f))
(define lecture-plan temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function literature-course-plan-1-laml-validate! "literature" (quote ()) (quote double) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "literature" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function literature-course-plan-1-laml-validate! "literature" (quote ()) (quote double) (quote course-plan-1) #t #f))
(define literature temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function reading-guide-course-plan-1-laml-validate! "reading-guide" (quote ()) (quote double) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "reading-guide" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function reading-guide-course-plan-1-laml-validate! "reading-guide" (quote ()) (quote double) (quote course-plan-1) #t #f))
(define reading-guide temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function exercises-course-plan-1-laml-validate! "exercises" (quote ()) (quote double) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "exercises" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function exercises-course-plan-1-laml-validate! "exercises" (quote ()) (quote double) (quote course-plan-1) #t #f))
(define exercises temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function references-course-plan-1-laml-validate! "references" (quote ()) (quote double) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "references" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function references-course-plan-1-laml-validate! "references" (quote ()) (quote double) (quote course-plan-1) #t #f))
(define references temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function misc-course-plan-1-laml-validate! "misc" (quote ()) (quote double) (quote course-plan-1) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "misc" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function misc-course-plan-1-laml-validate! "misc" (quote ()) (quote double) (quote course-plan-1) #t #f))
(define misc temp-mirror-function)

; Register the name of the language:
(register-xml-in-laml-language (quote course-plan-1) temp-language-map)

; Define the language variable
(define course-plan-1 (activator-via-language-map (quote course-plan-1)))

; Register the XML navigator of the language:
(register-xml-in-laml-navigator (quote course-plan-1) (quote (xml-navigator #((bottom-links #(bottom-links link-entry) #(href new-as-of target updated-as-of)) (course-info #(bottom-links course-info description div index-links lecture lecture-list link-entry ol p span subject subject-list time time-list ul) #(author-home-url brief-course-semester brief-course-title calendar-number-of-months calendar-start-month calendar-start-year color-scheme course-directory course-id course-semester course-title course-url-prefix day exercise-length exercise-model exercise-start floating-exercises hour href id language-preference last-lecture-number lecture-id make-overview-pages minute month new-as-of new-or-updated-n-days news-flash-level news-flash-string news-flash-url plenum-length plenum-start relative-source-destination-path room second shortcut-icon subject-id target teacher-name title updated-as-of url-of-external-calendar year)) (course-intro #(course-intro div ol p span ul) #()) (course-plan #(bottom-links course-info course-intro course-plan description div exercises index-links lecture lecture-list lecture-plan lecture-plan-list link-entry literature misc ol p reading-guide references span subject subject-list time time-list ul) #(author-home-url brief-course-semester brief-course-title calendar-number-of-months calendar-start-month calendar-start-year color-scheme course-directory course-id course-semester course-title course-url-prefix day exercise-length exercise-model exercise-start floating-exercises hour href id language-preference last-lecture-number lecture-id make-overview-pages minute month new-as-of new-or-updated-n-days news-flash-level news-flash-string news-flash-url plenum-length plenum-start relative-source-destination-path room second shortcut-icon subject-id target teacher-name title updated-as-of url-of-external-calendar year)) (description #(description div ol p span ul) #()) (exercises #(div exercises ol p span ul) #()) (index-links #(index-links link-entry) #(href new-as-of target updated-as-of)) (lecture #(description div lecture ol p span time ul) #(day exercise-length exercise-start hour href lecture-id minute month new-as-of plenum-length plenum-start room second subject-id title updated-as-of year)) (lecture-list #(description div lecture lecture-list ol p span time ul) #(day exercise-length exercise-start hour href lecture-id minute month new-as-of plenum-length plenum-start room second subject-id title updated-as-of year)) (lecture-plan #(div exercises lecture-plan literature misc ol p reading-guide references span ul) #(lecture-id)) (lecture-plan-list #(div exercises lecture-plan lecture-plan-list literature misc ol p reading-guide references span ul) #(lecture-id)) (link-entry #(link-entry) #(href new-as-of target updated-as-of)) (literature #(div literature ol p span ul) #()) (misc #(div misc ol p span ul) #()) (reading-guide #(div ol p reading-guide span ul) #()) (references #(div ol p references span ul) #()) (subject #(description div ol p span subject ul) #(href id title)) (subject-list #(description div ol p span subject subject-list ul) #(href id title)) (time #(time) #(day hour minute month second year)) (time-list #(time time-list) #(day hour minute month second year))))))

; Register the validation procedures of the language
(register-xml-in-laml-validators (quote course-plan-1) (vector (list "bottom-links" bottom-links-course-plan-1-laml-validate!) (list "course-info" course-info-course-plan-1-laml-validate!) (list "course-intro" course-intro-course-plan-1-laml-validate!) (list "course-plan" course-plan-course-plan-1-laml-validate!) (list "description" description-course-plan-1-laml-validate!) (list "exercises" exercises-course-plan-1-laml-validate!) (list "index-links" index-links-course-plan-1-laml-validate!) (list "lecture" lecture-course-plan-1-laml-validate!) (list "lecture-list" lecture-list-course-plan-1-laml-validate!) (list "lecture-plan" lecture-plan-course-plan-1-laml-validate!) (list "lecture-plan-list" lecture-plan-list-course-plan-1-laml-validate!) (list "link-entry" link-entry-course-plan-1-laml-validate!) (list "literature" literature-course-plan-1-laml-validate!) (list "misc" misc-course-plan-1-laml-validate!) (list "reading-guide" reading-guide-course-plan-1-laml-validate!) (list "references" references-course-plan-1-laml-validate!) (list "subject" subject-course-plan-1-laml-validate!) (list "subject-list" subject-list-course-plan-1-laml-validate!) (list "time" time-course-plan-1-laml-validate!) (list "time-list" time-list-course-plan-1-laml-validate!)))

; Register the content model map of the language.
; This makes the content model available for LAML at runtime.
(register-xml-in-laml-content-models (quote course-plan-1) (quote #(("bottom-links" (element-content (seq zero-or-more (name one link-entry)))) ("course-info" (element-content (seq one (name optional time-list) (name one lecture-list) (name optional subject-list) (name optional index-links) (name optional bottom-links)))) ("course-intro" (mixed-content (choice pcdata div span ol ul p))) ("course-plan" (element-content (seq one (name optional course-intro) (name one course-info) (name one lecture-plan-list)))) ("description" (mixed-content (choice pcdata div span ol ul p))) ("exercises" (mixed-content (choice pcdata div span ol ul p))) ("index-links" (element-content (seq zero-or-more (name one link-entry)))) ("lecture" (element-content (seq one (name optional time) (name optional description)))) ("lecture-list" (element-content (seq zero-or-more (name one lecture)))) ("lecture-plan" (element-content (seq one (name optional literature) (name optional reading-guide) (name optional exercises) (name optional references) (name optional misc)))) ("lecture-plan-list" (element-content (seq zero-or-more (name one lecture-plan)))) ("link-entry" (mixed-content pcdata)) ("literature" (mixed-content (choice pcdata div span ol ul p))) ("misc" (mixed-content (choice pcdata div span ol ul p))) ("reading-guide" (mixed-content (choice pcdata div span ol ul p))) ("references" (mixed-content (choice pcdata div span ol ul p))) ("subject" (element-content (seq one (name one description)))) ("subject-list" (element-content (seq zero-or-more (name one subject)))) ("time" empty) ("time-list" (element-content (seq zero-or-more (name one time)))))))

; Register the action procedure map of the language.
(register-xml-in-laml-action-procedures (quote course-plan-1) (vector (list "course-plan" (lambda (ast) (course-plan! ast)))))

