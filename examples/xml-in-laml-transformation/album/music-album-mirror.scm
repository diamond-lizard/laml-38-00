; This file is generated by an LAML script based on the LAML tool tools/xml-in-laml/xml-in-laml.scm. DO NOT EDIT!

; lib/xml-in-laml/xml-in-laml.scm is not loaded here. 
; You must load it yourself prior to the loading of this file.

(define music-album-xml-transliterate-character-data? #t)
(define music-album-xml-char-transformation-table html-char-transformation-table)
(define music-album-xml-non-transliteration-elements '())
(define music-album-xml-preformatted-text-elements '())
(define music-album-xml-pass-default-dtd-attributes? #f)
(define music-album-xml-accept-only-string-valued-attributes? #t)
(define music-album-xml-accept-extended-contents? #f)
(define music-album-xml-document-type-declaration "")
(define music-album-xml-represent-white-space? #t)
(define music-album-xml-duplicated-attribute-handling 'keep-all)


; Empty temporary language map
(set! temp-language-map (quote ()))



;;; The validation procedures

(define (album-music-album-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0) (dfa (quote (finite-state-automaton 0 (10) #((0 b 1) (1 c 2) (2 d 3) (2 e 4) (3 e 4) (4 f 5) (5 f 6) (5 g 7) (6 f 6) (6 g 7) (7 h 8) (8 i 9) (9 j 10)) #((artist  . c) (catalogno  . f) (coverart  . e) (notes  . i) (personnel  . g) (recordingdate  . d) (terminator$$  . j) (title  . b) (tracks  . h)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "album"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "album")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "album"))))

(define (title-music-album-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "title"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "title")) (if xml-validate-contents? (validate-as-pcdata! contents "title"))))

(define (artist-music-album-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "artist"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "artist")) (if xml-validate-contents? (validate-as-pcdata! contents "artist"))))

(define (recordingdate-music-album-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("date" "CDATA" "#IMPLIED") ("place" "CDATA" "#IMPLIED")))) (req-n 0)) (xml-check-for-empty-contents! contents "recordingdate") (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "recordingdate"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "recordingdate"))))

(define (coverart-music-album-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("style" "CDATA" "#REQUIRED")))) (req-n 1) (dfa (quote (finite-state-automaton 0 (2) #((0 b 1) (0 c 2) (1 c 2)) #((location  . b) (terminator$$  . c)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "coverart"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "coverart")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "coverart"))))

(define (location-music-album-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("thumbnail" "CDATA" "#IMPLIED") ("fullsize" "CDATA" "#IMPLIED")))) (req-n 0)) (xml-check-for-empty-contents! contents "location") (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "location"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "location"))))

(define (catalogno-music-album-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("label" "CDATA" "#REQUIRED") ("number" "CDATA" "#REQUIRED") ("format" ("CD" "LP" "MiniDisc") "#IMPLIED") ("releasedate" "CDATA" "#IMPLIED") ("country" "CDATA" "#IMPLIED")))) (req-n 2)) (xml-check-for-empty-contents! contents "catalogno") (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "catalogno"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "catalogno"))))

(define (personnel-music-album-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0) (dfa (quote (finite-state-automaton 0 (3) #((0 b 1) (1 b 2) (1 c 3) (2 b 2) (2 c 3)) #((player  . b) (terminator$$  . c)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "personnel"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "personnel")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "personnel"))))

(define (player-music-album-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("name" "CDATA" "#REQUIRED") ("instrument" "CDATA" "#REQUIRED")))) (req-n 2)) (xml-check-for-empty-contents! contents "player") (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "player"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "player"))))

(define (tracks-music-album-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote ())) (req-n 0) (dfa (quote (finite-state-automaton 0 (2) #((0 b 1) (0 c 2) (1 b 1) (1 c 2)) #((terminator$$  . c) (track  . b)))))) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "tracks"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "tracks")) (if xml-validate-contents? (validate-contents-by-dfa! contents dfa "tracks"))))

(define (track-music-album-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("title" "CDATA" "#REQUIRED") ("credit" "CDATA" "#IMPLIED") ("timing" "CDATA" "#IMPLIED")))) (req-n 1)) (xml-check-for-empty-contents! contents "track") (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "track"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "track"))))

(define (notes-music-album-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("author" "CDATA" "#IMPLIED")))) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "notes"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "notes")) (if xml-validate-contents? (validate-mixed-contents-by-simple-means! contents (quote (albumref trackref)) "notes"))))

(define (albumref-music-album-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("link" "CDATA" "#REQUIRED")))) (req-n 1)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "albumref"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "albumref")) (if xml-validate-contents? (validate-as-pcdata! contents "albumref"))))

(define (trackref-music-album-laml-validate! el-name attributes contents overlap-check?) (let ((attributes-of-elements (quote (("link" "CDATA" "#IMPLIED")))) (req-n 0)) (if (and overlap-check? xml-check-language-overlap?) (check-language-overlap! (as-symbol "trackref"))) (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n "trackref")) (if xml-validate-contents? (validate-as-pcdata! contents "trackref"))))

;;; Make and put XML mirror functions in the temporary language map:
(set! temp-mirror-function (generate-xml-mirror-function album-music-album-laml-validate! "album" (quote ()) (quote double) (quote music-album) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "album" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function album-music-album-laml-validate! "album" (quote ()) (quote double) (quote music-album) #t #t))
(define album temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function title-music-album-laml-validate! "title" (quote ()) (quote double) (quote music-album) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "title" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function title-music-album-laml-validate! "title" (quote ()) (quote double) (quote music-album) #t #f))
(define title temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function artist-music-album-laml-validate! "artist" (quote ()) (quote double) (quote music-album) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "artist" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function artist-music-album-laml-validate! "artist" (quote ()) (quote double) (quote music-album) #t #f))
(define artist temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function recordingdate-music-album-laml-validate! "recordingdate" (quote ()) (quote single) (quote music-album) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "recordingdate" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function recordingdate-music-album-laml-validate! "recordingdate" (quote ()) (quote single) (quote music-album) #t #f))
(define recordingdate temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function coverart-music-album-laml-validate! "coverart" (quote ()) (quote double) (quote music-album) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "coverart" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function coverart-music-album-laml-validate! "coverart" (quote ()) (quote double) (quote music-album) #t #f))
(define coverart temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function location-music-album-laml-validate! "location" (quote ()) (quote single) (quote music-album) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "location" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function location-music-album-laml-validate! "location" (quote ()) (quote single) (quote music-album) #t #f))
(define location temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function catalogno-music-album-laml-validate! "catalogno" (quote ()) (quote single) (quote music-album) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "catalogno" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function catalogno-music-album-laml-validate! "catalogno" (quote ()) (quote single) (quote music-album) #t #f))
(define catalogno temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function personnel-music-album-laml-validate! "personnel" (quote ()) (quote double) (quote music-album) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "personnel" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function personnel-music-album-laml-validate! "personnel" (quote ()) (quote double) (quote music-album) #t #f))
(define personnel temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function player-music-album-laml-validate! "player" (quote ()) (quote single) (quote music-album) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "player" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function player-music-album-laml-validate! "player" (quote ()) (quote single) (quote music-album) #t #f))
(define player temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function tracks-music-album-laml-validate! "tracks" (quote ()) (quote double) (quote music-album) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "tracks" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function tracks-music-album-laml-validate! "tracks" (quote ()) (quote double) (quote music-album) #t #f))
(define tracks temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function track-music-album-laml-validate! "track" (quote ()) (quote single) (quote music-album) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "track" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function track-music-album-laml-validate! "track" (quote ()) (quote single) (quote music-album) #t #f))
(define track temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function notes-music-album-laml-validate! "notes" (quote ()) (quote double) (quote music-album) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "notes" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function notes-music-album-laml-validate! "notes" (quote ()) (quote double) (quote music-album) #t #f))
(define notes temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function albumref-music-album-laml-validate! "albumref" (quote ()) (quote double) (quote music-album) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "albumref" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function albumref-music-album-laml-validate! "albumref" (quote ()) (quote double) (quote music-album) #t #f))
(define albumref temp-mirror-function)

(set! temp-mirror-function (generate-xml-mirror-function trackref-music-album-laml-validate! "trackref" (quote ()) (quote double) (quote music-album) #f #f))
(set! temp-language-map (put-mirror-function temp-language-map "trackref" temp-mirror-function))
(set! temp-mirror-function (generate-xml-mirror-function trackref-music-album-laml-validate! "trackref" (quote ()) (quote double) (quote music-album) #t #f))
(define trackref temp-mirror-function)

; Register the name of the language:
(register-xml-in-laml-language (quote music-album) temp-language-map)

; Define the language variable
(define music-album (activator-via-language-map (quote music-album)))

; Register the XML navigator of the language:
(register-xml-in-laml-navigator (quote music-album) (quote (xml-navigator #((album #(album albumref artist catalogno coverart location notes personnel player recordingdate title track trackref tracks) #(author country credit date format fullsize instrument label link name number place releasedate style thumbnail timing title)) (albumref #(albumref) #(link)) (artist #(artist) #()) (catalogno #(catalogno) #(country format label number releasedate)) (coverart #(coverart location) #(fullsize style thumbnail)) (location #(location) #(fullsize thumbnail)) (notes #(albumref notes trackref) #(author link)) (personnel #(personnel player) #(instrument name)) (player #(player) #(instrument name)) (recordingdate #(recordingdate) #(date place)) (title #(title) #()) (track #(track) #(credit timing title)) (trackref #(trackref) #(link)) (tracks #(track tracks) #(credit timing title))))))

; Register the validation procedures of the language
(register-xml-in-laml-validators (quote music-album) (vector (list "album" album-music-album-laml-validate!) (list "albumref" albumref-music-album-laml-validate!) (list "artist" artist-music-album-laml-validate!) (list "catalogno" catalogno-music-album-laml-validate!) (list "coverart" coverart-music-album-laml-validate!) (list "location" location-music-album-laml-validate!) (list "notes" notes-music-album-laml-validate!) (list "personnel" personnel-music-album-laml-validate!) (list "player" player-music-album-laml-validate!) (list "recordingdate" recordingdate-music-album-laml-validate!) (list "title" title-music-album-laml-validate!) (list "track" track-music-album-laml-validate!) (list "trackref" trackref-music-album-laml-validate!) (list "tracks" tracks-music-album-laml-validate!)))

; Register the content model map of the language.
; This makes the content model available for LAML at runtime.
(register-xml-in-laml-content-models (quote music-album) (quote #(("album" (element-content (seq one (name one title) (name one artist) (name optional recordingdate) (name one coverart) (seq one-or-more (name one catalogno)) (name one personnel) (name one tracks) (name one notes)))) ("albumref" (mixed-content pcdata)) ("artist" (mixed-content pcdata)) ("catalogno" empty) ("coverart" (element-content (seq optional (name one location)))) ("location" empty) ("notes" (mixed-content (choice pcdata albumref trackref))) ("personnel" (element-content (seq one-or-more (name one player)))) ("player" empty) ("recordingdate" empty) ("title" (mixed-content pcdata)) ("track" empty) ("trackref" (mixed-content pcdata)) ("tracks" (element-content (seq zero-or-more (name one track)))))))

; Register the action procedure map of the language.
(register-xml-in-laml-action-procedures (quote music-album) (vector (list "album" (lambda (ast) (album! ast)))))

