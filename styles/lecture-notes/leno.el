; The LENO Emacs mode support.

(provide 'leno-support)

(define-derived-mode 
	leno-mode laml-mode "Leno" 
  "A LENO Emacs mode mode, which binds some keys (see below) to a useful LENO functionality.
LENO is part of LAML, which means 'Lisp Abstracted Markup Language'.
LAML makes HTML and other XML languages (such as LENO) available as Scheme mirror functions."

  (font-lock-mode t)

)


; ---------------------------------------------------------------------------------------------------
; LENO specific Templates


; (defvar leno-version 'original
;   "Determines which generation of the LENO templates to apply. 
; With the symbol original, the first generation of the templates is applied.
; With the symbol xml-in-laml, the second generation is applied."
; ) ; original, xml-in-laml. Defined in laml-customize.el

(defvar leno-original-templates 
  (list
   '(title (empty "title") (empty "comment"))

   '(text (empty "text") (empty "comment"))

   '(point (empty "the-point") (empty "comment"))

   '(source-program "file-name" ("start" "end") 
           (list
              (list "from" "to" color face repeat-count )

           )
           '(slide-inline book-inline)
           "caption")

   ; items and item: special

   '(image "file-name" "comment" (list (quote vertical) (quote picture)))

   '(example (empty "the-example") (empty "comment"))

   ; opposing: special

   '(syntax (empty "the-syntax") (empty "comment"))

   '(slide-space)

   '(tabular 1 width-list list-of-list "comment")

   '(note-text (empty "text"))     

   '(slide-text (empty "text"))

   '(theme-text (p (empty "text")))     

   '(cross-references 
      (list reference-items)
    )

   '(internet-reference (empty "reference-text") "url")

   '(note-reference (empty "reference-text") lecture-id page-id)

   '(informal-reference "reference-text" "reference")

   '(exercise id "title" (formulation "formulation") (solution "solution"))

   '(long-slide)

   '(index-words (empty "word1") (empty "word2"))

   '(image-series "description" list-of-image-descriptions (list (quote slide-external) (quote book-inline)))

   '(section-title (empty "title"))

   '(slide-image "file-name")

   ; concept-list: special

   '(applet-program "class-file-name" "code-base" "explanation" width height)

   ; synopsis: special

   '(quotation "quotation-text" "commment")

   '(elucidate "description" "url" "target") 

   '(lecturer-photos start-photo-number number-of-photos)

   '(lecturer-photos-and-logo start-photo-number)
 
   '(splice-page-with (list tag1 tag2) lecture-id page-id)

   '(splice-page-without (list tag1 tag2) lecture-id page-id)

   '(side-track "track title" track-lecture-id "Explanation of track")

   ) "LENO template list, original syntax")

(defvar leno-xml-in-laml-templates 
  (list
   '(title (main-text (empty "title")) (annotation (empty "comment")))

   '(text (main-text (empty "text")) (annotation (empty "comment")))

   '(point (main-text (empty "the-point")) (annotation (empty "comment")))

   '(source-program
     'src "file-name" 
     'from-mark "from"
     'to-mark "to"
     'slide-mode "inline"
     'book-mode "inline"
     (color-decorations
      (color-decoration 'from-mark "from" 'to-mark "to" 'color "c" 'face "face")
     )
     (main-text "caption")
     (annotation "annotation")
    )

   '(image
    'src "file-name"
    'alignment "vertical"
    'first "picture"
    'second "text"
    (main-text (empty "text"))
   )

   '(example (main-text (empty "title")) (annotation (empty "comment")))


   '(point 
    (main-text
      (empty "the-point")
     )
    (annotation
      (empty "comment")
     )
   )

   '(cross-references
    (internet-reference
     'href (empty "URL") (main-text (empty "anchor-text")))
    (note-reference
     'lecture-id "" 'page-id "" (main-text (empty "anchor-text")))
    (informal-reference
     'title (empty "title") (main-text (empty "anchor-text")))
    (bibtex-reference
     'key (empty "key"))
   )

   '(internet-reference 'href "URL" 
       (main-text (empty "anchor-text"))
       (location-hints))

   '(note-reference 'lecture-id "lid" 'page-id "pid" 
       (main-text (empty "anchor-text"))
       (location-hints))

   '(informal-reference 'title "title" 
       (main-text (empty "description")))

   '(bibtex-reference 'key (empty "key"))

   '(example
    (main-text
     "title" )
    (annotation
     "comment" )
   )

   ; opposing: special

   '(syntax (main-text "syntax-text") (annotation "comment" ))

   '(slide-space)

   '(tabular
    'border "1"
    (row-widths
     (cell "number") (cell "number") (cell "number"))
   
    (row
     (cell "") (cell "") (cell ""))
    (row
     (cell "") (cell "") (cell ""))
    (row
     (cell "") (cell "") (cell ""))

     (annotation (empty "text"))
    )

   '(note-text (empty "text"))     

   '(slide-text (empty "text"))

   '(theme-text (p (empty "text")))     

   '(exercise
     'id ""
     'title ""
     (formulation "exercise formulation")
     (solution "exercice solution")
    )

   '(long-slide)

   '(index-words (index-word (empty "word")))

   '(image-series
      'title ""
      (image-series-item 'src "filename-1" "Description 1")
      (image-series-item 'src "filename-2" "Description 2")
      'slide-mode "external"
      'book-mode "external"
    )

   '(section-title (empty "title"))

   '(slide-image 'src "filename")

   '(applet-program
     'code "class-file"
     'code-base "base"
     'height "number"
     'width "number"
     (main-text (empty "caption"))
    )

   '(quotation (main-text (empty "text")) (annotation (empty "comment")))

   '(elucidate "description" 'href "url" 'target "target") 

   '(lecturer-photos 'start-number "n"  'number-of-photos "m")

   '(lecturer-photos-and-logo 'start-number "n")
 
   '(splice-page-with 'lecture-id "lid" 'page-id "pid" 'element-id "i"
      (leno-elements (element-name "n1") (element-name "n2")))

   '(splice-page-without 'lecture-id "lid" 'page-id "pid"
      (leno-elements (element-name "n1") (element-name "n2")))

   '(side-track (main-text "track title") (annotation "Explanation of track") 'track-lecture-id "track-lid")

   '(comment (empty "text"))

   '(quiz (question "question") (answers (answer 'correctness "percent-numer" (answer-possibility "answer") (answer-clarification "clarification"))))

   '(show-and-speak (slide-part 'number "1" 'seconds "5") (program-part 'number "1" 'seconds "5")
         (image-series-part 'number "1" (image-part 'seconds "5") (image-part 'seconds "5") ))

   '(meta-text 'type "readers-guide" (empty "meta-text"))

   '(flash-image 'width "pixel-width" 'height "pixel-height" 'src "flash-file-name" (main-text "caption"))

   '(svg-image 'width "pixel-width" 'height "pixel-height" 'src "svg-file-name" (main-text "caption"))

   '(theme-text (p ""))

   ) "LENO template list, xml-in-laml syntax")

; with (en ...) forms
(defvar1 leno-xml-in-laml-compact-templates-with-en 
  (list
   '(svg-image 'id (empty "id") 'width "800" 'height "800" CR
      (main-text (en (empty "text"))) CR
      (svg-here  CR
       (g 'transform "scale(1.0)" CR
           top-down-program-development)))

   '(concept-list 'id (empty "id") CR (concept 'concept-name (empty "name") (main-text (en (empty "concept")) )))

   '(title (main-text (en (empty "title"))))

   '(text 'id (empty "id") (main-text (en (empty "text"))))

   '(point 'id (empty "id") CR (main-text (en (empty "the-point")) ))

   '(source-program 'id (empty "id") CR
     'src (empty "source-file")
     'from-mark (empty "from")
     'to-mark (empty "to")
     'slide-mode "external"
     'book-mode "inline" CR
     (color-decorations
      (color-decoration 'from-mark (empty "from") 'to-mark (empty"to") 'color "red" 'face "bold")
     ) CR
     (main-text (en (empty "caption")))
    )

   '(image 'id (empty "id")
    'src "file-name"
    'alignment "vertical"
    'first "picture"
    'second "text" CR
    (main-text (en (empty "text")))
   )

   '(cross-references 'id (empty "id") CR
    (internet-reference
     'href (empty "URL") (main-text (en (empty "anchor-text")))) CR
    (note-reference
     'lecture-id "" 'page-id "" (main-text (en (empty "anchor-text")))) CR
    (informal-reference
     'title (empty "title") (main-text (en (empty "anchor-text")))) CR
    (bibtex-reference
     'key (empty "key"))
   )

   '(internet-reference  'href "URL" 
       (main-text (en (empty "anchor-text")))
       (location-hints))

   '(note-reference 'lecture-id "lid" 'page-id "pid" 
       (main-text (en (empty "anchor-text")))
       (location-hints))

   '(informal-reference 'title "title" 
       (main-text (en (empty "description"))))

   '(bibtex-reference 'key (empty "key"))

   '(example 'id (empty "id")
     (main-text (en "title")))


   '(opposing 'id (empty "id") CR
     (opposing-item 
      (left-item (en "")) 
      (right-item (en ""))))

   '(syntax 'id (empty "id") (main-text (en "syntax-text")))

   '(slide-space)

   '(tabular 'id (empty "id")
    'border "1"
    (row-widths
     (cell "number") (cell "number") (cell "number")) CR
   
    (row
     (cell (en "")) (cell (en "")) (cell (en ""))) CR
    (row
     (cell (en "")) (cell (en "")) (cell (en""))) CR
    (row
     (cell (en "")) (cell (en "")) (cell (en""))) CR
    )

   '(note-text 'id (empty "id") (en (empty "text")))     

   '(slide-text (en (empty "text")))

   '(theme-text (p (empty "text")))     

   '(exercise 'id (empty "id")
     'title ""
     (formulation "exercise formulation")
     (solution "exercice solution")
    )

   '(long-slide)

   '(index-words (index-word (en (empty "word"))))

   '(image-series 'id (empty "id")
      'title "" CR
      (image-series-item 'src "filename-1" "Description 1") CR
      (image-series-item 'src "filename-2" "Description 2") CR
      'slide-mode "external"
      'book-mode "external"
    )

   '(section-title (en (empty "title")))

   '(slide-image 'id (empty "id") 'src "filename")

   '(applet-program
     'code "class-file"
     'code-base "base"
     'height "number"
     'width "number"
     (main-text (empty "caption"))
    )

   '(quotation 'id (empty "id") (main-text (en (empty "text"))))

   '(elucidate "description" 'href "url" 'target "target") 

   '(lecturer-photos 'start-number "n"  'number-of-photos "m")

   '(lecturer-photos-and-logo 'start-number "n")
 
   '(splice-page-with 'lecture-id "lid" 'page-id "pid" 'element-id "i" CR
      (leno-elements (element-name "n1") (element-name "n2")))

   '(splice-page-without 'lecture-id "lid" 'page-id "pid" CR
      (leno-elements (element-name "n1") (element-name "n2")))

   '(side-track (main-text (en "track title")) CR 'track-lecture-id "track-lid")

   '(comment (empty "text"))

   '(quiz CR (question (en "question")) CR 
      (answers 
        (answer 'correctness "percent-numer" CR
           (answer-possibility (en "answer")) (answer-clarification (en"clarification") ))))

   '(show-and-speak CR
         (slide-part 'number "1" 'seconds "5") CR
         (program-part 'number "1" 'seconds "5") CR
         (image-series-part 'number "1" (image-part 'seconds "5") (image-part 'seconds "5") ))

   '(meta-text 'type "readers-guide" (empty "meta-text"))

   '(flash-image 'width "pixel-width" 'height "pixel-height" 'src "flash-file-name" (main-text "caption"))

   '(svg-image 'id (empty "id") 'width "pixel-width" 'height "pixel-height" 'src "svg-file-name" (main-text "caption"))

   '(item (main-text (en "")))

   '(iii CR
      (item (main-text (en ""))) CR
      (item (main-text (en ""))) CR
    )

   '(jjj CR
      (item (main-text (en "")) CR
        (items (item (main-text (en ""))) CR
               (item (main-text (en ""))))) CR
      (item (main-text (en "")))
    )

   

   ) "LENO template list, xml-in-laml syntax")

; without (en ...) forms
(defvar1 leno-xml-in-laml-compact-templates
  (list
   '(svg-image 'id (empty "id") 'width "800" 'height "800" CR
      (main-text (empty "text") ) CR
      (svg-here  CR
       (g 'transform "scale(1.0)" CR
           top-down-program-development)))

   '(concept-list 'id (empty "id") CR (concept 'concept-name (empty "name") (main-text (empty "concept") )))

   '(title (main-text (empty "title") ))

   '(text 'id (empty "id") (main-text (empty "text") ))

   '(point 'id (empty "id") CR (main-text (empty "the-point") ))

   '(source-program 'id (empty "id") CR
     'src (empty "source-file")
     'from-mark (empty "from")
     'to-mark (empty "to")
     'slide-mode "external"
     'book-mode "inline" CR
     (color-decorations
      (color-decoration 'from-mark (empty "from") 'to-mark (empty"to") 'color "red" 'face "bold")
     ) CR
     (main-text (empty "caption") )
    )

   '(image 'id (empty "id")
    'src "file-name"
    'alignment "vertical"
    'first "picture"
    'second "text" CR
    (main-text (empty "text") )
   )

   '(cross-references 'id (empty "id") CR
    (internet-reference
     'href (empty "URL") (main-text (empty "anchor-text") )) CR
    (note-reference
     'lecture-id "" 'page-id "" (main-text (empty "anchor-text") )) CR
    (informal-reference
     'title (empty "title") (main-text (empty "anchor-text") )) CR
    (bibtex-reference
     'key (empty "key"))
   )

   '(internet-reference  'href "URL" 
       (main-text (empty "anchor-text") )
       (location-hints))

   '(note-reference 'lecture-id "lid" 'page-id "pid" 
       (main-text (empty "anchor-text") )
       (location-hints))

   '(informal-reference 'title "title" 
       (main-text (empty "description") ))

   '(bibtex-reference 'key (empty "key"))

   '(example 'id (empty "id")
     (main-text "title" ))


   '(opposing 'id (empty "id") CR
     (opposing-item 
      (left-item "" ) 
      (right-item "" )))

   '(syntax 'id (empty "id") (main-text "syntax-text" ))

   '(slide-space)

   '(tabular 'id (empty "id")
    'border "1"
    (row-widths
     (cell "number") (cell "number") (cell "number")) CR
   
    (row
     (cell "" ) (cell "" ) (cell "" )) CR
    (row
     (cell "") (cell "") (cell "")) CR
    (row
     (cell "") (cell "") (cell "")) CR
    )

   '(note-text 'id (empty "id") (empty "text") )     

   '(slide-text (empty "text") )

   '(theme-text (p (empty "text")))     

   '(exercise 'id (empty "id")
     'title ""
     (formulation "exercise formulation")
     (solution "exercice solution")
    )

   '(long-slide)

   '(index-words (index-word (empty "word") ))

   '(image-series 'id (empty "id")
      'title "" CR
      (image-series-item 'src "filename-1" "Description 1") CR
      (image-series-item 'src "filename-2" "Description 2") CR
      'slide-mode "external"
      'book-mode "external"
    )

   '(section-title (empty "title") )

   '(slide-image 'id (empty "id") 'src "filename")

   '(applet-program
     'code "class-file"
     'code-base "base"
     'height "number"
     'width "number"
     (main-text (empty "caption"))
    )

   '(quotation 'id (empty "id") (main-text (empty "text") ))

   '(elucidate "description" 'href "url" 'target "target") 

   '(lecturer-photos 'start-number "n"  'number-of-photos "m")

   '(lecturer-photos-and-logo 'start-number "n")
 
   '(splice-page-with 'lecture-id "lid" 'page-id "pid" 'element-id "i" CR
      (leno-elements (element-name "n1") (element-name "n2")))

   '(splice-page-without 'lecture-id "lid" 'page-id "pid" CR
      (leno-elements (element-name "n1") (element-name "n2")))

   '(side-track (main-text "track title" ) CR 'track-lecture-id "track-lid")

   '(comment (empty "text"))

   '(quiz CR (question "question" ) CR 
      (answers 
        (answer 'correctness "percent-numer" CR
           (answer-possibility "answer" ) (answer-clarification "clarification" ))))

   '(show-and-speak CR
         (slide-part 'number "1" 'seconds "5") CR
         (program-part 'number "1" 'seconds "5") CR
         (image-series-part 'number "1" (image-part 'seconds "5") (image-part 'seconds "5") ))

   '(meta-text 'type "readers-guide" (empty "meta-text"))

   '(flash-image 'width "pixel-width" 'height "pixel-height" 'src "flash-file-name" (main-text "caption"))

   '(svg-image 'id (empty "id") 'width "pixel-width" 'height "pixel-height" 'src "svg-file-name" (main-text "caption"))

   '(item (main-text ""))

   '(iii CR
      (item (main-text "" )) CR
      (item (main-text "" )) CR
    )

   '(jjj CR
      (item (main-text "" ) CR
        (items (item (main-text "" )) CR
               (item (main-text "" )))) CR
      (item (main-text "" ))
    )

   

   ) "LENO template list, xml-in-laml syntax")


; ---------------------------------------------------------------------------------------------------
; Template pretty printing:

(defun pretty-print-template (x cr-after-1 cr-after-2)
  "x is a list. Return a string which is the pretty printing of x. 
Cr-after-1 is the primary separator. Cr-after-2 is the last separator in a list. Both are symbols."
 (let ((x1 (butlast x))
       (x2 (last x)))
  (concat 
     "(" 
     (concat (apply (function concat)
                    (append
                      (mapcar (function (lambda(e) (pretty-print-element e cr-after-1))) x1)
                      (mapcar (function (lambda(e) (pretty-print-element e cr-after-2))) x2))
             ) 
     )
     ")")))

(defun pretty-print-element (y cr-after)
 "LAML pretty prints a single element"
   (cond ((and (symbolp y) (eq y 'CR)) CR)  ; forced CR
         ((symbolp y) (concat  (symbol-to-string y) (space-element y 'space)))
         ((stringp y) (concat (string-it y) (space-element y 'space)))
         ((integerp y) (concat (int-to-string y) (space-element y cr-after)))
         ((and (listp y) (> (length y) 0) (eq 'empty (car y)))    ; empty
              (concat (pretty-print-element (if laml-pp-helpful (cadr y) "") 'nothing)
                     (if (eq leno-version 'original) (space-element y cr-after) (space-element y  cr-after ))))  ; 'cr!!!
         ((and (listp y) (> (length y) 0) (eq 'quote (car y)))    ; quote form
              (concat QUOTE (pretty-print-element (cadr y) 'space)))
         ((and (listp y) (> (length y) 0) (eq 'list (car y)))     ; list form
              (concat (pretty-print-template y 'space 'nothing) (space-element y cr-after)))
         ((and (listp y))  ; other lists
              (concat (pretty-print-template y cr-after 'nothing) (space-element y cr-after)))
         (t "??")))

(defun space-element (y cr-after)
  "Returns the space element (a string) given an element and an separator symbol"
  (cond 
        ((eq cr-after 'nothing) "")
        ((eq cr-after 'space) " ")
        ((eq cr-after 'cr) CR)
        (t " ")))

(defun actual-leno-template-list (leno-version)
  (cond ((eq leno-version 'xml-in-laml)  leno-xml-in-laml-compact-templates)  ; earlier: leno-xml-in-laml-templates
                                                                              ; alternatives: leno-xml-in-laml-compact-templates-with-en
        ((eq leno-version 'original)  leno-original-templates)
        (t (error "Cannot determine LENO template list"))))

(defun laml-insert (kind)
 "The near top level laml template insertion form. 
Kind is a symbol, one of the cars of the template elements in templates"
 (let ((template (assq kind (actual-leno-template-list leno-version))))
    (insert-lisp-form (pretty-print-template template 'space 'space) (determine-indentation))))     
    

; -----------------------------------------------------------------------------
; Generation of template insertion functions for LENO

(defun define-insertion-function (tag)
 "Defines an elist function (via eval) for tag, which must be a symbol"
 (let ((string-tag (symbol-to-string tag)))
  (eval (list 'defun (string-to-symbol (concat "leno-insert-" string-tag)) '() (list 'interactive)
          (list 'laml-insert (list 'quote tag))))))

; Programmatically defines insertion functions for all templates in the list leno-templates
(mapcar 
  (function (lambda (template) (define-insertion-function (car template))))
  (actual-leno-template-list leno-version)) 
 

; ---------------------------------------------------------------------------------------------------
; Special template insertions functions (which cannot be made from the template list above):
; Defined the hard way, and using a special version of insert-lisp-form, which in turn
; uses the transliteration stuff in order to make line separation, and in order to provided
; for pretty printing via indent-sexp.
; LENO specific
(defun leno-insert-items (n m)
  (interactive "nNumber of items at outer level: 
nNumber of items at the nested level: ")
  (insert-lisp-form (make-items n m t) (determine-indentation))
  (position-point-after "\"")
)

(defun leno-insert-items-without-annotations (n m)
  (interactive "nNumber of items at outer level (without annotations): 
nNumber of items at the nested level: ")
  (insert-lisp-form (pretty-print-template (make-items n m nil) 'space 'space) (determine-indentation))
  (position-point-after "\"")
)

(defun make-items (n m annotations)
  ; (if annotations (error "Annotations not yet supported"))
  (append
    (list 'items ''id "" 'CR)
    (apply (function append) (mapcar (function (lambda (n) (list (make-item m annotations) 'CR))) (number-interval 1 n)))))

(defun make-items-non-top (n m annotations)
  ; (if annotations (error "Annotations not yet supported"))
  (append
    (list 'items 'CR)
    (apply (function append) (mapcar (function (lambda (n) (list (make-item m annotations) 'CR))) (number-interval 1 n)))))

(defun make-item (m annotations)
  ;  (if annotations (error "Annotations not yet supported"))
  (if (= m 0)
      (list 'item (list 'main-text ""))
      (list 'item (list 'main-text "") 'CR
            (make-items-non-top m 0 annotations))))



(defun leno-insert-synopsis (n)
  (interactive "nNumber of synopsis items: ")
  (insert-lisp-form-0
    (make-synopsis n)
    (determine-indentation)))


(defun make-synopsis (n)
  (if (= n 0) 
      "" 
      (concat "(synopsis " (make-synopsis-item n) ")"))
)

(defun make-synopsis-item (n)
 (let ((single-item
	(cond ((eq leno-version 'original)  "(synopsis-item E E)")
	      ((eq leno-version 'xml-in-laml)  "(synopsis-item  (main-text E) (annotation E))")
	      (t (error "Make-synopsis-item: cannot determine LENO template list")))))
  (if (= n 0) 
      ""
      (concat 
         (concat single-item (make-synopsis-item (- n 1)))))))

; (defun insert-source-program ()
;   (insert-lisp-form-0
;     (laml-get-template "source-program")
;     (determine-indentation)))

(defun leno-insert-index-word ()
  (interactive)
  (insert "(index-word \"\")")
  (search-backward "\"" nil t))


; ---------------------------------------------------------------------------------------------------
; LENO templates supported via text insertions from template directories

(defun position-point-after (char-str)
  (search-forward char-str nil t))

(defun goto-next-input-field ()
  (interactive)
  (position-point-after "\"\"")
  (backward-char 1))

(defun leno-insert-notepage ()
  "Insert a LENO note page"
  (interactive)
  (let ((template-name
	(cond ((eq leno-version 'original) "notepage" )
	      ((eq leno-version 'xml-in-laml)  "notepage-xml-in-laml")
	      (t (error "Insert-note-page: Illegal leno-version: ")))))
   (laml-insert-template template-name)
   (position-point-after "\"")))


(defun leno-insert-notesection ()
  "Insert a LENO note section"
  (interactive)
  (let ((template-name
	(cond ((eq leno-version 'original) "notesection" )
	      ((eq leno-version 'xml-in-laml)  "notesection-xml-in-laml")
	      (t (error "Insert-note-page: Illegal leno-version: ")))))
   (laml-insert-template template-name)
   (position-point-after "\"")))


;(defun insert-leno-chapter ()
;  (interactive)
;  (insert-file (concat laml-dir "styles/lecture-notes/man/template-zip-dir/lecture-note-template.laml")))
;
;(defun insert-leno-single-chapter ()
;  (interactive)
;  (insert-file (concat laml-dir "styles/lecture-notes/man/template-zip-dir/lecture-note-single-chapter-template.laml")))

; ---------------------------------------------------------------------------------------------------
; Creation of a LENO directory structure, and set up of initial LENO file.
; Modelled after the elucidator function make-eluciator

(defun make-lecture-notes (dir0 leno-file-name)
  "Make a single chapter LENO slide/lecture-note set in dir0."
  (interactive "DMake single chapter LENO slide set in which directory: 
sName of the lecture-note file (plain name without extension): 
")
  (let ((dir (ensure-trailing-slash (expand-file-name dir0)))
       )
   (if dir
       (if (or (file-exists-p (concat dir "html")) (file-exists-p (concat dir "internal")))
          (progn 
            (beep) 
            (message "A lecture note set already seems to be set up in this directory. Nothing done!"))
          (progn
            (make-directory (concat dir "html/"))
            (make-directory (concat dir "html/applets/"))
            (make-directory (concat dir "html/external-html/"))
            (make-directory (concat dir "graphics/"))
            (make-directory (concat dir "graphics/small"))
            (make-directory (concat dir "html/graphics/"))
            (make-directory (concat dir "html/graphics/small"))
            (make-directory (concat dir "html/images/"))
            (make-directory (concat dir "html/javascript/"))
            (make-directory (concat dir "internal/"))
            (make-directory (concat dir "includes/"))
            (make-directory (concat dir "stylesheets/"))
            (make-directory (concat dir "images/"))
            (make-directory (concat dir "html/lecturer-photos/"))
            (let* ((leno-buf nil))

              ; Make the setup file
              (setq leno-buf
                    (make-a-file-from-laml-template 
                     (concat leno-file-name ".leno")
                     dir
                     (cond ((eq leno-version 'original) "leno-single-chapter")
                           ((eq leno-version 'xml-in-laml) "xml-in-laml-leno-single-chapter")
                           (t "xml-in-laml-leno-single-chapter"))
                     'leno-mode
                       (list
                         (list "CHAPTER-ID" leno-file-name)
                       )
                     ))

              (make-a-file-from-laml-template 
	       "post-notes.scm"
	       dir
	       "internal/leno-post-notes"
	       'scheme-mode
	       (list )
              )

              ; Initial logo file:
              (copy-file 
                (concat laml-dir "images/" "aalborg-university-logo.gif")
                (concat dir "images/logo.gif"))

              (show-buffer (selected-window) leno-buf)
              (set-buffer-file-coding-system (quote iso-latin-1))

              (message "Done.  Now process the buffer with  C-o .")

              )))

    (progn
      (beep)
      (message "Cannot determine the current directory. Sorry...")))))


; ----------------------------------------------------------------------------------------
; LENO theme support

(defvar remember-ref nil "A ref form made by the command make-theme-ref")

(defun make-theme-ref ()
 "Given that point is at the start of a leno-element, make and remember a reference to this element.
The reference can be inserted with insert-theme-ref."
 (interactive)
 (cond ((looking-at "(leno-element")
          (forward-char 1)
          (let*  ((file-name (buffer-name (current-buffer)))
                  (proper-file-name (file-name-proper file-name))
                  (section-name (current-theme-id))
                  (lecture-id (get-prop-of-current-form "lecture-id"))
                  (page-id (get-prop-of-current-form "page-id"))
                  (element-type (get-prop-of-current-form "element-type"))
                  (element-id (get-prop-of-current-form "element-id"))
                  (element-number (get-prop-of-current-form "element-number")))
            (setq remember-ref 
                  (concat
                   "(ref" " "
                   (string-it (concat proper-file-name "-" section-name)) " "
                   (string-it lecture-id) " "
                   (string-it page-id) " "
                   (string-it element-type) " "
                   (if element-id (string-it element-id) (string-it element-number))
                   ")"))
            (message "Use M-x insert-ref to insert the reference")))
       ((looking-at "(theme-side-box")
          (forward-char 1)
          (let*  ((file-name (buffer-name (current-buffer)))
                  (proper-file-name (file-name-proper file-name))
                  (section-name (current-theme-id))
                  (lecture-id (original-lecture-id-of-proper-theme-file-name proper-file-name))
                  (page-id "no-page")
                  (element-type "side-box")
                  (element-id (get-prop-of-current-form "id"))
                  )
            (setq remember-ref 
                  (concat
                   "(ref" " "
                   (string-it (concat proper-file-name "-" section-name)) " "
                   (string-it lecture-id) " "
                   (string-it page-id) " "
                   (string-it element-type) " "
                   (string-it element-id)
                   ")"))
            (message "Use M-x insert-ref to insert the reference")))))

(defun original-lecture-id-of-proper-theme-file-name (proper-theme-file-name)
  (let ((underscore-pos (find-in-string-from-position proper-theme-file-name ?_ 0)))
    (if underscore-pos
        (substring proper-theme-file-name 0 underscore-pos)
        (error "original-lecture-id-of-proper-theme-file-name: Cannot find underscore in name of theme source file"))))
       

(defun insert-theme-ref ()
  "Insert the ref form constructed by the latest make-theme-ref"
  (interactive)
  (insert remember-ref))

; Assume that point is within a form like (... 'prop "val" ...)
; Return val, or if not found, nil.
(defun get-prop-of-current-form (prop-string)
 (let ((p (point))
       (p1 nil)
       (p2 nil)
      )
  (save-excursion 
   (backward-up-list 1)
   (setq p1 (point))  (forward-sexp 1)
   (setq p2 (point))  (goto-char p)
   (setq res (search-forward prop-string p2 t))
   (if res
       (progn (search-forward "\"" nil t)
              (get-from-buffer "\"" "\""))
       nil))))

; From the context, get the theme-id
(defun current-theme-id ()
  (save-excursion
    (beginning-of-defun)
    (forward-char 1)
    (get-prop-of-current-form "id")))
     
(defun goto-leno-clause ()
  "From a LENO theme file, goto the referred Leno clause under point.
   Before activating this command, you should position the cursor point at the initial start parenthesis of a leno-element form."
  (interactive)
  (let* ((p0 (point))
         (pn (save-excursion (forward-sexp) (point)))
         (lecture-id (save-excursion
                       (let ((res (search-forward "'lecture-id" pn t)))
                         (if res
                             (progn (skip-through (list 10 13 32 34)) (name-under-point))
                           nil))))
         (page-id (save-excursion
                    (let ((res (search-forward "'page-id" pn t)))
                      (if res
                          (progn (skip-through (list 10 13 32 34)) (name-under-point))
                        nil))))
         (element-type (save-excursion
                         (let ((res (search-forward "'element-type" pn t)))
                           (if res
                               (progn (skip-through (list 10 13 32 34)) (name-under-point))
                             nil))))
         (element-id (save-excursion
                         (let ((res (search-forward "'element-id" pn t)))
                           (if res
                               (progn (skip-through (list 10 13 32 34)) (name-under-point))
                             nil))))
         (element-number (save-excursion
                         (let ((res (search-forward "'element-number" pn t)))
                           (if res
                               (progn (skip-through (list 10 13 32 34)) (name-under-point))
                             nil))))
         )

    (cond ((= (length (window-list)) 2)
             (other-window 1))
          (t (delete-other-windows)
             (split-window-vertically)
             (other-window 1)))

    (find-file (concat lecture-id ".leno"))
    (beginning-of-buffer)
    (let ((res (re-search-forward (regular-expression 
                                   `(concat "(note-page" (one-or-more " ") "'id" (one-or-more " ") "\"" ,page-id "\"")) nil t))
          (done nil))
      (if element-number
          (re-search-forward (regular-expression `(concat "(" (zero-or-more (char-set space tab newline return)) ,element-type))
                              nil t (string-to-int element-number))
          (re-search-forward (regular-expression 
                               `(concat "(" (zero-or-more (char-set space tab newline return)) ,element-type
                                       (zero-or-more (char-set space tab newline return)) "'id" 
                                       (zero-or-more (char-set space tab newline return)) "\"" ,element-id "\""))) )
      (backward-up-list 1))
;    (other-window 1)
  ))

(defun goto-leno-theme-clause ()
  "From a LENO file, goto a LENO theme in another file which  refers the currently selected LENO element.
   Before activating this command, you should position the cursor point at the initial start parenthesis of a leno-element form."
  (interactive)
  (let* ((p0 (point))
         (pn (save-excursion (forward-sexp) (point)))
         (lecture-id (file-name-proper (buffer-name)))
         (page-id (save-excursion
                    (beginning-of-defun 1)
                    (let ((res (search-forward "'id" pn t)))
                      (if res
                          (progn (skip-through (list 10 13 32 34)) (name-under-point))
                        nil))))
         (element-id  (save-excursion
			(let ((res (search-forward "'id" pn t)))
			  (if res
			      (progn (skip-through (list 10 13 32 34)) (name-under-point))
			    nil))))
         (element-type (save-excursion (forward-char 1) (skip-through (list 10 13 32 34)) (name-under-point))))

    (cond ((= (length (window-list)) 2)
             (other-window 1))
          (t (delete-other-windows)
             (split-window-vertically)
             (other-window 1)))

    (find-file (concat lecture-id "_" "themes" ".leno"))
    (beginning-of-buffer)
    
    (let ((res-1 (re-search-forward (regular-expression 
                                   `(concat "(leno-element" (one-or-more " ") "'lecture-id" (one-or-more " ") "\"" ,lecture-id "\"" (one-or-more " ")
                                                                              "'page-id" (one-or-more " ") "\"" ,(space-if-nil page-id) "\"" (one-or-more " ")
                                                                              "'element-type" (one-or-more " ") "\"" ,element-type "\"" (one-or-more " ")
                                                                              "'element-id" (one-or-more " ") "\"" ,(space-if-nil element-id) "\"" (one-or-more " "))
                                                          
                                    ) nil t)))
      (if res-1
          (progn (goto-char res-1) (backward-up-list 1))
          (let ((res-2 (re-search-forward (regular-expression 
                                   `(concat "(leno-element" (one-or-more " ") "'lecture-id" (one-or-more " ") "\"" ,lecture-id "\"" (one-or-more " ")
                                                                              "'page-id" (one-or-more " ") "\"" ,(space-if-nil page-id) "\"" (one-or-more " ")
                                                                              "'element-type" (one-or-more " ") "\"" ,element-type "\"")
                                                          
                                    ) nil t)))
             (if res-2 
                 (progn (goto-char res-2) (backward-up-list 1))
                 (message "Cannot find")))))))

(defun goto-other-clause ()
  "From a clause in LENO lecture note file, goto a similar theme clause. From a theme clause, goto the similar lecture clause"
  (interactive)
  (cond ((leno-lecture-context) (goto-leno-theme-clause))
        ((leno-theme-context)   (goto-leno-clause))
        (t (message "This command must be issued form a leno lecture file or a lene theme file."))))

(defun leno-lecture-context ()
  (let* ((buf-name (file-name-proper (buffer-name)))
         (buf-name-suffix (if (>= (length buf-name) 7) (substring-no-properties buf-name (- (length buf-name) 7)) "")))
    (and (eq major-mode 'leno-mode) (not (equal "_themes" buf-name-suffix)))))

(defun leno-theme-context ()
  (let* ((buf-name (file-name-proper (buffer-name)))
         (buf-name-suffix (if (>= (length buf-name) 7) (substring-no-properties buf-name (- (length buf-name) 7)) "")))
    (and (eq major-mode 'leno-mode) (equal "_themes" buf-name-suffix))))

(defun space-if-nil (x)
  (if (not x) "" x))
         

; ----------------------------------------------------------------------------------------
; Leno keybinding and Leno Menu

(laml-define-key leno-mode-map [menu-bar leno]
    (cons "Leno" (make-sparse-keymap "Leno")))


; Leno mode key bindings
(laml-define-key leno-mode-map (list "\C-l\C-a" "\C-c\C-l\C-a") 'leno-insert-notesection)
(laml-define-key leno-mode-map (list "\C-l\C-b" "\C-c\C-l\C-b") 'leno-insert-notepage)

(laml-define-key leno-mode-map (list "\C-l\C-t" "\C-c\C-l\C-t") 'leno-insert-theme-text)
(laml-define-key leno-mode-map (list "\C-l\C-p" "\C-c\C-l\C-p") 'leno-insert-point)
(laml-define-key leno-mode-map (list "\C-l\C-i" "\C-c\C-l\C-i") 'leno-insert-items)
(laml-define-key leno-mode-map (list "\C-l\C-l" "\C-c\C-l\C-l") 'leno-insert-items-without-annotations)
(laml-define-key leno-mode-map (list "\C-l\C-r" "\C-c\C-l\C-r") 'leno-insert-cross-references)
(laml-define-key leno-mode-map (list "\C-l\C-e" "\C-c\C-l\C-e") 'leno-insert-exercise)
(laml-define-key leno-mode-map (list "\C-l\C-m" "\C-c\C-l\C-m") 'leno-insert-image)
(laml-define-key leno-mode-map (list "\C-l\C-w" "\C-c\C-l\C-w") 'leno-insert-index-words)
(laml-define-key leno-mode-map (list "\C-l\C-n" "\C-c\C-l\C-n") 'leno-insert-note-text)
(laml-define-key leno-mode-map (list "\C-l\C-o" "\C-c\C-l\C-o") 'leno-insert-note-text)
(laml-define-key leno-mode-map (list "\C-l\C-x" "\C-c\C-l\C-x") 'leno-insert-slide-text)
(laml-define-key leno-mode-map (list "\C-l\C-q" "\C-c\C-l\C-q") 'leno-insert-quotation)
(laml-define-key leno-mode-map (list "\C-l\C-k" "\C-c\C-l\C-k") 'leno-insert-show-and-speak)
(laml-define-key leno-mode-map (list "\C-l\C-s" "\C-c\C-l\C-s") 'leno-insert-source-program)
(laml-define-key leno-mode-map (list "\C-l\C-y" "\C-c\C-l\C-y") 'leno-insert-syntax)
(laml-define-key leno-mode-map (list "\C-l\C-u" "\C-c\C-l\C-u") 'leno-insert-tabular)

; Not in menu
(laml-define-key leno-mode-map (list "\C-l\C-d" "\C-c\C-l\C-d") 'make-theme-ref)
(laml-define-key leno-mode-map (list "\C-l\C-f" "\C-c\C-l\C-f") 'insert-theme-ref)
(laml-define-key leno-mode-map (list "\C-l\C-g" "\C-c\C-l\C-g") 'goto-other-clause)
(define-key      leno-mode-map [f5] 'goto-next-input-field)


; ---------------------------------------------------------------------------------------------------
; LENO related menus

(laml-define-key leno-mode-map [menu-bar leno other-ins]
   (cons "Other insertions" (make-sparse-keymap "OtherInsertions")))

(laml-define-key leno-mode-map [menu-bar leno other-ins leno-insert-synopsis]
      '("Insert synopsis" . leno-insert-synopsis))


(laml-define-key global-map [menu-bar tools laml make-lecture-notes]
  '("Make new LENO lecture notes..." . make-lecture-notes))


(laml-define-key leno-mode-map [menu-bar leno menu-forms-sep5]
    '("----"))

(laml-define-key leno-mode-map [menu-bar leno]
    (cons "Leno" (make-sparse-keymap "Leno")))

(laml-define-key leno-mode-map [menu-bar leno make-lecture-notes]
      '("Make new LENO lecture notes..." . make-lecture-notes) )

(laml-define-key leno-mode-map [menu-bar leno menu-forms-sep6]
    '("----") )


(laml-define-key leno-mode-map [menu-bar leno leno-insert-theme-text]
    '("Insert theme-text" . leno-insert-theme-text))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-text]
    '("Insert text" . leno-insert-text))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-tabular]
    '("Insert tabular" . leno-insert-tabular))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-title]
    '("Insert title" . leno-insert-title))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-syntax]
    '("Insert syntax" . leno-insert-syntax))



(laml-define-key leno-mode-map [menu-bar leno leno-insert-splice-page-without]
    '("Insert splice page without" . leno-insert-splice-page-without))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-splice-page-with]
    '("Insert splice page with" . leno-insert-splice-page-with))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-source-program]
    '("Insert source-program" . leno-insert-source-program))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-slide-text]
    '("Insert slide text" . leno-insert-slide-text))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-slide-image]
    '("Insert slide image" . leno-insert-slide-image))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-side-track]
    '("Insert sidetrack" . leno-insert-side-track))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-show-and-speak]
    '("Insert show-and-speak" . leno-insert-show-and-speak))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-svg-image]
    '("Insert SVG image" . leno-insert-svg-image))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-section-title]
    '("Insert section title" . leno-insert-section-title))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-quotation]
    '("Insert quotation" . leno-insert-quotation))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-quiz]
    '("Insert quiz" . leno-insert-quiz))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-point]
    '("Insert point" . leno-insert-point))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-lecturer-photos-and-logo]
    '("Insert photos and logo" . leno-insert-lecturer-photos-and-logo))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-opposing]
      '("Insert opposing items..." . leno-insert-opposing))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-note-text]
    '("Insert note text" . leno-insert-note-text))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-note-reference]
    '("Insert note reference" . leno-insert-note-reference))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-lecturer-photos]
    '("Insert lecturer photos" . leno-insert-lecturer-photos))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-simple-items]
      '("Insert simple items..." . leno-insert-items-without-annotations))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-items]
      '("Insert items..." . leno-insert-items))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-internet-reference]
    '("Insert internet reference" . leno-insert-internet-reference))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-informal-reference]
    '("Insert informal reference" . leno-insert-informal-reference))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-index-words]
    '("Insert index words" . leno-insert-index-words))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-image-series]
    '("Insert image series" . leno-insert-image-series))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-image]
    '("Insert image" . leno-insert-image))


(laml-define-key leno-mode-map [menu-bar leno leno-insert-example]
    '("Insert example" . leno-insert-example))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-exercise]
    '("Insert exercise" . leno-insert-exercise))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-elucidate]
    '("Insert elucidate" . leno-insert-elucidate))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-cross-references]
    '("Insert cross references" . leno-insert-cross-references))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-concept-list]
      '("Insert concept list..." . leno-insert-concept-list))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-comment]
    '("Insert comment" . leno-insert-comment))


(laml-define-key leno-mode-map [menu-bar leno leno-insert-bibtex-reference]
    '("Insert BibTex reference" . leno-insert-bibtex-reference))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-applet-program]
    '("Insert applet program" . leno-insert-applet-program))

(laml-define-key leno-mode-map [menu-bar leno menu-forms-sep8]
    '("----"))


(laml-define-key leno-mode-map [menu-bar leno navigate-cross-ref]
      (cons "Navigate & Cross Ref" (make-sparse-keymap "NAVIGATE")))

(laml-define-key leno-mode-map [menu-bar leno navigate-cross-ref goto-other-clause]
      '("Goto other clause" . goto-other-clause))

(laml-define-key leno-mode-map [menu-bar leno navigate-cross-ref goto-other-clause]
      '("Goto other clause" . goto-other-clause))

(laml-define-key leno-mode-map [menu-bar leno navigate-cross-ref make-theme-ref]
      '("Make theme reference" . make-theme-ref))

(laml-define-key leno-mode-map [menu-bar leno navigate-cross-ref insert-theme-ref]
      '("Insert theme reference" . insert-theme-ref))


(laml-define-key leno-mode-map [menu-bar leno menu-forms-sep7]
    '("----"))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-notepage]
      '("Insert note page" . leno-insert-notepage))

(laml-define-key leno-mode-map [menu-bar leno leno-insert-notesection]
      '("Insert note section page" . leno-insert-notesection))



; End LENO bindings
; ---------------------------------------------------------------------------------------------------

(defun read-more ()
  "Insert at read-more clause (note-text clause) at the bottom of the current note-page.
You are supposed to be inside the note-page form, or at the start parenthesis of note-page, when this command is issued."
  (interactive)
  (if (not (and (= (column-number-of (point)) 0) (eq (char-under-point) 40)))  ; 40 is '('
      (beginning-of-defun 1))
  (search-forward "'id" nil t)
  (search-forward "\"")
  (let ((id (name-under-point)))
    (end-of-defun 1)
    (while (not (eq (char-under-point) 41)) (backward-char 1))  ; 41 is ')'
    (backward-sexp 1)  
    (let ((idt (column-number-of (point))))  
      (forward-sexp 1)
      (insert CR) (insert CR)
      (insert (make-string idt 32))
      (insert "(read-more " "\"" id "\"" " " "\"" "\"" ")")
      (backward-sexp 1)
      (goto-next-input-field)
  )))

(defun annotation ()
  "Add an annotation clause just after main-text. Must be initiated on the start parenthesis of the form."
  (interactive)
  (let ((p0 (point)))
   (forward-sexp 1)
    (let ((pn (point)))
     (forward-sexp -1)
     (search-forward "(main-text" pn t)
     (search-backward "(main-text" nil t)
     (let ((idt (column-number-of (point))))
        (forward-sexp 1)
        (insert CR) 
        (insert (make-string idt 32))
        (insert "(annotation \"\")")
        (backward-char 2)))))

