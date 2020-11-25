; Pending TEST of page-1, timed-page, timed-page-with-keypress-script.
; colorize-substrings: meget vanskelig og på tværs af AST approach.

; The LAML library and programs are written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999  Kurt Normark, normark@cs.auc.dk.
; 
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;;; This library contains a number of convenient functions on top of
;;;; the basic and the surface library. Some of these are only of historical
;;;; interest, but important for some of the existing LAML software packages.
;;;; But most functions are useful and/or powerful.
;;;; This library corresponds to ../html.scm and a few manually 
;;;; programmed functions in ../html-v1.scm. Recall that ../html.scm is the
;;;; original hand crafted ad hoc library of the LAML software package, which
;;;; is obsolete due to this library together with the basis and surface libraries.
;;;; Some functions in the ../html.scm correspond to HTML tags, and as
;;;; such they are defined in surface.scm in this directory. Other functions
;;;; in ../html.scm have the same name as an html tag wihtout really mirroring
;;;; the tag. Such tags have been renamed in this library. See the section below
;;;; for a description of these renamings.<p>
;;;; Besides the general library, this library requires the html4.01 transitional basis and surface libraries.
;;;; See also the <a href="basis.html">basis</a> and the <a href="surface.html">surface</a> libraries of the html4.01-transitional mirror.<p>

;;;; As a change in this version of convenience for the AST based validating mirror, we do not convert
;;;; contents to strings, from for instance numbers or symbols. It can affect backward compatibility.<p>

;;;; We strongly recommend that you use one of the XHTML mirrors in LAML instead of HTML4.01.


; ---------------------------------------------------------------------------------------------------
;;; Renamings compared to the original, very old html library: lib/html.scm.
;;; As mentioned in the introduction, a number of functions in this library have been 
;;; renamed, as compared with the similar functions in the original html library. It is
;;; important to be aware of these renamings if you convert old LAML-based software to 
;;; the HTML4.01-transitional mirror. The renamings are as follows
;;; <pre>
;;; form -> form-1
;;; select -> select-1
;;; textarea -> textarea-1
;;; ol -> ol-1
;;; ul -> ul-1
;;; table -> table-0 
;;; img -> img-0
;;; hr -> hr-1
;;; font -> font-1
;;; blockquote -> blockquote-1
;;; cite -> cite-1
;;; p -> p-1
;;; frame -> frame-1
;;; </pre>
;;;
;;; A few functions of the old html-v1 mirror are also affected:
;;; <pre>
;;; html:page -> page-1
;;; html:post-page -> post-page-with-comments
;;; html:pre-page-with-keypress-script -> pre-page-with-keypress-script
;;; html:page-with-keypress-script -> page-with-keypress-script
;;; </pre>
;;; <b>In addition all calls of functions named html:f must be changed to html4:f.</b>
; ---------------------------------------------------------------------------------------------------

;;; Basic HTML extension stuff. 
;;; In this section we implement mirrors of HTML stuff such as comment and character entities.
;;; Also there are a number of convenient white space functions (horizontal and vertical).
;;; As a special, but very useful function, we include an html-protect function which provides for presentation
;;; of verbatim HTML documents in a browser. There is also some basic javascript support in this section.

(define (html-comment comment)
  (string-append "<!-- " comment "-->"))

; The HTML4.01 loose document type clause. Just a backward compatibility alias.
(define doctype-clause document-type-declaration)

;; if x is a number return a numbered character entity.
;; if x is a symbol og string, return a named character entity.
(define (character-entity x)
 (cond ((number? x) (string-append "&#" (three-digit-string x) ";"))  ; remember to ensure leading zeros
       ((or (string? x) (symbol? x)) (string-append "&" (as-string x) ";"))
       (else (error "character-entity: the parameter must be numeric, a symbol, or a string"))))


; Return a number string of exactly length three
; n is a number between 0 and 999
(define (three-digit-string n)
  (cond ((and (>= n 0) (< n 10)) (string-append "00" (as-string n)))
        ((and (>= n 10) (< n 100)) (string-append "0" (as-string n)))
        ((< n 1000) (as-string n))
        (else (error "three-digit-string: parameter must be between 0 and 999"))))

;; The copyright character entity
(define copyright (character-entity "copy"))


;; Return n space special characters (horizontal space)
(define (space n)
  (apply string-append (make-list n (character-entity "nbsp"))))

;; Return n space special characters 
(define horizontal-space space)

;; Return n vertical spaces, i.e., n instances of the p tag.
(define (vertical-space n)
  (if (= n 0) '()
      (con (space 1) (p) (vertical-space (- n 1)))))

;; Protect HTML tags such that an HTML document can be shown verbatim in a browser.
;; Transliterate angle brackets in str to the particular html character entities.
;; With this useful function it is possible to show the html tags in a browser
(define (html-protect str)
 (transliterate
   (transliterate
     (transliterate 
         str #\& (character-entity "amp"))
     #\> (character-entity "gt")) 
    #\< (character-entity "lt")))

;; Translate the Danish letters in str to a string with appropriate use of HTML character entities.
(define (in-danish str)
 (letrec ((in-danish-1 
            (lambda (str letter-numbers)
              (cond ((null? letter-numbers) str)
		    (else (in-danish-1
			   (transliterate str (as-char (car letter-numbers)) (character-entity (car letter-numbers)))
			   (cdr letter-numbers)))))))
  (let ((danish-letter-numbers (list 230 248 229 198 216 197)))
    (in-danish-1 str danish-letter-numbers))))


;; Return a Javascript calling form, given function function-name and parameters.
;; Returns a string of the form: function-name(parameters).
;; This function adds commas between the actual Javascript parameters.
(define (js-call function-name parameters)
  (string-append function-name 
                   "(" 
                      (string-merge (map as-string parameters)
                                    (make-list (- (length parameters) 1) ", "))
                   ")"))



;; Return a manifest javascript array given its elements
(define (js-string-array elements)
  (string-append "[" 
                 (string-merge (map string-it-single (map as-string elements))
                               (make-list (- (length elements) 1) ","))
                 "]")
)

;; Concatenete a number of strings. Con is an alias of list in this version of the mirror.
;; It is the intension to use it when concatenating HTML constituents.
;; Returns a list.
;; .form (con . elements)
(define con list)

;; Like con, but ensure that there are white space in between the concatenated strings.
;; Implemented by a string-merge. 
;; Returns a list.
(define (con-space . string-list)
  (let ((space-list (make-list (- (length string-list) 1) explicit-space))) 
    (merge-lists-simple string-list space-list)))

;; Like con from the general library, but insert a p tag between all strings in string-list
(define (con-par . string-list)
  (let ((par-list (make-list (- (length string-list) 1) (p))))
    (merge-lists-simple string-list par-list)))


; ---------------------------------------------------------------------------------------------------
;;; Convenience variants of the HTML mirror functions. 
;;; The functions in this section are either quite close to the HTML mirror functions or very simple.

;; Return an a tag constructructed from the URL and the anchor. 
;; If no anchor is provided uses the url as anchor text.
(define (a-tag url . optional-parameter-list)
 (let ((anchor (optional-parameter 1 optional-parameter-list url)))
  (a anchor 'href (as-string url))))

;; A variant of a tag which supports a target attribute of the a-tag (where in browser to show the result).
(define (a-tag-target url anchor target)
  (a
    anchor
    'href (as-string url)
    'target (as-string target)))

;; Name the current place by means of an a tag with name attribute
(define (a-name name)
  (a ""
    'name (as-string name)))  

;; Return a mail link by means of the mailto facility in an a tag.
;; .form (mail-link email-adr [anchor-name])
(define (mail-link email-adr . optional-parameter-list)
 (let ((anchor-name (optional-parameter 1 optional-parameter-list email-adr)))
  (a-tag (string-append "mailto:" email-adr) anchor-name)))

;; Return a horizontal rule, a hr tag.
(define (hr-1 . optional-parameter-list)
 (let ((size (optional-parameter 1 optional-parameter-list #f)))
  (if (not size)
      (hr)
      (hr 'size (as-string size)))))

;; Returns h tags, h1 if i=1, h2 if i=2, etc.
;; If i is higher than 6, use size 6.
(define (h i x)
  (cond ((= i 1) (h1 x))
        ((= i 2) (h2 x))
        ((= i 3) (h3 x))
        ((= i 4) (h4 x))
        ((= i 5) (h5 x))
        ((>= i 6) (h6 x))
  ))


;; Returns a font tag with size and color attributes.
;; size is a number or the symbol normal. color is a rgb list of three, decimal integers or
;; a color symbol from the color library.
;; Corresponds to the old function font in the ad hoc html library. 
(define (font-1 size color x)
  (font  x 'size (convert-size size) 'color (rgb-color-encoding color)))

(define (convert-size size)
  (if (and (symbol? size) (eq? size 'normal)) "3" (as-string size)))

;; Like font, but only supports size.
;; size is a number and color is a rgb list of three, decimal integers or
;; a color symbol.
(define (font-size size x)
  (font  x 'size (convert-size size)))

;; Like font, but only supports color.
;; color is a rgb list of three, decimal integers
(define (font-color color x)
  (font  x 'color (rgb-color-encoding color)))

;; Present x in a blockquote tag. 
;; This has nothing to do with the html cite tag.
;; Corresponds to the old function cite in the ad hoc html library. 
(define (cite-1 x)
  (blockquote  x))

;; Present the string converted x in a blockquote tag. 
;; Corresponds to the old function blockquote in the ad hoc html library. 
;; Don't use it - this function is only included to serve old LAML tools.
(define (blockquote-1 x)
  (blockquote  x))

;; Present a p tag (paragraph). If no parameter is given, just return a p tag without contens.
;; If a parameter is given, return x embedded in a p tag. 
;; Corresponds to the old function p in the ad hoc html library. 
;; Don't use it - this function is only included to serve old LAML tools. Use p from the surface library instead.
(define (p-1 . x)
  (if (null? x) (p #f) (p  (car x))))

;; Generate a function which appends element. 
;; As an example use, (html-appender (p)) is a function of one parameter, which appends a p tag to the argument of the function.
(define (html-appender element)
  (lambda (existing-stuff)
    (con existing-stuff explicit-space element)))

;; Return a html fonted version of str. Initial letter is sized base-size+1.
;; The rest is size base-size.
;; Returns a list, not an ast. If rendered directly, use render-1.
(define (font-rise str base-size)
  (con
     (font-size (+ base-size 1) (substring  str 0 1)) explicit-space
     (font-size base-size (substring  str 1 (string-length str)))))

;;; Lists and trees.
;;; In this section there are various convenient functions which renders lists and trees.

;; Return an ol (ordered list) tag. 
;; The elements in list becomes the elements. li tags are inserted automatically by this function.
;; A natural Scheme variant of the ol HTML mirror function, in which the items are represented as a Lisp list.
(define (ordered-list lst)
  (ol (map (lambda(el) (li el)) lst)))


;; A convenient alias for ordered-list. 
;; A natural Scheme variant of the ol HTML mirror function, in which the items are represented as a Lisp list.
;; Corresponds to the old function ol in the ad hoc html library. 
;; .form (ol-1 list)
;; .internal-references "see also" "ordered-list"
(define ol-1 ordered-list)

;; Return an ul (uordered list) tag. 
;; The elements in list becomes the elements. Li tags are inserted automatically by this function. 
;; A natural Scheme variant of the ul HTML mirror function, in which the items are represented as a Lisp list.
;;AST-fixed.
(define (unordered-list lst)
  (ul
       (map (lambda(el) (li  el 'type "disc")) lst)))


;; Return a flat list, separate by breaks. 
;; The parameter lst is a list of items. 
;; In this version, returns a list, which cannot be rendered directly. 
;; However, render-1 works on the result.
(define (br-list lst)
  (map (lambda(el) (con el (br))) lst))

;; A convenient alias for br-list
;; .form (brl lst)
;; .internal-references "see also" "br-list"
(define brl br-list)

;; A convenient alias for unordered-list. 
;; A natural Scheme variant of the ul HTML mirror function, in which the items are represented as a Lisp list.
;; Corresponds to the old function ul in the ad hoc html library. 
;; .form (ul-1 list)
;; .internal-references "see also" "unordered-list"
(define ul-1 unordered-list)


;; Make a definition list.
;; lst is a list of lists. Each inner list must be of length two,
;; dt and dd respectively. I.e. definition terms and the defintion proper, resp.
;; As a special case supported, the inner list can be of lenght one, in which case
;; the dd is considered empty.
(define (definition-list lst)
  (dl
   (map (lambda(el) 
	  (let ((dt-data (car el))
		(dd-data (if (= 1 (length el)) "" (cadr el))))
	    (con (dt  dt-data)
		 (if (equal? dd "")
		     ""
		   (dd dd-data)))))
	lst)))

;; A convenient alias for definition-list. 
;; A natural Scheme variant of the dl HTML mirror function
;; .form (dl-1 list)
(define dl-1 definition-list)

;; Render lst as a bulleted list. Both parameters must be symbols.
;; Bullet-size is either 'small or 'large.
;; Bullet-color is a string (not a color constant) either 'red, 'green, 'yellow, 'blue, or 'cyan.
;; Depends on the presence of appropriate gif files in images directory.
;; In order to use this function you must specify where to find the bullet images;
;; This is done by setting image-file-access via the function set-image-file-path!
;; OBSOLETE. 
(define (bullet-list lst bullet-size bullet-color)
   (map (lambda(el) 
	  (table-3 0 '(18 "*") 
                      (list (list (bullet-image bullet-size bullet-color) el)) "baseline"))
	lst))

;; A large, red bulleted list. 
;; A convenient special version of bullet-list.
(define (bl lst)
  (bullet-list lst 'large 'red))

(define (bullet-image size color)
  (let ((path (image-file-path))
        (filename (bullet-filename size color)))
    (img-0 (string-append path filename))))

; size and color are symbols, small or large, and red, green or yellow, respectivel
(define (bullet-filename size color)
  (string-append (as-string color) "-" "ball" "-" (as-string size) "." "gif"))

;; Show tree as an indented, bulleted list.
;; A tree is a list.
;; The first element of the list is the root.
;; The tail of the list is the list of subtrees.
;; A subtree, which is a leaf, can be given as a string (cdata) or a contents element.
;; <pre>
;; Example   (a (b c d) (e (f g) h))
;;
;;               a
;;             /   \\
;;            b     e
;;           / \\   / \\
;;          c  d  f   h
;;                |
;;                g
;; </pre>
(define (ul-tree tree)
  (cond ((or (cdata? tree) (ast? tree)) (ul (li 'type "disc" tree)))
        ((pair? tree) 
	 (ul
  	    (li (car tree) 'type "disc"
  	      (map ul-tree (cdr tree)))))))


; --------------------------------------------------------------------------------------------------------
;;; Table functions.
;;; In this section there is a number of table functions which
;;; maps a list of rows (list of lists) to an HTML table. 

;; Return a table with elements from list-of-list.
;; The sublists of list represent the rows in the table. The border is an optional parameter.
;; Corresponds to the old function table in the ad hoc html library. 
;; .parameter list-of-list the list of table row lists.
(define (table-0 list-of-list . optional-parameter-list)
  (let ((table-row 
         (lambda (lst) (tr
			(map (lambda (cell)
			       (td cell))
			     lst))))
	(border (optional-parameter 1 optional-parameter-list "1")))
    (table
     (con 
      (tbody
       (map table-row list-of-list)))
     'border (as-string border))))


;; A more versatile variant of table-0.
;; A variant of table-0 which requires border (an integer, 0 if no border),
;; a list of cell widths, a list of column colors, the table contents (list-of-list - list of rows), and an optional valign parameter.
;; The valign parameter corresponds to the HTML valign attribute, the possible value of which are
;; "top", "middle", "bottom", and "baseline" (a string).
(define (table-1 border cell-width-list cell-color-list-1 list-of-list . optional-parameter-list)
  (let ((va (as-string (optional-parameter 1 optional-parameter-list "top"))))
    (table
     (con
      (tbody
       (map
	(lambda (row)
	  (tr
	   (map (lambda (cell width color-1)
		  (td 
		   cell
		   'width (as-string width) 'valign va 'bgcolor (rgb-string-list color-1)
		   )
		  )
		row cell-width-list cell-color-list-1))
	  )
	list-of-list
	)
       ))
     'border (as-string border))))


;; A variant of table and table-1 which supports a header row.
(define (table-2 border cell-width-list cell-color-list-1 header-list list-of-list)
  (table
   (con
    (tbody
     (cons   ; if con then a strange error - research it and try to catch it in AST composition phase
      (tr
       (map (lambda (h)(th h)) header-list))
      (map
       (lambda (row)
	 (tr
	  (map (lambda (cell width color-1)
		 (td 
		  cell
		  'width (as-string width) 'valign "top" 'bgcolor (rgb-color-encoding color-1)
		  )
		 )
	       row cell-width-list cell-color-list-1)
	  ))
       list-of-list
       ))))
  'border (as-string border)))

;; A transparant variant of table-1 - without specification of a column color list.
;; The cell color becomes identical with the background.
(define (table-3 border cell-width-list  list-of-list . optional-parameter-list)
  (let ((va (as-string (optional-parameter 1 optional-parameter-list "top"))))
    (table
     (con
      (tbody
       (map
	(lambda (row)
	  (tr
	   (map (lambda (cell width)
		  (td 
		   cell
		   'width (as-string width) 'valign va
		   )
		  )
		row cell-width-list))
	  )
	list-of-list
	)
       ))
     'border (as-string border))))

;; A variant of table-1 with a row color list instead of a column color list.
;; The length of row-color-list must be the number of rows in the table.
(define (table-4 border cell-width-list row-color-list list-of-list . optional-parameter-list)
  (let ((va (as-string (optional-parameter 1 optional-parameter-list "top"))))
    (table
     (con
      (tbody
       (map
	(lambda (row row-color)
	  (tr
	   (map (lambda (cell width)
		  (td 
		   cell
		   'width (as-string width) 'valign va 'bgcolor (rgb-color-encoding row-color)
		   )
		  )
		row cell-width-list)))
	list-of-list row-color-list
	)
       ))
     'border (as-string border))))

;; A variant of table-4 that allows individual background coloring of table cells.
;; The parameter list-of-color-list is a list of row colors, whereas row-color-list in table-4
;; is a fixed list of colors that apply to a row. Thus, the structure of list-of-color-list
;; is identical to the structure of list-of-list.
;; .internal-references "similar function" "table-4"
(define (table-5 border cell-width-list list-of-color-list list-of-list . optional-parameter-list)
  (let ((va (as-string (optional-parameter 1 optional-parameter-list "top"))))
    (table
     (con
      (tbody
       (map
	(lambda (row row-color-list)
	  (tr
	   (map (lambda (cell width row-color)
		  (td 
		   cell
		   'width (as-string width) 'valign va 'bgcolor (rgb-color-encoding row-color)
		   )
		  )
		row cell-width-list row-color-list)
	   ))
	list-of-list list-of-color-list
	)
       ))
     'border (as-string border))))
 

;; Return a banner with left, middle, and right justified contributions.
(define (left-middle-right-banner left middle right)
  (table
   (con
   (tbody
    (con
    (tr
     (con
      (td
       (con (font-size 2 left))
       'width "33%" 'align "left" 'valign "top")

      (td
       (con (font-size 2 middle))
       'width "34%" 'align "center" 'valign "top")

      (td
       (con (font-size 2 right))
       'width "33%" 'align "right" 'valign "top")
      )
     ))))
   'border "0" 'cellpadding "0" 'cellspacing "0" 'width "100%")
  )

;; Return a banner with left and right justified contributions.
(define (left-right-banner left right)
 (let ((font-size (lambda (x y) y))) ; local redef to circumvent improper user of font-size
  (table
   (con
    (tbody
     (con
      (tr
       (con
	(td
	 (con (font-size 2 left))
	 'width "50%" 'align "left" 'valign "top")

	(td
	 (con (font-size 2 right))
	 'width "50%" 'align "right" 'valign "top")
	)
       ))))
   'border "0" 'cellpadding "0" 'cellspacing "0" 'width "100%"))
  )

;; Return the standard LAML top banner with time of generation, copyright, and home icon
(define (laml-top-banner)
 (let ((yr (car (time-decode (current-time)))))
  (left-middle-right-banner
     (when-generated)
     (string-append "Copyright " copyright " " (as-string yr) " , Kurt Nørmark")
     (laml-home-button 0 "laml-home.gif"))))

;; Makes a horizontal menu in terms of a table with links.
;; The table is made on the basis of parameter mini-menu-list, which is a list of
;; menu entries. A menu entry is a list of anchor-text and URL pairs (lists of
;; two strings). Dark-color must be some dark color.
(define (mini-menu mini-menu-list dark-color)
 (letrec ((mini-menu-entry (lambda (e) 
                             (let ((text (car e))
                                   (url (cadr e)))
                              (con (a (font-1 2 white text) 'href url 'css:text-decoration "none")
                                   ))))
          (lgt (length mini-menu-list)))
   (table-1
     1
     (make-list lgt 160)
     (make-list lgt dark-color)
     (list (map mini-menu-entry mini-menu-list)))))


; ---------------------------------------------------------------------------------------------------
;;; HTML input form functions. 
;;; A number of convenient functions which supports the work with HTML input forms.

;; Embed x in to form of kind POST, which activates url upon form completion. 
;; Corresponds to the old function form in the ad hoc html library. 
(define (form-1 cgi-url x)
  (form x 'method "POST" 'action cgi-url))

;; Embed x into a multipart form. Activate cgi-url when the form is submitted.
;; A multipart form is used for file uploading. Files are written into 
;; target-directory when uploaded.
;; The parameter target-directory-url gives the URL of the directory, in which the file is uploaded.
;; This is used for subsequent WWW retrival of the file.
;; .reference "accompanying function" "multipart-decode" "../../man/encode-decode.html#multipart-decode"
(define (multipart-form cgi-url target-directory target-directory-url x)
  (form 
     (con
       (hidden-line "target-directory!!!" target-directory)
       (hidden-line "target-directory-url!!!" target-directory-url)
       x
      )
     'method "POST" 'enctype "multipart/form-data" 'action cgi-url))

;; Return an input tag of type checkbox. The name is a string or symbol which identifies the checkbox.
;; Checked is an optional boolean parameter. If checked is #t, the checkbox will be checked initially.
;; Returns the string true to the form processing application if checked.
(define (checkbox name . checked)
 (let ((checked1 (if (null? checked) #f (car checked))))
  (if checked1 
    (input 'type "CHECKBOX" 'checked "checked" 'value "true" 'name (as-string name))
    (input 'type "CHECKBOX" 'value "true" 'name (as-string name)))))

;; Return an input tag of type radio. 
;; checked is a boolean parameter, i.e. true or false (in Scheme sense). 
(define (radio-button value group-name . checked)
 (let ((is-checked (and (not (null? checked)) (boolean? (car checked)) (car checked))))
  (if is-checked
    (input 'type "RADIO" 'checked "checked" 'value (as-string value) 'name (as-string group-name))
    (input 'type "RADIO" 'value (as-string value) 'name (as-string group-name))
  )))

;; Return an input tag of type text. 
;; The name is a string of symbol which identifies the text line.
;; Size is the text line width in character positions.
;; Value is the initial value on the text line.
(define (text-line name size value)
  (input 'type "TEXT" 'name (as-string name) 'size (as-string size) 'value (as-string value)))

;; Return an input tag of type hidden. 
;; The name is a string or symbol which identifies the hidden line.
;; Value is the string contents of the hidden line.
(define (hidden-line name value)
  (input 'type "HIDDEN" 'name (as-string name) 'value (as-string value)))

;; Return an input tag of type file.
;; Such an input tag is used for file uploading.
;; The name of the uploading is name.
(define (file-upload name)  
  (input 'type "file" 'name (as-string name)))

;; Return an input tag of type password. 
;; The name is a string of symbol which identifies the password.
;; Size is the line width in character positions.
;; Value is the initial contents of the password field (not very useful...).
(define (password-line name size value)
  (input 'type "PASSWORD" 'name (as-string name) 'size (as-string size) 'value (as-string value)))

;; Return an input tag of type submit. Renders a button. 
;; Value is the string label of the button. If the optional parameter name
;; is given it identifies a particular submit button with a name, value pair in the submitted data.
;; .form (submit value [name])
(define (submit value . optional-parameters)
 (let ((name (optional-parameter 1 optional-parameters #f)))
  (if name
      (input 'type "SUBMIT" 'value (as-string value) 'name (as-string name))
      (input 'type "SUBMIT" 'value (as-string value)))))

;; Return an input tag of type reset. 
;; Value is the string label of the button.
(define (reset value)
  (input 'type "RESET" 'value (as-string value)))

;; Return a select tag, defining  multiple choice menu. Name is a string or symbol which identifies the selection.
;; Value-list is a list of the values to be returned upon selection.
;; Contents-list is the list of contents to be shown in the menu.
;; Selected-value is an optional value, which is to be selected initially. This value should be a member of value-list.
;; Corresponds to the old function select in the ad hoc html library. 
(define (select-1 name value-list contents-list . selected-value)
 (let* ((selected (if (null? selected-value) "" (car selected-value)))
        (body (map (lambda (value contents)
                           (if (equal? selected value)
                               (option (as-string contents) 'value (as-string value) 'selected "selected") 
                               (option (as-string contents) 'value (as-string value)))) 
			 value-list contents-list))
       )
   (select body 'name (as-string name))))

;; Return a textarea form. 
;; Rows is the number of rows of the text area.
;; Cols is the number of columns measured in characters.
;; Contents is the initial contents of the text area.
;; Corresponds to the old function textarea in the ad hoc html library. 
(define (textarea-1 name rows cols contents)
  (textarea (as-string contents) 'name (as-string name) 'rows (as-string rows) 'cols (as-string cols)))

; ---------------------------------------------------------------------------------------------------


;;; Page functions. 
;;; A number of convenient functions which make the overall elements of an HTML page.

;; Return a simple HTML page with html, head and title tags.
;; Also include the value of the parameter-less function (laml-standard-comment) as an initial html-comment.
;; The optional color-list must consist of a background color, text color, link color and visited link color.
(define (page ttl bdy . color-list)
 (if (and (not (null? color-list)) (not (= 4 (length color-list))))
     (display-warning "page: Either non or exactly four colors (bgcolor text link vlink) must be supplied"))
 (let ((color-attributes
         (if (null? color-list)
             '()
             (let ((bg-color (car color-list))
                   (text-color (cadr color-list))
                   (link-color (third color-list))
                   (vlink-color (fourth color-list)))
              (list 'bgcolor (rgb-color-encoding bg-color) 'text (rgb-color-encoding text-color)
                    'link (rgb-color-encoding link-color) 'vlink (rgb-color-encoding vlink-color)))))
      )
  (string-append
   (render
    (html
      (head 
        (title ttl)
      )
      (body general-page-message bdy color-attributes)))
    (laml-standard-comment) (newline-string)
    (tracing-comment) (newline-string))))


(define (css-link-clause relative-style-file)
  (link 'href relative-style-file 'rel "stylesheet" 'title (file-name-proper relative-style-file) 'type "text/css"))

;; Return an HTML page with html, head, meta and title tags.
;; This function is a more elaborate version of the page function from above.
;; This function assumes that the functions tracing-comment and meta-tag-clauses are defined.
;; The function meta-tag-clauses must return a property list of meta-descriptors, which represents
;; the standard meta clauses of this page. 
;; The parameter meta-contributions is a property list of meta descriptors: (key val key val ...).
;; Meta-tag-clauses and meta-contributions go into a HTML meta-tag in the beginning of the generated document.
;; Tracing-comment must return a string. Tracing-comment is meant to let us trace the LAML source files etc.
;; Tracing-comment is defaulted to return the empty string.
;; The first part of the string output is the standard DOCTYPE.
;; The keys must be symbols, the values must be strings.
;; The last four parameters must consist of a background color, text color, link color and visited link color (all color RGB lists).
;; Corresponds to the old function html:page in the html-v1 library. 
;; The first optional parameter addresses a css-style-file-list (defaulted to #f).
;; The second optional parameter gives the kind of body (css class) of the body of the page.
(define (page-1 ttl meta-contributions bdy   bg-color text-color link-color vlink-color . optional-parameter-list)
 (let ((meta-tags (append (meta-tag-clauses) meta-contributions))
       (copyright-cl (copyright-clause))
       (css-style-file-list (optional-parameter 1 optional-parameter-list #f))
       (kind-of-body (optional-parameter 2 optional-parameter-list #f))
      )
   (string-append
    (if (not (empty-string? copyright-cl)) (string-append copyright-cl (newline-string)) "")
    (doctype-clause) (newline-string)
    (render
     (html
      (head
       (if (not (null? meta-tags)) (make-meta-contribution meta-tags) '())
       (if css-style-file-list
	   (map css-link-clause css-style-file-list)
	   '())
       (title ttl)
       (if (is-a-laml-directory? (startup-directory)) (laml-shortcut-icon (laml-home-url-prefix)) "")
      )
      (body
        general-page-message bdy
        (list 
	 'bgcolor (rgb-color-encoding bg-color)
	 'text (rgb-color-encoding text-color)
	 'link (rgb-color-encoding link-color)
	 'vlink (rgb-color-encoding vlink-color))
        (if kind-of-body
            (list 'class kind-of-body)
	    '())
      )
    ))
    (newline-string)
    (laml-standard-comment) (newline-string)
    (tracing-comment) (newline-string))))



(define (make-meta-contribution list-of-meta-descriptors)
 (map make-a-meta-contribution list-of-meta-descriptors))

(define (make-a-meta-contribution meta-descriptor)
  (meta meta-descriptor))

;; A version of the page and page-1 functions with Javascript scripts as a new fourth to sixth parameter. 
;; The parameter script-loading can either be static or dynamic (symbols). If static, include the script in the html document.
;; If dynamic, refer to the script in the a javascript subdirectory.
;; The parameter script is a list of two file path components (prefix-static-part js-filename), both strings.
;; If script-loading is static (string-append prefix-static-part js-filename) is the name of the script file, the contents of 
;; which must be included inline.
;; If script-loading is dynamic js-filename is the name of a file in the html/javascript directory, including extension.
;; The parameter keypress-action is supposed to be an activation of a Javascript function defined in script.
;; The optional parameter is either single-press or double-press (a symbol). Determines whether advancement to next page is via single or double mouse click.
;; Default optional parameter is double-pres. This works in concert with the the loaded script.
;; Uses the function read-text-file from the file-read library.
;; In addition to key press events, single or double click mouse press is also supported (advancement to a designated next page).'
;; The second optional parameter addresses a css-style-file-list (defaulted to #f).
;; The third optional parameter gives the kind of body (css class) of the body of the page.
(define (page-with-keypress-script ttl meta-contributions bdy   script-loading script keypress-action   
                                   bg-color text-color link-color vlink-color . optional-parameter-list)
 (let ((advance (optional-parameter 1 optional-parameter-list 'double))
       (css-style-file-list (optional-parameter 2 optional-parameter-list #f))
       (kind-of-body (optional-parameter 3 optional-parameter-list #f))
       (meta-tags (append (meta-tag-clauses) meta-contributions))
       (script-clause 
        (cond ((eq? script-loading 'static) (script (read-text-file (string-append (car script) (cadr script))) 'type "text/javascript"))
	      ((eq? script-loading 'dynamic) (script "" 'src (string-append "javascript/" (cadr script)) 'type "text/javascript"))
	      (else (error "page-with-keypress-script in convenience: problems determining type of script loading"))))
      )
   (string-append
    (doctype-clause) (newline-string)
    (render
     (html
      (head
        (if (not (null? meta-tags)) (make-meta-contribution meta-tags) '())
        (if css-style-file-list
            (map css-link-clause css-style-file-list)
            '())
        (title ttl)
        (if (is-a-laml-directory? (startup-directory)) (laml-shortcut-icon (laml-home-url-prefix)) "")
        script-clause
       )
      (body 
	general-page-message bdy
	(list 
	 'onKeyPress keypress-action
	 (cond ((eq? advance 'double-press) 'onDblClick)
	       ((eq? advance 'single-press) 'onClick)
	       (else (error (string-append "page-with-keypress-script: optional parameter must be single-press or double-press"))))
	 keypress-action)
	(list
	 'bgcolor (rgb-color-encoding bg-color)
	 'text (rgb-color-encoding text-color)
	 'link (rgb-color-encoding link-color)
	 'vlink (rgb-color-encoding vlink-color))
        (if kind-of-body
	   (list 'class kind-of-body)
	   '()))))
    (laml-standard-comment) (newline-string)
    (tracing-comment) (newline-string))))

;; A variant of page-1 and page-with-keypress-script which includes show-time (in seconds) and continuation-url.
;; An empty string in continuation-url serves as 'no continuation'.
;; The first optional parameter addresses a css-style-file-list (defaulted to #f).
;; The second optional parameter gives the kind of body (css class) of the body of the page.
(define (timed-page ttl show-time continuation-url meta-contributions bdy   bg-color text-color link-color vlink-color . 
                    optional-parameter-list)
 (let ((meta-tags (append (meta-tag-clauses) meta-contributions))
       (css-style-file-list (optional-parameter 1 optional-parameter-list #f))
       (kind-of-body (optional-parameter 2 optional-parameter-list #f))
       (copyright-cl (copyright-clause))
      )
   (string-append
    (if (not (empty-string? copyright-cl)) (string-append copyright-cl (newline-string)) "")
    (doctype-clause) (newline-string)  
    (render
    (html
      (head
        (if (not (null? meta-tags)) (make-meta-contribution meta-tags) '())
        (if css-style-file-list
            (map css-link-clause css-style-file-list)
            '())
        (title ttl)
        (script "function navigateNext(nextUrl,ms){setTimeout(\"window.location = \" + \"'\" + nextUrl + \"'\",ms);}" 
                    'type "text/javascript")
       )
      (body 
        bdy
	(list 
	 'bgcolor (rgb-color-encoding bg-color)
	 'text (rgb-color-encoding text-color)
	 'link (rgb-color-encoding link-color)
	 'vlink (rgb-color-encoding vlink-color))
	(list 
	 'onload 
	 (js-call "navigateNext" (list (string-it-single continuation-url) (* (as-number show-time) 1000))))
        (if kind-of-body
	     (list 'class kind-of-body)
           '()))))
    (newline-string)
    (laml-standard-comment) (newline-string)
    (tracing-comment) (newline-string))))

;; A combination of time-page and page-with-keypress-script (with timed-page as starting point).
;; There are three extra parameters compared to timed-page: script-loading script keypress-action.
(define (timed-page-with-keypress-script ttl show-time continuation-url meta-contributions bdy
                                         script-loading script keypress-action
                                         bg-color text-color link-color vlink-color . 
                                         optional-parameter-list)
 (let ((meta-tags (append (meta-tag-clauses) meta-contributions))
       (css-style-file-list (optional-parameter 1 optional-parameter-list #f))
       (kind-of-body (optional-parameter 2 optional-parameter-list #f))
       (copyright-cl (copyright-clause))

       (advance 'double-press) ; fixed value in this version of timed-page-with-keypress-script
       (key-script-clause 
        (cond ((eq? script-loading 'static) (script (read-text-file (string-append (car script) (cadr script))) 'type "text/javascript"))
	      ((eq? script-loading 'dynamic) (script "" 'src (string-append "javascript/" (cadr script)) 'type "text/javascript"))
	      (else (error "page-with-keypress-script in convenience: problems determining type of script loading"))))

      )
   (string-append
    (if (not (empty-string? copyright-cl)) (string-append copyright-cl (newline-string)) "")
    (doctype-clause) (newline-string)
    (html
     (con
      (head
       (con
        (if (not (null? meta-tags)) (make-meta-contribution meta-tags) "")
        (if css-style-file-list
            (apply 
              string-append
              (map css-link-clause css-style-file-list))
            "")
        (title ttl)
        (script "function navigateNext(nextUrl,ms){setTimeout(\"window.location = \" + \"'\" + nextUrl + \"'\",ms);}" 
                    'type "text/javascript")
        key-script-clause
       )
      )

      (apply body 
       (append
	(list bdy)
        (list 
	 'onKeyPress keypress-action
	 (cond ((eq? advance 'double-press) 'onDblClick)
	       ((eq? advance 'single-press) 'onClick)
	       (else (error (string-append "advance must be single-press or double-press"))))
	 keypress-action)
	(list 
	 'bgcolor (rgb-color-encoding bg-color)
	 'text (rgb-color-encoding text-color)
	 'link (rgb-color-encoding link-color)
	 'vlink (rgb-color-encoding vlink-color))
	(list 
	 'onload 
	 (js-call "navigateNext" (list (string-it-single continuation-url) (* (as-number show-time) 1000))))
        (if kind-of-body
	     (list 'class kind-of-body)
           '())))
      )
     ) 
    (newline-string)
    (laml-standard-comment) (newline-string)
    (tracing-comment) (newline-string)
    )))


;; Write a HTML page consisting of header and body to file. 
;; File is without extension. This function adds the html extension
(define (write-html-page file header body)
  (write-text-file 
    (page header (string-append general-page-message body))
    (string-append file ".html")))


;; The result of this function is inserted as an HTML comment on each html page generated.
;; Redefine this function for your own tracing.
;; As of here, it returns the empty string.
;; .internal-references "functions using this variable" "page" "page-1" "page-with-keypress-script"
(define (tracing-comment) "")


;; A message (string) which is presented at the top of every page generated.
;; The default value is the empty string.
;; Must be redefined or re-assigned after the loading of this file.
;; The message is supposed to end with a proper line terminator, such as (br) or (p).
;; .internal-references "functions using this variable" "page" "page-1" "page-with-keypress-script"
(define general-page-message "")



; ---------------------------------------------------------------------------------------------------
;;; Multi column lists. 
;;; The functions in this section return multi-column lists. Given a list of elements the functions
;;; return a table in which the elements have been arranged in a number of columns. The first function,
;;; multi-column-list, arranges the elements in row major order. The two last functions arrange the
;;; the elements in column major order. These are the most advanced functions due to the way tables
;;; are organized in HTML.


;; Return a multi-column list, row major, with columns columns.
;; Columns (the first parameter) must be at least 2.
;; The total width (sum of column widths) is given as the last parameter.
;; Internally, a HTML table with zero border is formed and returned.
(define (multi-column-list columns elements total-width)
 (let* ((lgt (length elements))
        (rem (remainder lgt columns))
        (elements-2 (cond ((= lgt 0) (make-list columns " "))     ; ensure that list length is a multiplum of column, and at least column
                          ((= 0 rem) elements)
                          (else (append elements (make-list (- columns rem) " ")))))
        (rows (sublist-by-rows columns elements-2))
        (column-width (quotient total-width columns))
        (column-widths (make-list columns column-width)))
    (table-3 0 column-widths rows)))


;; Return a two column list, column major.
;; total-width (sum of column widths) is the width you want the resulting table to have.
;; Internally, a HTML table with zero border is formed and returned.
(define (two-column-list elements total-width)
 (let* ((lgt (length elements))
        (rem (remainder lgt 2))
 
        ; not used any more
        ; ensure that list length is a multiplum of column, and at least column
        (elements-2 (cond ((= lgt 0) (make-list 2 " ")) 
                          ((= 0 rem) elements)
                          (else (append elements (make-list (- 2 rem) " ")))))

        (rows (sublist-by-2columns elements " "))
        (column-width (quotient total-width 2))
        (column-widths (make-list 2 column-width)))
    (table-3 0 column-widths rows)))

;; Return an n column list, column-major, of the element list (second parameter).
;; This is a generalized version of two-column-list.
;; total-width (sum of column widths) is the width you want the resulting table to have.
;; n is the number of columns.
;; Internally, a HTML table with zero border is formed and returned.
(define (n-column-list n elements total-width)
 (let* ((lgt (length elements))
        (rows (sublist-by-columns n elements " "))
        (column-width (quotient total-width n))
        (column-widths (make-list n column-width)))
    (table-3 0 column-widths rows)))


;;; Images and image file access. 
;;; The functions in this section determine how images are accessed from this
;;; and other libraries.

;; The URL where the author of this library keeps a number of images (icons).
;; This variable is used if image-file-access is the symbol net.
(define kn-internet-image-path "http://www.cs.auc.dk/~normark/images/")

;; Determination of the actual file path to images in html files.
;; This function depends on the variable image-file-access, which MUST be defined external to this library.
;; The value of image-file-access can be changed via the procedure set-image-file-access!
;; One of the following symbols apply: local, parent, net, sub-directory, or fixed. Default is local.
;; local means that images are taken from the current directory.
;; parent means that images are tagen from ../images.
;; sub-directory means that images are taken from ./images.
;; fixed means that images are taken from fixed-image-directory (a variable which must be defined
;; external to this library).
;; Finally, net means that images are taken from kn-internet-image-path (a variable).
(define (image-file-path)
  (cond ((eq? image-file-access 'local) "")
        ((eq? image-file-access 'parent) "../images/")
        ((eq? image-file-access 'sub-directory) "./images/")
        ((eq? image-file-access 'net) kn-internet-image-path)
        ((eq? image-file-access 'fixed) fixed-image-directory)
  ))

;; Set the image-file-access variable. 
;; mode is symbol, either local, parent, sub-directory, net, or fixed.
;; Relative to the hmtl directory, where the target files are written.
(define (set-image-file-path! mode)
  (set! image-file-access mode))

;; Return the full path/address of the image file named file-name. 
;; Relies of the setting of the variable image-file-access via the procedure set-image-file-path!
;; File name must include file extension
(define (image-file file-name)
  (string-append (image-file-path) file-name ))


;; Return an img tag, in which a file on file-name is presented. An optional width parameter is supported. 
;; Corresponds to the old function img in the ad hoc html library. 
(define (img-0 file-name . width)
 (if (not (null? width))
  (img 'alt "" 'src (as-string file-name) 'width (as-string (car width)) 'border "0")
  (img 'alt "" 'src (as-string file-name) 'border "0")))

;; A variant of img which presents an image with a border
(define (img-with-border file-name . width)
 (if (not (null? width))
  (img 'src (as-string file-name) 'width (as-string (car width)))
  (img 'src (as-string file-name))))


;; Return an HTML a tag (anchor) which links to the LAML software home page via an small gif icon.
;; If possible, a relative URL is used as the href attribute.
;; The parameter extra-level is an extra level to add. Normally, extra-level is 0 (zero).
;; As an example, extra-level should be 1 in case HTML files are organized
;; in a sub-directory relative to the laml source file.
;; Text-or-image is either the symbol 'text or 'image, or a string. If 'text, a textual anchor is used.
;; if 'image, a small 'laml house' is used as image. If text-or-image is a string it is a name of an image file
;; in the laml image directory (including file name extension, excluding any file path).
;; The optional start-dir gives the directory, in which the home button is to be placed; It defaults to (startup-directory).
(define (laml-home-button extra-level text-or-image . start-dir)
 (let* ((start-dir-1 (if (null? start-dir) (startup-directory) (car start-dir)))
        (url-of-laml (laml-home-url-prefix extra-level start-dir-1))
        (help-text 
           (if (equal? url-of-laml laml-absolute-url-prefix)
               "The LAML software home page at Aalborg University"
               "The local LAML software home page"))
        (image-file 
           (cond ((eq? text-or-image 'text) "")      ; not defined
                 ((eq? text-or-image 'image) "images/blue-house.gif")
                 ((string? text-or-image) (string-append "images/" text-or-image))
                 (else "???")))
       )
   (a
     (cond ((eq? text-or-image 'text) "LAML home")
           ((or (eq? text-or-image 'image) (string? text-or-image))
                    (img 'src (string-append url-of-laml image-file) 'alt help-text 'border 0))
           (else "LAML home"))
     'href (string-append url-of-laml "index.html")
     'title help-text
     'target "_top")))


; ---------------------------------------------------------------------------------------------------
;;; Indenting, boxing, and framing.
;;; Here is a number of functions of indentation and frame making.

;; Indent text with p pixels
(define (indent-pixels p text)
  (table-3 0 (list p "*") 
    (list (list "" text))))

;; Show text in a column, narrowed from both left and right with p pixels
(define (narrow-with-pixels p text)
  (table-3 0 (list p "*" p) 
    (list (list "" text ""))))

;; Shown text in a simple frame. 
;; Has nothing to do with HTML frames.
;; Corresponds to the old function frame in the ad hoc html library. 
(define (frame-1 text) 
  (table-3 1 (list "*") 
    (list (list text))))

;; Embed text in an invisible (border-less) table with one cell.
;; The optional parameter, width, can be used to control the width of the table cell (defaults to "*").
;; .form (box text [width])
(define (box text . optional-parameter-list) 
 (let ((width (optional-parameter 1 optional-parameter-list "*")))
  (table-3 0 (list width)
    (list (list text)))))

;; Present the contents-list, which is a list of elements, in a narrow
;; column of width, separated with activations of separator-fn.
(define (narrow separator-fn width . contents-list)
  (let ((separator-list (make-list (- (length contents-list) 1) (separator-fn))))
   (table-3 0 (list width) 
    (list 
      (list (merge-lists-simple contents-list separator-list))))))

;; Embed text into a color frame. It is the background which is colored.
(define (color-frame text color) 
  (table-1 0 (list "*") (make-list 1 color)
    (list (list text)) "bottom"))

;; As color-frame, but this function supports and extra width parameter. This is an integer: the width of the frame in pixels.
(define (color-frame-width text color width) 
  (table-1 0 (list width) (make-list 1 color)
    (list (list text)) "bottom"))

;; Like frame, but with an extra width parameter. This is an integer: the width of the frame in pixels.
(define (frame-width text width) 
  (table-3 1 (list width) ; (make-list 1 slide-background-color)
    (list (list text))))

;; Embed text into a centered frame
(define (center-frame indentation text)
  (center
   (narrow-with-pixels indentation
     (frame-1 text))))


;;; Alphabetical index arrays. 
;;; The alphabetic index arrays are useful for presentation of alphabets linking to separate pages in a large index.

;; Return an 'array' of letter links to #letter
(define (alphabetic-link-array)
 (map 
  (lambda (letter)  (con (a-tag (string-append "#" letter) (capitalize-string-nd letter))  (horizontal-space 1)))
  (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" 
	"p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "æ" "ø" "å")))

;; Return an 'array' of letter links to (string-append target-file-prefix "-" letter ".html") for all letters in alphabet. 
;; This is a generalized version of alphabetic-link-array.
;; target-file-prefix is a prefix of the file names, in which the index files are located.
;; alphabet is a list of letters, for which to generate index links from the alphabet arrays. Some letters
;; may be missing from the alphabet compared with a complete alphabet.
;; emphasis-letter is an optional letter which we emphasize in the link array
(define (alphabetic-link-array-1 target-file-prefix alphabet . emphasis-letter)
  (let* ((em-let (if (not (null? emphasis-letter)) (as-string (car emphasis-letter)) #f))
         (alphabet-1 (map as-string alphabet)))
    (map 
     (lambda (letter) 
       (con
	(a-tag (string-append target-file-prefix "-" letter ".html")
	       (if (and em-let (equal? em-let letter))
		   (font-1 4 red (b (capitalize-string-nd letter)))
		   (capitalize-string-nd letter)))
	" "
       ))
     alphabet-1)))

; ---------------------------------------------------------------------------------------------------
;;; Substring Coloring.
;;; The function colorize-substring in this section is able to colorize specified substrings
;;; of a given string.

;; This is an advanced function which make font changes to substrings of str.
;; Surround substrings of str, as specified by the third parameter, in font tags.
;; Region-color-list is a list of coloring descriptors.  <p>
;; Each color descriptor is of the form:
;; (from-string to-string color face multiplicity). <p>
;; Face and multiplicity are optional.
;; From-string and to-strings delimits and identifies a substring of str to colorize etc.
;; color is a list of three integers: a rgb list.
;; We support the following face symbols: italic, bold, typewriter, underlined, plain (default bold).
;; Multiplicity is an integer telling how many times to to attempt the colorization on str (default 1). <p>
;; NB: In strange situations, the fontification of an early region-color-element may
;; affect the searching for latter region-color-elements. This is not an error, but a consequence of
;; the way font tags are puted into str.
(define (colorize-substrings str region-color-list)
  (set! last-coloring-length 0)
  (if (null? region-color-list)
      str
      (let* ((region-color (car region-color-list))
             (from-str (car region-color))
             (to-str (cadr region-color))
             (color (caddr region-color))
             (face (if (>= (length region-color) 4) (cadddr region-color) 'bold))
             (multiplicity (if (>= (length region-color) 5) (fifth region-color) 1))
            )
        (colorize-substrings
          (font-substring str 0 from-str to-str color face multiplicity)
          (cdr region-color-list)))))

; Return a face start tag of a given face symbol. 
; We support the following face symbols: italic, bold, typewriter, underlined, plain
(define (face-start-tag face-symbol)
  (cond ((eq? face-symbol 'italic) (start-tag 'i) )
        ((eq? face-symbol 'bold) (start-tag 'b))
        ((eq? face-symbol 'typerwriter) (start-tag 'kbd))
        ((eq? face-symbol 'underlined) (start-tag 'u))
        ((eq? face-symbol 'plain) "")
        (else (error "face start tag: Unknown face symbol"))
  )
)

; Return a face end tag of a given face symbol.
(define (face-end-tag face-symbol)
  (cond ((eq? face-symbol 'italic) (end-tag 'i))
        ((eq? face-symbol 'bold) (end-tag 'b))
        ((eq? face-symbol 'typerwriter) (end-tag 'kbd))
        ((eq? face-symbol 'underlined) (end-tag 'u))
        ((eq? face-symbol 'plain) "")
        (else (error "face end tag: Unknown face symbol"))
  )
)


; holds the length of font text from last substitution
(define last-coloring-length 0)
                               

(define (repeat-colorizing str start-index from-str to-str color face n)
  (if (> n 0)
      (font-substring str start-index from-str to-str color face n)
      str))
             
; surround a substring, delimited by from-delimiting-string and to-delimiting-string, by a html font tag
; with a color attribute.
; starting looking for delimiting strings at from-index
(define (font-substring str start-index from-delimiting-string to-delimiting-string color face multiplicity)
  (let ((from-index (substring-index str start-index from-delimiting-string)))
   (if from-index
       (let ((to-index (substring-index str 
                           (+ from-index (string-length from-delimiting-string))  ; addition 10.9.98
                           to-delimiting-string)))
          (if to-index
              (repeat-colorizing 
                 (font-substring-by-index str from-index (+ to-index (string-length to-delimiting-string)) color face)
                 (+ to-index last-coloring-length) from-delimiting-string to-delimiting-string color face (- multiplicity 1))
              (error (string-append "Substring fonting/colorizing: Cannot find the to delimiting strings: "
                                       to-delimiting-string))))
        (error (string-append "Substring fonting/colorizing: Cannot find the from delimiting strings: "
                               from-delimiting-string)))))


; to-index is larger than from-index.
; insert a font tag around index range
(define (font-substring-by-index str from-index to-index color face)
 (let* ((pre (string-append (face-start-tag face) (start-tag 'font 'color (rgb-color-encoding color))))
        (post (string-append (end-tag 'font) (face-end-tag face)))
       )
   (set! last-coloring-length (+ (string-length pre) (string-length post)))
   (put-around-substring
      str from-index pre to-index post)))



; ---------------------------------------------------------------------------------------------------

;;; Support of imperative printing of tags. 
;;; In some special situations it may be desirable to generate or print fragments 
;;; of an HTML element in terms of indidual start and end tags. Here we provide for
;;; functions which allow us to generate the initial and the final portions of an HTML
;;; document in separation from each other. These aspects are purely textual, and has
;;; nothing to do with ASTs. There is no HTML validation in play.

;; Return the part of an HTML page before the body. 
(define (pre-page ttl . color-list)
 (let ((color-attributes
         (if (null? color-list)
             '()
             (let ((bg-color (first color-list))
                   (text-color (second color-list))
                   (link-color (third color-list))
                   (vlink-color (fourth color-list)))
              (list 'bgcolor (rgb-color-encoding bg-color) 'text (rgb-color-encoding text-color)
                    'link (rgb-color-encoding link-color) 'vlink (rgb-color-encoding vlink-color))))))
  (string-append
    (laml-standard-comment) (newline-string) (tracing-comment) (newline-string)
    (start-tag 'html) 
      (render (head (title ttl))) 
        (apply start-tag (cons 'body color-attributes)))))

;; Return a pre-page, without body and trailing html stuff.
;; Used for imperative output of an HTML page.
;; Usage: First call this function, then output the body, and then call post-page or post-page-with-comments.
;; Corresponds to the old function html:pre-page-with-keypress-script in the html-v1 library. 
;; With respect to the parameters see the function page-with-keypress-script.
(define (pre-page-with-keypress-script ttl meta-contributions   script-loading script keypress-action   
                                                bg-color text-color link-color vlink-color . optional-par)

 (let ((advance (if (null? optional-par) 'double (car optional-par)))
       (meta-tags (append (meta-tag-clauses) meta-contributions))
       (script-clause (cond ((eq? script-loading 'static) (script (read-text-file (string-append (car script) (cadr script))) 'type "text/javascript"))
                            ((eq? script-loading 'dynamic) (script "" 'src (string-append "javascript/" (cadr script)) 'type "text/javascript"))
                            (else (error "page-with-keypress-script convenience: problems determining type of script loading"))))
      )
   (string-append
    (doctype-clause) (newline-string)

    (start-tag "html")
    (render (head
     (con
      (if (not (null? meta-tags)) (make-meta-contribution meta-tags) '())
      (title ttl)
     )
    ))
    script-clause
    (start-tag "body"
               'onKeyPress keypress-action
               (cond ((eq? advance 'double-press) 'onDblClick)
                     ((eq? advance 'single-press) 'onClick)
                     (else (error (string-append "pre-page-with-keypress-script: optional parameter must be single-press or double-press"))))
               keypress-action
               'bgcolor (rgb-color-encoding bg-color)
               'text (rgb-color-encoding text-color)
               'link (rgb-color-encoding link-color)
               'vlink (rgb-color-encoding vlink-color)))))

;; Return the part of an HTML page after the body. 
;; This includes the end body and the end html tags.
(define (post-page)
  (string-append (end-tag 'body) (end-tag 'html)))

;; Return the part of an HTML page after the body together with trailing laml standard comment and tracing comment.
;; This includes the end body and the end html tags. Furthermore the laml standard comment and the
;; tracing comment is printed as HTML comments.
;; Corresponds to the old function html:post-page in the html-v1 library. 
(define (post-page-with-comments)
  (con 
    (end-tag "body")
    (end-tag "html") (newline-string)
    (laml-standard-comment) (newline-string)
    (tracing-comment) (newline-string)))




; --------------------------------------------------------------------------------------------------------
;;; Miscelaneous

;; Embed x into a copyright indication
(define (copyright-owner x) (string-append x " " copyright))

; Redefinition of laml-welcome, with a special welcome to the html4.01 transitional mirror
(define (laml-welcome)
  (let ((vers (read-text-file (string-append laml-dir "distribution-version"))))
    (display (string-append "Welcome to LAML " vers " using HTML4.01 transitional")) (newline) 
    (display "(C) Kurt Normark, Aalborg University, Denmark.") (newline) ))

; --------------------------------------------------------------------------------------------------------

; Obsolte functions - reporting errors

(define (report-as-list alist-input)
  (error "report-as-list is obsolete. Use ul, ol or a similar function instead"))

(define (report-as-table alist-input)
  (error "report-as-table is obsolete. Use table-1, table-2, table-3 instead"))


