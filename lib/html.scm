; The LAML library and programs written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999  Kurt Normark.
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


;;;; This is the library of Scheme functions which mirrors HTML. The set of 
;;;; functions in this library is a rather unsystematic, ad hoc collection. Nevertheless,
;;;; it has proven quite useful. Much of our current software uses this library heavily. <p>
;;;; 
;;;; We have made 'a new and more systematic beginning' with a set of basic HTML mirror functions
;;;; in the library html-v1. In the future we intend to built op top of this basis.<p>
;;;; 
;;;; <b> This library is now obsolete</b>. Use <a href = "../html4.01-transitional-validating/man/convenience.html"> the convenience library </a>
;;;; together with the <a href="../html4.01-transitional-validating/man/basis.html">basis mirror</a> and <a href="../html4.01-transitional-validating/man/surface.html">surface mirror</a> of HTML4.01 transitional.

;;; Document type definition. 
;;; This section contains <kbd>document-type-declaration</kbd>, that returns the appropriate
;;; document type declaration of this mirror.

;; Return a document type declaration of this mirror.
(define (document-type-declaration)
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
        \"http://www.w3.org/TR/html4/loose.dtd\">")


;;; Table functions.
;;; A set of functions which generate HTML tables from Scheme list-of-list structures.

;; Return a table with elements from list-of-list. 
;; The sublists of list represent the rows in the table. The border is an optional parameter.
(define (table list-of-list . border)
  (let ((bdr (string-append "border = " (if (null? border) "1" (as-string (car border))))))
   (string-append
    "<table " bdr ">"
      (apply string-append
         (map table-row list-of-list))
    "</table>")))

(define (table-row lst)
  (string-append
   "<tr>"
      (apply string-append
             (map (lambda (cell)
                     (string-append "<td>" (as-string cell) "</td>"))
                  lst))               
   "</tr>"))

; more advance version of table, called table-1

;; A more versatile variant of table.
;; A variant of table which requires border (an integer, 0 if no border),
;; a list of cell widths, a list of column colors, the table contens (list-of-list), and an optional valign parameter

(define (table-1 border cell-width-list cell-color-list-1 list-of-list . valign)
  (let ((bdr (string-append "border = " (as-string border)))
        (va (if (null? valign) "top" (as-string (car valign)))))
    (string-append
     "<table " bdr ">"
     (apply string-append
	    (map
	     (lambda (row)
               (string-append
                "<tr>"
                (apply string-append
                       (map (lambda (cell width color-1)
                              (string-append "<td width = " (as-string width) " " "valign = " va " "
                                             "bgcolor = " (string-it (rgb-string-list color-1)) ">"
                                             (as-string cell) "</td>"))
                             row cell-width-list cell-color-list-1))
                "</tr>"))
             list-of-list))
    "</table>")))


;; A variant of table and table-1 which supports a header row
(define (table-2 border cell-width-list cell-color-list-1 header-list list-of-list )
  (let ((bdr (string-append "border = " (as-string border))))
    (string-append
     "<table " bdr ">"
     "<tr>"
     (apply string-append
            (map
	     (lambda (h)
               (string-append "<th>" h "</th>"))
             header-list))
     "</tr>"
     (apply string-append
	    (map
	     (lambda (row)
               (string-append
                "<tr>"
                (apply string-append
                       (map (lambda (cell width color-1)
                              (string-append "<td width = " (as-string width) " " "valign = top "
                                             "bgcolor = " (string-it (rgb-string-list color-1)) ">"
                                             (as-string cell) "</td>"))
                             row cell-width-list cell-color-list-1))
                "</tr>"))
             list-of-list))
    "</table>")))


;; A variant of table-1, but without a column color list.
;; The cell color becomes identical with the background.

(define (table-3 border cell-width-list  list-of-list . valign)
  (let ((bdr (string-append "border = " (as-string border)))
        (va (if (null? valign) "top" (as-string (car valign)))))
    (string-append
     "<table " bdr ">"
     (apply string-append
	    (map
	     (lambda (row)
               (string-append
                "<tr>"
                (apply string-append
                       (map (lambda (cell width)
                              (string-append "<td width = " (as-string width) " " "valign = " va " "
                                             ">"
                                             (as-string cell) "</td>"))
                             row cell-width-list))
                "</tr>"))
             list-of-list))
    "</table>")))

;; A variant of table-1 with a row color list instead of a column color list.
;; The length of row-color-list must be the same as the length of each row list in list-of-list.
(define (table-4 border cell-width-list row-color-list list-of-list . valign)
  (let ((va (if (null? valign) "top" (as-string (car valign)))))
   (html:table
     (apply string-append
	    (map
	     (lambda (row row-color)
              (html:tr
                (apply string-append
                       (map (lambda (cell width)
                              (html:td 
                                (as-string cell)
                                'width (as-string width) 'valign va 'bgcolor (rgb-string-list row-color)
                               )
                            )
			    row cell-width-list))
              ))
             list-of-list row-color-list
            )
      )
     'border (as-string border))))


; end more advanced table

; ---------------------------------------------------------------------------------------------------

;;; Form stuff.
;;; A number of functions which supports the work with (input) forms in Scheme.

;; Embed x in to form, which activates cgi-url upon form completion.
(define (form cgi-url x)
  (string-append  "<form method = "
                  (string-it "post") " "
                  "action = " (string-it cgi-url) ">"
  x
  "</form>"))

;; Embed x into a multipart form. Activate cgi-url when the form is submitted.
;; A multipart form is used for file uploading. Files are written into 
;; target-directory when uploaded.
;; The parameter target-directory-url gives the URL of the directory, in which the file is uploaded.
;; This is used for subsequent WWW retrival of the file.
(define (multipart-form cgi-url target-directory target-directory-url x)
  (string-append  "<form method = "
                  (string-it "post") " "
                  "enctype=" (string-it "multipart/form-data") " "
                  "action = " (string-it cgi-url) ">"
   (string-append
    (hidden-line "target-directory!!!" target-directory)
    (hidden-line "target-directory-url!!!" target-directory-url)
    x)
  "</form>"))

; OLD. New version defined 2.2.99
; (define (checkbox name)
;   (string-append
;       "<input type = " (string-it "checkbox") " "
;               "name = " (string-it (as-string name)) ">"))

;; Return an input tag of type checkbox. The name is a string or symbol which identifies the checkbox.
;; Checked is an optional boolean parameter. If checked is #t, the checkbox will be checked initially.
;; Returns the string true to the form processing application if checked.
(define (checkbox name . checked)
 (let ((checked1 (if (null? checked) #f (car checked))))
  (string-append
      "<input type = " (string-it "checkbox") " "
              (if checked1 "checked " "") " "
              "value = " (string-it "true") " "
              "name = " (string-it (as-string name)) ">")))

;; Return an input tag of type radio. 
;; checked is a boolean parameter, i.e. true or false (in Scheme sense). 
(define (radio-button value group-name . checked)
 (let ((is-checked (and (not (null? checked)) (boolean? (car checked)) (car checked))))
  (string-append
      "<input type = " (string-it "radio") " "
              (if is-checked "checked " "") " "
              "name = " (string-it (as-string group-name)) " "
	      "value = " (string-it (as-string value)) ">")))

;; Return an input tag of type text. 
;; The name is a string of symbol which identifies the text line.
;; Size is the the text line width in character positions.
;; Value is the initial value on the text line.
(define (text-line name size value)
  (string-append
      "<input type = " (string-it "text") " "
              "name = " (string-it (as-string name)) " "
	      "size = " (string-it (as-string size)) " "
	      "value = " (string-it (as-string value)) ">"))

;; Return an input tag of type hidden. 
;; The name is a string of symbol which identifies the hidden line.
;; Value is the string contents of the hidden line
(define (hidden-line name value)
  (string-append
      "<input type = " (string-it "hidden") " "
              "name = " (string-it (as-string name)) " "
	      "value = " (string-it (as-string value)) ">"))

;; Return an input tag of type file.
;; Such an input tag is used for file uploading.
;; The name of the uploading is name.
(define (file-upload name . optional-parameters)  
 (let ((value (optional-parameter 1 optional-parameters #f)))
  (if value 
      (string-append
        "<input type = " (string-it "file") " "
                "name = " (string-it (as-string name)) " "
                "value = " (string-it (as-string value)) 
        ">")
      (string-append
        "<input type = " (string-it "file") " "
                "name = " (string-it (as-string name)) " "
        ">"))))

;; Return an input tag of type password. 
;; The name is a string of symbol which identifies the password.
;; Size is the the line width in character positions.
;; Value is the initial contents of the password field (not very useful...).
(define (password-line name size value)
  (string-append
      "<input type = " (string-it "password") " "
              "name = " (string-it (as-string name)) " "
	      "size = " (string-it (as-string size)) " "
	      "value = " (string-it (as-string value)) ">"))

;; Return an input tag of type submit. Renders a button. 
;; Value is the string label of the button. If the optional parameter name
;; is given it identifies a particular submit button with a name, value pair in the submitted data.
;; .form (submit value [name])
(define (submit value . optional-parameters)
 (let ((name (optional-parameter 1 optional-parameters #f)))
  (if name
      (string-append
          "<input type = " (string-it "submit") " "
  	      "name = " (string-it (as-string name)) " "
	      "value = " (string-it (as-string value)) ">")
      (string-append
          "<input type = " (string-it "submit") " "
	      "value = " (string-it (as-string value)) ">"))))

;; Return an input tag of type reset. 
;; Value is the string label of the button.
(define (reset value)
  (string-append
      "<input type = " (string-it "reset") " "
	      "value = " (string-it (as-string value)) ">"))

; Return a select tag. 
; (define (select name value-list contents-list)
;   (string-append "<select name = " (string-it (as-string name)) "> "
;     (apply string-append
;       (map (lambda (value contents)
; 	     (string-append "<option value = " (string-it (as-string value)) ">"
; 			    (as-string contents) "</option>"))
; 	   value-list contents-list))
;       "</select>"))


;; Return a select tag, defining a multiple choice menu. Name is a string or symbol which identifies the selection.
;; Value-list is a list of the values to be returned upon selection.
;; Contents-list is the list of contents to be shown in the menu.
;; Selected-value is an optional value, which is to be selected initially. This value should be a member of value-list.
(define (select name value-list contents-list . selected-value)
 (let ((selected (if (null? selected-value) "" (car selected-value))))
  (string-append "<select name = " (string-it (as-string name)) "> "
    (apply string-append
      (map (lambda (value contents)
	     (string-append "<option value = " (string-it (as-string value)) 
                            (if (equal? selected value) " selected " "")
                            ">"
			    (as-string contents) 
                            
                            "</option>"))
	   value-list contents-list))
      "</select>")))

;; Return a textarea form. 
;; Rows is the number of rows of the text area.
;; Cols is the number of columns measured in characters.
;; Contents is the initial contents of the text area.
(define (textarea name rows cols contents)
  (string-append
      "<textarea "  
              "name = " (string-it (as-string name)) " "
	      "rows = " (string-it (as-string rows)) " "
              "cols = " (string-it (as-string cols)) ">"
              (as-string contents) "</textarea>"))

(define (formtest)
  (page "t"
     (form "URL"
       (con (checkbox "n") (p)
	    (radio-button "v" "g") (p)
	    (text-line 'n 50 'v) (p)
	    (submit 'send-det) (p)
	    (reset 'nulstil) (p)
	    (select 'n '(a b c d e) '(1 2 3 4 5))(p)
	    (textarea 'n 30 40 "noget indhold") (p)))))

; end form stuff



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
;; n is the number of columens
;; Internally, a HTML table with zero border is formed and returned.
(define (n-column-list n elements total-width)
 (let* ((lgt (length elements))
        (rows (sublist-by-columns n elements " "))
        (column-width (quotient total-width n))
        (column-widths (make-list n column-width)))
    (table-3 0 column-widths rows)))



; ---------------------------------------------------------------------------------------------------

;;; Uncategorized functions.
;;; The rest of the functions are not yet categorized.


;; The result of this function is inserted as an HTML comment on each html page generated.
;; Redefine this function for your own tracing.
;; As of here, it returns the empty string.
(define (tracing-comment) "") ; meant to be redefined in another place

(define (page title body . color-list)
 ;; Return a simple HTML page with html, head and title tags.
 ;; Also include the value of the parameter-less function (laml-standard-comment) as an initial html-comment.
 ;; The optional color-list must consist of a background color, text color, link color and visited link color.
 (let ((bd (if (null? color-list)
               "<body>"
               (apply body-tag color-list))))
  (string-append
    (copyright-clause) (newline-string)
    "<html><head><title> " title "</title></head>"
    bd body 
    "</body> </html>" 
    (newline-string)
    (laml-standard-comment) (newline-string) (tracing-comment) (newline-string)
)))



;; the part of a HTML page before the body. A html-v1 version of this function should be provided for.
(define (pre-page title . color-list)
 (let ((bd (if (null? color-list)
               "<body>"
               (apply body-tag color-list))))
  (string-append
    (laml-standard-comment) (newline-string) (tracing-comment) (newline-string)
    "<html><head><title>" title "</title></head>" bd )))

;; the part of a HTML page after the body. A html-v1 version of this function should be provided for.
(define (post-page)
  (string-append "</body></html>"))

(define (body-tag bg-color text-color link-color vlink-color)
  (string-append "<body " 
      "bgcolor = " quote-string (apply rgb-string bg-color) quote-string " "
      "text = " quote-string (apply rgb-string text-color) quote-string " "
      "link = " quote-string (apply rgb-string link-color) quote-string " "
      "vlink = " quote-string (apply rgb-string vlink-color) quote-string " "
      ">"))

; obsolete
(define (report-as-list alist-input)
  (string-append
   "<ul>"
   (list-report alist-input)
   "</ul>"))

; obsolete
(define (list-report alist)
 (if (null? alist)
     ""
     (let ((key (car (car alist)))
           (value (cdr (car alist))))
       (string-append
          "<li>" "<b>" key "</b>" "<br>" value
          (list-report (cdr alist))))))

; obsolete
(define (report-as-table alist-input)
  (string-append
   "<table border = 1>"
   (table-report alist-input)
   "</table>"))

; obsolete
(define (table-report alist)
 (if (null? alist)
     ""
     (let ((key (car (car alist)))
           (value (cdr (car alist))))
       (string-append
          "<tr>" "<td>" key "</td>" "<td>" value "</td>" "</tr>"
          (table-report (cdr alist))))))




;; Return an a tag constructructed from the URL and the anchor. 
;; If no anchor is provided uses the url as anchor text.
(define (a-tag url . anchor)
  (string-append 
    "<a href = " quote-string (as-string url)  quote-string ">" (if (null? anchor) url (as-string (car anchor))) "</a>"))

;; A variant of a tag which supports a target attribute of the a-tag (where in browser to show the result).
(define (a-tag-target url anchor target)
  (string-append 
    "<a href = " quote-string (as-string url)  quote-string " target = " target ">" (as-string anchor) "</a>"))

;; Name the current place by means of an a tag with name attribute
(define (a-name name)
  (string-append
    "<a name=" (string-it (as-string name)) ">"))

;; Return a mail link by means of the mailto facility in an a tag.
(define (mail-link email-adr . anchor-name)
  (if (null? anchor-name)
      (a-tag (string-append "mailto:" email-adr) email-adr)
      (a-tag (string-append "mailto:" email-adr) (as-string (car anchor-name)))))

;; Return an ol (ordered list) tag. The elements in list becomes the elements. Li tags are inserted automatically by this function.
(define (ordered-list lst)
  (string-append
    "<ol>"
     (apply string-append
            (map (lambda(el) (string-append "<li>" (as-string el))) lst))
    "</ol>"))

;; A convenient alias for ordered-list
(define ol ordered-list)

;; Return an ul (uordered list) tag. The elements in list becomes the elements. Li tags are inserted automatically by this function.
(define (unordered-list lst)
  (string-append
    "<ul>"
     (apply string-append
            (map (lambda(el) (string-append "<li type = disc>" (as-string el) "</li>")) lst))
    "</ul>"))


;; Return a flat list, separate by breaks
(define (br-list lst)
  (apply string-append
         (map (lambda(el) (string-append  (as-string el) (br))) lst)))

;; A convenient alias for br-list
(define brl br-list)

;; A convenient alias for unordered-list
(define ul unordered-list)

(define (definition-list lst)
  ;; Make a definition list.
  ;; lst is a list of lists. Each inner list must be of length two,
  ;; dt and dd respectively. I.e. definition terms and the defintion proper, resp.
  ;; As a special case supported, the inner list can be of lenght one, in which case
  ;; the dd is considered empty

 (string-append
    "<dl>"
    (apply string-append
       (map (lambda(el) 
               (let ((dt (car el))
                     (dd (if (= 1 (length el)) "" (cadr el))))
                 (string-append "<dt>" (as-string dt) " </dt>"
                                (if (equal? dd "")
                                    ""
                                    (string-append "<dd>" (as-string dd) " </dd>")))))
            lst))
    "</dl>"))

;; A convenient alias for definition-list
(define dl definition-list)

; -----------------------------------------------------------------------------
; Bullet list

(define (bullet-list lst  bullet-size bullet-color)
  ;; Render lst as a bulleted list. Both parameters must be symbols.
  ;; Bullet-size is either 'small or 'large.
  ;; Bullet-color is either 'red, 'green, 'yellow, 'blue, or 'cyan.
  ;; Depends on the presence of appropriate gif files in images directory.
  (apply string-append
         (map (lambda(el) (table-3 0 '(18 "*") 
                            (list (list (bullet-image bullet-size bullet-color) (con (as-string el) (p)))) "baseline"))
              lst)))

(define (bl lst)
  ;; a large, red bulleted list
  (bullet-list lst 'large 'red))

(define (bullet-image size color)
  (let ((path (image-file-path))
        (filename (bullet-filename size color)))
    (img (string-append path filename))))

(define (bullet-filename size color)
  ; size and color are symbols, small or large, and red, green or yellow, respectivel
  (string-append (as-string color) "-" "ball" "-" (as-string size) "." "gif"))

; -----------------------------------------------------------------------------


;; Show tree as an indented, bulleted list.
;; A tree is a list.
;; The first element of the list is the root.
;; The tail of the list is the list of subtrees.
;; A subtree, which is a leaf, can be given as a symbol, string or number.
(define (ul-tree tree)
  ; Example   (a (b c d) (e (f g) h))
  ;
  ;               a
  ;             /   \
  ;            b     e
  ;           / \   / \
  ;          c  d  f   h
  ;                |
  ;                g
  
  (cond ((or (symbol? tree) (string? tree) (number? tree)) (ul (list (as-string tree))))
        ((pair? tree) 
           (string-append 
             "<ul>" "<li type = disc>" (as-string (car tree))
                (apply string-append (map ul-tree (cdr tree)))
             "</ul>"))))


;; Present x in a kbd tag
(define (kbd x)
  (string-append "<kbd>" (as-string x) "</kbd>"))

;; Return a horizontal rule, a hr tag.
(define (hr . size)
  (if (null? size)
      "<hr>"
      (string-append "<hr size = " (as-string (car size)) ">")))

;; Return an img tag, in which a file on file-name is presented. An optional width parameter is supported.
(define (img file-name . width)
 (if (not (null? width))
  (string-append "<img src = " quote-string (as-string file-name) quote-string " width = " (as-string (car width)) " "
                 " border = 0 "
                  ">")
  (string-append "<img src = " quote-string (as-string file-name)
                 quote-string " border = 0 "  ">")))

;; A variant of img which presents an image with a border
(define (img-with-border file-name . width)
 (if (not (null? width))
  (string-append "<img src = " quote-string (as-string file-name) quote-string " width = " (as-string (car width)) " "
                  ">")
  (string-append "<img src = " quote-string (as-string file-name)
                 quote-string  ">")))

;; Returns h tags, h1 if i=1, h2 if i=2, etc.
(define (h i x)
  (let* ((start-tag (string-append "<h" (as-string i) ">"))
         (end-tag (string-append "</h" (as-string i) ">")))
    (string-append start-tag (as-string x) end-tag)))

;; Present x in a i tag
(define (i x)
  (string-append "<i>" (as-string x) "</i>"))

;; Present x in a strong tag
(define (strong x)
  (string-append "<strong>" (as-string x) "</strong>"))

;; Present x in a pre tag
(define (pre x)
  (string-append "<pre>" (as-string x) "</pre>"))

;; Present x in a center tag
(define (center x)
  (string-append "<center>" (as-string x) "</center>"))




(define (convert-size size)
  (if (and (symbol? size) (eq? size 'normal)) "3" (as-string size)))

(define (font size color x)
;; Returns a font tag.
;; size is a number and color is a rgb list of three, decimal integers or
;; a color symbol.
 (let ((ss (string-append "size = " (convert-size size)))
       (cs (string-append "color = "
                   quote-string (apply rgb-string color) quote-string)))
  (string-append "<font " ss " " cs ">" (as-string x) "</font>")))

(define (font-size size x)
;; Like font, but only supports size.
;; size is a number and color is a rgb list of three, decimal integers or
;; a color symbol.
 (let ((ss (string-append "size = " (convert-size size))))
  (string-append "<font " ss  ">" (as-string x) "</font>")))

(define (font-color color x)
;; Like font, but only supports color.
;; color is a rgb list of three, decimal integers
 (let ((cs (string-append "color = "
                   quote-string (apply rgb-string color) quote-string
             "")))
  (string-append "<font " cs ">" (as-string x) "</font>")))

;; Present x in a b tag (bold)
(define (b x)
  (string-append "<b>" (as-string x) "</b>"))

;; Present x in a u tag (underline)
(define (u x)
  (string-append "<u>" (as-string x) "</u>"))

;; Present x in a em tag (emphasize)
(define (em x)
  (string-append "<em>" (as-string x) "</em>"))

;; Present x in a blockquote tag
(define (cite x)
  (string-append "<blockquote>" (as-string x) "</blockquote>"))

;; Present x in a blockquote tag
(define blockquote cite)


;; Present a p tag (paragraph). If no parameter is given, just return a p tag without content.
;; If a parameter is given, return x embedded in a p tag.
(define (p . x)
  (if (null? x) "<p>" (string-append "<p>" (as-string (car x)) "</p>")))

;; Return a br tag (break)
(define (br)
   "<br>")

;; Return n space special characters (horizontal space)
(define (space n)
  (apply string-append (make-list n "&nbsp;")))

;; Return n space special characters 
(define horizontal-space space)

;; Return n vertical spaces, i.e., n p tags
(define (vertical-space n)
  (if (= n 0) ""
      (con (space 1) (p) (vertical-space (- n 1)))))


        
;; Return an HTML comment form. I.e, embed comment into the HTML comment characters
(define (html-comment comment)
  (string-append "<!-- " comment "-->"))


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
   (html:a
     (cond ((eq? text-or-image 'text) "LAML home")
           ((or (eq? text-or-image 'image) (string? text-or-image))
                    (html:img 'src (string-append url-of-laml image-file) 'alt help-text 'border 0))
           (else "LAML home"))
     'href (string-append url-of-laml "index.html")
     'title help-text
     'target "_top")))

;; Return a banner with left, middle, and right justified contributions.
(define (left-middle-right-banner left middle right)
  (html:table
   (html:tr
    (con
     (html:td
      (font-size 2 left)
      'width "33%" 'align "left" 'valign "top")

     (html:td
      (font-size 2 middle)
      'width "34%" 'align "center" 'valign "top")

     (html:td
      (font-size 2 right)
      'width "33%" 'align "right" 'valign "top")
    )
   )
   'border "0" 'cellpadding "0" 'cellspacing "0" 'width "100%")
)

;; Return a banner with left and right justified contributions.
(define (left-right-banner left right)
  (html:table
   (html:tr
    (con
     (html:td
      (font-size 2 left)
      'width "50%" 'align "left" 'valign "top")

     (html:td
      (font-size 2 right)
      'width "50%" 'align "right" 'valign "top")
    )
   )
   'border "0" 'cellpadding "0" 'cellspacing "0" 'width "100%")
)

;; Return the standard LAML top banner with time of generation, copyright, and home icon
(define (laml-top-banner)
  (left-middle-right-banner
     (when-generated)
     (string-append "Copyright " copyright " 2006, Kurt Nørmark")
     (laml-home-button 0 "laml-home.gif")))



; _____________________________________________________________________________

;;; Indenting and framing.
;;; Here is a number of functions of indentation and framing

;; Indent text with p pixels
(define (indent-pixels p text)
  (table-3 0 (list p "*") ; (make-list 2 slide-background-color)
    (list (list "" text))))

;; Show text in a column, narrowed from both left and right with p pixels
(define (narrow-with-pixels p text)
  (table-3 0 (list p "*" p) ; (make-list 3 slide-background-color)
    (list (list "" text ""))))

;; Shown text in a simple frame
(define (frame text) 
  (table-3 1 (list "*") ; (make-list 1 slide-background-color)
    (list (list text))))

;; Embed text in an invisible table with one cell
(define (box text) 
  (table-3 0 (list "*") 
    (list (list text))))

;; Present the contents-list, which is a list of elements, in a narrow
;; column of width, separated with activations of separator-fn.
(define (narrow separator-fn width . contents-list)
  (let ((separator-list (make-list (- (length contents-list) 1) (separator-fn))))
   (table-3 0 (list width) 
    (list 
      (list (string-merge contents-list separator-list))))))

;; Embed text into a color frame. It is the background which is colored.
(define (color-frame text color) 
  (table-1 0 (list "*") (make-list 1 color)
    (list (list text)) "bottom"))

;; As color-frame, but this function supports and extra widht parameter. This is an integer: the with of the frame in pixels.
(define (color-frame-width text color width) 
  (table-1 0 (list width) (make-list 1 color)
    (list (list text)) "bottom"))

;; Like frame, but with an extra width parameter. This is an integer: the with of the frame in pixels.
(define (frame-width text width) 
  (table-3 1 (list width) ; (make-list 1 slide-background-color)
    (list (list text))))

;; Embed text into a centered frame
(define (center-frame indentation text)
  (center
   (narrow-with-pixels indentation
     (frame text))))

; _____________________________________________________________________________

(define (html-protect str)
  ;; Transliterate angle brackets in str to the particular html character entities.
  ;; With this useful function it is possible to show the html tags in a browser
 (transliterate
   (transliterate
     (transliterate 
         str #\& "&amp;" )
     #\> "&gt;")
    #\< "&lt;"))

; -----------------------------------------------------------------------------
; Colorizing substrings:

; About faces:
; We support the following face symbols: italic, bold, typewriter, underlined, plain

(define (face-start-tag face-symbol)
  (cond ((eq? face-symbol 'italic) "<i>")
        ((eq? face-symbol 'bold) "<b>")
        ((eq? face-symbol 'typerwriter) "<kbd>")
        ((eq? face-symbol 'underlined) "<u>")
        ((eq? face-symbol 'plain) "")
        (else (error "face start tag: Unknown face symbol"))
  )
)

(define (face-end-tag face-symbol)
  (cond ((eq? face-symbol 'italic) "</i>")
        ((eq? face-symbol 'bold) "</b>")
        ((eq? face-symbol 'typerwriter) "</kbd>")
        ((eq? face-symbol 'underlined) "</u>")
        ((eq? face-symbol 'plain) "")
        (else (error "face end tag: Unknown face symbol"))
  )
)

(define (colorize-substrings str region-color-list)
  ;; This is a more advanced function which make font changes to substrings of str.
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
  (set! last-coloring-length 0)
  (if (null? region-color-list)
      str
      (let* ((region-color (car region-color-list))
             (from-str (car region-color))
             (to-str (cadr region-color))
             (color (caddr region-color))
             (face (if (= 4 (length region-color)) (cadddr region-color) 'bold))
             (multiplicity (if (= 5 (length region-color)) (fifth region-color) 1))
            )
        (colorize-substrings
          (font-substring str 0 from-str to-str color face multiplicity)
          (cdr region-color-list)))))

(define last-coloring-length 0) ; holds the length of font text from last substitution
                                ; set by font-substring-by-index

; (define (font-substring str start-index from-delimiting-string to-delimiting-string color face multiplicity)
;   ; surround a substring, delimited by from-delimiting-string and to-delimiting-string, by a html font tag
;   ; with a color attribute.
;   ; starting looking for delimiting strings at from-index
;   (let* ((from-index (substring-index str start-index from-delimiting-string))
;          (to-index   (substring-index str 
;                                       (+ from-index (string-length from-delimiting-string))  ; addition 10.9.98
;                                       to-delimiting-string)))
;     (if (and from-index to-index)
;         (font-substring-by-index str from-index (+ to-index (string-length to-delimiting-string)) color face)
;         (error (string-append "Substring fonting/colorizing: Cannot find delimiting strings: " from-delimiting-string ", " to-delimiting-string)))))

; ---------------------------------------------------------------------------------------------------------------
; new:

(define (repeat-colorizing str start-index from-str to-str color face n)
  (if (> n 0)
      (font-substring str start-index from-str to-str color face n)
      str))
             

(define (font-substring str start-index from-delimiting-string to-delimiting-string color face multiplicity)
  ; surround a substring, delimited by from-delimiting-string and to-delimiting-string, by a html font tag
  ; with a color attribute.
  ; starting looking for delimiting strings at from-index
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

; ---------------------------------------------------------------------------------------------------------------

(define (font-substring-by-index str from-index to-index color face)
  ; to-index is larger than from-index.
  ; insert a font tag around index range
 (let* ((cs (string-append "color = "
                   quote-string (apply rgb-string color) quote-string))
        (pre (string-append (face-start-tag face) "<font "  cs ">"))
        (post (string-append "</font>" (face-end-tag face)))
       )
   (set! last-coloring-length (+ (string-length pre) (string-length post)))
   (put-around-substring
      str from-index pre to-index post)))


; -----------------------------------------------------------------------------
;; Concatenete a number of strings. Con is an alias of string-append.
;; It is the intension to use it when concatenating HTML constituents.
;; .form (con . string-list)
;; .reference "similar function" "concatenate" "../html4.0-loose/man/surface.html#concatenate"
(define con string-append)

;; Like con, but ensure that there are white space in between the concatenated strings.
(define (con-space . string-list)
  (let ((space-list (make-list (- (length string-list) 1) " ")))
    (string-merge string-list space-list)))

;; Like con, but ensure that there are paragraphs marks in between the concatenated strings.
(define (con-par . string-list)
  ;; like con, but insert a p tag between all strings in string-list
  (let ((par-list (make-list (- (length string-list) 1) (p))))
    (string-merge string-list par-list)))

; Definition of underscore as space suppresssion. In this mirror the value is the empty string.
; This is used as a convenient notation for white space suppression in many documents.
; Serves the purpose of forward compatibility with newer mirrors.
(define _ "")

; -----------------------------------------------------------------------------
; Character entities:

(define (character-entity x)
  ;; if x is a number return a numbered character entity.
  ;; if x is a symbol og string, return a named character entity.

 (cond ((number? x) (string-append "&#" (three-digit-string x) ";"))  ; remember to ensure leading zeros
       ((or (string? x) (symbol? x)) (string-append "&" (as-string x) ";"))
       (else (error "character-entity: the parameter must be numeric, a symbol, or a string"))))


(define (three-digit-string n)
  ; Return a number string of exactly length three
  ; n is a number between 0 and 999
  (cond ((and (>= n 0) (< n 10)) (string-append "00" (as-string n)))
        ((and (>= n 10) (< n 100)) (string-append "0" (as-string n)))
        ((< n 1000) (as-string n))
        (else (error "three-digit-string: parameter must be between 0 and 999"))))

(define copyright (character-entity "copy"))

;; Embed x into a copyright indication
(define (copyright-owner x) (string-append x " " copyright))

; -----------------------------------------------------------------------------

;; Return the standard LAML comment, to be inserted at the top of every LAML generated page.
(define  (laml-standard-comment)
 (let ((dt (date-time (current-time))))
  (html-comment 
    (string-append 
      "Generated from an LAML (Lisp Abstracted Markup Language) source file. " 
      laml-version ". "
      "LAML is designed and implemented by Kurt Nørmark, normark@cs.auc.dk. "
      "Time of generation: " (car dt) " " (cadr dt)
    ))))


; -----------------------------------------------------------------------------

; alphabetical index index (links from letter array...)

;; Support of generation of alphabetic indexes. 
(define (alphabetic-link-array)
 ;; Return an 'array' of letter links to #letter
 (apply string-append
  (map 
    (lambda (letter) (string-append (a-tag (string-append "#" letter) (capitalize-string letter))  (horizontal-space 1)))
    (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" 
                       "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "æ" "ø" "å"))))

(define (alphabetic-link-array-1 target-file-prefix alphabet . emphasis-letter)
  ;; Return an 'array' of letter links to (string-append target-file-prefix "-" letter ".html") for all letters in alphabet. 
  ;; This is a generalized version of alphabetic-link-array.
  ;; target-file-prefix is a prefix of the file names, in which the index files are located.
  ;; alphabet is a list of letters, for which to generate index links from the alphabet arrays. Some letters
  ;; may be missing from the alphabet compared with a complete alphabet.
  ;; emphasis-letter is an optional letter which we emphasize in the link array
  (let* ((em-let (if (not (null? emphasis-letter)) (as-string (car emphasis-letter)) #f))
         (alphabet-1 (map as-string alphabet)))
    (apply string-append
	   (map 
	    (lambda (letter) 
	      (string-append 
	       (a-tag (string-append target-file-prefix "-" letter ".html")
		      (if (and em-let (equal? em-let letter))
			  (font 4 red (b (capitalize-string-nd letter)))
			  (capitalize-string-nd letter))
		      (horizontal-space 1))
               " "
            ))
	    alphabet-1))))

; -----------------------------------------------------------------------------

(define (html-appender element)
  ;; generate a function which appends element 
  (lambda (existing-stuff)
    (string-append existing-stuff " " element)))


(define (font-rise str base-size)
  ;; Return a html fonted version of str. Initial letter is sized base-size+1.
  ;; The rest is size base-size
  (string-append
     (font-size (+ base-size 1) (substring  str 0 1))
     (font-size base-size (substring  str 1 (string-length str)))))



; -----------------------------------------------------------------------------
; IMAGE FILE ACCESS

; A variable which tells how to access images (icons) via the function image-file. 
; (define image-file-access 'local)

;; The URL where the author of this library keeps a number of images (icons).
(define kn-internet-image-path "http://www.cs.auc.dk/~normark/images/")

;; Determination of the actual file path to images in html files.
;; This function depends on the variable image-file-access, which MUST defined external to this library.
;; The value of image-file-access can be changed via procedure set-image-file-access!
;; One of the following symbols: local, parent, net, sub-directory, or fixed. Default is local.
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

(define (set-image-file-path! mode)
  ;; Set the image-file-access variable. 
  ;; mode is symbol, either local, parent, sub-directory, net, or fixed.
  ;; Relative to the hmtl directory, where the target files are written.
  (set! image-file-access mode))

(define (image-file file-name)
  ;; Return the full path/address f image file named file-name. 
  ;; Relies of the setting of the variable image-file-access via the procedure set-image-file-path!
  ;; File name must include file extension
  (string-append (image-file-path) file-name ))



;; Return a Javascript calling form, given function function-name and parameters.
;; Returns a string of the form: function-name(parameters).
;; This functions adds commas between the actual Javascript parameters.
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
                 "]"))


;; Write a HTML page consisting of header and body to file. 
;; File is without extension. This function adds the html extension
(define (write-html-page file header body)
  (write-text-file 
    (page header body)
    (string-append file ".html")))


;; Makes a horizontal menu menu in terms of a table with links.
;; The table is made on the basis of parameter mini-menu-list, which is a list of
;; menu entries. A menu entry is a list of anchor-text and URL pairs (lists of
;; two strings). Dark-color must be some dark color.
(define (mini-menu mini-menu-list dark-color)
 (letrec ((mini-menu-entry (lambda (e) 
                             (let ((text (car e))
                                   (url (cadr e)))
                              (con (html:a (font 2 white text) 'href url 'css:text-decoration "none")
                                   (horizontal-space 5)))))
          (lgt (length mini-menu-list)))
   (table-1
     1
     (make-list lgt 160)
     (make-list lgt dark-color)
     (list (map mini-menu-entry mini-menu-list)))))