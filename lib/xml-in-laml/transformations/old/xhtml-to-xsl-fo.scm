; This is an initial attempt to transform XHTML to XSL-FO via use of the transformation function
; transform-ast in the libary lib/xml-in-laml/xml-in-laml.scm
; The transformation is not yet complete, and it is not a high priority to finalize this work.
; I wrote the transformations as an initial test case for use of XSL-FO together with LAML.
; Some of the transformations have been inspirred from the IBM work at
; http://www-128.ibm.com/developerworks/library/x-xslfo2app/
;
; Kurt Nørmark.


(define document-font-size "10pt")
(define document-page-master "A4-portrait")  ; "A4-landscape", "A4-portrait"
(define document-default-table-cell-width "2.0cm")


(define xhtml-to-xsl-fo
  (list
    (list (ast-of-type? 'element-name "html")
          (lambda (ast) 
	    (fo:root (int-fo-attr ast) 'xmlns:fo "http://www.w3.org/1999/XSL/Format"
		     (fo:layout-master-set
		      (fo:simple-page-master 'master-name "A4-portrait" 
					     'page-height "29.7cm" 'page-width "21cm"
					     (fo:region-body 'region-name "xsl-region-body"
;							     'margin "0.7in" 'padding "6pt"
                                             )
					     (fo:region-before 'region-name "xsl-region-before"
							       'extent "0.7in")
					     (fo:region-after 'region-name "xsl-region-after"
							      'extent "0.7in"))
		      (fo:simple-page-master 'master-name "A4-landscape" 
					     'page-width "29.7cm" 'page-height "21cm"
					     (fo:region-body 'region-name "xsl-region-body"
;							     'margin "0.7in" 'padding "6pt"
                                             )
					     (fo:region-before 'region-name "xsl-region-before"
							       'extent "0.7in")
					     (fo:region-after 'region-name "xsl-region-after"
							      'extent "0.7in"))
		      )
		     (fo:page-sequence 'master-reference document-page-master 
				       (apply-transformation-on (ast-subtree ast "body"))))))

   (list (ast-of-type? 'element-name "body")
         (lambda (ast)
	   (fo:flow (int-fo-attr ast) 'font-size document-font-size 'font-family "Times" 'flow-name "xsl-region-body"
		    (apply-transformation-on (ast-subtrees ast)))))


   (list (ast-of-type? 'element-name "table")           ; Take cell width from first row, if possible. Dimension of width (px/cm/mm) is necessary
         (lambda (ast)
	   (let* ((border (ast-attribute ast 'border "1px"))   ; Dimension of border necessary
		  (tbody (match-ast ast (location-step 'child "tbody")))
		  (ast1 (if (null? tbody) ast (first tbody)))
		  (rows (match-ast ast1 (location-step 'child "tr")))
		  (first-row (if (not (null? rows)) (first rows) '())) ; expected: a tr fragment
		  (number-of-cells-in-row (length (match-ast first-row (location-step 'child "td"))))
		  (width-in-td? (not (null? (match-ast first-row (location-step 'child "td") (location-step 'attribute "width")))))
		  (width-list (if width-in-td?
				  (match-ast first-row (location-step 'child "td") (location-step 'attribute "width") second)
				  (make-list number-of-cells-in-row document-default-table-cell-width)))  
		  )
	     (set! current-table-border-width border) ; bad solution
	     (fo:table (int-fo-attr ast)
		       'space-after "0.3cm"
		       'border-collapse "collapse" 
		       'table-layout "fixed"
		       (map (lambda (width) (fo:table-column 'column-width width)) width-list) 
		       (fo:table-body 
			(apply-transformation-on rows))))))

  (list (ast-of-type? 'element-name "tr")
        (lambda (ast)
	  (fo:table-row 'keep-together "always" (int-fo-attr ast) (apply-transformation-on (ast-subtrees ast)))))

  (list (ast-of-type? 'element-name "td")
        (lambda (ast)
	  (let ((colspan (ast-attribute ast 'colspan "1"))
		(rowspan (ast-attribute ast 'rowspan "1")))
	    (fo:table-cell (int-fo-attr ast)
			   'padding-start "3pt" 'padding-end "3pt" 'padding-before "3pt" 'padding-after "3pt"
					;           'border "solid 1px black"
			   'border-style "solid" ; not affecting result
			   'border-width current-table-border-width ; gobal variable
			   'border-color "black" ; OK
			   'number-columns-spanned colspan 'number-rows-spanned rowspan
					; align and valign not yet handled
			   (fo:block (apply-transformation-on (ast-subtrees ast)))))))


  (list (ast-of-type? 'element-name "img")  ; assumes that units is included in width or height
        (lambda (ast)
	  (let ((src (ast-attribute ast 'src))
		(height (ast-attribute ast 'height #f))
		(width (ast-attribute ast 'width #f)))
	    (fo:external-graphic (int-fo-attr ast) 'src src 
				 (if height (list 'height height) '()) 
				 (if width (list 'width width) '())
				 'scaling "uniform"
				 ))))

   (list (ast-of-type? 'element-name "p")
         (lambda (ast)
	   (fo:block (int-fo-attr ast) 'space-after "0.3cm" (apply-transformation-on (ast-subtrees ast)))))


   (list (ast-of-type? 'element-name "pre")
         (lambda (ast)
	   (fo:block (int-fo-attr ast) 'font-family "monospace" 'white-space-collapse "false" 'wrap-option "no-wrap" 
		     'text-align "start" ; does not work
		     (apply-transformation-on (ast-subtrees ast)))))

   (list (ast-of-type? 'element-name "em")
         (lambda (ast)
	   (fo:inline (int-fo-attr ast) 'font-style "italic" (apply-transformation-on (ast-subtrees ast)))))

   (list (ast-of-type? 'element-name "b")
         (lambda (ast)
	   (fo:inline (int-fo-attr ast) 'font-weight "bold" (apply-transformation-on (ast-subtrees ast)))))

   (list (ast-of-type? 'element-name "kbd")
         (lambda (ast)
	   (fo:inline (int-fo-attr ast) 'font-family "Courier" (apply-transformation-on (ast-subtrees ast)))))

   (list (ast-of-type? 'element-name "font")  
         (lambda (ast)
	   (let* ((size (ast-attribute ast 'size "1"))
		  (size-1 (if (numeric-string? size)
			      (cond ((equal? size "1") "8pt")
				    ((equal? size "2") "10pt")
				    ((equal? size "3") "12pt")
				    ((equal? size "4") "14pt")
				    ((equal? size "5") "18pt")
				    ((equal? size "6") "24pt")
				    ((equal? size "7") "36pt")
				    (else (laml-error "font: Unknown size" size)))
			      size))
		  (color (ast-attribute ast 'color (rgb-color-encoding 0 0 0)))
		  (font-familiy (ast-attribute ast 'font-family #f)))
	     (fo:inline (int-fo-attr ast) 'font-size size-1 'color color (apply-transformation-on (ast-subtrees ast))))))


   (list (ast-of-type? 'element-name "a")   ; incomplete
         (lambda (ast)
	   (let ((href-val (ast-attribute ast 'href #f)))
	     (cond (href-val 
		    (fo:basic-link (int-fo-attr ast) 'color "blue" 'external-destination href-val (apply-transformation-on (ast-subtrees ast))))
		   (else "???")))))
              


   (list (ast-of-type? 'element-name "ol")
         (lambda (ast)
	   (let ((items (find-asts ast "li")))
	     (fo:list-block (int-fo-attr ast)
					;           'provisional-label-separation "3mm"  ; no effect
			    'provisional-distance-between-starts "5mm"
			    (map (lambda (li-ast n)
				   (fo:list-item 'space-after "0.3cm"
						 (fo:list-item-label 'end-indent "label-end()" (fo:block (as-string n)))
						 (fo:list-item-body 'start-indent "body-start()" (fo:block (apply-transformation-on (ast-subtrees li-ast))))))
				 items
				 (number-interval 1 (length items)))))))

   (list (ast-of-type? 'element-name "ul")
         (lambda (ast)
	   (fo:list-block (int-fo-attr ast)
					;           'provisional-label-separation "3mmm"  ; no effect
			  'provisional-distance-between-starts "3.5mm"
			  (apply-transformation-on (ast-subtrees ast)))))

   (list (ast-of-type? 'element-name "li")   ; non ol list items
         (lambda (ast)
	   (fo:list-item (int-fo-attr ast) 'space-after "0.3cm"
			 (fo:list-item-label 'end-indent "label-end()" (fo:block (char-ref "#x2022")))
			 (fo:list-item-body 'start-indent "body-start()" (fo:block (apply-transformation-on (ast-subtrees ast)))))))

   (list (ast-of-type? 'element-name "center")
         (lambda (ast)
	   (fo:block (int-fo-attr ast) 'text-align "center" (apply-transformation-on (ast-subtrees ast)))))
   

   (list (ast-of-type? 'element-name "h1")
         (lambda (ast)
	   (fo:block (int-fo-attr ast) 'font-size "200%" 'space-after "0.6cm" (ast-text ast))))

   (list (ast-of-type? 'element-name "h2")
         (lambda (ast)
	   (fo:block (int-fo-attr ast) 'font-size "175%" 'space-after "0.6cm" (ast-text ast))))

   (list (ast-of-type? 'element-name "h3")
         (lambda (ast)
	   (fo:block (int-fo-attr ast) 'font-size "150%" 'space-after "0.4cm" (ast-text ast))))

   (list (ast-of-type? 'element-name "h4")
         (lambda (ast)
	   (fo:block (int-fo-attr ast) 'font-size "120%" 'space-after "0.4cm" (ast-text ast))))

   (list (ast-of-type? 'element-name "h5")
         (lambda (ast)
	   (fo:block (int-fo-attr ast) 'font-size "110%" 'space-after "0.3cm" (ast-text ast))))

   (list (ast-of-type? 'element-name "h6")
         (lambda (ast)
	   (fo:block (int-fo-attr ast) 'font-size "100%" 'space-after "0.3ocm" (ast-text ast))))


   (list (ast-of-type? 'element-name "div")
         (lambda (ast)
	   (fo:block (int-fo-attr ast) (apply-transformation-on (ast-subtrees ast)))))

   (list (ast-of-type? 'element-name "span")
         (lambda (ast)
	   (fo:inline (int-fo-attr ast) (apply-transformation-on (ast-subtrees ast)))))

   (list (ast-of-type? 'element-name "br")
         (lambda (ast)
	   (fo:block (int-fo-attr ast))))


   (list (ast-of-type? 'element-name "sub")   ; seems NOT to work in apache FOP
         (lambda (ast)
	   (fo:inline 'baseline-shift "sub" 'font-size "75%" (apply-transformation-on (ast-subtrees ast)))))

   (list (ast-of-type? 'element-name "sup")  ; seems NOT to work in apache FOP
         (lambda (ast)
	   (fo:inline 'vertical-align "super" 'font-size "80%" (apply-transformation-on (ast-subtrees ast)))))


   (list (lambda (x) (cdata? x))       ; Pass text through of processed elements
         (lambda (source) source))

   (list (lambda (x) (white-space-suppress? x))           ; Preserve white space suppress - transformation applied on an AST with negative white spacing
         (lambda (source) explicit-space-suppress))

   (list (lambda (x) #t)               ; Ignore others rule
         (lambda (source) '()))

 )
)

; A short alias that accesses the fo: prefixed internal attributes of ast
(define (int-fo-attr ast)
 (selected-internal-attributes ast "fo:"))


(define current-table-border-width #f)