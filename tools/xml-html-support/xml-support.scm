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


;;;; This is a simple, non-validating XML parser for LAML together with XML pretty printing support.
;;;; As of the current version, the parser is not complete. Nevertheless, it is useful tool for parsing most
;;;; everyday XML documents to a Lisp data structure. <p> Given a well-formed XML document
;;;; this parser returns a Lisp tree structure that represents the parse tree of the XML document.
;;;; The parser handles start tags, end tags, and empty tags (in this parser called start-end tags).
;;;; Entities and their declarations are not handled at all.<p>
;;;; The top level functions are xml-parse and xml-parse-file. The xml-parser can be loaded as a library as well.<p>
;;;; There exists <a href="../doc/xml-parser.html" target="_top">elucidative documentation</a> of this parser.
;;;; See also <a href="html-support.html">the HTML parsing and pretty printing support</a>, which is
;;;; built on top of the XML tools, and the illustrative 
;;;; <a href="../../../examples/xml-parser-and-pretty-printer/index.html">examples </a> 
;;;; of the XML parser and pretty printer.<p>
;;;; This tool assumes that <kbd> laml.scm </kbd> and the general library are loaded. 
;;;; The tool loads the collect-skip and the file-read libraries.<p>
;;;; The typographical rebreaking and re-indenting of running text is still missing.<p>
;;;; The LAML interactive tool procedures <kbd> xml-pp </kbd> and <kbd> xml-parse </kbd>
;;;; in <kbd> laml.scm </kbd> are convenient top-level pretty printing and parse procedures respectively.<p>
;;;; Please notice that this is not a production quality parser and pretty printer! It is currently used for
;;;; internal purposes.<p>
;;;; From LAML version 20, the XML pretty printing in lib/xml-in-laml/xml-in-laml.scm replaces the XML pretty printing
;;;; in this library.
;;;; .title Reference Manual of the XML parser and pretty printer for LAML


(lib-load "collect-skip.scm")
(lib-load "file-read.scm")


;;; The format of the parse tree.
;;; A <em>parse tree</em> T  produced by this tool is of the form
;;; <pre>    (tree N ST1 ST2 ... STn) </pre>
;;; where STi, i=1..n are parse trees (recursively) and N is a node (see below). <p>
;;; A leaf node N may be of the form
;;; <pre>    (tree N) </pre>
;;; or just N if N is a string (corresponding to textual contents) or an empty tag (a tag without contents).<p>
;;; An <em>inner node</em> of a parse tree corresponds to a tag (an element) with contents. Such a node is represented
;;; by the following 'tag structure':
;;; <pre>    (tag kind tag-name . attr-info) </pre>
;;; tag is a symbol (for tagging). kind is either start or start-end (both symbols).
;;; tag-name is a string. Attr-info is the attribute on property list format.<p>
;;; A <em>terminal node</em> may be a start-end node, a comment node or just a contents string. End tags are not represented 
;;; in the parse tree.<p>
;;; Here is an example of a start-end node (empty node) with two properties:
;;; <pre>    (tag start-end "title" role "xxx" size "5") </pre>
;;; Comments are represented as comment nodes of the form
;;; <pre>    (comment comment-string) </pre> <p>
;;; Declaration nodes of the form
;;; <pre>    (declaration kind value) </pre>
;;; are also possible. They are for instance used for document type (???) information in HTML. Finally nodes of the form
;;; <pre>    (xml-declaration attribute-property-list) </pre>
;;; are supported.

;;; Constants.
;;; Constants that affect the working of the parser. 

;; A constant that controls if white space is uniformly preserved by the XML parser.
(define xml-parser-preserve-white-space #f)

;; A list of tag names for which white space is preserved. A list of strings. No angle brackets ('<' or '>')should be given in the names.
;; Must be assigned to the appropriate value before a parse function is called.
(define white-space-preserving-tags '())


; ---------------------------------------------------------------------------------------------------
; Parse specific error and message funtions.

(define (parse-error . x)
  (display-message (string-append "PARSE ERROR: " (apply string-append (map as-string x))))
  (parser-status)
  (error "STOPPING THE PARSER"))


(define (parse-message . x)
  (if xml-parse-verbose
      (display-message (string-append (apply string-append (map as-string x))))))

; ---------------------------------------------------------------------------------------------------
; Overall functions

(define recently-skipped-white-space "")

(define (skip-white-space)
  (skip-while is-white-space?))

(define (skip-white-space)
  (let ((skipped-white-space (collect-until (negate is-white-space?))))
    (set! recently-skipped-white-space (string-append recently-skipped-white-space skipped-white-space))))

; Reset the recently skipped white space collection (to the empty string).
(define (reset-white-space)
  (set! recently-skipped-white-space ""))

(define (already-skipped-white-space)
  recently-skipped-white-space)
  

; Skip white space and XML comments
(define (skip-white-space-and-comments)
  (skip-white-space)
  (ensure-look-ahead 4)
  (if (and (not end-of-file?) (match-look-ahead? "<!--"))
      (begin 
        (parse-message "Skipping comment")
        (read-a-string 4)
        (skip-until-string "-->" #t)
        (skip-white-space-and-comments))))


; ---------------------------------------------------------------------------------------------------
;;; Native low-level level parser functions.
;;; The top level parser functions in this section deliver specialized low level parse trees (in some ad hoc list structure).

;; Top level parse function which takes an XML file name as input, and delivers a parse tree on out-file-path.
;; file-path is a file path (relative or absolute) with or without an extension. The default extension is xml.
;; The parse tree is written on the file out-file-path.
(define (parse-xml-file in-file-path out-file-path)
 (let ((init-in-path (file-name-initial-path in-file-path))
       (in-file-name-prop (file-name-proper in-file-path))
       (in-ext (file-name-extension in-file-path))
      )
   (reset-xml-parser)
   (let* ((input-port (open-input-file (string-append init-in-path in-file-name-prop "." (if (empty-string? in-ext) "xml" in-ext)))))
      (set! ip input-port)
      (let ((parse-tree (parse-xml-ip))
            (target-file-name out-file-path))
       (set! resulting-parse-tree parse-tree)
       (if (file-exists? target-file-name) (delete-file target-file-name))
       (let ((op (open-output-file target-file-name)))
         (write parse-tree op)
         (close-output-port op)))

      (display-message (string-append "DONE. The parse tree is in " out-file-path))
      (display-message "Use (scheme-pp <file>) to pretty pring the parse tree.")
      (display-message "The result is also in the variable resulting-parse-tree for interactive use.")
      (close-input-port ip))))

;; This function parses a file and returns the parse tree.
;; file-path is a file path (relative or absolute) without any extension. 
;; .returns The parse tree in the original, low level parse tree format (a list structure)
(define (parse-xml file-path)
 (let ((init-path (file-name-initial-path file-path))
       (file-name-prop (file-name-proper file-path))
       (ext (file-name-extension file-path))
      )
   (reset-xml-parser)
   (let* ((input-port (open-input-file (string-append init-path file-name-prop "." (if (empty-string? ext) "xml" ext)))))
      (set! ip input-port)
      (let ((parse-tree (parse-xml-ip)))
        (close-input-port ip)
        parse-tree))))

;; This function parses a string with XML contents and returns the parse tree.
;; xml-string is a string with xml contents
;; .returns The parse tree in the original, low level parse tree format (a list structure)
(define (parse-xml-string xml-string)
   (reset-xml-parser)
   (set! ip xml-string)
   (let ((parse-tree (parse-xml-ip)))
     parse-tree))


(define (reset-xml-parser)
  (reset-look-ahead-buffer)
  (set! parse-stack '()))


; ---------------------------------------------------------------------------------------------------
;;; AST-level parser functions.
;;; The top level parser functions in this section deliver XML-in-LAML abstract syntax trees.
;;; These trees are much more useful than the low-level parse trees delivered by the functions in the previous section.

;; Top level parse function which takes an XML file name as input, and delivers an XML-in-LAML AST on out-file-path.
;; The AST is written on the file out-file-path.
;; .parameter in-file-path a file path (relative or absolute) with or without an extension. The default extension is xml.
;; .parameter out-file-path path the output file.
;; .parameter xml-language the name of the XML language in LAML, to which the resulting AST belongs. A symbol.
(define (parse-xml-file-to-ast in-file-path out-file-path xml-language)
 (let ((init-in-path (file-name-initial-path in-file-path))
       (in-file-name-prop (file-name-proper in-file-path))
       (in-ext (file-name-extension in-file-path))
      )
   (reset-xml-parser)
   (let* ((input-port (open-input-file (string-append init-in-path in-file-name-prop "." (if (empty-string? in-ext) "xml" in-ext)))))
      (set! ip input-port)
      (let ((parse-tree (parse-tree-to-ast (parse-xml-ip) xml-language))
            (target-file-name out-file-path))
       (set! resulting-parse-tree parse-tree)
       (if (file-exists? target-file-name) (delete-file target-file-name))
       (let ((op (open-output-file target-file-name)))
         (write parse-tree op)
         (close-output-port op)))

      (display-message (string-append "DONE. The XML-in-LAML AST is in " out-file-path))
      (display-message "The result is also in the variable resulting-parse-tree for interactive use.")
      (close-input-port ip))))

;; This function parses an XML file and returns the corresponding XML-in-LAML AST.
;; .parameter file-path a file path (relative or absolute) without any extension. 
;; .parameter xml-language the name of the XML language in LAML, to which the resulting AST belongs. A symbol.
;; .returns An XML-in-LAML AST.
(define (parse-xml-to-ast file-path xml-language)
 (let ((init-path (file-name-initial-path file-path))
       (file-name-prop (file-name-proper file-path))
       (ext (file-name-extension file-path))
      )
   (reset-xml-parser)
   (let* ((input-port (open-input-file (string-append init-path file-name-prop "." (if (empty-string? ext) "xml" ext)))))
      (set! ip input-port)
      (let ((parse-tree (parse-tree-to-ast (parse-xml-ip) xml-language)))
        (close-input-port ip)
        parse-tree))))

;; This function parses a string with XML contents and returns an XML-in-LAML AST.
;; .parameter xml-string a string with xml contents.
;; .parameter xml-language the name of the XML language in LAML, to which the resulting AST belongs. A symbol.
;; .returns An XML-in-LAML AST.
(define (parse-xml-string-to-ast xml-string xml-language)
   (reset-xml-parser)
   (set! ip xml-string)
   (let ((parse-tree (parse-tree-to-ast (parse-xml-ip) xml-language)))
     parse-tree))


; ---------------------------------------------------------------------------------------------------
; ::parse-stack::
; Parse state: the parse stack

(define xml-parse-verbose #f)

(define parse-stack '())

(define (parse-stack-push x)
  (set! parse-stack (cons x parse-stack)))

(define (parse-stack-pop)
  (if (not (parse-stack-empty?))
      (set! parse-stack (cdr parse-stack))
      (parse-error (string-append "Trying to pop an empty parse stack"))))

(define (parse-stack-top)
  (if (not (parse-stack-empty?))
      (car parse-stack)
      (parse-error (string-append "Trying to access the top of an empty parse stack"))))

(define (parse-stack-empty?)
  (null? parse-stack))

(define (parse-stack-but-top)
  (if (not (parse-stack-empty?))
      (cdr parse-stack)
      (parse-error (string-append "Trying to access the top of an empty parse stack"))))

(define (parse-stack-top-and-pop)
  (if (not (parse-stack-empty?))
      (let ((res (car parse-stack)))
        (set! parse-stack (cdr parse-stack))
        res
      )
      (parse-error (string-append "Trying to access the top of an empty parse stack"))))

(define (parse-stack-bottom)
  (if (not (parse-stack-empty?))
      (last parse-stack)
      (parse-error (string-append "Trying to access the bottom of an empty parse stack"))))


; ---------------------------------------------------------------------------------------------------
; Tag structure and comment structure functions:

(define (make-tag-structure kind tag-name attribute-property-list)
  (cons 'tag (cons kind (cons tag-name attribute-property-list))))

(define (kind-of-tag-structure tag-structure)
  (as-symbol (cadr tag-structure)))

(define (tag-of-tag-structure tag-structure)
  (as-string (caddr tag-structure)))

(define (attributes-of-tag-structure tag-structure)
  (cdddr tag-structure))

; Return the attribute value of attribute-key (a symbol) in attribute-list.
; If attribute does not exist, return #f.
(define (attribute-value attribute-key attribute-list)
 (let ((a-list (propertylist-to-alist attribute-list)))
   (defaulted-get attribute-key a-list #f)))
  
(define (make-comment-structure comment-string)
  (list 'comment comment-string))

(define (make-processing-instruction-structure pi-two-element-list)
  (cons 'processing-instruction pi-two-element-list))

(define (make-declaration-structure kind val)
  (list 'declaration kind val))

(define (make-xml-declaration-structure attribute-plist)
  (list 'xml-declaration attribute-plist))

(define (comment-of-comment-structure node)
  (cadr node))

(define (kind-of-declaration-structure node)
  (cadr node))

(define (value-of-declaration-structure node)
  (caddr node))

(define (attributes-of-xml-declaration-structure node)
  (cadr node))

; -------------------------------------------------------
; Tag structure and comment structure predicates.

(define (html-tree-node? x)
  (and (list? x) (>= (length x) 1) (eq? (car x) 'html-tree)))

(define (xml-tree-node? x)
  (and (list? x) (>= (length x) 1) (eq? (car x) 'xml-tree))) 

(define (start-node? x)
  (and (list? x) (>= (length x) 2) (eq? (car x) 'tag) (eq? (cadr x) 'start)))

(define (start-end-node? x)
  (and (list? x) (>= (length x) 2) (eq? (car x) 'tag) (eq? (cadr x) 'start-end)))

(define (comment-node? x)
  (and (list? x) (= (length x) 2) (eq? (car x) 'comment)))

(define (processing-instruction-node? x)
  (and (list? x) (>= (length x) 2) (eq? (car x) 'processing-instruction)))

(define (declaration-node? x)
  (and (list? x) (= (length x) 3) (eq? (car x) 'declaration)))

(define (xml-declaration-node? x)
  (and (list? x) (= (length x) 2) (eq? (car x) 'xml-declaration)))

; ---------------------------------------------------------------------------------------------------
; Parse tree functions:

;; A global varible holding the latest produced parse tree
(define resulting-parse-tree #f)

(define (make-parse-tree node subtree-list)
  (cons 'tree (cons node subtree-list)))

(define (root-of-parse-tree tree)
  (cadr tree))

(define (subtrees-of-parse-tree tree)
  (cddr tree))

(define subtrees-of-xml-html-parse-tree cdr)

(define (parse-tree? x)
  (and (pair? x) (eq? (car x) 'tree)))

(define textual-content-node? string?)

(define (terminal-node? tree)
  (or (string? tree) 
      (and (list? tree) (= 2 (length tree)) (string? (cadr tree)))
      (and (list? tree) (= 2 (length tree)) (start-end-node? (cadr tree)))
      (start-end-node? tree)
      (comment-node? tree)
      (declaration-node? tree)
      (xml-declaration-node? tree)))

(define (inner-node? tree)
  (not (terminal-node? tree)))

; Return the node of a tree, which may be a contents string, a tag structure or a comment structure.
(define (node-of-tree tree)
 (cond ((terminal-node? tree)
          (cond ((string? tree) tree)
                ((start-end-node? tree) tree)
                ((comment-node? tree) tree)
                (else (root-of-parse-tree tree))))
       ((inner-node? tree)
          (root-of-parse-tree tree))))

; Return the node contents or tag name of the node (a symbol). An ad hoc function.
; The node may have been extracted ny node-of-tree.
(define (node-info node)
 (cond ((text-contents-entry? node) 
           (if (string? node) node (car node)))
       ((tag-entry? node)
           (as-symbol (tag-of-tag-structure node)))
       ((comment-entry? node)
           (comment-of-comment-structure node))
       (else (error "node-info: Should not happen"))))

; Return the attributes of a node. If there is no attributes of the node, return the empty list.
(define (node-attribute-info node)
 (cond ((text-contents-entry? node) 
           '())
       ((tag-entry? node)
           (attributes-of-tag-structure node))
       (else (error "node-attribute-inf: Should not happen"))))
 
 

; ---------------------------------------------------------------------------------------------------
; Predicates on trees and tag structures:
; Most useful to make sense of stack entries. Can also be used to distinguish various kinds of subtrees
; of a tree from each other.

(define (tag-entry? x)
  (and (list? x) (>= (length x) 2) (eq? (car x) 'tag)))

(define (comment-entry? x)
  (and (list? x) (= (length x) 2) (eq? (car x) 'comment)))

(define (declaration-entry? x)
  (and (list? x) (= (length x) 3) (eq? (car x) 'declaration)))

(define (xml-declaration-entry? x)
  (and (list? x) (= (length x) 2) (eq? (car x) 'xml-declaration)))

(define (start-tag-entry? x)
  (and (tag-entry? x) (eq? (cadr x) 'start)))

(define (start-end-tag-entry? x)
  (and (tag-entry? x) (eq? (cadr x) 'start-end)))

(define (tree-entry? x)
  (and (list? x) (>= (length x) 2) (eq? (car x) 'tree)))

(define (text-contents-entry? x)
  (or (string? x) (and (list? x) (= 1 (length x)) (string? (car x)))))

(define (html-root? x)
  (and (list? x) (>= (length x) 1) (eq? (car x) 'html-tree)))

(define (xml-root? x)
  (and (list? x) (>= (length x) 1) (eq? (car x) 'xml-tree)))
  


; ---------------------------------------------------------------------------------------------------
; Substantial parse functions.


(define (parse-xml-ip)
  (if (not end-of-file?) (skip-white-space))
  (cond ((and end-of-file? (not (parse-stack-empty?)) (>= (length parse-stack) 1))
          (aggregate-final-parse-tree 'xml-tree))
        ((and end-of-file? (parse-stack-empty?))
          (make-final-parse-tree 'xml-tree '()))
        ((not end-of-file?)
          (let ((next-parse-tree (parse-xml-balanced-expression xml-parser-preserve-white-space)))
            (parse-stack-push next-parse-tree)
            (parse-xml-ip)))
        (else (error (string-append "parse-xml-ip: strange parse error.")))))


; Skip all XML document prefix stuff, including comments.
; As of this version, it cannot skip an inline DTD (causes parse error).
(define (skip-front-matters)
 (parse-message "Skipping front matter")
 (skip-white-space)
 (ensure-look-ahead 2)
 (cond ((match-look-ahead? "<?")
          (read-a-string 2)
          (skip-until-string "?>" #t)
          (skip-front-matters))
       ((match-look-ahead? "<!") ; @a
         (let ((comment (collect-balanced-until (lambda (ch) (eqv? ch #\<)) (lambda (ch) (eqv? ch #\>)))))
            'do-nothing)
         (skip-front-matters))))

(define (parse-xml-balanced-expression preserve-white-space)
  (parse-message "Parsing balanced expression.")
  (if (not end-of-file?) (skip-white-space))
  (let ((what (what-is-ahead))) ; @a - looks ahead - does not read
    (cond ((eq? what 'tag) 
            (let* ((tag (read-tag)) ; @b
                   (is-tag-white-space-preserving? 
                       (or (member (tag-of-tag-structure tag) white-space-preserving-tags)
                            preserve-white-space  ; inherit white space preservation from parent
                       ))
                   (kind (kind-of-tag-structure tag)))
              (reset-white-space)
              (cond ((eq? kind 'start) ; @c
                       (parse-stack-push tag)
                       (read-and-push-subtrees-until-end-tag (tag-of-tag-structure tag) is-tag-white-space-preserving?) ; consumes the end tag too
                       (build-tree-from-stack (tag-of-tag-structure tag))  ; return inner node
                    )
                    ((eq? kind 'start-end) 
                       tag  ; return terminal node
                    )
                    ((eq? kind 'end) (parse-error "end tag encountered without matching start tag: " (as-string tag)))
                    (else (parse-error "parse-xml-balanced-expression: unknown kind of tag"))
              )
             )
           )
           ((eq? what 'contents-string)
              (let ((contents-string (read-contents-string preserve-white-space)))
                 (reset-white-space)
                 contents-string))
           ((eq? what 'comment)
              (let ((comment-string (read-comment)))
                 (make-comment-structure comment-string)))
           ((eq? what 'processing-instruction)
              (let ((pi-structure (read-processing-instruction)))
                 (make-processing-instruction-structure pi-structure)))
           ((eq? what 'declaration)
              (let ((declaration-structure (read-declaration)))
                 (make-declaration-structure 
                        (kind-of-declaration-structure declaration-structure)
                        (value-of-declaration-structure declaration-structure))))
           ((eq? what 'xml-declaration)
               (let ((attributes (read-xml-declaration)))
                  (make-xml-declaration-structure attributes)))
           (else (parse-error "parse-xml-balanced-expression: Parse problem")))))

; redefined next
(define (read-comment)
  (parse-message "Reading comment")
  (skip-white-space)
  (ensure-look-ahead 4)
  (if (match-look-ahead? "<!--")
      (begin
        (read-a-string 4)
        (skip-white-space)
        (let ((res (collect-until-string "--")))
          (read-a-string 2)
          (skip-white-space)
          (ensure-look-ahead 1)
          (if (match-look-ahead? ">")
              (read-a-char)
              (parse-error "End of comment char '>' expected"))
          res))
      (parse-error "Beginning of comment '<!--' expected")))

(define (read-comment)   ; april 24, 2009. Quick error fix.
  (parse-message "Reading comment")
  (skip-white-space)
  (ensure-look-ahead 4)
  (if (match-look-ahead? "<!--")
      (begin
        (read-a-string 4)
        (skip-white-space)
        (let ((res (collect-until-string "-->")))
          res))
      (parse-error "Beginning of comment '<!--' expected")))

(define (read-processing-instruction)
  (parse-message "Reading processing instruction")
  (skip-white-space)
  (ensure-look-ahead 4)
  (if (match-look-ahead? "<?")
      (begin
        (read-a-string 2)
        (skip-white-space)
        (let ((pi-target (collect-until is-white-space?)))
          (skip-white-space)
          (let ((pi-string (collect-until-string "?>")))
            (read-a-string 2)
            (skip-white-space)
            (list pi-target pi-string))))
       (parse-error "Beginning of processing instruction <? expected")))
            


(define (read-declaration) ; such as doctype. A primitive version.
  (parse-message "Reading declaration")
  (skip-white-space)
  (ensure-look-ahead 2)
  (if (match-look-ahead? "<!")
      (begin
         (read-a-string 2)
         (skip-white-space)
         (let ((kind (collect-until is-white-space?)))
           (skip-white-space)
           (let ((val (collect-until-string ">")))
             (read-a-char) ; >
             (make-declaration-structure kind val))))
      (parse-error "Beginning of decaration '<!' expected")))

; Return a the attribute list (property list) of an xml-declaration-structure.
; This procedures reads over the entire <? ... ?>  structure
(define (read-xml-declaration) ; <? ... ?> 
  (parse-message "Reading xml declaration")
  (skip-white-space)
  (ensure-look-ahead 5)
  (if (match-look-ahead? "<?xml")
      (begin
         (read-a-string 5)
         (skip-white-space)
         (let ((attributes (read-tag-attributes)))
           (skip-white-space)
           (ensure-look-ahead 2)
           (if (match-look-ahead? "?>")
               (read-a-string 2)
               (parse-error "?> expected"))
           attributes))
      (parse-error "Beginning of declaration '<?xml' expected")))



(define (what-is-ahead) 
  (ensure-look-ahead 5)
  (cond ((match-look-ahead? "<!--")  'comment)
        ((match-look-ahead? "<!") 'declaration)  ; right concept?
        ((match-look-ahead? "<?xml") 'xml-declaration)   ; right concept?
        ((match-look-ahead? "<?") 'processing-instruction)  
        ((match-look-ahead? "<") 'tag)
        (else 'contents-string)))

(define (read-contents-string preserve-white-space)
  (let ((cont-string (collect-until (lambda (ch) (eqv? ch #\<)))))
    (if preserve-white-space            ; earlier: (white-space-preserving-context? parse-stack)
        (string-append (already-skipped-white-space) cont-string)
        (remove-redundant-white-space cont-string))))

; In a white space preserving context, such as HTML pre, strings are not rebroken.
; In XML we should probably chose always to preserve white space.
(define (white-space-preserving-context? parse-stack)
  #f)


; remove extra white space from str by returning a truncated string. Turn extra white space in spaces. A pure function.
(define (remove-redundant-white-space str)
  (remove-redundant-white-space-1 str "" 0 (string-length str) #f))

(define (remove-redundant-white-space-1 str res i lgt removing)
  (cond ((= i lgt) res)
        ((and removing (is-white-space? (string-ref str i))) 
             (remove-redundant-white-space-1 str res (+ i 1) lgt #t))
        ((and removing (not (is-white-space? (string-ref str i)))) 
             (remove-redundant-white-space-1 str (string-append res (as-string (string-ref str i)))  (+ i 1) lgt #f))
        ((and (not removing) (is-white-space? (string-ref str i)))
             (remove-redundant-white-space-1 str (string-append res (as-string #\space))  (+ i 1) lgt #t))
        ((and (not removing) (not (is-white-space? (string-ref str i))))
             (remove-redundant-white-space-1 str (string-append res (as-string (string-ref str i)))  (+ i 1) lgt #f))
        (else (error "remove-redundant-white-space-1: should not happen"))))

(define (read-and-push-subtrees-until-end-tag end-tag-name preserve-white-space)
  (skip-white-space)
  (let* ((n (+ (string-length end-tag-name) 3)) ; @a
         (end-tag-string (string-append (as-string #\<) (as-string #\/) end-tag-name (as-string #\>)))
        )
    (ensure-look-ahead n)
    (if (match-look-ahead? end-tag-string) ; @b
        (begin 
          (read-a-string n) ; finish
;          (skip-white-space)
        )
        (let ((subtree (parse-xml-balanced-expression preserve-white-space))) ; @c
          (reset-white-space)
          (parse-stack-push subtree)
          (skip-white-space)
          (read-and-push-subtrees-until-end-tag end-tag-name preserve-white-space) ; tail recursive parsing of contents
        ))))

(define (build-tree-from-stack end-tag-name)
  (build-tree-from-stack-1 end-tag-name '()))

(define (build-tree-from-stack-1 tag-name tree-list)
  (let ((top (parse-stack-top-and-pop)))
    (if (and (start-tag-entry? top) (matches-stack-entry top tag-name))
        (make-parse-tree top tree-list)
        (build-tree-from-stack-1 tag-name (cons top tree-list)))))

(define (matches-stack-entry top-tag-structure tag-name)
  (equal? (downcase-string (tag-of-tag-structure top-tag-structure)) (downcase-string  tag-name)))



; We know that we are just in front of a tag. Return a tag structure.
(define (read-tag)
  (parse-message "Reading tag")
  (skip-white-space)
  (ensure-look-ahead 1)
  (if (match-look-ahead? "<")
      (read-a-char)
      (parse-error "'<' expected"))
  (ensure-look-ahead 1)
  (if (match-look-ahead? "/") ; end tag
      (begin ; @a
       (read-a-char)
       (let ((tag-name (collect-until (lambda (ch) (or (eqv? ch #\>) (is-white-space? ch))))))
        (ensure-look-ahead 1)
        (if (match-look-ahead? ">")
            (read-a-char) ; >
            (parse-error "An end tag must not contain anything after the tag name"))
        (parse-message "  " "End: " tag-name)
        (make-tag-structure 'end tag-name '())))
      (let ((tag-name (collect-until (lambda (ch) (or (eqv? ch #\>) (eqv? ch #\/) (is-white-space? ch))))))
        (skip-white-space)
        (ensure-look-ahead 2) 
        ; @b
        (cond ((and (>= (string-length (max-look-ahead-prefix)) 2) (match-look-ahead? "/>"))  ; @c - weird
               (read-a-string 2)
               (make-tag-structure 'start-end tag-name '()))
              ((and (>= (string-length (max-look-ahead-prefix)) 1) (match-look-ahead? ">"))  ; @d
               (read-a-char)
               (make-tag-structure 'start tag-name '()))
              (else (let ((attribute-list (read-tag-attributes))) ; @e - white space processes at this point
                      (ensure-look-ahead 2)
                      (cond ((and (>= (string-length (max-look-ahead-prefix)) 2) (match-look-ahead? "/>"))
                             (read-a-string 2)
                             (parse-message "  " "Start end: " tag-name ". Attributes: " (as-string attribute-list))
                             (make-tag-structure 'start-end tag-name attribute-list))
                            ((and (>= (string-length (max-look-ahead-prefix)) 1) (match-look-ahead? ">"))
                             (read-a-char)
                             (parse-message "  " "Start: " tag-name ". Attributes: " (as-string attribute-list))
                             (make-tag-structure 'start tag-name attribute-list))
                            (else (parse-error "read-tag: end of tag expected"))))
              )))))

; Return attribute value pairs as a property list
; Expect that we are at the first character the first attribute name
(define (read-tag-attributes)
  (skip-white-space)
  (let ((attr-val (read-attribute-value-pair))) ; cons-pair or #f.
    (skip-white-space)
    (if attr-val
        (cons 
         (car attr-val) 
         (cons 
           (cdr attr-val) 
           (read-tag-attributes) ; recursive reading
         )
        )
        '())))

; (define (read-attribute-value-pair)
;   (ensure-look-ahead 2)
;   (if (or (match-look-ahead? "/>") (match-look-ahead? "?>") (match-look-ahead? ">"))
;       #f
;       (let* ((attr-name (collect-until (lambda (ch) (or (eqv? ch #\=)  (is-white-space? ch))))))
;         (skip-white-space)
;         (ensure-look-ahead 1)
;         (if (match-look-ahead? "=")
;             (begin
;               (read-a-char)
;               (skip-white-space))
;             (parse-error "= expected after attribute value"))
;         (ensure-look-ahead 1)
;         (if (match-look-ahead? "\"")
;             (begin
;               (read-a-char)             ; reading first "
;               (let ((value (collect-until (char-predicate #\"))))
;                 (read-a-char)           ; reading second "
;                 (skip-white-space)
;                 (cons (as-symbol attr-name) value)))
;             (parse-error "Attribute value in string quotes expected")))))

; read a single attribute value pair from the input port and return it is a cons pair.
; If no more attributes exists (we see end of tag characters) return #f.
(define (read-attribute-value-pair)
  (ensure-look-ahead 2)
  (if (or (match-look-ahead? "/>") (match-look-ahead? "?>") (match-look-ahead? ">"))
      #f
      (let* ((attr-name (collect-until (lambda (ch) (or (eqv? ch #\=) (eqv? ch #\>)  (is-white-space? ch))))))
        (skip-white-space)
        (ensure-look-ahead 1)
        (cond ((match-look-ahead? "=")
               (read-a-char)
               (skip-white-space)
               (ensure-look-ahead 1)
               (cond ((match-look-ahead? "\"")
                         (read-a-char)      ; reading first "
                         (let ((value (collect-until (char-predicate #\"))))
                           (read-a-char)    ; reading second "
                           (skip-white-space)
                           (cons (as-symbol attr-name) value)))
                     ((match-look-ahead? "'")   
                         (read-a-char)      ; reading first '
                         (let ((value (collect-until (char-predicate #\'))))
                           (read-a-char)    ; reading second '
                           (skip-white-space)
                           (cons (as-symbol attr-name) value)))
                     (else
                         (let ((value (collect-until (lambda (ch) (or (is-white-space? ch) (eqv? ch #\>))))))
                           (skip-white-space)
                           (cons (as-symbol attr-name) value)))))
             (else
               (cons (as-symbol attr-name) (as-string attr-name)) ; boolean attribute
             )))))


; aggregate remaining stack entries as subtrees of a kind node. kind is a symbol, such
; as html-tree or xml-tree
(define (aggregate-final-parse-tree kind)
  (make-final-parse-tree kind (reverse (shallow-list-copy parse-stack))))

(define (make-final-parse-tree kind subtrees)
  (cons kind subtrees))

; Assume as a pre-condition that lst is a proper list
(define (shallow-list-copy lst)
  (cond ((null? lst) '())
        (else (cons (car lst) (shallow-list-copy (cdr lst))))))


; ---------------------------------------------------------------------------------------------------
;;; Utility parser functions. 
;;; The functions in this section are all miscelaneous and utility functions of the parser.

;; Traverse the parse tree, tree, and return a list of result-transformed nodes 
;; that satisfy the node-interesting? predicate in the parse tree.
;; In other words, apply the node-interesting? predicate to all subtrees of the tree during the traversal, and return
;; the result-transformed list of subtrees.
;; Both the functions node-interesting? and result-transformer are applied on trees and subtrees.
;; .example  (traverse-and-collect-from-parse-tree resulting-parse-tree (is-tag-of-kind? 'a) parse-tree-to-laml-expression) 
(define (traverse-and-collect-from-parse-tree tree node-interesting? result-transformer)
  (cond ((or (html-tree-node? tree) (xml-tree-node? tree))
          (let ((subtree-results
                   (map 
                    (lambda (subtr) (traverse-and-collect-from-parse-tree subtr node-interesting? result-transformer))
                    (cdr tree))))
             (flatten subtree-results)))
        ((and (terminal-node? tree) (node-interesting? tree)) (list (result-transformer tree)))
        ((and (terminal-node? tree) (not (node-interesting? tree))) '())
        ((inner-node? tree)
           (let ((subtree-results
                   (map 
                    (lambda (subtr) (traverse-and-collect-from-parse-tree subtr node-interesting? result-transformer))
                    (subtrees-of-parse-tree tree))))
             (if (node-interesting? tree)
                 (cons 
                   (result-transformer tree)
                   (flatten subtree-results))
                 (flatten subtree-results))))))


;; Traverse the parse tree, tree, and return the list all attribute values of the attribute attr-key found
;; in the tree.
;; .example (collect-attributes-in-tree tree 'href) 
(define (collect-attributes-in-tree tree attr-key)
 (filter (lambda (x) (if x #t #f))
  (traverse-and-collect-from-parse-tree tree 
        (lambda (tr) (tag-entry? (node-of-tree tr)))
        (lambda (tr) (attribute-value attr-key (attributes-of-tag-structure (node-of-tree tr)))))))


;; Return a predicate which tests whether a subtree or node is of tag-kind (a symbol or string).
;; This function is a useful second parameter to traverse-and-collect-from-parse-tree.
;; .internal-references "related function" "traverse-and-collect-from-parse-tree"
(define (is-tag-of-kind? tag-kind)
  (lambda (x)
    (or
      (and (tag-entry? x) (equal? (downcase-string (as-string tag-kind)) (downcase-string (as-string (tag-of-tag-structure x)))))
      (and (tree-entry? x) (tag-entry? (node-of-tree x)) (equal? (downcase-string (as-string tag-kind)) (downcase-string (as-string (tag-of-tag-structure (node-of-tree x)))))))))


;; Display parser status in case of error in the parse process.
(define (parser-status)
  (display (stack-status)) (newline)
  (display (input-status))
)

(define (stack-status)
 (string-append "THE PARSE STACK: " (as-string #\newline) (stack-status-1 parse-stack)))


(define (stack-status-1 lst)
 (if (null? lst)
    ""
    (let ((top (car lst)))
     (string-append
      (cond ((text-contents-entry? top) (as-string (node-info top)))
            ((tag-entry? top) (as-string (node-info top)))
            ((tree-entry? top) (as-string (node-info (node-of-tree top))))
            (else "???"))
      (as-string #\newline)
      (stack-status-1 (cdr lst))))))

(define (input-status)
  (ensure-look-ahead 100)
  (string-append "PREFIX OF REMAINING INPUT: " (as-string #\newline) (as-string #\") 
                 (max-look-ahead-prefix) (as-string #\") (as-string #\newline)))

  

                 
; ------------------------------------------------------------------------------------------------------------------------
; XML pretty printing.
; In reality this is mostly generic XML and HTML pretty printing.

; ------------------------------------------------------------------
;;; Top level XML pretty printing functions.

;; Pretty prints the XML parse tree (Lisp file) in in-file-path.
;; Outputs the pretty printed result in out-file-path,
;; which defaults to in-file-path if not explicitly passed.
;; .form (pretty-print-xml-parse-tree-file in-file-path [out-file-path])
;; .misc For XML-in-LAML ASTs use pretty-render-to-output-port instead of this function
(define (pretty-print-xml-parse-tree-file in-file-path . optional-parameters)
 (let ((out-file-path (optional-parameter 1  optional-parameters in-file-path)))
  (let ((parse-tree (file-read in-file-path)))
   (write-text-file
    (pretty-print-xml-parse-tree parse-tree) 
    out-file-path))))

;; Pretty prints a HTML parse tree, and return the result as a string.
;; .misc For XML-in-LAML ASTs use pretty-xml-render instead of this function
(define (pretty-print-xml-parse-tree parse-tree)
 (set! res '())
 (pretty-print-xml-html-parse-tree-1 parse-tree 0 #f)
 (linearize-pp-result (reverse res)))

; ------------------------------------------------------------------
;;; Variables that control the pretty printing.
;;; These variables apply for both HTML and XML.

;; An integer which gives the level of indentation
(define indentation-delta 3)

;; A boolean which controls the application of single line pretty printing.
;; If true, the pretty printer will pretty print short list forms on a single line
(define use-single-lining #t)

;; An integer that expresses the preferred maximum column width
(define prefered-maximum-width 90)

; -------------------------------------------------------------------
; The internal result list

(define res '())

(define (add-to-res x)
  (set! res (cons x res)))

(define (add-list-to-res lst)
 (for-each
   (lambda (el) (add-to-res el))
   lst))


(define (remove-from-res i)
  (if (> i 0)
      (begin
        (set! res (cdr res))
        (remove-from-res (- i 1)))))

(define (linearize-pp-result lst)
 (apply string-append
  (map as-string lst)))



; In response to Per Madsen's request - but not a real solution.
; space after a tag belongs to the word  just after the tag.
;(define (add-white-space single-lining?)
;   (if (not single-lining?) 
;        (if (eq? laml-platform 'windows) ; pc end of line convention
;            (begin (add-to-res #\newline)) ; (add-to-res #\return)
;            (add-to-res #\newline))))

; Is the last element on res ended with white space?
; Start tag strings will always give #t
(define (trailing-whitespace-on-res?)
  (if (null? res)
      #f
      (let ((lst-el (car res)))
        (cond ((string? lst-el)
                 (let ((lgt (string-length lst-el)))
                   (or 
                      (eqv? #\space (string-ref lst-el (- lgt 1)))
                      (start-tag-string? lst-el)
                   )
                  ))
              ((char? lst-el)
                 (or (eqv? lst-el #\space) (eqv? lst-el #\newline)))
              (else #f)))))


; precond: x is a string
(define (start-tag-string? x)
  (and (> (string-length x) 1)
       (eqv? (string-ref x 0) #\<)
       (not (eqv? (string-ref x 1) #\/))))


(define trailing-whitespace-pending #f)

(define (add-white-space single-lining?)
 (if #t ; (trailing-whitespace-on-res?)
     (if (not single-lining?) 
         (add-to-res #\newline))
     (set! trailing-whitespace-pending #t)))
     

(define (add-indentation i)
  (if #t ; (trailing-whitespace-on-res?)
      (add-to-res (indentation i))))

; -------------------------------------------------------------------

(define (indentation n)
  (make-string n #\space))

(define (single-liner-form? x start-col max-width)
  (if use-single-lining
      (let ((width (meassure-html-xml-form x)))
        (<= (+ width start-col) max-width))
      #f))

; ------------------------------------------------------------------------------------------------------------
; The substantial and central pretty printing function. This function
; support both XML and HTML.

(define problem-form #f)

(define (pretty-print-xml-html-parse-tree-1 form start-col single-lining?)
  (cond ((html-root? form)
            (add-subtrees-to-res (cdr form) start-col single-lining?))
        ((xml-root? form)
            (add-subtrees-to-res (cdr form) start-col single-lining?))
        ((tree-entry? form)
           (if (single-liner-form? form start-col prefered-maximum-width)
               (begin
                 (pretty-print-start-tag (root-of-parse-tree form) start-col #t)
                 (add-white-space #t)
                 (add-subtrees-to-res (subtrees-of-parse-tree form) (+ start-col indentation-delta) #t)
                 (pretty-print-end-tag (root-of-parse-tree form) start-col #t))
               (begin 
                 (pretty-print-start-tag (root-of-parse-tree form) start-col single-lining?)
                 (add-white-space single-lining?)
                 (add-subtrees-to-res (subtrees-of-parse-tree form) (+ start-col indentation-delta) single-lining?)
                 (if (not single-lining?) (add-indentation start-col))
                 (pretty-print-end-tag (root-of-parse-tree form) start-col single-lining?))))
        ((text-contents-entry? form) 
            (add-list-to-res (break-long-string (node-info form) start-col (max 10 (- prefered-maximum-width start-col)))))
;        ((text-contents-entry? form) 
;            (add-to-res (node-info form)))
        ((comment-entry? form)
            (pretty-print-comment (comment-of-comment-structure form)))
        ((declaration-entry? form)
            (pretty-print-declaration (kind-of-declaration-structure form) (value-of-declaration-structure form)))
        ((xml-declaration-entry? form)
            (pretty-print-xml-declaration (attributes-of-xml-declaration-structure form)))
        ((start-end-tag-entry? form)
            (pretty-print-empty-tag form start-col single-lining?))
        (else (begin (set! problem-form form) (error (string-append "pretty-print-xml-html-parse-tree-1: Unknown structure encountered: " (as-string form)))))))


; Break string into a list of substrings with interleaved indentation strings
; Handles the breaking and pretty printing of long textual pieces.
(define (break-long-string str indentation max-length)
 (if (and (string? str) (not (empty-string? str)))
     (let* ((str1 (transliterate str #\newline (as-string #\space)))
            (indentation-string (string-append (as-string #\newline) (make-string indentation #\space)))
            (broken-strings (break-long-string-1 str1 (string-length str) 0 max-length '()))
            (lgt (length broken-strings))
            )
       (merge-lists-simple
        broken-strings (make-list (- lgt 1) indentation-string)))
     (list str)))


(define (break-long-string-1 str str-lgt i max-length res)
  (cond ((>= i str-lgt) (reverse (cons str res)))
        ((and (< i str-lgt) (member (string-ref str i) white-space-char-list))
           (if (> i max-length)
               (let ((first (substring str 0 i))
                     (rest (substring str (+ i 1) str-lgt)))
                  (break-long-string-1 rest (string-length rest) 1 max-length (cons first res)))
               (break-long-string-1 str str-lgt (+ i 1) max-length res)))
        (else (break-long-string-1 str str-lgt (+ i 1) max-length res))))


(define (add-subtrees-to-res subtree-list start-col single-lining?)
  (for-each
    (lambda (tree)
      (if (not single-lining?) (add-indentation start-col))
      (pretty-print-xml-html-parse-tree-1 tree start-col single-lining?)
      (add-white-space single-lining?))
   subtree-list))

(define (pretty-print-start-tag tag-structure start-col single-lining?)
  (let ((tag-name (tag-of-tag-structure tag-structure))
        (attributes (attributes-of-tag-structure tag-structure)))
    (if (null? attributes)
        (add-to-res (string-append (as-string #\<) (as-string tag-name) (as-string #\>)))
        (begin
          (add-to-res (string-append (as-string #\<) (as-string tag-name)))
          (add-to-res #\space)
          (add-attributes-to-res attributes (+ start-col 2 (string-length tag-name)) single-lining?)
          (remove-from-res 1) ; last space before greater than char
          (add-to-res #\>)))))

; attributes is a property list
(define (add-attributes-to-res attributes start-col single-lining?)
  (cond ((and (not (null? attributes)) (>= (length attributes) 2))
         (let ((key (car attributes))
               (val (cadr attributes))
               (res-attributes (cddr attributes)))
           (add-single-attribute-to-res key val start-col single-lining?)
           (add-attributes-to-res (cddr attributes) start-col single-lining?)))
        ((and (not (null? attributes)) (< (length attributes) 2))
           (error (string-append "add-attributes-to-res: malformed attribute list: " (as-string attributes))))))

(define (add-single-attribute-to-res key val start-col single-lining?)
  (add-to-res key) (add-to-res "=") 
  (add-to-res (string-it val)) (add-to-res #\space))

(define (pretty-print-empty-tag tag-structure start-col single-lining?)
  (let ((tag-name (tag-of-tag-structure tag-structure))
        (attributes (attributes-of-tag-structure tag-structure)))
    (if (null? attributes)
        (add-to-res (string-append (as-string #\<) (as-string tag-name) (as-string #\/) (as-string #\>)))
        (begin
          (add-to-res (string-append (as-string #\<) (as-string tag-name)))
          (add-to-res #\space)
          (add-attributes-to-res attributes (+ start-col 2 (string-length tag-name)) single-lining?)
          (remove-from-res 1) ; last space before greater than char
          (add-to-res (string-append  (as-string #\/) (as-string #\>) ))))))

(define (pretty-print-end-tag tag-structure start-col single-lining?)
  (let* ((tag-name (tag-of-tag-structure tag-structure))
         (end-tag (string-append (as-string #\<) (as-string #\/) (as-string tag-name) (as-string #\>))))
    (add-to-res end-tag)))

(define (pretty-print-comment comment-string)
  (add-to-res "<!--") (add-to-res #\space)
  (add-to-res comment-string)
  (add-to-res "-->"))

(define (pretty-print-declaration kind value)
  (add-to-res "<!")
  (add-to-res (as-string kind))
  (add-to-res #\space)
  (add-to-res value)
  (add-to-res ">"))

(define (pretty-print-xml-declaration attributes)
  (add-to-res "<?xml")
  (add-to-res #\space)
  (let ((dummy 0))
    (add-attributes-to-res attributes dummy #f))
  (add-to-res #\space)
  (add-to-res "?>"))





; ------------------------------------------------------------------
; Html and xml meassuring

(define (meassure-html-xml-form form)
  (cond ((html-root? form)
           (accumulate-right + 0 
            (map meassure-html-xml-form (cdr form))))
        ((tree-entry? form) 
            (+ (meassure-html-xml-form (root-of-parse-tree form)) ; both start only
               1
               (accumulate-right + 0 
                 (map meassure-html-xml-form (subtrees-of-parse-tree form)))
               (string-length (as-string (tag-of-tag-structure (root-of-parse-tree form)))) 3))
        ((text-contents-entry? form) 
            (string-length (node-info form)))
        ((comment-entry? form)
            (+ 7 (string-length (comment-of-comment-structure form))))
        ((declaration-entry? form)
            (+ 3
               (string-length (as-string (kind-of-declaration-structure form)))
               (string-length (value-of-declaration-structure form))))
        ((or (start-end-tag-entry? form) (start-tag-entry? form))
            (meassure-tag-structure form))
        (else (error (string-append "meassure-html-xml-form: Unknown structure encountered: " (as-string form))))))

(define (meassure-tag-structure tag-structure)
  (let ((tag-name (tag-of-tag-structure tag-structure))
        (attributes (attributes-of-tag-structure tag-structure)))
    (+ (string-length (as-string tag-name))
        3
       (meassure-attributes attributes))))

(define (meassure-attributes attributes)
   (accumulate-right + 0   
     (map meassure-attribute (propertylist-to-alist attributes))))

(define (meassure-attribute attribute)
  (let ((key (car attribute))
        (val (cdr attribute)))
    (+ (string-length (as-string key))
       1
       (string-length (as-string val)))))
  
; ------------------------------------------------------------------

;;; Parse tree conversions.
;;; In this section we provide a number of conversion functions that work on parse trees.

;; Transform an XML or HTML parse tree to a similar surface LAML expression on output-file. 
;; This function accept parse tree rooted by the symbols html-tree, xml-tree, as well the symbol tree.
;; .parameter tree an XML or HTML parse tree
;; .parameter output-file The name of the file on which to write the LAML expression. Can be full path. Must include extension.
;; .reference "laml.scm function" "html-to-laml" "../../../man/laml.html#html-to-laml"
;; .misc When the resulting file is LAML processed, the LAML file will write the a LAML file, say f.laml, to f.html in the same directory as the laml file.
(define (parse-tree-to-laml tree output-file)
  (let ((prefix (string-append
                  "(load (string-append laml-dir \"laml.scm\")) " 
                  "(laml-style \"simple-html4.0-loose\")"
                  "(write-text-file "))
        (html-document (parse-tree-2-laml-string tree))
        (suffix " (string-append (startup-directory) laml-source-file-name-without-extension \".html\"))")
       )
    (write-text-file (string-append prefix html-document suffix) output-file)))

; Given a parse tree return a string with a LAML version of the tree.
(define (parse-tree-2-laml-string tree)
 (if (parse-tree? tree)
     (parse-tree-2-laml-string-1 tree)
     (let ((the-tree 
            (find-in-list 
             (lambda (x) (tree-entry? x))   (subtrees-of-xml-html-parse-tree tree))))
       (if the-tree
           (parse-tree-2-laml-string-1 the-tree)
           (laml-error "parse-tree-2-laml-string: Cannot find tree structure...")))))
 
; The function doing the real work
(define (parse-tree-2-laml-string-1 tree)
  (cond ((inner-node? tree)
           (cond ((html-tree-node? tree) 
                  (list-to-string (map parse-tree-2-laml-string-1 (cdr tree)) " "))
                 (else
                  (let ((root (root-of-parse-tree tree))
                        (subtrees (subtrees-of-parse-tree tree)))
                    (string-append
                     "(" 
                     (downcase-string (tag-of-tag-structure root)) " "
                     (lamlify-attributes-string (attributes-of-tag-structure root))
                     (list-to-string (map parse-tree-2-laml-string-1 subtrees) " ")
                     ")")))))
        ((terminal-node? tree)
         (cond ((text-contents-entry? tree) (string-it (string-protect (node-info tree))))
               ((start-end-node? tree) 
                (string-append 
                 "("
                 (downcase-string (tag-of-tag-structure tree)) " "
                 (lamlify-attributes-string  (attributes-of-tag-structure tree))
                 ")"))
               (else "")                ; drops comments and declarations from the output.
               )
         )
        (error "parse-tree-2-laml: Should not happen 2")))

(define (lamlify-attributes-string attr-list)
  (cond ((null? attr-list) "")
        (else (let ((key (downcase-string (as-string (car attr-list))))
                    (val (cadr attr-list)))
                (string-append 
                   "'" key " " (string-it val) " " (lamlify-attributes-string (cddr attr-list)))))))

; Protect internal string quotes in str with backslashes.
(define (string-protect str)
  (replace-string str (as-string #\") (string-append (as-string #\\) (as-string #\"))))



; Transform an XML or HTML parse tree to a LAML surface expressions (in terms of a Scheme list structure).
; This function is similar to parse-tree-to-laml which delivers a textual result 
; (a string) on an output file.
; Feb 2002: There are some problems with this function. Removed from public interface.
(define (parse-tree-to-laml-expression tree)
 (cond ((inner-node? tree)
           (cond ((and (html-tree-node? tree) (= 1 (length (cdr tree)))) 
                    (parse-tree-to-laml-expression (root-of-parse-tree tree)))
                 ((html-tree-node? tree) 
                    (map parse-tree-to-laml-expression (cdr tree)))
                 (else
                  (let ((root (root-of-parse-tree tree))
                        (subtrees (subtrees-of-parse-tree tree)))
                     (cons
                      (as-symbol (downcase-string (tag-of-tag-structure root)))
                      (append
                       (attributes-of-tag-structure root)
                       (map parse-tree-to-laml-expression subtrees)))))))
        ((terminal-node? tree)
         (cond ((text-contents-entry? tree)  (node-info tree))
               ((start-end-node? tree) 
                 (cons
                   (as-symbol (downcase-string (tag-of-tag-structure tree)))
                   (attributes-of-tag-structure tree)))
               (else "")                ; Problematic !!!
               ))
        (error "parse-tree-to-laml-expression: Should not happen")))



;; Convert a HTML/XML parse tree to a LAML abstract syntax tree in language.
;; The returned LAML abstract syntax tree will have positive spacing (which means that a white space is represented explicitly by the #t value).
;; This function accept parse tree rooted by the symbols html-tree, xml-tree, as well the symbol tree.
;; Recall that the syntax trees are used as the internal format by the validating mirrors of LAML.
;; .parameter pt The parse tree
;; .parameter language The name of the XML language, such as xhtml10-transitional. A symbol.
;; .internal-references "related function" "parse-tree-to-element-structure"
(define (parse-tree-to-ast pt language)
 (if (parse-tree? pt)
     (parse-tree-to-ast-1 pt language)
     (let ((the-tree 
            (find-in-list 
             (lambda (x) (tree-entry? x))   (subtrees-of-xml-html-parse-tree pt))))
       (if the-tree
           (parse-tree-to-ast-1 the-tree language)
           (laml-error "parse-tree-to-ast: Cannot locate parse tree proper in the parse tree: " pt)))))

; The function doing the real work.
(define (parse-tree-to-ast-1 pt language)
 (letrec ((make-ast  ; restricted version. Precondition: contents is a list.
           (lambda (element-name contents attributes kind language)
             (list 'ast (as-string element-name) contents attributes (as-symbol kind) (as-symbol language) '())))
          (make-xml-comment (lambda (str) (list 'xml-comment str)))
          (make-xml-processing-instruction (lambda (pi-target pi-string) (list 'processing-instruction pi-target pi-string)))
         )
  (cond ((parse-tree? pt)
          (let* ((node (root-of-parse-tree pt))
                 (subtrees (subtrees-of-parse-tree pt))
                 (subtrees-1 (space-expand-subtrees subtrees))  ; handling white spacing
                )
            (if (start-node? node)
                (make-ast 
                  (tag-of-tag-structure node)
                  (map (lambda (st) (parse-tree-to-ast-1 st language)) subtrees-1)
                  (attributes-of-tag-structure node)
                  'double
                  language
                )
                (laml-error "parse-tree-to-ast-1: Parse tree is expected to have a start-node as root: " pt))))
        ((textual-content-node? pt) 
            (if (equal? pt " ") #t pt))
        ((start-end-node? pt) (make-ast (tag-of-tag-structure pt) '() (attributes-of-tag-structure pt) 'single language))
        ((start-node? pt) 
          (laml-error "parse-tree-to-ast-1: Start-end node encountered as direct child of tree node. Should not happen: " pt))
        ((comment-node? pt)
            (make-xml-comment (second pt)))
        ((processing-instruction-node? pt)
            (make-xml-processing-instruction (second pt) (third pt)))
        ((or (declaration-node? pt) (xml-declaration-node? pt)  ""))
        (else (laml-error "parse-tree-to-ast-1: Unknown kind of parse tree: " pt)))))

; Expand an initial or trailing space in textual items of subtree-list to singleton space strings.
; Assume as a precondition, that textual items have been passed through the function remove-redundant-white-space.
(define (space-expand-subtrees subtree-list)
  (space-expand-subtrees-1 subtree-list '()))

; Iterative processing:
(define (space-expand-subtrees-1 subtree-list res)
  (cond ((null? subtree-list) (reverse res))
        ((textual-content-node? (first subtree-list))
           (let* ((str (first subtree-list))
                  (str-len (string-length str))
                  (init-space? (if (> str-len 0) (eqv? #\space (string-ref str 0)) #f))
                  (trailing-space? (cond ((> str-len 1) (eqv? #\space (string-ref str (- str-len 1))))
                                        (else #f)))
                  (new-res (cond ((and init-space? trailing-space?)
				  (append (list " " (substring str 1 (- str-len 1)) " ") res))
				 (init-space?
				  (append (list (substring str 1 str-len) " ") res)) ; reversed order
				 (trailing-space?
				  (append (list " " (substring str 0 (- str-len 1))) res))
				 (else (cons str res)))))
             (space-expand-subtrees-1 (cdr subtree-list) new-res)))
        (else (space-expand-subtrees-1 (cdr subtree-list) (cons (car subtree-list) res)))))


              

;; Convert a HTML/XML parse tree to an element structure ala LENO.
;; This function accept parse tree rooted by the symbols html-tree, xml-tree, as well the symbol tree.
;; Modelled after parse-tree-to-ast.
;; .internal-references "related function" "parse-tree-to-ast"
(define (parse-tree-to-element-structure pt)
 (if (parse-tree? pt)
     (parse-tree-to-element-structure-1 pt)
     (let ((the-tree 
            (find-in-list 
             (lambda (x) (tree-entry? x))   (subtrees-of-xml-html-parse-tree pt))))
       (if the-tree
           (parse-tree-to-element-structure-1 the-tree)
           (laml-error "parse-tree-to-element-structure: Cannot locate parse tree proper in the parse tree: " pt)))))

; The function doing the real work.
(define (parse-tree-to-element-structure-1 pt)
 (letrec ((make-element-structure (lambda (name contents attributes) (list 'element name contents attributes)))) ; COPIED FROM LENO
  (cond  ((parse-tree? pt)
          (let* ((node (root-of-parse-tree pt))
                 (subtrees (subtrees-of-parse-tree pt)))
            (if (start-node? node)
                (make-element-structure
                  (as-symbol (tag-of-tag-structure node))
                  (map parse-tree-to-element-structure-1 subtrees)
                  (propertylist-to-alist (attributes-of-tag-structure node)))
                (laml-error "parse-tree-to-element-structure-1: Parse tree is expected to have a start-node as root: " pt))))
        ((textual-content-node? pt) pt)
        ((start-end-node? pt) 
           (make-element-structure (as-symbol (tag-of-tag-structure pt)) '() (propertylist-to-alist (attributes-of-tag-structure pt))))
        ((start-node? pt) 
           (laml-error "parse-tree-to-element-structure-1: Start-end node encountered as direct child of tree node. Should not happen: " pt))
        ((or (declaration-node? pt) (xml-declaration-node? pt) (comment-node? pt) ""))
        (else (laml-error "parse-tree-to-element-structure-1: Unknown kind of parse tree: " pt)))))












