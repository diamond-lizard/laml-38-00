; The LAML library and programs are written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999-2006  Kurt Normark, normark@cs.aau.dk.
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



;;;; This is a non-validating HTML parser built on top of the simple XML parser for LAML. 
;;;; In addition there are HTML pretty printing procedures in this tool.
;;;; The implementation of the parser is done by redefining functions from the XML parser.
;;;; Most of the xml-parser stuff is reused in this parser.<p>
;;;; The top-level node is called a html-tree, which may hold top level comment nodes and
;;;; declaration nodes (docttype nodes).
;;;; The parser represents HTML comments within the document as special comment nodes.<p>
;;;; The parser will be very confused if it meets a less than or greater than character which isn't part of tag symbol.
;;;; Such character must be HTML protected (use the special character entities in HTML).<p>
;;;; As of LAML version 31, the parser is able to parse certain non-wellfored HTML document
;;;; (documents with crossing tags).
;;;; This tool assumes that <kbd> laml.scm </kbd> and the general library are loaded. 
;;;; The tool loads xml-support (which is the starting of this html support tool), collect-skip and file-read libraries.<p>
;;;; See <a href="xml-support.html">the XML support</a> for information about the format of 
;;;; parse trees and variables that control the pretty printing. See also
;;;; the illustrative
;;;; <a href="../../../examples/html-parser-and-pretty-printer/index.html">examples </a> 
;;;; of the HTML parsing and pretty printing tools. <p>
;;;; The typographical rebreaking and re-indenting of running text is still missing.<p>

;;;; The LAML interactive tool procedures <kbd> html-pp </kbd> and <kbd> html-parse </kbd>
;;;; in <kbd> laml.scm </kbd> are convenient top-level pretty printing and parse procedures respectively.<p>
;;;; .title Reference Manual of the HTML parser and pretty printer for LAML

;;;; Known problem: The handling of spaces after the start tag and before the end tag is not correct.<p>

;;;; Please notice that this is not a production quality parser and pretty printer! It is currently used for
;;;; internal purposes.

; Missing good support of pre tag, where a contents sting is not allowed to be rebroken.
; Cf. the function white-space-preserving-context?

(load (string-append laml-dir "tools/xml-html-support/" "xml-support.scm"))

; ---------------------------------------------------------------------------------------------------

;;; Top level HTML parsing function.

;; Parse the file in in-file-path, and deliver the parse tree in out-file-path.
;; If in-file-path has an empty file extension, html is added.
(define (parse-html-file in-file-path out-file-path)
 (let* ((init-in-path (file-name-initial-path in-file-path))
        (in-file-name-prop (file-name-proper in-file-path))
        (in-ext (file-name-extension in-file-path))
      )
   (reset-xml-parser)
   (let* ((input-port (open-input-file (string-append init-in-path in-file-name-prop "." (if (empty-string? in-ext) "html" in-ext)))))
      (set! ip input-port)
      (let ((parse-tree (parse-html-ip))
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

;; This function parses a file and return the parse tree.
;; Thus, the difference between this function and parse-html-file is that this function returns
;; the parse tree (no file output).
;; file-path is a file path (relative or absolute). An html extension is added, if necessary.
(define (parse-html file-path)
 (let ((init-path (file-name-initial-path file-path))
       (file-name-prop (file-name-proper file-path))
       (ext (file-name-extension file-path))
      )
   (reset-xml-parser)
   (let* ((input-port (open-input-file (string-append init-path file-name-prop "." (if (empty-string? ext) "html" ext)))))
      (set! ip input-port)
      (let ((parse-tree (parse-html-ip)))
        (close-input-port ip)
        parse-tree))))

(define (parse-html-ip)
  (if (not end-of-file?) (skip-white-space))
  (if (not end-of-file?)
      (parse-iteratively-html)
      '()  ; the empty tree
  )
)

(define (parse-iteratively-html)
  (parse-message "Parsing html iteratively.")
  (if (not end-of-file?) (skip-white-space))
 
  (cond ((and end-of-file? (not (parse-stack-empty?)) (>= (length parse-stack) 1))
           (aggregate-final-parse-tree 'html-tree))  ; aggregate the stack to a single parse tree
        ((and (not end-of-file?) (eq? 'tag (what-is-ahead)))
           (let* ((tag (read-tag)) 
                  (kind (kind-of-tag-structure tag)))
             (cond ((eq? kind 'start)
                     (parse-stack-push tag)
                     (parse-iteratively-html)
                   )
                   ((eq? kind 'start-end)
                     (parse-stack-push tag)
                     (parse-iteratively-html)
                   )
                   ((eq? kind 'end)
                     (let ((tree (build-html-tree-from-stack (tag-of-tag-structure tag))))
                       (parse-stack-push tree) 
                       (parse-iteratively-html)))
                   (else (laml-error "parse-iteratively-html: Unknown kind of tag" kind))
             )))
        ((and (not end-of-file?) (eq? 'contents-string (what-is-ahead)))
           (let ((contents-string (read-contents-string #f)))
                   (parse-stack-push contents-string)
                   (parse-iteratively-html)))
        ((and (not end-of-file?) (eq? 'comment (what-is-ahead)))
           (let ((comment-string (read-comment)))
                   (parse-stack-push (make-comment-structure comment-string))
                   (parse-iteratively-html)))
        ((and (not end-of-file?) (eq? 'declaration (what-is-ahead)))
           (let ((declaration-structure (read-declaration)))
                   (parse-stack-push 
                    (make-declaration-structure 
                     (kind-of-declaration-structure declaration-structure)
                     (value-of-declaration-structure declaration-structure)))
                   (parse-iteratively-html)))
        ((and (not end-of-file?) (eq? 'xml-declaration (what-is-ahead)))
           (let ((declaration-structure (read-xml-declaration)))
                   (parse-stack-push 
                    (make-xml-declaration-structure declaration-structure))
                   (parse-iteratively-html)))
        (else (parse-error "parse-iteratively-html: Parse problem."))))


(define (build-html-tree-from-stack end-tag-name)
  (parse-message "building tree: " end-tag-name)
  (build-html-tree-from-stack-1 end-tag-name '()))

; The list of tags which cannot have end tags.
(define non-end-tags (list "meta" "base" "isindex" "frame" "th" "td" "tr" "col" "colgroup" "tfoot" "thead" "option" "input" "li" "dd" "dt" "p" "hr" "param" "img" "link" "area" "br" "basefont"))

(define (build-html-tree-from-stack-1 end-tag-name tree-list)
  (let ((top (parse-stack-top-and-pop)))
    (cond ((and (start-tag-entry? top) (matches-stack-entry top end-tag-name))
             (make-parse-tree top tree-list)) ; finish
           ((and (start-tag-entry? top) (not (member (downcase-string (tag-of-tag-structure top)) non-end-tags))) 
              ; Generic end tag matching. Non-empty element instances are allowed to match an arbitrary end tag.
              ; Allows for parsing of documents which are not well-formed.
              (make-parse-tree top tree-list)) 
          ((and (start-tag-entry? top)) ; no matching - iterate. de facto an empty (start-end) tag
             (build-html-tree-from-stack-1 end-tag-name (cons (make-it-empty top) tree-list)))
          (else (build-html-tree-from-stack-1 end-tag-name (cons top tree-list))))))  ; iterate

(define (make-it-empty tag)
  (make-tag-structure 'start-end (tag-of-tag-structure tag) (attributes-of-tag-structure tag)))



; ------------------------------------------------------------------------------------------------------------------------
;;; HTML pretty printing functions.

; The tool xml-support, which is loaded by this file, contains the majority of the parsing
; stuff. Only the top level functions and a few redefinitions are done in this section.

; Still missing single line support and meassuring. 
; Move to xml-support, because most of it applies to XML as well as HMTL.

;; Pretty prints the HTML parse tree (lisp file) in in-file-path.
;; Outputs the pretty printed result in out-file-path, 
;; which defaults to in-file-path if not explicitly passed.
;; .form (pretty-print-html-parse-tree-file in-file-path [out-file-path])
(define (pretty-print-html-parse-tree-file in-file-path . optional-parameters)
 (let ((out-file-path (optional-parameter 1  optional-parameters in-file-path)))
  (let ((parse-tree (file-read in-file-path)))
   (write-text-file
    (pretty-print-html-parse-tree parse-tree) 
    out-file-path))))

;; Pretty prints a HTML parse tree, and return the result as a string.
(define (pretty-print-html-parse-tree parse-tree)
 (set! res '())
 (pretty-print-xml-html-parse-tree-1 parse-tree 0 #f)
 (linearize-pp-result (reverse res)))

; A redefinition from xml-support:
(define (pretty-print-empty-tag tag-structure start-col single-lining?)
  (pretty-print-start-tag tag-structure start-col single-lining?))

; Look up through the parse stack to find out if we are in a pre tag context.
(define (white-space-preserving-context? parse-stack)
  (find-in-list
    (lambda (x)
       (and (tag-entry? x) (equal? "pre" (downcase-string (tag-of-tag-structure x)))))
    parse-stack))

  
;; Parse the string str which is supposed to contain a HTML document. 
;; The parsing is done by writing str to the temp dir in the LAML directory, and then
;; using the function parse-html-file.
;; Precondition: The temp dir of the LAML directory must exist.
(define (parse-html-string str)
  (let* ((name "html-temp.html")
         (temp-file-path (string-append (laml-temp-file-path) name))
         (tree #f)
        )
    (if (file-exists? temp-file-path) (delete-file temp-file-path))
    (write-text-file str temp-file-path)
    (set! tree (parse-html temp-file-path))
    (delete-file temp-file-path)
    tree))