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


;;;; This is a collection of bibtex functionality for LAML.
;;;; One main part of this collection provides a parsing function of a bibtex file, leaving the result as
;;;; a list of Lisp association lists.
;;;; Another part accesses information in parsed bibtex file.
;;;; The last part has HTML/LAML presentation functions for bibtex. <p>
;;;;
;;;; Please notice that this is still not a complete bibtex parser. We currently support only the
;;;; most common and most used constructs. The Bibtex presentation functions are not complete either.<p>
;;;; 
;;;; The LAML bibtex parser is not very fast. So we envision that the
;;;; parser is activated every now and then on a bibtex file f.bib, in
;;;; order to produce the parsed lisp file f.lsp. We recommend that you
;;;; place an LAML command file together with you bibtex file which does
;;;; that job.
;;;; You can read such files using the function read-parsed-bibtex-files.<p>
;;;; 
;;;; Usage:<p>
;;;; 
;;;; It is possible to parse a bibtex file and get
;;;; the list bibtex entries represented as association lists. This is done by parse-bibtex-file.  The variable
;;;; parse-result will contain the entries. <p>
;;;; As an example, the parser is can be activated by
;;;; <pre>
;;;;    (parse-bibtex-file "file")
;;;; </pre>
;;;; where "file" is the file name without the bib extionsion.<p>
;;;; 
;;;; Given the existence of a list of bibtex entries represented as a list
;;;; of association lists bibtex-list, say parse-result, it is possible to HTML present this list by for instance
;;;; 
;;;; <pre>
;;;;    (present-bibtex-entries parse-result (p))
;;;; </pre>
;;;; Often it will be useful to extract some subset of a bibtex list. This
;;;; can, of course, be done be filtering, but the following methods makes
;;;; it possible to extract a subset given a key-list, for instance via
;;;; 
;;;; <pre>
;;;;    (bibtex-entries key-list bibtex-list warn-if-non-existing-entry)
;;;; </pre>
;;;; The keylist itself can be produced from a bibtext-list via the following call:
;;;; <pre>
;;;;    (key-list-matching "normark" bibtex-list)
;;;; </pre> <p>
;;;; Besides the general library, this library requires the collect-skip parsing library (not loaded by bibtex).
;;;; The presentation part also assumes that the HTML mirrors are loaded.<p>
;;;; 
;;;; There exists a function called bibtex in laml.scm which activates the bibtex parser, and which in addition
;;;; presents the bibtex entries in HTML via LAML. The bibtex function should be activated in an interactive
;;;; LAML session (from a Scheme prompt with LAML loaded).<p>
;;;;
;;;; This tool can be used together with the HTML4.01 and XHTML validating mirrors.
;;;; .title Reference Manual of the LAML BibTeX library


;;; Bibtex parsing. 
;;; The function bibtex-parse-file parses a bibtex file f.bib and delivers the parsed result on f.lsp.
;;; The parser is able to parse my all.bib file, but it cannot accept all variants allowed by bibtex.
;;; Multiple authors separated by 'and' are parsed to a list of authors.
;;; All bibtex values must be enclosed in {...}, not "...".

;; The result of bibtex-parsed can be accessed via the variable parse-result.
(define parse-result '())

;; A variable that controls whether bibtex-parse-file emits messages to standard output while parsing.
(define bibtex-verbose #t)

;; A boolean variable that controls if a link is to be generated from the title of a biblographic item.
;; Notice that a link is only generated if a url field is given in the bibtext record.
(define present-links-from-title? #t)

(define parsed-number 0)

; For test purposes and interactive use
(define (end-parse)
  (close-input-port ip))

; For test purposes and interactive use
(define (start-parse)
   (reset-look-ahead-buffer)
   (let* ((input-port (open-input-file "/user/normark/scheme/lib/sample.bib")))  
      (set! ip input-port)))  

(define parse-error error)

;; Parses a bibtex file file.bib and delivers the parsed result on file.lsp.
;; The result is a list of association lists where two initial keys are used:
;; type gives the bibtex type of an entry, and key gives the keyname which names the entry.
;; The other keys in the association list are given by the field names in the bibtex record.
;; All keys are symbols.
(define (parse-bibtex-file file)
   (reset-look-ahead-buffer)
   (set! parse-result '())
   (set! parsed-number 0)
   (let* ((input-port (open-input-file (string-append file "." "bib"))))  
      (set! ip input-port)
      (bibtex-parse-ip)

      (post-process! parse-result)

      (file-write
        (reverse parse-result)
        (string-append file "." "lsp"))

      (display-message (string-append "DONE. The result is in the file " file ".lsp" " and in the variable parse-result" ))

      (close-input-port ip)))

(define (bibtex-parse-ip)
  (if (not end-of-file?) (skip-white-space))
  (if (not end-of-file?)
      (let ((res (bibtex-parse-entry)))
         (set! parse-result (cons res parse-result))
         (bibtex-parse-ip))
      (if bibtex-verbose (display-message "EOF encountered"))))

(define (skip-white-space)
  (skip-while is-white-space?))

; Return an alist which represent the parsed bibtex entry
(define (bibtex-parse-entry)
  (set! parsed-number (+ parsed-number 1))
  (display (string-append (as-string parsed-number) ": "))
  (skip-white-space)
  (match-at-sign)
  (let ((bibtex-type (collect-bibtex-type))
        (bibtex-key (collect-bibtex-key))
        (entries (collect-bibtex-entries)))
    (if bibtex-verbose (display  bibtex-key))
    (skip-white-space)
    (skip-string "}" "Final } in bibtex entry expected")
    (skip-while (compose not (char-predicate #\@)))  ; skips everying after an entry an until the next @
                                                     ; this is a rough way to skip latex comments in between entries!
    (let ((res (cons
		(cons 'type bibtex-type)
		(cons
		 (cons 'key bibtex-key)
		 (map key-as-symbol entries)))))
      (display-message "  OK")
      res)))
     

(define (key-as-symbol key-value-pair)
 (let ((key (car key-value-pair))
       (value (cdr key-value-pair)))
  (cons (as-symbol key) (as-string value))))

(define (match-at-sign)
  (skip-white-space)
  (let ((ch (read-a-char)))
    (if (not (eqv? ch #\@))
        (parse-error "@ expected as leading character in a bibtex entry"))))


; we have just read a comma, just after key or after a bibtex entry.
(define (collect-bibtex-entries)
 (let ((keyword (collect-bibtex-key-in-entry))
       (value (collect-bibtex-value-in-entry))
       (comma? (is-there-a-comma-next?)))
;   (display-message (string-append keyword " " value)) ; enable for debugging purposes
   (if comma?
    (begin
      (skip-while (char-predicate #\,))
      (cons 
       (cons keyword value)
       (collect-bibtex-entries)))
    (list (cons keyword value)))))

; where are at the character after @
(define (collect-bibtex-type)
  (skip-white-space) ; if space between @ and type name. Possible? Legal?
  (let ((res (collect-until is-white-or-curly-begin?)))
    (skip-white-space)
    (skip-string "{" "{ expected after bibtex type")
    res))

(define (is-white-or-curly-begin? ch)
  (or (is-white-space? ch)
      (eqv? ch #\{)))


(define (collect-bibtex-key)
  (skip-white-space)
  (let ((res (collect-until is-white-or-comma?)))
    (skip-string "," ", expected after key")
    res))

(define (is-white-or-comma? ch)
  (or (is-white-space? ch)
      (eqv? ch #\,)))

(define (collect-bibtex-key-in-entry)
  (skip-white-space)
  (let ((res (collect-until is-white-or-equal?)))
    (skip-white-space)
    (skip-string "=" (string-append "= expected between key and value. Remember also that no comma is allowed after the last field!"))
    res))  

(define (is-white-or-equal? ch)
  (or (is-white-space? ch)
      (eqv? ch #\=)))

(define (collect-bibtex-value-in-entry)
  (skip-white-space)
  (let* ((res (collect-balanced-until (char-predicate #\{) (char-predicate #\})))  ; contains intial { and final }
         (res1 (substring res 1 (- (string-length res) 1))))
    (skip-white-space)
    res1))


; Is there a comma after possible white space
(define (is-there-a-comma-next?)
  (skip-white-space)
  (ensure-look-ahead 1)
  (eqv? (look-ahead-char) #\,))


(define (post-process! parse-result)
  (for-each do-parse-authors! parse-result)
  (for-each do-parse-editors! parse-result))

(define (do-parse-authors! bibtex-alist)
  (let* ((author-pair (assq 'author bibtex-alist)))
     (if author-pair
         (let* ((author-string-value (cdr author-pair))
                (author-list-value (extract-single-authors author-string-value)))
           (set-cdr! author-pair author-list-value)))))

(define (do-parse-editors! bibtex-alist)
  (let* ((editor-pair (assq 'editor bibtex-alist)))
     (if editor-pair
         (let* ((editor-string-value (cdr editor-pair))
                (editor-list-value (extract-single-authors editor-string-value)))
           (set-cdr! editor-pair editor-list-value)))))

; Return a list of single authors given str of the form "author1 and author2 and ... "
(define (extract-single-authors str)
  (extract-single-authors-1 str 0))

(define (extract-single-authors-1 str i-start)
  (let ((i1 (substring-index str i-start " and ")))
    (if i1 
        (cons (substring str 0 i1)
              (extract-single-authors-1 (substring str (+ i1 5) (string-length str)) 0))
        (list str))))
              
       
;;; Access to bibtex files and entries.
;;; The functions in this section accesses parsed bibtex files and bibtex entries (alist representations
;;; of bibtex entries).

;; Read and return an appended list of parsed bibtex files.
;; File-list is a list of full paths to bibtex-files without the trailing lsp extension.
(define (read-parsed-bibtex-files . file-list)
 (let ((res (accumulate-right append '()
              (map read-parsed-bibtex-file file-list))))
   (set! parse-result res)
   res))

;; Read and return the parsed entries of the bibtex file file, which is a full path without the trailing lsp extension.
(define (read-parsed-bibtex-file file)
 (file-read (string-append file "." "lsp")))

;; Access and return a bibtex entry (an association list) from bibtex-list (a parsed bibtex list).
;; If it does not exit return #f.
(define (get-bibtex-entry key bibtex-list)
  (find-in-list
    (lambda (e)
      (equal? key (get 'key e)))
    bibtex-list))

;; Return a subset of bibtex-list, as ennumerated by key-list (a list of key strings), and in the
;; order given by key-list. 
;; non-existing is a procedure or function, which is called with key as parameter if a key does not appear in bibtex-list.
;; non-existing can ignore the error, in which case the collection of entries continues, it can issue a warning,
;; or it can stop the collection.
(define (bibtex-entries key-list bibtex-list non-existing-key)
  (cond ((null? key-list) '())
        (else
          (let* ((key (car key-list))
                 (entry (get-bibtex-entry key bibtex-list)))
            (if entry
                (cons entry (bibtex-entries (cdr key-list) bibtex-list non-existing-key))
                (begin
                   (non-existing-key key)
                   (bibtex-entries (cdr key-list) bibtex-list non-existing-key)))))))

;; A possible non-existing-key function to bibtex-entries.
;; This function can be used as the last parameter to the function bibtex-entries.
(define (ignore-non-existing-entry key)
  #t)

;; A possible non-existing-key function to bibtex-entries.
;; This function can be used as the last parameter to the function bibtex-entries.
(define (warn-if-non-existing-entry key)
  (display-warning (string-append "Cannot find the bibtex key " (as-string key) " in bibtex list")))

;; A possible non-existing-key function to bibtex-entries.
;; This function can be used as the last parameter to the function bibtex-entries.
(define (stop-if-non-existing-entry key)
  (error (string-append "Cannot find the bibtex key " (as-string key) " in bibtex list")))


;; Return a list of keys (string) which match partially or fully a key in bibtex-list.
;; This function is useful to select a subset of the keys in a bibtex collection.
;; Use bibtex-entries to access these entries.
(define (key-list-matching key-part bibtex-list)
 (let ((key-part-dc (downcase-string key-part)))
 (filter 
  (lambda (str) (substring-index (downcase-string str) 0 key-part))
  (map (lambda (e) (get 'key e)) bibtex-list))))
  

;; A less than or equal predicate on parsed bibtex entries (association lists).
;; Assumes as a pre-condition that the year field is defined in both entries.
;; Takes a possible month field into consideration if necessary.
(define (bibtex-entry-leq-by-time? entry-1 entry-2)
  (let ((year-1 (defaulted-get 'year entry-1 0))
        (year-2 (defaulted-get 'year entry-2 0)))
    (cond ((< (as-number year-1) (as-number year-2)) #t)
          ((> (as-number year-1) (as-number year-2)) #f)
          (else
           (let ((month-1 (defaulted-get 'month entry-1 "??"))
                 (month-2 (defaulted-get 'month entry-2 "??")))
             (month-string-leq? month-1 month-2))))))

; A month less than or equal function on lower case, english month symbols
(define month-leq?
  (generate-leq '(january february march april may june july august september october november december) id-1))

; Is m1 <= m2. m1 and m2 are english moth names. 
; The case of m1 and m2 does not matter, but spelling does.
(define (month-string-leq? m1 m2)
 (let ((m1-dc-symbol (as-symbol (downcase-string m1)))
       (m2-dc-symbol (as-symbol (downcase-string m2))))
   (month-leq? m1-dc-symbol m2-dc-symbol)))


; An adhoc function which select a user's entries and returns them sorted by time
(define (user-bibex-entries user-key . parsed-bibtex-files)
 (let* ((bibtex-list (apply read-parsed-bibtex-files parsed-bibtex-files))
        (keys (key-list-matching user-key bibtex-list))
        (user-bibtex-list (bibtex-entries keys bibtex-list warn-if-non-existing-entry)))
   (sort-list user-bibtex-list bibtex-entry-leq-by-time?)))
   
  
;;; Bibtex HTML/LAML presentation functions. 
;;; The functions in this section presents a single bibtex entry as HTML via LAML.

;; The overall top-level function that presents a single bibtex entry. Returns an HTML string.
;; As an optional-parameter, trailing-presentation is a function that may present additional and specialized aspectes of the entry.
;; trailing-presentation is a function of one parameter, namely entry. It defaults to a function that returns the empty string.
;; .parameter entry A parsed bibtex entry.
;; .parameter trailing-presentation A function of entry that returns additional presentation to be string-appended to the main presentation. Defaults to (lambda (entry) "").
;; .returns a list of AST document constituents (pieces of markup, which for instance can be put into a div or p XHTML element).
;; .form (present-bibtex-entry entry [trailing-presentation])
(define (present-bibtex-entry entry . optional-parameters)
 (letrec ((trailing-presentation (optional-parameter 1 optional-parameters (lambda (entry) ""))))
  (let* ((type (get 'type entry))
         (presentation-function (get-presentation-function (as-symbol (downcase-string type)))))
    (con (presentation-function entry)
         (trailing-presentation entry)))))

;; Present a list of bibtex entries speparated by sep (a string). Returns an HTML string.
(define (present-bibtex-entries entries sep)
 (list-to-string
   (map present-bibtex-entry entries)
   sep))
   

(define (get-presentation-function type)
  (cond ((eq? type 'article) bibtex-present-article)
        ((eq? type 'phdthesis) bibtex-present-phdthesis)
        ((eq? type 'mastersthesis) bibtex-present-mastersthesis)
        ((eq? type 'inproceedings) bibtex-present-inproceedings)
        ((eq? type 'incollection) bibtex-present-incollection)
        ((eq? type 'techreport) bibtex-present-techreport)        
        ((eq? type 'misc) bibtex-present-misc)        
        ((eq? type 'inbook) bibtex-present-inbook)
        ((eq? type 'book) bibtex-present-book)
        ((eq? type 'manual) bibtex-present-manual)
        (else (error (string-append "Cannot find bibtex presentation function for a " (as-string type) " entry")))))

;; Get the field with key from the parsed bibtex entry.
;; The key names are as given in the bibtex source file. Two special key names apply: key and type. key is unique identification of a bibtex record.
;; The key url is special too. It accesses in reality rawurl, if available, else url.
;; type is the kind of bibtex record.
;; The optional parameter status is either the symbol required or optional, with required as default (used for warning purposes only).
(define (get-bibtex-field key entry . optional-parameters)
 (let* ((status-0 (optional-parameter 1 optional-parameters))
        (status (if (not status-0) 'required status-0))
        (real-key (cond ((and (eq? key 'url) (defaulted-get 'rawurl entry #f)) 'rawurl)
                        ((eq? key 'url) 'url)
                        (else key)))
        (res (defaulted-get real-key entry #f))
        (entry-identification 
          (lambda (entry) (string-append (get-bibtex-field 'type entry) ":" (get-bibtex-field 'key entry))))
       )
  (if (and bibtex-verbose (not res) (eq? status 'required))
      (display-warning (string-append "The required field " (as-string key) " cannot be found in " (entry-identification entry))))
  res))
     

(define (text-if condition text)
  (if condition text ""))

; (define (latex-danish-to-html-danish str)
;   (replace-strings
;     str
;     (list 
;       (list "{\\oo}" "??")
;       (list "{\\ao}" "??")
;       (list "{\\ou}" "??")
;       (list "{\\uu}" "??")
;       (list "{\\AU}" "??")
;       (list "{\\OU}" "??")
;       (list "{\\UU}" "??")
;       (list "{\\ae}" (character-entity "aelig"))
;       (list "{\\o}"  (character-entity "oslash"))
;       (list "{\\aa}" (character-entity "aring"))
;       (list "{\\AE}" (character-entity "AElig"))
;       (list "{\\O}"  (character-entity "Oslash"))
;       (list "{\\AA}" (character-entity "Aring")))))

(define (latex-danish-to-html-danish str)
  (replace-strings
    str
    (list 
      (list "{\\oo}" "??")
      (list "{\\ao}" "??")
      (list "{\\ou}" "??")
      (list "{\\uu}" "??")
      (list "{\\AU}" "??")
      (list "{\\OU}" "??")
      (list "{\\UU}" "??")
      (list "{\\ae}" "æ")
      (list "{\\o}"  "ø")
      (list "{\\aa}" "å")
      (list "{\\AE}" "Æ")
      (list "{\\O}"  "Ø")
      (list "{\\AA}" "Å"))))

;(define (no-latex-stuff str)
;  (let ((res1 (replace-string (replace-string (replace-string str "{" "") "}" "") "$" "")))
;    res1))

(define (no-latex-stuff str)
  (let ((res1 (replace-strings str '(("{" "") ("}" "") ("$" "") ("\\" "")))))
    res1))

; Perform all replacements specified by replacements in str, and return the 'replaced string'.
; Replacements is a list of replacement specifications. A replacement specification is, in turn,
; a list of two strings: (source target). 
; No side effect. There is a danger of infinit replacement if a target replacement contains a source replacement!
(define (replace-strings str replacement-list)
  (cond ((null? replacement-list) str)
        (else (let ((replacement (car replacement-list)))
                (replace-strings
                  (replace-string-multiple str (first replacement) (second replacement))
                  (cdr replacement-list))))))

; Replace all occurrences of str1 in str by str2.
; As a precondition, str1 must not occur as a substring of str2. 
; Should perhaps be extra checked.
(define (replace-string-multiple str str1 str2)
  (if (substring-index str 0 str1)
      (let ((res (replace-string str str1 str2)))
        (replace-string-multiple res str1 str2))
      str))


; -----------------------------------------------------------------------------

(define (bibtex-present-article e)
 (let ((authors (get-bibtex-field 'author e))
       (title (get-bibtex-field 'title e))
       (journal (get-bibtex-field 'journal e))
       (year (get-bibtex-field 'year e))
       (month (get-bibtex-field 'month e 'optional))
       (volume (get-bibtex-field 'volume e 'optional))
       (number (get-bibtex-field 'number e 'optional))
       (pages (get-bibtex-field 'pages e 'optional))
       (note (get-bibtex-field 'note e 'optional))
       (url (get-bibtex-field 'url e 'optional))
      )
  (con
   (present-authors-field (map latex-danish-to-html-danish authors)) _ (text-if authors ", ")
   (present-title-field (no-latex-stuff title) url)     _ (text-if title ", ")
   (present-journal-field journal) _ (text-if journal ", ")
   (present-volume-field volume)   _ (text-if volume ", ")
   (present-number-field number)   _ (text-if number ", ")
   (present-month-field month)     _ (text-if month " ")
   (present-year-field year)       _ (text-if year (if pages ", " ""))
   (present-pages-field pages)   _  ". "
;   (present-note-field note)       _ (text-if note ".")
  )))

(define (bibtex-present-phdthesis e)
 (let ((authors (get-bibtex-field 'author e))
       (title (get-bibtex-field 'title e))
       (school (get-bibtex-field 'school e))
       (year (get-bibtex-field 'year e))
       (month (get-bibtex-field 'month e 'optional))
       (address (get-bibtex-field 'address e 'optional)) 
       (note (get-bibtex-field 'note e 'optional))
       (url (get-bibtex-field 'url e 'optional))
      )
  (con
   (present-authors-field (map latex-danish-to-html-danish authors)) _ (text-if authors ", ")
   (present-title-field (no-latex-stuff title) url)     _ (text-if title ". ")
   "PhD thesis. "
   (present-school-field school)                    _ (text-if school ", ")
   (present-address-field address)                  _ (text-if address ", ")
   (present-month-field month)     (text-if month " ")
   (present-year-field year)       
   _ "."
  )))

(define (bibtex-present-mastersthesis e)
 (let ((authors (get-bibtex-field 'author e))
       (title (get-bibtex-field 'title e))
       (school (get-bibtex-field 'school e))
       (year (get-bibtex-field 'year e))
       (month (get-bibtex-field 'month e 'optional))
       (address (get-bibtex-field 'address e 'optional)) 
       (note (get-bibtex-field 'note e 'optional))
       (url (get-bibtex-field 'url e 'optional))
      )
  (con
   (present-authors-field (map latex-danish-to-html-danish authors)) _ (text-if authors ", ")
   (present-title-field (no-latex-stuff title) url)     _ (text-if title ", ")
   (present-school-field school)                    _ (text-if school ", ")
   (present-address-field address)                  _ (text-if address ", ")
   (present-month-field month)     (text-if month " ")
   (present-year-field year)       
   _ "."
  )))

(define (bibtex-present-techreport e)
 (let ((authors (get-bibtex-field 'author e))
       (title (get-bibtex-field 'title e))
       (institution (get-bibtex-field 'institution e))
       (year (get-bibtex-field 'year e))
       (month (get-bibtex-field 'month e 'optional))
       (address (get-bibtex-field 'address e 'optional))
       (number (get-bibtex-field 'number e 'optional))
       (pages (get-bibtex-field 'pages e 'optional))
       (note (get-bibtex-field 'note e 'optional))
       (url (get-bibtex-field 'url e 'optional))
       ; type not supported - conflict with our type field.
      )
  (con
   (present-authors-field (map latex-danish-to-html-danish authors)) _ (text-if authors ", ")
   (present-title-field (no-latex-stuff title) url)     _ (text-if title ", ")
   (present-number-field-tr number)   _ (text-if number ", ")
   (present-institution-field institution) _ (text-if institution ", ")
   (present-month-field month)     _ (text-if month " ")
   (present-year-field year)       _ "."
;   (present-pages-field pages)    _ 
;   (present-note-field note)       _ (text-if note ".")
  )))

(define (bibtex-present-inproceedings e)
 (let ((authors (get-bibtex-field 'author e))
       (title (get-bibtex-field 'title e))
       (booktitle (no-latex-stuff (get-bibtex-field 'booktitle e)))
       (year (get-bibtex-field 'year e))

       (editor (get-bibtex-field 'editor e 'optional))
       (pages (get-bibtex-field 'pages e 'optional))
       (organization (get-bibtex-field 'organization e 'optional))
       (publisher (get-bibtex-field 'publisher e 'optional))
       (address (get-bibtex-field 'address e 'optional))    ; not presented
       (month (get-bibtex-field 'month e 'optional))
       (note (get-bibtex-field 'note e 'optional))      
       (url (get-bibtex-field 'url e 'optional))
      )
  (con
   (present-authors-field (map latex-danish-to-html-danish authors)) _ (text-if authors ", ")
   (present-title-field (no-latex-stuff title) url)     _ (text-if title ". ")
   (present-booktitle-field booktitle) _ (text-if booktitle ", ")
   (present-pages-field pages) _ (text-if pages ", ")
   (present-publisher-field publisher) _ (text-if publisher ", ")
   (present-month-field month)     _ (text-if month " ")
   (present-year-field year) _
   "."
   (present-editor-field (if editor (map latex-danish-to-html-danish editor) #f))
   
;   (present-note-field note)       _ (text-if note ".")
  )))

(define (bibtex-present-incollection e)
 (let ((authors (get-bibtex-field 'author e))
       (title (get-bibtex-field 'title e))
       (booktitle (no-latex-stuff (get-bibtex-field 'booktitle e)))
       (year (get-bibtex-field 'year e))

       (chapter (get-bibtex-field 'chapter e 'optional))
       (editor (get-bibtex-field 'editor e 'optional))
       (publisher (get-bibtex-field 'publisher e 'optional))
       (pages (get-bibtex-field 'pages e 'optional))
       (address (get-bibtex-field 'address e 'optional))    ; not presented
       (month (get-bibtex-field 'month e 'optional))
       (note (get-bibtex-field 'note e 'optional))      

       (url (get-bibtex-field 'url e 'optional))
      )
  (con
   (present-authors-field (map latex-danish-to-html-danish authors)) _ (text-if authors ", ")
   (present-title-field (no-latex-stuff title) url)     _ (text-if title ". ")
   (present-booktitle-field booktitle) _ (text-if booktitle ", ")
   (present-chapter-field chapter #f) _ (text-if chapter ", ")
   (present-publisher-field publisher) _ (text-if publisher ", ")
   (present-month-field month)     _ (text-if month " ")
   (present-year-field year) _
   ". "
   (present-editor-field (if editor (map latex-danish-to-html-danish editor) #f))

  )))

(define (bibtex-present-misc e)
 (let ((authors (get-bibtex-field 'author e 'optional))
       (title (get-bibtex-field 'title e 'optional))
       (journal (get-bibtex-field 'journal e 'optional))
       (howpublished (get-bibtex-field 'howpublished e 'optional))
       (year (get-bibtex-field 'year e))
       (month (get-bibtex-field 'month e 'optional))
       (note (get-bibtex-field 'note e 'optional))
       (url (get-bibtex-field 'url e 'optional))
      )
  (con
   (if authors (present-authors-field (map latex-danish-to-html-danish authors)) "") _ (text-if authors ", ")
   (present-title-field (no-latex-stuff title) url)     _ (text-if title ", ")
   (present-month-field month)     _ (text-if month " ")
   (present-year-field year)       _ (text-if year ". ")
   (present-howpublished-field howpublished)     _ (text-if howpublished ".") 
;   (present-note-field note)       _ (text-if note ".")
  )))

(define (bibtex-present-inbook e)
 (let ((authors (get-bibtex-field 'author e 'optional))  ; author or editor       
       (editor (get-bibtex-field 'editor e 'optional)) 
       (title (get-bibtex-field 'title e))
       (chapter (get-bibtex-field 'chapter e 'optional))
       (pages (get-bibtex-field 'pages e 'optional))
       (publisher (get-bibtex-field 'publisher e 'optional))
       (year (get-bibtex-field 'year e))

       (volume (get-bibtex-field 'volume e 'optional))
       (series (get-bibtex-field 'series e 'optional))
       (address (get-bibtex-field 'address e 'optional))    ; not presented
       (edition (get-bibtex-field 'edition e 'optional))
       (month (get-bibtex-field 'month e 'optional))
       (note (get-bibtex-field 'note e 'optional))      
       (url (get-bibtex-field 'url e 'optional))
      )
  (if (and (not editor) (not authors))
      (display-warning (string-append "Author or editor field must occur in " (as-string e))))

  (if (and (not chapter) (not pages))
      (display-warning (string-append "Chapter or pages field must occur in " (as-string e))))

  (con
   (present-authors-field (map latex-danish-to-html-danish authors)) _ (text-if authors ", ")
   (present-title-field (no-latex-stuff title) #f)     _ (text-if title ", ")
   
   (cond (chapter (present-chapter-field chapter url))
         (pages   (present-pages-field pages))
         (else ""))
   "."
   (if editor (con "Edited by" (map latex-danish-to-html-danish editor) _ ".") "") 

   (present-volume-field volume) _ (text-if volume ", ")
   (present-series-field series) _ (text-if series ", ")

   (present-publisher-field publisher) _ (text-if publisher ", ")
   (present-month-field month)     _ (text-if month " ")
   (present-year-field year)       
   "."
  )))

(define (bibtex-present-book e)
 (let ((authors (get-bibtex-field 'author e 'optional))  ; author or editor       
       (editors (get-bibtex-field 'editor e 'optional)) 

       (title (get-bibtex-field 'title e))
       (publisher (get-bibtex-field 'publisher e))
       (year (get-bibtex-field 'year e))

       (volume (get-bibtex-field 'volume e 'optional))
       (series (get-bibtex-field 'series e 'optional))
       (address (get-bibtex-field 'address e 'optional))    ; not presented
       (edition (get-bibtex-field 'edition e 'optional))
       (month (get-bibtex-field 'month e 'optional))
       (note (get-bibtex-field 'note e 'optional))      

       (url (get-bibtex-field 'url e 'optional))
      )

  (if (and (not editors) (not authors))
      (display-warning (string-append "Author or editor field must occur in " (as-string e))))

  (if (and editors authors)
      (display-warning (string-append "You are not supposed to supply both author and editor in " (as-string e))))

  (con

   (cond  (authors (present-authors-field (map latex-danish-to-html-danish authors)))
          (editors (present-editor-field (map latex-danish-to-html-danish editors)))
          (else "???"))

   _ (text-if (or authors editors) ", ")

   (em (present-title-field-no-quotes (no-latex-stuff title) url))     

   (if edition ", " "")
   (present-edition-field edition)
   _ ". "

;   (present-editor-field (if editors (map latex-danish-to-html-danish editors) #f)) _ (text-if editors ". ")

   (present-publisher-field publisher) _ (text-if publisher ", ")    
   (present-month-field month)     (text-if month " ")
   (present-year-field year)  _ ". "

   (present-volume-field volume) _ (text-if volume ", ")
   (present-series-field series) _ (text-if series ", ")
   _ (text-if (or volume series) ".")
  )))


(define (bibtex-present-manual e)
 (let ((authors (get-bibtex-field 'author e 'optional))  ; the only required field 
       (title (get-bibtex-field 'title e))
       (edition (get-bibtex-field 'edition e 'optional))
       (organization (get-bibtex-field 'organization e 'optional))
       (year (get-bibtex-field 'year e 'optional))
       (month (get-bibtex-field 'month e 'optional))
       (address (get-bibtex-field 'address e 'optional))    ; not presented

       (note (get-bibtex-field 'note e 'optional)) ; not presented      
       (url (get-bibtex-field 'url e 'optional))
      )

  (con

   (cond  (authors (present-authors-field (map latex-danish-to-html-danish authors)))
          (else ""))

   _ (text-if (or authors) ", ")

   (em (present-title-field-no-quotes (no-latex-stuff title) url))     

   (if edition ", " "")
   (present-edition-field edition)
   _ ". "

   (present-organization-field organization) _ (text-if organization ", ")    
   (present-month-field month)     (text-if month " ")
   (present-year-field year)  _ ". "

  )))



; ----------------------------------------------------------------------------------------
; Field presentation functions

(define (present-authors-field authors)
 (cond ((and (boolean? authors) (not authors)) "")  ; should really not happen
       ((and (list? authors) (null? authors)) "")
       ((string? authors) authors)
       ((and (list? authors) (= (length authors) 1))
           (first authors))
       ((and (list? authors) (= (length authors) 2))
          (con (present-authors-field (first authors)) " and " (present-authors-field (second authors))))
       (else 
          (con (present-authors-field (first authors)) _ ", " (present-authors-field (cdr authors))))))

(define (present-title-field ttl url)
  (cond ((and (boolean? ttl) (not ttl)) "")
        ((and ttl url present-links-from-title?) (a 'href url (string-it ttl))) 
        (else (string-it ttl))))

(define (present-title-field-no-quotes ttl url)
  (cond ((and (boolean? ttl) (not ttl)) "")
        ((and ttl url present-links-from-title?) (a 'href url ttl)) 
        (else ttl)))

(define (present-journal-field f)
  (cond ((and (boolean? f) (not f)) "") 
        (else (em f))))

(define (present-volume-field f)
  (cond ((and (boolean? f) (not f)) "")
        (else (con "Vol." " " f))))

(define (present-number-field f)
  (cond ((and (boolean? f) (not f)) "")
        (else (con "No." " " f))))

(define (present-number-field-tr f)
  (cond ((and (boolean? f) (not f)) "")
        (else (con "Tech Report no." " " f))))

(define (present-month-field f)
  (cond ((and (boolean? f) (not f)) "")
        (else f)))

(define (present-year-field f)
  (cond ((and (boolean? f) (not f)) "")
        (else f)))

(define (present-pages-field f)
  (cond ((and (boolean? f) (not f)) "")
        (else (con "pp." " " f))))

(define (present-note-field f)
  (cond ((and (boolean? f) (not f)) "")
        (else f)))

(define (present-school-field f)
  (cond ((and (boolean? f) (not f)) "")
        (else f)))

(define (present-address-field f)
  (cond ((and (boolean? f) (not f)) "")
        (else f)))

(define (present-institution-field f)
  (cond ((and (boolean? f) (not f)) "")
        (else f)))

(define (present-booktitle-field f)
  (cond ((and (boolean? f) (not f)) "")
        (else (con "In " (em f)))))

(define (present-editor-field f)
  (cond ((and (boolean? f) (not f)) "")
        (else (con (present-authors-field f) (if (and (list? f) (> (length f) 1)) " (editors)" " (editor)")))))

(define (present-howpublished-field f)
  (cond ((and (boolean? f) (not f)) "")
        (else f)))

(define (present-publisher-field f)
  (cond ((and (boolean? f) (not f)) "")
        (else f)))

(define (present-organization-field f)
  (cond ((and (boolean? f) (not f)) "")
        (else f)))

(define (present-edition-field f)
  (cond ((and (boolean? f) (not f)) "")
        (else f)))

(define (present-series-field f)
  (cond ((and (boolean? f) (not f)) "")
        (else f)))       

(define (present-chapter-field f url)
  (cond ((and (boolean? f) (not f)) "")
        ((and f url present-links-from-title?) (con "the chapter " (a 'href url (string-it f)))) 
        (else (con "the chapter " (string-it f)))))

