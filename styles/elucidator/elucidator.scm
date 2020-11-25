; The LAML library and programs are written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999-2004 Kurt Normark, normark@cs.auc.dk.
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


; ---------------------------------------------------------------------------------------------------

; The elucidator style with Scheme source file support

; ---------------------------------------------------------------------------------------------------
; The Scheme system, variable scheme-system, defined at this point in the loading process,
; via previous loading of styles.scm

; ---------------------------------------------------------------------------------------------------
; Message function, version, and verbosity variable

(define elucidator-version "1")

;; If #t a number of messages are written on the output when processing is done.
;; If #f, nothing is written.
(define elucidator-verbose-mode #t)

(define (display-message message)
  (if elucidator-verbose-mode
      (begin (display (string-append message)) (newline))))

(define start-run-time (current-time))

;---------------------------------------------------------------------------------------------------
;;; Directory setup. 
;;; A number of variables and functions which gives information about relevant directories.

;; The directory in which all Scheme programs and libraries are located
(define software-base-directory laml-dir)

;; The scheme library relative to software-base-library
(define scheme-library "lib")

;; The directory in which the libraries are located
(define the-library (string-append software-base-directory scheme-library "/"))

;; The directory in which the elucidator is located
(define software-directory (string-append software-base-directory "styles/elucidator/"))

;; The directory in which the documentation source is locacted.
;; Must be redefined. Ends with a slash.
(define source-directory #f)

; The fragment of a file path which distinguishes the source-directory and the HTML destination directory.
; As default, the html files are placed in a subdirectory of the source directory named 'html'.
; You can redefine this function after the elucidator software is loaded in order to provide for another html destination.
(define (relative-source-html-destination-path-fragment)
  "html/")

;; The directory in which the generated html files are located. Depends on source-directory
(define (html-directory)
  (string-append source-directory (relative-source-html-destination-path-fragment)))

; Return the full path to the file name in the internal directory. 
; The parameter name includes a possible extension
(define (internal-file name)
 (string-append source-directory "internal/" name))

; Return the full path to the file name in the documentation source directory. 
; The parameter name includes a possible extension
(define (documentation-source-file name)
   (string-append source-directory  name))

; Return a file relative to the startup directory
(define (relative-to-startup-dir dir) (string-append (startup-directory) dir))


;---------------------------------------------------------------------------------------------------
; Requirements and loading:

(display-message 
  (string-append 
    "Welcome to the Scheme Elucidator (version "
    elucidator-version
    ") and the LAML software." ))

(display-message "Copyright (c) Kurt Normark (normark@cs.auc.dk), Aalborg University, Denmark")

(display-message "Loading libraries and the schemeDoc tool")

; Loading requirements:
; general.scm is loaded by laml.scm
(lib-load "file-read.scm")
(lib-load "html.scm")
(lib-load "html-v1.scm")
(lib-load "time.scm")
(lib-load "hex.scm")

; (load (string-append laml-dir "tools/schemedoc/schemedoc.scm"))
(load (string-append laml-dir "tools/schemedoc-extractor/schemedoc-extractor.scm"))

(display-message "Loading elucidator software")

; Read the scheme knowledge list
(define scheme-syntax-procedure-list (file-read (string-append laml-dir "r4rs/" "scheme-knowledge.lsp")))

; The URL prefix to the directory with the RS4R Scheme Report.
; Redefine in LAML setup file.
(define rs4r-url-prefix #f)

; The URL of the designated elucidator home. 
; Can be used as the root of a number of related elucidators.
; Relative to the html directory. 
; This URL Has nothing to do with the elucidator home page.
; If #f, no home-url is provided for.
(define elucidator-home-url #f)

; Do we use previous or next elucidator navigation? 
(define previous-next-elucidators #f)

; The URLs of the previous and the next elucidator.
; Used in contexts where several elucidators are connected.
; These URLs are only used if previous-next-elucidators is #t.
; Relative to the html directory. 
; If #f, but previous-next-elucidators is #t, we show 'empty' previous/next icons.
(define elucidator-previous-url #f)
(define elucidator-next-url #f)

; The color scheme of the program windows. 
; An alist mapping group strings to colors, or #f in case
; no color scheme is defined. Redefine in LAML setup file.
(define elucidator-color-scheme #f)

; ---------------------------------------------------------------------------------------------------
;;; Controlling the amount of processing. 
;;; There are a number of variables which control the amount of processing.
;;; The user of the eludicator does not set these directly via set!. Rather
;;; he or she uses a function interface, which in turn manipulates the variables.

(define make-duplicated-name-index? #t)
(define make-cross-reference-index? #t)
(define make-defining-name-index? #t)
(define make-large-source-files? #t)

; Link definitions to entries in the cross reference index?
(define link-definitions-to-cross-reference-index? #t)

;; A variable which controls whether to copy image icons from the software directory to the source (documentation) directory.
(define copy-image-files? #t)

;; make both duplicate, cross-reference and defining-name indexes
(define (make-all-indexes)
  (set! make-duplicated-name-index? #t)
  (set! make-cross-reference-index? #t)
  (set! make-defining-name-index? #t))

;; make neither duplicate, cross-reference or defining-name indexes
(define (make-no-indexes)
  (set! make-duplicated-name-index? #f)
  (set! make-cross-reference-index? #f)
  (set! make-defining-name-index? #f))

(define process-only-sources #f)

;; Only process the sources whose keys are given in the parameter.
;; If no parameteres are given, process no sources
;; If this form does not appear, process all sources.
(define (process-only . source-keys)
  (set! process-only-sources source-keys))

;; Set variables such that minimum processing is called for
(define (minimum-processing)
  (make-no-indexes)
  (process-only)
  (set! make-large-source-files? #f))

;; Set variables such that maximum processing is called for
(define (maximum-processing)
  (make-all-indexes)
  (set! make-large-source-files? #t))


; ---------------------------------------------------------------------------------------------------
; General set up, internal variables and set-functions

; Present a separate frame with a menu of programs in the documentation bundle. 
; The value of this variable must be boolean.
(define separate-program-menu? #f)

; A boolean variable that tells whether to underline links in program files
(define underline-program-links #f)

; A boolean variable that tells whether to underline links in documentation files
(define underline-documentation-links #f)

; Determination of the default font size as applied on program pages. Either large or small (symbols).
; If the value is large, we enforce generation of small as well as large programs.
; This takes considerable more processing time.
(define default-program-font-size 'small)

; Determination of which kind of 'table of contents' to show: Either overall or detailed (symbols).
(define default-table-of-content 'overall)

; Defines how to handle comments. 
; Possible values are syntactical and lexcical.
; With syntactical comment handling the comments are turned into syntactic constituents before
; the eludicator program procesing.
(define comment-handling 'syntactical)

(define syntactical-comment-designator "!!!comment")

; A boolean variable which controls whether to show a sectional comment name (within ::...::) in
; the rendering of a comment
(define show-sectional-comment-name #t)

; Redefinition from schemedoc.scm, which causes a less common name to be used as syntactical comment designator
(define COMMENT-FORM-START (string-append "(" syntactical-comment-designator " "))

; which kind of source markers to use in documentation: one of the symbols as-text, as-colored-text, as-image.
(define source-marker-kind 'as-image)

; The character used to mark detailed places in a program, and the corresponding one character string.
(define elucidator-marker-char #\@)
(define elucidator-marker-char-string (as-string elucidator-marker-char))

; The character used to escape characters with special interpretation
(define elucidator-escape-char #\\)
(define elucidator-escape-char-string (as-string elucidator-escape-char))


; Variables which are setable by set- procedures
(define documentation-filename-without-extension #f)
(define documentation-title #f)
(define documentation-author #f)
(define documentation-email #f)
(define documentation-affiliation #f)
(define documentation-abstract #f)

; A list of program sources of this elucidation batch.
; An element of this variable is triple of key, file-location, and language pairs.
; Contributions to this list are made by the program-source procedure
(define program-source-list '())

; A list of manual sources of this elucidation batch.
; Contributions to this list are made by the manual-source procedure
(define manual-source-list '())

; A list of list parsed source forms from all source files in this documentation batch.
(define source-list-list-process '())

; A list of all source keys of this documentation batch
(define source-key-list '())

; A list of all defining name occurrences of all source files in this documentation batch.
; Each element is a pair of the form (name . source-key).
(define defining-name-occurences '())

; A list of documentation source marker relations. 
; A list of tripples:  (program-id doc-id source-mark)
(define documentation-source-marker-occurences '())

; A list of name pairs of the form (applied-name . defined-name)
; The meaning is that the applied-name is used in a form: (define (defined-name...) ...).
(define defined-applied-names '())

; A list of (program-name doc-id weak/strong) triples. All constituents are symbols.
; Represents the relation between the documentation sections/entries in which certain program definitions are
; explained/mentioned either strongly or weakly.
; program-name is a link source (anchor) in the documentation text.
; doc-id is the identification of the sub-section in the documentation, where the anchor occurs.
; weak/strong is one of symbols weak or strong (meaning either a weak or a strong reference from doc to prog).
(define documented-name-occurences '())

; An alist which relates documentation-id to titles of sections and entries
(define documentation-key-title-alist '())

; An alist which relates documentation-id to the hierarcical numbers of sections and entries
(define documentation-key-numbering-alist '())

; A list of documentation elements, either sections or entries, kind-taged with 'section or 'entry resp.
(define documentation-elements '())


; An association list of names a full path manual (html) file names.
; Used to locate the relevant manual entry of a given name.
(define manual-name-file-map '())



;; Defines the source directory to be dir. The source directory is the directory which
;; contains the documentation laml file, and the path typically ends in doc. Ends in a slash.
(define (set-source-directory dir)
  (set! source-directory dir))

;; Define the name of the documentation. Per convention, this is the same
;; as the file name of the laml file, without extension.
(define (set-documentation-name name)
  (set! documentation-filename-without-extension name))

;; Define the title, affiliation, author, affiliation, and the abstract
(define (documentation-intro title author email affiliation abstract)
  (set! documentation-title title)
  (set! documentation-author author)
  (set! documentation-email email)
  (set! documentation-affiliation affiliation)
  (set! documentation-abstract abstract))

;; The number of empty lines in the bottom of an html file,
;; in order to allow navigation to bottom stuf
(define end-file-empty-lines 25)


;; The width (in pixels) of the browser
(define browser-pixel-width 1100)

;; The height of the top control frame in pixels
(define control-frame-pixel-height 130)

; The number of columns in the detailed table of contents
(define toc-columns-detail 3)

; The number of columns in the overall table of contents
(define toc-columns-overall 3)

;; The prefix character of links from documentation to program: p for program. 
;; Must be an absolute unique character in the documentation
(define p-link-prefix-char "{")  

;; The suffix character of links from documentation to program: p for program. 
;; Must be an absolute unique character in the documentation
(define p-link-suffix-char "}")


;; The prefix character of links from documentation to documentation: d for documentation. 
;; Must be an absolute unique character in the documentation
(define d-link-prefix-char "[")  

;; The suffix character of links from documentation to documentation: d for documentation. 
;; Must be an absolute unique character in the documentation
(define d-link-suffix-char "]")

;; Controls whether to present the identification of sections and entries, hidden using the background color.
(define present-hidden-ids? #f)

;; The character which defines strong linking from documentation to program. Is supposed to follow the p-link-prefix-char.
(define strong-link-char #\*)

;; The character which defines weak linking from documentation to program. Is supposed to follow the p-link-prefix-char.
;; As a convention, a link is also a weak link if there is no particular link-type modifier after the p-link-prefix-char.
(define weak-link-char #\+)

;; The character which defines a program word documentation to program. Is supposed to follow the p-link-prefix-char.
;; A program word is not linked, by type set in kbd font.
(define none-link-char #\-)

;; The default kind of linking in case a link from the documentation to program does not start with
;; either strong-link-char, weak-link-char, or none-link-char.
;; The value must be one of the symbols 'strong, 'weak, nor 'none.
(define default-program-link 'weak)


;; The frame to use for manual entries refered from the documentation.
;; A string. Either "program-frame", "documentation-frame", or another string.
(define manual-frame-from-documentation "program-frame" )

;; The frame to use for manual entries and R4RS Scheme manual information refered from the program.
;; A string. Either "program-frame", "documentation-frame", or another string.
(define manual-frame-from-program "documentation-frame")

; An enumeration of all elucidator icons. These icons are copied from the images directory of the software-directory
; to the images directory of the source-directory.
(define elucidator-image-files 
  (list "cross-index.gif" "doc-left.gif" "doc-left-weak.gif" "index.gif" 
        "question-left-arrow.gif" "question-right-arrow.gif" "small-square.gif" "three-frames-horizontal.gif"
        "three-frames.gif" "home.gif"
        "contents.gif" "overall-contents.gif"
        "xx.gif" "small-green-up-triangle.gif"

        "source-mark-black.gif" "source-mark-grey.gif" "source-mark-silver.gif"
        "source-mark-maroon.gif" "source-mark-red.gif" "source-mark-purple.gif"
        "source-mark-green.gif" "source-mark-lime.gif" "source-mark-olive.gif"
        "source-mark-yellow.gif" "source-mark-navy.gif" "source-mark-blue.gif"
        "source-mark-tetal.gif" "source-mark-aqua.gif" "source-mark-fuchsia.gif"   
 
        "small-up.gif" "small-up-blind.gif"    "small-next.gif" "small-next-blind.gif"
        "small-prev.gif" "small-prev-blind.gif"

        "nav-left.gif" "nav-right.gif"  "nav-left-empty.gif" "nav-right-empty.gif"

        "laml-mini-icon-1.gif"
        
))

;; A boolean variable controlling whether to split the cross reference index in alphabetic files.
;; If false, make one large cross reference index.
(define alphabetic-cross-reference-index? #t)


;; A boolean variale controlling whether to split the defined name index in alphabetic files.
;; If false, make one large cross reference index.
(define alphabetic-defined-name-index? #t)

;; A boolean variable that controls the initial content of the program frame.
;; If true - #t - no initial program is shown in the program frame.
;; If false - #f - the first program in the program source list is shown in the program frame.
;; Instead, a brief informative text is shown.
(define blank-initial-program? #f)

; ---------------------------------------------------------------------------------------------------

(define (display-warning message)
  (if elucidator-verbose-mode
      (begin (display (string-append "WARNING: " message)) (newline))))

; Return the interal anchor name of id.
; id can be a string or symbol.
(define (internal-reference id)
  (string-append (as-string id)))

; ---------------------------------------------------------------------------------------------------
;;; Top level functions

;; Define a documentation source in terms of a number of elements. 
(define (program-source . elements)
  (set! program-source-list
        (cons elements program-source-list)))

;; Define a manual source to be taken into consideration. Full path.
;; A manual source is supposed to contain a single file-location element, which
;; is supposed to hold the full path (with or without extension) to a manual file manlsp file.
;; A path without extension is recommended.
;; Also, it must contain an url-location field with the address of the manual html file.
(define (manual-source . elements)
  (set! manual-source-list 
        (cons elements manual-source-list)))

;; Define a documentation-section. 
;; Internally, this function collect information about a documentation section
(define (documentation-section . elements)
 (set! section-number (+ section-number 1))
 (set! subsection-number 0)
 (let ((id (get-value 'id elements))
       (title (get-value 'title elements))
       (numbering (section-numbering))
       (raw-numbering (list section-number subsection-number)) ; always 0 as subsection-nuber
      )
  (set! documentation-elements
        (cons
          (append (make-associations
                     (list 'kind    'numbering 'raw-numbering)
                     (list 'section numbering   raw-numbering))
                  elements)
          documentation-elements))
  (set! documentation-key-title-alist
        (cons (cons id title) documentation-key-title-alist))
  (set! documentation-key-numbering-alist
        (cons (cons id numbering) documentation-key-numbering-alist))
 ))

;; Define a documentation entry.
;; Internally, this function collects information about a documentation entry.
(define (documentation-entry . elements)
 (set! subsection-number (+ subsection-number 1))
 (let ((id (get-value 'id elements))
       (title (get-value 'title elements))
       (numbering (subsection-numbering))
       (raw-numbering (list section-number subsection-number))
      )
  (set! documentation-elements
        (cons
          (append (make-associations
                     (list 'kind    'numbering   'raw-numbering)
                     (list 'entry    numbering   raw-numbering))
                  elements)
          documentation-elements))
  (set! documentation-key-title-alist
        (cons (cons id title) documentation-key-title-alist))
  (set! documentation-key-numbering-alist
        (cons (cons id numbering) documentation-key-numbering-alist))
))

; Make an a list, associating with list (not cons).
(define (make-associations keys values)
  (pair-up keys (map list values)))

;; Begin the documentation part. This ends the preamble section.
(define (begin-documentation)

  ; only make html directory in the simple case where it resides as a subdirectory in the source-directory
  (if (and (equal? "html/" (relative-source-html-destination-path-fragment))
           (not (directory-exists? (string-append source-directory "html/"))))
      (make-directory-in-directory source-directory "html"))

  (ensure-directory-existence! (string-append source-directory (relative-source-html-destination-path-fragment)) "images")

  (ensure-directory-existence! source-directory "internal")
)

;; End of documentation part.
;; Makes all the html stuff. Until now we have collected stuff. Here we generate html files
;; based on the collected stuff.
(define (end-documentation)

  ; make the help page in the actual html directory
  (display-message "Making the help page")
  (make-elucidator-help-page)

  ; copy image files from the software directory to the html directory
  (if copy-image-files?    
      (begin
        (display-message "Copying image files")
        (copy-files 
          elucidator-image-files
          (string-append software-directory "images/")
          (string-append source-directory (relative-source-html-destination-path-fragment) "images/") )))

  ; reversing source and documentation lists:
  (set! program-source-list (reverse program-source-list))
  (store-lisp-expression program-source-list (internal-file "program-source-list"))  ; @a

  (set! manual-name-file-map (pre-process-manual-lsp-files manual-source-list))

  (set! documentation-elements (reverse documentation-elements))

  ; @b save the list of documentation keys. This is for communication with the editor part of the elucidator
  (store-lisp-expression (reverse (map car documentation-key-title-alist)) (internal-file "documentation-ids"))  

  (let ((program-source-list-process 
          (filter process-source? program-source-list))
        (program-source-list-non-process
          (filter (negate process-source?) program-source-list)))

    ; Pre-processing comments in source files, thereby defining new source files in the internal directory
     (if (eq? comment-handling 'syntactical) 
        (begin 
          (display-message "Pre-processing lexical comments in source files")
          (pre-process-comments-in-files! program-source-list-process)
        )
        (display-message "NO Pre-processing lexical comments in source files")
     )

    ; parse source files (only those to be processed), and store the list of the results:
    (display-message "Parsing source files")
    (set! source-list-list-process
          (map read-source   ; @c
               (map source-file-determinator program-source-list-process)))
  
    (set! source-key-list
          (map (lambda (ps) (get-value 'key ps)) program-source-list))

    (let ((source-key-list-process (map (lambda (ps) (get-value 'key ps)) program-source-list-process))
          (source-key-list-non-process (map (lambda (ps) (get-value 'key ps)) program-source-list-non-process)))

   
     ; collect all defining names from all source files @d
     ; each element is a pair of the form (name source-key).
     ; store newly calculcated defined names in -.name files
     (display-message "Collecting and reading defining name occurences")
     (set! defining-name-occurences
           (append
            (accumulate-right                              ; FIRST PART @e
              append 
              '()
              (map2 (lambda (sl key) 
                      (let ((def-names (defined-names sl)))
                         (store-defined-names key def-names) ; store them!
                         (map (lambda (dn) (cons dn key)) def-names)))
                    source-list-list-process
                    source-key-list-process))
            (accumulate-right                              ; SECOND PART @f
              append
              '()
              (map restore-defined-names source-key-list-non-process)))
     )

     ; @g make documentation file. Hereby the global variable documented-name-occurences is assigned.
     (display-message "Presenting and resolving links in the documentation")
     (write-text-file
        (page 
          "documentation"
          (documentation-contents)
          (color-of-group "doc") black black black
        )
        (html-destination "documentation"))

     ; save documented-name-occurences. This is for communication with the editor part of the elucidator
     (store-lisp-expression (reverse documented-name-occurences) (internal-file "documented-names"))

     ; reverse documentation-source-marker-occurences such that documentation source markers are encountered in the right sequence
     ; when we process the program source files.
     (set! documentation-source-marker-occurences (reverse documentation-source-marker-occurences))
   
     ; make program files @g:
     (display-message "Making program source files")
     (for-each 
        (lambda(ps source-list)
          (display-message (string-append "  " (get-value 'key ps)))
          (make-source-program-file (get-value 'key ps) (defaulted-get-value 'group ps "program") (source-file-determinator ps) (get-value 'language ps) 
                                    source-list defining-name-occurences documented-name-occurences 'small))
        program-source-list-process
        source-list-list-process
     )

     ;@i
     (if (or make-large-source-files? (eq? default-program-font-size 'large))
       (begin
       ; make large source files:
       (display-message "Making LARGE program source files")
       (for-each 
         (lambda(ps source-list)
           (display-message (string-append "  " (get-value 'key ps)))
           (make-source-program-file (get-value 'key ps) (defaulted-get-value 'group ps "program") (source-file-determinator ps) (get-value 'language ps) 
                                     source-list defining-name-occurences documented-name-occurences 'large))
         program-source-list-process
         source-list-list-process
       )))

   
     ; @j make control file:
     (display-message "Making the control file")
     (write-text-file
        (page 
           "control" 
           (con-space 
             (icon-bar)
             (when-generated))
           (color-of-group "index") black black black
        )
        (html-destination "control"))

     ; @k
     (if make-duplicated-name-index?
      (begin
       ; make duplicate report:
       (display-message "Making the duplicate report")
       (write-text-file
          (page "Duplicate report"
            (con
              (icon-bar)
              (present-duplicated-definitions)
            ) (color-of-group "index") black black black
          )
          (html-destination "duplicate-report")))
       (display-message "NO duplicated name index is being generated")
     )

     ; make index: defined names
     (if make-defining-name-index?      ;@l
         (let ((sorted-defining-name-occurences (sort-list defining-name-occurences name-entry-leq?)))
           (display-message "Making index of defined names")
           (display-message 
             (if alphabetic-defined-name-index? 
                 "  alphabetically broken"
                 "  as one large index"))

           (if alphabetic-defined-name-index?
               (let* ((splitted-defining-name-occurences (split-defining-name-occurences sorted-defining-name-occurences))
                      (alphabet (map downcase-string (map first-letter-of (map caar splitted-defining-name-occurences)))))
                 (map2 (lambda (dan letter)
                         (make-defining-name-index dan letter alphabet))
                       splitted-defining-name-occurences
                       alphabet)
                 (make-overall-defining-name-index alphabet))
             (begin
              (write-text-file
               (page "Alphabetic index of defined names"
                     (con
                      (icon-bar)  
                      (present-defined-name-index sorted-defining-name-occurences)
                      ) (color-of-group "index") black black black
                     )
               (html-destination "defining-name-index")))))
       (display-message "NO index of defined names is being generated")
       )

     (if make-cross-reference-index?
      ; extracting applied-defined name pairs
      (begin
       (display-message "Extracting applied-defined name pairs from parsed source files")
       (set! defined-applied-names
          (applied-names-multiple-sources
             (append
                source-list-list-process  ; the list of sources processed in this run
                (map read-source 
                   (map (lambda (ps) (get-value 'file-location ps)) program-source-list-non-process)) ; the list of sources that need to be read
             )))
                
       ; make index: cross references involving applied names
       ;@o
       (display-message "Presenting the extracted cross reference index") 
       (display-message 
         (if alphabetic-cross-reference-index? 
             "  alphabetically broken"
             "  as one large index"))
       (let ((extended-defined-applied-names 
              (merge-defined-and-defined-applied-lists
               defined-applied-names
               (sort-list 
                (map (lambda (x) (cons (car x) #f)) defining-name-occurences) (lambda (x y) (string<=? (as-string x) (as-string y)))))))

                
         (if alphabetic-cross-reference-index? 
             (let* ((sdan (split-defined-applied-names extended-defined-applied-names))
                    (alphabet (map downcase-string  (map first-letter-of (map caar sdan)))))   
               (map2 (lambda (dan letter)
                       (make-cross-reference-index dan letter alphabet))
                     sdan 
                     alphabet)
               (make-overall-cross-reference-index alphabet)  ; with the alphabet navigator
               )
           (write-text-file
            (page "Alphabetic cross reference index"
                  (con
                   (icon-bar)  
                   (present-cross-reference-index
                    extended-defined-applied-names)
                   ) (color-of-group "index") black black black
                  )
            (html-destination "cross-reference-index")))))
       (display-message "NO cross reference index is being generated")
      )

      ; @l documentation section contents
      (display-message "Presenting overall documentation table of contents")
      (write-text-file
          (page "Documentation table of contents"
            (con
              (icon-bar)  
              (present-documentation-contents documentation-elements 'overall)
              (when-generated)
            ) (color-of-group "index") black black black
          )
          (html-destination "documentation-toc-overall"))

      (display-message "Presenting detailed documentation table of contents")
      (write-text-file
          (page "Documentation table of contents"
            (con
              (icon-bar)  
              (present-documentation-contents documentation-elements 'detail)
              (when-generated)
            ) (color-of-group "index") black black black
          )
          (html-destination "documentation-toc-detail"))

     ; Making program menu frame:

     (write-text-file
         (page "Program Menu"
            (source-file-links-for-program-menu program-source-list)
            (color-of-group "index") black black black
         )
         (html-destination "program-menu"))

     ; @m make frame files, in which the program is the first mentioned program source
     (display-message "Making frame files")


     (make-frame-file-in-html-dir
        "Scheme Elucidator"
        (elucidator-frame 
           (documentation-toc-name) 
           "program-menu"
           "documentation"
           (initial-program-page program-source-list) ; program starting point
           ""  ; in html directory
        )
        "index")

     (display-message (string-append "The Elucidator result is available in " (source-filename-without-extension scheme-system) ".html,"))
     (display-message (string-append "which is located in the same directory as the setup and documentation files"))

      ; Also make a frame file in the source directory, for easy and convenient start of the browsing:
     (make-frame-file-in-source-dir
        "Scheme Elucidator"
        (elucidator-frame 
           (documentation-toc-name)
           "program-menu"
           "documentation"
           (initial-program-page program-source-list) ; program starting point
           (relative-source-html-destination-path-fragment)
        )
        (source-filename-without-extension scheme-system))
     
     (make-frame-file-in-html-dir
        "Scheme Elucidator"
        (elucidator-frame-horizontal   ; always in html-dir
           (documentation-toc-name) 
           "program-menu"
           "documentation"
           (initial-program-page program-source-list) ; program starting point
           ""
        )
        "index-horizontal")


     (let ((program-frame-content
            (con
              (vertical-space 1)
              (center (font 6 grey "The Scheme Elucidator"))
              (center (font 6 grey "Program Frame"))
              (vertical-space 1)
              (center (narrow-with-pixels 100 
                       (font 4 grey 
                        (con
                         (p "Scheme source programs are shown here when they are selected in the documentation frame.")
                         (p
                           (if separate-program-menu?
                               "You can also select the programs in the upper right menu."
                               "You can also select the programs in the upper control frame.")))))))) 
          )
      (write-text-file
         (page "Blank Initial Program"
            program-frame-content
            white black black black
         )
         (html-destination "blank-initial-program"))

      (write-text-file
         (page "Blank Initial Program"
            program-frame-content
            white black black black
         )
         (html-destination "blank-initial-program-LARGE")))

     (display-message (string-append "Total processing time: " (present-time-interval (- (current-time) start-run-time))))
   
 )))

(define (initial-program-page program-source-list)
 (if blank-initial-program?
     "blank-initial-program"
     (get-value 'key (car program-source-list))))

(define (pre-process-manual-lsp-files manual-source-list)
 (flatten 
  (map pre-process-manual-lsp-file
       (map (lambda (entry) (car (get 'file-location entry))) manual-source-list)
       (map (lambda (entry) (car (get 'url-location entry))) manual-source-list))))

; Return a contribution to the manual map.
; full-manual-file-path can be with or without lsp extension.
; We actually force the manlsp extension if such a file exists, else a lsp extension.
(define (pre-process-manual-lsp-file full-manual-file-path manual-url)
 (let* ((full-manual-file-path-manlsp (string-append (file-name-initial-path full-manual-file-path) (file-name-proper full-manual-file-path) "." "manlsp"))
        (full-manual-file-path-lsp (string-append (file-name-initial-path full-manual-file-path) (file-name-proper full-manual-file-path) "." "lsp"))
        (actual-full-manual-file-path
          (cond ((file-exists? full-manual-file-path-manlsp) full-manual-file-path-manlsp)
                ((file-exists? full-manual-file-path-lsp) full-manual-file-path-lsp)
                (else (laml-error "pre-process-manual-lsp-file: Cannot locate internal manual file:" full-manual-file-path-manlsp "or " full-manual-file-path-lsp))))
       )
  (letrec ((manual-page? (lambda (lsp-entry) (equal? "manual-page" (car (get 'kind lsp-entry)))))
	   (get-symbol-name-of-lsp-entry (lambda (lsp-entry) (car (get 'title lsp-entry))))
	   )
          
    (if (file-exists? actual-full-manual-file-path)
	(let ((lsp-structure (file-read actual-full-manual-file-path)))
	  (map (lambda (name)
		 (cons name manual-url))
	       (map get-symbol-name-of-lsp-entry (filter manual-page? lsp-structure))))
	(laml-error "Cannot locate LAML manual file: " actual-full-manual-file-path)))))



; Return either "documentation-toc-overall" or "documentation-toc-detail" depending
; on the global variable "documentation-toc-overall"
(define (documentation-toc-name)
 (cond ((eq? default-table-of-content 'overall) "documentation-toc-overall")
       ((eq? default-table-of-content 'detailed) "documentation-toc-detail")
       (else (laml-error "documentation-toc-name: Unknown default-table-of-content: " default-table-of-content))))



; Return the name of the source file to parse, given source descriptor and the global variable comment-handling
(define (source-file-determinator source-descriptor)
  (cond ((eq? comment-handling 'syntactical) (internal-syntactic-commented-file (get-value 'key source-descriptor)))
        ((eq? comment-handling 'lexical) (get-value 'file-location source-descriptor))
        (else (error "source-file-determinator: Unknown kind of comment-handling"))))

; Return the name of the file holding the comment transformed source file (with syntactic comments).
(define (internal-syntactic-commented-file source-key)
 (string-append source-directory "internal/" (as-string source-key) "-syntactical-comments"))



; Pre-process all source files in source-file-list, which is a source file descriptor
(define (pre-process-comments-in-files! source-file-list)
  (map pre-process-comments! source-file-list))

; Pro-process a single source-file-descriptor. 
; This defines a file in the internal directory
(define (pre-process-comments! source-file-descriptor)
  (let* ((input-file (get-value 'file-location source-file-descriptor))
         (source-key (get-value 'key source-file-descriptor))
         (output-file (internal-syntactic-commented-file source-key)))
    (lexical-to-syntactical-comments! input-file output-file)))

(define (when-generated)
 (let* ((dt (date-time (current-time)))
        (date (car dt))
        (time (cadr dt)))
  (font 2 red (con "Generated: " date ", " time))))


; Store the lisp expression exr on the file with full path file-path.
(define (store-lisp-expression expr file-path)
 (if (file-exists? file-path) (delete-file file-path))
 (with-output-to-file file-path
       (lambda () (write expr))))


(define (icon-bar)
 (left-right-banner
  (table-3 0 (append (if previous-next-elucidators (list 30 30 30) '())
                     (if elucidator-home-url (list 30 30) '())
                     (list 30 30   30    30 30 30    30    30 30  30  30 30   60  1000)) 
           (list
            (append
              (if previous-next-elucidators
                  (list
                   (if elucidator-previous-url 
                       (a-tag-target elucidator-previous-url (image "nav-left.gif" "Go to previous elucidator") "_top")
                       (image "nav-left-empty.gif" ""))
 
                   (if elucidator-next-url 
                       (a-tag-target elucidator-next-url (image "nav-right.gif" "Go to next elucidator") "_top")
                       (image "nav-right-empty.gif" ""))

                   " "
                  )
                  '())

              (if elucidator-home-url
                  (list 
                    (a-tag-target elucidator-home-url (image "home.gif" "Go home") "_top")
                    " ")
                  '())
             (list 

	      (a-tag-target "index.html" (image "three-frames.gif" "Reset Elucidator to vertical layout") "_top")
	      (a-tag-target "index-horizontal.html" (image "three-frames-horizontal.gif" "Reset Elucidator to horizontal layout") "_top")
	      " "
	      (a-tag-target "defining-name-index.html" 
			    (image "index.gif" "Alphabetic index of defined names in the program") "control-frame")  
	      (a-tag-target "cross-reference-index.html" (image "cross-index.gif" "Cross reference index") "control-frame") 
	      (a-tag-target "duplicate-report.html" (image "xx.gif" "Duplicated definitions") "control-frame")

	      " "
	      (a-tag-target "documentation-toc-detail.html" (image "contents.gif" "Detailed documentation table of contents") "control-frame")
	      (a-tag-target "documentation-toc-overall.html" (image "overall-contents.gif" "Overall documentation table of contents") "control-frame")

	      " "
	      (a-tag-target "elucidator-help.html" 
			    (image "question-left-arrow.gif" "Elucidator Help Page to be shown in the documentation frame")
			    "documentation-frame")

	      (a-tag-target "elucidator-help.html"
			    (image "question-right-arrow.gif" "Elucidator Help Page to be shown in the program frame")
			    "program-frame")


	      " "

	      (if (not separate-program-menu?) (source-file-links program-source-list) "")
	      )))
           'middle
           )
    (laml-power-icon 0 'small)
  )
)

; do we have to process program-source (a triple of key, file-location and language)?
(define (process-source? program-source)
 (let ((source-key (get-value 'key program-source)))
  (if process-only-sources
    (member source-key process-only-sources)
    #t)))

; read the list of defined names (list of (name . source-key)) from file
; if no file found, return the empty list
(define (restore-defined-names source-key)
  (let ((restore-filename (defining-names-file source-key)))
    (if (file-exists? restore-filename)
        (let* ((ip (open-input-file restore-filename))
               (res (read ip)))
          (display-message (string-append " Restoring defined names from " source-key ".names"))
          (close-input-port ip)
          (map 
            (lambda (entry) (cons (as-symbol (car entry)) (cdr entry)))
            res))
        (begin
          (display-warning (string-append "No defining names stored for " source-key))
          '()))))

; Write the list of defined names (list of (name . source-key)) to file
(define (store-defined-names source-key defined-names)
  (let ((store-filename (defining-names-file source-key))
        (keyed-names (map (lambda (dn) (cons (as-string dn) source-key)) defined-names)))
    (if (file-exists? store-filename) (delete-file store-filename))
    (with-output-to-file store-filename
       (lambda () (write keyed-names)))))


; return the file name (full path) of the name file for source-key
(define (defining-names-file source-key)
  (string-append source-directory "internal/" source-key ".names"))

; source file links - inline version for icon-bar
(define (source-file-links program-source-list)
 (let* ((source-key-list   (map (lambda (ps) (get-value 'key ps)) program-source-list))   ; extract keys from program-source-list
        (source-group-list (map (lambda (ps) (defaulted-get-value 'group ps "program")) program-source-list)) ; similarly extract the groups
        (source-file-list (map (lambda (ps) (get-value 'file-location ps)) program-source-list)) ; similarly extract the file-locations
        (size-string (if (eq? default-program-font-size 'large) "-LARGE" ""))
       )
  (table-1 1 
    (map (lambda (sk) (* (string-length sk) 7)) source-key-list)
    (map color-of-group source-group-list)
    (list
     (map2
      (lambda (sk sf)
         (html:a
          (font-size 2 sk)
          'href (add-file-extension (string-append sk size-string) "html")
          'title sf
          'target "program-frame"
          'style (if underline-program-links "{text-decoration: underline;}" "{text-decoration: none;}")
         )
      )
      source-key-list
      source-file-list)))))

; source file links - version of program menu frame
(define (source-file-links-for-program-menu program-source-list)
 (let* ((source-key-list   (map (lambda (ps) (get-value 'key ps)) program-source-list))   ; extract keys from program-source-list
        (source-group-list (map (lambda (ps) (defaulted-get-value 'group ps "program")) program-source-list)) ; similarly extract the groups
        (source-file-list (map (lambda (ps) (get-value 'file-location ps)) program-source-list)) ; similarly extract the file-locations
        (size-string (if (eq? default-program-font-size 'large) "-LARGE" ""))
       )
  (table-4 1 
    (list 240)
    (map color-of-group source-group-list)
    (map2
      (lambda (sk sf)
       (list
         (html:a
          (font-size 2 sk)
          'href (add-file-extension (string-append sk size-string) "html")
          'title sf
          'target "program-frame"
          'style (if underline-program-links "{text-decoration: underline;}" "{text-decoration: none;}")
         )
       )
      )
      source-key-list
      source-file-list))))


; ---------------------------------------------------------------------------------------------------


; Syntax functions

; Return a syntax function
(define (make-syntax-function syntax-symbol)
  (lambda values
    (cons syntax-symbol values)))

; Tag elements with a kind, defined to be kind-symbol
(define (tag-kind kind-symbol elements)
  (cons (list 'kind kind-symbol) elements))

; Get the key value from elements. Elements is assumed to be a list of double-lists.
; In this context, a double list is a list of length two.
; Example of elements (("x" 5) ("y" 6)).
(define (get-value key elements)
 (let ((res (assoc key elements)))
   (if (and (list? res) (> (length res) 1))
       (cadr res)
       (error (string-append "get-value in elucidator: Problems accessing a value of a syntax element: " (as-string res))))))

; As get-value, but instead of a fatal error we return default-value if key is not in elements.
(define (defaulted-get-value key elements default-value)
 (let ((res (assoc key elements)))
   (if (and (list? res) (> (length res) 1))
       (cadr res)
       default-value)))

; Get the list of values of an element
(define (get-values key elements)
  (cdr (assoc key elements)))

(define key (make-syntax-function 'key))
(define file-location (make-syntax-function 'file-location))
(define url-location (make-syntax-function 'url-location))
(define language (make-syntax-function 'language))
(define group (make-syntax-function 'group))

(define id (make-syntax-function 'id))
(define title (make-syntax-function 'title))
(define index-words (make-syntax-function 'index-words))
(define intro (make-syntax-function 'intro))
(define sources (make-syntax-function 'sources))
(define body (make-syntax-function 'body))

; General functions

(define (html-destination filename)
  (string-append (html-directory) filename ".html"))

(define (source-destination filename)
  (string-append source-directory filename ".html"))

; ---------------------------------------------------------------------------------------------------
; Color settings:

;; apply black and white coloring if you make hard copies for black and white proceedings or articles
(define black-and-white-coloring #f) 

(define defined-color (make-color 255 0 0))
; (define comment-color (make-color 0 100 0)) 
(define comment-color (make-color 112 168 0)) 
(define applied-color (make-color 0 0 128)) 
(define documentation-section-color (make-color 0 204 255))
(define documentation-entry-color (make-color 0 204 255))
(define documentation-program-link-color red)
(define documentation-program-link-color-weak applied-color)
(define documentation-documentation-link-color blue)
(define none-reference-color (make-color 70 70 70))
(define r4rs-scheme-color brown)
(define manual-name-color (make-color 0 90 0)) ; dark green

(define default-background-color white)

(define documentation-background-color (make-color 255 236 217))

(define program-background-color-1     white)                     ; white
(define program-background-color-2     (make-color 221 255 221))  ; very light green
(define program-background-color-3     (make-color 198 226 255))  ; very light blue  (make-color 222 222 239)
(define program-background-color-4     (make-color 255 230 230))  ; very light red
(define program-background-color-5     (make-color 226 226 199))  ; very light brown
(define program-background-color-6     (make-color 255 255 193))  ; very light yellow
(define program-background-color-7     (make-color 224 224 224))  ; very light grey
(define program-background-color-8     (make-color 255 210 255))  ; very light purple
(define program-background-color-9     (make-color 155 255 255))  ; another very light blue
(define program-background-color-10    (make-color 255 214 193))  ; very light orange

;; Re-assigns the colors in order to provide for good printing in black and white
(define (apply-black-and-white-hardcopy-colors!)
  (set! defined-color black)
  (set! comment-color black) 
  (set! applied-color black) 
  (set! documentation-section-color (make-color 0 204 255))
  (set! documentation-entry-color (make-color 0 204 255))
  (set! documentation-program-link-color black)
  (set! documentation-program-link-color-weak black)
  (set! documentation-documentation-link-color black)
  (set! none-reference-color black)
  (set! underline-program-links #f)
  (set! underline-documentation-links #t)
  (set! r4rs-scheme-color black)
)

(if black-and-white-coloring (apply-black-and-white-hardcopy-colors!))


; ---------------------------------------------------------------------------------------------------
(define image-file-access 'sub-directory)

(define (image file-name help-text) (html:img 'src (image-file file-name) 'alt help-text 'border 0))

; ---------------------------------------------------------------------------------------------------
;;; Scheme source file reading. 

;; Read the file (a lisp source file) and return a list of the lisp expressions found in the source file
(define (read-source file)
 (let* ((ip (open-input-file file))
        (res (read-source-1 ip '())))
   (close-input-port ip)
   (reverse res)))


(define (read-source-1 input-port source-list)
  (if (eof-object? (peek-char input-port))
      source-list
      (read-source-1 input-port (cons (read input-port) source-list))))


; ---------------------------------------------------------------------------------------------------
;;; Extraction of top level defined names from parsed Scheme expressions. 

;; Return the list of top-level defined names in the source list
;; Source list may be as returned by read-source.
(define (defined-names source-list)
  (defined-names-1 source-list '()))

(define (defined-names-1 source-list res)
  (if (null? source-list) 
      (reverse res)
      (let ((form (car source-list)))
        (if (is-define-form? form)
            (defined-names-1 (cdr source-list) (cons (defined-name form) res))
            (if (syntactical-comment? form)
                (let ((section-name (section-name-comment? (comment-string-of-syntactical-comment form))))
                   (if section-name 
                       (defined-names-1 (cdr source-list) (cons (as-symbol section-name) res))
                       (defined-names-1 (cdr source-list) res)))
                (defined-names-1 (cdr source-list) res))))))
          

(define (is-define-form? x)
  (and (list? x) 
       (> (length x) 1)
       (or (eq? (car x) 'define)
	   (and (symbol? (car x))
		(let ((s (symbol->string (car x))))
		  (and (> (string-length s) 6)
		       (string=? (downcase-string (substring s 0 7)) "define-")))))))

(define (syntactical-comment? x)
  (and (list? x) 
       (not (null? x))
       (eq? (car x) (as-symbol syntactical-comment-designator))))

; syntactical comment selectors:

(define comment-string-of-syntactical-comment (make-selector-function 3 'comment-string-of-syntactical-comment))
(define comment-level-of-syntactical-comment (make-selector-function 2 'comment-level-of-syntactical-comment))

; This function takes the string of a syntactical comment and returns whether
; it is a section name comment. A positive answer returns the sectional comment name (a string without double colons).
(define (section-name-comment? comment-string)
  (let ((p1 (skip-chars-in-string comment-string white-space-char-list 0)))
    (if (looking-at-substring? comment-string p1 "::")
        (let ((p2 (find-in-string comment-string  #\: (+ p1 2))))    ; finding first colon at the end of name
          (if p2
              (substring comment-string (+ p1 2) p2)  ; returning portin of string between double colons
              #f))
        #f)))

; Returned the defined name in x, given that x is a define form.
(define (defined-name x)
  (if (pair? (cadr x))
      (car (cadr x))
      (cadr x)))

; Return the bounded names of the form f.
; This function works on an arbitrary form.
; As a peculiarity, this function does not recognize the name in (define n ...) as a bound name.
; But the names (x y z) are bound in (define (n x y z) ...)
(define (bounded-names x)
  (cond ((is-define-form? x) (parameter-names x))  
        ((let-form? x) (let-names x))
        ((lambda-form? x) (lambda-names x))
        (else '())))

(define (parameter-names x)
  ; Return the bounded names in x, which is a define form
  ; Assume as a pre-condition that x is a define form.
    (cond ((pair? (cadr x))
           (let ((call-form (cadr x)))
             (cond ((list? call-form) (cdr call-form))
                   ((pair? call-form) 
                      (cond ((pair? (cdr call-form)) (append (proper-part (cdr call-form)) (list (first-improper-part (cdr call-form)))))
                            ((symbol? (cdr call-form)) (list (cdr call-form))))
                      ))))
          ((symbol? (cadr x))
           (if (> (length x) 2)
               (let ((y (caddr x)))  ; possible lambda form
                 (if (and (pair? y) (eq? (car y) 'lambda))
                     (let ((par (cadr y)))
                       (cond ((symbol? par) (list par))
                             ((list? par) par)
                             ((pair? par) (append (proper-part par) (list (first-improper-part par))))))
                     '()))
               '()))
         (else '())))

; ---------------------------------------------------------------------------------------------------
; Scheme knowledge selectors: All take an entry in the Scheme knowledge list.

(define symbol-of-scheme-knowledge (make-selector-function 1 'symbol-of-scheme-knowledge))
(define category-of-scheme-knowledge (make-selector-function 2 'category-of-scheme-knowledge))
(define essentiality-of-scheme-knowledge (make-selector-function 3 'essentiality-of-scheme-knowledge))

; piece the URL together from third and fourth component of entry, or return #f if they are missing
(define (url-of-scheme-knowledge entry)
  (if (and rs4r-url-prefix (>= (length entry) 4))
      (string-append rs4r-url-prefix "r4rs_" (as-string (fourth entry)) ".htm#" (fifth entry))
      #f))



; ---------------------------------------------------------------------------------------------------

;;; Scheme dependent elucidator.

;; Decorate the Scheme source-file with anchors and links.
;; Source-path is the name of the file with the Scheme source text (full path and extension).
;; Destination-path is the name of the html file with where the decorated Scheme source is to be written (full path and extension).
;; Source-list is the list of, read Scheme expressions on source-file.
;; Defined-names is a list of name-definitions to which we link applied names.
;; A name-definition is a list of the form (name . source-key), where source-key identifies the
;; source file, in which name is a defining name occurence
;; Documented names is a list of name descriptors, which are documented in the elucidated program.
;; In this context, a name descriptor is a pair of the form (documented-name documentation-id).
;; documented-name is a program name which occurs (in curly brackets) in the documentation.
;; documentation-id is the id of the subsection, in which the name occurs.
(define (elucidate-program-source source-path destination-path source-list defined-names documented-names size source-key source-group)
 (let ((of destination-path))
  (if (file-exists? of) (delete-file of))
  (let ((ip (open-input-file source-path))
        (op (open-output-file of)))
    (write-string-to-port
      (pre-page (string-append "Source file") (color-of-group source-group) black black black)
      op)
    (write-string-to-port (con (start-tag "font" 'size (if (eq? size 'small) 2 3)) (start-tag "pre")) op)
    (elucidate-program-source-1 ip op source-list defined-names documented-names size source-key (length source-list))
    (write-string-to-port (con (end-tag "pre") (end-tag "font")) op)
    (write-string-to-port (vertical-space end-file-empty-lines) op)
    (write-string-to-port
      (post-page)
      op)
    (close-input-port ip)
    (close-output-port op) )))

(define (add-file-extension f ext)
  (string-append f "." ext))

; source-length is the length of source-list
(define (elucidate-program-source-1 ip op source-list defined-names documented-names size source-key source-length)
 (set! last-define-a-name #f)
 (skip-white-space ip op)
 (if (not (eof-object? (peek-char ip)))
     (let ((form (car source-list))
           (next-form (if (> source-length 1) (cadr source-list) #f)))
       (elucidate-program-form ip op form next-form defined-names documented-names size source-key #t)
       (elucidate-program-source-1 ip op (cdr source-list) defined-names documented-names size source-key (- source-length 1)))
     ))

; The name of the definition, in which we currently are located.
(define enclosing-definition-name #f)

; The name of the last definition from which an anchor name has been written to the output port
; Set imperatively by elucidate-program-form
(define last-define-a-name #f)

; The central elucidation function. ip and op are input and output ports. 
; f is the form to be elucidated.  nf is the next form, or #f if no such form exist,
; or of the next form is unknown (not important for the processing).
; defined-names and documented-names are lists. 
; size is 'small or 'large.
; Source-key is the source-key of the file, we are elucidating.
; The optional parameter trailing-parenthesis applies only for pairs; It controls whether to match a trailing parenthesis.
; The value is no in case we deal with elements in a proper list.
(define (elucidate-program-form ip op f nf defined-names documented-names size source-key at-top? . optional-parameter-list)
 (let ((trailing-parenthesis (optional-parameter 1 optional-parameter-list 'yes)))
  (cond ((null? f)
          (if (eq? trailing-parenthesis 'yes)
              (begin
                (skip-white-space ip op)
                (match-start-parenthesis ip op)
                (skip-white-space ip op)
                (match-end-parenthesis ip op)
                (skip-white-space ip op)
               )))

        ((quote-in-input? ip f)
          (begin
           (write-char #\' op)
           (elucidate-program-form ip op (cadr f) #f '() documented-names size source-key #f)  ; Passing empty defined-list to prevent linking from quoted form
           (skip-white-space ip op)))

        ; We need to pass defined-names to the recursive call of elucidate-program-form, but 
        ; linkings to defined-names should only be in effect within unquotings. This is not yet made.
        ((backquote-in-input? ip f) ; @d
          (begin
           (write-char #\` op)
           (elucidate-program-form ip op (cadr f) #f defined-names documented-names size source-key #f)
           (skip-white-space ip op)))

        ((unquote-in-input? ip f)   ; @e handles both unquote and and unquote-splicing
          (begin
           (write-char #\, op)
           (let ((ch (peek-char ip)))
             (if (eqv? #\@ ch)
                 (begin
                   (read-char ip)
                   (write-char #\@ op)
                   (elucidate-program-form ip op (cadr f) #f defined-names documented-names size source-key #f)
                   )
                 (begin
		   (elucidate-program-form ip op (cadr f) #f defined-names documented-names size source-key #f)
                 )))
           (skip-white-space ip op)
          )
        )
         
        ((eof-object? f) ; nothing
        )

        ((symbol? f) (match-symbol f ip op defined-names size) 
                     (skip-white-space ip op))

        ((string? f) (match-string f ip op)
                     (skip-white-space ip op))

        ((number? f) (match-number f ip op)
                     (skip-white-space ip op))

        ((char? f) (match-char f ip op)
                   (skip-white-space ip op))

        ((boolean? f) (match-boolean f ip op)
                      (skip-white-space ip op))

        ((syntactical-comment? f)  ; @h
           (let ((sectional-comment (section-name-comment? (comment-string-of-syntactical-comment f))))
            (if sectional-comment 
                (set! enclosing-definition-name sectional-comment))
            (match-syntactical-comment-without-output ip)
            (read-char ip)  ; eats the empty after each syntactical comment
                            ; compensates for this accedential (and wrong) behaviour of lexical-to-syntactical-comments! in SchemeDoc

            (if sectional-comment 
               (write-string-to-port (con (total-doc-navigator (as-symbol sectional-comment) documented-names size source-key) (br)) op)) 

            ; write anchor name of next defined form before the rendering of the comment
            (if (is-define-form? nf)
                (let ((def-name (defined-name nf)))
                  (write-string-to-port (a-name (as-string def-name)) op)
                  (set! last-define-a-name def-name)))

            ; render the comment
            (write-string-to-port (render-syntactical-comment (comment-string-of-syntactical-comment f) (comment-level-of-syntactical-comment f)) op)
;            (write-string-to-port "CCC" op)
           )
        )

        ((is-define-form? f) ;@a
         (let* ((bn (bounded-names f))
                (reduced-defined-names (list-difference-2 defined-names bn)))
           (set! enclosing-definition-name (defined-name f)) ;@b
           (skip-white-space ip op)
           (if (not (eq? last-define-a-name (defined-name f)))  ; in case there was no comment before the define form
               (write-string-to-port (a-name (as-string (defined-name f))) op))
           (set! last-define-a-name #f) ; forget about the last written anchor name
           (if at-top?
	       (write-string-to-port (con (total-doc-navigator (defined-name f) documented-names size source-key) (br)) op))
           (match-start-parenthesis ip op)
           (skip-white-space ip op)
           (match-symbol (car f) ip op '() size)
           (skip-white-space ip op)

           (write-string-to-port (con (start-tag "b") (start-tag "font" 'color (rgb-string-list defined-color))) op)
           ; make sure that only the next form (no comments) is matched here:
           (elucidate-restricted-define-form ip op (cadr f) size)
           (write-string-to-port (con (end-tag "font") (end-tag "b")) op)

           (skip-white-space ip op)
           (for-each 
              (lambda (sf nf) 
                 (skip-white-space ip op)
                 (elucidate-program-form 
                     ip op sf nf 
                     reduced-defined-names ;@c
                     documented-names size source-key #f))
              (cddr f)
              (if (null? (cddr f)) '() (append (cdddr f) (list #f)))  ; next forms, of same length as (cddr f) because of trailing #f
           )
           (skip-white-space ip op)
           (match-end-parenthesis ip op)
           (skip-white-space ip op)))

        ((pair? f) ; @h
          (let* ((bn (bounded-names f))
                 (reduced-defined-names (list-difference-2 defined-names bn))
                )
            (skip-white-space ip op)
            
            (if (eq? trailing-parenthesis 'yes) (match-start-parenthesis ip op))

            ; Process (car f)
            (elucidate-program-form
               ip op (car f) #f   ; no next form passed - is that safe?
               reduced-defined-names documented-names size source-key #f)
            (skip-white-space ip op)

            ; Process (cdr f)
            (let ((next-ch (peek-char ip)))
              (if (eqv? next-ch #\.)  ; full dot notation, perhaps improper list
                  (begin
                    ; handle the dot on input and look what is next
                    (read-char ip)
                    (let ((next-ch (peek-char ip)))
                      (if (white-space? next-ch) 

                          (begin                         ; dot notation
                           (write-char #\. op)  
                           (skip-white-space ip op)
                           (elucidate-program-form
			    ip op (cdr f) #f 
			    reduced-defined-names documented-names size source-key #f))

                          (begin                         ; initial dot in lexeme, such as .56 or ...

                            ; do not write the dot.
                            ; The subsequent (read ip) reads the rest of the lexeme, but is actually not used.
                            ; The textual rendering in the elucidator is controlled by (cdr f)

                            (elucidate-program-form
			     ip op (cdr f) #f   
			     reduced-defined-names documented-names size source-key #f 'no)
                          )
                       )
                      )
                  )
                  (let ((rest (cdr f)))
                    (elucidate-program-form
                      ip op rest #f   
                      reduced-defined-names documented-names size source-key #f 'no)
                  )
               )
            )

            (skip-white-space ip op)
            (if (eq? trailing-parenthesis 'yes) (match-end-parenthesis ip op))
            (skip-white-space ip op)
           )
        )

        ((vector? f) ; @i
          (let* ((lf (vector->list f))
                 (bn (bounded-names f))
                 (reduced-defined-names (list-difference-2 defined-names bn)))
           (match-number-sign ip op)
;           (skip-white-space ip op)
           (match-start-parenthesis ip op)
           (for-each 
              (lambda (sf nf)
                 (skip-white-space ip op)
                 (elucidate-program-form 
                       ip op sf nf
                       reduced-defined-names 
                       documented-names
                       size source-key #f))
              lf
              (if (null? lf) '() (append (cdr lf) (list #f)))  ; next forms, of same length as f because of trailing #f
           )
           (skip-white-space ip op)
           (match-end-parenthesis ip op)
           (skip-white-space ip op))
        )

        (else (error (string-append "elucidate-program-form: unknown kind of expression" (as-string f))))))
  )

; Not used - experimental.
(define (dot-notation-ahead? ip)
 (let ((ch1 (peek-char ip)))
  (if (eqv? ch1 #\.)
      (let ((ch2 (peek-char ip)))  ; WRONG - only look ahead of length one
         (white-space? ch2)
      )
      #f)))
      


; A specialized procdures which reads through a syntactical comment on ip without
; outputting anyting on op
(define (match-syntactical-comment-without-output ip)
  (read-char ip)  ;  read start-parenthesis
  (read ip)       ;  read comment symbol
  (read ip)       ;  read comment level
  (read ip)       ;  read comment string 
  (read-char ip)       ;  read end-parenthesis which follows right next to the string
)

; ---------------------------------------------------------------------------------------------------
; Processing of a syntactical comment string via a state machine.

; An internal variable in which we register whether the comment string parameter of
; render-syntactical-comment is considered a sectional-comment.
(define indeed-section-comment #f)

; Render comment-string, at comment-level in the HTML program synthesis.
; This is the 'main function' for these purposes which uses a lot of helping functions, and a state
; machine.
(define (render-syntactical-comment comment-string comment-level)
 (let* ((sectional-comment (section-name-comment? comment-string))
        (decorate-comment   ; the function with wich the outermost comment decoration is performed
          (lambda (comment-rendering)
            (cond ((and sectional-comment (= comment-level 1))
                       (html:b (font-color (make-color 49 72 0) comment-rendering) 'style "{background-color: rgb(255,255,0);}")) ; yellow background
                   ((and sectional-comment (= comment-level 2))
                       (html:b (font-color (make-color 49 72 0) comment-rendering) 'style "{background-color: rgb(255,255,0);}"))  ; yellow background
                   ((and sectional-comment (>= comment-level 3))
                       (html:b (font-color (make-color 49 72 0) comment-rendering) 'style "{background-color: rgb(255,255,0);}"))  ; yellow background
                  (else              (font-color comment-color comment-rendering))
            )
          )
        )
       )
   (set! indeed-section-comment sectional-comment)
   (let ((comment-string-1 (strip-trailing-characters (list #\newline #\return) comment-string)))
    (set! state-list '())
    (decorate-comment
      (string-append 
       (make-string comment-level #\;)    ; initial comment characters 
       " "
       (do-render-syntactical-comment 
          comment-string-1 comment-level 0 (string-length comment-string-1)
          comment-output-string 0 comment-max-length 'normal ""))))))

; The maximum length of a comment (meassured in characters) which can be
; handled by the elucidator
(define comment-max-length 10000)

; The string in which the output of the rendering is placed. Reused from rendering to rendering.
(define comment-output-string (make-string comment-max-length #\space))

(define debugging-syntactical-comment-rendering #f)
(define state-list '())

(define (do-render-syntactical-comment c-str c-lev inptr inlength outstr outptr outlength current-state collected-str)
  (if (>= outptr (- outlength 500))
      (error "do-render-syntactical-comment: Close to output string overflow. Make comment-max-length larger"))
  (if (= inptr inlength)
      (string-append 
        (substring outstr 0 outptr)
        (cond ((and (eq? current-state 'source-char) (> (string-length collected-str) 0))  ; pending source marker on end at line
                    (render-source-char collected-str))
              (else ""))
      )
      (let* ((inch (string-ref c-str inptr))
             (trans-res (syntactical-comment-transition current-state inch collected-str c-lev))
             (next-state (car trans-res))
             (toput (as-string (cadr trans-res)))
             (collected-str (caddr trans-res))
            )
       (if debugging-syntactical-comment-rendering
            (set! state-list (cons (list (as-string inch) next-state collected-str) state-list)))
       (put-into-string! outstr outptr toput)
       (do-render-syntactical-comment c-str c-lev (+ 1 inptr) inlength outstr  (+ outptr (string-length toput))
                                        outlength next-state collected-str)
  )))


(define sectional-comment-char #\:)
(define sectional-comment-char-string (as-string sectional-comment-char))
(define elucidator-marker-char-string (as-string elucidator-marker-char))

; A simple version of html-protect which only work and single character strings.
(define (hp single-string-char)
 (cond ((equal? single-string-char "<") "&lt;")
       ((equal? single-string-char ">") "&gt;")
       (else single-string-char)))

(define (syntactical-comment-transition in-state ch collected-str c-level)
 (let ((char (as-string ch))
       (expl (string-append "A link to a program source marker in " (as-string previous-strong-program-word))))
   (cond 
        ((and (symbol? in-state) (eq? in-state 'normal))
            (cond ((equal? char sectional-comment-char-string)            (list 'colon-initial-1 "" ""))
                  ((equal? char elucidator-marker-char-string)             (list 'at-sign "" ""))
                  ((equal? char (as-string #\newline))                     (list 'newline "" ""))
;                  ((equal? char (as-string #\<))                           (list 'normal "&lt;" ""))
;                  ((equal? char (as-string #\>))                           (list 'normal "&gt;" ""))
                  (else                                                    (list 'normal (hp char) collected-str))))

        ((and (symbol? in-state) (eq? in-state 'colon-initial-1))
            (cond ((equal? char sectional-comment-char-string)             (list 'colon-initial-2 "" ""))
                  ((equal? char elucidator-marker-char-string)             (list 'at-sign (as-string sectional-comment-char) ""))
                  ((equal? char (as-string #\newline))                     (list 'newline
                                                                                 (string-append (as-string sectional-comment-char)) ""))
                  (else                                                    (list 'normal (string-append (as-string sectional-comment-char) (hp char)) 
                                                                                 collected-str))))

        ((and (symbol? in-state) (eq? in-state 'colon-initial-2))
            (cond ((equal? char sectional-comment-char-string)             (error "syntactical-comment-transition: more than two colons not allowed"))
                  ((equal? char elucidator-marker-char-string)             (error "syntactical-comment-transition: @ in section name not allowed"))
                  ((equal? char (as-string #\newline))                     (error "syntactical-comment-transition: newline not allowed in section name"))
                  (else                                                    (list 'within-section-name "" 
                                                                                 (string-append collected-str char)))))

        ((and (symbol? in-state) (eq? in-state 'within-section-name))
            (cond ((equal? char sectional-comment-char-string)             (list 'colon-after-1 "" collected-str))
                  ((equal? char elucidator-marker-char-string)             (error "syntactical-comment-transition: @ in section name not allowed"))
                  ((equal? char (as-string #\newline))                     (error "syntactical-comment-transition: newline not allowed in section name"))
                  (else                                                    (list 'within-section-name "" 
                                                                                 (string-append collected-str char)))))

        ((and (symbol? in-state) (eq? in-state 'colon-after-1))
            (cond ((equal? char sectional-comment-char-string)             (list 'normal (render-sectional-comment collected-str) ""))
                  ((equal? char elucidator-marker-char-string)             (error "syntactical-comment-transition: @ in section name not allowed"))
                  ((equal? char (as-string #\newline))                     (error "syntactical-comment-transition: newline not allowed in section name"))
                  (else                                                    (list 'within-section-name "" 
                                                                                 (string-append collected-str sectional-comment-char-string char)))))

        ((and (symbol? in-state) (eq? in-state 'colon-after-2))  ; blind
            (cond ((equal? char sectional-comment-char-string)             (error "syntactical-comment-transition: three colons not allowed"))
                  ((equal? char elucidator-marker-char-string)             (list 'at-sign (render-sectional-comment collected-str) ""))
                  ((equal? char (as-string #\newline))                     (list 'newline (render-sectional-comment collected-str) ""))
                  (else                                                    (list 'normal (string-append (render-sectional-comment collected-str) (hp char)) ""))))

        ((and (symbol? in-state) (eq? in-state 'at-sign))
            (cond ((equal? char sectional-comment-char-string)             (error "syntactical-comment-transition: colon after source mark char not allowed"))
                  ((equal? char elucidator-marker-char-string)             (error "syntactical-comment-transition: double @ not allowed"))
                  ((equal? char (as-string #\newline))                     (error "syntactical-comment-transition: newline after @ not allowed"))
                  (else                                                    (list 'source-char "" char))))

        ((and (symbol? in-state) (eq? in-state 'source-char))
            (cond ((equal? char sectional-comment-char-string)             (list 'colon-initial-1 elucidator-marker-char-string ""))
                  ((equal? char elucidator-marker-char-string)             (list 'at-sign elucidator-marker-char-string ""))
                  ((equal? char (as-string #\space))                       (list 'normal
                                                                                 (string-append (render-source-char collected-str) " ") ""))
                  ((equal? char (as-string #\return))                      (list 'source-char "" collected-str))  ; just eat the return - char 13
                  ((equal? char (as-string #\newline))                     (list 'newline 
                                                                                 (render-source-char collected-str) ""))
                  (else                                                    (list 'normal (string-append elucidator-marker-char-string (hp char)) ""))))

        ((and (symbol? in-state) (eq? in-state 'space-after-source-char))  ;blind
            (cond ((equal? char sectional-comment-char-string)             (list 'colon-initial-1 (render-source-char collected-str) ""))
                  ((equal? char elucidator-marker-char-string)             (list 'at-sign (render-source-char collected-str) ""))
                  (else                                                    (list 'normal (string-append (render-source-char collected-str) (hp char)) ""))))

        ((and (symbol? in-state) (eq? in-state 'newline))
            (cond ((equal? char sectional-comment-char-string)             (list 'colon-initial-1 (comment-glyph c-level) ""))
                  ((equal? char elucidator-marker-char-string)             (list 'at-sign (comment-glyph c-level) ""))
                  ((equal? char (as-string #\space))                       (list 'newline-and-spaces "" char))
                  ((equal? char (as-string #\newline))                     (list 'newline (comment-glyph c-level) ""))
                  (else                                                    (list 'normal (string-append (comment-glyph c-level) " " (hp char)) ""))))

        ((and (symbol? in-state) (eq? in-state 'newline-and-spaces))
            (cond ((equal? char (as-string #\space))                       (list 'newline-and-spaces "" (string-append collected-str char)))
                  ((equal? char sectional-comment-char-string)             (list 'colon-initial-1 (string-append (comment-glyph c-level collected-str)) ""))
                  ((equal? char elucidator-marker-char-string)             (list 'at-sign (string-append (comment-glyph c-level collected-str)) ""))
                  ((equal? char (as-string #\newline))                     (list 'newline (string-append (comment-glyph c-level collected-str)) ""))
                  (else                                                    (list 'normal (string-append (comment-glyph c-level collected-str) " " (hp char)) ""))))

        (else                                                              (error (string-append 
                                                                                   "syntactical-comment-transition error: unknown state "
                                                                                   (as-string in-state)))
        )

  )))

(define (comment-glyph comment-level . in-between-newline-and-semicolon)
 (let ((in-between (if (null? in-between-newline-and-semicolon) #f (car in-between-newline-and-semicolon))))
  (string-append 
    (as-string #\newline)
    (if in-between in-between "")
    (make-string comment-level #\;)
  )))

(define (render-sectional-comment section-name)
  (if indeed-section-comment
      (begin
       (set! indeed-section-comment #f)  ; such that no other section names in this comment are rendered as sectioin comments
       (string-append
       (a-name section-name)
       (if show-sectional-comment-name
           (b (font-color red section-name))
           ""))
      )
      (string-append 
       (as-string sectional-comment-char) (as-string sectional-comment-char)
       section-name
       (as-string sectional-comment-char) (as-string sectional-comment-char))))
      

(define (render-source-char source-char-string)
  (string-append 
   (a-name 
    (string-append
     (as-string enclosing-definition-name) ;@i
     "-@" source-char-string)) 
   (doc-source-marker-link              ;@a
    documentation-source-marker-occurences
    source-char-string
    enclosing-definition-name)
  ))


; End state machine and processing of syntactical comment string
; ---------------------------------------------------------------------------------------------------
  

; Return a link to the documentation frame. NOT USED.
(define (doc-navigator name documented-names)
  (let ((res (assq name documented-names)) )
    (if res
        (let* ((res-docid (cadr res))
               (weak-strong (caddr res))
               (res-doc-title (cdr (assq res-docid documentation-key-title-alist)))
              )
          (con (a-tag-target (string-append "documentation.html" "#" (as-string res-docid))
                             (cond ((eq? strong-weak 'strong) (image "doc-left.gif" title))
                                   ((eq? strong-weak 'weak) (image "doc-left-weak.gif" title))
                                   (else (error "doc-link: problems determining strong or weak documentation link")))
                             "documentation-frame"
               )
               (br)))
        "")))


(define (total-doc-navigator name documented-names size source-key)
  (let* ((doc-entries (filter (lambda (e) (eq? name (car e))) documented-names))
         (reversed-doc-entries (reverse doc-entries))
         (unique-reversed-doc-entries-0
           (remove-duplicates-by-predicate
             reversed-doc-entries
             (lambda (x y) (and (eq? (cadr x) (cadr y)) (eq? (caddr x) (caddr y))) )))
         (unique-reversed-doc-entries (remove-redundant-weak-entries unique-reversed-doc-entries-0))
        )
   (con   ;@a
    (if (or make-large-source-files? (eq? default-program-font-size 'large))
     (con
      (if (eq? size 'small) 
          (a-tag (string-append source-key "-LARGE" ".html" "#" (as-string name)) (image "small-square.gif" "Show source file in large font"))
          (a-tag (string-append source-key ".html" "#" (as-string name)) (image "small-square.gif" "Show source file in small font")))
      (horizontal-space 1)
     )
     "")

    (if link-definitions-to-cross-reference-index?
     (let* ((name-string (as-string name))
            (name-first-letter (as-string (string-ref name-string 0))))
      (con
       (a-tag-target
         (if alphabetic-cross-reference-index?
             (string-append "cross-reference-index" "-" (hygienic-file-character (downcase-string name-first-letter)) ".html" "#" name-string)
             (string-append "cross-reference-index" ".html" "#" name-string))
         (image "small-green-up-triangle.gif" 
                (string-append "In " source-key ": " "Link from " name-string " to it's cross reference table entry"))
         "control-frame")
       (horizontal-space 1)
      ))
     "")
    

    (if (not (null? unique-reversed-doc-entries))
        (string-merge
           (map 
             (lambda (de)
               (let* ((doc-id (cadr de))
                      (strong-weak (caddr de))
                      (number (cdr (assq doc-id documentation-key-numbering-alist)))
                      (doc-entry-title (cdr (assq doc-id documentation-key-title-alist))))
                 (doc-link name doc-id (string-append number ". " doc-entry-title) strong-weak))
             )
             unique-reversed-doc-entries)
           (make-list (- (length unique-reversed-doc-entries) 1) (horizontal-space 1)))
        ""))))


(define (remove-redundant-weak-entries entries)
  ; Entries is a subset of documented-name-occurenes. In this function we remove possible weak entries
  ; for which also a strong entry exist in the list of entries.
 (remove-redundant-weak-entries-1 entries entries '()))

(define (remove-redundant-weak-entries-1 all-entries entries res)
  (letrec ((redundant-weak-entry? 
            (lambda (e1 e2) 
               (and (not (equal? e1 e2)) (eq? 'weak (caddr e1)) (eq? (cadr e1) (cadr e2))))))
   (cond ((null? entries) (reverse res))
         ((member-by-predicate (car entries) all-entries redundant-weak-entry?)
            (remove-redundant-weak-entries-1 all-entries (cdr entries) res))
         (else (remove-redundant-weak-entries-1 all-entries (cdr entries) (cons (car entries) res))))))
             

(define (documentation-url doc-id)
  (string-append "documentation.html" "#" (as-string doc-id)))

; Return a link to the documentation frame given name (a name in the program frame)
; a doc-id (the identification of a section or unit in the documentation frame) and
; title (the title of the section or unit in the documentation frame).
; strong-weak is a symbol (strong or weak) which tells whether to insert a strong or a weak documentation reference
(define (doc-link name doc-id title strong-weak)
  (a-tag-target 
    (documentation-url doc-id) 
    (cond ((eq? strong-weak 'strong) (image "doc-left.gif" title))
          ((eq? strong-weak 'weak) (image "doc-left-weak.gif" title))
          (else (error "doc-link: problems determining strong or weak documentation link")))
    "documentation-frame"))


; A specialized version of list-difference, where the first parameter is a list of pairs (name . key),
; and the second parameter is a simple list of names
; Returns a list of pairs (a subset of defined-name-pairs).
(define (list-difference-2 defined-name-pairs bounded-names)
  (list-difference-3 defined-name-pairs bounded-names '()))

(define (list-difference-3 lst1 lst2 res)
  (cond ((null? lst1) (reverse res))
        ((memq (caar lst1) lst2) (list-difference-3 (cdr lst1) lst2 res))
        (else (list-difference-3 (cdr lst1) lst2 (cons (car lst1) res)))))

; match the cadr symbol of a define form f, without matching trailing comments
(define (elucidate-restricted-define-form ip op f size)
  (cond ((symbol? f) (match-simple-symbol f ip op))
        ((list? f) 
           (match-start-parenthesis ip op)
           (for-each 
              (lambda (sf)
                 (skip-white-space ip op)
                 (elucidate-restricted-define-form ip op sf size))
              f)
           (skip-white-space ip op)
           (match-end-parenthesis ip op))
        ((pair? f)
           (let ((p1 (proper-part f))
                 (p2 (first-improper-part f)))
            (skip-white-space ip op)
            (match-start-parenthesis ip op)
            (for-each 
              (lambda (sf)
                 (skip-white-space ip op)              
                 (elucidate-restricted-define-form ip op sf size))
              p1)
            (skip-white-space ip op)
            (match-dot ip op)
            (skip-white-space ip op)
            (elucidate-restricted-define-form ip op p2 size)
            (skip-white-space ip op)
            (match-end-parenthesis ip op)
          ))))


(define (quote-in-input? ip form)
  (let ((ch (peek-char ip)))
    (if (and (eqv? #\' ch)  (and (list? form) (> (length form) 1) (eq? (car form) 'quote)) )
        (begin 
          (read-char ip)
          #t)
        #f)))

(define (backquote-in-input? ip form)
  (let ((ch (peek-char ip)))
    (if (and (eqv? #\` ch) (and (list? form) (> (length form) 1) (eq? (car form) 'quasiquote)))
        (begin 
          (read-char ip)
          #t)
        #f)))

(define (unquote-in-input? ip form)
  (let ((ch (peek-char ip)))
    (if (and (eqv? #\, ch) (and (list? form) (> (length form) 1) (or (eq? (car form) 'unquote) (eq? (car form) 'unquote-splicing))))
        (begin 
          (read-char ip)
          #t)
        #f)))


; defined names is a list of (name . source-key) elements
(define (match-symbol sym ip op defined-names size)
  (read ip)
  (let* ((source-key (name-memq sym defined-names))
         (sym-string (as-string sym))
         (protected-symbol-string (html-protect sym-string))
         (symbol (as-symbol protected-symbol-string))  
         (size-string (if (eq? size 'large) "-LARGE" ""))
        )
    (if source-key
        (write-string-to-port 
         (html:a
          (font-color applied-color protected-symbol-string)   ; before 01.17.00: symbol
          'href (string-append source-key size-string ".html" "#" (as-string sym))
          'title source-key
          'style (if underline-program-links "{text-decoration: underline;}" "{text-decoration: none;}")
         )
         op)
         (let ((entry (scheme-knowledge-entry sym)))
          (if entry
              (let ((url (url-of-scheme-knowledge entry)))
               (if url
                   (write-string-to-port 
                    (html:a
                      ((scheme-syntax-procedure-decorate entry) protected-symbol-string)
                      'href url
                      'style (if underline-program-links "{text-decoration: underline;}" "{text-decoration: none;}")
                      'target manual-frame-from-program
                      'title "R4RS Scheme form"
                    )
                    op)
                   (write-string-to-port 
                    ((scheme-syntax-procedure-decorate entry) protected-symbol-string)
                    op)))
              (let ((man-entry (find-manual-entry sym-string)))
                (if man-entry
                    (let ((url (string-append (cdr man-entry) "#" sym-string)))
                     (write-string-to-port 
		      (html:a
		       (font-color manual-name-color protected-symbol-string)
		       'href url
		       'style (if underline-program-links "{text-decoration: underline;}" "{text-decoration: none;}")
		       'target manual-frame-from-program
                       'title (string-append "Manual: " (file-name-proper url)) 
                      )
		      op))
                    (display protected-symbol-string op))))))))

(define (match-simple-symbol sym ip op)
  (read ip)
  (let* ((sym-string (as-string sym))
         (protected-symbol-string (html-protect sym-string))
         (symbol (as-symbol protected-symbol-string))  
        )
    (display protected-symbol-string op)))


;; The parameter entry is a an entry from scheme-syntax-procedure-list.
;; Return the a one-argument procedure, with which to decorate a kind symbol in the program presentation.
(define (scheme-syntax-procedure-decorate entry)
  (cond ((eq? 'syntax (category-of-scheme-knowledge entry)) b)
        ((eq? 'procedure (category-of-scheme-knowledge entry)) brown-normal)
        (else id-1)))

(define (brown-normal txt)
  (font-color r4rs-scheme-color (if black-and-white-coloring (em txt) txt)))

; The identify function
(define (id-1 x) x)

; Return an entry in scheme-syntax-procedure-list, if symbol is found in that list, or else #f
(define (scheme-knowledge-entry symbol)
  (scheme-knowledge-entry-1 symbol scheme-syntax-procedure-list))

(define (scheme-knowledge-entry-1 symbol lst)
  (cond ((null? lst) #f)
        ((eq? (symbol-of-scheme-knowledge (car lst)) symbol)
           (car lst))
        (else (scheme-knowledge-entry-1 symbol (cdr lst)))))

; Return the source-key component of the matching sym in defined-names.
; defined names i a list of pairs, where each pair is of the form (name . source-key).
; If no match, return #f
(define (name-memq sym defined-names)
  (cond ((null? defined-names) #f)
        ((eq? sym (caar defined-names)) (cdar defined-names))
        (else (name-memq sym (cdr defined-names)))))

; Return an entry in manual-name-file-map matching sym.
; Returns #f if not found
; Name is supposed to be a string.
(define (find-manual-entry name)
 (find-in-list
   (lambda (name-file-pair)
     (equal? (car name-file-pair) name))
   manual-name-file-map))
      


(define (match-string str ip op)
  (read ip)
  (write (html-protect str) op))

(define (match-char ch ip op)
  (read ip)
  (write ch op))

(define (match-number n ip op)
  (read ip)
  (write n op))

(define (match-boolean b ip op)
  (read ip)
  (write b op))

(define (match-start-parenthesis ip op)
  (let ((ch (read-char ip)))
    (if (or (eqv? ch #\() (eqv? ch #\[)) 
	(write-char ch op)
	(error (string-append "match error: start parenthesis expected:" (as-string ch))))))

(define (match-end-parenthesis ip op)
  (let ((ch (read-char ip)))
    (if (or (eqv? ch #\)) (eqv? ch #\]))
        (write-char ch op)
        (error "match error: end parenthesis expected:" (as-string ch)))))

(define (match-dot ip op)
  (let ((ch (read-char ip)))
    (if (eqv? ch #\.)
        (write-char #\. op)
        (error "match error: dot expected. Problems if we deal with unnormlized dotted forms"))))

(define (match-number-sign ip op)
  (let ((ch (read-char ip)))
    (if (eqv? ch #\#)
        (write-char #\# op)
        (error "match error: number sign expected:" (as-string ch)))))

(define (skip-white-space ip op)
  (let ((ch (peek-char ip)))
    (cond ((white-space? ch) (begin (read-char ip) (write-char ch op) (skip-white-space ip op)))
          ((comment-begin? ch) (begin  (skip-comment ip op) (skip-white-space ip op)))
          (else 'empty))))

(define (white-space? ch)
 (if (eof-object? ch)
     #f
    (let ((n (char->integer ch)))
      (or (eqv? n 32) (eqv? n 9) (eqv? n 10) (eqv? n 12) (eqv? n 13)))))

(define (comment-begin? ch)
  (eqv? #\; ch))

(define (skip-comment ip op)
  ; skip rest of line.
  (write-string-to-port (start-tag "font" 'color (rgb-string-list comment-color)) op)
  (skip-comment-1 ip op)
  (write-string-to-port (end-tag "font") op))


(define (report-ambiguous-doc-source-markers amb-list)
  (let ((doc-sections
          (map (lambda (pid-did-sm)
                   (let ((doc-id (cadr pid-did-sm)))
                     (cdr (assq doc-id documentation-key-numbering-alist))))
               amb-list)))
    (string-append
       CR "The relation is ambiguous." CR
       (if (= 1 (length amb-list)) "The other relevant section is " "The other relevant sections are ")
       (string-merge doc-sections (make-list (- (length amb-list) 1) ", " )))))

; Return the link (an a-tag-target) from a program source marker to the documentation.
; Issue a warning in cases of ambiguities or a non-existing relation.
(define (doc-source-marker-link documentation-source-marker-occurences mark-char enclosing-definition-name)
 (let* ((relevant-occurences 
         (filter (lambda (pid-did-sm)
                   (let ((pid (car pid-did-sm))
                         (sm (caddr pid-did-sm)))
                     (and (equal? (as-string pid) (as-string enclosing-definition-name))
                          (equal? (as-string sm) (as-string mark-char)))))
                 documentation-source-marker-occurences))
        (lgt (length relevant-occurences)))

  ; possible warning side effect
  (cond ((= lgt 0) (display-warning 
                     (string-append "No corresponding source marker in the documention: Marker '" (as-string mark-char) "' in "
                                    (as-string enclosing-definition-name))))
        ((> lgt 1) (display-warning 
                    (string-append "Ambiguous source marker '" (as-string mark-char) "' for "
                                  (as-string enclosing-definition-name) " in the documentation.  Using the first one")))
        (else "")) ; no warning 
  
  (cond ((>= lgt 1) 
          (let* ((used-occ (car relevant-occurences))
                 (doc-id (cadr used-occ))
                 (num (cdr (assq doc-id documentation-key-numbering-alist)))
                 (sec-title (cdr (assq doc-id documentation-key-title-alist)))
                 (ambiguous? (if (> lgt 1) (report-ambiguous-doc-source-markers (cdr relevant-occurences)) ""))  ;@o
                 (explanation 
                    (string-append "A linked program source marker to section " num ":"  CR (string-it-single sec-title)  ambiguous? CR
                                   "Mark char: " (as-string mark-char) ))
                )
            (a-tag-target (string-append "documentation.html" "#" (as-string doc-id) "-" "@" (as-string mark-char))
                          (source-marker-image mark-char explanation)
                          "documentation-frame")))
        (else (source-marker-image mark-char "A program source marker WITHOUT a link to the documentation")))))
  
(define (skip-comment-1 ip op)
  ; skip rest of line.
  (let ((ch (read-char ip)))
    (cond ((eof-object? ch) #f)  ; do nothing.
          ((eol? ch) (write-char ch op))
          ((eqv? ch #\<) (write-string-to-port "&lt;" op) (skip-comment-1 ip op))
          ((eqv? ch #\>) (write-string-to-port "&gt;" op) (skip-comment-1 ip op))
          ((eqv? ch elucidator-marker-char)  
             (let ((source-marker-char (read-char ip))  ; assume not eof
                   (next-char (read-char ip))           ; assume not eof
                  )
               (if (is-white-space? next-char)
                   (write-string-to-port
                       (string-append 
                         (a-name 
                          (string-append
                            (as-string enclosing-definition-name)  ;@i
                            "-@" (as-string source-marker-char))) 
                         (doc-source-marker-link ;@a
                            documentation-source-marker-occurences
                            source-marker-char
                            enclosing-definition-name)
                         (as-string next-char)) 
                       op)
                   (write-string-to-port
                       (string-append (as-string elucidator-marker-char) (as-string source-marker-char) (as-string next-char))
                       op))
               (skip-comment-1 ip op)))
          (else (begin (write-char ch op) (skip-comment-1 ip op))))))

(define (eol? ch)
  (eqv? ch #\newline))  


; ---------------------------------------------------------------------------------------------------
; Handy test procedure of the Scheme elucidator. OBSOLETE.
(define (lucid file)
  (let* ((source-list (read-source (add-file-extension file "scm")))
         (defining-names (defined-names source-list)))
    (elucidate-program-source (string-append file ".scm") (string-append file ".html") source-list defining-names '())))

; ---------------------------------------------------------------------------------------------------



; Overall frame setup in terms of the control, documentation, and program file names.
; Directory prefix is added in front of all three frames of the elucidator.
(define (elucidator-frame control-filename program-menu-filename documentation-filename program-filename directory-prefix)
  (letrec ((frame-file (lambda (f) (string-append directory-prefix (add-file-extension f "html"))))
           (sized-frame-file 
            (lambda (f) (frame-file (string-append f (if (eq? default-program-font-size 'large) "-LARGE" "")))))
          )
   (html:frameset 
    (con (control-frame control-filename program-menu-filename directory-prefix) 
         (html:frameset
            (con (html:frame "" 'name "documentation-frame" 'src (frame-file documentation-filename) 'scrolling "yes")
                 (html:frame "" 'name "program-frame" 'src (sized-frame-file program-filename) 'scrolling "yes")
            )
            'cols "50%,50%" 'border 5 'bordercolor (rgb-string-list black))
    )
    'rows (string-append (as-string control-frame-pixel-height) ",*")  'border 5 'bordercolor (rgb-string-list black)))
)

(define (elucidator-frame-horizontal control-filename program-menu-filename documentation-filename program-filename directory-prefix)
  (letrec ((frame-file (lambda (f) (string-append directory-prefix (add-file-extension f "html"))))
           (sized-frame-file 
            (lambda (f) (frame-file (string-append f (if (eq? default-program-font-size 'large) "-LARGE" "")))))
          )
   (html:frameset
    (con (control-frame control-filename program-menu-filename directory-prefix)
         (html:frame "" 'name "documentation-frame" 'src (frame-file documentation-filename) 'scrolling "yes")
         (html:frame "" 'name "program-frame" 'src (sized-frame-file program-filename) 'scrolling "yes")
    )
    'rows (string-append (as-string control-frame-pixel-height) ",360,*") 'border 5 'bordercolor (rgb-string-list black)))
)

; Return a control frame or frameset. 
(define (control-frame control-filename program-menu-filename directory-prefix)
 (letrec ((frame-file (lambda (f) (string-append directory-prefix (add-file-extension f "html")))))
  (if separate-program-menu?
      (html:frameset
            (con (html:frame "" 'name "control-frame" 'src (frame-file control-filename) 'scrolling "auto")
                 (html:frame "" 'name "program-menu"  'src (frame-file program-menu-filename) 'scrolling "auto")
            )
            'cols "*,240" 'border 1 'bordercolor (rgb-string-list black))
      (html:frame "" 'name "control-frame" 'src (frame-file control-filename) 'scrolling "auto"))))


(define (make-frame-file-in-html-dir title frames filename)
  (write-text-file
    (html:html
      (con (html:head 
             (html:title documentation-title))
           frames))
    (html-destination filename)))

(define (make-frame-file-in-source-dir title frames filename)
  (write-text-file
    (html:html
      (con (html:head 
             (html:title documentation-title))
           frames))
    (source-destination filename)))

; Return the body of the documentation page. 
; This function uses the global variables, such as documentation-abstract and documentation-elements.
(define (documentation-contents)
  (con
    (a-name "START")
    (h 1 (font-color blue (guard-text documentation-title)))
    (present-author-info (map guard-text (list documentation-author documentation-email documentation-affiliation))) (p)
    (present-abstract (guard-text documentation-abstract)) (vertical-space 1)

    (accumulate-right
       string-append 
       ""
      (map present-documentation-element documentation-elements))

    (vertical-space end-file-empty-lines)
  ))

(define (present-documentation-element doc-el)
  (let ((kind (get-value 'kind doc-el)))
    (cond ((eq? kind 'section) (present-documentation-section doc-el))
          ((eq? kind 'entry) (present-documentation-entry doc-el))
          (else (error "present-documentation-element: unknown kind of documentation element")))))

(define section-number 0)
(define subsection-number 0)

(define (section-numbering)
  (string-append (as-string section-number)))

(define (subsection-numbering)
  (string-append (as-string section-number) "." (as-string subsection-number)))

(define (present-documentation-section doc-el)
 (let* ((title (get-value 'title doc-el))
        (section-numbering (get-value 'numbering doc-el))
        (section-number (car (get-value 'raw-numbering doc-el))) ; an integer
        (title-1 (con section-numbering (horizontal-space 2) title))
        (intro (get-value 'intro doc-el))
        (id (get-value 'id doc-el))
        (hidden-id-pres (font 2 documentation-entry-color (as-string id)))
        (subsection-elements (filter (subsections? section-number) documentation-elements))
       )
   (con
    (a-name (internal-reference id))
      (con
         (color-frame-width
            (con (section-navigation-banner doc-el) (horizontal-space 1) (if present-hidden-ids? hidden-id-pres "") (br)  ;@i
                 (b (con-space (font-size 5 title-1) )) (br)
                 (do-program-link-documentation intro id)
                 )
            documentation-section-color
            "1200") 
          (indent-pixels 10 (brl (map present-documentation-subsection-element subsection-elements)))
      )
    (vertical-space 1))))


(define (present-documentation-entry doc-el)
 (let* ((title (get-value 'title doc-el))
        (entry-numbering (get-value 'numbering doc-el))
        (title-1 (con entry-numbering (horizontal-space 2) title))
        (body (get-value 'body doc-el))
        (id (get-value 'id doc-el))
        (hidden-id-pres (font 2 documentation-entry-color (as-string id)))
       )
    (con 
      (a-name (internal-reference id))
      (color-frame-width (con-space (section-navigation-banner doc-el) (if present-hidden-ids? hidden-id-pres "") (br)  ;@i
                                    (b (font-size 4 title-1))
                                    )
                         documentation-entry-color "1200")
      (do-program-link-documentation body id)
      (vertical-space 2))))

; return a predicate which return #t on entries in section n
(define (subsections? n)
  (lambda (doc-el)
    (let ((kind (get-value 'kind doc-el))
          (raw-num (get-value 'raw-numbering doc-el)))
      (and (eq? kind 'entry) (eqv? n (car raw-num))))))

; return a more general predicate which returns #t on entry n.m
; n.0 means section n
(define (section-subsection? n m)
  (lambda (doc-el)
    (let ((raw-num (get-value 'raw-numbering doc-el)))
      (and (eqv? n (car raw-num)) (eqv? m (cadr raw-num))))))

(define (present-author-info au)
 (let ((au1 (if (not (null? au)) (cons (copyright-owner (car au)) (cdr au)) au)))
   (h 3
      (con 
       (apply con
              (map (lambda (e) (con e (horizontal-space 4))) au1))
       ))))

(define (present-abstract abstr)
 (let ((width 1200))
   (con (color-frame-width (em (con (b "Abstract. ") abstr)) grey2 width) (p)) ))

(define (guard-text str)
  (if str str "???"))


(define (make-source-program-file source-key source-group source-file language source-list defining-names documented-names size)
  (elucidate-program-source
    source-file
    (string-append (html-directory) source-key (if (eq? size 'large) "-LARGE" "") ".html")
    source-list
    defining-names
    documented-names
    size
    source-key
    source-group))

; ---------------------------------------------------------------------------------------------------

; transform words surrounded by curly brackets (or more correctly, p-link-prefix-char and p-link-suffix-char)
; to links to one of the source programs. Use the information in the global variable
; defining-name-occurences to do so.

(define linking-output-factor 10)

(define (do-program-link-documentation str doc-id)
 (let* ((strlgt (string-length str))
        (outmax (+ 900 (* linking-output-factor strlgt)))
        (res-str (make-string outmax #\space))   ; estimate - perhaps not enough
       )
  (set! state-list '())
  (do-program-link-documentation-1 doc-id str 0 strlgt res-str 0 outmax 'normal-text "")))


(define state-list '()) ; for debugging purposes
(define debugging-program-linking #f)


(define (do-program-link-documentation-1 doc-id instr inptr inlength outstr outptr outlength current-state collected-word)
  (if (>= outptr (- outlength 500))
      (error "do-program-link-documentation-1: Close to output string overflow. Make linking-output-factor larger"))
  (if (= inptr inlength)
      (substring outstr 0 outptr)
      (let* ((inch (string-ref instr inptr))
             (trans-res (program-linking-transition current-state inch collected-word doc-id))
             (next-state (car trans-res))
             (toput (as-string (cadr trans-res)))
             (collected-word (caddr trans-res))
            )
       (if debugging-program-linking
            (set! state-list (cons (list (as-string inch) next-state collected-word) state-list)))
       (put-into-string! outstr outptr toput)
       (do-program-link-documentation-1 doc-id instr (+ 1 inptr) inlength outstr  (+ outptr (string-length toput))
                                        outlength next-state collected-word)
  )))


; STATES 
; normal-text:          We are outside a name from which to link
; inside-marker:        We have just seen a program source mark
; end-marker:           About to output marker or mark literal
; inside-p-link-word:   We are inside a word from which to link to program
; entering-p-link-word
; leaving-p-link-word
; inside-d-link-word:   We are inside a word from which to link to another section in the documentation
; entering-d-link-word
; leaving-d-link-word

(define (program-linking-transition in-state ch collected-word doc-id)
 (let ((char (as-string ch))
       (expl (string-append "A link to a program source marker in " (as-string previous-strong-program-word))))
   (cond ((and (symbol? in-state) (eq? in-state 'normal-text))
            (cond ((equal? char p-link-prefix-char)                        (list 'entering-p-link-word "" collected-word))
                  ((equal? char d-link-prefix-char)                        (list 'entering-d-link-word "" collected-word))
                  ((equal? char p-link-suffix-char)  (display-warning "Misplaced end-of-link char") 
                                                                         (list 'normal-text "" collected-word))
                  ((equal? char elucidator-marker-char-string)           (list 'inside-marker "" ""))
                  ((equal? char elucidator-escape-char-string)           (list 'normal-text-escape "" collected-word))
                  (else                                                  (list 'normal-text char collected-word))))

         ((and (symbol? in-state) (eq? in-state 'inside-marker))        ; char identifies the marker
            (cond ((or (equal? char p-link-suffix-char) (equal? char p-link-prefix-char)
                       (equal? char d-link-prefix-char) (equal? char d-link-suffix-char))
                                                     (display-warning "Unexpected marker char")
                                                                         (list 'normal-text (string-append elucidator-marker-char-string char)  collected-word))

                  (else                                                  (list 'normal-text
                                                                                (begin
                                                                                  (source-mark-register previous-strong-program-word doc-id char)
                                                                                  (con (source-mark-anchor (source-marker-glyph char expl) char)   ;@a
                                                                                       (a-name (string-append (as-string doc-id) "-" "@" (as-string char)))))
                                                                                collected-word))
            ))

         ((and (symbol? in-state) (eq? in-state 'normal-text-escape)) 
            (cond (else                                                  (list 'normal-text char collected-word))))


         ((and (symbol? in-state) (eq? in-state 'entering-p-link-word))
            (cond ((equal? char p-link-suffix-char)  (display-warning "Empty link word")
                                                                         (list 'leaving-p-link-word ""  collected-word))
                  ((equal? char p-link-prefix-char)  (display-warning "Misplaced begin-of-link char") 
                                                                         (list 'inside-p-link-word "" collected-word))
                  ((or (equal? char d-link-prefix-char) (equal? char d-link-prefix-char))  (display-warning "Misplaced documentation link char") 
                                                                         (list 'inside-p-link-word "" collected-word))
                  (else                                                  (list 'inside-p-link-word "" char))))

         ((and (symbol? in-state) (eq? in-state 'entering-d-link-word))
            (cond ((equal? char d-link-suffix-char)  (display-warning "Empty link word")
                                                                         (list 'leaving-d-link-word ""  collected-word))
                  ((equal? char d-link-prefix-char)  (display-warning "Misplaced begin-of-link char") 
                                                                         (list 'inside-d-link-word "" collected-word))
                  ((or (equal? char p-link-prefix-char) (equal? char p-link-prefix-char))  (display-warning "Misplaced program link char") 
                                                                         (list 'inside-d-link-word "" collected-word))
                  (else                                                  (list 'inside-d-link-word "" char))))

         ((and (symbol? in-state) (eq? in-state 'inside-p-link-word))
            (cond ((equal? char p-link-suffix-char)                        (list 'leaving-p-link-word (linking-from-doc-to-prog collected-word doc-id) ""))
                  ((equal? char p-link-prefix-char)  (display-warning "Misplaced begin-of-link prog char") 
                                                                         (list 'inside-p-link-word "" collected-word))
                  ((or (equal? char d-link-prefix-char) (equal? char d-link-prefix-char))  (display-warning "Misplaced documentation link char") 
                                                                         (list 'inside-p-link-word "" collected-word))
                  (else                                                  (list 'inside-p-link-word "" (string-append collected-word char)))))

         ((and (symbol? in-state) (eq? in-state 'inside-d-link-word))
            (cond ((equal? char d-link-suffix-char)                        (list 'leaving-d-link-word (linking-from-doc-to-doc collected-word doc-id) ""))
                  ((equal? char d-link-prefix-char)  (display-warning "Misplaced begin-of-link doc char") 
                                                                         (list 'inside-d-link-word "" collected-word))
                  ((or (equal? char p-link-prefix-char) (equal? char p-link-prefix-char))  (display-warning "Misplaced program link char") 
                                                                         (list 'inside-d-link-word "" collected-word))
                  (else                                                  (list 'inside-d-link-word "" (string-append collected-word char)))))

         ((and (symbol? in-state) (eq? in-state 'leaving-p-link-word))
            (cond ((equal? char p-link-suffix-char)  (display-warning "Misplaced end-of-link prog char")
                                                                         (list 'leaving-p-link-word "" collected-word))
                  ((equal? char p-link-prefix-char)                      (list 'inside-p-link-word "" collected-word))  ; ??
                  ((equal? char d-link-prefix-char)                      (list 'inside-d-link-word "" collected-word))  ; ?? 
                  (else                                                  (list 'normal-text char collected-word))))

         ((and (symbol? in-state) (eq? in-state 'leaving-d-link-word))
            (cond ((equal? char d-link-suffix-char)  (display-warning "Misplaced end-of-link doc char")
                                                                         (list 'leaving-p-link-word "" collected-word))
                  ((equal? char p-link-prefix-char)                      (list 'inside-p-link-word "" collected-word))  ; ??
                  ((equal? char d-link-prefix-char)                      (list 'inside-d-link-word "" collected-word))  ; ?? 
                  (else                                                  (list 'normal-text char collected-word))))

         (else                                                           (error "program-linking-transition error: unknown state"))

  )))

; add an entry to the variable documentation-source-marker-occurences
(define (source-mark-register previous-strong-program-word doc-id char)
  (set! documentation-source-marker-occurences
     (cons (list previous-strong-program-word doc-id char)
           documentation-source-marker-occurences)))
                                                                                        
;; This function is called during the traversal of a documentation body.
;; It returns the a-tag'ed and fonted link word, which links to another place in the documentation
(define (linking-from-doc-to-doc collected-word doc-id)
  (let* ((ass-number (assq (as-symbol collected-word) documentation-key-numbering-alist))
         (ass-title (assq (as-symbol collected-word) documentation-key-title-alist))
         (ref-number (if ass-number (cdr ass-number) #f))
         (ref-title (if ass-title (cdr ass-title) #f))  ; the title of the section referred
         (url (if ref-number (string-append "documentation.html" "#" collected-word) #f)))
   (if url 
       (html:a
          (font-color documentation-documentation-link-color ref-number)
          'href url
          'style "{text-decoration: underline;}"
          'target "documentation-frame"
          'title (if ref-title ref-title ""))
    (begin
      (display-warning (string-append "Cannot find a linking target of the documentation linking word: " collected-word))
      collected-word))))

; previous strong word relation in the documentation
(define previous-strong-program-word #f)

;; This function is called during the traversal of a documentation body.
;; It returns the a-tag'ed and fonted link word.
;; As a side-effect, it collects the documented names in the list documented-name-occurences.
(define (linking-from-doc-to-prog word doc-id)
  (let* ((kind (kind-of-program-link? word))
         (qualification (qualified-program-link? word))
         (word-1 (proper-linking-word word qualification))
         (strong? (eq? kind 'strong))
         (strong-weak-symbol (if strong? 'strong 'weak))
         (link-targets (filter (lambda (dno) (equal? word-1 (as-string (car dno)))) defining-name-occurences))
         (size-string (if (eq? default-program-font-size 'large) "-LARGE" ""))
        )
       
     (cond ((eq? kind 'none)   ; no linking, only fonting
              (font-color none-reference-color (kbd word-1)))

           ((and (empty-string? word-1) (not (empty-string? qualification)))  ; link to file as such
              (let ((source-key qualification))
                 (html:a
                   (font-color (if strong? documentation-program-link-color documentation-program-link-color-weak)  (kbd (b source-key)))
                   'href (string-append source-key size-string ".html")
                   'target "program-frame"
                   'title (string-append "Link to program file: " source-key)
                   'style (if underline-documentation-links "{text-decoration: underline;}" "{text-decoration: none;}")
                 )
              ))

           ((= (length link-targets) 0)
             (let ((man-entry (find-manual-entry word-1))
                  )
	       (if man-entry  ; attempt linking to a manual entry - disregarding qualification
		   (let ((url (string-append (cdr man-entry) "#" word-1)))
		      (html:a
		       (font-color manual-name-color word-1)
		       'href url
		       'style (if underline-program-links "{text-decoration: underline;}" "{text-decoration: none;}")
		       'target manual-frame-from-documentation
                       'title (string-append "Manual: " (file-name-proper url)) 
		       ))
		   (begin 
                     (display-warning (string-append "Documentation to program linking: Cannot find linking target of " word-1))
                     word-1)))
              )

           ((= (length link-targets) 1)
              (let ((source-key (cdr (car link-targets))))
                 (if strong? (set! previous-strong-program-word word-1))  ;@i
                 (set! documented-name-occurences (cons (list (as-symbol word-1) doc-id strong-weak-symbol) documented-name-occurences))
                 (if (and qualification (not (equal? qualification source-key)))
                     (display-warning (string-append "Disregarding the qualification of " word)))
                 (html:a
                   (font-color (if strong? documentation-program-link-color documentation-program-link-color-weak)  (if (and strong? black-and-white-coloring) (b (kbd word-1)) (kbd word-1)))
                   'href (string-append source-key size-string ".html" "#" word-1)
                   'target "program-frame"
                   'title source-key
                   'style (if underline-documentation-links "{text-decoration: underline;}" "{text-decoration: none;}")
                 )
              ))

           ((and (> (length link-targets) 1) qualification)  ; @a 
              (let* ((possible-source-keys (map cdr link-targets))
                     (qualification-ok (member qualification possible-source-keys))
                     (source-key (if qualification-ok qualification (cdr (car link-targets)))))
                 (if strong? (set! previous-strong-program-word word-1))  
                 (set! documented-name-occurences (cons (list (as-symbol word-1) doc-id strong-weak-symbol) documented-name-occurences))

                 (if (not qualification-ok)
                     (display-warning (string-append "Illegal qualification in " word ". Using that in " source-key)))

                 (html:a
                   (font-color (if strong? documentation-program-link-color documentation-program-link-color-weak) (kbd word-1))
                   'href (string-append source-key size-string ".html" "#" word-1)
                   'target "program-frame"
                   'title source-key
                   'style (if underline-documentation-links "{text-decoration: underline;}" "{text-decoration: none;}")
                 )
              ))

           ((> (length link-targets) 1)
              (let ((source-key (cdr (car link-targets))))
                 (if strong? (set! previous-strong-program-word word-1))  ;@h
                 (set! documented-name-occurences (cons (list (as-symbol word-1) doc-id strong-weak-symbol) documented-name-occurences))

                 (display-warning (string-append "Multiple targets of the program reference " word-1 
                                                 ". Consider a qualification. " "Using that in " source-key))
                 (html:a
                   (font-color (if strong? documentation-program-link-color documentation-program-link-color-weak) (kbd word-1))
                   'href (string-append source-key size-string ".html" "#" word-1)
                   'target "program-frame"
                   'title source-key
                   'style (if underline-documentation-links "{text-decoration: underline;}" "{text-decoration: none;}")
                 )
              ))
     )))

; Return the qualification (source key) of word, or #f if there
; is no qualification. The parameter word is of the form 
; *qual$ref, qual$ref, or just ref (loosely speaking).
; A qualification must match one of the source
; keys in source-key-list to qualify.
(define (qualified-program-link? word)
  (let ((end-qual (find-in-string word #\$)))
    (if end-qual
        (let* ((init-char (string-ref word 0))
               (start-qual 
                (cond ((eqv? init-char strong-link-char) 1)
                      ((eqv? init-char weak-link-char) 1)
                      ((eqv? init-char none-link-char) 1)
                      (else 0)))
               (candidate-qual (substring word start-qual end-qual)))
          (if (member candidate-qual source-key-list)
              candidate-qual
              #f))
        #f)))

; Return the proper linking word from word (without qualification and kind-information).
(define (proper-linking-word word qualification)
  (if qualification
      (substring word (+ 1 (find-in-string word #\$)) (string-length word))
      (let* ((init-char (string-ref word 0))
             (start (cond ((eqv? init-char strong-link-char) 1)
                          ((eqv? init-char weak-link-char) 1)
                          ((eqv? init-char none-link-char) 1)
                          (else 0))))
        (substring word start (string-length word)))))

         

; does the program link word start with a strong-link-char
(define (strong-program-link? word)
  (if (>= (string-length word) 1) 
      (eqv? (string-ref word 0) strong-link-char)
      #f))

; Return a symbol which classifies the linking word.
; Possible results are the symbols strong, weak, none.
; In case word does not begin with either strong-link-char, weak-link-char, or none-link-char
; the value of default-program-link is used.
; If word is empty, return #f
(define (kind-of-program-link? word)
  (if (>= (string-length word) 1)
      (let ((ch (string-ref word 0)))
        (cond ((eqv? ch strong-link-char) 'strong)
              ((eqv? ch weak-link-char) 'weak)
              ((eqv? ch none-link-char) 'none)
              (else default-program-link)))
      #f))

; Disregard the initial star of star-word. Assume that there is an initial star in star-word
(define (linking-word-of-strong-link star-word)
  (substring star-word 1 (string-length star-word)))

; Happens to be identical to linking-word-of-strong-link
(define (linking-word-of-other-link link-word)
  (substring link-word 1 (string-length link-word)))


         
; ---------------------------------------------------------------------------------------------------

; Return a list of duplicates in name-def-list.
; Name-def-list is a list of name-entries. A name entry is of the 
; form (name-symbol source-key-string).

(define (duplicated-definitions name-def-list)
  (let* ((sorted-names (sort-list name-def-list name-entry-leq?))
         (paired-names (if (null? sorted-names) '() (pair-up sorted-names (cdr sorted-names))))
         (filtered-pairs (filter (lambda (p) (eq? (car (car p)) (car (cdr p)))) paired-names))
         (duplicate-names (map caar filtered-pairs)))
    (filter (lambda (ne) (memq (car ne) duplicate-names)) sorted-names)))


(define (present-duplicated-definitions)
  (let ((dd (duplicated-definitions defining-name-occurences)))
    (con
      (indent-pixels 10 
        (multi-column-list 4 (map present-a-duplicate dd) browser-pixel-width)) 

      (font-size 1 (em "Navigation to duplicates in the same source file is not supported")))))

; Present a single duplicate. d is a pair of (name . source-key)
(define (present-a-duplicate d)
 (con
  (a-tag-target
      (string-append (cdr d) ".html" "#" (as-string (car d)))
      (font-size 2 (con (as-string (car d))))
      "program-frame")
  (font-size 2 (con " in file " (cdr d)))))
      

(define (name-entry-leq? x y)
  (string<=?  (as-string (car x))  (as-string (car y))))



; ---------------------------------------------------------------------------------------------------

;; Index support: total index of all defining name occurences.

(define (present-defined-name-index sorted-defining-name-occurences)
  (con
      (indent-pixels 10 
        (multi-column-list 6
           (map present-a-defining-name-entry sorted-defining-name-occurences) browser-pixel-width)) 
  ))


(define (present-a-defining-name-entry d)
 (let ((sourcefile (cdr d))
       (size-string (if (eq? default-program-font-size 'large) "-LARGE" ""))
      )
   (html:a
       (font 2 defined-color (con (html-protect (as-string (car d)))))
       'href (string-append sourcefile size-string ".html" "#" (as-string (car d)))
       'target "program-frame"
       'title sourcefile)))



; ---------------------------------------------------------------------------------------------------

;; Index support: cross references involving both applied and defining name occurences

(define (applied-names-multiple-sources source-list-list)
  (sort-list
   (accumulate-right
     append
     '()
     (map applied-names source-list-list))
   name-entry-leq?))

(define (applied-names source-list)
  (applied-names-1 source-list '()))

(define (applied-names-1 source-list res)
  (cond ((null? source-list) res)

        ((is-define-form? (car source-list)) 
           (let* ((define-form (car source-list))
                  (def-name (defined-name define-form))
                  (this-contribution (map (lambda (appl-name) (cons appl-name def-name)) (applied-names-one-form define-form))))
             (applied-names-1 (cdr source-list) (append this-contribution res))))

        (else (applied-names-1 (cdr source-list) res)) ;drop (car source-list) because it is a non-define form
))

(define (applied-names-one-form f)
 (cond ((eof-object? f)                 ; nothing
        )
       ((symbol? f) (if (defining-in-batch? f) (list f) '()))
       ((string? f) '())
       ((number? f) '())
       ((char? f) '())
       ((boolean? f) '())
       ((vector? f) (applied-names-one-form (vector->list f)))
       ((and (list? f) (null? f)) '())

       ; special processing of forms with defining names:
       ((and (list? f) (function-define-form? f)) (applied-names-one-form (cdddr f)))
       ((and (list? f) (is-define-form? f)) (applied-names-one-form (cddr f)))
       ((and (list? f) (lambda-form? f)) (applied-names-one-form (cddr f)))
       ((and (list? f) (let-form? f)) (append (applied-names-one-form (let-vals f)) (applied-names-one-form (cddr f))))

       ((list? f) (append (applied-names-one-form (car f)) (applied-names-one-form (cdr f))))
       ((pair? f)                       ; improper list 
        (let ((p1 (proper-part f))
              (p2 (first-improper-part f)))
          (append (applied-names-one-form p1) (applied-names-one-form p2))
        ))
       (else (error (string-append "applied-names-one-form: unknown kind of expression" (as-string f))))))

(define (defining-in-batch? name)
  (if (assq name defining-name-occurences) #t #f))

(define (function-define-form? x)
  (and (list? x) 
       (> (length x) 2)
       (eq? (car x) 'define)
       (symbol? (cadr x))
       (pair? (caddr x))))

(define (lambda-form? x)
  (and (list? x) 
       (> (length x) 2)
       (eq? (car x) 'lambda)))

(define (let-form? x)
  (and (list? x) 
       (> (length x) 2)
       (or (eq? (car x) 'let) (eq? (car x) 'let*) (eq? (car x) 'letrec))))

; Return a list of expressions bound to names in let-form
(define (let-vals let-form)
  (let ((binding-forms (if (named-let? let-form)
                           (caddr let-form)
                           (cadr let-form))))
    (accumulate-right append '() (map cdr binding-forms))))

; Return a list of names bound in let-form
(define (let-names let-form)
  (let ((binding-forms (if (named-let? let-form)
                           (caddr let-form)
                           (cadr let-form))))
    (accumulate-right append '() (map (lambda (b) (list (car b))) binding-forms))))

(define (named-let? let-form)
  (symbol? (cadr let-form)))

; Return the formal parameter names of a lamba construct. Always returns a list.
; The paramter lambda-form must be a lambda expression.
(define (lambda-names lambda-form)
  (let ((par-list (cadr lambda-form)))
    (cond ((list? par-list) par-list)
          ((symbol? par-list) (list par-list))
          ((pair? par-list) (append (proper-part par-list) (list (first-improper-part par-list))))
          (error "lambda name: unknown kind of the lambda form's parameter list"))))

; ---------------------------------------------------------------------------------------------------
; Presentation of cross references

(define (present-cross-reference-index appl-def-name-list-1)
 (let* ((appl-def-name-sublisted ; @a
           (sublist-by-predicate appl-def-name-list-1 (lambda (x y n) (not (eq? (car x) (car y))))))
        (appl-def-name-sublisted-1 ; @b
           (map (lambda (sublist) 
                   (remove-duplicates-by-predicate
                     sublist
                     (lambda (x y) (eq? (cdr x) (cdr y)))))
           appl-def-name-sublisted))
       )
  (indent-pixels 5 
    (table-3 0 
             (list 200 1000)
             (map present-applied-sublist appl-def-name-sublisted-1)))))


(define (present-applied-sublist sl)
  (let* ((sorted-sl (sort-list sl (lambda (x y) (string<=? (as-string (cdr x)) (as-string (cdr y))))))
         (appl-name (car (car sl)))     ; take the first element of an arbitrary entry, the first
         (def-table 
           (multi-column-list 5
                              (map present-defined-entry sorted-sl) (- browser-pixel-width 200)))
         (sourcefile (source-key-of-defining-name appl-name))
         (size-string (if (eq? default-program-font-size 'large) "-LARGE" ""))
        )
   (list (con 
          (a-name (html-protect (as-string appl-name))) ; name this entry in the cross reference index, allows direct access to entry from program
          (box                           ; box it in order to allign with def-table
           (html:a
            (b (font 2 defined-color (html-protect (as-string appl-name))))
            'href (string-append sourcefile size-string ".html" "#" (as-string appl-name))
            'target "program-frame"
            'title sourcefile)))
          def-table)))


(define (present-defined-entry appl-def-entry)
 (let* ((appl-name (car appl-def-entry))
        (def-name (cdr appl-def-entry))
        (sourcefile (source-key-of-defining-name def-name))
        (size-string (if (eq? default-program-font-size 'large) "-LARGE" ""))
       )
   (if def-name
     (html:a
       (font-size 2 (con (html-protect (as-string def-name))))
       'href (string-append sourcefile size-string ".html" "#" (as-string def-name))
       'target "program-frame"
       'title sourcefile)
     (font-size 2 (em "not used")))))


; lookup the source key (file name information) of the name in defining-name-occurences.
(define (source-key-of-defining-name name)
 (let ((res (filter (lambda (dno) (eq? name (car dno)))  defining-name-occurences)))
   (cond ((= (length res) 0) "??")            ; question mark, leading to undefined link
         ((= (length res) 1) (cdr (car res))) ; the normal case
         ((> (length res) 1) (cdr (car res))) ; we take the first
   )))

; Merge the def-applied-list and the def-list to a single list.
; Both def-applied-list and the result are alist with entries of the form (name . name-of-definition).
; def-list is just a list of pairs of the form (name . #f) reflecting all the definitions in the documentation bundle.
; The resulting list is also sorted.
(define (merge-defined-and-defined-applied-lists def-applied-list def-list)
   (merge-defined-and-defined-applied-lists-1 def-applied-list def-list '()))

; The special purpose asymmetric merge used above.
(define (merge-defined-and-defined-applied-lists-1 lst1 lst2 res)
 (letrec ((lt-cars? (lambda (x y) (string<? (as-string (car x)) (as-string (car y)))))
          (eq-cars? (lambda (x y) (eq? (car x) (car y))))
         )
  (cond ((and (null? lst1) (null? lst2)) (reverse res))
        ((null? lst1) (append (reverse res) lst2))
        ((null? lst2) (append (reverse res) lst1))
        ((eq-cars? (car lst1) (car lst2)) ; normal case
           (merge-defined-and-defined-applied-lists-1 (cdr lst1) (cdr lst2) (cons (car lst1) res)))
        ((lt-cars? (car lst1) (car lst2)) ; should not happen
           (merge-defined-and-defined-applied-lists-1 (cdr lst1) lst2 (cons (car lst1) res)))
        ((lt-cars? (car lst2) (car lst1)) ; if there is a defined name which is not applied
           (merge-defined-and-defined-applied-lists-1 lst1 (cdr lst2) (cons (car lst2) res)))
        (else (error "merge-defined-and-defined-applied-lists-1: should not happen!")))))
        
; Present the doc-elements in a two column list. If kind is 'detail show toc entries for both
; sections and entries. If kind is 'overall only show sections.
(define (present-documentation-contents doc-elements kind)
  (let ((doc-elements-1
          (cond ((eq? kind 'detail) doc-elements)
                ((eq? kind 'overall) (filter (lambda (e) (eq? (get-value 'kind e) 'section)) doc-elements)))))
  (n-column-list 
    (if (eq? kind 'detail) toc-columns-detail toc-columns-overall)
    (map present-documentation-content-element doc-elements-1)
     browser-pixel-width)))

(define (present-documentation-content-element element)
  (let ((kind (get-value 'kind element))
        (doc-id (get-value 'id element))
        (n (get-value 'numbering element))
        (ttl (get-value 'title element)))
   (font-size 2
    (con 
     (cond ((eq? kind 'entry) (horizontal-space 4))
           ((eq? kind 'section)  "")
           (else (error "present-documentation-content-element: unknown kind of documentation element")))
     n (horizontal-space 2)
     (a-tag-target (string-append "documentation.html" "#" (as-string doc-id))
                       ttl
                       "documentation-frame"
                       )))))

; Return a string which represents an entry in a local table of contents
; within a documentation section
(define (present-documentation-subsection-element element)
  (let ((doc-id (get-value 'id element))
        (n (get-value 'numbering element))
        (ttl (get-value 'title element)))
   (font-size 2
    (con 
     n (horizontal-space 2)
     (a-tag-target (string-append "documentation.html" "#" (as-string doc-id))
                       (font-color black ttl)
                       "documentation-frame"
                       )))))



; ---------------------------------------------------------------------------------------------------
;;; Support of a simple, line-base documentation text format. 
;;; This is an alternative to the lisp format based on documentation-entry and documentation-section
;;; forms.

; The format is

; .SECTION eee
; .TITLE ttt
; .BODY
; Section text
; More section text
; .END
; -----------------------------------------------------------------------------
; 
; .ENTRY eee
; .TITLE ttt
; .BODY
; Entry text
; More entry text
; .END
; -----------------------------------------------------------------------------

; A line starting with -- is a comment
; The lines formed of dashes are comments, and thus ignored.


;; Extract and parse documentation from a simple text file.
;; Translate to the documentation-section and -entry forms, and evaluate these.
;; Thus, a documentation-from clause is equivalent to the sequence of documentation-section
;; and documentation-entry forms represented by the clause
;; file is just a name without prefix path. The file is opened in the source-directory
(define (documentation-from file)
   (display-message (string-append "Parsing the textual documentation file"))
   (reset-collection)
   (let* ((ip (open-input-file (string-append source-directory file))))
      (documentation-intro-from-port ip)
      (documentation-units-from-port ip)
      (close-input-port ip)))

(define (documentation-intro-from-port ip)
  (let* ((skip1 (skip-while white-space-or-separator? ip))
         (intro (accept-documentation-intro ip)))
    (define-documentation-intro! intro)))


(define (documentation-units-from-port ip)
 (let* ((skip1 (skip-while white-space-or-separator? ip))
        (unit (accept-documentation-unit ip))
        (separator-skip (skip-while white-space-or-separator? ip))
       )
    (if (unit-ok? unit)
        (define-unit! unit)
        (error (string-append "documentation-units-from-port: Malformed documentation unit: " (as-string unit))))
    (if (not (eof-object? next-doc-char))
        (documentation-units-from-port ip))))

(define (unit-ok? unit)
  #t)

; not used
(define unit-list '())

(define (define-unit! unit)
  (let ((doc-form (make-documentation-form unit)))
    (set! unit-list (cons doc-form unit-list)) 
    (eval-cur-env doc-form)))

(define (define-documentation-intro! intro-list)
 (documentation-intro 
    (first intro-list) (second intro-list) (third intro-list)
    (fourth intro-list) (fifth intro-list)))

; Transform a unit, as extracted from the documentation, to a documentation-entry or documentation-section Lisp form
(define (make-documentation-form unit)
  (let* ((kind-string (car (car unit)))
         (kind (cond ((equal? kind-string ".ENTRY") 'documentation-entry)
                     ((equal? kind-string ".SECTION") 'documentation-section)
                     (else (error "make-documentation-form: Unknown documentation kind"))))
         (id (as-symbol (cadr (car unit))))
         (title (cadr unit))
         (body (caddr unit)))
   (list kind
         (list 'id (list 'quote id))
         (list 'title title)
         (if (eq? kind 'documentation-entry) (list 'body body) (list 'intro body)))))
        

(define (accept-documentation-unit ip)
 (let* ((id   (accept-doc-id ip))
        (ttl  (accept-doc-title ip))
        (bd   (accept-doc-body ip)))
    (list id ttl bd)))

(define (accept-documentation-intro ip)  ; title, author, email, affiliation, and abstract
 (let* ((ttl    (accept-doc-title ip))
        (aut    (accept-doc-author ip))
        (email  (accept-doc-email ip))
        (af     (accept-doc-affiliation ip))
        (abstr  (accept-doc-abstract ip))
       )
    (list ttl aut email af abstr)))



(define (accept-doc-author ip)
  (let* ((keyword (collect-until is-white-space? ip))
         (res (doc-check (equal? keyword ".AUTHOR") ".AUTHOR expected"))
         (skip1 (skip-while is-white-space? ip))
         (res (collect-until end-of-line? ip))
         (skip2 (skip-while is-white-space? ip)))
   res))

(define (accept-doc-email ip)
  (let* ((keyword (collect-until is-white-space? ip))
         (res (doc-check (equal? keyword ".EMAIL") ".EMAIL expected"))
         (skip1 (skip-while is-white-space? ip))
         (res (collect-until end-of-line? ip))
         (skip2 (skip-while is-white-space? ip)))
   res))

(define (accept-doc-affiliation ip)
  (let* ((keyword (collect-until is-white-space? ip))
         (res (doc-check (equal? keyword ".AFFILIATION") ".AFFILIATION expected"))
         (skip1 (skip-while is-white-space? ip))
         (res (collect-until end-of-line? ip))
         (skip2 (skip-while is-white-space? ip)))
   res))

(define (accept-doc-abstract ip)
  (let* ((keyword (collect-until is-white-space? ip))
         (res (doc-check (equal? keyword ".ABSTRACT") ".ABSTRACT expected"))
         (skip1 (skip-while is-white-space? ip))
         (body (accept-body-text ip)))
   body))


; assume the next char is the dot in ENTRY or SECTION
(define (accept-doc-id ip)
  (let* ((unit (collect-until is-white-space? ip))
         (res (doc-check (or (equal? unit ".ENTRY") (equal? unit ".SECTION")) ".ENTRY or .SECTION expected"))
         (skip1 (skip-while is-white-space? ip))
         (id (collect-until is-white-space? ip))
         (skip2 (skip-while is-white-space? ip)))
    (list unit id)))

(define (accept-doc-title ip)
  (let* ((keyword (collect-until is-white-space? ip))
         (res (doc-check (equal? keyword ".TITLE") ".TITLE expected"))
         (skip1 (skip-while is-white-space? ip))
         (ttl (collect-until end-of-line? ip))
         (skip2 (skip-while is-white-space? ip)))
   ttl))

(define (accept-doc-body ip)
  (let* ((keyword (collect-until is-white-space? ip))
         (res (doc-check (equal? keyword ".BODY") ".BODY expected"))
         (skip1 (skip-while is-white-space? ip))
         (body (accept-body-text ip)))
   body))

(define (accept-body-text ip)
  (let* ((body-list (reverse (accept-body-text-1 ip '())))
         (cr-list (make-list (- (length body-list) 1) CR-string)))
   (string-merge 
     body-list
     cr-list)))

(define CR-string (as-string #\newline))


(define (accept-body-text-1 ip res)
 (let* ((line (collect-until end-of-line? ip))
        (skip1 (eat-eol-chars ip)))
    (cond ((end-unit? line) res)
           (else (accept-body-text-1 ip (cons line res))))))

(define (doc-check condition error-text)
  (if (not condition)
      (error (string-append "Line " (as-string doc-line-number) ": " error-text))))

(define (end-unit? line)
  (if (< (string-length line) 4)
      #f
      (equal? ".END" (substring line 0 4))))

; (define (end-unit? line)
;   (if (< (string-length line) 4)
;       #f
;       (let ((res (equal? ".END" (substring line 0 4))))
;         (if res
;             (display "end unit")
;             (display (string-append "not end unit: " line))
;         )
;         res)))
             
 


; Collection and skipping functions: Functions to read characters from an input port.
; All functions (as used above) are linebased. As such we can relatively safely assume that there is an upper
; limit on the amount of characters to be collected (although, of course, lines can be long...)

; Collection state variables and constants:
(define buffer-length 10000)
(define collection-buffer (make-string buffer-length #\space))
(define next-doc-char #f)
(define doc-line-number 1)



(define (reset-collection)
  (set! collection-buffer (make-string buffer-length #\space))
  (set! next-doc-char #f)
  (set! doc-line-number 1))

; return the string collected from the input port ip.
; collection stops when the predicate p holds holds on the character read.
; The last read character is putted back in the variable next-doc-char
(define (collect-until p ip)
  (collect-until-1 p ip collection-buffer 0)
)

(define (collect-until-1 p ip buffer next)
 (let ((ch (read-next-doc-char ip)))
   (if (or (p ch) (eof-object? ch))
       (begin
          (set! next-doc-char ch)
          (substring buffer 0 next))
       (begin 
         (string-set! buffer next ch)
         (collect-until-1 p ip buffer (+ 1 next))))))



(define (read-next-doc-char ip)
  (if next-doc-char
      (let ((res next-doc-char))
        (set! next-doc-char #f)
        res)
      (let ((ch (read-char ip)))
         (if (and (not (eof-object? ch)) (= 10 (char->integer ch))) (set! doc-line-number (+ doc-line-number 1)))
         ch)))


; skip characters on ip while p holds
(define (skip-while p ip)
 (let ((ch (read-next-doc-char ip)))
   (if (p ch)
       (skip-while p ip)
       (set! next-doc-char ch))))

; Situation: an eol character (13 (CR) on a PC) is in next-doc-char.
; Drop the buffer, and read a 10 char (LF) if it is there
; Should also work on UNIX
(define (eat-eol-chars ip)
  (let ((ch (read-char ip)))
    (cond ((eof-object? ch) (set! next-doc-char ch))        ; allow the eof condition to be rediscovered by the context
          ((= 10 (as-number ch)) (set! next-doc-char #f))  ; force real reading from from ip next time. Buffer is empty
          (else (set! next-doc-char ch)))))                ; put ch in buffer such that it will be read again
  
; not used
(define (skip-once p ip)
 (let ((ch (read-next-doc-char ip)))
   (if (p ch)
       (let ((ch (read-next-doc-char ip)))
         (set! next-doc-char ch))
       (set! next-doc-char ch))))


; Useful predicates

(define (is-white-space? ch)
  (if (eof? ch) 
      #f
      (let ((n (as-number ch)))
        (or (eqv? n 32) (eqv? n 9) (eqv? n 10) (eqv? n 12) (eqv? n 13)))))

(define (white-space-or-separator? ch)
  (if (eof? ch) 
      #f
      (or (is-white-space? ch) (eqv? #\- ch))))

(define (end-of-line? ch)
  (if (eof? ch) 
      #f
      (let ((n (as-number ch)))
         (or  (eqv? n 10) (eqv? n 13)))))

(define (eof? ch)
  (eof-object? ch))



; ---------------------------------------------------------------------------------------------------
; Procedure making the elucidator help file
(define (make-elucidator-help-page)
 (let ((kn-email "normark@cs.auc.dk")
       (kn-www "http://www.cs.auc.dk/~normark/")
      )
  (letrec ((an-entry (lambda (x y) (con (font-color red (b x)) (br) y))))
   (write-text-file
    (page
      "Elucidator help page"
      (con-par
       (h 1 (font-color blue "The Elucidator Help Page"))

       (con "The " 
       (a-tag-target "http://www.cs.auc.dk/~normark/elucidative-programming/index.html" "elucidative programming home page" "elu-home") " and the " 
       (a-tag-target "http://dopu.cs.auc.dk" "DOPU page" "dopu-home") " are the primary places to find information
        about elucidative programming.")

       (con-space "The pages shown in this browser is the result of 'elucidating' a number of programs and a documentation file.
        The main purpose is to present " (em "internal program documentation") " side by side with a number of source programs.
        The leftmost window shows the documentation, and the rightmost window one of the programs. The topmost window
        is a menu and index window, from which a number of aspects can be controlled.")

       (con-space (em "Elucidative programming") " is variant of " 
        (a-tag "http://www.loria.fr/services/tex/english/litte.html" "literate programming") ", as coined by Knuth in the early eighties.
        In most literate programming tools (called WEB tools), fragments of programs are defined inside the
        program documentation. In literate programming, a tool (called tangle) can extract and assemble the program fragments according to 
        the rules of the programming language. Another tool (called weave) formats the documentation, generates indexes,
        and presents all of it in a nice-looking paper format.")

       "The main characteristics of elucidative programming in relation to literate programming are:"

       (ol (list
          (an-entry "The program source files are not affected at all."
                 "It is not necessary to split the programs into fragments,
                  and to organize these in the context of the program explanations.
                  An existing program source file can be handled.")

          (an-entry "The program and the documentation are shown side by side."
                 "We do not go for an embedded presentation of the program inside its documentation.
                  Rather, we provide for mutual navigation
                  between program and documentation in a two-frame layout")

          (an-entry "The program units which we document, are whole abstractions."
                 "Things get simpler when we can settle on documentation of named abstractions
                  instead of arbitrary program fragments (sometimes called 'chunks' or 'scraps')")

          (an-entry "We support on-line presentation in a browser."
                 "Literate programming tools were primary oriented towards presentation of the weaved results on a static paper medium.")

          (an-entry "The elucidator tool use specific knowledge about the programming language."
                 (con "The language knowledge is used to identify the names in the program. Applied names are related to their definitions,
                  and the program is decorated with colors and extensive linking. Currently we support the programming language " 
                  (a-tag "http://www.schemers.org" "Scheme") " and Java (see " (a-tag "http://dopu.cs.auc.dk/portal.html" "the web pages about the Java elucidator") ")."
          ))

          (an-entry "Program and documentation indexes are available."
                 "A tables of contents, an index of the program definitions, and a cross reference index is available")

          (an-entry "The creation of the format, from which the elucidated information is generated,
                     is supported by a special set of editor commands."
                 "In that way it is realistic to handle the practical aspect of documenting a program while it is written")
       ))

       (con-space "A " (em "documentation bundle") " consist of a single documentation file, a number of program files, and a setup file.
        The documentation file is described in very simple, textual format, which allows the use of HTML tags for formatting.
        As mentioned above, there are no special requirements to the program files.
        The setup files is a Scheme file, which describes the the constituents of the documentation bundle
        together with a number of processing parameters.
        Running the setup file through a Scheme processor generates the HTML pages shown in this browser.")

       "The icons in the menu and index frame (at the top) are now described:"
          

       (table-3
         1
         (list 100 600)
         (list
          (map b (list "Icon" "Explanation"))
          (list (image "three-frames.gif" "")
                "Reset the elucidator to vertical layout (the default layout). All frames are reverted to the 'start position'.") 

          (list (image "three-frames-horizontal.gif" "")  
                "Reset the elucidator to a horizontal layout. This is an alternative layout in which the documentation and
                 a selected program are shown under each other, in full width")

          (list (image "index.gif" "" )
                "Presents an index of all defined names in the menu and index frame, just below the icons at the top of the window.
                The index is pr. default broken into fragments according to starting letter of the defined name.")
 
          (list (image "cross-index.gif" "" )
                "Presents a cross reference index in the menu and index frame.
                A cross reference index relates all applied names to the definition, in which they occur.
                The index is pr. default broken into fragments according to starting letter of the applied name.")
 
          (list (image "xx.gif" "") 
                "Present an index of all named defined more than once in the documentation bundle.
                This is useful information in a Lisp program")

          (list (image "overall-contents.gif" "")  "Present an overall table of contents for the documentation in the menu and index frame.
                                                    This table of contents only covers the top-level section, but no subsections.")
 
          (list (image "contents.gif" "")  "Present a table of contents for the documentation in the menu and index frame.
                                            This table of contents convers both top-level sections and subsections (also called entries).")

          (list (image "question-left-arrow.gif" "")  "Present an Elucidator help page in the documentation frame to the left")
          (list (image "question-right-arrow.gif" "") "Present an Elucidator help page in the program frame to the right") 
         )
       )

       "The icons in the rightmost group allows navigation to each of the program files in a documentation bundle."

       (con-space "From the documentation frame (to the left) it is possible to adjust the program window, such that a given piece of 
       program is shown. Similarly, from the program frame (to the right), the yellow left arrows "
       (image "doc-left.gif" "") " can be used to find the 
       section in the documentation, which " (em "explains") " the particular program unit. The light yellow arrows " (image "doc-left-weak.gif" "") " refer to a documentation section
       which " (em "mentions") " the definition (as opposed to explaining it).  We talk about strong  and weak relations between the documentation and the program resp.
       Besides these means of navigation
       it is possible to navigate inside the documentation frame, and inside the program frames.")

       (con "Inside the program and inside documentation sections you may find small color bullets like " (image "source-mark-red.gif" "") ". These are called " (em "source markers") ".
        The source markers are used
        to point out a particular place in a piece of program, which is discussed in a documentation section. You can click on a source marker in
        the documentation in order to navigate to the corresponding source marker in the program. Also navigation in the opposite direction is supported
        from most source markers. The popup text, which appears in most browsers when the cursor rests on a source marker, gives useful additional information
        about the source marker.
        Notice that a source marker in the documentation is associated with
        the closest preceding " (em "strong") " documentation-program relation.")
  
       (con-space "The source programs are, by default, shown using a fairly small font size. The small square symbols "
                  (image "small-square.gif" "") " can be used to toggle the program frames to use larger font.
                  Notice that the small square symbol is only shown in certain configurations 
       (when the variable " (kbd "make-large-source-files?") " is true or in the case that variable
        default-program-font-size is set to large)")

       (con "The icon " (image "small-green-up-triangle.gif" "") 
        " is an anchor of a link from a definition to an entry in the cross reference index. 
        This link is very convenient because it allows us to follow call chains via the cross reference index:
        Go from a definition of N to the cross reference entry N. Find via that entry a function F which calls N;
        Go the cross reference entry of F, and find a function G which calls F, etc.")

       (con-space "The elucidator is written in Scheme, using the "
                  (a-tag "http://www.cs.auc.dk/~normark/laml/" "LAML") " software packages.")

       (em "You can use the browser's back button to establish the original contents of this frame,
            or you can activate the reset elucidator icon in the top left corner
            to return to the standard layout.")


       (con-space "Kurt Nørmark" (br) "Aalborg University" (br) kn-email (br) (a-tag kn-www))

      )

      white black blue blue)
   (html-destination "elucidator-help") ))))



; ---------------------------------------------------------------------------------------------------
; Source markers

; The association between marker characters and colors
(define marker-associations
  (list 
    (list #\a "red"      '(255 0 0))
    (list #\b "green"    '(0 128 0))
    (list #\c "blue"     '(0 0 255))
    (list #\d "black"    '(0 0 0))
    (list #\e "maroon"   '(128 0 0))
    (list #\f "grey"     '(128 128 128))
    (list #\g "purple"   '(128 0 128))
    (list #\h "silver"   '(192 192 192))
    (list #\i "tetal"    '(0 128 128))
    (list #\j "aqua"     '(0 255 255))
    (list #\k "lime"     '(0 255 0))
    (list #\l "olive"    '(128 128 0))
    (list #\m "yellow"   '(255 255 0))
    (list #\n "navy"     '(0 0 128))
    (list #\o "fuchsia"  '(255 0 255))
   ))

; Return a source marker for the character ch.
; A source marker is graphical image, which identifies a particular place in a source program.
(define (source-marker-image ch explanation)
  (let* ((ch1 (as-char ch))
         (ass-res (assv ch1 marker-associations))
         (color (if ass-res (cadr ass-res) "error")))
     (image (string-append "source-mark-" color ".gif") explanation)))

; Return the source marker glyph (text or image) depending on ch (a char) and the global variable source-marker-kind 
(define (source-marker-glyph ch explanation)
  (cond ((eq? source-marker-kind 'as-text) (source-marker-text ch #f))
        ((eq? source-marker-kind 'as-colored-text) (source-marker-text ch #t))
        ((eq? source-marker-kind 'as-image) (source-marker-image ch explanation))
        (else (error (string-append 
                        "source-marker-glyph: Problems determining the kind of source marker in the documentation: "
                        (as-string source-marker))))))

; return the string "c marker" where c is a color.
; if color? then color the string by means of font-color application
(define (source-marker-text ch color?)
  (let* ((ch1 (as-char ch))
         (ass-res (assv ch1 marker-associations))
         (color (if ass-res (cadr ass-res) "??"))
         (text (string-append color " " "marker"))
         (rgb-list (if ass-res (caddr ass-res) '(0 0 0))))
    (font-color (if color? rgb-list '(0 0 0)) (b text))))

; Return an anchor tag of the glyph - typically but not necssarily
; The destination of the anchor is determined by the global variable previous-strong-program-word, 
; as encountered earlier in the documentation text.
(define (source-mark-anchor mark-glyph mark-char)
  (let ((link-targets (filter (lambda (dno) (equal? previous-strong-program-word (as-string (car dno)))) defining-name-occurences))
        (size-string (if (eq? default-program-font-size 'large) "-LARGE" ""))
       )
     (cond ((= (length link-targets) 0)
            (display-warning 
             (string-append "Linking from source marker in documentation: Cannot find linking target of " ))
            mark-glyph)
           ((= (length link-targets) 1)
            (let ((source-key (cdr (car link-targets))))
              (a-tag-target 
               (string-append source-key size-string ".html" "#" previous-strong-program-word "-@" mark-char)
               mark-glyph
               "program-frame")))
           ((>= (length link-targets) 1)
            (let ((source-key (cdr (car link-targets))))
              (display-warning (string-append "Linking from source marker in documentation: Multiple targets of " previous-strong-program-word))
              (a-tag-target 
               (string-append source-key size-string ".html" "#" previous-strong-program-word "-@" mark-char)
               mark-glyph
               "program-frame"))))))


; ---------------------------------------------------------------------------------------------------
; Making section navigation banners which allow us to navigate to parrent and sibling sections and entries.

; Return a banner which navigates to up, next and down URLs of
; doc-el, which is the elements of a section or entry.
(define (section-navigation-banner doc-el)
  (let* ((cur-nums (get-value 'raw-numbering doc-el))
         (cur-sect (car cur-nums))
         (cur-subsect (cadr cur-nums)))
    (if (= 0 cur-subsect) ; a section
        (let ((up (documentation-url "START"))
              (prev (if (= 1 cur-sect)  ; @a
                        #f
                        (doc-section-url (- cur-sect 1) 0)))
              (next (doc-section-url (+ cur-sect 1) 0)))
          (section-navigation-banner-1 doc-el up prev next))
        (let ((up (doc-section-url cur-sect 0)) ; an entry
              (prev (if (= 1 cur-subsect) ; @b
                        #f
                        (doc-section-url cur-sect (- cur-subsect 1))))
              (next (doc-section-url cur-sect (+ cur-subsect 1))))
          (section-navigation-banner-1 doc-el up prev next)))))


; Return a banner which navigates to the URLs up, prev, and next in doc-el.
; up, prev, and next may be an URL (a string) or a cons-pair of (URL . section-title), or #f.
; If one of these are #f, present a blind navigation button.
; This function handles the presentation details given the URLS passed as parameters.
(define (section-navigation-banner-1 doc-el up prev next)
 (letrec ((url-of (lambda (x) (cond ((pair? x) (car x))
                                     ((string? x) x)
                                     (else (error "url-of: unknown type of parameter")))))
           (title-of (lambda (x) (cond ((pair? x) (cdr x))
                                       ((string? x) "")
                                       (else (error "title-of: unknown type of parameter"))))))
  (con
    (if up (a-tag (url-of up) (image "small-up.gif" (title-of up))) (image "small-up-blind.gif" "")) (horizontal-space 1)
    (if prev (a-tag  (url-of prev) (image "small-prev.gif" (title-of prev))) (image "small-prev-blind.gif" "")) (horizontal-space 1)
    (if next (a-tag (url-of next) (image "small-next.gif" (title-of next))) (image "small-next-blind.gif" "")))))


; Return a cons pair of URL and section title of documentation entry n.m
; if m is 0, we mean section n.
; If no such entry/section exists, return #f
(define (doc-section-url n m)
  (let ((res (filter (section-subsection? n m) documentation-elements)))
    (cond ((= 1 (length res)) 
             (let* ((element (car res))
                    (id (get-value 'id element))
                    (ttl (get-value 'title element))
                   )
               (cons (documentation-url id) ttl)))
          ((= 0 (length res)) #f)
          ((> (length res) 1) 
             (error (string-append "doc-subsection-url: multiple sections/entries cannot exists: "
                                   (as-string n) "." (as-string m)))))))

; ---------------------------------------------------------------------------------------------------
; Splitted cross reference index.

(define (split-defined-applied-names dan-list)
  (sublist-by-predicate
    dan-list
    (lambda (cur prev n) ;@a
       (not (eqv? (string-ref (as-string (car cur)) 0)
                  (string-ref (as-string (car prev)) 0))))))


(define (first-letter-of x)
  (as-string (string-ref (as-string x) 0)))

;; makes a cross reference index for a single letter
(define (make-cross-reference-index da-names letter alphabet)
  (write-text-file
   (page (string-append "Alphabetic cross reference index: letter " letter)
         (con
          (icon-bar)  

          (b (font 3 blue "Cross reference index: ")) (horizontal-space 2)
          (alphabetic-link-array-1 "cross-reference-index" alphabet letter)  ; at top
          (present-cross-reference-index da-names) (p)   ; fejl! Lav specialiseret udgave
          (alphabetic-link-array-1 "cross-reference-index" alphabet letter)  ; at bottom
          (vertical-space 8)
          ) (color-of-group "index") black black black
         )
   (html-destination (string-append "cross-reference-index" "-" (hygienic-file-character (downcase-string letter))))))

; Make the overall cross reference index, in terms of an alphabet array with links to smaller indexes.
(define (make-overall-cross-reference-index alphabet)
  (write-text-file
   (page "Overall alphabetic cross reference index"
         (con
           (icon-bar)  

           (b (font 3 blue "Cross reference index: ")) (horizontal-space 2)
           (alphabetic-link-array-1
             "cross-reference-index"
            (map downcase-string alphabet)) (br)
            (font-size 2 (em "Navigate to subindexes via tha alphabet above"))
          )
          (color-of-group "index") black black black
         )
   (html-destination "cross-reference-index")))


; ---------------------------------------------------------------------------------------------------
; Splitted defining name index.

(define (split-defining-name-occurences dno)
  (sublist-by-predicate
    dno
    (lambda (cur prev n)
       (not (eqv? (string-ref (as-string (car cur)) 0)
                  (string-ref (as-string (car prev)) 0))))))

(define (make-defining-name-index dno letter alphabet)
  (write-text-file
   (page (string-append "Defining name index: letter " letter)
         (con
          (icon-bar)  

          (b (font 3 blue "Index of definitions: ")) (horizontal-space 2)
          (alphabetic-link-array-1 "defining-name-index" alphabet letter)  ; at top
          (present-defined-name-index dno) 
          ) (color-of-group "index") black black black
         )
   (html-destination (string-append "defining-name-index" "-"  (hygienic-file-character (downcase-string letter))))))

(define (make-overall-defining-name-index alphabet)
  (write-text-file
   (page "Overall defining name index"
         (con
           (icon-bar)

           (b (font 3 blue "Index of definitions: ")) (horizontal-space 2)
           (alphabetic-link-array-1
              "defining-name-index"
              (map downcase-string alphabet)) (br)
              (font-size 2 (em "Navigate to subindexes via tha alphabet above"))
          )
          (color-of-group "index") black black black
         )
   (html-destination "defining-name-index")))

; ---------------------------------------------------------------------------------------------------------------

; A high level syntax function for definition of the color scheme
; Returns an association list that maps group strings to colors
(define (make-color-scheme . group-color-plist)
  (propertylist-to-alist group-color-plist))

; Return the color that is going to represent group.
; Group is the string given as group in program-source forms
(define (color-of-group group)
 (if elucidator-color-scheme
  (let ((group-color (assoc group elucidator-color-scheme)))
    (if (pair? group-color)
        (cdr group-color)
        default-background-color))
  default-background-color))




; A redefinition from laml.scm - due to use of the very old mirror in the elucidator.

;; Return the LAML POWER icon with link to the LAML home page.
;; Intended for the footer of LAML generated pages, from which the author wish to acknowledge the use of LAML.
;; The LAML icon is located in (string-append (laml-home-url-prefix extra-level) "images/laml-power-icon-1.gif"),
;; where extra-level is the optional parameter of the current function.
;; The optional parameter extra-level can be given if the generated HTML files are placed in a different directory than the startup directory.
;; The default value is 0.
;; The optional parameter icon-size can either be small or large. large is the default value.
;; The role of extra-level is the same as in the procedure laml-home-url-prefix.
;; .form (laml-power-icon [extra-level icon-size])
;; .internal-references "related procedure" "laml-home-url-prefix"
(define (laml-power-icon . optional-parameter-list)
 (let ((extra-level (optional-parameter 1 optional-parameter-list 0))
       (icon-size   (as-symbol (optional-parameter 2 optional-parameter-list 'large)))
      )
   (a-tag-target "http://www.cs.auc.dk/~normark/laml/"
      (html:img 'src (string-append 
                              (cond ((eq? icon-size 'large) "images/laml-power-icon-1.gif")
                                    ((eq? icon-size 'small) "images/laml-mini-icon-1.gif")
                                    (else (laml-error "laml-power-icon: third parameter must either be large or small"))))
            'alt "Program Oriented Web Engineering - using LAML") "_top")))


; Convert ch (character or sigular string) to a character that is legal in file names.
; The problems with '<' and '>' in Windows XP file names is the motivation behind this function.
; Returns a string.
(define (hygienic-file-character ch)
  (let ((ch-n (as-number (as-char ch))))
    (cond ((or (= ch-n 60) (= ch-n 62)) ;  '<' or '>' 
             (string-append "c" (as-string ch-n)))
          (else ch))))


; Redefined and specialized version of alphabetic-link-array-1 from html.scm
; Handle file hygienic letters appropriately.
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
	       (a-tag (string-append target-file-prefix "-" (hygienic-file-character letter) ".html")
		      (if (and em-let (equal? em-let letter))
			  (font 4 red (b (capitalize-string-nd letter)))
			  (capitalize-string-nd letter))
		      (horizontal-space 1))
               " "
            ))
	    alphabet-1))))