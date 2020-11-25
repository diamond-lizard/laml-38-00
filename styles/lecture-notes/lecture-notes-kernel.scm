; fix exercise-slide-header and image series headers (and perhaps others) to allow linear navigation.

; The kernel part of LENO, which is common to the original system and the new LAML/XML based version of LENO.
; HTML mirror in use: html4.0 loose, non-validating.

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

(define (set-language language)
  ; either the symbol 'danish og 'english.
 (set! language-preference language) ; language-preference is in laml.scm
)

(define notes-subtitle #f)

; The type of lecture: Either normal or side-track (a symbol).
(define lecture-type 'normal)

(define slide-header-as-frame? #f)

; A number describing the verbosity of LENO.
; The number 0 means no messages.
; The number 1 implies some messages, including a message pr. note page
; The number 2 implies the maximum verbosity
(define lecture-note-verbose-mode 1)


(define lecture-list-two-colummn-threshold 16)

; A variable that controls the alignment of item clauses and their annotations in note view.
(define align-items-and-annotations #t)

; A variable which controls the generation of the header banner in slide view.
; If the value is the symbol normal use normal slide header (navigation buttons).
; If the value of the variable is none generate an empty header banner.
; In the value is minimal generate a tiny small font info about the slide.
; With the values none or minimal, navigation from slide to slide must be done via the keyboard.
(define slide-header? 'normal) ; one of the symbols normal, none, or minimal

; A variable which controls whether to make a parallel trail of the lecture.
; This makes a frameset for each slide, cause many more files in the html directory.
(define trail-of-lecture? #f)

; A variable that controls which we to embed in a trail. The normal value is the symbol slide.
; Another possible value is annotated-slide
(define trail-embedding-mode 'slide)

; A variable which controls the generation of an index page side by side with the LAML LENO source file.
(define source-level-index-page? #t)

; A variable which controls whether to make a nice and simple title page.
; This also controls the abstract page.
(define lecture-title-page? #t)

; A variable which controls whether to make a nice and simple title page at top level.
(define top-level-title-page? #t)

; A variable which controls whether to make a nice and simple abstract page at top level.
; If this variable is false, #f, inline the abstract on the top level index page.
(define top-level-abstract-page? #t)

; If #f, do not generate Javascript scripts at all.
(define java-scripting #t)

; The number of ciffers support by numbered keyboard navigation.
; 1 - 9, a - m.
(define max-keyboard-navigation-number 22) ; 

; Add extra space such that the slide contents is displaced to the right.
; Useful if the browser or projector clips the left part of the screen.
; A number of pixels.
(define slide-page-offset 0)

; A variable which determine how Javascript scripts are loaded.
; The value must be one of the symbols 'static or 'dynamic.
; If static, the javascript is included inline into the generated HTML page.
; if dynamic, the javascript is loaded from the javascript sub-directory of the html directory.
; This variable is only relevant if java-scripting is #t
; Dynamic is recommended.
(define javascript-loading 'dynamic)

; It is possible to advance from one slide to the next via either single or double click.
; The value of this variable determines whether to use single or double click for page advancement.
; Use one of the values single-press or double-press (symbols).
; The value double-press is default and recommended. If you use single press advancement this collides with text selection.
(define mouse-advancement 'double-press)

; A variable which controls whether to copy image icons from the software directory to the html destination directory.
(define copy-image-files? #t)

; A variable which controls whether to copy user made graphics files (normal and small) from the software directory to the html destination directory.
(define copy-note-graphics? #t)

; A variable which controls whether to copy lecturer photos from the software directory to the html destination directory.
(define copy-lecturer-photos? #t)

; The news flash string to display on certain LENO pages.
; An empty newflash will not cause any news flashing.
; Redefine in, for instance, post-notes.
(define news-flash-string "")

(define news-flash-url #f)

; The news flash level determines the scope the newsflash (on how many pages news-flash-string will appear).
; A low number gives little of no news flash.
; Redefine in, for instance, post-notes.
; Level 0: None
; Level 1: Only the specific lecture overview page, for which news-flash-string and news-flash-level apply.
; Level 2 or higher: Main page: main overview page of all lectures.
; Level 3 or higher: Level 2 + each lecture overview page.
; Level 4 or higher: Level 3 + other overview pages.
; Level 5: Level 3 + a number of other pages generates by LENO. (NOT YET MADE).
(define news-flash-level 2)

; A variable which controls whether to process all lectures, and not only a single one as requested
(define do-process-all-lectures? #f)

; A variable which controls whether to rinse the html and html/graphics directories.
(define do-clean-html-directory? #f)

(define note-contents-description "") ; can be redefined in the scheme suffix part

; A variablel that controls how to treat svg-images. A symbol.
; Values: svg. Normal svg treatment.
;         png. Always use png graphics instead of svg file
;         png-if-exist:  use png graphics instead of svg, but only if a similar png file actually exists. 
(define treat-svg-as 'svg)

; True if special LENO end-of-line documentation comments should be extracted and presented.
; The default value is false (#f).
(define extract-and-present-end-of-line-doc-comments #f)

; The end of line doc comment string prefix.
; The contents following then end-of-line-doc-comment-prefix is regarded as documentation. 
; Only used if extract-and-present-end-of-line-doc-comments is true.
(define end-of-line-doc-comment-prefix "")


(define pdf-version-as-of #f)    ; date of pdf version, or #f. Format  "y-m-d"


; ------------------------------------------------------------------
; View control.
; Variables that control which views to generate and link into the material.

(define make-slide-view? #t)
(define make-annotated-slide-view? #t)
(define make-aggregated-view? #t)

(define the-primary-view 'slide-view)

; If true, a print page is made with links to printable pdf files with theme stuff.
(define make-print-page? #f)

; ---------------------------------------------------------------------------------------------------------------
; Show and speak control

; A variable that controls the automatic progressing slide show and speak.
; If #t, generate pages for automatic slide show and speak. If #f, do not.
; Redefine or re-assign after the loading of lecture-notes.scm (this file).
(define show-and-speak? #f)

; A variable that gives extra help to the author when sound clips are recorded.
; In normal use, this variable should always be false.
; Re-assign in post-notes.scm.
(define show-and-speak-recording? #f)

; The default showing time in second for automatic show (and speak).
; Does only apply if show-and-speak is true.
(define default-showing-time 2)

; A number of seconds added to every showing time. Normally 0.
; Useful to make additional, fixed delay of show-and-speak showing times
(define additional-showing-time 0)

; A variable that defines the sound source. One of the symbols real-audio or wave-file.
; Notice that the value of this variable is coupled to speak-url-prefix and speak-file-prefix.
; Defaults to real-audio.
(define sound-source 'real-audio)

; The element-part of the default show-and-speak-clause
; This is the show-and-speak clause supplied on a note page if no explicit show-and-speak clause is given.
(define default-show-and-speak-clause
  (list (list 'slide-part 1 default-showing-time)))

; The prefix of the url to speaker sound
; Must be redefined after the loading of lecture-note.scm, such as in post-notes.scm.
; Notice that the value of this variable is coupled to sound-source and speak-file-prefix.
(define speak-url-prefix "")

; An optional file prefix to the sound files
; If #f, there is no such file prefix. Thus, #f is a legal value of this variable,
; meaning that we cannot check for sound file existence.
; Can be redefined after the loading of this file. For instance in post-notes.scm
; Notice that the value of this variable is coupled to sound-source and speak-url-prefix.
(define speak-file-prefix #f)


; A boolean variable which controls whether to make faked sound files
; These are useful if the sound files are actually inaccessible for LENO.
; We generate faked sound files for all pages with non zero playing times.
(define make-faked-show-and-speak-files? #f)

; Return the sound source file extension.
; In case we provide for real-audio we assume that the file source is wav files.
(define (sound-source-extension)
  (cond ((eq? sound-source 'real-audio) "wav")
        ((eq? sound-source 'wave-file) "wav")
        ((eq? sound-source 'mp3-file) "mp3")
        (else (laml-error "sound-source-extension: Unknown kind of sound source:" sound-source))))


; Return the full sound file path of sound, of #f it it is not available
(define (sound-file-path sound)
  (if speak-file-prefix
      (string-append note-source-directory speak-file-prefix (as-string sound) "." (sound-source-extension))
      #f))

; Return a speak-url formed by speak-url-prefix, name (the parameter) and the fixed extension .wav
(define (speak-url name)
 (cond ((eq? sound-source 'wave-file) (speak-url-wave name))
       ((eq? sound-source 'real-audio) (speak-url-real-audio name))
       (else (error (string-append "speak-url: Unknown sound-source: " (as-string sound-source))))))

(define (speak-url-wave name)
  (string-append speak-url-prefix name "." (sound-source-extension)))

(define (speak-url-real-audio name)
  (string-append speak-url-prefix name ".rm"))

; An internal variable in which to collect speak entries. 
; Goes to the internal speak file, named by the function speak-file-name.
(define speak-list-in-this-lecture '())

; The speak list from the .spk file from an earier run.
; We use information from this list to predict show-and-speak information.
; Assigned in begin-notes. Not used yet (we would use it to access timing information, but it was not necessary).
(define previous-speak-list-in-this-lecture #f)

; Return the full path of the internal spk file. The parameter defaults to the id of the current lecture.
; Alternatively, the id of another lecture may be passed.
(define (speak-file-name . optional-parameter-list)
 (let ((id (optional-parameter 1 optional-parameter-list (as-string lecture-id))))
   (string-append note-source-directory "internal/" id ".spk")))

; A internal global variable that controls if we are located in a show and speak context.
; From a functional programming point of view, this variable is a bad idea.
; Alternatively, we should have entirely separate functions for generation of 
; show and speak pages. 
(define show-and-speak-context #f)

; ----------------------------------------------------------------------------------------------------------------------------
; Elucidator linking defaults:

; The name of the elucidator browser window, as passed as target attribute to an anchor element.
; If false (#f) do not apply target.
(define elucidator-default-target #f)

; An enumeration of all LENO image (icon) files. These files are copied from the images directory of the software directory to the 
; images directory of the note source directory, if the copy-image-files? variable is true.
(define leno-image-files
  (list "nav-up.gif" "nav-left.gif" "nav-right.gif" "nav-right-red.gif" "todisk.gif" "index.gif" "question.gif"
        "nav-home.gif" "nav-book.gif" "nav-aggr.gif" "nav-notes.gif" "nav-slide.gif" "arrow-down.gif" "arrow-up.gif" "exclamation.gif"
        "exclamation-red-ring.gif" "no-frames.gif"

        "program-icon.gif" "listing-icon.gif"
        "exercise-icon-en.gif" "exercise-icon-dk.gif" "image-series-icon-dk.gif"  "image-series-icon-en.gif"
        "program-icon-small.gif" "listing-icon-small.gif"
        "exercise-icon-en-small.gif" "exercise-icon-dk-small.gif" "image-series-icon-dk-small.gif"
        "image-series-icon-en-small.gif"

        "elucidator-icon.gif" "elucidator-icon-small.gif"

        "side-track-icon-dk.gif" "side-track-icon-dk-small.gif"
        "side-track-icon-en.gif" "side-track-icon-en-small.gif"
        "side-track-back.gif"   "side-track-label.gif"

        "readers-guide-dk.gif" "readers-guide-en.gif"

        "nav-contents-frame.gif" "nav-up-frame.gif" 
        "nav-current-frame.gif" "nav-down-frame.gif" "think.gif"

        "blank-pillar.gif"  
        "left-quote.gif" "right-quote.gif"

        "nav-down-empty.gif"  "nav-down-frame-empty.gif"  "nav-left-empty.gif" "nav-left-frame-empty.gif" "nav-right-empty.gif"
        "nav-right-frame-empty.gif" "nav-up-empty.gif"  "nav-up-frame-empty.gif"

        "red-ball-large.gif" "red-ball-small.gif"
        "yellow-ball-large-rised.gif" "yellow-ball-large.gif" "yellow-ball-small.gif"
        "blue-ball-small.gif" "blue-ball-large.gif"
        "cyan-ball-large.gif" "cyan-ball-small.gif"
        "green-ball-large.gif" "green-ball-small.gif"

        "blue-ring.gif" "red-ring.gif" "yellow-ring.gif"
        "red-triangle.gif"  "red-half-oval.gif"

        "small-speaker.gif" "small-speaker-empty.gif" 

        "atomspin.gif" "atomspin-freeze.gif"

        "speaker.gif" "speaker-empty.gif" "no-speaker.gif" "pause.gif" ; not used yet

        "print-icon.gif" "adobe-pdf.gif"

        "logo.gif"

        "wall_white.gif" "powerpoint-title-bar.jpg" "powerpoint-rectangles.jpg"                           ; background graphics - from Per
        "blue-grid.gif"  "aau-logo-grey-background.gif" 
        "aau-logo-blue-background.gif" "aau-logo-dk.gif" "aau-logo-light.gif"            ; kn background graphics
        "aau-logo-upper-left.gif" "aau-logo-lower-right.gif"

        "letter-l.gif" "letter-t.gif"

        "laml-power-icon-1.gif" "leno-icon-2.gif" "laml-power-icon-4.gif" 

        "triangle-long.gif" "box.gif"
        "explained-icon.gif" "red-star.gif"
  )
)

; Return the program icon name, given language (danish or english) and size (large or small)
(define (program-icon-name language size . optional-parameters)
 (let ((kind (as-symbol (optional-parameter 1 optional-parameters 'source-program))))
  (cond ((eq? kind 'source-program)
            (cond ((eq? size 'large) "program-icon.gif")
                  ((eq? size 'small) "program-icon-small.gif")
                  (else (laml-error "program-icon-name. Unknown size"))))
        ((eq? kind 'listing)
            (cond ((eq? size 'large) "listing-icon.gif")
                  ((eq? size 'small) "listing-icon-small.gif")
                  (else (laml-error "program-icon-name. Unknown size"))))
        (else (laml-error "program-icon-name. Unknown kind")))))
     

; Return the exercise icon name, given language (danish or english) and size (large or small)
(define (exercise-icon-name language size)   
  (cond ((and (eq? size 'large) (eq? language 'danish)) "exercise-icon-dk.gif")
        ((and (eq? size 'small) (eq? language 'danish)) "exercise-icon-dk-small.gif")
        ((and (eq? size 'large) (eq? language 'english)) "exercise-icon-en.gif")
        ((and (eq? size 'small) (eq? language 'english)) "exercise-icon-en-small.gif")
        (else (error "exercise-icon-name. Unknown language or size"))))

; Return the image-series icon name, given language (danish or english) and size (large or small)
(define (image-series-icon-name language size)  
  (cond ((and (eq? size 'large) (eq? language 'danish)) "image-series-icon-dk.gif")
        ((and (eq? size 'small) (eq? language 'danish)) "image-series-icon-dk-small.gif")
        ((and (eq? size 'large) (eq? language 'english)) "image-series-icon-en.gif")
        ((and (eq? size 'small) (eq? language 'english)) "image-series-icon-en-small.gif")
        (else (error "image-series-icon-name. Unknown language or size"))))

(define (elucidator-icon-name language size)   
  (cond ((and (eq? size 'large) (eq? language 'danish)) "elucidator-icon.gif")
        ((and (eq? size 'small) (eq? language 'danish)) "elucidator-icon-small.gif")
        ((and (eq? size 'large) (eq? language 'english)) "elucidator-icon.gif")
        ((and (eq? size 'small) (eq? language 'english)) "elucidator-icon-small.gif")
        (else (error "elucidator-icon-name. Unknown language or size"))))

(define (side-track-icon-name language size)   
  (cond ((and (eq? size 'large) (eq? language 'danish)) "side-track-icon-dk.gif")
        ((and (eq? size 'small) (eq? language 'danish)) "side-track-icon-dk-small.gif")
        ((and (eq? size 'large) (eq? language 'english)) "side-track-icon-en.gif")
        ((and (eq? size 'small) (eq? language 'english)) "side-track-icon-en-small.gif")
        (else (error "side-track-icon-name. Unknown language or size"))))

(define (readers-guide-icon-name language)
  (cond ((eq? language 'danish) "readers-guide-dk.gif")
        ((eq? language 'english) "readers-guide-en.gif")
        (else (error "readers-guide-icon-name. Unknown language."))))



(define image-file-access #f)


; The preferred maximum length of file names, including extension, excluding initial path.
; This size may be important for CD file systems.
; A warning is issed if the preferred maxiumum length is exceeded.
(define preferred-maximum-filename-length 64)

(define leno-stylesheet-directory (string-append leno-software-directory "stylesheets/"))

(define note-stylesheet-directory (string-append note-source-directory "stylesheets/"))

; The directory in which the note source is located must be defined by the variable note-source-directory. Must be redefined in notes.scm

(define front-index? #t)  ; make front index (top level)
(define (include-front-index) (set! front-index? #t))
(define (exclude-front-index) (set! front-index? #f))

(define word-index? #t)   ; make word index - only affects the generation and the presentation of the word index, 
                          ; not the adding of word to internal idx files.
(define split-word-index-alphabetically? #t)  ; make a separate index for each letter in the alphabet
(define (include-word-index) (set! word-index? #t))
(define (exclude-word-index) (set! word-index? #f))


(define cross-references? #f) ; generate a separate page of lecture note wide cross references
(define global-cross-reference-name "all-references") ; the name of the lecture wide cross reference page

(define bibtex-support? #t)  ; Do we allow bibtex references and cites in themes. Only relevant in themes.
(define bibtex-files '())  ; A list of full paths to the parsed bibtex files in (lisp -.lsp) format
                           ; in which to find bibtex-reference cross references.
(define all-bibtex-entries '())  ; assigned in begin-notes. A flat list of all entries from bibtex-files.

(define course-dir #f) ; #f if no relevant course dir exists. The course dir is the directory of the course-plan directory,
                       ; from which we can extract information about the course in which these notes are used.
                       ; redefined in post-notes.scm if necessary.

(define note-specific-image-files '()) ; the list of note specific image files which are copied from the note
                                       ; source file directory to the html/images directory.
                                       ; If you have such images in your notes, redefine this variable in post-notes.scm.
                                       ; One of these file can be logo.gif, which is used by the logo-img function to present
                                       ; a university logo on some pages.
                                       ; Notice that these images will eventually be copied to the same directory 
                                       ; as the native, LENO specifiy icons and images. Be careful not to overwrite one of these.

(define note-specific-graphics-files '()) ; A list in which to accumulate the user made graphics file names (name and extension only).

; The URL which is linked to from the logo.
; If it is the empty string, do not link. 
(define logo-url "http://www.auc.dk/") 


; Relative address of the directory with lecture photos. Relative to the destination html directory.
; Can also be an absolute URL.
; Can be redefined or assigned after lecture-notes.scm (this file) is loaded. Can be done in post-notes.scm.
; The default, however, is in lecturer-photos in the destination html directory.
; You should not change the value of this variable.
(define lecturer-photo-url-prefix "lecturer-photos/")

; The width of a table cell for lecturer photos
(define lecturer-photo-width 200)

; The list of photo names, such as ("normark1.jpg" "normark2.jpg") of photos found at the address lecturer-photo-url-prefix.
; Must be redefined or assigned after lecture-notes.scm (this file) is loaded. Can be done in post-notes.scm.
(define lecturer-photo-list '())

; Is there thematic pages (themes) in this material.
; Set in post-notes.scm. Corresponds to the boolean XML-in-LAML attribute theme-view.
(define themes? #f)

; A variable that controls whether a new and fresh secondary theme source file is generated.
; Possible values: overwrite, new, delta, none (symbols).
;   overwrite: Overwrites the canonically named theme source file. DANGEROUS - BE CAREFUL.
;   new: Make a fresh unique theme source file (using a new unique filename).
;   none: Do not make a secondary theme source file.
(define theme-source-mode 'none)

; A variable that controls automatic processing of the secondary themes source file, following the
; processing of the primary LENO source file.
(define theme-auto-process-mode #f)

; A list of the author name and affiliation informations.
; Define in post-notes.scm.
; In case no author information is passed to the lecture-intro form, 
; this variable is used as default.
(define author-and-affiliation-list '())

; A variable that controls whether a new and fresh trail source file is to be generated. 
; As the default, the trail covers all pages in all lectures.
; Possible values: overwrite, new, none (symbols).
;   overwrite: Overwrites the canonically named trail source file. DANGEROUS - BE CAREFUL.
;   new: Make a fresh unique theme source file (using a new unique filename).
;   none: Do not make a trail source file.
(define trail-source-mode 'none)

(define source-program-index? #t)

; -----------------------------------------------------------------------------------------------------------------------------------------------------------
; ANNOTATIONS
; Annotations of LENO material is supported by an external CGI based tool called Annotator.
; Contains a few redundant defintions from the annotator stuff. Consider loading annotator lib if more is needed.

; Notice that the use of the Annotator assumes that the function course-absolute-url is defined

(define annotator-support? #f) ; Generate links to the annotator - an external tool to LENO.
                              ; Change the value of this variable after this file is loaded - for instance in post-notes.scm

; A symbol that designates the top level annotation name (becomes directory name in the annotator data dir).
; Must be redefined in post-notes.scm if annotator-support is #t.
(define annotator-symbol #f)

; The URL prefix of the annoator. Change this according to you local installation.
; Only used if annotator-support? is #t.
(define annotator-url-prefix "http://www.cs.auc.dk/~normark/cgi-bin/annotator/")

; The absolute path to the Annotator CGI directory.
; Only used if annotator-support? is #t.
(define annotator-cgi-directory "/user/normark/.public_html/cgi-bin/annotator/")

(define (annotator-url annotator-cgi-program key-list val-list)
  (string-append annotator-url-prefix annotator-cgi-program "?" (make-url-parameters key-list val-list)))

; Return a full path to an annotator cgi-bin directory file
(define (annotator-cgi-file final-path)  
  (string-append annotator-cgi-directory final-path))

(define annotator-id-separator-char #\$)

; Encode id-list to a dollar (or whatever) separated string suitable for URL parameter passing purposes
(define (encode-annotation-ids id-list)
  (list-to-string id-list (as-string annotator-id-separator-char)))

; Return the list of existing annotations (a-list format) of the given category
; Corresponds roughly to the function annotation-catalogue-list in annotator-lib.scm (modulo the filtering with non-categorial).
(define (list-of-submitted-annotations . decoded-id-list)
  (let ((all-data-path (annotator-cgi-file (string-append "data/" (list-to-string decoded-id-list "/") "/" "all.dat"))))
   (letrec ((non-categorial string?))
    (if (file-exists? all-data-path)
        (filter non-categorial (file-read all-data-path))
        '()))))


; -----------------------------------------------------------------------------------------------------------------------------------------------------------
; EXERCISES:

; Notice that the use of the Exercise Manager assumes that the function course-absolute-url is defined

(define exercise-index? #t)   ; generate a separate exercise index pr. lecture, an overall exercise index, and appropriate linking to these

(define exercise-manager-support? #f) ; generate framed exercises with support of the exercise-manager - synchronous (an external tool to LENO).
                                      ; only one of exercise-manager-support? and distance-education-support? are allowed to be true

(define distance-education-support? #f) ; generate framed exercises with support of the distance education tool  - asynchronous (an external tool to LENO).
                                        ; Meant for IDAFUS connection.
                                        ; In case of distance-education-support? a number of other variables are assumed to be defined.

(define exercise-manager-url-prefix "http://www.cs.auc.dk/~normark/cgi-bin/exercise-manager-1/")

(define (exercise-manager-url cgi-program key-list val-list)
  (string-append exercise-manager-url-prefix cgi-program "?" (make-url-parameters key-list val-list)))


(define reveal-exercise-solutions? #f)  ; should the solutions be revealed from these lecture notes.
                                        ; set it in individual note laml files.
                                        ; does not ALONE affect the overall exercise index. See reveal-all-solutions?

(define reveal-all-solutions? #f) ; Controls whether solutions are shown in overall exercise index.
                                  ; Should and must be #f during the semester.
                                  ; reveal-exercise-solutions? does not provide enough control, because we do never want to hide
                                  ; already revealed solutions.

(define lecture-exercise-list '())  ; The list of exercises in a lecture
                                    ; Entries of the form (exercise-id title page-id exercise-number-on-page body exercise-number-in-lecture solution-symbol).


; -------------------------------------------------------------------------------------------------
; IDAFUS Integration variables
; Does only apply if distance-education-support? is #t

; The absolute path to the IDAFUS instance in cgi-bin:
(define idafus-absolute-cgi-path #f)

; The id of the IDAFUS unit to which the exercises belong
(define idafus-unit-id #f)


; The start date of the activities. Format ddmmyyyy
(define idafus-start-date #f)

; The end date of the activities. Format ddmmyyyy
(define idafus-end-date #f)

; A teacher user name - only a single name is supported
(define idafus-teacher #f)


; -------------------------------------------------------------------------------------------------

(define lecture-list-from-course-plan '())  ; We attempt to read the internal course plan file in begin-notes.

; Return whether (note) exercises are shown in a frame.
; It requires a different form of navigation out from such a frame.
(define (frame-exercises?)
  exercise-manager-support?) 

; An a list of lecture-id solution-password pairs.
; Defined in post-notes.scm
; If it is the empty list, no solution password constituents are added to solution file names.
(define solution-passwords '())

; Remembers the actually used exercise model. Assigned by the procedure exercise-model.
(define used-exercise-model 'make-indexes)

; Define the overall exercise model for these notes.
; The procedures sets the variables above.
; Kind is one of the symbols:
; none: No exercises at all in these notes.
; make-indexes: Make exercise indexes in these notes.
; make-indexes-and-solutions-per-lecture: Make exercise indexes and lecture-specific solution indexes.
; make-indexes-and-solutions-lecture-wide: Make exercise indexes, lecture-specific solution indexes, and a global solution index.
; synchronous-exercises: Make exercise indexes, solution indexes at a/after a certain point in time, plus framed exercises with room for the exercise manager.
; asynchronous-exercises: Make exercise indexes, NO solution indexes all, plus framed exercises with room for the asynchronous distance education tool.
(define (exercise-model kind)
  (set! used-exercise-model kind)
  (cond ((eq? kind 'none)
             (begin (set! exercise-index? #f) 
                    (set! exercise-manager-support? #f)
                    (set! distance-education-support? #f)
                    (set! reveal-exercise-solutions? #f)
                    (set! reveal-all-solutions? #f)
             ))
       ((eq? kind 'make-indexes)
             (begin (set! exercise-index? #t) 
                    (set! exercise-manager-support? #f)
                    (set! distance-education-support? #f)
                    (set! reveal-exercise-solutions? #f)
                    (set! reveal-all-solutions? #f)
             ))
       ((eq? kind 'make-indexes-and-solutions-per-lecture)
             (begin (set! exercise-index? #t) 
                    (set! exercise-manager-support? #f)
                    (set! distance-education-support? #f)
                    (set! reveal-exercise-solutions? #t)
                    (set! reveal-all-solutions? #f)
             ))
       ((eq? kind 'make-indexes-and-solutions-lecture-wide)
             (begin (set! exercise-index? #t) 
                    (set! exercise-manager-support? #f)
                    (set! distance-education-support? #f)
                    (set! reveal-exercise-solutions? #t)
                    (set! reveal-all-solutions? #t)
             ))
       ((eq? kind 'synchronous-exercises)
             (begin (set! exercise-index? #t) 
                    (set! exercise-manager-support? #t)
                    (set! distance-education-support? #f)
                    (set! reveal-exercise-solutions? #f)   ; reveal at a certain point in time - controlled by other means
                                                           ; see the function do-reveal-exercise-solutions?
                    (set! reveal-all-solutions? #f)
             ))
       ((eq? kind 'asynchronous-exercises)
             (begin (set! exercise-index? #t) 
                    (set! exercise-manager-support? #f)
                    (set! distance-education-support? #t)
                    (set! reveal-exercise-solutions? #f)
                    (set! reveal-all-solutions? #f)
             ))
       (else (error "Exercise model: Unknow exercise model"))))


; a more elaborate decission function than the variable reveal-exercise-solutions? determining if the solutions
; should be revealed in these notes
; in case exercise-manager-support? is #t this function depends on lecture-exercise-reveal-time from notes.scm
(define (do-reveal-exercise-solutions?)
  (cond (exercise-manager-support? (and reveal-exercise-solutions? (> (current-time) (lecture-exercise-reveal-time lecture-id))))
        (else reveal-exercise-solutions?)))

; ------------------------------------------------------------------------------------------------------------------------------------
; QUIZ SUPPORT

(define quiz-support? #t)

(define quiz-url-prefix "http://www.cs.auc.dk/~normark/cgi-bin/quiz/")
(define quiz-software-dir (string-append laml-dir "tools/quiz/"))
(define quiz-cgi-directory "/user/normark/.public_html/cgi-bin/quiz/")

; The list of questions/answers in this lecture
(define quiz-list '())

; Make and return a LENO quiz entry as stored in the -.quiz file
; This is not the same as a quiz entry as supported by the quiz tool.
(define (make-leno-quiz-entry question answer-list absolute-note-url)
  (list question answer-list absolute-note-url))

; Selectors for internal LENO quiz entries:
(define leno-quiz-question (make-selector-function 1 "leno-quiz-question"))
(define leno-quiz-answer-list (make-selector-function 2 "leno-quiz-answer-list"))
(define leno-quiz-background-url (make-selector-function 3 "leno-quiz-background-url"))


; Return the name of the quiz file. The parameter defaults to the id of the current lecture.
; Alternatively, the id of another lecture may be passed.
(define (quiz-file-name . optional-parameter-list)
 (let ((id (optional-parameter 1 optional-parameter-list (as-string lecture-id))))
   (string-append note-source-directory "internal/" id ".quiz")))

; Return the external (quiz tool defined) format of leno-quiz-entry (as found on the -.quiz file).
; The second parameter n is the number of the question
(define (internal-quiz-format leno-quiz-entry n)
  (let* ((question (car (element-contents (leno-quiz-question leno-quiz-entry)))) ; the element-contents is always a list
         (question-id (string-append lecture-id "-" (as-string n)))
         (answers  (element-contents (leno-quiz-answer-list leno-quiz-entry)))
         (internal-answers (map internal-quiz-answer-format answers))
         (back-url (leno-quiz-background-url leno-quiz-entry))
        )
    (make-quiz-entry (as-symbol question-id) question back-url internal-answers)))

(define (internal-quiz-answer-format leno-answer-entry)
  (let* ((answer-contents (element-contents leno-answer-entry))
         (answ (first answer-contents))
         (corr (second answer-contents))
         (clar (third answer-contents)))
    (make-answer-entry answ (as-number corr) clar)))


; ---------------------------------------------------------------------------------------------------------------------------------------

(define include-source-file-listing? #t)  ; add a list of source files to book view. Normally #f
                                          ; only of interest to the author of these notes

(define help-and-about-page? #t)  ; generate a fixed LENO help and about page


(define lecture-id (source-filename-without-extension))
(define lecture-title "") ; assigned later
(define lecture-author-info "") ; assigned later - in the lecture-intro clause.
(define lecture-abstract "") ; assigned in lecture-intro
(define lecture-date "") ; assigned in lecture-intro

; Is x an original lecture-sections structure
(define (original-lecture-sections-format? x)
 (if (and (list? x) (not (null? x)))
     (let ((first-el (car x)))
       (and (list? first-el) (= (length first-el) 2)))
     #f))

; Is x a new, simple lecture-sections structure
(define (simple-list-lecture-sections-format? x)
 (if (and (list? x) (not (null? x)))
     (let ((first-el (car x)))
       (string? first-el))
     #f))

; Originally, lecture-sections was a list with entries of the form
;    ("lecture-id" "lecture-title")
; Now we also support entries of just the form "lecture-id". With this, lecture-sections
; becomes a list of strings instead of a list of list.
; We support both forms to stay backward compatible.
; We assume that there are at least one element, and that this element can be used to 
; determine the form of the list.
; Please notice that in do-begin-notes, we re-assign lecture-sections to the original form if necessary.
; This allows old software to survive.
(define lecture-list 
 (cond ((original-lecture-sections-format? lecture-sections)
           (map car lecture-sections))
       ((simple-list-lecture-sections-format? lecture-sections)
           lecture-sections)
       (else (error "Cannot determine the form of lecture-sections."))))


(define lecture-titles
 (cond ((original-lecture-sections-format? lecture-sections)
           (map cadr lecture-sections))
       ((simple-list-lecture-sections-format? lecture-sections)
           #f)  ; lecture-titles assigned later
       (else (error "Cannot determine the form of lecture-sections."))))

 

(define target-extension "html")
(define file-name-of-word-index "note-index") ; i.e., place word index on a file note-index. Becomes html/note-index.html
(define top-level-note-file-name "index")     ; i.e., place top level file at index. Becomes html/index.html
(define top-level-theme-index-file-name "theme-index")  ; the name of the similar theme index.
(define overall-exercise-index-name "exercise-index")  ; the name of the overall exercise index.
(define secret-overall-exercise-index-name #f)  ; the name of the secret overall exercise index. If #f, do not make such a secret index

(define list-of-source-files '())   ; a list of all source files processed by source-program in this lecture
                                    ; Each entry is of the form (source-file-descriptor source-file-name title kind) where
                                    ; source-file-descriptor is a tag (a symbol), the two next elements are strings, and kind is a symbol:
                                    ; source-programs for ordinary source programs, and listing for output listings.
(define cross-refs-in-this-lecture '())   ; in this list we collect all cross references in this lecture
(define lsp-contents-in-this-lecture '())  ; in this list we collect the full contents of the notes as Lisp data structure 

; user level function
; add a cross reference entry to this lecture's list of cross references
; ref is of the for (internet-reference ...) (note-reference ...) or (informal-reference ...)

(define (add-lecture-reference ref)
  (set! cross-refs-in-this-lecture (cons ref cross-refs-in-this-lecture)))


(if quiz-support?
    (load (string-append quiz-software-dir "quiz-lib.scm")))


; Now assigning image-file-access. Images (icons) are located in a subdirectory of the html directory
(set-image-file-path! 'sub-directory)

; Given the leno language (a symbol, either 'danish og 'english) return the language string used in a meta lang attribute
(define (meta-language leno-language)
  (cond ((eq? leno-language 'danish) "da")      ; see: http://www.indigo.ie/egt/standards/iso639/iso639-1-en.html
        ((eq? leno-language 'english) "en-us")
        (else "dk")))

; (define (copyright-clause)
;   (html-comment 
;     "The material in this file is copyrighted (c) by Kurt Nørmark, Aalborg University, Denmark. Email: normark@cs.auc.dk. All rights reserved."))

(define (copyright-clause)
  (html-comment
   (string-append  
    "The material in this file is copyrighted (c) by "
    (list-to-string lecture-author-info ", ") ". "
    " All rights reserved.")))

; Force the name of html files generated from this source to have 
; a certain lecture name.  Must be placed before begin-notes
; If not given, the lecture name becomes the proper name of the laml source file.
(define (set-lecture-name name-without-extension)
  (set! lecture-id (as-string name-without-extension)))

(define (set-lecture-number n)
  (set! lecture-number n))

; ----------------------------------------------------------
; Selectors to and function of the lecture-sections list:

; Return the title of section n from lecture-sections
(define (lecture-title-of lecture-sections n)
 (second (list-ref lecture-sections (- n 1))))

; Return the id of section n from lecture-sections. The returned id is a string.
(define (lecture-id-of lecture-sections n)
 (as-string (first (list-ref lecture-sections (- n 1)))))

; Return the number of the lecture with lecture-id id, based on the sequence of
; lectures in lecture-sections. The first lecture counts as number one.
; Returns #f if the lecture cannot be located in lecture-sections.
(define (lecture-number-of lecture-sections lecture-id)
 (let ((res (lecture-number-of-1 lecture-sections lecture-id 1)))
   (if res
       res
       (laml-error "lecture-number-of:" "Unknown lecture-id" lecture-id))))

(define (lecture-number-of-1 lecture-sections lecture-id i)
 (cond ((null? lecture-sections) #f)
       ((equal? (as-string (car (first lecture-sections))) (as-string lecture-id)) i)
       (else (lecture-number-of-1 (cdr lecture-sections) lecture-id (+ i 1)))))

; Return the title of section lecture-id, as extracted from lecture-sections
(define (lecture-title-given-lecture-id lecture-id)
  (let ((lst-res (assoc (as-string lecture-id) lecture-sections)))
    (if lst-res 
        (second lst-res)
        "???" )))
  

; ----------------------------------------------------------


; obsolete:
(define (set-previous-lecture name-without-extension)
  ; set the previous lecture, such that linking at lecture level becomes possible
  (error "set-previous-lecture is obsolete. Now calculated automatic from current-lecture and lecture-number")
  (if (and (string? name-without-extension) 
           (> (string-length name-without-extension) 0))
    (set! previous-lecture name-without-extension)))

; obsolete:
(define (set-next-lecture name-without-extension)
  ; set the next lecture, such that linking at lecture level becomes possible
  (error "set-next-lecture is obsolete. Now calculated automatic from current-lecture and lecture-number")
  (if (and (string? name-without-extension) 
           (> (string-length name-without-extension) 0))
    (set! next-lecture name-without-extension)))

(define (set-previous-lecture name-without-extension)
  (display-warning "The call of set-previous-lecture is ignored"))

(define (set-next-lecture name-without-extension)
  (display-warning "The call of set-next-lecture is ignored"))

; calculation of previous lecture from lecture counts
(define (calculate-previous-lecture! this-lecture last-lecture)
 (if (> last-lecture 1)
  (if (> this-lecture 1) 
      (set! previous-lecture (list-ref lecture-list (- this-lecture 2)))  ; adjusting because list-ref counts from 0
      (set! previous-lecture #f))
  (set! previous-lecture #f))
)

; calculation of next lecture from lecture counts
(define (calculate-next-lecture! this-lecture last-lecture)
 (if (> last-lecture 1)
  (if (< this-lecture last-lecture) 
      (set! next-lecture (list-ref lecture-list this-lecture))            ; actually adjusting because list-ref counts from 0
      (set! next-lecture #f))
  (set! next-lecture #f))
)
  

; arrange that slide headers are generated as frames.
; default: simple and plain slide headers
; this facility is still problematic
(define (generate-frame-slide-headers)
  (set! slide-header-as-frame? #t))


; ----------------------------------------------------------------------
; name and file related functions:

; Name of programs, excercises, and image series
; Presentation-kind is a symbol or string: slide, note, book
; Kind is a symbol or string: program, images, exercise.  Image is used for image series.
; Number is a non-zero integer: The number of element on slide.
; The optional subnumber is used to number sub element, such as individual images in an image series
; Programs, exercises, and image series are numbered pr. page, and individually pr. kind.
(define (page-element-name lecture-id page-id presentation-kind kind number . sub-number)
  (let ((sn (if (not (null? sub-number)) (car sub-number) #f)))
   (if (page-element-ok? presentation-kind kind)
       (string-append (as-string lecture-id) "-" (as-string page-id) "-"
                      (as-string presentation-kind) "-" (as-string kind) "-" (as-string number)
                      (if sn "-" "") (if sn (as-string sn) ""))
       (error (string-append "page-element-name: Parameter problems: " (as-string presentation-kind) " " (as-string kind))))))

(define (page-element-ok? pk k)
  (let ((pk1 (as-string pk))
        (k1  (as-string k)))
   (and 
     (or (equal? pk1 "slide") (equal? pk1 "note") (equal? pk1 "book") (equal? pk1 "show"))
     (or (equal? k1 "program") (equal? k1 "images") (equal? k1 "exercise") (equal? k1 "exercise-frame")))))

(define (slide-name-0 lecture-name slide-name)
  (string-append (as-string lecture-name) "-" "slide" "-" (as-string slide-name)))

(define (slide-name lecture-name slide-name)
  (if slide-header-as-frame?
     (filename-of-main-frame lecture-name slide-name)  
     (slide-name-0 lecture-name slide-name)))

(define (show-and-speak-slide-name lecture-name slide-name number)
  (string-append (as-string lecture-name) "-" "show" "-" (as-string slide-name) "-" (as-string number)))

; Naming of sound files in the speak directory
; Kind is either slide-part, program-part, exercise-part, or image-series-part (a symbol).
; Number is the part number from the show-and-speak clause.
; There may be one optional parameter: a subnumber in case kind is image-series-part
(define (speak-name lecture-id page-id kind number . optional-parameter-list)
 (let ((sub-number (optional-parameter 1 optional-parameter-list #f)))
  (string-append (as-string lecture-id) "-" (as-string page-id) "-" (as-string kind) 
                 "-" (as-string number)
                 (if sub-number
                     (string-append "-" (as-string sub-number))
                     ""))))

(define (note-name lecture-name slide-name)
  (string-append (as-string lecture-name) "-" "note"  "-" (as-string slide-name)))

(define (book-name lecture-name)
  (string-append (as-string lecture-name) "-" "book"))

(define (index-name lecture-name)
  (as-string lecture-name))

; Return a program name given lecture-id, page-id, presentation-kind (slide, note, book, show)
; and the program number on this page
(define (program-name lecture-id page-id presentation-kind number)
  (page-element-name lecture-id page-id presentation-kind 'program number))

(define (image-name lecture-name page-id presentation-kind number sub-number)
  (page-element-name lecture-id page-id presentation-kind 'images number sub-number))

; Return the name of an exercise. presentation-kind may be one of the symbols slide, note, book, as
; well as the primary view symbols slide-view, annotated-slide-view, and aggregated-view.
(define (exercise-name lecture-id page-id presentation-kind number)
 (let ((presentation-kind-1
         (cond ((memq presentation-kind '(slide note book)) presentation-kind)
               ((eq? presentation-kind 'slide-view) 'slide)
               ((eq? presentation-kind 'annotated-slide-view) 'note)
               ((eq? presentation-kind 'aggregated-view) 'book)
               (else 'slide))))
  (page-element-name lecture-id page-id presentation-kind-1 'exercise number)))

; Return the name of a pdf file - without extension.
(define (print-name . optional-parameter-list)
  (let ((lecture-id (optional-parameter 1 optional-parameter-list #f)))
    (if lecture-id
        (as-string lecture-id)
        "all")))

; The proper name of the HTML print page name which is genrated if make-print-page? is #t
(define (print-page-name)
  "leno-print-page")
  

; We don't want the students to guess the solution file names.
; Therefore we add a password-like constituent pr. lecture-id
(define (exercise-solution-name lecture-id page-id number)
 (let ((solution-password (defaulted-get (as-symbol lecture-id) solution-passwords "")))
  (string-append (as-string lecture-id) solution-password "-" (as-string page-id) "-" "solution" "-" (as-string number))))

; A variant of exercise-solution-name which returns a 'secret' file name. Only revealed to teaching assistants.
(define (exercise-secret-solution-name lecture-id page-id number)
  (string-append (as-string lecture-id) "-" (as-string page-id) "-" "aau25" "-" (as-string number)))

; The name of the frame file, which contains the exercise and the exercise manager
(define (exercise-frame-name lecture-id page-id presentation-kind number)
 (let ((presentation-kind-1
         (cond ((memq presentation-kind '(slide note book)) presentation-kind)
               ((eq? presentation-kind 'slide-view) 'slide)
               ((eq? presentation-kind 'annotated-slide-view) 'note)
               ((eq? presentation-kind 'aggregated-view) 'book)
               (else 'slide))))
  (page-element-name lecture-id page-id presentation-kind-1 'exercise-frame number)))

(define (end-show-and-speak-page-name lecture-id)
  (string-append (as-string lecture-id) "-" "end-show-speak"))

(define (internal-show-and-speak-page-name lecture-id)
  (string-append (as-string lecture-id) "-" "internal-show-speak"))


(define (word-index-name lecture-name)
  (string-append (as-string lecture-name) "-" "index"))

(define (source-program-index-name lecture-name)
  (string-append (as-string lecture-name) "-" "source-programs"))

(define (exercise-index-name lecture-name)
  (string-append (as-string lecture-name) "-" "exercise-index"))

(define (all-exercises-name lecture-name)
  (string-append (as-string lecture-name) "-" "all-exercises"))

(define (all-exercises-and-solutions-name lecture-name)
 (let ((solution-password (defaulted-get (as-symbol lecture-name) solution-passwords "")))
  (string-append (as-string lecture-name) solution-password "-" "all-exercises-solutions")))

; We could add a lecture-specific 'cryptic' string to this name:
(define (secret-all-exercises-and-solutions-name lecture-name)
  (string-append (as-string lecture-name) "-" "exercises-internal"))

; the name of the top navigation bar
(define (filename-of-top-navigation lecture-name slide-name)
  (string-append (as-string lecture-name) "-" "TOPBAR" "-" "slide" "-" (as-string slide-name)))

; the name of the frame which contains the main slide contents and the top navigation bar
(define (filename-of-main-frame lecture-name slide-name)
  (string-append (as-string lecture-name) "-" "FRAME" "-" "slide" "-" (as-string slide-name)))

; the name of the exercise manager control pane - teacher only 
(define (em-control-name lecture-name)
  (string-append (as-string lecture-name) "-" "em"))

; the name of lecture-name's title page
(define (title-page-name lecture-name)
  (string-append (as-string lecture-name) "-" "title-page"))

; the name of lecture-name's abstract page
(define (abstract-page-name lecture-name)
  (string-append (as-string lecture-name) "-" "abstract-page"))

; the name of the top level title page
(define (top-level-title-page-name)
  (string-append "top-level-title-page"))

; the name of the top level abstract page
(define (top-level-abstract-page-name)
  (string-append "top-level-abstract-page"))

; the name of the default trail, parallel with a normal lecture, which can be generated automatically,
; depending on the variable trail-of-lecture?
(define (default-trail-name lecture-id)
  lecture-id "-" "trail")

; The name of a side track hosted in host-lecture-id and host-page-id. In other words, in the context of
; host-lecture-id and host-page-id, we have some side track. 
; kind is either the symbol slide, note, book, or theme
; side-track-number is a the number of the side track relative to the current page.
(define (side-track-name host-lecture-id host-page-id side-track-number kind)
  (string-append (as-string host-lecture-id) "-" (as-string host-page-id) "-" (as-string kind) 
                 "-" "TRACK" "-" (as-string side-track-number)))

(define (side-track-navigation-frame-name host-lecture-id host-page-id side-track-number kind)
  (string-append (as-string host-lecture-id) "-" (as-string host-page-id) "-" (as-string kind) 
                 "-" "TRACKNAV" "-" (as-string side-track-number)))

; The path difference between the LENO source directory and the HTML target directory.
; Set via the XML-in-LAML leno-front-matters attribute source-destination-delta.
(define source-destination-delta-path-fragment "html/")

; The fragment of a file path which distinguishes the note-source-directory and the HTML destination directory.
; As default, the html files are placed in a subdirectory of the source directory named 'html'.
; (You can redefine this function after the leno software is loaded in order to provide for another html destination.)
; You should affect this variable by setting the leno-front-matters attribute source-destination-delta.
(define (relative-source-html-destination-path-fragment)
  source-destination-delta-path-fragment)

; The relative path from the html destination directory to the print pdf directory.
(define html-destination-pdf-path-fragment "../pdf/")

; The relative path from the html destination directory to the print pdf directory.
; (Can be redefined in post-notes)
; You should affect this variable by setting the leno-front-matters attribute html-pdf-delta.
(define (relative-html-destination-pdf-path-fragment)
  html-destination-pdf-path-fragment)


;; Copy the file in source-file-path (path relative to the note-source-directory) to a file in the destination-path in the
;; html destination directory (as determined by the function relative-source-html-destination-path-fragment).
;; The destination path is an optional second parameter, defaulted to the empty string.
;; The directories in destination-path is created if they do not exist.
;; The target file name is the same name as the source file, unless a optional third parameter explicitly
;; names the target file (name.extension).
;; .form (copy-source-to-destination source-file-path [destination-path destination-file-name])
(define (copy-source-to-destination source-file-path . optional-parameter-list)
  (let* ((real-destination-path (optional-parameter 1 optional-parameter-list ""))
         (target-file-name-par (optional-parameter 2 optional-parameter-list #f))
         (target-file-name (if target-file-name-par 
                               target-file-name-par  
                               (string-append (file-name-proper source-file-path) "." (file-name-extension source-file-path))))
        )
   (if (not (empty-string? real-destination-path))
       (ensure-directory-path-existence! (relative-source-html-destination-path-fragment) real-destination-path))
   (let ((source (string-append note-source-directory source-file-path))
         (target (string-append (relative-source-html-destination-path-fragment) real-destination-path target-file-name)))
     (if (file-exists? target) (delete-file target))
     (copy-file source target))))



; This function determines the placement of the generated html files relative to note-source-directory.
(define (destination-path name)
 (let* ((name-string (as-string name))
        (lgt (+ (string-length name-string) (string-length target-extension) 1)))
  (if (> lgt preferred-maximum-filename-length)
      (display-warning (string-append "The filename " name-string " is longer than the preferred maximum ("
                                      (as-string preferred-maximum-filename-length) " chars). Actual length: " (as-string lgt))))
  (string-append note-source-directory  (relative-source-html-destination-path-fragment) name-string "." target-extension)))

(define (source-path name)
 (let* ((name-string (as-string name)))
  (string-append note-source-directory  name-string "." target-extension)))

(define (html-file name)
  (string-append name "." "html"))

(define (pdf-file name)
  (string-append name "." "pdf"))


; relative final path to an external file from the html directory
(define (external-html-file final-path)
  (string-append "external-html/" final-path))

(define relative-applet-path "./applets/")

; -----------------------------------------------------------------------------
; Locally addapted html version 1 functions.
; Must be named ...-LN


(define (img-LN file-name help-text . optional-parameter-list)
 (let ((width (optional-parameter 1 optional-parameter-list #f)))
  (if width
   (html4:img 'src (image-file file-name) 'title help-text 'alt help-text 'border 0 'width width)
   (html4:img 'src (image-file file-name) 'title help-text 'alt help-text 'border 0))))

(define (no-frames-button self-url)
  (a-tag-target self-url (img-LN "no-frames.gif" (text-choice "Bryd ud af mulig frame system" "Exit possible frame set")) "_top"))  ; only allowed _top



; -----------------------------------------------------------------------------

; url functions. All urls in current directory:

(define (front-index-url)
  "index.html")

(define (slide-url lecture-id slide-id)
  (string-append (slide-name lecture-id slide-id) ".html"))

(define (slide-show-and-speak-url lecture-id slide-id number)
  (string-append (show-and-speak-slide-name lecture-id slide-id number) ".html"))

(define (note-url lecture-id slide-id)
  (string-append (note-name lecture-id slide-id) ".html"))

(define (book-url lecture-id . slide-id1)
  (let ((slide-id (if (null? slide-id1) #f (car slide-id1))))
    (if slide-id
        (string-append (book-name lecture-id)  ".html" "#" (as-string slide-id))
        (string-append (book-name lecture-id)  ".html"))))

(define (primary-view-url lecture-id slide-id)
 (cond ((eq? the-primary-view 'slide-view) (slide-url lecture-id slide-id))
       ((eq? the-primary-view 'annotated-slide-view) (note-url lecture-id slide-id))
       ((eq? the-primary-view 'aggregated-view) (book-url lecture-id slide-id))
       (else (laml-error "primary-view-url: Unknown primary-view: " the-primary-view))))


; A LENO cross reference URL function which is good for making explicit cross references
; from one part of the material to another. 
; It is not possible to keep you inside the current view, because a (leno-url ...) form
; is only evaluated once.
; .form (leno-url lecture-id [slide-id])
(define (leno-url lecture-id . optional-parameter-list)
 (let ((slide-id (optional-parameter 1 optional-parameter-list #f)))
   (if slide-id
       (primary-view-url lecture-id slide-id)
       (contents-url lecture-id))))


(define (contents-url lecture-id)
  (string-append (as-string lecture-id)  ".html"))

(define (word-index-url lecture-id)
  (string-append (word-index-name lecture-id) ".html"))

(define (all-word-index-url lecture-id)
  ; a fixed file produced via note-index.laml
  (string-append "note-index.html"))

; Return an URL of an exercise formulation identified by lecture-id, page-id, and exercise-number.
; Presentation-kind is either the symbol 'slide or 'note.
(define (exercise-formulation-url lecture-id page-id presentation-kind exercise-number)
  (html-file (exercise-name lecture-id page-id presentation-kind exercise-number)))

; Return an URL of an exercise formulation identified by lecture-id and the exercise-id.
; This function finds the page and the exercise-number on the page via the internally generated -.exc files.
; Thus, this function assumes that the -.exc file of lecture-id exists. If not, this function
; returns "???" and issues a warning.
; presentation-kind is an optional parameter which defaults to the symbol 'note. Other possible value 'slide.
; Does never address an exercise manager frameset, but always the exercise formulation.
(define (exercise-formulation-url-given-id lecture-id exercise-id . presentation-kind)
 (let* ((presentation-kind-1 (if (not (null? presentation-kind)) (car presentation-kind) 'note))
        (exc-file-name (exercise-idx-file-name lecture-id))
        (lecture-exc-info (if (file-exists? exc-file-name)
                              (file-read exc-file-name)
                              '()))
        (exc-info (find-in-list 
                     (lambda (e) (eq? (third (car e)) exercise-id))
                     lecture-exc-info)))
  (if exc-info
      (let ((ids (first exc-info))
            (title (second exc-info))
            (p-numbers (third exc-info))
            (solution-status (fourth exc-info))
           )
        (let* ((lec-id (first ids))
               (page-id (second ids))
               (sl-number (fourth ids))
               (main-number (car p-numbers))
               (sub-number (cadr p-numbers)))
         (html-file ((if exercise-manager-support? exercise-name exercise-name) lec-id page-id presentation-kind-1 sl-number))))
      (begin
         (display-warning (string-append "Cannot find exercise " (as-string exercise-id) " in lecture " (as-string lecture-id) ". "
                                         "Using dummy URL"))
         "???"))))


(define (exercise-solution-url lecture-id page-id exercise-number)
  (html-file (exercise-solution-name lecture-id page-id exercise-number)))

(define (local-html-url file-name)
  ; html url in current directory
  (string-append file-name ".html"))



(define about-laml-url "leno-help-and-about.html")  ; generated from this source file





; Interface function, used directly i LAML source file:
; OBSOLETE

; (define (inline-from-file file-name . mark)
;   ; Read the contens of file-name, such as a program, and return the text read.
;   ; If a second mark parameter is given, just read between (and excluding) the marks.
;   (if (null? mark)
;       (read-text-file file-name)
;       (read-text-file-between-marks file-name mark)))
; 
; (define (external-from-file file-name id title . mark)
;   ; Read the contents of file-name, such as a program. Write it to a html file
;   ; (derived from unique id parameter, a symbol).
;   ; Title names the program (string).
;   ; and return a link (a-tag) to it.
;   ; If a second mark parameter is given, just read between (and excluding) the marks.
;  (let ((progr-text 
;             (if (null? mark)
;                 (read-text-file file-name)
;                 (read-text-file-between-marks file-name mark)))
;        (name-of-program-file (program-name lecture-id id))
;      )
;    (write-text-file
;      (program-slide
;         (con "Program " title)
;         (program-text (pre progr-text)))
;      (destination-path name-of-program-file))
;   
;    (a-tag (string-append name-of-program-file ".html") title) ))


; --------------------------------------------------------------------------------------------------------------
; SVG

(define svg-download-url "http://www.adobe.com/svg/viewer/install/")
(define svg-download-url-firefox "http://plugindoc.mozdev.org/windows-all.html#AdobeSVG")


(define please-download-svg-player #f) ; assigned in do-begin-notes


; ---------------------------------------------------------------------------------------------------------------

; Leno CSS support.

; Does these notes apply css styling? - In XML-in-LAML it reflects the existence the css-stylesheet attribute
(define apply-css-styling? #f)

; The proper file name of the CSS style sheet.
; The value of this variable is a string if and only if apply-css-styling? is true.
(define the-css-stylesheet-name #f) 

; The similar name of the prestylesheet. A prestylesheet is a normal CSS stylesheet, which
; comes before the-css-stylesheet-name, and which is intended for font size information.
(define the-css-prestylesheet-name #f) 

; A boolean value that controls the actual copying of a stylesheet file name (either from the LENO software dir
; of the LENO source dir to the HTML target dir. #t in normal use. In development situations, it may be handy to 
; set it to false, however, in order to avoid constant overwriting of the HTML target stylesheets/stylesheet.css file.
(define css-stylesheet-do-copying #t) 


; Return a div enclosed contents of kind.
; leno-clause is a name or symbol in the list possible-leno-elements.
; view is one of slide, note, or book.
; .form (leno-css-styled view leno-clause content [id])
(define (leno-css-styled view leno-clause content . optional-parameter-list)
 (let* ((id (optional-parameter 1 optional-parameter-list #f))
        (id-list (if id (list 'id id) '()))
       )
  (div 'class (leno-css-class view leno-clause) id-list content)))

; As line-css-styled, but using span instead of div
(define (leno-css-styled-span view leno-clause content . optional-parameter-list)
 (let* ((id (optional-parameter 1 optional-parameter-list #f))
        (id-list (if id (list 'id id) '()))
       )
  (span 'class (leno-css-class view leno-clause) id-list content)))

; A variant of leno-css-styled with an extra parameter for an additional distinction.
; Provided separately in order to pass parameters in a convenient order.
; .form (leno-css-styled-extra view leno-clause extra content [id])
(define (leno-css-styled-extra view leno-clause extra content . optional-parameter-list)
 (let* ((id (optional-parameter 1 optional-parameter-list #f))
        (id-list (if id (list 'id id) '()))
       )
  (div 'class (leno-css-class view leno-clause extra) id-list content)))

; Like leno-css-styled-extra, but using span instead of div
(define (leno-css-styled-extra-span view leno-clause extra content . optional-parameter-list)
 (let* ((id (optional-parameter 1 optional-parameter-list #f))
        (id-list (if id (list 'id id) '()))
       )
  (span 'class (leno-css-class view leno-clause extra) id-list content)))

; Return the class name of leno-clause in view.
; The first optional parameter is typically a leno-clause, such as title, point, etc.
; The second optional parameter is an additional detailing of the construct, such as captions of images etc.
(define (leno-css-class view . optional-parameter-list)
 (let ((leno-clause (optional-parameter 1 optional-parameter-list #f))
       (extra-name-component (optional-parameter 2 optional-parameter-list #f)))
  (string-append "leno-" (as-string view) 
                  (if leno-clause 
                      (string-append "-" (as-string leno-clause))
                      "")
                  (if extra-name-component 
                      (string-append "-" (as-string extra-name-component))
                      ""))))

; A list of style sheet file names, as found in the stylesheet directory of the LENO software.
; To be copied to the target directory by end-notes.
(define style-sheets
  (list "leno-slide-fancy-02.css" "leno-slide-basic.css" "leno-slide-font.css"))

; A list of note specific style sheet file names, as found in the stylesheet directory of your note source directory.
; To be copied to the target directory by end-notes.
; Can be redefined in a LENO laml source file.
(define note-specific-style-sheets '())


; The funtions linking-style-sheets and actual-stylesheets represent an older and more complictated
; design of the style sheet management. In the current version, there is one fixed 'linking style sheet',
; which is also the one returned (with relative path from the HTML target dir) by the function
; actual-stylesheets.

(define (linking-style-sheets) (list "stylesheet.css"))

; The optional parameter controls the actual addressing of the stylesheet file. If #t, the relative-source-html-destination-path-fragment
; is injected in the addressing.
(define (actual-stylesheets apply-css-styling? . optional-parameter-list)
 (let ((from-source? (optional-parameter 1 optional-parameter-list #f)))
  (if apply-css-styling?
      (map (lambda (sh) (string-append (if from-source? (relative-source-html-destination-path-fragment) "") "stylesheets/" sh)) (linking-style-sheets))
      #f)))

; -----------------------------------------------------------------------------

; Constants

(define (leno-icon)
  (a 'href leno-homepage-url (img-LN "leno-icon-2.gif" "LENO - LEcture NOtes with LAML technology")))

(define leno-homepage-url "http://www.cs.auc.dk/~normark/leno/")

(define (make-color r g b) (list r g b))

(define yellow-1 (make-color 255 255 160))
(define purple-1 (make-color 204 126 250)) 
(define purple-2 (make-color 183 73 248))

(define slide-font-size 5)
(define slide-background-color white)
(define tabular-slide-color (make-color 255 255 193))
(define tabular-note-color tabular-slide-color)
(define tabular-book-color white)
(define title-color blue)

(define note-font-size-left 3)  ; font size in left part
(define note-font-size-right 2) ; font size in right part
(define note-background-color white)
(define note-foreground-color blue)

(define line-number-bg-color (make-color 230 230 230))

(define book-header-foreground-color black)
(define book-title-foreground-color blue)
(define book-foreground-color black)
(define book-background-color white)
(define book-font-size 3)

(define syntax-frame-color (make-color 243 226 50))  ;  before: (245 232 135) (248 245 165)
(define synopsis-frame-color green1)

(define concept-background  (make-color 6 22 96))


; -----------------------------------------------------------------------------
; Variables to be assigned once or more

(define lecture-number #f)  ; default: do not show lecture number 
(define previous-lecture #f)  ; default: do not show previous lecture
(define next-lecture #f)  ; default: do not show next lecture
(define word-index '())  ; a list of index-elements. An element is of the form (word url)

; ----------------------------------------------------------------------
; Word indexing:

; Make all the non-blank words in words (a list of strings) refer to url
; kind is a symbol, such as 'index, 'title, 'concept, etc
(define (add-words-to-index words lecture-id page-id kind)
 (let ((reference-list 
         (map (lambda (word) (make-word-index-entry word lecture-id page-id kind))
              (filter (negate blank-string?) words))))
   (set! word-index (append word-index reference-list))))

; Return the full path of an idx file for id.
; Id defaults to the value of the global varible lecture-id
(define (word-index-file-name . id)
 (let ((id-1 (if (null? id) (as-string lecture-id) (as-string (first id)))))
   (string-append note-source-directory "internal/" id-1 ".idx")))

; Word index constructor:

(define (make-word-index-entry word lecture-id page-id kind)
  (list word lecture-id page-id kind))

; Selectors:

(define word-of-index-entry (make-selector-function 1 "word-of-index-entry"))
(define lecture-id-of-index-entry (make-selector-function 2 "lecture-id-of-index-entry"))
(define page-id-of-index-entry (make-selector-function 3 "page-id-of-index-entry"))
(define kind-id-of-index-entry (make-selector-function 4 "kind-id-of-index-entry"))


; ----------------------------------------------------------------------

; We collect cross references in a file per lecture
; in order to ease spotting of wrong references

; Return the full path of an crs file for id.
; Id defaults to the value of the global varible lecture-id
(define (cross-ref-file-name . id)
 (let ((id-1 (if (null? id) (as-string lecture-id) (as-string (first id)))))
   (string-append note-source-directory "internal/" id-1 ".crs")))

; ----------------------------------------------------------------------

; We collect front matter information in a front info file name,
; mainly in order to transfer information from the primary LENO source
; to the secondary theme source.

; Return the full path of a frt file for id.
; Id defaults to the value of the global varible lecture-id
(define (front-matter-info-file-name . id)
 (let ((id-1 (if (null? id) (as-string lecture-id) (as-string (first id)))))
   (string-append note-source-directory "internal/" id-1 ".frt")))




; ----------------------------------------------------------------------


(define book-port 'nil)

; Define the lecture intro info in terms of a mandatory title and optional
; author/affiliation list, abstract, and date.
; Notice that the title is related to a single lecture, as opposed to notes-title.
; Also notice that the abstract is this lecture's abstract, as a contrast to the note-abstract.
; If you pass #f as author-list, the default - author-and-affiliation-list - will be used.
(define (lecture-intro title . optional-parameter-list)
 (let* ((author-list (optional-parameter 1 optional-parameter-list author-and-affiliation-list))
        (author-list-1 (if (not author-list) author-and-affiliation-list author-list))
        (abstract (optional-parameter 2 optional-parameter-list ""))
        (date (optional-parameter 3 optional-parameter-list ""))
       )
  (set! lecture-title title)
  (set! lecture-author-info author-list-1)
  (set! lecture-abstract abstract)
  (set! lecture-date date)
))


; begin-notes defined in lecture-notes.scm as an alias of do-end-notes.
(define (do-begin-notes)

 (check-primary-view!)

 ; First re-assign lecture-sections to the original form: A list of entries of the form ("lecture-id" "lecture-title").
 (if (simple-list-lecture-sections-format? lecture-sections)
     (set! lecture-sections
       (map (lambda (lid) (list lid (find-original-chapter-title lid))) lecture-sections)))


 (if (not lecture-titles)  ; take the titles from aux files
     (set! lecture-titles
           (map cadr lecture-sections)))


 (ensure-directory-existence! note-source-directory "internal")

 ; only make html directory in the simple case where it resides as a subdirectory in the note-source-directory
 (if (equal? "html/" (relative-source-html-destination-path-fragment))
     (ensure-directory-existence! note-source-directory "html"))

 (ensure-directory-existence! (string-append note-source-directory (relative-source-html-destination-path-fragment)) "images")
 (ensure-directory-existence! (string-append note-source-directory (relative-source-html-destination-path-fragment)) "javascript")
 (ensure-directory-existence! (string-append note-source-directory (relative-source-html-destination-path-fragment)) "graphics")
 (ensure-directory-existence! (string-append note-source-directory (relative-source-html-destination-path-fragment) "graphics/") "small")
 (ensure-directory-existence! (string-append note-source-directory (relative-source-html-destination-path-fragment)) "lecturer-photos")
 (ensure-directory-existence! (string-append note-source-directory (relative-source-html-destination-path-fragment)) "stylesheets")
 (ensure-directory-existence! (string-append note-source-directory (relative-source-html-destination-path-fragment)) "source-programs")

 (if do-clean-html-directory?
     (do-clean-leno-target-directory (string-append note-source-directory (relative-source-html-destination-path-fragment))))



 (if help-and-about-page? (make-leno-help-page))

 (let ((book-file (destination-path (book-name lecture-id))))

;  (if (equal? lecture-title "") (display-warning "Remember that (begin-notes) must come after (lecture-intro ...)"))

  (if (and lecture-number (number? lecture-number))  ; a the lecture number has been set
      (calculate-previous-lecture! lecture-number current-lecture))

  (if (and lecture-number (number? lecture-number))  ; a the lecture number has been set
      (calculate-next-lecture! lecture-number current-lecture))


  (if (file-exists? book-file) (delete-file book-file))
  (if make-aggregated-view?
    (set! book-port (open-output-file (destination-path (book-name lecture-id)))))

  (if make-aggregated-view?  
   (write-string-to-port   ; as of now, no alternative without javascript key event handling
     (pre-page-with-keypress-script 
      (con (text-choice "Forelæsningsnoter" "Lecture notes") ": " lecture-title)
      '()

      javascript-loading
      (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
      (js-call "lenoNavigate" 
                (map string-it-single 
                     (list (if previous-lecture (book-url previous-lecture) "")
                           (if next-lecture (book-url next-lecture) "") 
                           (contents-url lecture-id)
                           "" 
                           "" "" "" )))

      white black black black
      mouse-advancement
      )
    book-port))

  ; write author, affiliation, and abstract to bookport:
  (if make-aggregated-view?
   (if (not (equal? lecture-abstract ""))
    (write-string-to-port
     (con 
      (font-1 7 book-title-foreground-color 
        (if (and lecture-number (number? lecture-number) (> (length lecture-list) 1))
            (b (con (text-choice "Kapitel" "Chapter") " " (as-string lecture-number) (br) lecture-title))
            (con (b lecture-title)))) (p)

     (con
       (font-size 5 (brl lecture-author-info)) (br)
       (font-size 4 lecture-date))

     (if (>= news-flash-level 4) (leno-news-flash 3) "")

     (p) (hr-1) (p)
     (table-1 0 '(180 12 500) (make-list 3 book-background-color) 
       (list (list (con (font-1 4 book-title-foreground-color (text-choice "Sammendrag" "Abstract")) (br)
                        (font-size 1 
                         (con-space
                          (if previous-lecture (a-tag (book-url previous-lecture) (text-choice "Forrige lektion" "Previous lecture")) "")
                          (if next-lecture (a-tag (book-url next-lecture) (text-choice "Næste lektion" "Next lecture")) "") (br)
                          (a-tag (all-word-index-url lecture-id) (text-choice "Stikord" "Index"))
                          (a-tag "#REFERENCES" (text-choice "Referencer" "References"))
                          (a-tag (contents-url lecture-id) (text-choice "Indhold" "Contents"))
                        )))
                   "" (i (font-1 3 black lecture-abstract)) )))
     (p)
    )
    book-port)))
  )


  ; The lecture-list data structure from the course plan facility, or '()
  (set! lecture-list-from-course-plan
   (if course-dir
    (let* ((internal-lecture-list-file (string-append course-dir "internal/lecture-list.lsp"))
           (lecture-list-file-exits? (file-exists? internal-lecture-list-file)))
      (if lecture-list-file-exits?
          (display-message "The lecture-list data structure from the course plan has been read")
          (display-message "The lecture-list data structure could not be located - only a problem if using the exercise manager"))
      
      (if lecture-list-file-exits?
          (file-read internal-lecture-list-file)
          '()))
     '()))

  (if (>= lecture-note-verbose-mode 1)
      (display-message (string-append "PROCESSING THE LECTURE: " lecture-title)))

  (if (>= lecture-note-verbose-mode 1)
      (display-message (string-append "Exercise model: " (as-string used-exercise-model))))

  (let ((prev-speak-filename (speak-file-name lecture-id)))
    (set! previous-speak-list-in-this-lecture 
      (if (file-exists? prev-speak-filename)
          (file-read prev-speak-filename)
          #f)))

  ; assign a flat list of all bibtex entries to all-bibtex-entries
  (set! all-bibtex-entries 
        (flatten (map file-read bibtex-files)))

  (set! please-download-svg-player (text-choice 
    (con "For at se dette billede bedes du downloade og installere " (a-tag svg-download-url "Adobe's SVG plugin") "." "I Firefox gå til" (a-tag svg-download-url-firefox "denne side."))
    (con "To see this image you must download and install the " (a-tag svg-download-url "SVG plugin from Adobe") "." "In Firefox please consult" (a-tag svg-download-url-firefox "this page."))))


)


;----------------------------------------------------------------------

; page sequencing administration:

; format of aux file:
; ("lecture title" (slide-title-string slide-id-symbol section-boolean)*)

; Return the full path of the aux file for id.
; Id defaults to the value of the global varible lecture-id
(define (aux-file-name . optional-parameter-list)
 (let ((id-1 (as-string (optional-parameter 1 optional-parameter-list lecture-id))))
   (string-append note-source-directory "internal/" id-1 ".aux")))

(define note-page-list '())   ; for collection of note page ids in this run
(define note-title-list '())
(define remembered-old-page-list '())  ; to facilitate that the note page list from previous run is only read once
(define remembered-old-aux-structure '()) ; super stucture of remembered-old-page-list - 
                                          ; (Aug 2, 2002: I needed more info than that in remembered-old-page-list)

(define separate-elements-on-current-page '()) ; a list of external programs, exercises, etc
                                               ; Entries of the form (kind number-on-page)

; Return the aux structure from the internal aux file
; cannot be a variable because it - in that case - is read before the file name is known.
; Accepts an optional lecture-id parameter
(define (old-note-page-list . optional-parameter-list)
 (let ((id (as-string (optional-parameter 1 optional-parameter-list lecture-id))))
  ; note page ids from previous run
  ; Please notice the caching in remembered-old-page-list
  (cond ((not (null? remembered-old-page-list)) remembered-old-page-list)
        ((file-exists? (aux-file-name id)) 
           (let* ((aux-structure (file-read (aux-file-name id)))
                  (onpl (prepare-aux aux-structure)))
             (set! remembered-old-aux-structure aux-structure)
	     (set! remembered-old-page-list onpl)
	     onpl))
        (else '()))))

(define (prepare-aux lst)
  ; return the note-page-list, i.e. a list of slide-id symbols
  ; lst starts with lecture-title and then has a strings of (title slide-id section-boolean) tripples
  (map id-of-aux-entry (entries-of-aux-file lst)))

(define page-number 0) ; The number of the current note page. Assigned later.
(define page-id 'nil)  ; the id of the current note page. Assigned later.
(define page-element-id #f) ; The id of the the current element on the current note page. Assigned by the procedure present-element. #f if no id is provide.
(define page-title 'nil) ; the title of the current note page. Assigned later.
(define current-exercise-id 'nil) ; the id of an exercise - symbol. Assigned when we meet an exercise.
(define current-show-and-speak-specification '()) ; the show and speak clause of the current-page
(define current-show-and-speak-count 1)  ; used to guide source file names of sound files
(define corresponding-theme-url #f) ; assigned in original-note-page
(define page-svg-number 0) ; A number used for unique numbering of inlined svg contents relative to a note page

; Form and return an id (a string) composed by the note-page id and the id of the immediate constitutent of a note page. 
; For multi-lecture material, we also include the lecture-id.
; An optional extra parameter is string-appended if present.
; In such materials, the id is composed of the lecture-id, the page-id and the the id of the surrounding constitutent of a note page. 
; Notice that a lecture-id and page-id always are defined within the scope of a note-page, but that page-element-id only is defined occasionally, namely if an id is passed to the element.
; In case no page-element-id is defined, this function returns #f.
; .form (aggregated-page-element-id [extra])
(define (aggregated-page-element-id . optional-parameter-list)
 (let ((extra (optional-parameter 1 optional-parameter-list "")))
  (let ((lecture-contribution
         (if (> (length lecture-sections) 1)
             (string-append (as-string lecture-id) "-")
             ""))
        (extra-contribution
         (if (empty-string? extra) "" (string-append "-" extra)))
       )
    (if page-element-id
	(string-append lecture-contribution (as-string page-id) "-" (as-string page-element-id) extra-contribution)
	#f))))

; Form a return an id (a string) of a body of a note page.
(define (aggregated-page-body-element-id)
 (let ((lecture-contribution
         (if (> (length lecture-sections) 1)
             (string-append (as-string lecture-id) "-")
             "")))
   (string-append lecture-contribution (as-string page-id))))

; For a LENO element (a subelement of a note page) return an id list (id val) ready to 
; be spliced into some HTML form. The value of the id is the value of (aggregated-page-element-id), which
; in turn depends of the two global variables page-id and page-element-id. Return the empty list of
; no aggregated id can be formed.
(define (id-attribute-contribution)
  (let ((aggregated-id (aggregated-page-element-id)))
    (if aggregated-id (list 'id aggregated-id) '())))

(define (previous-page)
 ; uses the global variable page-number and the value of (old-note-page-list) to return the name of the previous page.
 ; returns #f in case we are located at the first page.
 (let ((old-length (length (old-note-page-list))))
  (cond ((or (< page-number 1) (> page-number old-length)) #f) 
        ((= 1 page-number) #f)
        (else (list-ref (old-note-page-list) (- page-number 2))))))

(define (next-page)
 ; uses the global variable page-number and the value of (old-note-page-list) to return the name of the next page.
 ; returns #f in case we are located at the last page.
 (let ((old-length (length (old-note-page-list))))
  (cond ((or (< page-number 1) (> page-number old-length)) #f)
        ((= old-length page-number) #f)
        (else (list-ref (old-note-page-list) page-number)))))


(define (current-page)
 ; uses the global variable page-number and the value of (old-note-page-list) to return the name of the current page.
 (let ((old-length (length (old-note-page-list))))
  (cond ((or (< page-number 1) (> page-number old-length)) #f) 
        (else (list-ref (old-note-page-list) (- page-number 1))))))

(define (total-number-of-slides)
  (length (old-note-page-list)))


; -----------------------------------------------------------------------------
; Support for html saving programs, read from the source

; Numbering pr. notepage. We use identical numbering on slides, notes, and books.
(define program-number 1)
(define exercise-number 1)
(define image-number 1)
(define side-track-number 1)

(define (reset-page-element-numbers) 
  (set! next-exercise-number next-exercise-number-0)
  (set! program-number 1)
  (set! exercise-number 1)
  (set! image-number 1)
  (set! side-track-number 1)
)

; As reset-page-element-numbers-1, but does not reset next-exercise-number
(define (reset-page-element-numbers-1) 
  (set! program-number 1)
  (set! exercise-number 1)
  (set! image-number 1)
  (set! side-track-number 1)
)

; number is the number of the program, relative to the externally presented program on this page.
(define (save-slide-program prog-text file-name context-url left-url right-url annotation-url show-and-speak-context-url caption 
                            proper-source-program-file-name number background-color)
 (let ((program-show-and-speak-clause (filter (speak-select-number 'program-part number) current-show-and-speak-specification)))
  (write-text-file
   (program-slide
      context-url left-url right-url annotation-url
      "Program"
      (pre prog-text)
 
;       (let* ((number-of-lines (number-of-lines-in-text prog-text))
;              (line-number-text (line-number-text-column number-of-lines)))
;         (table
;           (tr (td 'width "15" 'bgcolor (rgb-string-list line-number-bg-color) 'valign "top" line-number-text)
;               (td 'width "*"  'valign "top" (pre prog-text)))
;         ))

      file-name 
      caption
      proper-source-program-file-name
      number 
      background-color)
   (destination-path file-name))

  ; @a show-and-speak program slide:
  (if (and show-and-speak? (= 1 (length program-show-and-speak-clause))) ; @b
      (let* ((the-clause (first program-show-and-speak-clause))
             (clause-number (speak-clause-number the-clause))
             (clause-time (speak-clause-time the-clause))
             (clause-next-name (speak-clause-next-name the-clause))
            )
       (if (and make-faked-show-and-speak-files? (not (zero-time-page? clause-time)))
        (make-faked-show-and-speak-file! (speak-name lecture-id page-id 'program-part clause-number)))

       (write-text-file
        (show-and-speak-program
         show-and-speak-context-url
         "Program"
         (con 
           (background-speaker-sound  (speak-name lecture-id page-id 'program-part clause-number))
           (pre prog-text)
         )
         (html-file clause-next-name)
         clause-time
         number
        )
        (destination-path (program-name lecture-id page-id 'show clause-number)))       
       )
   )
 )
)

(define (save-note-program prog-text doc-text file-name context-url left-url right-url annotation-url number caption)
 (write-text-file
   (program-note
      context-url left-url right-url annotation-url
      "Program"
      (pre prog-text)
      doc-text 
      number
      caption
   )
   (destination-path file-name)))


(define (save-book-program prog-text file-name context-url)
 (write-text-file
   (program-book
      context-url
      "Program"
      (pre prog-text))
   (destination-path file-name)))

; OBSOLETE - see hereafter
; asumes that the title is the first element on a note page:
(define (title-of-elements elements)
  ; return the title string from the elements of a slide
  (car (element-contents (car elements))) )

; more general version of title-of-elements.
; does not assume any particular position of title
(define (title-of-elements elements)
  ; return the title string from the elements of a slide
 (let ((section-title-element (find-element 'section-title elements))
       (title-element (find-element 'title elements)))
  (cond (section-title-element (car (element-contents section-title-element)))
        (title-element (car (element-contents title-element)))
        (else "No title"))))

(define (find-element kind element-list)
  ; return the first element of kind in element-list.
  ; If no such element is found, return #f
  (cond ((null? element-list) #f)
        ((eq? kind (element-tag (car element-list))) (car element-list))
        (else (find-element kind (cdr element-list)))))


; extraction of keywords (index-words and concept names) to the meta tag:

; Return a list of strings, which are the extracted keywords in elements.
(define (extract-note-page-keywords elements)
  (cond ((null? elements) '())
        ((and (eq? 'index-words (element-tag (car elements))) (not (null? (element-contents (car elements)))))
            (append (element-contents (car elements)) (extract-note-page-keywords (cdr elements))))
        ((and (eq? 'concept-list (element-tag (car elements))) (not (null? (element-contents (car elements)))) 
            (append (concept-words-of-concept-list (car elements)) (extract-note-page-keywords (cdr elements)))))
        (else (extract-note-page-keywords (cdr elements)))))

; given a concept-list, return a list of 
(define (concept-words-of-concept-list a-concept-list)
  (let ((list-of-concepts (element-contents a-concept-list)))
    (map (lambda(concept) (concept-name (element-contents concept))) list-of-concepts)))
        


; -----------------------------------------------------------------------------

; Support for html saving exercises, read from the source

; The number of the next exercise in this lecture.
; This number is specific to a lecture, not to a single page.
; See also the variable exercise-number, which is page specific.
(define next-exercise-number 1)

; The start value of next-exercise-number on a note-page
; This variable is used to control exercise numbering accross slides, notes, and book styles.
; It is also used in the theme stuff.
(define next-exercise-number-0 1)

(define (save-slide-exercise title text file-name context-url left-url right-url solution-url number rank)
 (let ((hier-num (if (and lecture-number (number? lecture-number))
                     (con (as-string lecture-number) "." (as-string number))
                     (as-string number)))
      )
                          
  (write-text-file
   (exercise-slide
      context-url
      left-url right-url
      (con-space (text-choice "Opgave" "Exercise") hier-num (br) title (exercise-rank-presentation rank))
      text
      solution-url
      number
      title)
   (destination-path file-name))))


; rank is either false or a numeric string
; Return a string that somehow ranks the exercise.
(define (exercise-rank-presentation rank)
  (if rank
      (let ((rank-number (as-number rank)))
         (font-color red (make-string rank-number #\*)))
      ""))      

(define (save-note-exercise title text file-name context-url solution-url left-url right-url number rank)
 (let ((hier-num (if (and lecture-number (number? lecture-number))
                     (con (as-string lecture-number) "." (as-string number))
                     (as-string number))))
  (write-text-file
   (exercise-note
      context-url left-url right-url
      (con-space (text-choice "Opgave" "Exercise") hier-num (br) title (exercise-rank-presentation rank))
      text
      solution-url
      number
      title
   )
   (destination-path file-name))))

; Save the HTML exercise frame file
(define (save-note-frame-exercise-file lecture-id page-id exercise-id exercise-title exercise-file-name relative-solution-url)
  (let* ((absolute-solution-url (course-absolute-url (string-append "html/noter/" relative-solution-url)))
         (frame-file-name (exercise-frame-name lecture-id page-id 'note exercise-number))
         (emu (exercise-manager-url "exercise-companion-frame.cgi"
                                   (list 'lecture-id 'exercise-id 'exercise-title 'activation-mode 'solution-url 'language) 
                                   (list  lecture-id exercise-id  exercise-title  'non-save absolute-solution-url language-preference))))
  (write-text-file
    (html4:frameset
            (con (html4:frame 'name "exercise-frame" 'src (html-file exercise-file-name) 'scrolling "yes")
                 (html4:frame 'name "exercise-manager-frame" 'src  emu 'scrolling "yes")
            )
            'rows "75%,25%" 'border 5 'bordercolor (rgb-string-list black))
    (destination-path frame-file-name))))


; Save the exercise solution.
(define (save-exercise-solution title solution-text solution-file-name context-url number)
 (let ((hier-num (if (and lecture-number (number? lecture-number))
                     (con (as-string lecture-number) "." (as-string number))
                     (as-string number))))
  (write-text-file
   (exercise-solution-note  
      context-url
      (con-space (text-choice "Opgaveløsning:" "Exercise solution:") (br) title)  ; May 4, 2010. I have dropped hier-num in exercise solutions, because it was inconsistent with the exercise number.
      solution-text                                                               ; Reason: save-exercise-solution is called both in slide and note context. It is not easy to find a good solution. Thus
   )                                                                              ; it is better to drop the number here than have a wrong number...
   (destination-path solution-file-name))))


; -----------------------------------------------------------------------------
; cross reference support

; The Cross reference constructors are part of the surface level - lecture-notes.scm
; To be used in note pages.

; Cross reference selectors:
; To be used in this software.

; (define (cross-ref-type record)
;   ; returns a symbol
;   (car record))
; 
; (define (cross-ref-title record)
;   (cadr record))
; 
; (define (cross-ref-reference record)
;   (if (eq? (cross-ref-type record) 'informal-reference)
;       (caddr record)
;       (error "No reference proper in this cross reference. Should be informal-reference to use this function")))
; 
; (define (cross-ref-url record)
;   (if (eq? (cross-ref-type record) 'internet-reference)
;       (caddr record)
;       (error "No URL in this cross reference")))
; 
; (define (cross-ref-lecture-id record)
;   (if (eq? (cross-ref-type record) 'note-reference)
;       (caddr record)
;       (error "No lecture id in this cross reference")))
; 
; (define (cross-ref-page-id record)
;   (if (eq? (cross-ref-type record) 'note-reference)
;       (cadddr record)
;       (error "No page id in this cross reference")))

; returns the type of the reference (a symbol)
(define (cross-ref-type reference-element)
  (as-symbol (element-tag reference-element)))

; returns the element attribute of the cross reference element
(define (cross-ref-attributes reference-element)
  (element-attributes reference-element))

; return the title of the reference. Applies to all types of references.
(define (cross-ref-title reference-element)
  (if (eq? (cross-ref-type reference-element) 'bibtex-reference)
      (let ((bib-entry (locate-bibtex-entry (cross-ref-key reference-element))))
        (if bib-entry 
            (bibtex-return-title bib-entry)
            "???"))
      (first (element-contents reference-element))))

; return the cross-reference of an informal reference 
(define (cross-ref-reference reference-element)
  (if (eq? (cross-ref-type reference-element) 'informal-reference)
      (second (element-contents reference-element))
      (error "No reference proper in this cross reference. Should be informal-reference to use this function")))

; return the url of a internet reference
(define (cross-ref-url reference-element)
  (if (eq? (cross-ref-type reference-element) 'internet-reference)
      (second (element-contents reference-element))
      (error "No URL in this cross reference. Should be an internet-reference element.")))

; return the lecture-id of a note-reference
(define (cross-ref-lecture-id reference-element)
  (if (eq? (cross-ref-type reference-element) 'note-reference)
      (second (element-contents reference-element))
      (error "No lecture id in this cross reference. Should be a note-reference")))

; return the page-id of a note-reference
(define (cross-ref-page-id reference-element)
  (if (eq? (cross-ref-type reference-element) 'note-reference)
      (third (element-contents reference-element))
      (error "No lecture id in this cross reference. Should be a note-reference")))

; return the key of a bibtex reference (a symbol).
(define (cross-ref-key reference-element)
  (if (eq? (cross-ref-type reference-element) 'bibtex-reference)
      (as-symbol (first (element-contents reference-element)))
      (error "No key of this reference. Should be a bibtex reference")))

; ----------------------------------------------------------
; Particular bibtex cross reference searching and accessors

; Find a bibtex record among the entries in bibtex-files.
; Return #f if none is found
(define (locate-bibtex-entry key)
  (find-in-list
    (lambda (bibentry) 
      (let ((bibkey (get 'key bibentry)))
        (equal? (downcase-string bibkey) (downcase-string (as-string key)))))
    all-bibtex-entries))

; return the title of a bibtex-entry. 
; Returns a string in *all cases*, 
; even in the unlikely situation where there is no title.
; precond: bibtex-entry is not #f
(define (bibtex-return-title bibtex-entry)
 (defaulted-get 'title bibtex-entry "Unknown title"))

; Return a possible url field of a bibtex entry, of #f if none exists.
; precond: bibtex-entry is not #f
(define (bibtex-return-url bibtex-entry) 
 (defaulted-get 'url bibtex-entry #f))

; ---------------------------------------------------------

; Return the list of location hints of the record
; (define (cross-ref-location-hints record)
;  (let ((ctype (cross-ref-type record)))
;   (cond ((eq? 'note-reference ctype)     (fifth record))
;         ((eq? 'internet-reference ctype) (fourth record))
;         ((eq? 'informal-reference ctype) (fourth record))
;         (else '()))))

; Return the list of location hints of the cross-reference element
; The format is due to historical decissions.
(define (cross-ref-location-hints reference-element)
 (let ((ctype (cross-ref-type reference-element))
       (ec (element-contents reference-element))
      )
  (cond ((eq? 'note-reference ctype)     (cons 'note-reference (if (> (length ec) 3) (fourth ec) '())))
        ((eq? 'internet-reference ctype) (cons 'internet-reference (if (> (length ec) 2) (third ec) '()) ))
        ((eq? 'informal-reference ctype) (list 'informal-reference))
        ((eq? 'bibtex-reference ctype) (cons 'bibtex-reference (if (> (length ec) 1) (second ec) '())))
        (else '()))))



; Cross reference display procedures (for error messages):

(define (display-crs-entry crs-entry)
 (let ((type (cross-ref-type crs-entry)))
   (cond ((eq? type 'informal-reference) (display-informal-ref crs-entry))
         ((eq? type 'internet-reference) (display-internet-ref crs-entry))
         ((eq? type 'note-reference) (display-note-ref crs-entry))
         ((eq? type 'bibtex-reference) (display-bibtex-ref crs-entry))
         (else (error (string-append "display-crs-entry: Unknown cross reference type: " (as-string type)))))))

(define (display-informal-ref crs-entry)
  (display "An informal cross reference"))  ; put details in if necessary

(define (display-internet-ref crs-entry)
  (display "An internet reference"))        ; put details in if necessary

(define (display-note-ref crs-entry)
 (let ((title (cross-ref-title crs-entry))
       (lecture (cross-ref-lecture-id crs-entry))
       (page (cross-ref-page-id crs-entry)))
   (display (string-append "Target lecture: " (as-string lecture) "      " "Target page: " (as-string page) "      "))
   (display (string-append "Title: " title)) 
))

(define (display-bibtex-ref crs-entry)
  (display "A bibtex reference"))        ; put details in if necessary



  


; -----------------------------------------------------------------------------
; The function which generates an appropriate formatter function for
; a cross reference record.

(define (generate-cross-reference-formatter url-function)
  ; given an url function (one of slide-url, note-url and book-url)
  ; the url function allows book-to-book references and slide-to-slide references.
  ; return a function which can format a cross reference record.
  ; Depends on the function cross-reference-location-hints, which is not supplied in this file.
  ; cross-reference-location-hints is normally defined in notes.scm together with the lectures.
  ; In the simple case, it just returns an empty string.
  ; The generated function returns a list of two elements: an url and the hint (both strings).
  (lambda (cross-ref-record)
    (let ((descr (cross-ref-title cross-ref-record))
	  (type (cross-ref-type cross-ref-record))
          (attributes (cross-ref-attributes cross-ref-record)) ; attributes as provided by xml-in-laml
	  (loc-hint (cross-reference-location-hints (cross-ref-location-hints cross-ref-record)))
	  )
     (let ((target (defaulted-get 'target attributes #f))) ; only available in xml-in-laml
      (cond ((eq? type 'internet-reference)
	     (let ((url (cross-ref-url cross-ref-record)))
	       (list (if target (a-tag-target url descr target) (a-tag url descr)) loc-hint)))
	    ((eq? type 'note-reference)
	     (let ((lect-id (cross-ref-lecture-id cross-ref-record))
		   (page-id (cross-ref-page-id cross-ref-record))
		   )
	       (list (if target (a-tag-target (url-function lect-id page-id) descr target) (a-tag (url-function lect-id page-id) descr))
                     loc-hint)))
	    ((eq? type 'informal-reference)
	     (let ((informal-ref (cross-ref-reference cross-ref-record)))
	       (list (string-append descr ": " informal-ref) loc-hint)))
	    ((eq? type 'bibtex-reference)
	     (let* ((bibtex-entry (locate-bibtex-entry (cross-ref-key cross-ref-record)))
		    (url (if bibtex-entry (bibtex-return-url bibtex-entry) #f))
		    )
	       (if url 
		   (list (if target (a-tag-target url descr target) (a-tag url descr)) loc-hint)
		   (list descr loc-hint))))
	    (else (error "Problems in format-cross-ref. Second item is unknown"))))
    ))
)

; Given that refs is a list of references, each a reference (url) and a hint
; Glue the references and hints together.
; Return a list of glued links and hints
(define (glue-ref-and-hint refs)
  (map (lambda (url-hint-pair) (string-append (car url-hint-pair) (cadr url-hint-pair))) refs))


; -----------------------------------------------------------------------------
; The original note page:

(define (original-note-page importance id . elements-0)

 (if (not (note-page-id-valid? id))
     (laml-error "A note page id must be composed of characters (lower case only), numeric, and '-' characters only. Offending id: " id))

 (set! page-id id)			; for convenient direct variable reference below
 (set! separate-elements-on-current-page '())
 (set! page-svg-number 0)

 (let ((elements (do-element-splicing elements-0)))
 
   (if (>= lecture-note-verbose-mode 1) (begin (display (string-append (as-string lecture-id) ":" (as-string id) "  ")) ))

   (let ((title-element (title-of-elements elements))
	 (long-slide? (find-element 'long-slide elements))
	 (section-start? (turn-into-boolean (find-element 'section-title elements)))
	 (element-keywords (extract-note-page-keywords elements))
	 (show-and-speak-clause (find-element 'show-and-speak elements))
         (separate-page-elements (filter separate-page-element? elements))
	)

     (set! note-page-list (cons id note-page-list))
     (set! page-number (+ page-number 1))
     (set! note-title-list (cons (make-aux-entry title-element id section-start? page-number) note-title-list))
     (set! page-title title-element)	; for convenient direct variable reference below
     (let ((lsp-page (make-lsp-page page-id elements-0)))    
        (set! lsp-contents-in-this-lecture (cons lsp-page lsp-contents-in-this-lecture))
        (check-ids! lsp-page))
     (set! separate-elements-on-current-page (make-separate-element-items separate-page-elements))

     ; keep the url of corresponding theme page in a global variable. It is too expensive to calculate it more than once.
     (set! corresponding-theme-url (theme-url-of-lecture-id-and-page-id (as-symbol lecture-id) (as-symbol page-id)))

     (let ((the-next-page (next-page)))

       ; take show-and-speak information of this page out in a global variable. A list of show-and-speak clauses
       (set! current-show-and-speak-specification
	     (normalize-speak (if show-and-speak-clause (element-contents show-and-speak-clause) default-show-and-speak-clause) 
			      lecture-id id
			      (if the-next-page 
				  (show-and-speak-slide-name lecture-id the-next-page 1)
				  (end-show-and-speak-page-name lecture-id))))


       ; add information to the variable speak-list-in-this-lecture
       (if show-and-speak? (add-to-internal-speak-entries current-show-and-speak-specification))

       (set! next-exercise-number-0 next-exercise-number) ; the start value on this page.

       (reset-page-element-numbers)	; @c ensure program/exercise/image numering pr. page pr. presentation-kind (slide, note, book)

                                        ; make the standard slide

       ; Even if make-slide-view? is false, it is important to generate the slide internally.
       ; The reason is that some side effects take place during slide generation, such as word-index collection
       (let ((standard-slide-result
              (standard-slide
	       title-element
	       (as-slide-text
		(apply con
		       (map
			(lambda (e)
			  (present-element
			   'slide-style
			   (element-tag e)
			   (element-contents e)
			   (element-attributes e)
			   ) 
			  )
			elements)))
	       long-slide?
	       section-start?
	       element-keywords
	       (down-links elements lecture-id page-id 'slide) ;@h
               importance
	       )))
         (if make-slide-view?
	     (write-text-file
	      standard-slide-result
	      (destination-path (if slide-header-as-frame? (slide-name-0 lecture-id id) (slide-name lecture-id id))))))



                         ; If wanted, make the automatically progressing show and speak variationS of the slide:
                         ; @j
       (if (and make-slide-view? show-and-speak?) 
	   (let ((slide-show-and-speak-specs (filter (speak-select 'slide-part) current-show-and-speak-specification)))
	     (set! show-and-speak-context #t)
	     (for-each 
	      (lambda (slide-spec)
		(make-show-and-speak-slide slide-spec id elements))
	      slide-show-and-speak-specs)
	     (set! show-and-speak-context #f)
	     )
	   )

       ; OBSOLETE - never do it
       (if (and slide-header-as-frame? #f)
	   (let ((navigation-filename (filename-of-top-navigation lecture-id id))
		 (frame-filename (filename-of-main-frame lecture-id id))
		 (slide-filename (slide-name-0 lecture-id id)))

                                        ; WRITE FRAME:
	     (write-text-file
	      (html4:html
	       (con (html4:head (html4:title "title"))
		    (html4:frameset 
		     (con (html4:frame 'name "leno:navigation" 'src (string-append navigation-filename ".html") 'scrolling "no"
				       'noresize "noresize" 'frameborder "0") 
			  (html4:frame 'name "leno:main" 'src (string-append slide-filename ".html") 'scrolling "auto" 'frameborder "0"))
		     'rows "45,*")))
	      (destination-path frame-filename))      

                                        ; WRITE TOP, HORIZONTAL NAVIGATION BAR
	     (write-text-file
	      (page "navigation"
		    (slide-header-topbar)
		    slide-background-color black black black)
	      (destination-path navigation-filename))        
	     )
	   )


       (reset-page-element-numbers-1)	; @d ensure program/exercise/image numering pr. page pr. presentation-kind (slide, note, book)
       (if make-annotated-slide-view?
        (write-text-file
	 (standard-note 
	  title-element
	  (apply con
		 (map
		  (lambda (e)
		    (present-element
		     'note-style
		     (element-tag e)
		     (element-contents e)
		     (element-attributes e)
		     ))
		  elements))
	  element-keywords
	  (down-links elements lecture-id page-id 'note) ;@i
	  )
	 (destination-path (note-name lecture-id id))))




       (reset-page-element-numbers-1)	; @e ensure program/exercise/image numering pr. page pr. presentation-kind (slide, note, book)
       (if make-aggregated-view?
        (write-string-to-port
	 (apply con
		(map
		 (lambda (e)
		   (present-element
		    'book-style
		    (element-tag e)
		    (element-contents e)
		    (element-attributes e)
		    ))
		 elements))
	 book-port))

       (if (>= lecture-note-verbose-mode 1) (begin (display " OK") (newline)))

       (if show-and-speak?
        (set! current-show-and-speak-count (+ current-show-and-speak-count (show-and-speak-length current-show-and-speak-specification))))

       ))))



; Check that id is suitable, without white space. A conservative approach.
; Returns a boolean result.
; Notice that we forbid upper case letters to avoid PC/Unix problems
; (july 31, 02) and other problems as well.
(define note-page-id-valid?
 (let* (
        (lower-alphabetic-char-list (map as-char (number-interval (as-number #\a) (as-number #\z))))
        (upper-alphabetic-char-list (map as-char (number-interval (as-number #\A) (as-number #\Z))))
        (number-list (map as-char (number-interval (as-number #\0) (as-number #\9))))
        (others-list (list #\-))

        (all-char-list (append lower-alphabetic-char-list number-list others-list))
       )
   (lambda (id)
     (let ((id-string (as-string id)))
       (string-of-char-list? id-string all-char-list)))))


; Process elements and splice the elements specified by the splice-page-without tag
(define (do-element-splicing elements)
 (cond ((null? elements) '())
       ((or (eq? 'splice-page-with (element-tag (car elements)))
            (eq? 'splice-page-without (element-tag (car elements))))
             (append (fetch-splice-contribution (car elements)) 
                     (do-element-splicing (cdr elements))))
       (else (cons (car elements) (do-element-splicing (cdr elements))))))

; Extract the splice contribution from the -.lsp internals file and eliminate
; the 'minus tags'
(define (fetch-splice-contribution splice-page-form)
 (let* ((kind (as-symbol (element-tag splice-page-form)))
        (splice-page-contents (element-contents splice-page-form))
        (special-tag-names (map as-symbol (first splice-page-contents)))
        (lect-id (as-symbol (second splice-page-contents)))
        (pg-id (as-symbol (third splice-page-contents)))
        (el-id 
          (if (>= (length splice-page-contents) 4) (as-symbol (fourth splice-page-contents)) #f))
        (external-page-elements (lookup-page-clause lect-id pg-id))
       )
  (if external-page-elements
      (if (not (and (eq? (as-symbol page-id) pg-id) (eq? (as-symbol lecture-id) lect-id)))
	  ((if (eq? kind 'splice-page-without) eliminate-minus-tags keep-plus-tags)
            special-tag-names external-page-elements el-id)
	  (laml-error "You cannot splice the current note page into itself" lecture-id page-id))
      (begin
        (display-warning (string-append "Cannot locate splice page - RERUN to fix this problem:" 
                         (as-string lect-id) " " (as-string pg-id)))
        '()
      ))))

; filter elements such that none with minus-tags are included. 
; el-id is not taken into account.
(define (eliminate-minus-tags minus-tags elements el-id)
 (filter
   (lambda (e)
     (not (memq (element-tag e) minus-tags)))
   elements))

; filter elements such that only the plus tags are included
; If el-id is non false, only include elements with the given id.
(define (keep-plus-tags plus-tags elements el-id)
 (filter
   (lambda (e)
     (and (memq (element-tag e) plus-tags)
          (if el-id                            
              (eq? (element-id e) el-id)
              #t)
     )
   )
   elements))

; Constructor and selectors of aux entries. Not yet used consistently.
(define (make-aux-entry title-element id section-start? page-number)
 (list title-element id section-start? page-number))

(define title-of-aux-entry (make-selector-function 1 "title-of-aux-entry"))
(define id-of-aux-entry (make-selector-function 2 "id-of-aux-entry"))
(define section-start?-of-aux-entry (make-selector-function 3 "section-start?-of-aux-entry"))
(define page-number-of-aux-entry (make-selector-function 4 "page-number-of-aux-entry"))

; old:
;(define title-from-aux-file car)

; We support both a new and an old format of titles in aux files:
(define (title-from-aux-file x)
 (let ((candidate (car x)))
   (cond ((string? candidate) candidate)
         ((list? candidate) (car candidate))
         (else (error "title-from-aux-file: should not happen")))))

; Return the type of the lecture (the symbol normal or side-track). Take information from aux info.
(define (lecture-type-from-aux-file x)
 (let ((candidate (car x)))
   (cond ((string? candidate) 'normal)
         ((list? candidate) (as-symbol (cadr candidate)))
         (else (error "leture-type-from-aux-file: should not happen")))))

(define (entries-of-aux-file aux-structure)
  (if (null? aux-structure) '() (cdr aux-structure)))


; Constructor and selectors of entries in the lsp representation on internal lsp files.
(define make-lsp-page cons)
(define id-of-lisp-page car)
(define elements-of-lisp-page cdr)

; ----------------------------------------------------------------------------------------
; The list of elements that apply to any element.
; The element names correspond directly to css attribute names.
(define general-element-attributes 
  (list 'margin 'margin-top 'margin-bottom 'margin-left 'margin-right))

(define (general-element-attribute? attr-pair)
  (turn-into-boolean (memq (car attr-pair) general-element-attributes)))

; Return a css attribute list on property list format, and keys prefixed with 'css:'
(define (css-property-list attr-alist)
  (alist-to-propertylist
   (map (lambda (pair) (cons (as-symbol (string-append "css:" (as-string (car pair)))) (cdr pair)))
       attr-alist)))
; ----------------------------------------------------------------------------------------

; October 9, 2001: We pass an attribute list (for now, empty) on to the individual processing procedures.
; May 2, 2002: LENO margin attributes are only applied in slide-style.
;   Alternatively, the measures should be scaled down in note-style (and book-style).
(define (present-element style kind contents . optional-parameter-list)
 (let* ((attribute-list (optional-parameter 1 optional-parameter-list '()))
        (general-element-attributes (filter general-element-attribute? attribute-list))

        (drop-element? (as-boolean (defaulted-get 'drop attribute-list #f)))
        (element-id (defaulted-get 'id attribute-list #f))
       )
  (set! page-element-id element-id)
  (let ((processing-procedure (if drop-element? empty-slide-element (find-processor style kind))))
    (if (and (not (null? general-element-attributes)) (eq? style 'slide-style))
        (div (css-property-list general-element-attributes) (apply processing-procedure contents))
        (apply processing-procedure contents))       ; ready for (append contents (list attribute-list)). Calls for a number of parameter extensions.
)))

; Not used any longer - see make-lecture-note-contents-page.
(define (make-original-lecture-note-contents-page)
 (let* ((normal-html-dest? (lambda () (equal? (relative-source-html-destination-path-fragment) "html/")))

        (prev (if previous-lecture (contents-url previous-lecture) ""))
        (next (if next-lecture (contents-url next-lecture) ""))
        (down-links (list (slide-url lecture-id (first-slide-id lecture-id))
                          (note-url lecture-id (first-slide-id lecture-id))
                          (book-url lecture-id)))  ;@a
        (body (con 
               (index-page-header) (br)

               ; title and author info side by side with logo image.
               (table-3 0
                (list 752 200)
                  (list
                    (list
                      (h 3
                        (con 
                          (h 1 (con lecture-title))

                          (list-to-string
                            (map (lambda (e) (con e (br)))  (append lecture-author-info (list lecture-date)) )
                            ""
                          )))

                     (logo-img)))
                     "middle")

;               (vertical-space 1)


               (a-tag (string-append (book-name lecture-id) ".html") 
                      (font-size 2 (text-choice "Sammensat forelæsningsnote" "Aggregated lecture notes")))
               (horizontal-space 12)

               (if show-and-speak?
                   (con (a-tag 
                          (slide-show-and-speak-url lecture-id (first-slide-id lecture-id) 1)  
                          (font-size 2 (text-choice "Indtalt forelæsning fra starten" "Audio lecture from the start")))
                        (horizontal-space 12))
                   "")

               (if lecture-title-page?
                   (a-tag
                      (html-file (title-page-name lecture-id))
                      (font-size 2 (text-choice "Forside" "Title page"))
                   )
                   "")

               (horizontal-space 12)

               (if lecture-title-page?
                   (a-tag
                      (html-file (abstract-page-name lecture-id))
                      (font-size 2 (text-choice "Sammendrag" "Abstract"))
                   )
                   "")

               (horizontal-space 12)

               (a-tag (string-append (book-name lecture-id) ".html" "#REFERENCES") 
                      (font-size 2 (text-choice "Referencer fra denne lektion" "References from this lecture")))

               (horizontal-space 12)

               (if (and exercise-index? (> (length lecture-exercise-list) 0))
                 (con (a-tag (string-append (exercise-index-name lecture-id) ".html") 
                             (font-size 2 (text-choice "Opgaver i denne lektion" "Exercises in this lecture")))
                      (horizontal-space 12))
                 "")

               (br)

               (if (and quiz-support? (> (length quiz-list) 0))
                   (con 
                     (a-tag (quiz-url "show-quiz.cgi" 
                                     (list 'quiz-filename 'activation-mode 'language)
                                     (list (string-append note-source-directory "internal/" (as-string lecture-id) ".quiz")
                                           "present"
                                           (as-string language-preference)))  
                           (font-size 2 (text-choice "Quiz om denne lektion" "Quiz about this lecture")))
                     (horizontal-space 12))
                   "") 

               (p)

               (if show-and-speak? 
                   (let* ((secs (sum-list (map internal-speak-time speak-list-in-this-lecture)))
                          (time-str (speak-playing-time secs))
                          )
                     (font-size 2 
                       (em (con (text-choice "Samlet spilletid af indtalt forelæsning: "
                                             "Total playing time of audio lecture: ")
                                 time-str (p))
                       )))
                   (p))

               (if (>= news-flash-level 3) (leno-news-flash 4 910) "")
 
               (table-1 1
                        '(440 70 70 130 165)   
                        (make-list 5 slide-background-color)
                        (map2 
                         (lambda (e number) ; number not used...
                           (let ((slide-title (car e))
                                 (slide-id (cadr e))
                                 (slide-section-start? (caddr e)))
                             (list (con 
                                    (if slide-section-start?
                                        (font-color red (b slide-title))
                                      slide-title))
                                   (con (text-choice "Side " "Page ") (as-string number))
                                   (con 
                                      (a-tag (html-file (slide-name lecture-id slide-id)) "Slide") 
                                      (if show-and-speak?
                                         (con (horizontal-space 2)
                                              (if (file-exists-robust? (sound-file-path (speak-name lecture-id slide-id 'slide-part 1)))
                                                  (a-tag (html-file (show-and-speak-slide-name lecture-id slide-id 1)) (img-LN "small-speaker.gif" "Show and speak slide"))
                                                  (img-LN "small-speaker-empty.gif" ""))

                                         )
                                         ""))
                                   (a-tag (string-append (note-name lecture-id slide-id) ".html") 
                                          (text-choice "Kommenteret slide" "Annotated slide")) 
                                   (a-tag (string-append (book-name lecture-id) ".html" "#" (as-string slide-id))
                                          (text-choice "Forelæsningsnote" "Aggregated notes"))
                                   )))

                         (reverse note-title-list) (number-interval 1 (length note-title-list)))) (p)

               (if show-and-speak? 
                   (con (a-tag (html-file (internal-show-and-speak-page-name lecture-id)) (font-size 1 "Intern 'show-and-speak' oversigt")) 
                        (p))
                   "")

               (table-3 0 (list 180 "*") 
                  (list (list  (laml-power-icon "") (when-generated))))  ; the LAML power icon is part of the images icon of LENO to ensure it can always be displayed.

               )))
    (write-text-file
     (if java-scripting
         (page-with-keypress-script
           (con lecture-title)
           '()
           body

          javascript-loading
          (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
          (js-call "lenoNavigate"   ;@b
                   (append
                     (map string-it-single 
                          (list prev
                                next 
                                (if front-index? (front-index-url) "")
                                ""
                                "" "" ""))
                     (list (js-string-array down-links))))

           white black blue blue
           mouse-advancement
           (actual-stylesheets apply-css-styling?)
           (leno-css-class 'index)
         )

       (page-1
        (con lecture-title)
        '()
        body
        white black blue blue
        (actual-stylesheets apply-css-styling?)
        (leno-css-class 'index)
        ))
     (destination-path (index-name lecture-id)))))

(define (make-lecture-note-contents-page)
 (let* ((normal-html-dest? (lambda () (equal? (relative-source-html-destination-path-fragment) "html/")))

        (prev (if previous-lecture (contents-url previous-lecture) ""))
        (next (if next-lecture (contents-url next-lecture) ""))
;         (down-links (list (slide-url lecture-id (first-slide-id lecture-id))
;                           (note-url lecture-id (first-slide-id lecture-id))
;                           (book-url lecture-id)))  ;@a
        (down-links (map (lambda (pg-id)  (primary-view-url lecture-id pg-id))
                         (front-sublist (reverse note-page-list) max-keyboard-navigation-number)))  ; 1 - 9, a - m
        (body (con 
               (index-page-header) (br)

               (title-author-logo-strip lecture-title #f lecture-author-info lecture-date lecture-number)              

;               (vertical-space 1)

               (if lecture-title-page?
                   (a-tag
                      (html-file (title-page-name lecture-id))
                      (font-size 2 (text-choice "Forside" "Title page"))
                   )
                   "")

               (horizontal-space 12)

               (if lecture-title-page?
                   (a-tag
                      (html-file (abstract-page-name lecture-id))
                      (font-size 2 (text-choice "Sammendrag" "Abstract"))
                   )
                   "")

               (horizontal-space 12)

               (if show-and-speak?
                   (con (a-tag 
                          (slide-show-and-speak-url lecture-id (first-slide-id lecture-id) 1)  
                          (font-size 2 (text-choice "Indtalt forelæsning fra starten" "Audio lecture from the start")))
                        (horizontal-space 12))
                   "")

               (on-condition make-aggregated-view?
                (a-tag (string-append (book-name lecture-id) ".html" "#REFERENCES") 
                      (font-size 2 (text-choice "Referencer fra denne lektion" "References from this lecture"))))

               (on-condition make-aggregated-view? (horizontal-space 12))

               (if (and exercise-index? (> (length lecture-exercise-list) 0))
                 (con (a-tag (string-append (exercise-index-name lecture-id) ".html") 
                             (font-size 2 (text-choice "Opgaver i denne lektion" "Exercises in this lecture")))
                      (horizontal-space 12))
                 "")

               (if (and source-program-index? (> (length list-of-source-files) 0))
                 (con (a-tag (string-append (source-program-index-name lecture-id) ".html") 
                             (font-size 2 (text-choice "Kildeprogrammer i denne lektion" "Source programs in this lecture")))
                      (horizontal-space 12))
                 "")



               (br) ; ---------------------------------------------------------------------------------------------



               (if (and quiz-support? (> (length quiz-list) 0))
                   (con 
                     (a-tag (quiz-url "show-quiz.cgi" 
                                     (list 'quiz-filename 'activation-mode 'language)
                                     (list (string-append note-source-directory "internal/" (as-string lecture-id) ".quiz")
                                           "present"
                                           (as-string language-preference)))  
                           (font-size 2 (text-choice "Quiz om denne lektion" "Quiz about this lecture")))
                     (horizontal-space 12))
                   "") 

               (p)

               (if show-and-speak? 
                   (let* ((secs (sum-list (map internal-speak-time speak-list-in-this-lecture)))
                          (time-str (speak-playing-time secs))
                          )
                     (font-size 2 
                       (em (con (text-choice "Samlet spilletid af indtalt forelæsning: "
                                             "Total playing time of audio lecture: ")
                                 time-str (p))
                       )))
                   (p))

               (if (>= news-flash-level 2) (leno-news-flash 4 910) "")
 
               (div (style 'type "text/css" "a:link, a:visited  {color: black}, a:hover {color: black; background-color: #17ffff; }")  ; not here!
               
               ; MAIN PART 
               (if (> (length note-title-list) lecture-list-two-colummn-threshold)
                 (let* ((note-title-list-1 (reverse note-title-list))
                        (note-title-list-numbered (map (lambda (e n) (cons n e)) note-title-list-1 (number-interval 1 (length note-title-list))))
                        (paired-note-title-list-numbered (sublist-by-2columns note-title-list-numbered #f))
                       )
                  (table-1 0
                        (if show-and-speak? '(10  55 20  350  55 20  350) '(10  60  350  60  350))   
                        (make-list (if show-and-speak? 7 5) slide-background-color)
                        (map
                         (lambda (e)
                          (let* ((first-col-entry (car e))
                                 (second-col-entry (cadr e))

                                 (number1 (first first-col-entry))
                                 (slide-title1 (second first-col-entry))
                                 (slide-id1 (third first-col-entry))
                                 (slide-section-start1? (fourth first-col-entry))

                                 (number2 (if second-col-entry (first second-col-entry) #f))
                                 (slide-title2 (if second-col-entry (second second-col-entry) #f))
                                 (slide-id2 (if second-col-entry (third second-col-entry) #f))
                                 (slide-section-start2? (if second-col-entry (fourth second-col-entry) #f))

                                )
                             (list-flatten-one
                                   ""
                                   (font-color grey1 (b (con (text-choice "Side " "Page ") (as-string number1))))
 
                                   (if show-and-speak? (list (speaker-link lecture-id slide-id1)) '())
                                   
                                   (a 'href (primary-view-url lecture-id slide-id1) 'css:text-decoration "none"
                                     (con 
                                     (if slide-section-start1?
                                         (font-color red (b slide-title1))
                                       slide-title1)
                                     ))

                                   (if slide-title2 
                                      (font-color grey1 (b (con (text-choice "Side " "Page ") (as-string number2)))) "")

                                   (if show-and-speak? 
                                       (if slide-id2 (list (speaker-link lecture-id slide-id2)) (list ""))
                                       '())
 
                                   (if slide-id2
                                       (a 'href (primary-view-url lecture-id slide-id2) 'css:text-decoration "none"
					  (con 
					   (if slide-section-start2?
					       (font-color red (b slide-title2))
					     slide-title2)

                                           )) "")

                                   )))

                         paired-note-title-list-numbered)))
                     (table-1 0
                        (if show-and-speak? '(10 55 20 400) '(10 60 400)   )
                        (make-list (if show-and-speak? 4 3) slide-background-color)
                        (map2 
                         (lambda (e number) 
                           (let ((slide-title (car e))
                                 (slide-id (cadr e))
                                 (slide-section-start? (caddr e)))
                             (list-flatten-one
                                   ""
                                   (font-color grey1 (b (con (text-choice "Side " "Page ") (as-string number))))

                                   (if show-and-speak? (list (speaker-link lecture-id slide-id)) '())

                                   (a 'href (primary-view-url lecture-id slide-id) 'css:text-decoration "none"
                                     (con 
                                     (if slide-section-start?
                                        (font-color red (b slide-title))
                                      slide-title)
                                    ))

                                   )))

                         (reverse note-title-list) (number-interval 1 (length note-title-list))))))

               (p)

               (if show-and-speak? 
                   (con (a-tag (html-file (internal-show-and-speak-page-name lecture-id))
                        (font-size 1 (text-choice "Intern 'show-and-speak' oversigt" "Internal 'show-and-speak' overview"))) 
                        (p))
                   "")

               (left-middle-right-banner
                 (leno-icon)
                 (when-generated)
                 (laml-power-icon "")
               )

;                (table-3 0 (list 180 "*") 
;                   (list (list  (con (leno-icon) (horizontal-space 1) (laml-power-icon "")) )))

               )))
    (write-text-file
     (if java-scripting
         (page-with-keypress-script
           (con lecture-title)
           '()
           body

          javascript-loading
          (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
          (js-call "lenoNavigate"   ;@b
                   (append
                     (map string-it-single 
                          (list prev
                                next 
                                (if front-index? (front-index-url) "")
                                ""
                                "" "" (theme-url 
                                       (theme-name-of lecture-id)
                                       (first-slide-id (theme-name-of lecture-id)) ; takes information from theme aux file
                                       )))
                     (list (js-string-array down-links))))

           white black blue blue
           mouse-advancement
           (actual-stylesheets apply-css-styling?)
           (leno-css-class 'index)
         )

       (page-1
        (con lecture-title)
        '()
        body
        white black blue blue
        (actual-stylesheets apply-css-styling?)
        (leno-css-class 'index)
        ))
     (destination-path (index-name lecture-id)))))

; Like list, but flattens lists at top level.
(define (list-flatten-one . lst)
 (list-flatten-one-1 lst))

(define (list-flatten-one-1 lst)
 (cond ((null? lst) '())
       ((list? (car lst)) (append (car lst) (list-flatten-one-1 (cdr lst))))
       (else (cons (car lst) (list-flatten-one-1 (cdr lst))))))

(define (speaker-link lecture-id slide-id)
  (if show-and-speak?
      (if (file-exists-robust? (sound-file-path (speak-name lecture-id slide-id 'slide-part 1)))
	       (a-tag (html-file (show-and-speak-slide-name lecture-id slide-id 1)) (img-LN "small-speaker.gif" "Show and speak slide"))
	       (img-LN "small-speaker-empty.gif" ""))
      ""))

; -----------------------------------------------------------------------------
; word index stuff

(define (index-elements-leq? el1 el2)
  (string<=? (downcase-string (string-copy (car el1))) (downcase-string (string-copy (car el2))))) 

; There is a new version, makeing a word index of all sections until current-lecture
; (define (make-lecture-word-index-page)
;   ; write a page with the word index of this lecture
;   (write-text-file
;     (page 
;       (con lecture-title " Index")
;       (con 
;         (word-index-header) (br)
;         (h 1 (con "Index: " lecture-title))  (br)
; 
;        (indent-pixels 10
;          (brl
;           (map format-word-in-index
;                (sort-list word-index index-elements-leq?)))))
;       slide-background-color black black black )
;    (destination-path (word-index-name lecture-id))))

; -----------------------------------------------------------------------------

; make a cross reference contribution to the book port:
(define (make-cross-reference-section-in-book cross-ref-list)
 (if make-aggregated-view?
  (write-string-to-port
   (let ((formatter-function (generate-cross-reference-formatter book-url)))
    (con (hr-1) (a-name "REFERENCES")
     (book-element 
       (font-1 4 book-title-foreground-color 
          (con  (text-choice "Samlede referencer" "Collected references") (br)
             (font-size 1 
              (con
               (a-tag (contents-url lecture-id) (text-choice "Indhold" "Contents") ) (space 1)
               (a-tag (all-word-index-url lecture-id) (text-choice "Stikord" "Index")) ))  ))
       (table-3 0 (list 420 80)
           (remove-duplicates (map formatter-function cross-ref-list))))
        (p)))
  book-port)))

; make a source file listing contributin to the book port:
; Obsolete - not used.
(define (make-source-file-listing list-of-source-files)
 (if make-aggregated-view?
  (write-string-to-port
    (con (hr-1) (a-name "SOURCE-FILE-LIST")
     (book-element-unlimited
       (font-1 4 book-title-foreground-color 
         (con
          (text-choice "Kildefiler i denne lektion" "Source files in this lecture") (br)
          (font-size 1 
            (con
             (a-tag (contents-url lecture-id) (text-choice "Indhold" "Contents") ) (space 1)
             (a-tag (all-word-index-url lecture-id) (text-choice "Stikord" "Index")) (space 1)
             (a-tag "#REFERENCES" (text-choice "Referencer" "References")) ))) )
       (con
        (brl
          (map (lambda (f) (font-size 2 f)) (reverse (remove-duplicates list-of-source-files))))
        (p) )))
  book-port)))

; Make an index page to be placed in the note source directory with useful links to the pages in the html directory.
; An optional parameter 'minimal (a symbol) will only generate a link to the lecture overview page. Use this for trails.
(define (make-source-level-index-page . optional-parameters)
 (let ((minimal? (if (null? optional-parameters) #f (eq? (car optional-parameters) 'minimal)))
       (normal-html-dest? (lambda () (equal? (relative-source-html-destination-path-fragment) "html/")))
      )
  (let ((body 
         (con
          (h 1 (con lecture-title))
          (h 3
          (list-to-string
	   (map (lambda (e) (con e (br))) (append lecture-author-info (list lecture-date)))
            ""
          )
          ) 

          ; do not show abstract here - links will not both work from source dir and from HTML target dir!

          (p)

          (indent-pixels 10
            (con 
               (if lecture-title-page?
                   (con 
                     (a-tag (string-append (relative-source-html-destination-path-fragment) (title-page-name lecture-id) ".html") 
                            (text-choice "Forside" "Title page")) (p))
                   "")

               (if lecture-title-page?
                   (con 
                     (a-tag (string-append (relative-source-html-destination-path-fragment) (abstract-page-name lecture-id) ".html") 
                            (text-choice "Sammendrag" "Abstract")) (p))
                   "")

               (a-tag (string-append (relative-source-html-destination-path-fragment)  (as-string lecture-id) ".html") 
                      (text-choice "Oversigt over denne lektion" "Lecture overview")) (p)

               (if (not minimal?)
                   (con

                    (a-tag (string-append (relative-source-html-destination-path-fragment)
                                          (slide-name lecture-id (first-slide-id lecture-id)) ".html")
                           (text-choice "Første slide" "First slide")) (p)

                    (a-tag (string-append (relative-source-html-destination-path-fragment)
                                          (note-name lecture-id (first-slide-id lecture-id)) ".html")
                           (text-choice "Første annoterede slide" "First annotated slide")) (p)

                    (a-tag (string-append (relative-source-html-destination-path-fragment) (book-name lecture-id) ".html" "#REFERENCES") 
                           (text-choice "Referencer fra denne lektion" "References from this lecture")) (p)
     
                    (if (and exercise-index? (> (length lecture-exercise-list) 0))
                      (a-tag (string-append (relative-source-html-destination-path-fragment) (exercise-index-name lecture-id) ".html") 
                             (text-choice "Opgaver i denne lektion" "Exercises in this lecture"))
                      "") (p)

                    (if (and quiz-support? (> (length quiz-list) 0))
                      (a-tag (quiz-url "show-quiz.cgi" 
                                       (list 'quiz-filename 'activation-mode)
                                       (list (string-append note-source-directory "internal/" (as-string lecture-id) ".quiz") "present"))  
                             (text-choice "Quiz om denne lektion" "Quiz about this lecture"))
                      "") (p)
     
                    (if trail-of-lecture?
                      (a-tag (string-append (relative-source-html-destination-path-fragment) (default-trail-name lecture-id) ".html")
                             (text-choice "Trail udgave af denne lektion" "Trail edition of this lecture"))
                      ""))
                   "")

                (vertical-space 1)

               ))

        (table-3 0 (list 180 "*") 
		 (list (list  (laml-power-icon (relative-source-html-destination-path-fragment)) (when-generated))))
     )))
    (write-text-file
     (page-1
        (con lecture-title)
        '()
        body
        white black blue blue
        (actual-stylesheets apply-css-styling? #t)
        (leno-css-class 'index)
     )
     (source-path lecture-id)))))

(define (delete-source-level-index-page)
 (let ((file-path (source-path lecture-id)))
  (if (file-exists? file-path)
      (delete-file file-path))))


(define (make-lecture-title-page first-url first-show-and-speak-url)
 (let* (
        (a-tag-tiny 
         (lambda (url anchor) (if apply-css-styling? 
                                 (a 'href url 'class (leno-css-class 'intro 'link) anchor)
                                 (a-tag url (font-1 2 grey1 anchor)))))
        (body
         (con 
          (center
            (con
               (vertical-space 2)
               (if apply-css-styling?
                   (div 'class (leno-css-class 'intro 'title) lecture-title)
                   (font-size 7  (con lecture-title)))
               (if (and notes-subtitle (= (length lecture-sections) 1)) ; only subtitle for single lecture materials
                   (con (br) (if apply-css-styling? (div 'class (leno-css-class 'intro 'sub-title) notes-subtitle) (font-size 5 notes-subtitle))
                   ) "")
               (vertical-space 3)
               (h 1 (em (car lecture-author-info))) (br)   ; author
               (h 2
                  (apply con
                         (map (lambda (e) (con e (br))) (cdr lecture-author-info)))
                  )
               (h3 lecture-date)

               (vertical-space 1) 
               (a-tag-tiny (html-file (abstract-page-name lecture-id)) "Abstract") (br)               

               (cond ((eq? the-primary-view 'slide-view)
                       (a-tag-tiny first-url (text-choice "Første slide" "First slide")))

                     ((eq? the-primary-view 'annotated-slide-view)
                       (a-tag-tiny first-url (text-choice "Første annoterede slide" "First annotated slide")))

                     ((eq? the-primary-view 'aggregated-view)
                       (a-tag-tiny (book-url lecture-id) (text-choice "Samlet materiale" "Aggregated material")))

                     (else ""))

               (br)

               (if show-and-speak?
                   (con (a-tag-tiny first-show-and-speak-url (text-choice "Første indtalte slide" "First slide with speaker sound")) (br))
                   "")

               (if trail-of-lecture?
                   (con (a-tag-tiny (html-file (filename-of-trail-frame (default-trail-name lecture-id) lecture-id (first-slide-id lecture-id))) "Trail start") (br))
                   "")

               (a-tag-tiny (html-file (index-name lecture-id)) (text-choice "Indholdsfortegnelse" "Table of contents"))
               (vertical-space 3)
               (logo-img)
            )
          )
        ))) 
  (write-text-file
    (if java-scripting
      (page-with-keypress-script
         (as-string lecture-title) 
         '()
         body

         javascript-loading
         (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
         (js-call "lenoNavigate" 
                   (map string-it-single 
                        (list ""
                              (html-file (abstract-page-name lecture-id))  ; the abstract page
                              ""
                              "" 
                              "" "" "")))

         slide-background-color black black black
         mouse-advancement
         (actual-stylesheets apply-css-styling?)
         (leno-css-class 'intro)
      )

      (page-1
       (as-string lecture-title) 
       '()
       body
       slide-background-color black black black
       (actual-stylesheets apply-css-styling?)
       (leno-css-class 'intro)
      )

    )
    (destination-path (title-page-name lecture-id)))))

(define (make-lecture-abstract-page first-slide-url first-show-and-speak-url)
 (let* ((a-tag-tiny 
         (lambda (url anchor) (if apply-css-styling? 
                                 (a 'href url 'class (leno-css-class 'intro 'link) anchor)
                                 (a-tag url (font-1 2 grey1 anchor)))))
        (body (con
              (center
               (con
                (vertical-space 2)
                (font-size 6 (b (text-choice "Sammendrag" "Abstract"))) (br)
                (font-size 4 lecture-title) (br)
                (vertical-space 2)
                (narrow
                 br                     ; does not matter
                 650
                 (font-size 4 (em lecture-abstract))
                 )
                (vertical-space 1)
                (a-tag-tiny first-slide-url "Start") (br)
                (if show-and-speak?
                    (con (a-tag-tiny first-show-and-speak-url 
                              (text-choice "Start indtalte slides" "Start audio slides")) (br))
                    "")
                (if trail-of-lecture?
                    (con (a-tag-tiny (html-file (filename-of-trail-frame (default-trail-name lecture-id) lecture-id (first-slide-id lecture-id)))
                                "Trail start") (br))
                    "")
                (a-tag-tiny (html-file (index-name lecture-id)) (text-choice "Indholdsfortegnelse" "Table of contents")))) (br)

                )
               )
         )
  (write-text-file
    (if java-scripting
      (page-with-keypress-script
         (as-string lecture-title) 
         '()
         body

         javascript-loading
         (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
         (js-call "lenoNavigate" 
                   (map string-it-single 
                        (list ""
                              first-slide-url
                              ""
                              ""
                              "" "" "")))

         slide-background-color black black black
         mouse-advancement
         (actual-stylesheets apply-css-styling?)
         (leno-css-class 'intro)
      )

      (page-1
       (as-string lecture-title) 
       '()
       body
       slide-background-color black black black
       (actual-stylesheets apply-css-styling?)
       (leno-css-class 'intro)
      )
   )
  (destination-path 
    (abstract-page-name lecture-id)))))


(define (make-top-level-abstract-page first-lecture-index-url)
 (let* ((a-tag-tiny 
         (lambda (url anchor) (if apply-css-styling? 
                                 (a 'href url 'class (leno-css-class 'intro 'link) anchor)
                                 (a-tag url (font-1 2 grey1 anchor)))))
        (body (con
              (center
               (con
                (vertical-space 2)
                (font-size 6 (b (text-choice "Sammendrag" "Abstract")))
                (vertical-space 2)
                (narrow
                 br                     ; does not matter
                 650
                 (font-size 4 (em note-abstract))
                 )
                (vertical-space 1)
                (a-tag-tiny first-lecture-index-url "Start") (br)

                )
               )
              )))
  (write-text-file
    (if java-scripting
      (page-with-keypress-script
         (as-string lecture-title) 
         '()
         body

         javascript-loading
         (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
         (js-call "lenoNavigate" 
                   (map string-it-single 
                        (list ""
                              first-lecture-index-url
                              ""
                              ""
                              "" "" "")))

         slide-background-color black black black
         mouse-advancement
         (actual-stylesheets apply-css-styling?)
         (leno-css-class 'intro)
      )

      (page-1
       (as-string lecture-title) 
       '()
       body
       slide-background-color black black black
       (actual-stylesheets apply-css-styling?)
       (leno-css-class 'intro)
      )
   )
  (destination-path 
    (top-level-abstract-page-name))))
)

(define (make-top-level-title-page first-lecture-index-url)
 (let* ((a-tag-tiny 
         (lambda (url anchor) (if apply-css-styling? 
                                 (a 'href url 'class (leno-css-class 'intro 'link) anchor)
                                 (a-tag url (font-1 2 grey1 anchor)))))
        (body
         (con 
          (center
            (con
               (vertical-space 2)
               (font-size 7 (con notes-title))
               (if notes-subtitle (con (br) (font-size 5 notes-subtitle)) "")
               (vertical-space 2)
               (h 1 (em (car lecture-author-info))) (br)   ; author
               (h 2
                  (apply con
                         (map (lambda (e) (con e (br))) (cdr lecture-author-info)))
                  )
               (h3 lecture-date)

      
               (vertical-space 1) 

               (a-tag-tiny (html-file top-level-note-file-name) (text-choice "Indeks" "Index")) (br)

               (a-tag-tiny (html-file (top-level-abstract-page-name)) "Abstract") (br)               

               (a-tag-tiny first-lecture-index-url (text-choice "Første lektion" "First lecture"))

               (vertical-space 2)
               (logo-img)
            )
          )
        ))) 
  (write-text-file
    (if java-scripting
      (page-with-keypress-script
         (as-string notes-title) 
         '()
         body

         javascript-loading
         (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
         (js-call "lenoNavigate" 
                   (map string-it-single 
                        (list ""
                              (html-file (top-level-abstract-page-name))  ; the abstract page
                              ""
                              "" 
                              "" "" "")))

         slide-background-color black black black
         mouse-advancement
         (actual-stylesheets apply-css-styling?)
         (leno-css-class 'intro)
      )

      (page-1
       (as-string notes-title) 
       '()
       body
       slide-background-color black black black
       (actual-stylesheets apply-css-styling?)
       (leno-css-class 'intro)
      )

    )
    (destination-path (top-level-title-page-name)))))

(define (read-text-file-if-exists file-path)
  (if (file-exists? file-path)
      (read-text-file file-path)
      ""))

; end-notes defined in lecture-notes.scm as an alias of do-end-notes.
(define (do-end-notes)

 (let (
       (do-do-process-all-lectures? (and do-process-all-lectures? (> (length lecture-list) 1)))
       (actual-lecture-id lecture-id)
      )


 (rerun-notice! (map id-of-aux-entry (reverse note-title-list)) (old-note-page-list))

 (if (file-exists? (aux-file-name))
     (delete-file (aux-file-name)))
 
 ; generate new aux file
 (with-output-to-file (aux-file-name)
     (lambda () (write (cons (list lecture-title lecture-type) (reverse note-title-list)) )))  ; earlier note-page-list

 (if (> (length cross-refs-in-this-lecture) 0)
   (make-cross-reference-section-in-book
      (reverse cross-refs-in-this-lecture)))

 ; Don't use source file listing in book view any more - we now support a separate source file index.
 (if (and #f include-source-file-listing? (> (length list-of-source-files) 0))
   (make-source-file-listing
      list-of-source-files))

 (if make-aggregated-view?
  (write-string-to-port
   (book-footer) book-port))

 (if make-aggregated-view?
  (write-string-to-port
    (post-page-with-comments)
    book-port))
 

 (if make-aggregated-view?
  (close-output-port book-port))

 ; make contents page of this lecture
 (make-lecture-note-contents-page)


 ; make word index file, in order to be able to collect a global word index.
 (if (file-exists? (word-index-file-name))
     (delete-file (word-index-file-name)))


 (with-output-to-file (word-index-file-name)
     (lambda () (write word-index)))


 ; make cross reference file in order collect all cross references in one place:
 (if (file-exists? (cross-ref-file-name))
     (delete-file (cross-ref-file-name)))

 (with-output-to-file (cross-ref-file-name)
     (lambda () (write cross-refs-in-this-lecture)))

          
 (if (file-exists? (front-matter-info-file-name))
     (delete-file (front-matter-info-file-name)))
 
 ; generate new front info file
 (with-output-to-file (front-matter-info-file-name)
     (lambda () (write (make-front-matter-info) )))

 (if (and source-program-index? (not (null? list-of-source-files)))
    (make-source-program-index! lecture-id list-of-source-files))
     

 ; word index covering all sections until current-lecture
 (if word-index? 
   (if split-word-index-alphabetically? 
       (make-lecture-word-index-pages (all-index-words))
       (make-lecture-word-index-page (all-index-words))))

 ; requires that all cross reference files, -.crs files, in lecture set are defined:
 (if cross-references? (make-total-cross-references-page (all-cross-references)))

 (if exercise-index?
     (begin
       (make-all-exercise-page lecture-list (list "Opgaver" "Exercises") 
                                            exercise-formulation-entry all-exercises-name)
       (if (or reveal-exercise-solutions? reveal-all-solutions?)
           (make-all-exercise-page lecture-list (list "Opgaver og løsninger" "Exercises and solutions") 
                                            exercise-formulation-solution-entry all-exercises-and-solutions-name))
     ))

 ; a "secret", non-linked-to page with exercises and solutions.
 ; good to teaching assistents etc.

 (make-all-exercise-page lecture-list (list "Opgaver og løsninger" "Exercises and solutions") 
                                       exercise-formulation-solution-entry secret-all-exercises-and-solutions-name)


 ; make the pr. lecture exercise index
 (if exercise-index? (make-exercise-index lecture-exercise-list))

 ; save information about the exercises in this lecture
 (if exercise-index?
     (save-exercise-info lecture-exercise-list ))


 ; warn if it is too early to reveal exercise solutions:
 (if (and exercise-manager-support?)
     (if (> (current-time) (lecture-exercise-reveal-time lecture-id))
         (display-message "Exercise manager info: We revealed exercise solutions")
         (let* ((tm (lecture-exercise-reveal-time lecture-id))
                (day (weekday tm))
                (dt (date-time tm))
                (date (car dt))
                (time (cadr dt))
               )
           (display-warning (string-append "Can first reveal exercises " day ", " date ", " time)))))

 ; generate exercise manager control page (for teachers only):
 (if #t
     (make-exercise-manager-control-center lecture-id))



 ; make the overall exercise index, covering all lectures up to current-lecture
 ; must take place after saving the exercise info about the current lecture
 (make-overall-exercise-index)

 (if secret-overall-exercise-index-name
     (make-overall-exercise-index 'secret))


 (if (and java-scripting (eq? javascript-loading 'dynamic) (eq? mouse-advancement 'single-press))
     (display-warning "** Single mouse click page shift is no longer supported **"))

 ; Copying the appropriate lenonavigate file from the software directory to the notes directory. 
 ; Depends on the variable mouse-advancement:
 (if (and java-scripting (eq? javascript-loading 'dynamic))
     (copy-text-file (string-append leno-software-directory "javascript/" 
                        (cond ((eq? mouse-advancement 'double-press) "lenonavigate-double.js")
                              ((eq? mouse-advancement 'single-press) "lenonavigate-no-digits.js") ; !! Ad hoc: Sept 12, 2011.
                              (else (error (string-append "end-notes: Cannot determine which lenonavigate to copy."))))
                     )
                     (string-append note-source-directory (relative-source-html-destination-path-fragment) "javascript/lenonavigate.js")
                     #t  ; overwrite if necessary
     )
 )

 ; Copying the images (icons) from the software directory to the notes directory:
 (if copy-image-files? 
      (begin
        (copy-files 
          leno-image-files
          (string-append leno-software-directory "images/")
          (string-append note-source-directory (relative-source-html-destination-path-fragment) "images/") )))

 ; Copying note specific icons
 ; The variable note-specific-image-files can be redefined in post-notes.scm, NOT in notes.scm
 (if copy-image-files? 
      (begin
        (copy-files 
          (ensure-logo-image-if-wanted note-specific-image-files)
          (string-append note-source-directory "images/")
          (string-append note-source-directory (relative-source-html-destination-path-fragment) "images/")
          #t )))

 ; Check for overlap between LENO images (LENO icons) and note-specific images:
 (let ((image-intersection (list-intersection-by-predicate note-specific-image-files leno-image-files equal?)))
   (if (not (null? image-intersection))
       (begin
         (display-warning
          (string-append "Some note-specific images overwrite LENO defined icons: " 
                         (list-to-string image-intersection ", "))))))




 ; Copying note graphics - user made illustrations to the the HTML graphics directory
 ; Copied to same directory as the leno image files - redefinition and overwriting is possible.
 (if copy-note-graphics?
      (begin
        (copy-files
          note-specific-graphics-files
          (string-append note-source-directory "graphics/")
          (string-append note-source-directory (relative-source-html-destination-path-fragment) "graphics/")
          #t )))

 ; Copying note small graphics - user made illustrations to the the HTML small graphics directory.
 ; SVG files should not be copied, however, because SVG images scale down without problems. 
 (if copy-note-graphics?
      (begin
        (copy-files 
          (filter (compose not svg-or-flash-file?) note-specific-graphics-files)
          (string-append note-source-directory "graphics/small/")
          (string-append note-source-directory (relative-source-html-destination-path-fragment) "graphics/small/")
          #t )))

 ; Copying lecturer graphics
 (if (and show-and-speak? copy-lecturer-photos?)
      (begin
        (copy-files 
          lecturer-photo-list
          (string-append note-source-directory "lecturer-photos/")
          (string-append note-source-directory (relative-source-html-destination-path-fragment) "lecturer-photos/")
          #t )))


 
 ; Check for page-id duplicates;
 (check-page-id-duplicates!)

 ; Check cross references in this lecture
 (if (>= lecture-note-verbose-mode 2)
     (begin (display (string-append "Checking all note references in " (as-string lecture-id))) (newline)))

 (let ((problem-references (check-crs-file lecture-id (all-page-info (map car lecture-sections)))))
  (if (>= lecture-note-verbose-mode 1)   
      (if (not (null? problem-references))
	  (begin
	    (display "The following note references refer to unknown targets: ") (newline) 
	    (for-each 
	     (lambda (problem-ref)
	       (display-crs-entry problem-ref) (newline))
	     problem-references)
	    )
	  (if (>= lecture-note-verbose-mode 2) (display-message "All note cross references are OK")))))

 (if trail-of-lecture?
   (begin
     (display-message 
       (string-append "Making trail which parallels the lecture - see " 
                      (string-append (relative-source-html-destination-path-fragment) (default-trail-name lecture-id) ".html")))

     ; Make trail of this lecture - provides for a convenient alternative navigation
      (note-trail-1 
       (as-symbol (default-trail-name lecture-id))
       (map (lambda (page-id) (slide-select lecture-id page-id)) (reverse note-page-list)))
   )
 )
 
 (if source-level-index-page? 
    (begin 
      (display-message (string-append "You can start browsing these lecture notes at " lecture-id "." target-extension)) 
      (make-source-level-index-page))
      (delete-source-level-index-page)
 )

 (if lecture-title-page? 
    (let ((first-slide-url (slide-url lecture-id (first-slide-id lecture-id)))
          (first-note-url (note-url lecture-id (first-slide-id lecture-id)))
          (first-show-and-speak-url (slide-show-and-speak-url lecture-id (first-slide-id lecture-id) 1))
         )
      (make-lecture-title-page (cond ((eq? the-primary-view 'slide-view) first-slide-url)
                                      ((eq? the-primary-view 'annotated-slide-view) first-note-url)
                                      (else ""))
                               first-show-and-speak-url)
      (make-lecture-abstract-page (cond ((eq? the-primary-view 'slide-view) first-slide-url)
                                      ((eq? the-primary-view 'annotated-slide-view) first-note-url)
                                      ((eq? the-primary-view 'aggregated-view) (book-url lecture-id))
                                      (else "")) first-show-and-speak-url)))


 ; Top level title page:
 (if top-level-title-page?
    (let ((first-lecture-index-url (html-file (index-name (car (car lecture-sections))))))
      (make-top-level-title-page first-lecture-index-url)))

 ; Top level abstract page:
 (if top-level-abstract-page?
    (let ((first-lecture-index-url (html-file (index-name (car (car lecture-sections))))))
      (make-top-level-abstract-page first-lecture-index-url)))



 (if (and quiz-support? (not (null? quiz-list)))
     (file-write
      (make-quiz-file
        lecture-id
        lecture-title
        (map2 internal-quiz-format (reverse quiz-list) (number-interval 1 (length quiz-list)))
      )
      (quiz-file-name)))


 (if show-and-speak?
     (write-text-file
       (end-show-and-speak-page)
       (destination-path (end-show-and-speak-page-name lecture-id))))

 ; Always make the show and speak input sheet, as a service to the author who perhaps
 ; will need to record sound to this lecture
 (make-show-and-speak-input-sheet)

 ; make cross reference file in order collect all cross references in one place:
 (if (file-exists? (speak-file-name lecture-id))
     (delete-file (speak-file-name lecture-id)))


 ; Reverse speak-list-in-this-lecture:
 (set! speak-list-in-this-lecture (reverse speak-list-in-this-lecture))

 ; Add, as the last component, a 'source file name number' to each entry.
 (set! speak-list-in-this-lecture
   (map2 (lambda (e n)
           (append e (list n)))
         speak-list-in-this-lecture
         (number-interval 1 (- current-show-and-speak-count 1))))

 
 (with-output-to-file (speak-file-name lecture-id)
     (lambda () (write speak-list-in-this-lecture)))


 (if show-and-speak?
     (write-text-file
       (internal-show-and-speak-page speak-list-in-this-lecture)
       (destination-path (internal-show-and-speak-page-name lecture-id)))
 )


 ; make full contents lsp file in order to save entire contents querrying purposes.
 (if (file-exists? (lsp-file-name))
     (delete-file (lsp-file-name)))

 (with-output-to-file (lsp-file-name)
     (lambda () (write (reverse lsp-contents-in-this-lecture))))

 ; make index of all lectures - absolute top level page - in the index.html file
 ; Good to do this as one of the last things in end-notes - it depends on details in the -.spk files
 (if front-index? (make-front-index))

 (if make-print-page? (make-a-print-page!))

 ; Stylesheets copying
 (if (and apply-css-styling? css-stylesheet-do-copying)
     (let ((leno-source-css-pre-filepath (string-append note-source-directory "stylesheets/" the-css-prestylesheet-name ".css"))
           (leno-software-css-pre-filepath  (string-append leno-software-directory "stylesheets/" the-css-prestylesheet-name ".css"))
           (leno-source-css-filepath (string-append note-source-directory "stylesheets/" the-css-stylesheet-name ".css"))
           (leno-software-css-filepath  (string-append leno-software-directory "stylesheets/" the-css-stylesheet-name ".css"))
           (leno-target-css-filepath (string-append note-source-directory 
                                       (relative-source-html-destination-path-fragment) "stylesheets/" "stylesheet.css")) ; fixed target name
          )
       (write-text-file
           (string-append
              (read-text-file-if-exists leno-software-css-pre-filepath) CR CR (read-text-file-if-exists leno-source-css-pre-filepath) CR CR
              (read-text-file-if-exists leno-software-css-filepath) CR CR (read-text-file-if-exists leno-source-css-filepath)
           )
           leno-target-css-filepath
       )

       (if (and (not (file-exists? leno-source-css-pre-filepath))
                (not (file-exists? leno-software-css-pre-filepath)))
           (display-warning "Cannot find locate any CSS prestylesheet named" the-css-prestylesheet-name))

       (if (and (not (file-exists? leno-source-css-filepath))
                (not (file-exists? leno-software-css-filepath)))
           (display-warning "Cannot find locate any CSS stylesheet named" the-css-stylesheet-name))
    ))


 ; make the secondary theme source file
 ; overwrite the existing secondary theme source file if theme-source-mode is overwrite,
 ; make a new file theme source file if theme-source-mode is new.
 (if (or (eq? theme-source-mode 'new) (eq? theme-source-mode 'overwrite) (eq? theme-source-mode 'delta))
     (begin
       (make-theme-source-file lecture-id)
       (if (>= lecture-note-verbose-mode 2)
           (display-message (string-append "The theme source file " (theme-source-file-name lecture-id) " was made automatically - existing file overwritten."))))
     
 )


 ; make a secondary LENO trail source file of the current-lecture prefix part of lecture-list.
 (if (or (eq? trail-source-mode 'new) (eq? trail-source-mode 'overwrite))
     (begin
        (make-secondary-trail-source-file (front-sublist lecture-list current-lecture) "trail")
        (if (>= lecture-note-verbose-mode 2)
            (cond ((eq? trail-source-mode 'overwrite)
                     (display-message (string-append "A trail source file was made automatically - existing file overwritten!!!")))
                  ((eq? trail-source-mode 'new) (display-message (string-append "A trail source file was made automatically - new and fresh file is created.")))
                  (else 'no-message)))
     )
 )


 ; It is rather tricky to do both theme processing and processing of all lecture
 ; The solution was to define a couple of controlling variable locally to do-end-notes:
 ; do-do-process-all-lectures? and actual-lecture-id

 ; process accompaning theme source file if it exits?
 (if (and theme-auto-process-mode
                  (or (file-exists? (in-startup-directory (theme-source-file-name lecture-id "laml")))
                      (file-exists? (in-startup-directory (theme-source-file-name lecture-id "leno")))))
     (begin
       (display-message "Processing theme source file:") 
       (leno-laml (theme-source-file-name lecture-id))
     ))

 ; Now process remaining lectures, if so demanded through use of the process-all-lectures attribute of the front-matters clause
 ; However, never process more lectures than demanded by the variable current-lecture.
 (if do-do-process-all-lectures?    ; only relevant for multi lecture collections
     (begin
       (display-message "Processing remaining lectures.")
       (for-each 
         (lambda (lid)
           (display-message "")
           (display-message "---------------------------------------------------")
           (leno-laml lid))
         (filter (lambda (x) (not (equal? (as-string actual-lecture-id) x)))
                 (list-prefix lecture-list current-lecture)))))


 (end-laml)

 ) ; end let
      
)

; LAML execute f, which is a proper file name *without* extension.
; First, if f.leno exists, it is laml executed. Second, if f.laml exists, it is executed.
(define (leno-laml f)
 (cond ((file-exists? (in-startup-directory (string-append (as-string f) ".leno")))
          (laml (string-append (as-string f) ".leno")))
       ((file-exists? (in-startup-directory (string-append (as-string f) ".laml")))
          (laml (string-append (as-string f) ".laml")))
       (else (laml-error "leno-laml: Neither a leno nor a laml file exist with the proper name" f))))

; Is f an svg file. f is a file name such as "name.gif" or "name.svg"
(define (svg-or-flash-file? f)
 (let ((extension (file-name-extension f)))
   (or (equal? (downcase-string extension) "svg")
       (equal? (downcase-string extension) "swf")   ; flash file?
   )))


; add "logo.gif" to image-list (non-destructively) if a logo is needed
(define (ensure-logo-image-if-wanted image-list)
 (let ((logo-image-file "logo.gif"))
   (if (not (empty-string? logo-url)) ; we want a logo
       (if (not (member logo-image-file image-list)) 
           (cons logo-image-file image-list)
           image-list)
       image-list)))

(define (rerun-notice! new-aux-list old-aux-list)
  (if (not (equal? new-aux-list old-aux-list))
    (display-message "** RERUN TO GET PAGE LINKING CORRECT **")))



; Check for duplicated page ids. Fatal condition.
(define (check-page-id-duplicates!)
  (let ((duplicates (duplicates-by-predicate note-page-list eq?)))
    (if (not (null? duplicates))
        (laml-error (string-append 
                      "THERE ARE PAGES WITH DUPLICATED IDS IN THIS LECTURE: "
                      (list-to-string (map as-string duplicates) ",") ". "
                      "Please change id(s) and re-process the document."))
        (if (>= lecture-note-verbose-mode 2)
            (display-message "There are no duplicated page ids in this lecture"))
  )))




; -----------------------------------------------------------------------------
; Functions which html formats a piece of source program for slides, notes and books:

(define (program-slide context-url left-url right-url annotation-url title contents file-name 
                       caption proper-source-program-file-name . optional-parameter-list)  
 (let* ((program-number   (optional-parameter 1 optional-parameter-list 1))
        (background-color (optional-parameter 2 optional-parameter-list slide-background-color))
       )
  (let ((body (if apply-css-styling? 
                  (con (program-header context-url left-url right-url annotation-url 'slide
                                    (key-shortcut (text-choice "Tilbage til slide" "Back to slide") "u") 
                                    #f program-number (string-append proper-source-program-file-name " - " caption)) (p)
                       contents)
                  (con (program-header context-url left-url right-url annotation-url 'slide
                                    (key-shortcut (text-choice "Tilbage til slide" "Back to slide") "u") 
                                    #f program-number (string-append proper-source-program-file-name " - " caption)) (p)
                       (indent-pixels 10 (font-size slide-font-size contents)))))
       )
    (if java-scripting
      (page-with-keypress-script
         (as-string title) 
         '()
         body

         javascript-loading
         (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
         (js-call "lenoNavigate" 
                   (map string-it-single 
                        (list (if left-url left-url "")
                              (if right-url right-url "")
                              context-url
                              (if annotation-url annotation-url "")
                              (if show-and-speak? (html-file (program-name lecture-id page-id 'show program-number) ) "")  ; x
                              (if show-and-speak? (speak-url (speak-name lecture-id page-id 'program-part program-number)) "")  ; y 
                              "")))

         background-color black black black
         mouse-advancement
         (actual-stylesheets apply-css-styling?)
         (leno-css-class 'slide 'program)
      )

      (page-1
       (as-string title) 
       '()
       body
       background-color black black black
       (actual-stylesheets apply-css-styling?)
       (leno-css-class 'slide 'program)
     )))))


(define (program-note context-url left-url right-url annotation-url title prog-contents doc-contents number caption)
  (let ((body  (con (program-header context-url left-url right-url annotation-url 'note
                                    (key-shortcut (text-choice "Tilbage til noter" "Back to notes") "u")
                                    #f number caption)
                    (if (and extract-and-present-end-of-line-doc-comments (string? doc-contents) (not (blank-string? doc-contents)))
                        (program-presentation-slice  
                          (font-size note-font-size-left prog-contents)
                          (font-1 note-font-size-left note-foreground-color (pre (html-protect doc-contents))))
                        (indent-pixels 10 (font-size note-font-size-left prog-contents))
                    )
               )))
    (if java-scripting
      (page-with-keypress-script
         (as-string title) 
         '()
         body

         javascript-loading
         (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
         (js-call "lenoNavigate" 
                  (map string-it-single 
                       (list (if left-url left-url "")
                             (if right-url right-url "")
                             context-url
                             (if annotation-url annotation-url "")
                             "" "" "")))

         slide-background-color black black black
         mouse-advancement
         (actual-stylesheets apply-css-styling?)
         (leno-css-class 'note 'program)
      )
      (page-1
        (as-string title) 
        '()
        body
        slide-background-color black black black 
        (actual-stylesheets apply-css-styling?)
        (leno-css-class 'note 'program)
    ))))

(define (program-book context-url title contents)
  (let ((body (con (program-header context-url #f #f #f 'book
                     (key-shortcut (text-choice "Tilbage til forelæsningsnoter" "Back to lecture notes") "u")) (vertical-space 1)
                     (font-size book-font-size contents))))
    (if java-scripting
      (page-with-keypress-script
         (as-string title) 
         '()
         body

         javascript-loading
         (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
         (js-call "lenoNavigate" 
                  (map string-it-single 
                       (list ""
                             ""
                             context-url
                             ""
                             "" "" "")))

         slide-background-color black black black
         mouse-advancement
         (actual-stylesheets apply-css-styling?)
         (leno-css-class 'book 'program)
     )
     (page-1
        (as-string title) 
        '()
        body
        slide-background-color black black black 
        (actual-stylesheets apply-css-styling?)
        (leno-css-class 'book 'program)
   ))))

; -----------------------------------------------------------------------------

(define (exercise-slide context-url left-url right-url title contents solution-url number exercise-title)
  (let ((body  (con (exercise-slide-header 
                       context-url left-url right-url
                       (key-shortcut 
                         (text-choice "Gå til slide, hvor denne opgave er tilknyttet"
                                      "Go to the slide, where this exercise belongs") "u")
                       number
                       exercise-title
                    )
                    (br) (font-color blue (h 1 title)) (p) (hr-1) (p)

                    (if apply-css-styling?
                        (leno-css-styled 'slide 'exercise contents)
                        (indent-pixels 10 (font-size slide-font-size contents)))    (p) (hr-1) (p)

                    (cond ((equal? solution-url "") (font-size 2 (text-choice "Der findes ingen løsning til denne opgave" "There is no solution to this exercise")))
                          ((and (do-reveal-exercise-solutions?) (not (equal? solution-url ""))) (a-tag solution-url (text-choice "Løsning" "Solution")))
                          ((not (equal? solution-url "")) (font-size 2 (text-choice "Løsningen til denne opgave er pt. ikke frigivet" "The solution to this exercise has not yet been released")))
                          (else ""))
                        
                    )))

    (if java-scripting
      (page-with-keypress-script
        (as-string title) 
        '()
         body

         javascript-loading
         (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
         (js-call "lenoNavigate" 
                  (map string-it-single 
                       (list (if left-url left-url "") 
                             (if right-url right-url "")
                             context-url
                             ""
                             "" "" "")))

         slide-background-color black black black
         mouse-advancement
         (actual-stylesheets apply-css-styling?)
         (leno-css-class 'slide 'exercise)
      )
      (page-1
        (as-string title) 
        '()
         body

         slide-background-color black blue blue 
         (actual-stylesheets apply-css-styling?)
         (leno-css-class 'slide 'exercise)
 ))))


(define (exercise-note context-url left-url right-url title contents solution-url number exercise-title)
  (let ((body  (con (exercise-note-header
                       context-url left-url right-url
                       (key-shortcut 
                         (text-choice "Gå til annoteret slide, hvor denne opgave er tilknyttet"
                                      "Go to the notes, in which this exercise belongs") "u")
                       number
                       exercise-title
                    )
                    (br) (font-color blue (h 1 title)) (p) (hr-1) (p)
                    (indent-pixels 10 (font-size note-font-size-left contents)) (p) (hr-1) (p)

                    (cond ((equal? solution-url "") (font-size 2 (text-choice "Der findes ingen løsning til denne opgave" "There is no solution to this exercise")))
                          ((and (do-reveal-exercise-solutions?) (not (equal? solution-url ""))) (a-tag solution-url (text-choice "Løsning" "Solution")))
                          (else ""))
                )))
    (if java-scripting
      (page-with-keypress-script
        (as-string title) 
        '()
         body

         javascript-loading
         (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
         (js-call "lenoNavigate" 
                  (map string-it-single 
                       (list (if left-url left-url "") 
                             (if right-url right-url "")
                             context-url
                             ""
                             "" "" "")))

         slide-background-color black black black
         mouse-advancement
         (actual-stylesheets apply-css-styling?)
         (leno-css-class 'note 'exercise)
      )
      (page-1
        (as-string title) 
        '()
         body

         note-background-color black blue blue 
         (actual-stylesheets apply-css-styling?)
         (leno-css-class 'note 'exercise)
      )
 )))

; Similar to exercise-note, but here for solutions
(define (exercise-solution-note context-url title contents)
  (let ((body  (con (exercise-solution-header
                        context-url 
                       (key-shortcut 
                         (text-choice "Gå til annoteret slide, hvor denne opgave er tilknyttet"
                                      "Go to the notes, in which this exercise belongs") "u")
                       exercise-number
                       title
                    )
                    (br) (font-color blue (h 1 title)) (p) (hr-1) (p)
                    (font-size note-font-size-left contents) (p) (hr-1) (p)
                )))
    (if java-scripting
      (page-with-keypress-script
        (as-string title) 
        '()
         body

         javascript-loading
         (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
         (js-call "lenoNavigate" 
                  (map string-it-single 
                       (list ""
                             ""
                             context-url
                             ""
                             "" "" "")))

         slide-background-color black black black
         mouse-advancement
         (actual-stylesheets apply-css-styling?)
         (leno-css-class 'note 'exercise)
      )
      (page-1
        (as-string title) 
        '()
         body

         note-background-color black blue blue
         (actual-stylesheets apply-css-styling?)
         (leno-css-class 'note 'exercise)
      )
)))



; -----------------------------------------------------------------------------


(define (comma-separated-string string-list)
  (string-merge string-list (make-list (- (length string-list) 1) ", ")))



; Make a standard LENO slides with or without Javascript, depending on the variable java-scripting
; importance is a number between 0 and 100. 100 is normal = high importance.
(define (standard-slide title contents long? sectional-slide? keywords-to-meta down-links importance)
 (let* ((meta-contribution (if (null? keywords-to-meta) 
                              '()
                              (list (list 'name "keywords" 
                                  'lang (meta-language language-preference)
                                  'content (comma-separated-string keywords-to-meta)))))
        (prev (if (previous-page) (previous-page) #f))
        (next (if (next-page) (next-page) #f))
        (body (con 
                 (slide-start-marker)
                 (if slide-header-as-frame?
                    "" 
                    (cond ((eq? slide-header? 'normal) (con (slide-header 'slide) (br)))
                          ((eq? slide-header? 'none) "")
                          ((eq? slide-header? 'minimal) (minimal-slide-header importance))
                          (else (error (string-append "Standard-slide: Unknown slide-header?: " (as-string slide-header?))))))
                 contents
                 (if slide-header-as-frame? "" (if long? 
                                                   (empty-slide-footer)     ; alternatively (long-slide-footer)
                                                   (empty-slide-footer)))
                 (slide-end-marker)))
        (body1 (if (> slide-page-offset 0) (indent-pixels slide-page-offset body) body))
        (body-id (aggregated-page-body-element-id))
       )
  (if java-scripting
   (page-with-keypress-script
    (as-string title)
    meta-contribution
    body1

    javascript-loading
    (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
    (js-call "lenoNavigate" 
      (append
       (map string-it-single 
         (list (if prev (slide-url lecture-id prev)  "")
               (if next (slide-url lecture-id next) "")
               (contents-url lecture-id)
               (if make-annotated-slide-view? (note-url lecture-id (current-page)) "")
               (if show-and-speak? (slide-show-and-speak-url lecture-id page-id 1) "")  ; x 
               (if show-and-speak? (speak-url (speak-name lecture-id page-id 'slide-part 1)) "")  ; y 
               (if themes? (if corresponding-theme-url corresponding-theme-url "") "")
              ))
       (list (js-string-array down-links)) ))  ;@a

    slide-background-color black black black
    mouse-advancement
    (actual-stylesheets apply-css-styling?)
    (if sectional-slide? (leno-css-class 'slide-section-start) (leno-css-class 'slide))
    body-id
  )
  (page-1 
    (as-string title)
    meta-contribution
    body1
    slide-background-color black black black 
    (actual-stylesheets apply-css-styling?)
    (if sectional-slide? (leno-css-class 'slide-section-start) (leno-css-class 'slide))
    body-id
 ))))


(define (slide-end-marker)
  (a-name "slide-end"))

(define (slide-start-marker)
  (a-name "slide-start"))

; The header of a program slide. Contains an up navigation button
(define (program-header context-url left-url right-url annotation-url from-where explanation . optional-parameter-list)
 (let* ((next-in-show (optional-parameter 1 optional-parameter-list #f))
        (program-number-maybe-false (optional-parameter 2 optional-parameter-list #f)) 
        (program-number (optional-parameter 2 optional-parameter-list 1)) 
        (middle-info (optional-parameter 3 optional-parameter-list ""))
        (sound (speak-name lecture-id page-id 'program-part program-number))
       )
  (header-banner-3
    (con 
     (if next-in-show    ; means show and speak
         (con
          (a-tag (html-file (program-name lecture-id page-id 'slide program-number)) 
                 (img-LN "atomspin.gif"  (text-choice "Stop afspilning med lyd" "Stop show with sound"))) 

          (space 2)
 
          (a-tag 
            next-in-show
            (img-LN "nav-right-red.gif" (key-shortcut (text-choice "Næste slide i dette show" "Next slide in show") "n")))

          (space 2)

         )
       
        
          (con                          ; non show and speak
           (if show-and-speak?
               (con
                (a-tag (html-file (program-name lecture-id page-id 'show program-number) ) 
                       (img-LN "atomspin-freeze.gif" 
                               (key-shortcut (text-choice "Afspil indtalte slides" "Play audio slide show") "x")))
             
                (space 2)
   

                )
               "")

           (a-tag context-url (img-LN "nav-up.gif" explanation)) (space 2)

           (if left-url (a-tag left-url (img-LN "nav-left.gif" (key-shortcut (text-choice "forrige" "previous") "p")))  (space 6))
           (if left-url (space 2) "")

           (if right-url (a-tag right-url (img-LN "nav-right.gif" (key-shortcut (text-choice "næste" "next") "n"))) (space 6))
           (if right-url (space 2) "")

           (cond ((eq? from-where 'slide)
                     (if annotation-url
			 (a-tag annotation-url
				(img-LN "nav-notes.gif"
					(key-shortcut (text-choice "Annoteret program" "Annotated program") "t")))
			 (space 6)))
                 ((eq? from-where 'note)
                     (if annotation-url
			 (a-tag annotation-url
				(img-LN "nav-slide.gif"
					(key-shortcut (text-choice "Slide program" "Slide program") "t")))
			 (space 6)))
                 (else (space 6)))
                 

           (if annotation-url (space 2) "")

           (space 2)


           (if show-and-speak?
               (con
                (if (file-exists-robust? (sound-file-path sound))
                    (a-tag (speak-url sound)
                           (img-LN "speaker.gif" 
                                   (key-shortcut (text-choice "Afspil lyd på denne slide" "Play sound for this slide") "y")))
                    (img-LN "speaker-empty.gif" ""))
            
                (space 2))
               "")

           )))

   (em (font-1 2 grey1 middle-info))

   (con 
    (if (and lecture-number (> (length lecture-list) 1))
        (b (con (text-choice "Lektion " "Lecture ") (as-string lecture-number) " - slide "
                (as-string page-number) " : " (as-string (total-number-of-slides))))
        (b (con 
            "Slide " (as-string page-number) " : " (as-string (total-number-of-slides)) )))   (br)
    (if program-number-maybe-false 
        (b (con (text-choice "Program" "Program") " " (as-string program-number)))
        "")
   )

   (list) ; not used
  )))


(define (exercise-slide-header context-url left-url right-url explanation number exercise-title)
  (con
    (if exercise-index? 
        (a-tag (html-file (exercise-index-name lecture-id)) 
               (img-LN "nav-up.gif" 
                       (text-choice "Opgaver i denne lektion" "Exercises in this lecture")))
        (img-LN "nav-up-empty.gif" "")) (space 2)

    (if left-url (a-tag left-url (img-LN "nav-left.gif" (key-shortcut (text-choice "forrige" "previous") "p")))  (space 6))
    (if left-url (space 2) "")

    (if right-url (a-tag right-url (img-LN "nav-right.gif" (key-shortcut (text-choice "næste" "next") "n"))) (space 6))
    (if right-url (space 2) "")

    (a-tag context-url (img-LN "nav-slide.gif" explanation)) (space 2)

    (if annotator-support?
            (let ((number-of-annotations-now (length (list-of-submitted-annotations  "oop-exercises" (as-string current-exercise-id)))))
             (con
              (a-tag
               (annotator-url "annotator.cgi"
                  (list 'annotation-id 'detail-level 'activation-mode 'language 'informative-title 'back-url)
                  (list (encode-annotation-ids (list (as-string annotator-symbol) "exercises" (as-string current-exercise-id)))
                        "flat" "present-only" language-preference exercise-title 
                        (course-absolute-url (string-append "html/noter/" (html-file (exercise-name lecture-id page-id 'note number)))))
               )
               (img-LN 
                 (if (> number-of-annotations-now 0) "exclamation-red-ring.gif" "exclamation.gif")
                 (text-choice "Læs eller indsend kommentarer om denne opgave" "Read or submit annotations to this exercise")
               )
              )
              (space 2)))
            "") 

))


(define (exercise-note-header context-url left-url right-url explanation number exercise-title)
 (let ((annotator-url-contr 
       (if annotator-support?
            (let ((number-of-annotations-now (length (list-of-submitted-annotations "oop-exercises" (as-string current-exercise-id)))))
             (con
              (a-tag
               (annotator-url "annotator.cgi"
                  (list 'annotation-id 'detail-level 'activation-mode 'language 'informative-title 'back-url)
                  (list (encode-annotation-ids (list (as-string annotator-symbol)  "exercises" (as-string current-exercise-id)))
                        "flat" "present-only" language-preference exercise-title 
                        (course-absolute-url (string-append "html/noter/" (html-file (exercise-name lecture-id page-id 'note number)))))
               )
               (img-LN 
                 (if (> number-of-annotations-now 0) "exclamation-red-ring.gif" "exclamation.gif")
                 (text-choice "Læs eller indsend kommentarer om denne opgave" "Read or submit annotations to this exercise")
               )
              )
              (space 3)))
            "")))
  (if (frame-exercises?)
      (con
       (if exercise-index? 
           (a-tag-target
                  (html-file (exercise-index-name lecture-id)) 
                  (img-LN "nav-up.gif" 
                          (text-choice "Opgaver i denne lektion" "Exercises in this lecture"))
                  "_top"
                  )
         (img-LN "nav-up-empty.gif" "")) (space 3)
       (a-tag-target context-url (img-LN "nav-notes.gif" explanation) "_top")  (space 3)
       annotator-url-contr
       (a-tag (all-word-index-url lecture-id) (img-LN "index.gif" (text-choice "Alfabetisk indeks" "Alphabetic index"))) (space 3)
       (a-tag-target course-home-url (img-LN "nav-home.gif" (text-choice "Kursets hjemmeside" "Course home")) "_top") (space 3)
       (if distance-education-support?  ; idafus
           (a-tag-target (idafus-url "student-part/contribution-frameset.cgi" (list "unit-id" "activity-id") (list idafus-unit-id current-exercise-id)) "IDAFUS" "idafus")
           "") (space 3)
      )
    (con
     (if exercise-index? 
         (a-tag (html-file (exercise-index-name lecture-id)) 
                (img-LN "nav-up.gif" 
                        (text-choice "Opgaver i denne lektion" "Exercises in this lecture")))
       (img-LN "nav-up-empty.gif" "")) (space 3)

     (if left-url (a-tag left-url (img-LN "nav-left.gif" (key-shortcut (text-choice "forrige" "previous") "p")))  (space 6))
     (if left-url (space 2) "")

     (if right-url (a-tag right-url (img-LN "nav-right.gif" (key-shortcut (text-choice "næste" "next") "n"))) (space 6))
     (if right-url (space 2) "")

     (a-tag context-url (img-LN "nav-notes.gif" explanation)) (space 3)
     annotator-url-contr
     (a-tag (all-word-index-url lecture-id) (img-LN "index.gif" (text-choice "Alfabetisk indeks" "Alphabetic index"))) (space 3)
     (a-tag course-home-url (img-LN "nav-home.gif" (text-choice "Kursets hjemmeside" "Course home"))) (space 3)
     (if distance-education-support?  ; idafus
         (a-tag-target (idafus-url "idafus.cgi" (list "unit-id" "activity-id") (list idafus-unit-id current-exercise-id)) "IDAFUS" "idafus")
         "") (space 3)
     )

)))

(define (exercise-solution-header context-url explanation number exercise-title)
 (let ((annotator-url-contr 
       (if annotator-support?
            (let ((number-of-annotations-now (length (list-of-submitted-annotations "oop-exercise-solutions" (as-string current-exercise-id)))))
             (con
              (a-tag
               (annotator-url "annotator.cgi"
                  (list 'annotation-id 'detail-level 'activation-mode 'language 'informative-title 'back-url)
                  (list (encode-annotation-ids (list (as-string annotator-symbol) "exercise-solutions" (as-string current-exercise-id)))
                        "flat" "present-only" language-preference exercise-title 
                        (course-absolute-url (string-append "html/noter/" (html-file (exercise-solution-name lecture-id page-id number)))))
               )
               (img-LN 
                 (if (> number-of-annotations-now 0) "exclamation-red-ring.gif" "exclamation.gif")
                 (text-choice "Læs eller indsend kommentarer om denne opgave løsning" "Read or submit annotations to the solution of this exercise")
               )
              )
              (space 3)))
            "")))
  (if (frame-exercises?)
      (con
       (if exercise-index? 
           (a-tag-target 
                  (html-file (exercise-index-name lecture-id)) 
                  (img-LN "nav-up.gif" 
                          (text-choice "Opgaver i denne lektion" "Exercises in this lecture"))
                  "_top")
         (img-LN "nav-up-empty.gif" "")) (space 3)
       (a-tag-target context-url (img-LN "nav-notes.gif" explanation) "_top")  (space 3)
       annotator-url-contr
       (a-tag (all-word-index-url lecture-id) (img-LN "index.gif" (text-choice "Alfabetisk indeks" "Alphabetic index"))) (space 3)
       (a-tag-target course-home-url (img-LN "nav-home.gif" (text-choice "Kursets hjemmeside" "Course home")) "_top") (space 3)
      )
    (con
     (if exercise-index? 
         (a-tag (html-file (exercise-index-name lecture-id)) 
                (img-LN "nav-up.gif" 
                        (text-choice "Opgaver i denne lektion" "Exercises in this lecture")))
       (img-LN "nav-up-empty.gif" "")) (space 3)
     (a-tag context-url (img-LN "nav-notes.gif" explanation)) (space 3)
     annotator-url-contr
     (a-tag (all-word-index-url lecture-id) (img-LN "index.gif" (text-choice "Alfabetisk indeks" "Alphabetic index"))) (space 3)
     (a-tag course-home-url (img-LN "nav-home.gif" (text-choice "Kursets hjemmeside" "Course home"))) (space 3)
     ))))

(define (word-index-header)
   (cond (themes? (a-tag (html-file top-level-theme-index-file-name) (img-LN "nav-up.gif" (text-choice "Gå til tematisk indeks" "Navigate to theme index")) ))
         (front-index? (a-tag (front-index-url) (img-LN "nav-up.gif" (text-choice "Gå til lektionsliste" "Navigate to list of lectures")) ))
         (else (img-LN "nav-up-empty.gif" "")))
)

(define (word-index-header-extended prev-letter next-letter)
 (con
   (cond (themes? (a-tag (html-file top-level-theme-index-file-name) (img-LN "nav-up.gif" (text-choice "Gå til tematisk indeks" "Navigate to theme index")) ))
         (front-index? (a-tag (front-index-url) (img-LN "nav-up.gif" (text-choice "Gå til lektionsliste" "Navigate to list of lectures")) ))
         (else (img-LN "nav-up-empty.gif" ""))) (space 2)

   (if prev-letter
       (a-tag (html-file (string-append file-name-of-word-index  "-"  (downcase-string prev-letter)))
	      (img-LN "nav-left.gif" (key-shortcut (text-choice (string-append "Stikordsregister for " prev-letter) (string-append "Subject index of " prev-letter)) "p")) )
       (img-LN "nav-left-empty.gif" "")) (space 2)

   (if next-letter
       (a-tag (html-file (string-append file-name-of-word-index  "-"  (downcase-string next-letter)))
              (img-LN "nav-right.gif" (key-shortcut (text-choice (string-append "Stikordsregister for " next-letter) (string-append "Subject index of " next-letter)) "n")) )
       (img-LN "nav-right-empty.gif" "")) (space 2)
 )  
)

(define cross-reference-header word-index-header)

(define (source-program-index-header lecture-id)
 (con
   (a-tag-target (string-append (as-string lecture-id) ".html")
                        (img-LN  "nav-up.gif" (key-shortcut (text-choice "Lektionsindhold" "Lecture overview") "u")) "_top")  (space 2)

   (if previous-lecture
       (a-tag (contents-url (string-append (as-string previous-lecture) "-" "source-programs"))
	      (img-LN "nav-left.gif" (key-shortcut (text-choice "Kildeprogrammer i forrige lektion" "Source programs in previous lecture") "p")) )
       (img-LN "nav-left-empty.gif" "")) (space 2)

   (if next-lecture
       (a-tag (contents-url (string-append (as-string next-lecture) "-" "source-programs"))
              (img-LN "nav-right.gif" (key-shortcut (text-choice "Kildeprogrammer i næste lektion" "Source programs in next lecture") "n")) )
       (img-LN "nav-right-empty.gif" "")) (space 2)

   (a-tag-target course-home-url (img-LN "nav-home.gif" (text-choice "Kursets hjemmeside" "Course home")) "_top")
 )
)

(define (exercise-index-header)
 (con
   (a-tag (html-file overall-exercise-index-name) 
          (img-LN "nav-up.gif" (text-choice "Gå til oversigt over alle opgaver i noterne" 
                                            "Navigate to overview of all exercises in these notes"))
   ) (space 3)
   (a-tag (all-word-index-url lecture-id) (img-LN "index.gif" (text-choice "Alfabetisk indeks" "Alphabetic index"))) (space 3)
   (a-tag course-home-url (img-LN "nav-home.gif" (text-choice "Kursets hjemmeside" "Course home"))) (space 3)
 )
)

(define (all-exercise-page-header)
 (con
   (a-tag (html-file (exercise-index-name lecture-id)) 
               (img-LN "nav-up.gif" 
                       (text-choice "Index over opgaver i denne lektion" "Exercise index of this lecture")))
   (space 3)
   (a-tag (all-word-index-url lecture-id) (img-LN "index.gif" (text-choice "Alfabetisk indeks" "Alphabetic index"))) (space 3)
   (a-tag course-home-url (img-LN "nav-home.gif" (text-choice "Kursets hjemmeside" "Course home"))) (space 3)
  )
)

(define (overall-exercise-index-header)
 (con
   (a-tag (front-index-url) (img-LN "nav-up.gif" (text-choice "Gå til lektionsliste" "Navigate to list of lectures"))) (space 3)
   (a-tag (all-word-index-url lecture-id) (img-LN "index.gif" (text-choice "Alfabetisk indeks" "Alphabetic index"))) (space 3)
   (a-tag course-home-url (img-LN "nav-home.gif" (text-choice "Kursets hjemmeside" "Course home"))) (space 3)
 )
)

(define (front-index-header)
 (header-banner
  (con
   (img-LN "nav-up-empty.gif" "") (space 2)

   (a-tag course-home-url (img-LN "nav-home.gif" (text-choice "Kursets hjemmeside" "Course home"))) (space 2)
    
   (if (not (equal? note-download-url ""))
       (a-tag note-download-url (img-LN "todisk.gif" (text-choice "Download noter" "Download notes (zip file)")))
       (space 3))   (space 2)

   (if word-index? 
       (a-tag (all-word-index-url lecture-id) (img-LN "index.gif" (text-choice "Alfabetisk indeks" "Alphabetic index")))
       (space 3))

   (space 2)
;
;   (if exercise-index? 
;       (con (a-tag (html-file overall-exercise-index-name) (img-LN "think.gif" (text-choice "Opgave oversigt" "Exercise index"))) (space 2))
;       (space 3))
   
   (no-frames-button (html-file top-level-note-file-name))  (space 3)

   (if themes?
       (con (a-tag (html-file top-level-theme-index-file-name)
                   (img-LN "nav-book.gif" (text-choice "Lærebog" "Textbook")))
        (space 3))
       "")

   (on-condition
      make-print-page?
	(con 
	 (horizontal-space 4)
	 (a-tag (html-file (print-page-name))
		(img-LN "print-icon.gif" (text-choice "PDF print side" "PDF print page")))))

 )

 (a-tag about-laml-url (img-LN "question.gif" (text-choice "Hjælp om disse noter" "Help page about these notes")))
 )
)   
  

(define (index-page-header . optional-parameter-list)
 (let ((header-mode (optional-parameter 1 optional-parameter-list 'normal)))  ; trail or normal
 (let ((the-first-slide-id (first-slide-id lecture-id)))
  (header-banner
   (con (if front-index? 
            (a-tag (front-index-url) (img-LN "nav-up.gif" 
                                             (key-shortcut (text-choice "Gå til lektionsliste" "Navigate to list of lectures") "u")))
            (img-LN "nav-up-empty.gif" ""))

        (space 2)
        (if previous-lecture
            (a-tag (contents-url previous-lecture)
                   (img-LN "nav-left.gif" (key-shortcut (text-choice "Forrige lektion" "Previous lecture") "p")) )
            (img-LN "nav-left-empty.gif" "")) (space 2)
        (if next-lecture
            (a-tag (contents-url next-lecture)  (img-LN "nav-right.gif" (key-shortcut (text-choice "Næste lektion" "Next lecture") "n")) )
            (img-LN "nav-right-empty.gif" "")) (space 2)
        (a-tag course-home-url (img-LN "nav-home.gif" (text-choice "Kursets hjemmeside" "Course home"))) (space 8)



        (if annotator-support?
            (let ((number-of-annotations-now (length (list-of-submitted-annotations (as-string annotator-symbol) (as-string lecture-id)))))
             (con
              (a-tag-target
               (annotator-url "annotator.cgi"
                  (list 'annotation-id 'detail-level 'activation-mode 'language 'informative-title 'back-url)
                  (list (encode-annotation-ids (list (as-string annotator-symbol)  (as-string lecture-id)))
                        "flat" "present-only" language-preference lecture-title 
                        (course-absolute-url (string-append "html/noter/" (html-file (book-name lecture-id)))))
               )
               (img-LN 
                 (if (> number-of-annotations-now 0) "exclamation-red-ring.gif" "exclamation.gif")
                 (text-choice "Læs eller indsend kommentarer om denne lektion" "Read or submit annotations to this lecture")
               )
               "annotator"
              )
              (space 2)))
            "")

        (if word-index? 
            (a-tag (all-word-index-url lecture-id) (img-LN "index.gif" (text-choice "Alfabetisk indeks" "Alphabetic index")))
            (space 3))

        (space 2)
        (no-frames-button (html-file lecture-id))


        (space 8)


        (on-condition (and make-slide-view? (eq? header-mode 'normal))
          (a-tag (string-append (slide-name lecture-id the-first-slide-id) ".html") 
                 (img-LN "nav-slide.gif" (text-choice "Første slide" "First slide"))))

        (on-condition (and make-slide-view? (eq? header-mode 'normal)) (horizontal-space 2))

        (on-condition (and make-annotated-slide-view? (eq? header-mode 'normal))
         (a-tag (string-append (note-name lecture-id the-first-slide-id) ".html") 
                (img-LN "nav-notes.gif" (text-choice "Første annoterede slide" "First annotated slide"))))

        (on-condition (and make-annotated-slide-view? (eq? header-mode 'normal)) (horizontal-space 2))

        (on-condition (and make-aggregated-view? (eq? header-mode 'normal))
           (a-tag (string-append (book-name lecture-id) ".html") 
                  (img-LN "nav-aggr.gif"  
                     (text-choice "Forelæsningsnoter - alle sider sammen" "Lecture notes - all slides and notes together"))))

        (on-condition (and make-aggregated-view? (eq? header-mode 'normal)) (horizontal-space 2))

        (on-condition (and themes? (eq? header-mode 'normal))
           (a-tag (theme-url 
		   (theme-name-of lecture-id)
		   (first-slide-id (theme-name-of lecture-id)) ; takes information from theme aux file
		   )
		  (img-LN "nav-book.gif" (text-choice "Lærebog for denne lektion" "Textbook of this lecture"))))

        (on-condition (and themes? (eq? header-mode 'normal))  (horizontal-space 2))

        (on-condition make-print-page?
           (con 
            (horizontal-space 4)
            (a-tag (html-file (print-page-name))
		   (img-LN "print-icon.gif" (text-choice "PDF print side" "PDF print page")))))

        (on-condition make-print-page? (horizontal-space 2))


        (horizontal-space 6)


        (a-tag about-laml-url (img-LN "question.gif" (text-choice "Hjælp om disse noter" "Help page about these notes")))


        

        (br)
   )

   (font-1 3 grey1 (if (and lecture-number (> (length lecture-list) 1))
       (b (con (text-choice "Lektion " "Lecture ") (as-string lecture-number) ))
       ""))

   (list 360 600 280)

 ))))


   
; Return a slide header banner. Context is either slide og show-and-speak (a symbol).
; Takes two optional parameters. First is next-in-show (URL). Second is a middle info string
(define (slide-header context . optional-parameter-list)
  (let* ((next-in-show (optional-parameter 1 optional-parameter-list #f))
         (middle-info  (optional-parameter 2 optional-parameter-list ""))
         (prev (previous-page))
         (next (next-page))
 
         (this-lecture-context 
           (lambda (lecture-number lecture-title)
             (string-append (em lecture-title))))

         (old-title 
           (lambda (page-id)
             (let ((aux-record (find-aux-record remembered-old-aux-structure page-id)))
               (string-append
                 (if aux-record
                     (title-of-aux-entry aux-record)
                     "???")
                 (if (section-start?-of-aux-entry aux-record) " [Section]" "")))))

        )
    (header-banner-3
      (con
        (if show-and-speak?
            (con
             (cond ((eq? context 'slide)       
                      (a-tag (html-file (show-and-speak-slide-name lecture-id page-id 1)) 
                             (img-LN "atomspin-freeze.gif" 
                                     (key-shortcut (text-choice "Afspil indtalte slides" "Play audio slide show") "x"))))
                   ((eq? context 'show-and-speak)
                      (a-tag (html-file (slide-name lecture-id page-id)) 
                                    (img-LN "atomspin.gif"  
                                            (key-shortcut (text-choice "Stop afspilning med lyd" "Stop show with sound") "x"))))
                   (else ""))
              (space 2))
            "")


        (if (and (eq? context 'show-and-speak) next-in-show)
            (con
             (a-tag 
               next-in-show
               (img-LN "nav-right-red.gif" (key-shortcut (text-choice "Næste slide i dette show" "Next slide in show") "n")))
             (space 2))
             "")

        (if (eq? context 'slide)
            (con
        
             (a-tag (string-append (as-string lecture-id) ".html")
                    (img-LN  "nav-up.gif" (key-shortcut (text-choice "Lektionsindhold" "Lecture overview") "u"))) (space 2)

             (cond ((eq? context 'slide)
                    (if prev (a-tag (string-append (slide-name lecture-id prev) ".html")
                                    (img-LN "nav-left.gif" 
                                            (key-shortcut 
                                              (string-append (text-choice "Forrige side: " "Previous page: ") (old-title prev)) "p")))
                      (img-LN "nav-left-empty.gif" "")))
                   ((eq? context 'show-and-speak)
                    (if prev (a-tag (string-append (show-and-speak-slide-name lecture-id prev 1) ".html")
                                    (img-LN "nav-left.gif" 
                                            (key-shortcut 
                                              (string-append (text-choice "Forrige side: " "Previous page: ") (old-title prev)) "p")))
                      (img-LN "nav-left-empty.gif" "")))
                   (else "")
                   )

             (space 2)

             (cond ((eq? context 'slide)       
                    (if next (a-tag (string-append (slide-name lecture-id next) ".html") 
                                    (img-LN "nav-right.gif" 
                                      (key-shortcut 
                                        (string-append (text-choice "Næste side: " "Next page: ") (old-title next)) "n")))
                      (img-LN "nav-right-empty.gif" "")))
                   ((eq? context 'show-and-speak)
                    (if next (a-tag (string-append (show-and-speak-slide-name lecture-id next 1) ".html") 
                                    (img-LN "nav-right.gif" 
                                      (key-shortcut 
                                        (string-append (text-choice "Næste side: " "Next page: ") (old-title next)) "n")))
                      (img-LN "nav-right-empty.gif" "")))
                   (else ""))

             (space 2)

             (on-condition make-aggregated-view?
              (a-tag (book-url lecture-id (current-page))
                    (img-LN "nav-aggr.gif" 
                            (text-choice "Forelæsningsnoter - alle slides sammen"
                                         "Lecture notes - all slides together"))))

             (on-condition make-aggregated-view? (space 2))

             (on-condition make-annotated-slide-view?
              (a-tag (note-url lecture-id (current-page))
                    (img-LN "nav-notes.gif" (key-shortcut (text-choice "Annoteret slide" "Annotated slide") "t"))))

             (on-condition make-annotated-slide-view? (space 2))

             (if themes?
                 (let ((theme-url corresponding-theme-url))  ; global variable - assigned in original-note-page.
                   (if theme-url
	               (con (a-tag theme-url
				   (img-LN "nav-book.gif" (key-shortcut (text-choice "Lærebog" "Textbook") "v")))
			    (space 2))
                       ""))
		 "")

             (if word-index? 
                 (a-tag (all-word-index-url lecture-id) (img-LN "index.gif"(text-choice "Alfabetisk indeks" "Alphabetic index")))
               (space 3)) (space 2)

             (a-tag about-laml-url (img-LN "question.gif" (text-choice "Hjælp om disse noter" "Help page about these notes"))) (space 2)

             (if annotator-support?
                 (let ((number-of-annotations-now (length (list-of-submitted-annotations (as-string annotator-symbol) (as-string lecture-id) (as-string page-id)))))
                   (con
                    (a-tag-target
                     (annotator-url "annotator.cgi"
                                    (list 'annotation-id 'detail-level 'activation-mode 'language 'informative-title 'back-url)
                                    (list (encode-annotation-ids (list (as-string annotator-symbol) (as-string lecture-id) (as-string page-id)))
                                          "flat" "present-only" language-preference page-title 
                                          (course-absolute-url (string-append "html/noter/" (html-file (note-name lecture-id page-id)))))
                                    )
                     (img-LN 
                      (if (> number-of-annotations-now 0) "exclamation-red-ring.gif" "exclamation.gif")
                      (text-choice "Læs eller indsend kommentarer om denne side" "Read or submit annotations to this page")
                      )
                      "annotator"
                     )
                    (space 2)))
               "") 

             (a-tag course-home-url (img-LN "nav-home.gif" (text-choice "Kursets hjemmeside" "Course home"))) (space 2)


             (if show-and-speak?
                 (let ((sound (speak-name lecture-id page-id 'slide-part 1)))
                  (con
                  (cond ((eq? context 'slide)
 
                           (if (file-exists-robust? (sound-file-path sound))
                               (a-tag (speak-url sound)
                                     (img-LN "speaker.gif" 
                                             (key-shortcut (text-choice "Afspil lyd på denne slide" "Play sound for this slide") "y")))
                                     (img-LN "speaker-empty.gif" ""))
        
                         )
 
                        (else ""))
                  (space 2)))
               "")

             (space 2)

             (if (= page-number 1) (con (no-frames-button (html-file (slide-name lecture-id page-id))) (space 2)) "")

;            (font-1 2 white (as-string page-id))    ; hidden - for copying by knowledgable users.
            )
            ""
          ))


     (em (font-1 2 grey1 middle-info))

     (if (and lecture-number (> (length lecture-list) 1))
       (font-1 3 grey1 (b (con (this-lecture-context lecture-number lecture-title) 
               " - " 
               "slide " (as-string page-number) " : " (as-string (total-number-of-slides)))))
       (font-1 3 grey1 (b (con 
            "Slide " (as-string page-number) " : " (as-string (total-number-of-slides)) ))))

     (list) ; not used

    )))

(define showing-importance? #t)

(define (minimal-slide-header . optional-parameter-list)
 (let ((importance (optional-parameter 1 optional-parameter-list 100)))
  (left-right-banner
   ""
   (font-1 2 grey1 
           (con

            (if showing-importance?
                (font-color grey1
                            (cond ((<= importance 20)  "")
                                  ((and (>= importance 21) (<= importance 45)) "*")                                  
                                  ((and (>= importance 46) (<= importance 80)) "**")                                  
                                  ((>= importance 81) "***")))
                ""
                ) 

            " "

            (if lecture-number 
                (con (text-choice "Lektion " "Lecture ") (as-string lecture-number) " - ")
                "")
            "Slide " (as-string page-number) " : " (as-string (total-number-of-slides))
            )))))


(define (header-banner left right . unused-information)
 (left-right-banner left right))

(define (header-banner-3 left middle right . unused-information)
 (left-middle-right-banner left middle right))

(define (slide-header-topbar)
  (let* ((prev (previous-page))
         (next (next-page))
        )
    (table-3 0 (list 360 700 280)
     (list 
      (list
       (con 
        (a-tag-target (string-append (as-string lecture-id) ".html")
                        (img-LN  "nav-up.gif" (text-choice "Lektions indhold" "Lecture overview")) "_top") (space 2)
        (if prev (a-tag-target (string-append (slide-name lecture-id prev) ".html")
                        (img-LN "nav-left.gif" (text-choice "Forrige side" "Previous page")) "_top") (img-LN "nav-left-empty.gif" "")) (space 2) 
        (if next (a-tag-target (string-append (slide-name lecture-id next) ".html") 
                        (img-LN "nav-right.gif" (text-choice "Næste side" "Next page")) "_top") (img-LN "nav-right-empty.gif" "")) (space 2)
        (a-tag-target (string-append (slide-name-0 lecture-id (current-page)) ".html" "#slide-end")
                      (img-LN "arrow-down.gif" (text-choice "Til nederste del af siden" "To bottom of slide")) "main") (space 2)
        (a-tag-target (book-url lecture-id (current-page))
                        (img-LN "nav-aggr.gif" (text-choice "Forelæsningsnoter - alle sider sammen" "Lecture notes - all slides and notes together")) "_top") (space 2)
        (a-tag-target (note-url lecture-id (current-page))
                        (img-LN "nav-notes.gif" (text-choice "annoteret slide" "annotated slide")) "_top") (space 2)
        (if word-index? 
           (a-tag-target (all-word-index-url lecture-id) (img-LN "index.gif"(text-choice "Alfabetisk indeks" "Alphabetic index")) "_top")
           (space 3))

        (space 2)
        (a-tag-target course-home-url (img-LN "nav-home.gif" (text-choice "Kursets hjemmeside" "Course home")) "_top") (space 2)
;       (font-1 2 white (as-string page-id))  ; hidden - for copying by knowledgable users.
       )
     ""
     (if lecture-number
       (b (con (text-choice "Lektion " "Lecture ") (as-string lecture-number) " - slide " (as-string page-number) " : " (as-string (total-number-of-slides))))
       (b (con (font-color slide-background-color (con (text-choice "Lektion - " "Lecture - ") (as-string 0)))  ; suppress it
               "Slide " (as-string page-number) " : " (as-string (total-number-of-slides)) )))     )))
  ))

(define (old-slide-footer)
  (let* ((dt (date-time (current-time)))
         (date (car dt))
         (time (cadr dt))
         (prev (previous-page))
         (next (next-page))
         (cur (current-page)))
    (con (vertical-space 1)
         (b (con "Slide " (as-string page-number))) (space 3)
         (if prev (a-tag (string-append (slide-name lecture-id prev) ".html") "Previous") "") (if prev (space 3) "")
         (if next (a-tag (string-append (slide-name lecture-id next) ".html") "Next") "") (space 3)
         (if cur (a-tag (string-append (note-name lecture-id (current-page)) ".html") "Notes") "") (space 3)
         (a-tag (string-append (book-name lecture-id) ".html" "#" (as-string page-id)) "Chapter tutorial") (space 3)
         (a-tag-target course-home-url "Course home" "_top") (space 3)
         (a-tag (string-append (as-string lecture-id) ".html") "Index") (space 3)
         (a-tag about-laml-url "About the production of this web")
         (br)
         (font-1 2 red (con (text-choice "Genereret: " "Generated: ") date ", " time)) )))

(define (long-slide-footer)
    (let* ((prev (previous-page))
         (next (next-page)))
    (table-3 0 (list 200 1000 225)
     (list 
      (list
       (con 
        (a-tag (string-append (as-string lecture-id) ".html")
                        (img-LN  "nav-up.gif" (text-choice "Lektions indhold" "Lecture overview") )) (space 2)
        (if prev (a-tag (string-append (slide-name lecture-id prev) ".html")
                        (img-LN "nav-left.gif" (text-choice "Forrige slide" "Previous slide")))(img-LN "nav-left-empty.gif" "")) (space 2) 
        (if next (a-tag (string-append (slide-name lecture-id next) ".html") 
                        (img-LN "nav-right.gif" (text-choice "Næste slide" "Next slide"))) (img-LN "nav-right-empty.gif" "")) (space 2)
        (a-tag "#slide-start" (img-LN "arrow-up.gif" (text-choice "Til øverste del af slide" "To top of slide"))) (space 2)
        (a-tag (book-url lecture-id (current-page))
                        (img-LN "nav-aggr.gif" (text-choice "Forelæsningsnoter - alle sider sammen" "Lecture notes - all slides and notes together"))) (space 2)
        (a-tag course-home-url (img-LN "nav-home.gif" (text-choice "Kursets hjemmeside" "Course home")))
       )
     ""
     (if lecture-number
       (b (con (text-choice "Lektion " "Lecture ") (as-string lecture-number) " - slide " (as-string page-number) " : " (as-string (total-number-of-slides))))
       (b (con (font-color slide-background-color (con (text-choice "Lektion - " "Lecture - ") (as-string 0))) ; suppress it
               "Slide " (as-string page-number) " : " (as-string (total-number-of-slides)) )))     )))
  ))

(define (empty-slide-footer) "")

(define (note-header)
  (let* ((prev (previous-page))
         (next (next-page))

         (this-lecture-context 
           (lambda (lecture-number lecture-title)
             (string-append (em lecture-title))))

         (old-title 
           (lambda (page-id)
             (let ((aux-record (find-aux-record remembered-old-aux-structure page-id)))
                (if aux-record
                     (string-append
                        (title-of-aux-entry aux-record)
                        (if (section-start?-of-aux-entry aux-record) " [Section]" ""))
                     "???"))))


        )
   (presentation-slice
    (left-right-banner
      (con 
            (a-tag (string-append (as-string lecture-id) ".html")
                             (img-LN  "nav-up.gif" (key-shortcut (text-choice "Lektions indhold" "Lecture overview") "u"))) (space 2)
            (if prev
                (a-tag (string-append (note-name lecture-id prev) ".html")
		       (img-LN "nav-left.gif" 
                         (key-shortcut (string-append (text-choice "Forrige side: " "Previous page: ") (old-title prev)) "p")))
                (img-LN "nav-left-empty.gif" "")) (space 2) 

            (if next 
                (a-tag (string-append (note-name lecture-id next) ".html") 
		       (img-LN "nav-right.gif" 
                         (key-shortcut (string-append (text-choice "Næste side: " "Next page: ") (old-title next)) "n")))
                (img-LN "nav-right-empty.gif" ""))  (space 2)


            (on-condition make-aggregated-view?
             (a-tag (book-url lecture-id (current-page))
		   (img-LN "nav-aggr.gif"  
                           (text-choice "Forelæsningsnoter - alle sider sammen" "Lecture notes - all slides and notes together"))))

            (on-condition make-aggregated-view? (space 2))

            (on-condition make-slide-view?
              (a-tag (slide-url lecture-id (current-page))
                        (img-LN "nav-slide.gif" (key-shortcut (text-choice "slide" "slide") "t"))))

            (on-condition make-slide-view? (space 2))

            (if themes?
                 (let ((theme-url corresponding-theme-url))  ; global variable - assigned in original-note-page.
                   (if theme-url
	               (con (a-tag theme-url
				   (img-LN "nav-book.gif" (key-shortcut (text-choice "Lærebog" "Textbook") "v")))
			    (space 2))
                       ""))
		 "")

            (a-tag about-laml-url (img-LN "question.gif" (text-choice "Hjælp om disse noter" "Help page about these notes"))) (space 2)

            (if annotator-support?
            (let ((number-of-annotations-now (length (list-of-submitted-annotations (as-string annotator-symbol) (as-string lecture-id) (as-string page-id)))))
             (con
              (a-tag-target
               (annotator-url "annotator.cgi"
                  (list 'annotation-id 'detail-level 'activation-mode 'language 'informative-title 'back-url)
                  (list (encode-annotation-ids (list (as-string annotator-symbol) (as-string lecture-id) (as-string page-id)))
                        "flat" "present-only" language-preference page-title 
                        (course-absolute-url (string-append "html/noter/" (html-file (note-name lecture-id page-id)))))
               )
               (img-LN 
                 (if (> number-of-annotations-now 0) "exclamation-red-ring.gif" "exclamation.gif")
                 (text-choice "Læs eller indsend kommentarer om denne side" "Read or submit annotations to this page")
               )
               "annotator"
              )
              (space 2)))
            "")

            (if word-index? 
              (a-tag (all-word-index-url lecture-id) (img-LN "index.gif" (text-choice "Alfabetisk indeks" "Alphabetic index")))
            (space 3)) (space 2)

            (a-tag course-home-url (img-LN "nav-home.gif" (text-choice "Kursets hjemmeside" "Course home")))
            (space 2)

            (if show-and-speak?
                 (let ((sound (speak-name lecture-id page-id 'slide-part 1)))
                  (con
                    (if (file-exists-robust? (sound-file-path sound))
			(a-tag (speak-url sound)
			       (img-LN "speaker.gif" 
				       (key-shortcut (text-choice "Afspil lyd på denne side" "Play sound for this page") "y")))
			(img-LN "speaker-empty.gif" ""))
                    (space 2))
                 )
               "")

        )
     (font-1 2 grey1 
      (if (and lecture-number (> (length lecture-list) 1))
       (b (con ; (text-choice "Lektion " "Lecture ") (as-string lecture-number) " - " 
               (text-choice "Side " "Page ") (as-string page-number) " : " (as-string (total-number-of-slides))))
       (b (con (text-choice "Side " "Page ") (as-string page-number) " : " (as-string (total-number-of-slides)) )))) )

    ; Annotation part header
    (left-right-banner
      (font-1 2 grey1 
        (con 
	 (if (> (length lecture-sections) 1) (con (em (b notes-title)) (br)) "")
	 (b (this-lecture-context lecture-number lecture-title)) 
         (if show-and-speak? (con (br) (this-page-source-file-interval)) "")

	 ))      
      ;(eciu-note-view-context-navigation-table 1 page-number 1)
      (eciu-note-view-context-selector page-id)
    )
  )
  ))


(define (note-footer)
 (let* ((dt (date-time (current-time)))
        (date (car dt))
        (time (cadr dt))
        (prev (previous-page))
        (next (next-page))
        (cur (current-page)))
 (con (vertical-space 1)
   (b (con (text-choice "Side " "Page ") (as-string page-number))) (space 3)
   (if prev (a-tag (string-append (note-name lecture-id prev) ".html") (text-choice "Forrige" "Previous")) "") (if prev (space 3) "")
   (if next (a-tag (string-append (note-name lecture-id next) ".html") (text-choice "Næste" "Next")) "") (space 3)
   (if cur (a-tag (string-append (slide-name lecture-id (current-page)) ".html") "Slide") "") (space 3)
   (a-tag (string-append (book-name lecture-id) ".html" "#" (as-string page-id)) (text-choice "Forelæsningsnoter" "Lecture notes")) (space 3)
   (a-tag course-home-url (text-choice "Kursets hjemmeside" "Course home")) (space 3)  ; ???
   (a-tag (string-append (as-string lecture-id) ".html") (text-choice "Indeks" "Index")) (space 3)
   (a-tag about-laml-url (text-choice "Om frembringelsen af disse sider" "About producing this web") )
   (br) (font-1 2 red (con (text-choice "Genereret: " "Generated: ") date ", " time)) )))


(define (book-footer)
 (let* ((dt (date-time (current-time)))
        (date (car dt))
        (time (cadr dt)))
 (font-size 2
  (con 
   (vertical-space 1)
   (if (and lecture-number (number? lecture-number) (> (length lecture-list) 1))
       (i (con (text-choice "Kapitel" "Chapter") " " (as-string lecture-number) ": " lecture-title))
       (i  lecture-title))
   (br)  
   (a-tag course-home-url (text-choice "Kursets hjemmeside" "Course home")) (space 5)
   (a-tag author-home-url (text-choice "Forfatteren's hjemmeside" "Author home")) (space 5)
   (a-tag about-laml-url (text-choice "Om frembringelsen af disse sider" "About producing this web")) (space 5)
   (if previous-lecture
       (a-tag (book-url previous-lecture) (text-choice "Forrige lektion (top)" "Previous lecture (top)"))
       (font-color white (text-choice "Forrige lektion (top)" "Previous lecture (top)"))) (space 5)
   (if next-lecture
       (a-tag (book-url next-lecture) (text-choice "Næste lektion (top)" "Next lecture (top)"))
       (font-color white (text-choice "Næste lektion (top)" "Next lecture (top)"))) (space 5)
   (if previous-lecture
       (a-tag (book-url previous-lecture "BOTTOM") (text-choice "Forrige lektion (bund)" "Previous lecture (bund)"))
       (font-color white (text-choice "Forrige lektion (bund)" "Previous lecture (bund)"))) (space 5)
   (if next-lecture
       (a-tag (book-url next-lecture "BOTTOM") (text-choice "Næste lektion (bund)" "Next lecture (bund)"))
       (font-color white (text-choice "Næste lektion (bund)" "Next lecture (bund)"))) (space 5)
   (br) (font-1 2 red (con (text-choice "Genereret: " "Generated: ") date ", " time))
   (a-name "BOTTOM") ))))




(define (standard-note title contents keywords-to-meta down-links)
 (let ((meta-contribution (if (null? keywords-to-meta) 
                              '()
                              (list (list 'name "keywords" 
                                  'lang (meta-language language-preference)
                                  'content (comma-separated-string keywords-to-meta)))))
       (body (con 
               (note-header)
               contents
               ; (note-footer)
             ))

       (prev (if (previous-page) (previous-page) #f))
       (next (if (next-page) (next-page) #f))
      )


   (if java-scripting
    (page-with-keypress-script
      (as-string title) 
      meta-contribution
      body

      javascript-loading
      (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
      (js-call "lenoNavigate" 
       (append
        (map string-it-single 
         (list (if prev (note-url lecture-id prev)  "")
               (if next (note-url lecture-id next) "")
               (contents-url lecture-id)
               (if make-slide-view? (slide-url lecture-id (current-page)) "")
               "" 
               (if show-and-speak? (speak-url (speak-name lecture-id page-id 'slide-part 1)) "")  ; y
               (if themes? (if corresponding-theme-url corresponding-theme-url "") "") ; z
              ))
       (list (js-string-array down-links))))

      note-background-color black black black
      mouse-advancement
      (actual-stylesheets apply-css-styling?)
      (leno-css-class 'note)
    )
    (page-1 
      (as-string title) 
      meta-contribution
      body
      note-background-color black black black 
      (actual-stylesheets apply-css-styling?)
      (leno-css-class 'note)
    )
 )))


;-----------------------------------------------------------------------------

; keyword handling

; Element constructors:

(define possible-leno-elements 
  '(title  point  point  items  item  text  program  source-program  image  concepts  example  opposing  
    comment  language-syntax  slide-space  tabular  note-text  slide-text  cross-references  exercise  formulation
    solution  long-slide  index-words  image-series  section-title slide-image  concept-list  concept  
    applet-program  synopsis  synopsis-item  quotation  quiz  question  answers  answer  show-and-speak 
    lecturer-photos lecturer-photos-and-logo elucidate elucidator-program-fragment 
    splice-page-with splice-page-without side-track svg-image flash-image theme-text meta-text))


; ------------------------------------------------------------------
; Original style element selectors. 
; Correspond to the constructor make-element in lecture-notes.scm

(define element-tag first)
(define element-id second)
(define element-contents third)
(define element-attributes fourth) ; for future use - XML-like LAML.

; Predicates:

; Have we requested that original-element should be dropped.
; Original element is an original element in LENO, 
(define (drop-element? original-element)
 (let ((attributes (element-attributes original-element))) ; alist
   (if attributes   ; always true
       (let ((drop-value (defaulted-get 'drop attributes #f)))
          drop-value)
       #f)))

; ------------------------------------------------------------------

(define (keyword-parameter key par-list)
 (let ((res (assq key par-list)))
   (if res (cdr res) "")))



;-----------------------------------------------------------------------------

; dispatching function;

(define (find-processor style kind)
  ; return function given the symbols style and kind
 (cond ((eq? style 'slide-style)
        (cond ((eq? kind 'title) slide-style-title)
              ((eq? kind 'text) slide-style-text)
              ((eq? kind 'items) slide-style-items)
              ((eq? kind 'point) slide-style-point)
              ((eq? kind 'program) slide-style-program)
              ((eq? kind 'source-program) slide-style-source-program)
              ((eq? kind 'image) slide-style-image)
              ((eq? kind 'concepts) slide-style-concepts)
              ((eq? kind 'example) slide-style-example)
              ((eq? kind 'opposing) slide-style-opposing)
              ((eq? kind 'comment) empty-slide-element)  ; Alternative: slide-style-comment
              ((eq? kind 'language-syntax) slide-style-syntax)
              ((eq? kind 'slide-space) slide-style-slide-space)
              ((eq? kind 'tabular) slide-style-tabular)
              ((eq? kind 'note-text) slide-style-note-text)
              ((eq? kind 'slide-text) slide-style-slide-text)
              ((eq? kind 'cross-references) slide-style-cross-references)
              ((eq? kind 'exercise) slide-style-exercise)
              ((eq? kind 'long-slide) empty-slide-element)
              ((eq? kind 'index-words) slide-style-index-words)
              ((eq? kind 'image-series) slide-style-image-series)
              ((eq? kind 'section-title) slide-style-section-title)
              ((eq? kind 'slide-image) slide-style-slide-image)
              ((eq? kind 'concept-list) slide-style-concept-list)
              ((eq? kind 'applet-program) slide-style-applet-program)
              ((eq? kind 'synopsis) slide-style-synopsis)
              ((eq? kind 'quotation) slide-style-quotation)
              ((eq? kind 'quiz) slide-style-quiz)
              ((eq? kind 'show-and-speak) slide-style-show-and-speak)
              ((eq? kind 'lecturer-photos) slide-style-lecturer-photos)
              ((eq? kind 'lecturer-photos-and-logo) slide-style-lecturer-photos-and-logo) 
              ((eq? kind 'elucidate) slide-style-elucidate)
              ((eq? kind 'elucidator-program-fragment) slide-style-elucidator-program-fragment)
              ((eq? kind 'side-track) slide-style-side-track)
              ((eq? kind 'svg-image) slide-style-svg-image)
              ((eq? kind 'flash-image) slide-style-flash-image)
              ((eq? kind 'theme-text) empty-slide-element)
              ((eq? kind 'meta-text) empty-slide-element)
              (else (error (string-append "Cannot find slide processor function: " (as-string kind))))))
       ((eq? style 'note-style)
        (cond ((eq? kind 'title) note-style-title)
              ((eq? kind 'text) note-style-text)
              ((eq? kind 'items) note-style-items)
              ((eq? kind 'point) note-style-point)
              ((eq? kind 'program) note-style-program)
              ((eq? kind 'source-program) note-style-source-program)
              ((eq? kind 'image) note-style-image)
              ((eq? kind 'concepts) note-style-concepts)
              ((eq? kind 'example) note-style-example)
              ((eq? kind 'opposing) note-style-opposing)
              ((eq? kind 'comment) note-style-comment)
              ((eq? kind 'language-syntax) note-style-syntax)
              ((eq? kind 'slide-space) note-style-slide-space)
              ((eq? kind 'tabular) note-style-tabular)
              ((eq? kind 'note-text) note-style-note-text)
              ((eq? kind 'slide-text) empty-slide-element)
              ((eq? kind 'cross-references) note-style-cross-references)
              ((eq? kind 'exercise) note-style-exercise)
              ((eq? kind 'long-slide) empty-slide-element)
              ((eq? kind 'index-words) empty-slide-element)
              ((eq? kind 'image-series) note-style-image-series)
              ((eq? kind 'section-title) note-style-section-title)
              ((eq? kind 'slide-image) note-style-slide-image)
              ((eq? kind 'concept-list) note-style-concept-list)
              ((eq? kind 'applet-program) note-style-applet-program)
              ((eq? kind 'synopsis) note-style-synopsis)
              ((eq? kind 'quotation) note-style-quotation)
              ((eq? kind 'quiz) empty-slide-element)
              ((eq? kind 'show-and-speak) empty-slide-element)
              ((eq? kind 'lecturer-photos) empty-slide-element)
              ((eq? kind 'lecturer-photos-and-logo) empty-slide-element)
              ((eq? kind 'elucidate) note-style-elucidate)
              ((eq? kind 'elucidator-program-fragment) note-style-elucidator-program-fragment) 
              ((eq? kind 'side-track) note-style-side-track) 
              ((eq? kind 'svg-image) note-style-svg-image)
              ((eq? kind 'flash-image) note-style-flash-image)
              ((eq? kind 'theme-text) empty-slide-element)
              ((eq? kind 'meta-text) note-style-meta-text)
              (else (error (string-append "Cannot find note processor function: " (as-string kind))))))
       ((eq? style 'book-style)
        (cond ((eq? kind 'title) book-style-title)
              ((eq? kind 'text) book-style-text)
              ((eq? kind 'items) book-style-items)
              ((eq? kind 'point) book-style-point)
              ((eq? kind 'program) book-style-program)
              ((eq? kind 'source-program) book-style-source-program)
              ((eq? kind 'image) book-style-image)
              ((eq? kind 'concepts) book-style-concepts)
              ((eq? kind 'example) book-style-example)
              ((eq? kind 'opposing) book-style-opposing)
              ((eq? kind 'comment) book-style-comment)
              ((eq? kind 'language-syntax) book-style-syntax)
              ((eq? kind 'slide-space) empty-slide-element)
              ((eq? kind 'tabular) book-style-tabular)
              ((eq? kind 'note-text) book-style-note-text)
              ((eq? kind 'slide-text) empty-slide-element)
              ((eq? kind 'cross-references) book-style-cross-references)
              ((eq? kind 'exercise) book-style-exercise)
              ((eq? kind 'long-slide) empty-slide-element)
              ((eq? kind 'index-words) empty-slide-element)
              ((eq? kind 'image-series) book-style-image-series)
              ((eq? kind 'section-title) book-style-section-title)
              ((eq? kind 'slide-image) empty-slide-element)
              ((eq? kind 'concept-list) book-style-concept-list)
              ((eq? kind 'applet-program) book-style-applet-program)
              ((eq? kind 'synopsis) book-style-synopsis)
              ((eq? kind 'quotation) book-style-quotation)
              ((eq? kind 'quiz) empty-slide-element)
              ((eq? kind 'show-and-speak) empty-slide-element)
              ((eq? kind 'lecturer-photos) empty-slide-element)
              ((eq? kind 'lecturer-photos-and-logo) empty-slide-element)
              ((eq? kind 'elucidate) book-style-elucidate)
              ((eq? kind 'elucidator-program-fragment) book-style-elucidator-program-fragment) 
              ((eq? kind 'side-track) book-style-side-track)
              ((eq? kind 'svg-image) book-style-svg-image )
              ((eq? kind 'flash-image) book-style-flash-image)
              ((eq? kind 'theme-text) empty-slide-element)
              ((eq? kind 'meta-text) book-style-meta-text)
              (else (error (string-append "Cannot find book processor function: " (as-string kind))))))

       (else (error "Cannot find slide, note, book, or theme style"))))



;-----------------------------------------------------------------------------

(define (as-slide-text txt)
  (if apply-css-styling?
      txt
      (font-size slide-font-size txt)))

(define (note-text-left txt)
  (if apply-css-styling?
      txt
      (font-size note-font-size-left txt)
  ))

; (define (note-text-right txt)
;   (if #f ; earlier: apply-css-styling?
;       txt
;       (font-1 note-font-size-right note-foreground-color txt)))

; Introduced May 7, 2008:
(define (note-text-right txt)
  (if #f ; earlier: apply-css-styling?
      txt
      (leno-css-styled 'note 'note-text txt)))

(define (book-text txt)
  (if apply-css-styling?
      txt 
      (font-1 book-font-size book-foreground-color txt)))

(define (expand-item i)
  ; return a triple (slide-item note-item sub-items)
 (cond ((string? i)                                                          
             (list i "" ""))
       ((and (pair? i) (eq? (element-tag i) 'item) (= (length (element-contents i)) 1))
             (list (car (element-contents i)) "" ""))
       ((and (pair? i) (eq? (element-tag i) 'item) (= (length (element-contents i)) 2))
             (list (car (element-contents i)) (cadr (element-contents i)) ""))
       ((and (pair? i) (eq? (element-tag i) 'item) (= (length (element-contents i)) 3))
           (list (car (element-contents i)) (cadr (element-contents i)) (caddr (element-contents i))))
       (else (error "Problems expanding slide or note item"))))

; extracts a slide item from i, and processes subitems recursively.
(define (slide-item i)
  (let* ((ei (expand-item i))
         (raw-sub-items (caddr ei)))
    (if (equal? "" (car ei))
        (display-warning "First parameter of an item is empty. Causes problems in some browsers"))
    (if (not (equal? raw-sub-items ""))
        (let ((sub-items (present-element 'slide-style  ; may in some contexts be note-style
                                          'items (element-contents raw-sub-items) (element-attributes raw-sub-items) )))
           (con (car ei) (p) sub-items))
        (con (car ei) (p))
    )))



; includes item comments of level 1 and 2. Deeper item comments are ignored:    
(define (note-item i)  ; i may be a string or of the form (item "x" "y")
  (let* ((ei (expand-item i))
         (sub-comments (aggregate-comments (caddr ei))))
    (con (cadr ei) (p) sub-comments)))

(define (aggregate-comments itms)
  (cond  ((equal? "" itms) "")
         ((and (pair? itms) (eq? (car itms) 'items))
            (let* ((sub-items (element-contents itms))
                   (sub-items-1 (map expand-item sub-items)))
              (string-merge (map cadr sub-items-1) (make-list (length sub-items-1) (p)))))
         (else "")))

(define (program-extract program-file-name mark)
  ; Extract file portion based on file name and mark, which can be the empty string (extract entire file),
  ; a string (extract portion between identical start and end marks), or a list of length two, extract 
  ; string between and including potentially different start and end mark.

  (cond ((and (string? mark) (equal? mark "")) 
           (no-trailing-CRs-in-string (read-text-file program-file-name)))
        ((and (string? mark) (> (string-length mark) 0))  
           (no-trailing-CRs-in-string (read-text-file-between-marks program-file-name mark)))
        ((and (pair? mark) (= (length mark) 2))
           (no-trailing-CRs-in-string (read-text-file-including-marks program-file-name (car mark) (cadr mark))))
        (else (error "Problems in source program extraction"))))

; Remove trailing CRs and spaces in str. Do, however, not remove spaces from last line.
(define (no-trailing-CRs-in-string str)
  (let ((pos (find-in-string-from-end-by-predicate 
                (lambda (ch) (not (or (eqv? ch (as-char 13)) (eqv? ch (as-char 32)) (eqv? ch (as-char 10)))))
                str)))
    (if pos
        (let ((pos1 (skip-chars-in-string str (list (as-char 32)) (+ pos 1))))
          (if pos1
              (substring str 0 pos1)
              str  ; should not happen
          ))
        str)))
                   
; -----------------------------------------------------------------------------      
; concept-list selectors:

(define concept-name car)
(define concept-description cadr)
(define concept-explanation caddr)

; -----------------------------------------------------------------------------      

; slide-style

(define (empty-slide-element . nt)
   "")

(define (slide-style-title . x)
 (let* ((title (car x))
        (comment (if (> (length x) 1) (cadr x) ""))
        (body title) ; add vertical-space?
       )

  ; add title to index words:
  (if (not show-and-speak-context)
      (add-words-to-index (list title) lecture-id page-id 'title))

  (if apply-css-styling?
      (leno-css-styled 'slide 'title body)
      (con (font-1 7 title-color (b body)) (vertical-space 1))
  )))


(define (slide-style-text . x)
 (let* ((text (car x))
        (comment (if (> (length x) 1) (cadr x) ""))
        (body text)
       )
  (if apply-css-styling?
      (leno-css-styled 'slide 'text body) 
      (indent-pixels 10 (con (as-slide-text body) (vertical-space 1))))))

; (define (slide-style-items . i)
;   (if apply-css-styling?
;       (ul (map (lambda (x) (leno-css-styled 'slide 'items (li x))) (map slide-item i)))
;       (ul-1 (map slide-item i))))

(define (slide-style-items . i)
 (let ((li-div (modify-element li 'class (leno-css-class 'slide 'item) ))
       (non-drop-element? (negate drop-element?))
      )
  (let ((i1 (filter non-drop-element? i)))
   (if apply-css-styling?
      (ul 'class (leno-css-class 'slide 'items) (id-attribute-contribution) (map li-div (map slide-item i1)))
      (ul-1 (map slide-item i1))))))



(define (slide-style-point . x)
 (let* ((point (car x))
        (comment (if (> (length x) 1) (cadr x) ""))
        (body point)
       )
  (if apply-css-styling?
      (center (leno-css-styled 'slide 'point body (aggregated-page-element-id)))        
      (con (center-frame 30 (font-color red (as-slide-text body))) (vertical-space 1)))))

(define (slide-style-program . x)
 (let* ((program-text (car x))
        (comment (if (> (length x) 1) (cadr x) ""))
        (body (pre program-text))
       )
  (if apply-css-styling?
      (leno-css-styled 'slide 'program-clause body (aggregated-page-element-id)) 
      (indent-pixels 10 (con (as-slide-text body) (vertical-space 1))))))

; Four required parameters: program-file-name, mark, color-description-triples, how-to-show.
; Optional positional parameters: comment (5), background-colors (6), indexed? (7), and index-title (8)
; If background-color is #f, apply no background color.
(define (slide-style-source-program . x)
 (let* ((program-file-name (if (absolute-file-path? (first x))
                               (first x) 
                               (string-append note-source-directory (first x))))
        (mark (second x))
        (colorizing (third x))
        (extracted-program-and-doc (program-extract program-file-name mark))
        (program-text 
           (html-protect
             (if extract-and-present-end-of-line-doc-comments
                 (extract-program-source extracted-program-and-doc)
                 extracted-program-and-doc)))
        (documentation-text? (turn-into-boolean (substring-index extracted-program-and-doc 0 end-of-line-doc-comment-prefix)))
        (colorized-program-text (colorize-substrings program-text colorizing)) 
        (how-to-show (fourth x))
        (comment (if (> (length x) 4) (fifth x) ""))
        (background-color (if (> (length x) 5) (sixth x) slide-background-color))    
        (initial-sentence (first-sentence-in-string comment))
        (indexed? (if (> (length x) 6) (seventh x) #f))    ; #f to protect old material
        (index-title (if (> (length x) 7) 
                         (let ((par-index-title (eighth x)))
                            (if (blank-string? par-index-title) initial-sentence par-index-title))
                         initial-sentence))
        (kind (if (> (length x) 8) (as-symbol (nineth x)) 'source-program))
       )
   
   (slide-style-source-program-1 program-file-name colorized-program-text how-to-show 
                                 comment initial-sentence background-color indexed? index-title kind documentation-text?)))


; The function doing the real work of slide-style-source-program. Also used by slide-style-elucidator-program-fragment.
(define (slide-style-source-program-1 program-file-name colorized-program-text how-to-show 
                                      comment initial-sentence background-color indexed? index-title kind is-program-annotated?)
  (if indexed?
    (set! list-of-source-files 
          (cons (make-source-file-descriptor program-file-name index-title kind page-id) list-of-source-files)))   

  (let ((backgr-color-list-css (if background-color (list 'css:background-color background-color) '()))
	(backgr-color (if background-color background-color slide-background-color)))

   (if (eq? (car how-to-show) 'slide-inline)
       (let ((body (pre colorized-program-text))
            )
	 (if apply-css-styling?
	     (leno-css-styled 'slide 'source-program (div backgr-color-list-css body) (aggregated-page-element-id)) 
	     (center (con (color-frame (div 'align "left" (as-slide-text body)) backgr-color)  (vertical-space 1)))))
       (let* ((file-name (program-name lecture-id page-id 'slide program-number)) ;@a
	      (this-number program-number)
              (file-extension (file-name-extension program-file-name))
              (short-program-file-name (string-append (file-name-proper program-file-name) 
                                              (if (empty-string? file-extension) "" ".")
                                              file-extension))
              (left-sep-item (separate-page-item-before 'program this-number))
              (right-sep-item (separate-page-item-after 'program this-number))
              (icon-element (a-tag (string-append file-name ".html") 
						     (img-LN (program-icon-name language-preference 'large kind) short-program-file-name)))
             ) 

;         Elided March 28, 2007:
;         (if (not show-and-speak-context)
;             (add-words-to-index (list short-program-file-name) lecture-id page-id 'source-program))

         (set! program-number (+ program-number 1)) ;@b
         (save-slide-program colorized-program-text file-name (slide-url lecture-id page-id) 
                             (if left-sep-item 
                                 (local-html-url (apply separate-slide-page-item-name
                                                          (append (list lecture-id page-id) 
                                                          left-sep-item) ))
                                 #f)
                             (if right-sep-item 
                                 (local-html-url (apply separate-slide-page-item-name
                                                          (append (list lecture-id page-id)
                                                          right-sep-item)))
                                 #f)
                             (if is-program-annotated?
                                 (local-html-url (program-name lecture-id page-id 'note this-number))
                                 #f
                             )
                             (slide-show-and-speak-url lecture-id page-id 1) initial-sentence   (some-suffix-of program-file-name)     ; Before Oct 25, 2007:  short-program-file-name
                              this-number backgr-color )
         (if apply-css-styling?
             (table 'class (leno-css-class 'slide 'source-program) 
                    (tr (td 'width "110" icon-element)
                        (td 'width "900" 
                            (leno-css-styled-extra 'slide 'source-program 'caption initial-sentence (aggregated-page-element-id)))))
             (indent-pixels 10 
			(con
			 (table-3 0 (list 100 900) 
				  (list (list icon-element
					      (if apply-css-styling?
						  (leno-css-styled-extra 'slide 'source-program 'caption initial-sentence (aggregated-page-element-id))
						  (font-color grey1 (b initial-sentence))
						  )))
				  "middle")
			 (if apply-css-styling? "" (vertical-space 1)))))))))

; This function can be redefined in post-notes.scm if you wish that a larger part of the program-file-path appears in your teaching material:
(define (some-suffix-of program-file-path)
 (let ((file-name (file-name-proper program-file-path))
       (file-extension (file-name-extension program-file-path)))
   (string-append file-name 
		  (if (empty-string? file-extension) "" ".")
		  file-extension)))  


; about the formatter third parameter to image: Either
; '(vertical picture)                 just show picture          DEFAULT
; '(vertical text)                    just show text
; '(vertical picture text)            show picuture above text
; '(vertical text picture)            show picture below text 
; '(horizontal text picture)          show picture to the right of text
; '(horizontal picture text)          show picture to the left of text
; only used in slide style.

(define (slide-style-image . x)
 (let* ((image-file (car x))
       (comment (if (> (length x) 1) (cadr x) ""))
       (formatter (if (> (length x) 2) (caddr x) '(vertical picture)))
      )
  (if (not show-and-speak-context)
      (set! note-specific-graphics-files (cons image-file note-specific-graphics-files)))
  (cond ((eq? (car formatter) 'vertical) (vertical-slide-style-image image-file comment (cdr formatter)))
        ((eq? (car formatter) 'horizontal) (horizontal-slide-style-image image-file comment (cdr formatter)))
        (else (error "Problems in slide-style-image: First formatter element must be the symbol vertical or horizontal")))))



(define (vertical-slide-style-image image-file comment element-sequence)
  ;element sequence is a list of symbols: 'picture and/or 'text
 (let ((image-contribution-css (img 'alt "" 'src (string-append  "." "/" "graphics/" image-file) 'class (leno-css-class 'slide 'image) (id-attribute-contribution)))
       (image-contribution-non-css (img 'alt "" 'src (string-append  "." "/" "graphics/" image-file) 'border 0))
      )
 (con 
  (apply
    string-append
    (map (lambda (formatting)
           (cond ((eq? formatting 'text)
                     (if apply-css-styling?
                         (center (leno-css-styled-extra 'slide 'image 'text comment (aggregated-page-element-id "text")))
                         (con (center (as-slide-text comment)) (vertical-space 1))))
                 ((eq? formatting 'picture) (con (center (if apply-css-styling? image-contribution-css image-contribution-non-css)) (vertical-space 1)))
                 (else (error "Image formatter element must be either the symbol text or picture"))
           )
         )
         element-sequence
     ))
 )))

(define (horizontal-slide-style-image image-file comment element-sequence)
 (let ((image-text 
         (if apply-css-styling?
             (leno-css-styled-extra 'slide 'image 'text comment (aggregated-page-element-id))
             (as-slide-text comment)))
       (image-contribution-css (img 'alt "" 'src (string-append  "." "/" "graphics/" image-file) 'class (leno-css-class 'slide 'image) (id-attribute-contribution)))
       (image-contribution-non-css (img 'alt "" 'src (string-append  "." "/" "graphics/" image-file) 'border 0))
      )

  (indent-pixels 10
   (con
    (cond ((equal? element-sequence '(picture text))
           (table-3 0 (list "47%" "6%" "47%")
                    (list (list (center (if apply-css-styling? image-contribution-css image-contribution-non-css)) "" image-text)) "top"))
          ((equal? element-sequence '(text picture))
           (table-3 0 (list "47%" "6%" "47%")
                    (list (list image-text "" (center  (if apply-css-styling? image-contribution-css image-contribution-non-css)))) "top"))
          (else (error "Image formatter must be either (horizontal text picture) or (horizontal picture text)")))
   ))))

(define (slide-style-example . x)
 (let* ((the-example (car x))
        (about-the-example (if (> (length x) 1) (cadr x) ""))
        (body the-example)
       )
  (if apply-css-styling?
      (leno-css-styled 'slide 'example body (aggregated-page-element-id)) 
      (indent-pixels 10
        (con (i (as-slide-text the-example)) (vertical-space 2))))))


; Old colored dot version:
;(define (slide-style-concepts . x)
;  (con (indent-pixels 10 (bullet-list (map as-slide-text (map extra-space-after (map slide-item x))) 'large 'yellow))
;       (vertical-space 1)))
;
;(define (extra-space-after x) (con x (vertical-space 1)))


; html
(define (center-frame-color indentation color text)
  (center
   (narrow-with-pixels indentation
     (color-frame text color))))

; html

; probably obsolete
(define (slide-style-concepts . x)
  (con (center-frame-color 30 concept-background
         (apply string-append
             (map (lambda (x) (con (p) (font-color white x)))
                  (map as-slide-text (map slide-item x)))))
       (vertical-space 1)))


(define (slide-style-concept-list . x)
 ; x is a list of concept forms: ((concept . (- - -)) ...)
 (let* ((real-concept-list (map element-contents x))
        (raw-body    ; redundant
          (apply string-append
               (map (lambda (x) (con (p) x)) ; perhaps only x
                    (map emphasize-concept-name 
                              real-concept-list))))
       )
  (if (not show-and-speak-context)
      (add-words-to-index (map concept-name real-concept-list) lecture-id page-id 'concept))

  (if apply-css-styling?
      (center (leno-css-styled 'slide 'concept-list raw-body (aggregated-page-element-id))) 
      (con (center-frame-color 30 concept-background
           (apply string-append
                (map (lambda (x) (con (p) (font-color white x)))
                     (map as-slide-text  
                       (map emphasize-concept-name 
                            real-concept-list)))))
          (vertical-space 1)))))

; Return the concept description of c, with emphasis of the concept name
(define (emphasize-concept-name c)
 (embed-substring 
  (concept-name c) 
  (embed-substring (capitalize-string-nd (concept-name c)) (concept-description c) em)
  em))

(define (slide-style-opposing . x)
  (if apply-css-styling?
      (center
       (table 
         'border 0
         'class (leno-css-class 'slide 'opposing) (id-attribute-contribution)
         (map
           (lambda (two-item-list)
             (tr (list (td 'class (leno-css-class 'slide 'opposing 'left-column) (slide-item two-item-list))
                       (td 'class (leno-css-class 'slide 'opposing 'right-column) (note-item two-item-list)))))
           x)))
      (con
       (center
        (table-1 0 (list 10 "*" 40 "*") (make-list 4 slide-background-color)
                 (map (lambda (a1-a2) 
                        (list "" (as-slide-text (slide-item a1-a2))
                              (img-0 (image-file "blank-pillar.gif")) (as-slide-text (con (note-item a1-a2) (p) ))))
                      x)
                 ))
       (vertical-space 1)
       )
  )
)

(define (slide-style-comment . x)
   (let* ((comment (car x))
          (body comment)
         )
  (if apply-css-styling?
      (leno-css-styled 'slide 'comment body (aggregated-page-element-id)) 
      (indent-pixels 10
        (i (as-slide-text (con (text-choice "Intern kommentar: " "Internal comment: ") body (vertical-space 1))))))))

; old non-centered
; (define (slide-style-syntax . x)
;    (let* ((syntax-form (car x))
;           (syntax-form-1 (string-append CR syntax-form))
;           (comment (if (> (length x) 1) (cadr x) "")))
;      (indent-pixels 10
;        (con (color-frame (pre (as-slide-text syntax-form-1)) syntax-frame-color) (vertical-space 1)))))

; centered:
(define (slide-style-syntax . x)
   (let* ((syntax-form (car x))
          (syntax-form-1 (string-append CR syntax-form))
          (comment (if (> (length x) 1) (cadr x) ""))
          (raw-body (pre syntax-form-1))
         )
     (if apply-css-styling?
         (leno-css-styled 'slide 'syntax raw-body (aggregated-page-element-id))      
         (center
;            (con (color-frame (pre (div 'align "left" 'css:margin_bottom "-5mm" 'css:margin_top "-5mm" (as-slide-text syntax-form-1))) syntax-frame-color) (vertical-space 1))
            (con (color-frame (div 'align "left" 'css:margin-bottom "-5mm" 'css:font-size "25px" (pre syntax-form-1)) syntax-frame-color) (vertical-space 1))
     ))))


(define (slide-style-slide-space . x)
  (if apply-css-styling?
      (leno-css-styled 'slide 'slide-space (vertical-space 1) (aggregated-page-element-id))   ; temporarily
      (vertical-space 1)))

(define (slide-style-tabular border column-widths list-of-list . comment)
  (let* ((com (if (null? comment) "" (car comment))))
   (if apply-css-styling?
       (center
        (table 
         'border (as-string border)
         'class (leno-css-class 'slide 'tabular) (id-attribute-contribution) 
         (map
           (lambda (row)
             (tr (map (lambda (cell width)
                        (td 'width (as-string width) 'align "left" (as-string cell)))
                      row column-widths)))
           list-of-list)))
         
       (con
         (center
	  (table 
	   'border (as-string border)
	   (map
	    (lambda (row)
	      (tr (map (lambda (cell width)
			 (td 'width (as-string width) 'align "left"
                         'bgcolor (rgb-color-encoding tabular-slide-color)
                         (font-size 5 (as-string cell))))
		       row column-widths)))
	    list-of-list)))
         (vertical-space 1)))))



(define (slide-style-note-text nt)
   "")

(define (slide-style-slide-text st)
  (if apply-css-styling?
      (leno-css-styled 'slide 'slide-text st (aggregated-page-element-id))   
      (con
         (indent-pixels 10 (font-size slide-font-size st))
         (vertical-space 1))))

(define (slide-style-cross-references list-of-cross-refs)
 (let* ((hv (cond ((= (length list-of-cross-refs) 1) (text-choice "Henvisning" "Reference"))
                  ((> (length list-of-cross-refs) 1) (text-choice "Henvisninger" "References"))
                  ((< (length list-of-cross-refs) 1) (text-choice "Ingen henvisninger" "No references"))))
        (formatter-function (generate-cross-reference-formatter slide-url))
        (filtered-refs list-of-cross-refs)  ; before: (filter no-informal-references list-of-cross-refs )
        (formatted-refs-0 (map formatter-function filtered-refs)) ; gives a list or url and hint
        (formatted-refs (glue-ref-and-hint formatted-refs-0))
        (formatted-ref-string-non-css (ul-1 (map (lambda (r) (font-size 2 r)) formatted-refs)))
        (formatted-ref-string-css (ul-1 (map (lambda (r) (leno-css-styled-extra 'slide 'cross-reference 'item r )) formatted-refs)))
       )
  (if (not show-and-speak-context)
      (set! cross-refs-in-this-lecture (append list-of-cross-refs cross-refs-in-this-lecture)))
  (if (> (length filtered-refs) 0)
      (if apply-css-styling?
          (leno-css-styled 'slide 'cross-reference formatted-ref-string-css (aggregated-page-element-id))
        (con
         (indent-pixels 
          10                            ; cross references at right hand side of slide:
          (table-3 0 (list 1200 200)
                   (list (list "" formatted-ref-string-non-css)))

          )
         (vertical-space 1)))
   "")
  )
)



(define (no-informal-references cross-ref-record)
  ; true for all but informal references
  (not (eq? (cross-ref-type cross-ref-record) 'informal-reference)))


(define (slide-style-exercise id-symbol title formulation-clause . optional-parameter-list)
 (let ((solution-clause-1 (optional-parameter 1 optional-parameter-list #f))
       (rank (optional-parameter 2 optional-parameter-list #f))
      ) 
  (let* ((formulation-text (car (element-contents formulation-clause)))
 ;        (solution-clause-1 (if (not (null? solution-clause)) (car solution-clause) #f))
         (solution-text (if solution-clause-1 (car (element-contents solution-clause-1)) ""))
         (solution-file-name (exercise-solution-name lecture-id page-id exercise-number))
         (secret-solution-file-name (exercise-secret-solution-name lecture-id page-id exercise-number))
         (file-name (exercise-name lecture-id page-id 'slide exercise-number))
         (link-file-name ((if exercise-manager-support? exercise-frame-name  exercise-name) lecture-id page-id 'slide exercise-number))
         (solution-url (if solution-clause-1 (exercise-solution-url lecture-id page-id exercise-number) ""))
         (icon-element (a-tag (string-append link-file-name ".html")
                              (img-LN (exercise-icon-name language-preference 'large) (text-choice "Gå til opgave" "Go to exercise"))))
         (left-sep-item (separate-page-item-before 'exercise exercise-number))
         (right-sep-item (separate-page-item-after 'exercise exercise-number))
        )
    (set! current-exercise-id id-symbol)
    (if (not show-and-speak-context)
        (set! lecture-exercise-list 
              (cons 
                (list id-symbol title page-id exercise-number formulation-text next-exercise-number solution-text rank)
                lecture-exercise-list)))

    ; Add exercise title to the word index:
    (if (not show-and-speak-context)
        (add-words-to-index (list (string-append title))
                            lecture-id page-id 'exercise))

    (save-slide-exercise title formulation-text file-name (slide-url lecture-id page-id)
                         (if left-sep-item 
                             (local-html-url (apply separate-slide-page-item-name
                                                    (append (list lecture-id page-id) 
                                                            left-sep-item) ))
                             #f)
                         (if right-sep-item 
                             (local-html-url (apply separate-slide-page-item-name
                                                    (append (list lecture-id page-id)
                                                            right-sep-item)))
                             #f)
                         solution-url next-exercise-number rank)

    (set! exercise-number (+ 1 exercise-number))

    ; The solution also saved in note-style-exercise.
    ; this is made to ensure that solutions also are saved when no note style is generated.

    ; always save solution file. Hard to find because of password ingredient in name. 
    ; Links are only made to this file in case we want to reveal solutions.
    (save-exercise-solution title solution-text solution-file-name (slide-url lecture-id page-id)
                                next-exercise-number)

    ; always store the 'secret' solutions
    (save-exercise-solution title solution-text secret-solution-file-name (slide-url lecture-id page-id)     
                                next-exercise-number)


    (set! next-exercise-number (+ 1 next-exercise-number))
    (if apply-css-styling?
        (table 'class (leno-css-class 'slide 'exercise) 
               (tr (td 'width "110" icon-element)
                   (td 'width "900" (leno-css-styled-extra 'slide 'image 'caption title (aggregated-page-element-id)))))
         (indent-pixels 
          10 
          (con
           (table-3 0 (list 100 900) 
                    (list (list icon-element
                                (if apply-css-styling?
                                    (leno-css-styled-extra 'slide 'exercise 'caption title (aggregated-page-element-id))
                                    (font-color grey1 (b title)))))
                    "middle")
           (vertical-space 1)))))))


(define (slide-style-index-words . words)
; collect index words and return ""
 (if (not show-and-speak-context)
     (add-words-to-index words lecture-id page-id 'index))
 "")  

; -----------------------------------------------------------------------------

; slide style image series:

(define (slide-style-image-series series-title img-subtitle-list how-to-show-list)
  (let* ((how-to-show-slide (car how-to-show-list))
        (image-file-names (map car img-subtitle-list)))
    (for-each 
      (lambda (fn) 
        (set! note-specific-graphics-files (cons fn note-specific-graphics-files)))
      image-file-names)
    (cond ((eq? how-to-show-slide 'slide-inline) (slide-style-image-series-inline series-title img-subtitle-list))
          ((eq? how-to-show-slide 'slide-external) (slide-style-image-series-external series-title img-subtitle-list))
          (else (error "Slide-style-image-series: Problems in how to show")))))

(define (slide-style-image-series-inline series-title img-subtitle-list)
 (apply string-append
  (map slide-style-image (map car img-subtitle-list))))

(define (slide-style-image-series-external series-title img-subtitle-list)
 (let* ((total-number (length img-subtitle-list))
        (numbers (number-interval 1 total-number))
        (file-name-list (map2 (lambda (dummy sub-number)
                                (image-name lecture-id page-id 'slide image-number sub-number))
                            img-subtitle-list numbers))
        (file-name-list-show (map2 (lambda (dummy sub-number)
                                (image-name lecture-id page-id 'show image-number sub-number))
                            img-subtitle-list numbers))
        (icon-element (a-tag (local-html-url (car file-name-list)) 
                             (img-LN  (image-series-icon-name language-preference 'large) 
                                      (text-choice "Gå til billedserie" "Navigate to image series"))))
        
        (initial-sentence (first-sentence-in-string series-title))
        (this-image-number image-number)
       )
  (set! image-number (+ 1 image-number))
  (map3 (lambda (img-subtitle file-name number)
         (save-slide-image (car img-subtitle) (cadr img-subtitle)
          file-name (slide-url lecture-id page-id)
          (img-relative-url file-name-list (- number 1)) (img-relative-url file-name-list (+ number 1))
          this-image-number number total-number
         )

         (save-slide-image-show-and-speak (car img-subtitle) (cadr img-subtitle)
          file-name (slide-show-and-speak-url lecture-id page-id 1)
          (img-relative-url file-name-list-show (- number 1)) (img-relative-url file-name-list-show (+ number 1)) 
          this-image-number number total-number
         )

        )
       img-subtitle-list file-name-list numbers)
  (if apply-css-styling?
      (table 'class (leno-css-class 'slide 'image-series) 
        (tr (td 'width "110" icon-element)
            (td 'width "900" (leno-css-styled-extra 'slide 'image-series 'caption series-title (aggregated-page-element-id))))) 
      (con (indent-pixels 10 (image-slide-presentation icon-element initial-sentence)) (vertical-space 1))
  )
 ))

(define (image-slide-presentation icon-element series-title)
 (table-3 0 (list 110 900)  
    (list 
     (list icon-element (if apply-css-styling?
                            (leno-css-styled-extra 'slide 'image-series 'caption series-title (aggregated-page-element-id))
                            (font-color grey1 (b series-title))))
    )
    "middle"))

(define (img-relative-url file-name-list n)
  ; return an url made out of element n of file-name-list
  ; if n is outside range, return #f
 (let ((lgt (length file-name-list)))
  (cond ((and (>= n 1) (<= n lgt)) (local-html-url (list-ref file-name-list (- n 1))))
        (else #f))))

(define (save-slide-image img-file img-text dest-file-name context-url prev-url next-url . optional-parameter-list)
 (let* ((number (optional-parameter 1 optional-parameter-list 1))
        (sub-number (optional-parameter 2 optional-parameter-list 1))
        (total-number (optional-parameter 3 optional-parameter-list 1))
       )
  (write-text-file
   (image-slide
      context-url prev-url next-url
      "Image in series"
      img-text img-file
      number sub-number total-number)
   (destination-path dest-file-name))))

(define (image-slide context-url prev-url next-url header title image-file number sub-number . optional-parameter-list)
 (let* ((total-number (optional-parameter 1 optional-parameter-list 1))
        (body  (con (image-header context-url prev-url next-url #f number sub-number "" total-number) (vertical-space 1)
                    (indent-pixels 10 
                      (con (if apply-css-styling?
                               (leno-css-styled-extra 'slide 'image-series 'text title (aggregated-page-element-id))
                               (con (font-size slide-font-size title) (vertical-space 2))
                           )
                           (img-0 (string-append  "." "/" "graphics/" image-file)))))))
    (if java-scripting
      (page-with-keypress-script
        (as-string header) 
        '()
        body

        javascript-loading
        (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
        (js-call "lenoNavigate" 
                 (map string-it-single 
                      (list (if prev-url prev-url "")
                            (if next-url next-url "")
                            context-url
                            ""
                            (if show-and-speak? (html-file (image-name lecture-id page-id 'show number sub-number) ) "")
                            (if show-and-speak? (speak-url (speak-name lecture-id page-id 'image-series-part number sub-number)) "")  ; y 
                            ""
                            )))

        slide-background-color black black black
        mouse-advancement
        (actual-stylesheets apply-css-styling?)
      )
      (page-1
        (as-string header) 
        '()
        body

        slide-background-color black black black 
        (actual-stylesheets apply-css-styling?) ))))

(define (image-header context-url prev-url next-url . optional-parameter-list)
 (let* ((next-in-show (optional-parameter 1 optional-parameter-list #f))
        (number (optional-parameter 2 optional-parameter-list 1))
        (sub-number (optional-parameter 3 optional-parameter-list 1))
        (middle-info (optional-parameter 4 optional-parameter-list ""))
        (total-number (optional-parameter 5 optional-parameter-list 1))
        (sound (speak-name lecture-id page-id 'image-series-part number sub-number))
       )
  (header-banner-3
   (con 
    (if next-in-show ; means show and speak
        (con
          (a-tag (html-file (image-name lecture-id page-id 'slide number sub-number)) 
                 (img-LN "atomspin.gif"  (text-choice "Stop afspilning med lyd" "Stop show with sound"))) 

          (space 2)

          (a-tag 
            next-in-show
            (img-LN "nav-right-red.gif" (key-shortcut (text-choice "Næste slide i dette show" "Next slide in show") "n")))
          (space 2)
        )

        (con 
          (if show-and-speak? 
              (con
                (a-tag (html-file (image-name lecture-id page-id 'show number sub-number) ) 
                                  (img-LN "atomspin-freeze.gif" 
                                          (key-shortcut (text-choice "Afspil indtalte slides" "Play audio slide show") "x")))
                (space 2))
               "")

          (a-tag context-url
                 (img-LN "nav-up.gif" (key-shortcut (text-choice "Gå ét niveau op" "Navigate one level up") "u")))              (space 2)
          (if prev-url
              (a-tag prev-url (img-LN "nav-left.gif" (key-shortcut (text-choice "Gå til forrige billede" "Navigate to previous image") "p")))
              (img-LN "nav-left-empty.gif" ""))                                                     (space 2) 
          (if next-url
              (a-tag next-url (img-LN "nav-right.gif" (key-shortcut (text-choice "Gå til næste billede" "Navigate to next image") "n"))) 
              (img-LN "nav-right-empty.gif" ""))                                                     (space 2)

          (if show-and-speak?
              (if (file-exists-robust? (sound-file-path sound))
                  (a-tag (speak-url sound)
                                    (img-LN "speaker.gif" 
                                           (key-shortcut (text-choice "Afspil lyd på denne slide" "Play sound for this slide") "y")))
                  (img-LN "speaker-empty.gif" ""))
              ""))))

  (em (font-1 2 grey1 middle-info))

  (con 
    (if (and lecture-number (> (length lecture-list) 1))
        (b (con (text-choice "Lektion " "Lecture ") (as-string lecture-number) " - slide "
                (as-string page-number) " : " (as-string (total-number-of-slides))))
        (b (con 
            "Slide " (as-string page-number) " : " (as-string (total-number-of-slides)) )))  (br)
    (b (con (text-choice "Billedserie" "Image series") " -  " (text-choice "billede" "image") " " 
            (as-string sub-number) ":" (as-string total-number))  )
   ) 

   (list)
   )
 )
)


; -----------------------------------------------------------------------------

(define (slide-style-section-title title)
  (if apply-css-styling?
      (leno-css-styled 'slide 'section-title title (aggregated-page-element-id)) 
      (con (vertical-space 5)
           (center (font-1 7 red title)))))


; intended for repetition of an image on a slide:

(define (slide-style-slide-image image-file)
  (con (center (img-0 (string-append  "." "/" "graphics/" image-file))) (vertical-space 1)))


(define (slide-style-applet-program class-file codebase explanation width height . optional-parameter-list)
 (let* ((param-list (optional-parameter 1 optional-parameter-list '())) ; a list of cons pairs with name/value strings.
        (param-rendering (present-applet-parameters param-list))
       )
  (let ((class-file-1 (string-append relative-applet-path class-file)))
    (center (html4:applet param-rendering 'code class-file 'codebase codebase 'height height 'width width)))))


(define (slide-style-synopsis . items)
  (let* ((slide-relevant-items (map (lambda (i) (car (element-contents i))) items))
         (formatted-slide-relevant-items (map (lambda (e) (font-rise e (+ 1 slide-font-size))) (map upcase-string slide-relevant-items)))
        )
  (if apply-css-styling?
      (leno-css-styled 'slide 'synopsis (ul-1 slide-relevant-items) (aggregated-page-element-id))
      (con 
        (indent-pixels 10 (center (color-frame (bullet-list formatted-slide-relevant-items 'large-rised 'yellow) synopsis-frame-color)))
        (vertical-space 1)))
  ))

(define start-quote "\"")
(define end-quote "\"")


(define (slide-style-quotation . x)
 (let* ((quotation (car x))
        (comment (if (> (length x) 1) (cadr x) ""))
        (raw-body 
          (string-append 
           (img-0 (image-file "left-quote.gif"))
           quotation
           (if (>= (string-length quotation) 60) (br) "")
           (img-0 (image-file "right-quote.gif")) ))
       )
  (if apply-css-styling?
      (leno-css-styled 'slide 'quotation raw-body (aggregated-page-element-id)) 
      (con (indent-pixels 10 (narrow-with-pixels 100
               (i (con (img-0 (image-file "left-quote.gif"))
                       (as-slide-text (span 'css:line-height "170%" quotation))
                       (if (>= (string-length quotation) 60) (br) "")
                       (img-0 (image-file "right-quote.gif")) ))))
           (vertical-space 1))
  )
 )
)

(define (slide-style-quiz question answer-list)
  (if (not show-and-speak-context)
      (set! quiz-list 
       (cons
         (make-leno-quiz-entry question answer-list
             (course-absolute-url (string-append (relative-source-html-destination-path-fragment) (note-url lecture-id page-id))))
         quiz-list)))

  "" ; leave no trace of quiz in the slide
)

(define (slide-style-show-and-speak . rest)
  ; We extract this information early and use it imperatively
  "")

; The url of a lecturer photo composed by img-name (including extension) and the constant lecturer-photo-url-prefix.
(define (lecturer-photo-url img-name)
  (string-append lecturer-photo-url-prefix img-name))

; Return an actual list of photos from photo-list, starting at start.
; The returned list will lgt long. Takes photos cyclically from photo-list.
; Start is a number between 0 and (- (length photo-list) 1) that specify where in the photo list to start
(define (actual-photo-list photo-list start lgt)
  (actual-photo-list-1 photo-list start lgt 0 '()))

; start: where in the photo-list to start
; lgt: How many elements to return
; n: the actual length of res
; res: the resulting list  
(define (actual-photo-list-1 photo-list start lgt n res)
  (if (= n lgt)
      (reverse res)
      (actual-photo-list-1 
        photo-list 
        (if (= start (- (length photo-list) 1)) 0 (+ start 1))
        lgt
        (+ n 1)
        (cons (list-ref photo-list start) res))))

; Slide presentation of lecturer photos. 
; First parameter is the start photo - first is number 0.
; The second parameter is the number of photos to show in a row.
(define (slide-style-lecturer-photos . rest)
  (let* ((start-photo (optional-parameter 1 rest))
        (number-of-photos (optional-parameter 2 rest)))
   (if (and show-and-speak? show-and-speak-context)
    (con 
     (vertical-space 1)
     (center
      (table-1 0
               (make-list number-of-photos lecturer-photo-width)
               (make-list number-of-photos slide-background-color)
               (list
                (map 
                 (lambda (image-name) (html4:img 'src (lecturer-photo-url image-name) 'alt "" 'width (- lecturer-photo-width 20)))
                 (actual-photo-list lecturer-photo-list start-photo number-of-photos)
                 )
                )
               "middle"
               )))
     "")))


; Slide presentation of lecturer photos. 
; As a contrast to lecture-photos, this function gives a fixed presentation of two photos with
; the logo in between. The logo is assumed to be part of the lecture specific images.
; First parameter is the start photo - first is number 0. Can be an arbitrary large integer (we normalize it to an appropriate range).
(define (slide-style-lecturer-photos-and-logo . rest)
  (let* ((start-photo-0 (optional-parameter 1 rest))
         (start-photo (remainder start-photo-0 (length lecturer-photo-list)))
         (number-of-photos 3)
         (photo-1-name (list-ref lecturer-photo-list start-photo))
         (photo-2-name (list-ref lecturer-photo-list (if (= start-photo (- (length lecturer-photo-list) 1)) 0 (+ 1 start-photo))))
         (photo-1 (html4:img 'src (lecturer-photo-url photo-1-name) 'alt ""))
         (photo-2 (html4:img 'src (lecturer-photo-url photo-2-name) 'alt ""))
         (logo (logo-img))
         (logo-width 150)
         (space-width 80)
        )
   (if (and show-and-speak?
            show-and-speak-context
       )
    (con 
     (vertical-space 2)
     (center
      (table-1 0
               (list lecturer-photo-width space-width logo-width space-width lecturer-photo-width)
               (make-list (+ number-of-photos 2) slide-background-color)
               (list
                 (list photo-1 "" logo "" photo-2)
               )
               "middle"
               )))
     "")))

(define (slide-style-elucidate . rest)
 (let* ((e-title (first-sentence-in-string (first rest)))
        (e-url (second rest))
        (e-target (if (>= (length rest) 3) (third rest) elucidator-default-target))
        (icon-element (if e-target
                                (a-tag-target 
                                 e-url
                                 (img-LN (elucidator-icon-name language-preference 'large) 
                                         (text-choice "Forklar dette program i separat browser vindue" 
                                                      "Elucidate this program in a separate browser window"))
                                 e-target)
                                (a-tag 
                                 e-url
                                 (img-LN (elucidator-icon-name language-preference 'large) 
                                         (text-choice "Forklar dette program" 
                                                      "Elucidate this program")))))
       )
 (if apply-css-styling?
     (table 'class (leno-css-class 'slide 'elucidate)
            (tr (td 'width "110" icon-element)
                (td 'width "900" 
                    (leno-css-styled-extra 'slide 'elucidate 'caption e-title (aggregated-page-element-id)))))
     (indent-pixels 
      10 
      (con
       (table-3 0 (list 100 900) 
                (list (list icon-element
                            (if apply-css-styling?
                                (leno-css-styled-extra 'slide 'elucidate 'caption e-title (aggregated-page-element-id))
                                (font-color grey1 (b e-title)))))
                "middle")
       (vertical-space 1))))))

(define (slide-style-elucidator-program-fragment . rest)
 (let* ((path (first rest))
        (elucidator-program-fragment-structure (file-read path))
        (context-url (context-url-of-elucidator-program-fragment elucidator-program-fragment-structure))
        (html-fragment (program-fragment-of-elucidator-program-fragment elucidator-program-fragment-structure))

        (how-to-show (second rest))
        (comment (if (>= (length rest) 3) (third rest) ""))
        (initial-sentence (first-sentence-in-string comment))
       )
  (slide-style-source-program-1 path html-fragment how-to-show comment initial-sentence slide-background-color #f "" 'source-program #f)))

; Selectors of elucidator-program-fragment structures:

(define context-url-of-elucidator-program-fragment (make-selector-function 1 "context-url-of-elucidator-program-fragment"))
(define program-fragment-of-elucidator-program-fragment (make-selector-function 2 "program-fragment-of-elucidator-program-fragment"))


(define (slide-style-side-track . rest)
  (let* ((track-title (first rest))
         (track-lecture-id (second rest))  ; either track-lecture-id or track-url is provided. One of them may be #f
         (track-url (third rest))          ; absolute or relative to HTML target dir
         (track-explanation (fourth rest)) ; not used here

         (track-target-url 
           (if track-lecture-id
               (html-file (slide-name track-lecture-id (first-slide-id track-lecture-id)))
               track-url))

         (track-name (side-track-name lecture-id page-id side-track-number 'slide))
         (icon-element (a-tag (html-file track-name)
                                     (img-LN (side-track-icon-name language-preference 'large)
                                             (text-choice "Gå til sidespor" "Go to side track"))))
        )


   ; make underlying track frameset and navigation frame...
   (make-side-track-frame-set lecture-id page-id track-lecture-id side-track-number 'slide 
                              track-target-url
                              (html-file (slide-name lecture-id page-id))
   )

   (set! side-track-number (+ side-track-number 1))


   (if apply-css-styling?
       (table 'class (leno-css-class 'slide 'side-track)
              (tr (td 'width "110" icon-element)
                  (td 'width "900" 
                      (leno-css-styled-extra 'slide 'side-track 'caption track-title (aggregated-page-element-id)))))
       (indent-pixels 
        10 
        (con
         (table-3 0 (list 120 900) 
                  (list (list icon-element
                              (if apply-css-styling?
                                  (leno-css-styled-extra 'slide 'side-track 'caption track-title (aggregated-page-element-id))
                                  (font-color grey1 (b track-title)))))
                  "middle")
         (vertical-space 1))))))


(define (slide-style-svg-image . x)
 (let* ((image-file (car x))
        (width (if (> (length x) 1) (second x) #f))
        (height (if (> (length x) 2) (third x) #f))
        (comment (if (> (length x) 3) (fourth x) ""))
        (svg-clause-id (if (> (length x) 4) (fifth x) ""))
        (svg-inline-ast (if (> (length x) 5) (sixth x) #f))

        (svg-image-file #f)
        (png-image-file #f)
       )

  (if svg-inline-ast
      (let* ((proper-svg-ast (build-svg-ast-from-inline-contribution svg-inline-ast width height))
             (target-proper-file-name (string-append (as-string page-id) "-" svg-clause-id))
             (target-file-path (string-append note-source-directory (relative-source-html-destination-path-fragment)
                                              "graphics/" target-proper-file-name "." "svg"))
            )
        (write-html '(pp prolog) proper-svg-ast target-file-path)
        (set! svg-image-file (string-append target-proper-file-name "." "svg"))
        (set! png-image-file (string-append target-proper-file-name "." "png"))
      )
      (let ; ????
           ((target-proper-file-name (string-append (as-string page-id) "-" svg-clause-id)))
        (set! svg-image-file image-file)
        (set! png-image-file (string-append target-proper-file-name "." "png"))   ; most probably not correct. Is it used?
      )
  )



  (if (and (not show-and-speak-context) (not svg-inline-ast))
      (set! note-specific-graphics-files (cons image-file note-specific-graphics-files)))

  (let ((target-dir (string-append note-source-directory (relative-source-html-destination-path-fragment)))

        (svg-image-contribution-css
	 (object 'width (as-string width) 'height (as-string height)
		 'data (string-append "graphics/" svg-image-file) 'type "image/svg+xml"
		 'class (leno-css-class 'slide 'svg-image) (id-attribute-contribution)
		 please-download-svg-player))
	(svg-image-contribution-non-css 
	 (object 'width (as-string width) 'height (as-string height)
		 'data (string-append  "graphics/" svg-image-file) 'type "image/svg+xml"
		 please-download-svg-player
		 )
	 )

       (png-image-contribution-css (img 'alt "" 'src (string-append  "." "/" "graphics/" png-image-file) 'class (leno-css-class 'slide 'image) (id-attribute-contribution)))
       (png-image-contribution-non-css (img 'alt "" 'src (string-append  "." "/" "graphics/" png-image-file) 'border 0))
      )
    (cond ((eq? treat-svg-as 'svg)
	   (if apply-css-styling? (center svg-image-contribution-css) (con (center svg-image-contribution-non-css) (vertical-space 1))))
	  ((eq? treat-svg-as 'png)
	   (if apply-css-styling? (center png-image-contribution-css) (con (center png-image-contribution-non-css) (vertical-space 1))))
	  ((and (eq? treat-svg-as 'png-if-exist) (file-exists? (string-append target-dir "graphics/" png-image-file)))
	   (if apply-css-styling? (center png-image-contribution-css) (con (center png-image-contribution-non-css) (vertical-space 1))))
	  ((and (eq? treat-svg-as 'png-if-exist) (not (file-exists? (string-append target-dir "graphics/" png-image-file))))
	   (if apply-css-styling? (center svg-image-contribution-css) (con (center svg-image-contribution-non-css) (vertical-space 1))))
          (else (laml-error "slide-style-svg-image: Should not happen"))
    ) )))


; Syntactic convenience around the use of svg-inline and with-xml-language!
(define-syntax svg-here
  (syntax-rules ()
   ((svg-here svg-form)
    (svg-inline
      (with-xml-language! svg-language '(filter)
      svg-form)))))

; Return the numeric value of a m. m is a string, such as "3.7cm", "70px" or "70".
; Return the number 3.7, 70, and 70 resp.
; Assume as a precondition that at least one ciffer is present in m.
; To xml-in-laml lib
(define (number-of-measure m)
 (let ((sp (measure-split-point m))) ; the last character position in the numeric part
   (as-number (substring m 0 (+ 1 sp)))))

; Return the numeric value of a m. m is a string, such as "3.7cm", "70px" or "70".
; Return the string "cm", "px", and "" resp.
; Assume as a precondition that at least one ciffer is present in m.
; To xml-in-laml lib
(define (unit-of-measure m)
 (let ((sp (measure-split-point m))) ; the last character position in the numeric part
   (substring m (+ 1 sp) (string-length m))))

; Return the last character position in the numeric part.
; If no numeric part is present, return -1
(define (measure-split-point m)
 (measure-split-point-help m (string-length m) 0))

(define (measure-split-point-help m m-lgt i)
  (cond ((= i m-lgt) (- m-lgt 1))
        ((memv (string-ref m i) (list #\+ #\- #\. #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
          (measure-split-point-help m m-lgt (+ i 1)))
        (else (- i 1))))
  

(define (build-svg-ast-from-inline-contribution svg-inline-ast width height)
 (let ((the-svg-constituent (first (ast-subtrees svg-inline-ast)))
       (width-no-unit (number-of-measure width))
       (height-no-unit (number-of-measure height))
       (measure-1 (unit-of-measure width))
       (measure-2 (unit-of-measure height))
       (svg-props (list 'xmlns "http://www.w3.org/2000/svg" 'xmlns:xlink "http://www.w3.org/1999/xlink"))
      )
  (if (not (equal? measure-1 measure-2))
      (laml-error "build-svg-ast-from-inline-contribution: Use same measures of width and height." width height))

  (cond ((equal? "svg" (ast-element-name the-svg-constituent))
            the-svg-constituent)
        ((and (equal? "g" (ast-element-name the-svg-constituent)) (or (equal? measure-1 "px") (equal? measure-1 "")))
         (letrec ((box (lambda four-numbers (list-to-string (map as-string four-numbers) " "))))
           (let ((g-subtree the-svg-constituent))
             (svg svg-props 'width (as-string width) 'height (as-string height) 
                  'viewBox (box 0 0 width-no-unit height-no-unit)
                  g-subtree))))
        (else (laml-error "build-svg-ast-from-inline-contribution: NOT SUPPORTED YET:" (ast-element-name the-svg-constituent))))))

(define (generate-svg-file-name)
  (set! page-svg-number (+ page-svg-number 1))
  (string-append (as-string page-id) "-" (as-string page-svg-number)))

(define (slide-style-flash-image . x)
 (let* ((image-file (car x))
        (width (if (> (length x) 1) (second x) #f))
        (height (if (> (length x) 2) (third x) #f))
        (comment (if (> (length x) 3) (fourth x) ""))

        (image-file-path (string-append  "." "/" "graphics/" image-file))
       )
  (if (not show-and-speak-context)
      (set! note-specific-graphics-files (cons image-file note-specific-graphics-files)))

  (let ((image-contribution-css " Flash images not yet supported.")
        (image-contribution-non-css 
          (object 'width (as-string width) 'height (as-string height)
                  'classid "clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"
                  'codebase "http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0"
                  (param 'name "movie" 'value image-file-path)
                  (param 'name "quality" 'value "high")
                  (param 'name "salign" 'value "T")
                  ;bgcolor ?
                  (free-html-element "embed" 
                    'src image-file-path
                    'type "application/x-shockwave-flash" 
                    'pluginspage "http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash")

          )
        )
       )
    (if apply-css-styling? (display-warning "There is no CSS support for flash images"))
    (con (center image-contribution-non-css) (vertical-space 1))  
)))




; Make frameset and the navigation frame of the side track
; target-target-url is the URL of the main portion of the rightmost frame in the frameset.
; back-url is the URL of the page on which the side track is anchored originally: The page with with the track icon.
(define (make-side-track-frame-set from-lecture-id from-page-id target-track-lecture-id side-track-number kind track-target-url back-url)
  (let ((frameset-name (side-track-name from-lecture-id from-page-id side-track-number kind))
        (track-navigation-filename (side-track-navigation-frame-name from-lecture-id from-page-id side-track-number kind))
       )

   ; First make the frameset:
   (write-text-file
    (html4:html
     (con (html4:head (html4:title "Sidetrack"))
	  (html4:frameset 
	   (con (html4:frame 'name "leno:tracknavigation" 'src (html-file track-navigation-filename)
			     'scrolling "no" 'noresize "noresize" 'frameborder "0" ) 
		(html4:frame 'name "leno:trackmain" 'src track-target-url
			     'scrolling "auto" 'frameborder "0" ))
	   'cols "35,*"  
	   )
	  ))
    (destination-path frameset-name))

   ; Next make the track navigation bar
   (let* ((back-name (slide-name from-lecture-id from-page-id)) ; not general enough
          (navigation-body 
	   (center 
	    (a-tag-target 
	     back-url
	     (img-LN "side-track-back.gif" 
		     (text-choice "Tilbage på sporet" "Back on track")) 
	     "_top") ; not always top - will NOT combine with other side tracks or trails.
          )))
     (write-text-file
      (page 
       "track-navigation"
       navigation-body
       slide-background-color black black black)
      (destination-path track-navigation-filename)))

   )
)




; -----------------------------------------------------------------------------
; NOTE-STYLE

(define slide-area-width 470) 
(define slide-note-separation 30)
(define note-area-width 470)  

(define (presentation-slice x y)
  (table-1 0 (list slide-area-width slide-note-separation note-area-width)
             (make-list 3 note-background-color)
    (list (list x "" y))))

(define (program-presentation-slice x y)
  (table-1 0 (list (+ slide-area-width 100) slide-note-separation (- note-area-width 100))
             (list note-background-color (make-color 247 247 247) (make-color 240 240 240))  
    (list (list x "" y))))


; -----------------------------------------------------------------------------

(define (note-style-title . x)
 (let ((title (car x))
       (comment (if (> (length x) 1) (cadr x) "")))
   (presentation-slice 
     (con (font-1 5 title-color (b title)) (p))
     (con (note-text-right comment) (p)))
 ))

(define (note-style-text . x)
 (let ((text (car x))
       (comment (if (> (length x) 1) (cadr x) "")))
   (presentation-slice 
     (con (indent-pixels 10 (note-text-left text)) (p))
     (con (note-text-right comment) (p)))))


(define (note-style-items . i)
 (let ((non-drop-element? (negate drop-element?)))
  (let ((i1 (filter non-drop-element? i)))
   (if align-items-and-annotations
      (con (apply string-append (map note-style-single-item i1)) (p))
      (presentation-slice
       (con (note-text-left (ul-1 (map slide-item-note-style i1))) (p))
       (con 
	(note-text-right 
	 (string-merge
	  (map note-item i1)
	  (make-list (length i1) (p))))
	(p)))))))

(define (note-style-single-item i)
  (presentation-slice
     (con (note-text-left (ul-1 (list (slide-item-note-style i)))))  ; earlier: slide-item. 
                                                                     ; We should avoid to mix slide-style and note-style. OK now.
     (con 
      (note-text-right (note-item i)))))

; A note style variant of slide-item
; We avoid recursive activation of the presetation, via (present-element 'slide-style ...) because it will give
; slide-style CSS presentation attributes. 
(define (slide-item-note-style i)
  (let* ((ei (expand-item i))
         (raw-sub-items (caddr ei)))
    (if (equal? "" (car ei))
        (display-warning "First parameter of an item is empty. Causes problems in some browsers"))
    (if (not (equal? raw-sub-items ""))
        (let ((sub-items (ul-1 (map slide-item-note-style (element-contents raw-sub-items)))))  ; no recursive present-element here!
           (con (car ei) (p) sub-items))
        (con (car ei) (p))
    )))


; (define (note-style-point . x)
;  (let ((point (car x))
;        (comment (if (> (length x) 1) (cadr x) "")))
;   (presentation-slice
;      (con (note-text-left 
;             (center-frame 30 (font-color red (note-text-left point)))
;           )
;           (p)
;      )
;      (con (note-text-right comment) (p)))
; ))

(define (note-style-point . x)
 (let ((point (car x))
       (comment (if (> (length x) 1) (cadr x) "")))
  (presentation-slice
      (if apply-css-styling?
          (center (leno-css-styled 'note 'point point (aggregated-page-element-id)))
          (con (note-text-left 
                (center-frame 30 (font-color red (note-text-left point)))
                )
               (p)
          )
     )
     (con (note-text-right comment) (p)))
))



(define (note-style-program . x)
 (let ((program-text (car x))
       (comment (if (> (length x) 1) (cadr x) "")))
  (presentation-slice
     (con (indent-pixels 10 (note-text-left (font-size 2 (pre program-text)))) (p))
     (con (note-text-right comment) (p)))
))

(define (note-style-source-program . x)
 (let* ((program-file-name (if (absolute-file-path? (first x))
                               (first x) 
                               (string-append note-source-directory (first x))))
        (mark (second x))
        (colorizing (third x))
        (extracted-program-and-doc (program-extract program-file-name mark))
        (program-annotations? (if (> (length x) 9) (as-boolean (tenth x)) #t))
        (program-text 
           (html-protect
             (if extract-and-present-end-of-line-doc-comments
                 (extract-program-source extracted-program-and-doc)
                 extracted-program-and-doc)))
        (documentation-text 
             (if (and extract-and-present-end-of-line-doc-comments program-annotations?)
                 (extract-program-documentation extracted-program-and-doc)
                 #f))
        (documentation-text? (and
                                (turn-into-boolean (substring-index extracted-program-and-doc 0 end-of-line-doc-comment-prefix))
                                (and extract-and-present-end-of-line-doc-comments program-annotations?)))
        (colorized-program-text (colorize-substrings program-text colorizing)) 
        (how-to-show (fourth x))
        (comment (if (> (length x) 3) (fifth x) ""))
        (background-color (if (> (length x) 5) (sixth x) slide-background-color))    
        (initial-sentence (first-sentence-in-string comment))
        (kind (if (> (length x) 8) (as-symbol (nineth x)) 'source-program))
       )
  (note-style-source-program-1 program-file-name colorized-program-text documentation-text
                               how-to-show comment initial-sentence background-color kind documentation-text?)))

; The procedure doing the real work of note-style-source-program. Also used by note-style-elucidator-program-fragment.
(define (note-style-source-program-1 program-file-name colorized-program-text documentation-text how-to-show comment initial-sentence 
                                     background-color kind documentation-text? . optional-parameter-list)
  (let ((context-url (optional-parameter 1 optional-parameter-list #f))
	)
    (let* ((linking-text (text-choice "Link to elucidative program" "Link til elucidativt program"))
	   (linking-anchor (if context-url (a-tag-target context-url (font-color note-foreground-color linking-text) "elucidator") ""))
           (backgr-color-list-css (if background-color (list 'css:background-color background-color) '())) ; not used yet
           (backgr-color (if background-color background-color slide-background-color))
           (this-number program-number)
           (left-sep-item (separate-page-item-before 'program this-number))
           (right-sep-item (separate-page-item-after 'program this-number))
	  )
      (if (eq? (car how-to-show) 'slide-inline)
	  (presentation-slice
	     (con (indent-pixels 15 (color-frame (note-text-left (font-size 2 (pre colorized-program-text))) backgr-color)) (p))
    	     (con (note-text-right (con comment (br) linking-anchor)) (p)))
	  (let ((file-name (program-name lecture-id page-id 'note program-number))) ;@a
            (set! program-number (+ program-number 1)) ;@b
            (save-note-program colorized-program-text documentation-text 
                               file-name 
                               (note-url lecture-id page-id)
                               (if left-sep-item 
                                   (local-html-url (apply separate-note-page-item-name
                                                          (append (list lecture-id page-id) 
                                                                  left-sep-item) ))
                                   #f)
                               (if right-sep-item 
                                   (local-html-url (apply separate-note-page-item-name
                                                          (append (list lecture-id page-id)
                                                                  right-sep-item)))
                                   #f)
                               (local-html-url (program-name lecture-id page-id 'slide this-number))  ; July 4, 2008
                               this-number initial-sentence) ; program saved again,
					; under another name, in another size
            (presentation-slice
             (div 'css:margin-top "3px" 
              (table-3 0 (list 100 390) 
                       (list (list (a-tag (string-append file-name ".html")
                                          (img-LN (program-icon-name language-preference 'small kind) program-file-name))
                                   (con (note-text-left (font-color grey1 (b initial-sentence))) 
                                        (if documentation-text? (con " " (img-LN "red-star.gif" "This program is explained")) ""))
                       ))
                       "middle") (p))
             (con (note-text-right (con (but-first-sentence-of-string comment) (br) linking-anchor)) (p)))
            )))))

(define (note-style-image . x)
 (let ((image-file (car x))
       (comment (if (> (length x) 1) (cadr x) "")))
    (presentation-slice
     (con (center (img-0 (string-append  "." "/" "graphics/small/" image-file))) (p))
     (con (note-text-right comment) (p)))))




;(define (note-style-concepts . x)
;  (apply note-style-items x))

; (define (note-style-concepts . i)
;   (presentation-slice
;      (con (note-text-left (bullet-list (map note-text-left (map slide-item i)) 'large 'yellow)) (p))
;      (con 
;       (note-text-right 
;        (string-merge
;                 (map note-item i)
;                 (make-list (length i) (p))))
;        (p))))


(define (note-style-concepts . i)
  (presentation-slice
     (con (note-text-left 
            (center-frame-color 30 concept-background
             (apply string-append
               (map (lambda (x) (con (p) (font-color white x)))
                    (map note-text-left (map slide-item i)))))) (p))
     (con 
      (note-text-right 
       (string-merge
                (map note-item i)
                (make-list (length i) (p))))
       (p))))



(define (note-style-concept-list . x)
 (let ((real-concept-list (map element-contents x)))
  (presentation-slice
     (con (note-text-left 
            (center-frame-color 30 concept-background
             (apply string-append
               (map (lambda (x) (con (p) (font-color white x)))
                    (map note-text-left (map emphasize-concept-name real-concept-list)))))) (p))
     (con 
      (note-text-right 
       (string-merge
                (map concept-explanation real-concept-list)
                (make-list (length real-concept-list) (p))))
       (p)))))

(define (note-style-example . x)
 (let ((the-example (car x))
       (about-the-example (if (> (length x) 1) (cadr x) "")))
   (presentation-slice 
     (con (indent-pixels 10 (i (note-text-left the-example))) (p))
     (con (note-text-right about-the-example) (p)))
   ))

(define (note-style-opposing . x)
 (presentation-slice
   (table-1 0 (list "*" 40 "*") (make-list 3 slide-background-color)
      (map (lambda (a1-a2) (list (note-text-left (slide-item a1-a2)) "" (note-text-left (note-item a1-a2)))) x))
   ""))

(define (note-style-comment . x)
   (let ((comment (car x)))
   (presentation-slice 
     (con (text-choice "Intern kommentar: " "Internal comment: ")  (i (note-text-left comment)) (p))
     "")))


; Commented out december 17, 2007
;(define (note-style-syntax . x)
; (let* ((syntax-form (car x))
;        (syntax-form-1 (string-append CR syntax-form))
;        (comment (if (> (length x) 1) (cadr x) "")))
;  (presentation-slice
;     (con (indent-pixels 10 (center (color-frame (pre (font-size 2 syntax-form-1)) syntax-frame-color))) (p))
;     (con (note-text-right comment) (p)))
;))

(define (note-style-syntax . x)
 (let* ((syntax-form (car x))
        (syntax-form-1 (string-append CR syntax-form))
        (comment (if (> (length x) 1) (cadr x) "")))
  (presentation-slice
     (con (note-text-left 
             (con 
               (center-frame-color 30 syntax-frame-color
                              (pre (font-size 2 syntax-form-1)))
               (p))))
     (con (note-text-right comment) (p)))
))

(define (note-style-slide-space . x)
   "")

(define (note-style-tabular border column-widths list-of-list . comment)
  (let ((com (if (null? comment) "" (car comment))))
   (presentation-slice
    (con
     (table-1 border
             column-widths
             (make-list (length column-widths) tabular-note-color)
             (map (lambda (row) (map (lambda (el) (font-size (- note-font-size-left 1) el)) row)) list-of-list)) (p))
    (con (note-text-right com) (p)))))

(define (note-style-note-text nt)
  (let ((text "")
        (comment nt))
   (presentation-slice 
     text
     (con (note-text-right comment) (p)))))

(define (note-style-cross-references list-of-cross-refs)
 (let* ((hv (cond ((= (length list-of-cross-refs) 1) (text-choice "Henvisning" "Reference"))
                  ((> (length list-of-cross-refs) 1) (text-choice "Henvisninger" "References"))
                  ((< (length list-of-cross-refs) 1) (text-choice "Ingen henvisninger" "No references"))))
        (formatter-function (generate-cross-reference-formatter note-url))
        (filtered-refs list-of-cross-refs)  ; before: (filter no-informal-references list-of-cross-refs )
        (formatted-refs-0 (map formatter-function filtered-refs))
        (formatted-refs (glue-ref-and-hint formatted-refs-0))
        (formatted-ref-string (ul-1 (map (lambda (r) (font-size 1 r)) formatted-refs)))
       )
 (presentation-slice 
   (if (> (length filtered-refs) 0)
    (con
      ; cross references at right hand side of slide:
      (table-3 0 (list 300 180)
        (list (list "" formatted-ref-string)))
     (vertical-space 1))
    "")
   (p) )
  )
) 


(define (note-style-exercise id-symbol title formulation-clause . optional-parameter-list)
 (let ((solution-clause-1 (optional-parameter 1 optional-parameter-list #f))
       (rank (optional-parameter 2 optional-parameter-list #f))
      ) 
  (let* ((formulation-text (car (element-contents formulation-clause)))
         (solution-text (if solution-clause-1 (car (element-contents solution-clause-1)) ""))
         (formulation-file-name (exercise-name lecture-id page-id 'note exercise-number))
         (formulation-link-file-name ((if exercise-manager-support? exercise-frame-name  exercise-name) lecture-id page-id 'note exercise-number))
         (solution-file-name (exercise-solution-name lecture-id page-id exercise-number))
         (secret-solution-file-name (exercise-secret-solution-name lecture-id page-id exercise-number))
         (solution-url (if solution-clause-1 (exercise-solution-url lecture-id page-id exercise-number) ""))
         (left-sep-item (separate-page-item-before 'exercise exercise-number))
         (right-sep-item (separate-page-item-after 'exercise exercise-number))
        )

    (set! current-exercise-id id-symbol)

    (if exercise-manager-support?
        (save-note-frame-exercise-file lecture-id page-id id-symbol title formulation-file-name solution-url))

    (if distance-education-support?  ; idafus support
        (make-idafus-activity! id-symbol title formulation-text solution-text))

    (save-note-exercise title formulation-text formulation-file-name (note-url lecture-id page-id)
                        solution-url
                        (if left-sep-item 
                            (local-html-url (apply separate-note-page-item-name
                                                   (append (list lecture-id page-id) 
                                                           left-sep-item) ))
                            #f)
                        (if right-sep-item 
                                 (local-html-url (apply separate-note-page-item-name
                                                          (append (list lecture-id page-id)
                                                          right-sep-item)))
                                 #f)
                        next-exercise-number rank)

    ; always save solution file. Hard to find because of password ingredient in name. 
    ; Links are only made to this file in case we want to reveal solutions.
    (save-exercise-solution title solution-text solution-file-name (note-url lecture-id page-id)
                                next-exercise-number)

    ; always store the 'secret' solutions
    (save-exercise-solution title solution-text secret-solution-file-name (note-url lecture-id page-id)     
                                next-exercise-number) 

    (set! exercise-number (+ 1 exercise-number))

    ; (set! next-exercise-number (+ 1 next-exercise-number))    ; Commented out Sept 16, 2008
    (presentation-slice
       (table-3 0 (list 100 390) 
                  (list (list (a-tag (string-append formulation-link-file-name ".html")
                                     (img-LN (exercise-icon-name language-preference 'small) (text-choice "Gå til opgave" "Go to exercise")))
                              (font-1 note-font-size-left grey1 (b title))))
                  "middle")
       "")
)))

(define (note-style-image-series series-title img-subtitle-list how-to-show-list)
  (let* ((initial-sentence (first-sentence-in-string series-title))
         (total-number (length img-subtitle-list))
         (numbers (number-interval 1 total-number))
         (file-name-list (map2 (lambda (dummy sub-number)
                                (image-name lecture-id page-id 'slide image-number sub-number))
                            img-subtitle-list numbers))
         (im-ser-url (local-html-url (car file-name-list)))
         (im-ser-target #f)

        )
   (set! image-number (+ 1 image-number))
   (presentation-slice
       (table-3 0 (list 100 390) 
                  (list (list (if im-ser-target 
                                  (a-tag-target 
                                     im-ser-url
                                     (img-LN (image-series-icon-name language-preference 'small) 
                                         (text-choice "Gå til billedserie" "Go to image series"))
                                     e-target)
                                  (a-tag 
                                     im-ser-url
                                     (img-LN (image-series-icon-name language-preference 'small) 
                                         (text-choice "Gå til billedserie" "Go to image series")))
                              )
                              
                              (font-1 note-font-size-left grey1 (b initial-sentence))))
                  "middle")
        ""
       )
;   (presentation-slice
;     (con (font-1 note-font-size-left grey1 (b (con initial-sentence (text-choice " (se slide)" " (see slide)")) )) (p))
;     "")
))

(define (note-style-section-title title)
 (presentation-slice
  (con (vertical-space 2)
       (center (font-1 5 red title)))
   ""))


(define (note-style-slide-image image-file)
 (presentation-slice
     (con (center (img-0 (string-append  "." "/" "graphics/small/" image-file))) (p))
     (con (note-text-right "") (p))))


(define (note-style-applet-program class-file codebase explanation width height . optional-parameter-list)
 (let* ((param-list (optional-parameter 1 optional-parameter-list '())) ; a list of cons pairs with name/value strings.
        (param-rendering (present-applet-parameters param-list))
      )
  (presentation-slice
     (con (center (html4:applet param-rendering 'code class-file 'codebase codebase 'height height 'width width)) (p))
     (con (note-text-right explanation) (p)))))


(define (note-style-synopsis . items)
  (let* ((slide-relevant-items (map (lambda (i) (car (element-contents i))) items))
         (explanation-relevant-items  (map extract-synopsis-explanation items))
         (explanation-relevant-items-1 (filter (lambda(e) (not (equal? "" e))) explanation-relevant-items))
         (formatted-slide-relevant-items (map (lambda (e) (font-rise e (+ 1 note-font-size-left))) (map upcase-string slide-relevant-items)))
        )
   (presentation-slice
     (con 
       (indent-pixels 10  (center (color-frame (bullet-list formatted-slide-relevant-items 'small 'yellow) synopsis-frame-color)))
       (p)
     )
     (con 
      (note-text-right 
       (string-merge
                explanation-relevant-items-1
                (make-list (length explanation-relevant-items-1) (p))))
       (p)))))

; extract the explanation component "y" of a synopsis item:  (synopsis-item "x" "y")
; in case the list is of lenght 2, return the empty string.
(define (extract-synopsis-explanation i)
 (let ((contents (element-contents i)))
   (if (>= (length contents) 2)
       (cadr contents)
       "")))


(define (note-style-quotation . x)
 (let ((quotation (car x))
       (comment (if (> (length x) 1) (cadr x) ""))
      )
 (presentation-slice
  (con (indent-pixels 10 (narrow-with-pixels 50
           (i (string-append (font-rise start-quote note-font-size-left) (note-text-left quotation) (font-rise end-quote note-font-size-left)))))
       (p))
  (con (note-text-right comment) (p)))))

(define (note-style-elucidate . rest)
 (let ((e-title (first-sentence-in-string (first rest)))
       (e-full-expl (first rest))
       (e-url (second rest))
       (e-target (if (>= (length rest) 3) (third rest) elucidator-default-target))
      )
    (presentation-slice
       (table-3 0 (list 100 390) 
                  (list (list (if e-target 
                                  (a-tag-target 
                                     e-url
                                     (img-LN (elucidator-icon-name language-preference 'small) 
                                         (text-choice "Gå til elucidator" "Go to elucidator"))
                                     e-target)
                                  (a-tag 
                                     e-url
                                     (img-LN (elucidator-icon-name language-preference 'small) 
                                         (text-choice "Gå til elucidator" "Go to elucidator")))
                              )
                              
                              (font-1 note-font-size-left grey1 (b e-title))))
                  "middle")
       (note-text-right e-full-expl))))

(define (note-style-elucidator-program-fragment . rest)
 (let* ((path (first rest))
        (elucidator-program-fragment-structure (file-read path))
        (context-url (context-url-of-elucidator-program-fragment elucidator-program-fragment-structure))
        (html-fragment (program-fragment-of-elucidator-program-fragment elucidator-program-fragment-structure))

        (how-to-show (second rest))
        (comment (if (>= (length rest) 3) (third rest) ""))
        (initial-sentence (first-sentence-in-string comment))
       )
  (note-style-source-program-1 path html-fragment "" how-to-show comment initial-sentence slide-background-color context-url) ; misses par?
 ))


(define (note-style-side-track . rest)
  (let* ((track-title (first rest))
         (track-lecture-id (second rest))
         (track-url (third rest))          ; absolute or relative to HTML target dir
         (track-explanation (fourth rest))

         (track-target-url 
           (if track-lecture-id
               (html-file (note-name track-lecture-id (first-slide-id track-lecture-id)))
               track-url))

         (track-name (side-track-name lecture-id page-id side-track-number 'note))
        )

   ;make underlying track frameset...
   (make-side-track-frame-set lecture-id page-id track-lecture-id side-track-number 'note 
                              track-target-url
                              (html-file (note-name lecture-id page-id))
   )

   (set! side-track-number (+ side-track-number 1))

   (presentation-slice
       (table-3 0 (list 100 390) 
                  (list (list (a-tag (html-file track-name)
                                 (img-LN (side-track-icon-name language-preference 'small)
                                            (text-choice "Gå til sidespor" "Go to side track")))
                              (font-1 note-font-size-left grey1 (b track-title))))
                  "middle")
       (note-text-right track-explanation))))


(define (note-style-svg-image . x)
 (let* ((image-file (car x))
        (width (if (> (length x) 1) (second x) #f))
        (height (if (> (length x) 2) (third x) #f))
        (reduced-width (string-append (as-string (* (number-of-measure width) 0.4)) (unit-of-measure width)))
        (reduced-height (string-append (as-string (* (number-of-measure height) 0.4)) (unit-of-measure height)))
        (comment (if (> (length x) 3) (fourth x) ""))
        (svg-clause-id (if (> (length x) 4) (fifth x) ""))
        (svg-inline-ast (if (> (length x) 5) (sixth x) #f))

        (svg-image-file #f)
        (png-image-file #f)
       )
  (if svg-inline-ast
      (let* ((proper-svg-ast (build-svg-ast-from-inline-contribution svg-inline-ast width height))
             (target-proper-file-name (string-append (as-string page-id) "-" svg-clause-id))
	     )
        (set! image-file (string-append target-proper-file-name "." "svg"))
        (set! svg-image-file (string-append target-proper-file-name "." "svg"))
        (set! png-image-file (string-append target-proper-file-name "." "png"))
	)
   (let*  ; ????
         ((target-proper-file-name (string-append (as-string page-id) "-" svg-clause-id)))
        (set! svg-image-file image-file)
        (set! png-image-file (string-append target-proper-file-name "." "png"))
	)  
  )

  (let ((target-dir (string-append note-source-directory (relative-source-html-destination-path-fragment)))

	(svg-image-contribution-non-css 
	 (object 'width (as-string reduced-width) 'height (as-string reduced-height)
		 'data (string-append  "graphics/" svg-image-file) 'type "image/svg+xml"
		 please-download-svg-player
		 )
	 )

       (png-image-contribution-non-css (img 'alt "" 'src (string-append  "." "/" "graphics/small/" png-image-file) 'border 0))
      )
   (presentation-slice
    (cond ((eq? treat-svg-as 'svg)
	   (con (center svg-image-contribution-non-css) (p)))
	  ((eq? treat-svg-as 'png)
	   (con (center png-image-contribution-non-css) (p)))
	  ((and (eq? treat-svg-as 'png-if-exist) (file-exists? (string-append target-dir "graphics/small/" png-image-file)))
	   (con (center png-image-contribution-non-css) (p)))
	  ((and (eq? treat-svg-as 'png-if-exist) (not (file-exists? (string-append target-dir "graphics/small/" png-image-file))))
	   (con (center svg-image-contribution-non-css) (p)))
    )
    (con (note-text-right comment) (p)))) ))


(define (note-style-flash-image . x)
 (let* ((image-file (car x))
        (width (if (> (length x) 1) (second x) #f))
        (height (if (> (length x) 2) (third x) #f))
        (comment (if (> (length x) 3) (fourth x) ""))

        (reduced-width (as-number (* (as-number width) 0.6)))
        (reduced-height (as-number (* (as-number height) 0.6)))

        (image-file-path (string-append  "." "/" "graphics/" image-file))
       )
  (presentation-slice
     (con (center 
             (let (
                   (image-contribution
		    (object 'width (as-string reduced-width) 'height (as-string reduced-height)
			    'classid "clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"
			    'codebase "http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0"
			    (param 'name "movie" 'value image-file-path)
			    (param 'name "quality" 'value "high")
			    (param 'name "salign" 'value "T")
					;bgcolor ?
			    (free-html-element "embed" 
					       'src image-file-path
					       'type "application/x-shockwave-flash" 
					       'pluginspage "http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash")
			    )
		    )
		   )
	       image-contribution
	       )
           ))
     (con (note-text-right comment) (p)))

  )
)

(define (note-style-meta-text . x)
 (let* ((text (first x))
        (type (second x))
        (icon (if (equal? (as-string type) "readers-guide") 
                  (img-LN (readers-guide-icon-name language-preference)
                          "")
                  ""))
        (icon-contribution 
          (if (empty-string? icon) "" (con icon (br))))
       )
   (presentation-slice 
     ""
     (con icon-contribution  (note-text-right (em text)) (p)))))






; -----------------------------------------------------------------------------      

; BOOK-STYLE
; indfør kald af book-text i stedet for font

(define (book-style-title . x)
 (let ((title (car x))
       (body (if (> (length x) 1) (cadr x) ""))
       (annotator-url
          (if annotator-support?
              (annotator-url "annotator.cgi"
                  (list 'annotation-id 'detail-level 'activation-mode 'language 'informative-title 'back-url)
                  (list (encode-annotation-ids (list (as-string annotator-symbol) (as-string lecture-id) (as-string page-id)))
                        "flat" "present-only" language-preference page-title 
                        (course-absolute-url (string-append "html/noter/" (html-file (note-name lecture-id page-id)))))
               )
              ""))
       )
   (con 
    (a-name page-id)
    (table-1 0 '(180 12 500) (make-list 3 book-background-color) 
      (list (list (con (font-1 4 book-title-foreground-color title) (br)
                       (font-size 1 
                         (con (on-condition make-slide-view? (a-tag (slide-url lecture-id page-id) "Slide")) 
                              (on-condition make-slide-view? (space 1))
                              (on-condition make-annotated-slide-view? (a-tag (note-url lecture-id page-id) 
                                                                              (text-choice "Annoteret slide" "Annotated slide")))
                              (on-condition make-annotated-slide-view? (space 1))
                              (a-tag (contents-url lecture-id) (text-choice "Indhold" "Contents") ) (space 1)
                              (a-tag (all-word-index-url lecture-id) (text-choice "Stikord" "Index")) (br)
                              (a-tag "#REFERENCES" (text-choice "Referencer" "References")) (space 1)
                              (let ((sound (speak-name lecture-id page-id 'slide-part 1)))
                               (if show-and-speak?
                                   (if (file-exists-robust? (sound-file-path sound))
                                       (a-tag (speak-url sound)
                                              "Speak")
                                       ""
                                       )
                                   ""))
                              (if themes?
				  (let ((theme-url corresponding-theme-url)) ; global variable - assigned in original-note-page.
				    (if theme-url
					(con (a-tag theme-url (text-choice "Lærebog" "Textbook")) (space 1))
					""))
				  "")
                              (if annotator-support? (a-tag annotator-url (text-choice "Kommenter" "Comment")) "")
                         )))
                  ""
                  (font-1 3 black body))))
    (p))))


(define (book-style-text . x)
 (let ((text (car x))
       (comment (if (> (length x) 1) (cadr x) "")))
   (con
    (table-1 0 '(180 12 500)  (make-list 3 book-background-color) 
      (list (list "" "" (con (font-1 3 black text) (br)
                             (font-1 3 black comment)))))
    (p)) ))


(define (book-style-items . i)
 (let ((non-drop-element? (negate drop-element?)))
  (let ((i1 (filter non-drop-element? i)))
   (con
    (table-1 0 '(180 12 500)  (make-list 3 book-background-color) 
	     (list (list "" "" (con (frame-width (ul-1 (map slide-item i1)) 490) (p)
				    (string-merge
				     (map note-item i1)
				     (make-list (length i1) (p))
				     )))))
    (p)))))


(define (book-style-point . x)
 (let ((point (car x))
       (body (if (> (length x) 1) (cadr x) "")))
   (con
    (table-1 0 '(180 12 500)  (make-list 3 book-background-color) 
      (list (list ""
                  ""
                  (con (frame-width (font-1 3 red point) 490) (p) (font-1 3 black body)))))
    (p))))

(define (book-style-program . x)
 (let ((program-text (car x))
       (comment (if (> (length x) 1) (cadr x) "")))
   (con
    (table-1 0 '(180 12 500)  (make-list 3 book-background-color) 
      (list (list (font-1 2 book-header-foreground-color
                          (i (con (u "Program") ": " comment)))
                  ""
                  (con (font-1 3 black (pre program-text))))))
    (p))))

(define (book-style-source-program . x)
  (let* (
         (program-file-name (if (absolute-file-path? (first x))
                               (first x) 
                               (string-append note-source-directory (first x))))
	 (mark (second x))
	 (colorizing (third x))
         (program-text 
           (html-protect
             (if extract-and-present-end-of-line-doc-comments
                 (extract-program-source (program-extract program-file-name mark))
                 (program-extract program-file-name mark))))
	 (colorized-program-text (colorize-substrings program-text colorizing))
	 (how-to-show (fourth x))
	 (comment (if (> (length x) 3) (fifth x) ""))
         (background-color (if (> (length x) 5) (sixth x) slide-background-color))    
        )
    (book-style-source-program-1 program-file-name colorized-program-text how-to-show comment "" background-color)))

; The procedure doing the real work of book-style-source-program. Also used by book-style-elucidator-program-fragment.
(define (book-style-source-program-1 program-file-name colorized-program-text how-to-show comment initial-sentence 
                                     background-color . optional-parameter-list)
  (let ((context-url (optional-parameter 1 optional-parameter-list #f))
	)
    (let* ((linking-text (text-choice "Link to elucidative program" "Link til elucidativt program"))
	   (linking-anchor (if context-url (a-tag-target context-url linking-text "elucidator") ""))
           (backgr-color-list-css (if background-color (list 'css:background-color background-color) '())) ; not used yet
           (backgr-color (if background-color background-color slide-background-color))
	  )
      (if (eq? (cadr how-to-show) 'book-inline)
	  (con
	   (table-1 0 '(180 12 500)  (make-list 3 book-background-color) 
		    (list (list (font-1 2 book-header-foreground-color
					(i (con (u "Program") ": " (con comment (br) linking-anchor))))
				""
				(con (color-frame (font-1 2 black (pre colorized-program-text)) backgr-color)))))
	   (p))
	(let ((file-name (program-name lecture-id page-id 'book program-number)))
	  (set! program-number (+ program-number 1))
	  (save-book-program colorized-program-text file-name (book-url lecture-id page-id))
	  (con
	   (table-1 0 '(180 12 500)  (make-list 3 book-background-color) 
		    (list (list (font-1 2 book-header-foreground-color
					(i (con (u "Program") ": " (con comment (br) linking-anchor))))
				""
				(con (font-1 3 black (a-tag (string-append file-name ".html") 
							    (img-LN (program-icon-name language-preference 'small) 
								    program-file-name)))))))
	   (p))
	  )))))


(define (book-style-image . x)
 (let ((image-file (car x))
       (comment (if (> (length x) 1) (cadr x) "")))

   (con
    (table-1 0 '(180 12 500)  (make-list 3 book-background-color) 
      (list (list (i (font-1 2 book-header-foreground-color (con (u (text-choice "Figur" "Figure")) ". " comment)))
                  ""
                  (center (img-0 (string-append  "." "/" "graphics/small/" image-file))))))
    (p))))




(define (book-style-concepts . i)
  (con
    (table-1 0 '(180 12 500) (make-list 3 book-background-color)
      (map (lambda (e) (list (font-1 2 book-header-foreground-color (b (con (u (text-choice "Begreb" "Concept")) ": " (slide-item e))))
                             ""
                             (note-item e))) i)
    )
    (p)))


(define (book-style-concept-list . i)
 (let ((real-concept-list (map element-contents i)))
  (con
    (table-1 0 '(180 12 500) (make-list 3 book-background-color)
      (map (lambda (e) (list (font-1 2 book-header-foreground-color (b (con (u (con-space (text-choice "Begrebet" "The concept")
                                                                                        (concept-name e))) ": " (concept-description e))))
                             ""
                             (concept-explanation e)))
            real-concept-list)
    )
    (p))))


; (define (book-style-example . x)
;  (let ((the-example (car x))
;        (about-the-example (if (> (length x) 1) (cadr x) "")))
;    (con
;     (table-1 0 '(180 12 500)  (make-list 3 book-background-color) 
;       (list (list (font-1 2 book-header-foreground-color (con (u "Eksempel") ". " the-example))
;                   ""
;                   (con (font-1 3 black about-the-example)))))
;     (p))))

(define (book-style-example . x)
 (let ((the-example (car x))
       (about-the-example (if (> (length x) 1) (cadr x) "")))
   (con
     (book-element
       (font-1 2 book-header-foreground-color (con (u "Eksempel") ". " the-example))
       (font-1 3 black about-the-example))
     (vertical-space 1))))


(define (book-style-opposing . x)
  (con
   (table-1 0 '(180 12 500)  (make-list 3 book-background-color)
          (list  
            (list  "" 
                   ""
                   (table-1 0 (list 200 40 200) (make-list 3 book-background-color)
                            (map (lambda (a1-a2) (list (book-text (slide-item a1-a2)) "" (book-text (note-item a1-a2)))) x)))))
   (p)))

(define (book-style-comment . x)
 (let ((comment (car x)))
   (con
    (table-1 0 '(180 12 500)  (make-list 3 book-background-color) 
      (list (list (i (b (text-choice "Intern kommentar: " "Internal comment")))
                  ""
                  (con  (i (note-text-left comment)) (vertical-space 1)))))
    (p))))


(define (book-style-syntax . x)
 (let* ((syntax-form (car x))
        (syntax-form-1 (string-append CR syntax-form))
        (comment (if (> (length x) 1) (cadr x) "")))
   (con
    (table-1 0 '(180 12 500)  (make-list 3 book-background-color) 
      (list (list (font-1 2 book-header-foreground-color
                          (i (con (u "Syntax") ": " comment)))
                  ""
                  (con (color-frame (pre (font-size 2 syntax-form-1)) syntax-frame-color)))))
    (p))))


(define (book-style-tabular border column-widths list-of-list . comment)
  (let ((com (if (null? comment) "" (car comment))))
   (con
    (table-1 0 '(180 12 500)  (make-list 3 book-background-color) 
      (list (list (font-size 2 (i (con (u (text-choice "Tabel" "Table")) ". " com)))
                  ""
                  (con  
                      (table-1 border
                               column-widths
                               (make-list (length column-widths) tabular-book-color)
                               (map (lambda (row) (map (lambda (el) (font-size (- book-font-size 1) el)) row)) list-of-list))

                      (vertical-space 1)))))
    (p))))


(define (book-style-note-text nt)
  (let ((text "")
        (comment nt))
   (con
    (table-1 0 '(180 12 500)  (make-list 3 book-background-color) 
      (list (list "" "" (font-1 3 black comment))))
    (p))))



(define (book-style-cross-references list-of-cross-refs)
 (let* ((hv (cond ((= (length list-of-cross-refs) 1) (text-choice "Henvisning" "Reference"))
                  ((> (length list-of-cross-refs) 1) (text-choice "Henvisninger" "References") )
                  ((< (length list-of-cross-refs) 1) (text-choice "Ingen henvisninger" "No references"))))
        (formatter-function (generate-cross-reference-formatter book-url))
        (formatted-refs-0 (map formatter-function list-of-cross-refs))
        (formatted-refs (glue-ref-and-hint formatted-refs-0))
        (formatted-ref-string (ul-1 (map (lambda (r) (font-size 2 r)) formatted-refs)))
       )
  (con
    (table-1 0 '(180 12 500)  (make-list 3 book-background-color) 
      (list (list (color-frame-width (con (u (i (font-size 2 hv))) formatted-ref-string) grey2 180)
                  ""
                  "")))
    (p))))


(define (book-style-exercise id-symbol title formulation-clause . optional-parameter-list)
 (let ((solution-clause-1 (optional-parameter 1 optional-parameter-list #f))
       (rank (optional-parameter 2 optional-parameter-list #f))
      ) 
  (let* ((formulation-text (car (element-contents formulation-clause)))
         (solution-text (if solution-clause-1 (car (element-contents solution-clause-1)) "")))
    (set! exercise-number (+ 1 exercise-number))
    (let* ((num next-exercise-number)
           (hier-num (if (and lecture-number (number? lecture-number))
                         (con (as-string lecture-number) "." (as-string num))
                       (as-string num))))
;      (set! next-exercise-number (+ 1 next-exercise-number))  ; assumed to be incremented in slide-style-exercise
      ; solution only saved in note-style-exercise
      (con
       (table-1 0 '(180 12 500)  (make-list 3 book-background-color) 
                (list (list (font-size 2 (con (u (con (text-choice "Opgave" "Exercise") " " (as-string hier-num))) ". " title))
                            "" 
                            (font-1 3 black formulation-text))))
       (p))))))



(define (book-style-image-series series-title img-subtitle-list how-to-show-list)
  (let ((how-to-show-slide (cadr how-to-show-list)))
    (cond ((eq? how-to-show-slide 'book-inline) (book-style-image-series-inline series-title img-subtitle-list))
          ((eq? how-to-show-slide 'book-external) (book-style-image-series-external series-title img-subtitle-list))
          (else (error "Book-style-image-series: Problems in how to show")))))

(define (book-style-image-series-inline series-title img-subtitle-list)
 (let ((initial-sentence (first-sentence-in-string series-title)))
  (set! image-number (+ 1 image-number))
  (con
   (book-element (i (font-1 2 book-header-foreground-color (con (u (text-choice "Billedserie:" "Image series:")) " " initial-sentence)))
                 (con (b (font-1 3 black series-title)) (p)))
   (apply string-append
    (map3 present-book-style-image 
          (map car img-subtitle-list)
          (map cadr img-subtitle-list)
          (number-interval 1 (length img-subtitle-list)) )))))

(define (book-style-image-series-external series-title img-subtitle-list)
 ; in essence, just link to the slide image series
 (let* ((numbers (number-interval 1 (length img-subtitle-list)))
        (file-name-list (map2 (lambda (dummy sub-number)
                                (image-name lecture-id page-id 'book image-number sub-number))
                            img-subtitle-list numbers))
        (icon-element (a-tag (local-html-url (car file-name-list)) 
                                             (img-LN  (image-series-icon-name language-preference 'small) 
                                                     (text-choice "Gå til billedserie" "Navigate to image series"))))
       )
  (set! image-number (+ 1 image-number))
  (map3 (lambda (img-subtitle file-name number)
         (save-slide-image (car img-subtitle) (cadr img-subtitle)
          file-name (book-url lecture-id page-id)
          (img-relative-url file-name-list (- number 1)) (img-relative-url file-name-list (+ number 1)) 
           
          )
         )
       img-subtitle-list file-name-list numbers)
  (con 
   (book-element
     (i (font-1 2 book-header-foreground-color (con (u (text-choice "Billedserie:" "Image series:")) " " series-title))) icon-element)
   (p))))

                 

(define (present-book-style-image image-file subtitle number)
  (con
   (book-element
     (i (font-1 2 book-header-foreground-color (con (u (con (text-choice "Billed nr. " "Image no. ") (as-string number))) ". " subtitle)))
     (img-0 (string-append  "." "/" "graphics/small/" image-file)))
   (vertical-space 1))
)


(define (book-style-section-title title)
  (con  (hr-1)
    (a-name page-id)
    (book-element (font-1 4 red title)
                  "")
        (hr-1)  ))

(define (book-style-applet-program class-file codebase explanation width height . optional-parameter-list)
 (let* ((param-list (optional-parameter 1 optional-parameter-list '())) ; a list of cons pairs with name/value strings.
        (param-rendering (present-applet-parameters param-list))
      )
 (con
   (book-element
     (i (font-1 2 book-header-foreground-color (con (u "Applet") ". " explanation)))
     (center (html4:applet param-rendering 'code class-file 'codebase codebase 'height height 'width width)))
   (vertical-space 1))))

(define (book-style-synopsis . items)
 (let* ((slide-relevant-items (map (lambda (i) (car (element-contents i))) items))
        (explanation-relevant-items  (map extract-synopsis-explanation items))
        (explanation-relevant-items-1 (filter (lambda(e) (not (equal? "" e))) explanation-relevant-items))
        (formatted-slide-relevant-items (map book-text slide-relevant-items))
       )
 (con
   (book-element
     ""
     (con (color-frame-width (bullet-list formatted-slide-relevant-items 'small 'yellow) synopsis-frame-color 490))
   ) 
   (p)
   (book-element
     ""  
     (font-1 book-font-size book-foreground-color
       (string-merge
          explanation-relevant-items-1
          (make-list (length explanation-relevant-items-1) (p)))))
   (vertical-space 1))))

; should be used generally:


(define (book-style-quotation . x)
 (let ((quotation (car x))
       (comment (if (> (length x) 1) (cadr x) ""))
      )
 (book-element
  ""
  (con (center 
         (narrow-with-pixels 18
           (i (string-append (font-rise start-quote book-font-size) (note-text-left quotation) (font-rise end-quote book-font-size)))))
       (p) 
       comment (p)))))

(define (book-style-elucidate . rest)
 (let ((e-title (first-sentence-in-string (first rest)))
       (e-full-expl (first rest))
       (e-url (second rest))
       (e-target (if (>= (length rest) 3) (third rest) elucidator-default-target))
      )
    (book-element
       e-full-expl
       (table-3 0 (list 100 390) 
                  (list (list (if e-target
                                  (a-tag-target e-url
                                    (img-LN (elucidator-icon-name language-preference 'small) 
                                         (text-choice "Gå til elucidator" "Go to elucidator"))
                                    e-target
                                  )
                                  (a-tag e-url
                                    (img-LN (elucidator-icon-name language-preference 'small) 
                                         (text-choice "Gå til elucidator" "Go to elucidator")))
                              )
                              (font-1 note-font-size-left grey1 (b e-title))))
                  "middle"))))

(define (book-style-elucidator-program-fragment . rest)
 (let* ((path (first rest))
        (elucidator-program-fragment-structure (file-read path))
        (context-url (context-url-of-elucidator-program-fragment elucidator-program-fragment-structure))
        (html-fragment (program-fragment-of-elucidator-program-fragment elucidator-program-fragment-structure))

        (how-to-show (second rest))
        (comment (if (>= (length rest) 3) (third rest) ""))
        (initial-sentence (first-sentence-in-string comment))
       )
  (book-style-source-program-1 path html-fragment how-to-show comment initial-sentence book-background-color context-url)))



(define (book-element left-contribution right-contribution)
 ; return a horizontal slize of a book style contribution
 (table-1 0 '(180 12 500)  (make-list 3 book-background-color) 
      (list (list left-contribution
                  ""
                  right-contribution))))

; A variant of book-element with an right-contribution column of unlimited width
(define (book-element-unlimited left-contribution right-contribution)
 ; return a horizontal slize of a book style contribution
 (table-1 0 '(180 12 *)  (make-list 3 book-background-color) 
      (list (list left-contribution
                  ""
                  right-contribution))))


(define (book-style-side-track . rest)
  (let* ((track-title (first rest))
         (track-lecture-id (second rest))
         (track-url (third rest))          ; absolute or relative to HTML target dir
         (track-explanation (fourth rest))

         (track-target-url 
           (if track-lecture-id
               (html-file (book-name track-lecture-id))
               track-url))

         (track-name (side-track-name lecture-id page-id side-track-number 'book))
        )

   ;make underlying track frameset...
   (make-side-track-frame-set lecture-id page-id track-lecture-id side-track-number 'book
                              track-target-url
                              (string-append (book-name lecture-id)  ".html" "#" (as-string page-id))
   )

   (set! side-track-number (+ side-track-number 1))

    (book-element
       (font-1 2 book-header-foreground-color (em track-explanation))
       (table-3 0 (list 100 390) 
                  (list (list (a-tag (html-file track-name)
                                 (img-LN (side-track-icon-name language-preference 'small)
                                            (text-choice "Gå til sidespor" "Go to side track")))
                              (font-1 note-font-size-left grey1 (b track-title))))
                  "middle"))))


(define (book-style-svg-image . x)
 (let* ((image-file (car x))
        (width (if (> (length x) 1) (second x) #f))
        (height (if (> (length x) 2) (third x) #f))
        (reduced-width (string-append (as-string (* (number-of-measure width) 0.6)) (unit-of-measure width)))
        (reduced-height (string-append (as-string (* (number-of-measure height) 0.6)) (unit-of-measure height)))
        (comment (if (> (length x) 3) (fourth x) ""))
        (svg-clause-id (if (> (length x) 4) (fifth x) ""))
        (svg-inline-ast (if (> (length x) 5) (sixth x) #f))

        (svg-image-file #f)
        (png-image-file #f)
       )
  (if svg-inline-ast
      (let* ((proper-svg-ast (build-svg-ast-from-inline-contribution svg-inline-ast width height))
             (target-proper-file-name (string-append (as-string page-id) "-" svg-clause-id))
            )
        (set! image-file (string-append target-proper-file-name "." "svg"))
        (set! svg-image-file (string-append target-proper-file-name "." "svg"))
        (set! png-image-file (string-append target-proper-file-name "." "png")) )
      (let*  ; ???
            ((target-proper-file-name (string-append (as-string page-id) "-" svg-clause-id)))
        (set! svg-image-file image-file)
        (set! png-image-file (string-append target-proper-file-name "." "png")) )
  )

  (let ((target-dir (string-append note-source-directory (relative-source-html-destination-path-fragment)))

	(svg-image-contribution-non-css 
	 (object 'width (as-string reduced-width) 'height (as-string reduced-height)
		 'data (string-append  "graphics/" svg-image-file) 'type "image/svg+xml"
		 please-download-svg-player
		 )
	 )

       (png-image-contribution-non-css (img 'alt "" 'src (string-append  "." "/" "graphics/small/" png-image-file) 'border 0))
      )

   (book-element
    (font-1 2 book-header-foreground-color (em comment))
    (cond ((eq? treat-svg-as 'svg)
	   (con (center svg-image-contribution-non-css) (p)))
	  ((eq? treat-svg-as 'png)
	   (con (center png-image-contribution-non-css) (p)))
	  ((and (eq? treat-svg-as 'png-if-exist) (file-exists? (string-append target-dir "graphics/small/" png-image-file)))
	   (con (center png-image-contribution-non-css) (p)))
	  ((and (eq? treat-svg-as 'png-if-exist) (not (file-exists? (string-append target-dir "graphics/small/" png-image-file))))
	   (con (center svg-image-contribution-non-css) (p)))
    ))
 ) ))


(define (book-style-flash-image . x)
 (let* ((image-file (car x))
        (width (if (> (length x) 1) (second x) #f))
        (height (if (> (length x) 2) (third x) #f))
        (comment (if (> (length x) 3) (fourth x) ""))

        (reduced-width (as-number (* (as-number width) 0.6)))
        (reduced-height (as-number (* (as-number height) 0.6)))

        (image-file-path (string-append  "." "/" "graphics/" image-file))
       )
  (book-element
     (font-1 2 book-header-foreground-color (em comment))
     (con (center 
           (let (
                   (image-contribution
		    (object 'width (as-string reduced-width) 'height (as-string reduced-height)
			    'classid "clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"
			    'codebase "http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0"
			    (param 'name "movie" 'value image-file-path)
			    (param 'name "quality" 'value "high")
			    (param 'name "salign" 'value "T")
					;bgcolor ?
			    (free-html-element "embed" 
					       'src image-file-path
					       'type "application/x-shockwave-flash" 
					       'pluginspage "http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash")
			    )
		    )
		   )
	       image-contribution))
          (p))
  )
 ))

(define (book-style-meta-text . x)
 (let* ((text (first x))
        (type (second x))
        (icon (if (equal? (as-string type) "readers-guide") 
                  (img-LN (readers-guide-icon-name language-preference)
                          "")
                  ""))
       )
  (book-element 
   icon
   (con (book-text (em text)) (p)))))

     

 
; -----------------------------------------------------------------------------

; Experimental program page
; 
; (define (program-page id program-file mark)
;   (let* ((progr-text (read-text-file-between-marks program-file mark)))
;     (write-text-file
;       (standard-slide 
;         "Program"
;         (program-text (pre progr-text))
;         #f)
;       (destination-path (string-append "program-" (as-string id)))
; )))
; 
; (define program-font-size 4)
; 
; (define (program-text txt)
;   (font-size program-font-size txt))


; -----------------------------------------------------------------------------
; Functions for generation of top level index page

; Return the id of the first slide. Take the information from the stored -.aux file.
; Return the empty string if it cannot be located.
(define (first-slide-id lecture-id)
  ; find first slide id from aux file
  (let ((aux-file (aux-file-name lecture-id)))
    (if (file-exists? aux-file)
        (let ((entries (entries-of-aux-file (file-read aux-file))))
          (if (null? entries) "" (id-of-aux-entry (first entries))))
        "")))

; Return the id of the first trail slide given the trail-list and trail-id.
(define (first-trail-slide-id trail-id trail-list)
  (if (not (null? trail-list))
      (letrec ((trail-entry-lecture-id first)
               (trail-entry-slide-id second))
        (filename-of-trail-frame trail-id (trail-entry-lecture-id (car trail-list)) (trail-entry-slide-id (car trail-list))))
      ""  ; Leads to linking error. alternative (error ...) here
  ))

; Extract slide title from the information in the aux file
(define (slide-title lecture-id page-id)
  (let ((entry (find-original-slide-info lecture-id page-id)))
    (if entry (car entry) "Title")))

; id is lecture-id
(define (old-front-index-entry id title normal-this-number this-number current-number)
  (let* ((fs-text (text-choice "Første slide" "First slide"))
         (first-slides-id (first-slide-id id))
         (lect-type (find-original-lecture-type id))
        )
    (append 
     (list title 

          (if (eq? lect-type 'normal)
              (string-append (text-choice "Lektion" "Lecture") " " (as-string normal-this-number))
              (img-LN "side-track-label.gif" ""))

          (if (<= this-number current-number)
              (a-tag (contents-url id) (text-choice "Oversigt" "Overview"))
            (text-choice "Oversigt" "Overview"))
              
          (if (<= this-number current-number)
              (if (equal? "" first-slides-id) 
                  fs-text
                  (if (is-there-show-and-speak-in-lecture? id)
                      (con (a-tag (slide-url id first-slides-id) fs-text) (horizontal-space 1)
                           (a-tag (slide-show-and-speak-url id first-slides-id 1) 
                             (img-LN "small-speaker.gif"
                                (text-choice "Første slide med indtalt forelæsning" "First audio lecture page"))))
                      (a-tag (slide-url id first-slides-id) fs-text)))
              fs-text)

          (if (<= this-number current-number)
              (a-tag (book-url id) (text-choice "Forelæsningsnote" "Lecture notes"))
            (text-choice "Forelæsningsnote" "Lecture notes")))
      (if exercise-index?
          (list 
            (if (<= this-number current-number)
                (if (> (length (exercise-info-list id)) 0)
                    (a-tag (html-file (exercise-index-name id)) (text-choice "Opgaver" "Exercises"))
                    "-")
              (text-choice "Opgaver" "Exercises"))
          )
          '())
)))

(define (front-index-entry id title normal-this-number this-number current-number)
  (let* ((fs-text (text-choice "Første slide" "First slide"))
         (first-slides-id (first-slide-id id))
         (lect-type (find-original-lecture-type id))
        )
    (append 
     (list 

          ""

          (string-append (font-color grey1 (b (text-choice "Lektion" "Lecture") " " (as-string normal-this-number))) )
          

          (if (and make-slide-view? (<= this-number current-number))
	      (if (equal? "" first-slides-id) 
		  ""
		  (if (is-there-show-and-speak-in-lecture? id)
		      (a-tag (slide-show-and-speak-url id first-slides-id 1) 
			     (img-LN "small-speaker.gif"
				     (text-choice "Første slide med indtalt forelæsning" "First audio lecture page")))
		      ""))
	      "")


          (con 

            (if (<= this-number current-number)
	       (a 'href (contents-url id)  'css:text-decoration "none"  title)
	       title)

          )

          (if (eq? lect-type 'side-track)
	      (con (img-LN "side-track-label.gif" "") (horizontal-space 2))
	      "")

              
    ))))




; Make and write the top level front page index.
; Provide for keyboard navigation to first 'row' in table.
; Old Original version - table stuff
(define (make-old-front-index)
                                        
  (let* ((w1 420)
         (w5 70)
         (w2 70)
         (w3 90)
         (w4 120)
         (w6 (if exercise-index? 70 0))
         (w-total (+ w1 w2 w3 w4 w5 w6))
         (normal-html-dest? (lambda () (equal? (relative-source-html-destination-path-fragment) "html/")))
        )
   (let* ((body 
           (con 
            (front-index-header)
            (font-size 6 (b notes-title)) (br) 
            (if notes-subtitle (font-size 5 (b notes-subtitle)) "") (p)

            (h 3
               (con 
                (list-to-string 
                  (map (lambda (e) (con e (br))) (append lecture-author-info (list lecture-date)))
                  ""
                )
                )
               )

            (if (>= news-flash-level 2) (indent-pixels 20 (leno-news-flash)) "")

            (if (not (equal? note-abstract "")) (con (indent-pixels 20 (color-frame-width (em note-abstract) grey2 (- w-total 40))) (p)) "")

            (if (not (null? (lecture-wide-exercise-list)))
                (con 
                  (a-tag (html-file overall-exercise-index-name) 
                         (font-size 2 (text-choice "Alle opgaver i dette materiale" "All exercises in this material")))
                  (horizontal-space 8)
                )
                "")

            (if cross-references?
                (a-tag (html-file global-cross-reference-name)
                       (font-size 2 (text-choice "Alle referencer fra dette materiale" "All references in this material")))
                "")  


            (p)

            (table-1 1
                     (append (list w1 w5 w2 w3 w4) (if exercise-index? (list w6) '()))
                     (make-list (if exercise-index? 6 5) slide-background-color)
                     (map front-index-entry 
                           lecture-list
                           lecture-titles
                           (special-lecture-interval lecture-list)  ; earier just: (number-interval 1 (length lecture-list))
                           (number-interval 1 (length lecture-list))
                           (make-list (length lecture-list) current-lecture)  )) (p)

            (table-3 0 (list 180 "*") 
              (list (list  (laml-power-icon "") (when-generated))))

            ))
          (first-lecture-id (car lecture-list))
          (down-links (list (contents-url first-lecture-id)
                            (slide-url first-lecture-id (first-slide-id first-lecture-id))
                            (book-url first-lecture-id)))
          )
      (write-text-file
       (if java-scripting
           (page-with-keypress-script
            notes-title
            '()
            body

            javascript-loading
            (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
            (js-call "lenoNavigate" 
                     (append
                      (map string-it-single 
                           (list ""
                                 "" 
                                 ""
                                 ""
                                 "" "" ""))
                      (list (js-string-array down-links))))

            white black blue blue
            mouse-advancement
            (actual-stylesheets apply-css-styling?)
            (leno-css-class 'index)
         )

         (page-1
          notes-title
          '()
          body
          white black blue blue
          (actual-stylesheets apply-css-styling?)
          (leno-css-class 'index)
          ))
 
      (destination-path top-level-note-file-name)))))

(define (make-front-index)
                                        
  (let* ((w1 420)
         (w5 70)
         (w2 70)
         (w3 90)
         (w4 120)
         (w6 (if exercise-index? 70 0))
         (w-total (+ w1 w2 w3 w4 w5 w6))
         (normal-html-dest? (lambda () (equal? (relative-source-html-destination-path-fragment) "html/")))
        )
   (let* ((body 
           (div (style 'type "text/css" "a:link, a:visited  {color: black}, a:hover {color: black; background-color: #17ffff; }")
            (front-index-header) (p)

            (title-author-logo-strip notes-title notes-subtitle lecture-author-info lecture-date #f)

            (if (>= news-flash-level 1) (indent-pixels 20 (leno-news-flash)) "")

            (if (and (not top-level-abstract-page?) (not (equal? note-abstract ""))) (con (indent-pixels 20 (color-frame-width (em note-abstract) grey2 (- w-total 40))) (p)) "")

            (if top-level-title-page?
		(a-tag
		 (html-file (top-level-title-page-name))
		 (font-size 2 (text-choice "Forside" "Title page"))
		 )
		"")

            (horizontal-space 12)

            (if top-level-abstract-page?
		(a-tag
		 (html-file (top-level-abstract-page-name))
		 (font-size 2 (text-choice "Sammendrag" "Abstract"))
		 )
		"")

            (horizontal-space 12)

            (if (not (null? (lecture-wide-exercise-list)))
                (con 
                  (a-tag (html-file overall-exercise-index-name) 
                         (font-size 2 (text-choice "Alle opgaver i dette materiale" "All exercises in this material")))
                  (horizontal-space 8)
                )
                "")

            (if cross-references?
                (a-tag (html-file global-cross-reference-name)
                       (font-size 2 (text-choice "Alle referencer fra dette materiale" "All references in this material")))
                "") 

            (p)

            (if show-and-speak? 
                   (let* ((secs (sum-list (map internal-speak-time (speak-list-of-all-lectures))))
                          (time-str (speak-playing-time secs))
                          )
                     (font-size 2 
                       (em (con (text-choice "Samlet spilletid af indtalte forelæsningr: "
                                             "Total playing time of all audio lectures: ")
                                 time-str (p))
                       )))
                   (p))


            (table-1 0
                     (list 10 90 15 425 100)
                     (make-list 5 slide-background-color)
                     (map front-index-entry 
                           lecture-list
                           lecture-titles
                           (number-interval 1 (length lecture-list))  ; earier just:  (special-lecture-interval lecture-list)
                           (number-interval 1 (length lecture-list))
                           (make-list (length lecture-list) current-lecture)  )) (p)

            (left-middle-right-banner
                 (leno-icon)
                 (when-generated)
                 (laml-power-icon "")
               )

;            (table-3 0 (list 180 "*") 
;              (list (list  (laml-power-icon "") (when-generated))))

            ))
          (first-lecture-id (car lecture-list))
;           (down-links (list (contents-url first-lecture-id)
;                             (slide-url first-lecture-id (first-slide-id first-lecture-id))
;                             (book-url first-lecture-id)))
          (down-links (map (lambda (lec-sec-entry)  (contents-url (car lec-sec-entry)))
                         (front-sublist lecture-sections max-keyboard-navigation-number)))  ; 1 - 9, a - m
          )
      (write-text-file
       (if java-scripting
           (page-with-keypress-script
            notes-title
            '()
            body

            javascript-loading
            (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
            (js-call "lenoNavigate" 
                     (append
                      (map string-it-single 
                           (list ""
                                 "" 
                                 ""
                                 ""
                                 "" "" (html-file top-level-theme-index-file-name)))
                      (list (js-string-array down-links))))

            white black blue blue
            mouse-advancement
            (actual-stylesheets apply-css-styling?)
            (leno-css-class 'index)
         )

         (page-1
          notes-title
          '()
          body
          white black blue blue
          (actual-stylesheets apply-css-styling?)
          (leno-css-class 'index)
          ))
 
      (destination-path top-level-note-file-name)))))


; Return a special number interval of lecture-id-list.
; Lectures that corresponds to normal lectures get a number.
; Lectures that corresponds to side track lectures, do not. For these we return a #f entry.
(define (special-lecture-interval lecture-id-list)
 (letrec ((special-lecture-interval-1
            (lambda (n lecture-id-list)
              (cond ((null? lecture-id-list) '())
                    (else (if (eq? (find-original-lecture-type (car lecture-id-list)) 'normal)
                              (cons n  (special-lecture-interval-1 (+ n 1) (cdr lecture-id-list)))
                              (cons #f (special-lecture-interval-1 n (cdr lecture-id-list)))))))))
  (special-lecture-interval-1 1 lecture-id-list)))





; ---------------------------------------------------------------------------------------------------------------
; Functions for generation of note index (stikordsregister) from idx files.

; Collect all index words from relevant idx files.
; This function is roboust for non-existing idx files.
(define (all-index-words)
    (accumulate-right append '()
              (map (lambda (f) (if (file-exists? f) (file-read f) '()))
                   (map (lambda (f) (word-index-file-name f))  
                        (front-sublist lecture-list current-lecture)))))

; A single line in the word index.
; Index-element is of the form 
; Old version
(define (format-word-in-index-slide index-element)
  ; an index element constructed by make-word-index-entry
  (if (string? index-element) ; Initial letter in non-page divided word-index
      (con (p) (a-name (downcase-string index-element)) (font-size 5 (b index-element)))
      (let ((lid (lecture-id-of-index-entry index-element))
            (pid (page-id-of-index-entry index-element))
            (ttl (word-of-index-entry index-element))
            (kind (kind-id-of-index-entry index-element))
           )

        (list 
          (cond ((eq? kind 'concept) (b ttl))
                ((eq? kind 'index) (em ttl))
                ((eq? kind 'source-program) (kbd ttl))
                (else ttl))
          (a 'href
             (primary-view-url lid pid)
             'title 
             (cond ((eq? kind 'concept)   (text-choice "Begreb" "Concept"))
                   ((eq? kind 'index)     (text-choice "Indeks ord" "Index word"))
                   ((eq? kind 'exercise)  (text-choice "Opgave titel" "Exercise title"))
                   ((eq? kind 'title)     (text-choice "Overskrift på side" "Page title"))
                   ((eq? kind 'source-program)  (text-choice "Kildeprogram" "Source program"))
                   (else ""))
             "On slide"   
             )
          "-"))))


; New version as of march 2007
(define (format-word-in-index-slide-and-theme index-element)
  ; an index element constructed by make-word-index-entry
  (if (string? index-element) ; Initial letter in non-page divided word-index
      (con (p) (a-name (downcase-string index-element)) (font-size 5 (b index-element)))
      (let* ((lid (lecture-id-of-index-entry index-element))
             (pid (page-id-of-index-entry index-element))
             (ttl (word-of-index-entry index-element))
             (kind (kind-id-of-index-entry index-element))
             (theme-url (theme-url-of-lecture-id-and-page-id (as-symbol lid) (as-symbol pid)))
            )
        (list
           (cond ((eq? kind 'concept) (b ttl))
		 ((eq? kind 'index) (em ttl))
		 ((eq? kind 'source-program) (kbd ttl))
		 (else ttl))

           (a 'href
              (primary-view-url lid pid)
              'title 
              (cond ((eq? kind 'concept)   (text-choice "Begreb" "Concept"))
                    ((eq? kind 'index)     (text-choice "Indeks ord" "Index word"))
                    ((eq? kind 'exercise)  (text-choice "Opgave titel" "Exercise title"))
                    ((eq? kind 'title)     (text-choice "Overskrift på side" "Page title"))
                    ((eq? kind 'source-program)  (text-choice "Kildeprogram" "Source program"))
                    (else ""))
              "On slide"   
              )

           (if theme-url
               (a 'href
                    theme-url
                    'title 
                    (cond ((eq? kind 'concept)   (text-choice "Begreb" "Concept"))
                          ((eq? kind 'index)     (text-choice "Indeks ord" "Index word"))
                          ((eq? kind 'exercise)  (text-choice "Opgave titel" "Exercise title"))
                          ((eq? kind 'title)     (text-choice "Overskrift på side" "Page title"))
                          ((eq? kind 'source-program)  (text-choice "Kildeprogram" "Source program"))
                          (else ""))
                    "In textbook"   
                    )
                "-")
        )
      )
  )
) 

; Controls the word index: Either slide or slide-and-theme (symbols).
; Assigned from fron-matters attribute
(define word-index-type 'slide)  ; slide  or   slide-and-theme

; Selection of format-word-in-index: Either only references to slides, or references to both slides and themes.
; The latter is much more expensive to generate.
; Return a function, either format-word-in-index-slide or format-word-in-index-slide-and-theme.
(define (format-word-in-index) 
  (cond ((eq? word-index-type 'slide) format-word-in-index-slide)
        ((eq? word-index-type 'slide-and-theme) format-word-in-index-slide-and-theme)
        (else (laml-error "Unknown word-index-type" word-index-type))))


(define (about-the-word-index)
 (center
  (color-frame 
    (em 
      (text-choice
       (con-par "I det alfabetiske indeks er kursive ord egentlige indeksord,
              ikke-kursiverede ord er slideoverskrifter, fede ord er begreber
              som er defineret i noterne, og ord med skrivemaskinefont er kildeprogrammer. " 
              "Via nedenståene listning af alfabetet kan man navigere til
              det ønskede ord's begyndelsesbogstav." )
       (con-par "In this alphabetic index the words in italic are proper index words,
                 non-italic words are section headings, words in bold face are concepts
                 defined in these notes, and words set with keyboard font are source programs. "
                "In the alphabet listing below you can navigate to the desired initial
                 letter of a word."
       )
      )
    )
    grey2 )))

(define (make-lecture-word-index-pages index-list)
  ; write an index page for each letter in the alphabet
 (let* ((alphabetic-predicate (string-starting-with-predicate
                                          (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" 
                                                "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "æ" "ø" "å"
                                                "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" 
                                                "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "Æ" "Ø" "Å")))
        (alphabetic-index-list (filter (lambda (entry)
                                         (alphabetic-predicate (car entry)))
                                       index-list))
        (splitted-index-list (split-index (sort-list alphabetic-index-list index-elements-leq?)))
        (alphabet (map downcase-string (map first-letter-of (map caar splitted-index-list))))
        (lecture-author-info-1 (if (and (list? lecture-author-info) (not (null? lecture-author-info))) 
                                   lecture-author-info
                                   author-and-affiliation-list)) 
       )

  ; Make overall index page:

  (let ((prev "")
	(next "")
	(up     (cond (themes? (html-file top-level-theme-index-file-name))
		      (front-index? (front-index-url))
		      (else "")))
	(down-links (map (lambda (letter)
                            (html-file (string-append file-name-of-word-index  "-"  (downcase-string letter))))
                         alphabet))
       )
    (write-text-file
     (page-with-keypress-script 
      (con-space " Index:" notes-title)
      '()
      (con 
       (word-index-header) (br)
       (h 1 (con-space "Index:" notes-title))
       (h 3 (list-to-string (append lecture-author-info-1 (list lecture-date)) ", ")) (p)
       (about-the-word-index) (p)
       (alphabetic-link-array-1 file-name-of-word-index alphabet) (p)

       (when-generated)
       )
      javascript-loading
      (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
      (js-call "lenoNavigate"		;@b
	       (append
		(map string-it-single 
		     (list prev
			   next 
			   up
			   ""
			   "" "" ""))
		(list (js-string-array down-links))))
      slide-background-color black black black
      mouse-advancement
      (actual-stylesheets apply-css-styling?)
      (leno-css-class 'index)
      )
     (destination-path file-name-of-word-index)))

   ; Make individual index pages: 
   (if (not (null? alphabet))
       (map4 (lambda (index-list letter next-letter prev-letter)
	       (make-single-letter-index index-list letter next-letter prev-letter alphabet))
	     splitted-index-list
	     alphabet
	     (append (cdr alphabet) (list #f))
	     (append (list #f) (butlast alphabet))
	     ))

 )
)

; Generate a predicate which accepts a string starting with a letter in char-list.
; Char-list is either a list of single letter strings or chars
(define (string-starting-with-predicate char-list)
 (let ((char-list-1 (map as-string char-list)))
  (lambda (str)
   (if (empty-string? str)
       #f
      (let ((first-char (as-string (string-ref str 0))))
        (member first-char char-list-1))))))

(define (first-letter-of x)
  (as-string (string-ref (as-string x) 0)))

; Split the total index list alphabetically.
; The input list is supposed to be sorted alphabetically.
(define (split-index index-list)
  (sublist-by-predicate
    index-list
    (lambda (cur prev n) 
       (not (eqv? (string-ref (as-string (decapitalize-string-nd (car cur))) 0)
                 (string-ref (as-string (decapitalize-string-nd (car prev))) 0))))))


(define (make-single-letter-index index-list letter alphabet)
  (write-text-file
   (page (string-append "Note index: letter " letter)
         (con
          (word-index-header) (br)
          (h 1 (con-space (text-choice "Indeks:" "Index:") notes-title))
          (h 2 (string-append (text-choice "Startbogstav: " "First letter: ") (as-string letter)))

          (alphabetic-link-array-1 file-name-of-word-index alphabet letter) (p)

          (indent-pixels 10
            (table-3 0 (list 400 80 100)
              (map (format-word-in-index) index-list)))

          )
          white black black black
   )
   (destination-path (string-append file-name-of-word-index  "-"  (downcase-string letter))))
)

; Redefinition. For improved navigation.
(define (make-single-letter-index index-list letter next-letter prev-letter alphabet)
   (let ((prev (if prev-letter (html-file (string-append file-name-of-word-index  "-"  (downcase-string prev-letter))) ""))
         (next (if next-letter (html-file (string-append file-name-of-word-index  "-"  (downcase-string next-letter))) ""))
         (up   (cond (themes? (html-file top-level-theme-index-file-name))
		     (front-index? (front-index-url))
		     (else "")))
         (down-links '())
        )
    (write-text-file
     (page-with-keypress-script
           (string-append "Note index: letter " letter)
           '()
	   (con
	    (word-index-header-extended prev-letter next-letter) (br)
	    (h 1 (con-space (text-choice "Indeks:" "Index:") notes-title))
	    (h 2 (string-append (text-choice "Startbogstav: " "First letter: ") (as-string letter)))

	    (alphabetic-link-array-1 file-name-of-word-index alphabet letter) (p)

	    (indent-pixels 10
			   (table-3 0 (list 400 80 100)
				    (map (format-word-in-index) index-list)))

	    )
           javascript-loading
           (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
           (js-call "lenoNavigate"   ;@b
                   (append
                     (map string-it-single 
                          (list prev
                                next 
                                up
                                ""
                                "" "" ""))
                     (list (js-string-array down-links))))
	   white black black black
           mouse-advancement
           (actual-stylesheets apply-css-styling?)
           (leno-css-class 'index)
	   )
     (destination-path (string-append file-name-of-word-index  "-"  (downcase-string letter))))))

     
(define (make-lecture-word-index-page index-list)
  ; write a page with the word index of this lecture
 (let ((lecture-author-info-1 (if (and (list? lecture-author-info) (not (null? lecture-author-info))) 
                                   lecture-author-info
                                   author-and-affiliation-list)))
  (write-text-file
    (page-1 
      (con lecture-title (con-space " Index:" notes-title))
      '()
      (con 
        (word-index-header) (br)
        (h 1 (con-space "Index:" notes-title))
        (h 3 (apply con-space (append lecture-author-info-1 (list lecture-date)))) (p)
        (about-the-word-index) (p)
        (alphabetic-link-array) (p)

       (indent-pixels 10
         (table-3 0 (list 400 80 100)
          (map (format-word-in-index)
               (merge-lists
                 (sort-list index-list index-elements-leq?)
                 (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" 
                        "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "Æ" "Ø" "Å")
                 (lambda (e1 e2) (string<=? (decapitalize-string  (string-copy e2)) (decapitalize-string (string-copy  (car e1)))))
               )

          ))
       )
       (when-generated)
      )
      slide-background-color black black black
      (actual-stylesheets apply-css-styling?)
      (leno-css-class 'index)
    )
   (destination-path file-name-of-word-index))))

;; Return a string which tells about the generating computer system.
;; Default: the empty string.
(define (generating-system)
 "")


(define (when-generated)
 (let* ((tm (current-time))
        (day (weekday tm))
        (dt (date-time tm))
        (date (car dt))
        (time (cadr dt)))
  (font-1 2 red (con (text-choice "Genereret: " "Generated: ") (capitalize-string-nd day) " " date ", " time (br)
                   (generating-system)))))




; return a total concatented list of all lecture note pages on idx format (as found in the aux file)
; (title-string idsymbol is-title-boolean page-number)
(define (all-idx)
 (accumulate-right
   append
   '()
   (map
    (lambda (lecture-id)
        (let* ((aux-file (aux-file-name lecture-id))
               (aux-contents (file-read aux-file)))
      (entries-of-aux-file aux-contents)))
    (front-sublist lecture-list current-lecture))))


; -----------------------------------------------------------------------------

; returns a string which provides for tracing of the
; underlying software and sources:
(define (file-and-directory-identification)
 (apply string-append
  (map format-key-value-pairs
   (list
     (list "LAML source file" (string-append note-source-directory lecture-id "." "laml"))
     (list "Scheme system" scheme-system)
     (list "Scheme library directory" the-library)
     (list "Style Scheme file" (string-append leno-software-directory "lecture-notes.scm"))
   ))))

(define (format-key-value-pairs el)
  (let ((key (car el))
        (val (cadr el)))
    (string-append (as-string key) ": " (as-string val) "; ")))

; a comment with tracing information.
; inserted on every generated laml page
(define (tracing-comment)
  (html-comment (file-and-directory-identification)))


; ---------------------------------------------------------------------------------------------------
; TRAIL STUFF:

(define lecture-trail-list '()) ; a list of two-element lists:
                                  ; such as ((gui litterature) (gui andre-komponenter))

(define trail-length 0) 

(define (slide-select lecture-id slide-id)
  (cons 'slide-select (list lecture-id slide-id)))

; User level trail function. Convenient syntactic top-level form.
; trail-id is a symbol identifying the trail.
; elements is a list of slide-select forms (mainly syntax). See the slide-select function.
; Generates, as a side-effect, a simple top-level index file
(define (note-trail trail-id . elements)
  (note-trail-1 trail-id elements)
  (if source-level-index-page? 
    (begin 
      (display-message (string-append "You can start browsing these lecture notes at " lecture-id "." target-extension)) 
      (make-source-level-index-page 'minimal)
    )
  )

  ; Make trail-specific title and abstract page with proper continuation
   (if lecture-title-page? 
     (begin 
       (make-lecture-title-page (html-file (first-trail-slide-id trail-id lecture-trail-list)) "???")
       (make-lecture-abstract-page  (html-file (first-trail-slide-id trail-id lecture-trail-list)) "???")
     )
   )
)


; A programmatic variant of note-trail. Takes two regular parameter. Easier to call from a program.
(define (note-trail-1 trail-id elements)
  (let ((trail-list (map (lambda (e) (list (second e) (third e))) elements)))
   (set! lecture-trail-list trail-list)
   (set! trail-length (length trail-list))
   (make-trail trail-id trail-list)
   (make-trail-front-index trail-id (if (null? trail-list) #f (first (car trail-list))))
))

(define (slide-name-url-1 trail-id lecture-id slide-id)
  (if (and (eq? lecture-id 'nil) (eq? slide-id 'nil))
      ""
      (string-append (filename-of-trail-frame trail-id lecture-id slide-id) ".html")))

(define (filename-of-trail-navigation trail-id lecture-id slide-id)
  (string-append (as-string trail-id) "-TB-" (slide-name lecture-id slide-id)))

(define (filename-of-trail-frame trail-id lecture-id slide-id)
  (string-append (as-string trail-id) "-TF-" (slide-name lecture-id slide-id)))


(define (make-trail trail-id trail-list)
  ; trail-list is a list of pairs (lecture-id slide-id)
  (map5
    (lambda (e trail-id prev next current-number) 
       (make-trail-page trail-id (car e) (cadr e) (apply slide-name-url-1 (cons trail-id prev)) (apply slide-name-url-1 (cons trail-id next)) current-number)
    )
    trail-list
    (make-list (length trail-list) trail-id)
    (cons (list 'nil 'nil) trail-list)                ; previous list - first element has no previous
    (append (cdr trail-list) (list (list 'nil 'nil))) ; next list - last element has no next
    (number-interval 1 trail-length)
))



(define (make-trail-page trail-id lecture-id slide-id previous-page-url next-page-url current-trail-number)
  (let* ((slide-filename (cond ((eq? trail-embedding-mode 'slide) (slide-name lecture-id slide-id))
                               ((eq? trail-embedding-mode 'annotated-slide) (note-name lecture-id slide-id))
                               (else (laml-error "make-trail-page: Unknown value of trail-embedding-mode:" trail-embedding-mode))))
         (title (slide-title lecture-id slide-id))
         (navigation-filename (filename-of-trail-navigation trail-id lecture-id slide-id))
         (frame-filename (filename-of-trail-frame trail-id lecture-id slide-id))
         (trail-index-url (string-append (as-string trail-id) ".html"))
         (this-page-url (slide-name-url-1 trail-id lecture-id slide-id))
         (script (list (string-append leno-software-directory "javascript/") "lenonavigate.js"))
         (script-clause 
              (cond ((eq? javascript-loading 'static)
                         (html4:script (read-text-file (string-append (car script) (cadr script))) 'type "text/javascript"))
                    ((eq? javascript-loading 'dynamic) 
                         (html4:script "" 'src (string-append "javascript/" (cadr script)) 'type "text/javascript"))
                    (else (error "page-with-keypress-script in html-v1: problems determining type of script loading"))))
        )

     (if (>= lecture-note-verbose-mode 1)
         (display-message (string-append "Trail page " (as-string trail-id) ":" (as-string lecture-id)
                                          ":"  (as-string slide-id) " OK")))
     (write-text-file
      (if java-scripting
        (html4:html
         (con (html4:head (html4:title title))
              script-clause
              (con
               (html4:frameset 
                (con (html4:frame 'name "leno:navigation" 'src (string-append navigation-filename ".html")
                                 'scrolling "no" 'noresize "noresize" 'frameborder "0") 
                     (html4:frame 'name "leno:main" 'src (string-append slide-filename ".html")
                                 'scrolling "auto" 'frameborder "0"))
                'cols "50,*"  
                'onkeypress   ; causes validation - fixed in HTML4.0 loose DTD by adding events attributes to frameset
                (js-call "lenoTrailframeNavigate" 
			 (map string-it-single 
			      (list previous-page-url
				    next-page-url
				    trail-index-url)))
                )
              )
              ))
         (html4:html
          (con (html4:head (html4:title title))
               (html4:frameset 
                (con (html4:frame 'name "leno:navigation" 'src (string-append navigation-filename ".html")
                                 'scrolling "no" 'noresize "noresize" 'frameborder "0") 
                     (html4:frame 'name "leno:main" 'src (string-append slide-filename ".html")
                                 'scrolling "auto" 'frameborder "0" ))
                'cols "50,*" 
                )
               )))
      (destination-path frame-filename))

   (let ((body (center (con
                        (font-size 2 (b (con (as-string current-trail-number) ":" (as-string trail-length)))) (p)
                        (a-tag-target trail-index-url 
                            (img-LN "nav-contents-frame.gif" 
				    (key-shortcut
				     (text-choice "Oversigt over dette spor" "Overview of this trail") "r")) "_top") (br)
                        (if (equal? previous-page-url "")
                            (img-LN "nav-up-frame-empty.gif" "")
                          (a-tag-target previous-page-url 
                              (img-LN "nav-up-frame.gif" 
				      (key-shortcut
				       (text-choice "Forrige slide i dette spor" "Previous slide in this trail") "q")) "_top")) (br)

                        (a-tag-target this-page-url 
                              (img-LN "nav-current-frame.gif" 
                                       (text-choice "Nuværende slide i dette spor" "Current slide in this trail") ) "_top") (br)


                        (if (equal? next-page-url "")
                            (img-LN "nav-down-frame-empty.gif" "")
                          (a-tag-target next-page-url 
                              (img-LN "nav-down-frame.gif" 
				      (key-shortcut 
                                       (text-choice "Næste slide i dette spor" "Next slide in this trail") "w")) "_top"))
                        ))))
     (write-text-file
      (if java-scripting
           (page-with-keypress-script
            "navigation"
            '()
            body

            javascript-loading
            (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
            (js-call "lenoTrailbarNavigate" 
                                 (map string-it-single 
                                       (list previous-page-url
                                             next-page-url
                                             trail-index-url)))

            slide-background-color black black black
            mouse-advancement
            (actual-stylesheets apply-css-styling?)
          )

          (page "navigation"
                body
                slide-background-color black black black))
      (destination-path navigation-filename)))
  )
)


; make and write the top level front page index for trails
; surrounding-lecture-id is the lecture id of the first slide in the trail.
(define (make-trail-front-index trail-id surrounding-lecture-id)
 (let ((lecture-author-info-1 (if (and (list? lecture-author-info) (not (null? lecture-author-info))) 
                                   lecture-author-info
                                   author-and-affiliation-list))
       (down-links (map (lambda (lecid-pgid) 
                         (let ((original-lecture-id (car lecid-pgid))
                               (original-slide-id (cadr lecid-pgid)))
                           (html-file (filename-of-trail-frame trail-id original-lecture-id original-slide-id)))) lecture-trail-list))

      )
  (write-text-file
   (page-with-keypress-script
    notes-title
    '()
    (con 
     (index-page-header 'trail) (br)
     (h 1 lecture-title)
     (h 3
       (con 
        (list-to-string
         (map (lambda (e) (con e (br))) (append lecture-author-info-1 (list lecture-date)))
         ""
        )
       )
     ) (p)

     ; guiding text with possible link to containg index:
     (let ((containing-index-url 
             (if (and (= (length lecture-list) 1) surrounding-lecture-id)
                 (html-file (index-name surrounding-lecture-id))
                 #f)))
      (cond ((> (length lecture-list) 1)
              (em (text-choice "Disse slides er et spor, som er lagt ned over et antal lektioner i samlingen af forelæsningsnoter"
                               "These slides is a trail imposed on a number of lectures in a collection of lecture notes")))
            ((and (= (length lecture-list) 1) containing-index-url)
              (em (text-choice (con "Disse slides er et spor, som er lagt ned over " (a-tag containing-index-url "en større samling af slides og forelæsningsnoter") )
                               (con "These slides form a trail imposed on " (a-tag containing-index-url "a larger collection of slides and lecture notes") ))))
            ((= (length lecture-list) 1)
              (em (text-choice "Disse slides er et spor, som er lagt ned over en større samling af slides og forelæsningsnoter"
                               "These slides form a trail imposed on a larger collection of slides and lecture notes")))
            (else ""))
     ) (horizontal-space 24)

     (if lecture-title-page?
         (a-tag
          (html-file (title-page-name lecture-id))
          (text-choice "Forside af spor" "Trail title page")
          )
         "")
     (horizontal-space 12)

     (if lecture-title-page?
         (a-tag
          (html-file (abstract-page-name lecture-id))
          (text-choice "Sammendrag af spor" "Trail abstract")
         )
        "")
     (p)

     (if (>= news-flash-level 3) (leno-news-flash 4 950) "")

     (div (style 'type "text/css" "a:link, a:visited  {color: black}, a:hover {color: black; background-color: #17ffff; }")  ; not here!
      (table-1 0
        '(10 70 420 390)
        (make-list 4 slide-background-color)
        (map2 
          (lambda (lectureid-slideid number) ; number not used...
            (let* ((original-slide-id (cadr lectureid-slideid))
                   (original-lecture-id (car lectureid-slideid))
                   (slide-descr (find-original-slide-info original-lecture-id original-slide-id))
                   (original-chapter-title (find-original-chapter-title original-lecture-id))
                   (slide-title (car slide-descr))
                   (slide-section-start? (caddr slide-descr))
                   (the-page-number (if (= 4 (length slide-descr)) (fourth slide-descr) "-"))
                  )
               (list ""
                     (font-color grey1
                       (b (con (text-choice "Side " "Page ") (as-string number))))

                     (a 
                        'href (string-append (filename-of-trail-frame trail-id original-lecture-id original-slide-id) ".html")
                        'css:text-decoration "none"
                        (if slide-section-start?
				(font-color red (b slide-title))
				slide-title)
                        'target "_top")

                     (font-color grey1 (con original-chapter-title (con ", " (text-choice "side " "page ") (as-string the-page-number))))

               )))

          lecture-trail-list (number-interval 1 (length lecture-trail-list))))) (p)

     (when-generated)
   )

   javascript-loading
   (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
   (js-call "lenoNavigate"		;@b
	    (append
	     (map string-it-single 
		  (list ""
			"" 
			""
			""
			"" "" ""))
	     (list (js-string-array down-links))))

   white black blue blue
   mouse-advancement
   (actual-stylesheets apply-css-styling?)
   (leno-css-class 'index)
   )
   (destination-path (as-string trail-id))))
)

; lookup a record in the aux-file
(define (find-original-slide-info lecture-id slide-id)
  (let ((aux-file (aux-file-name lecture-id)))
    (if (file-exists? aux-file)
        (let* ((aux-contents (file-read aux-file))
               (res (find-in-list 
                      (lambda (el) (eq? (id-of-aux-entry el) slide-id))
                      (entries-of-aux-file aux-contents))))
          (if res res (error (string-append "find-original-slide-info: Did not find " (as-string slide-id)
                                            " in aux file of lecture " (as-string lecture-id)))))
        (error (string-append "find-original-slide-info: Did not find aux file of " (as-string lecture-id))))))

(define (find-original-chapter-title lecture-id)
  (let ((aux-file (aux-file-name lecture-id)))
    (if (file-exists? aux-file)
        (let ((aux-contents (file-read aux-file)))
           (title-from-aux-file aux-contents))
        "-")))   ; (as-string lecture-id) perhaps better and more informative than "???"

; Return the lecture type of lecture-id, as read out of the aux file.
; If the information is not there, return normal (a symbol).
(define (find-original-lecture-type lecture-id)
  (let ((aux-file (aux-file-name lecture-id)))
    (if (file-exists? aux-file)
        (let ((aux-contents (file-read aux-file)))
           (lecture-type-from-aux-file aux-contents))
        'normal)))

; Lookup and return the aux entry of page-id (a symbol) in aux-structure.
; Return #f if not found.
(define (find-aux-record aux-structure page-id)
 (let ((entries (entries-of-aux-file aux-structure)))
  (find-in-list
    (lambda (entry) (eq? (as-symbol page-id) (id-of-aux-entry entry)))
    entries)))
      


; ----------------------------------------------------------------------------------------

;;; Trail utility functions - now also used internally, as controlled by the front-matters attribute trail-source 
;;; (and the lecture-notes-kernel.scm variable trail-source-mode.
;;; A trail is definition of a sequence of existing lecture pages from different lectures in a collection.
;;; In a trail it is possible to select slides from different chapters, and to adjust the sequencing among these.
;;; A trail can be defined very conveniently by simple mentioning the lecture-id and page-id of each slide, we want
;;; to include in the trail.<p>
;;; 
;;; The main function in this utility is total-trail. This function makes a total trail of all the slides in lecture-list, with a
;;; cutoff defined by the variable current-lecture.
;;; Bring the trail description into a text editor, and delete unwanted slides, and reorder them as appropriate.
;;; This facility uses the philosophy that it is always easier to delete something existing than writing somthing new. <p>
;;;
;;; Usage: See the next section of this documentation.
 

;; Return a full trail of the lecture with lecture-id
(define (full-trail lecture-id)
  (let* ((aux-file (aux-file-name lecture-id))
         (aux-contents (file-read aux-file)))
   (accumulate-right 
      (lambda (x y) (string-append x CR  y))
      ""
      (map trace-entry-pretty-print (map (lambda (entry) (list 'slide-select lecture-id  (cadr entry))) (entries-of-aux-file aux-contents))))))

(define (trace-entry-pretty-print entry)
  (string-append "(" (as-string (car entry)) " " "'" (as-string (cadr entry)) " " "'" (as-string (caddr entry)) ")"))

;; Return a full trail of all slides in all lectures in the lecture-list
(define (total-trail)
  (accumulate-right
    (lambda (x y) (string-append x CR y))
    ""
    (map full-trail (front-sublist lecture-list current-lecture))))


; Here follows XML-in-LAML counterparts of full-trail and total-trail

; Return a string with a sequence of (page-select ...) clauses.
; list-of-lectures is a list of lecture-ids, to take into account (a list of strings).
(define (total-trail-xml-in-laml list-of-lectures)
 (let ((CR (as-string #\newline)))
  (accumulate-right
    (lambda (x y) (string-append x CR y))
    ""
    (map full-trail-xml-in-laml list-of-lectures))))

(define (full-trail-xml-in-laml lecture-id)
  (let* ((CR (as-string #\newline))
         (aux-file (aux-file-name lecture-id))
         (aux-contents (file-read aux-file))
        )
   (accumulate-right 
      (lambda (x y) (string-append x CR  y))
      ""
      (map page-select-xml-in-laml-pp 
           (map (lambda (entry) (list lecture-id (id-of-aux-entry entry))) (entries-of-aux-file aux-contents))))))

(define (page-select-xml-in-laml-pp lid-pid-entry)
  (let ((lid (car lid-pid-entry))
        (pid (cadr lid-pid-entry))
       )
    (string-append "(" "page-select "  "'lecture-id " (string-it (as-string lid)) " 'page-id " (string-it (as-string pid))  ")")))


; -----------------------------------------------------------------------------
; Return a list of all cross references in this set of lectures
; Does not any longer require that a prefix, lectures until current-lecture, has been processed
(define (all-cross-references)
 (let ((file-read-empty-if-none (lambda (f) (if (file-exists? f) (file-read f) '()))))
  ; collect all cross references from -.crs files
    (accumulate-right append '()
              (map file-read-empty-if-none
                   (map (lambda (f) (cross-ref-file-name f))  
                        (front-sublist lecture-list current-lecture))))))


; make a separate page with cross references
; presumably, depending on the parameter, with all cross references in these notes
(define (make-total-cross-references-page total-cross-ref-list)
 (let ((formatter-function (generate-cross-reference-formatter slide-url)))
  (write-text-file
    (page-1 
      (con lecture-title (con-space "Cross references:" notes-title))
      '()
       (con
        (cross-reference-header) (br)
        (font-size 6 (b notes-title)) (br) 
        (if notes-subtitle (font-size 5 (con (b notes-subtitle) (br))) "")
        (font-size 3 (text-choice "Referencer" "References")) (p)
        (indent-pixels 10
         (brl
          (map book-text (glue-ref-and-hint (map formatter-function total-cross-ref-list))))))
      slide-background-color black black black
      (actual-stylesheets apply-css-styling?)
      (leno-css-class 'index)
    )
    (destination-path global-cross-reference-name))))


; ---------------------------------------------------------------------------------------------------------------

;;; Utility functions for interactive processing of lectures notes. 
;;; Here we provide for Scheme functions which are able to process one or more lecture notes. The functions
;;; laml-process-all and laml-process-all-1 are useful for programmatic processing of all lectures in a set of notes.<p>
;;; 
;;; Practical procedure:
;;; <ul>
;;;   <li> Start up and interactive LAML and Scheme session from the directory where the lecture notes sources are located, for instance
;;;        by calling M-x run-laml-interactively from Emacs. 
;;;   <li> Define the prepare-interactive-processing (see below) function in your scheme system by loading it from lecture-notes.scm. 
;;;   <li> Call the function prepare-interactive-processing (without parameters). This will load the necessary software for you.
;;;   <li> Finally call the function laml-process-all without parameters, or activate other utility functions such as 
;;;        (check-all-note-references) or (total-trail).
;;; </ul>

;; Load this function in order to prepare processing of lecture notes. This function loads the necessary software.
;; As a precondition, assume that this function is called from a context where LAML is started up, for instance
;; via run-laml-interactively via Emacs.
(define (prepare-interactive-processing)
 (let ((note-dir (startup-directory)))
   (load (string-append laml-dir "laml.scm"))
   (if (file-exists? (string-append note-dir "notes.scm"))
       (load (string-append note-dir "notes.scm"))
       (display-messag "Cannot find notes.scm")
   )
   (laml-style "lecture-notes/lecture-notes")
   (if (file-exists? (string-append note-dir "post-notes.scm"))
       (load (string-append note-dir "post-notes.scm"))
       (display-messag "Cannot find post-notes.scm")
   )))


;; Process a single lecture, identified with the symbol section-name, n times.
;; Recall that a lecture must be processed at least twice in order to settle cross references.
(define (laml-process section-name . n)
  ; proces the laml files n times
 (let ((n-times (if (null? n) 1 (car n))))
  (laml (string-append (as-string section-name) ".laml"))
  (if (> n-times 1)
      (laml-process section-name (- n-times 1)))))

;; Process all lectures in section list n times
(define (laml-process-list section-list . n)
  ; laml process all functions in section-list
  (let ((n-times (if (null? n) 1 (car n))))
    (map (lambda (s) (laml-process s n-times)) section-list)))

;; Process all lectures in lecture-list two times. 
;; This povides for a total regeneration given that lecture-list and current-lecture are
;; defined in the file notes.scm. The definition of these variables normally takes places in notes.scm
(define (laml-process-all)
 (laml-process-list (front-sublist lecture-list current-lecture) 2))

;; Process all lectures in lecture-list once.
;; This function is similar to laml-process-all, but it only process each lecture once.
(define (laml-process-all-1)
 (laml-process-list (front-sublist lecture-list current-lecture) 1))

; ---------------------------------------------------------------------------------------------------------------
;;; Utility for checking note cross references. 
;;; It is possible to make cross references from one note page to another via a cross-references clause.
;;; This utility checks that all cross references of type note-reference has a well-defined target in the current
;;; lecture notes. The check depends on the information in the aux files (with an entry pr. slide) and the crs files
;;; (with an entry pr. cross reference). This utility does not check internet-references. Informal references cannot
;;; by checked, or course.
;;; The top level function of this utility is check-all-note-references.
;;; We also list a set of other useful functions, which is used internally in check-all-note-references.
;;; The execuction of the check procedure is now integrated in LENO such that the checking is done during
;;; the processing of each lecture. Be aware that some forward references may result in unnecessary warnings.

;; Return a list of all pages on the form (lecture-id page-title-string page-id-symbol whether-section-or-not page-number-in-section).
;; Lecture-list is a list of lecture-ids (strings) to consider.
;; You can use this function on (map car lecture-sections) to get a complete list of slide pages.
(define (all-page-info lecture-list)
  (accumulate-right
    append
    '()
    (map (lambda (lecture)
           (let* ((aux-file (string-append note-source-directory "internal/" (as-string lecture) ".aux"))
                  (pages (if (file-exists? aux-file) 
                             (entries-of-aux-file (file-read aux-file))
                             '()
                         )
                  )
                 )
             (map (lambda (p) (cons (as-symbol lecture) p)) pages))
         )
         lecture-list)))

; check a single note reference on the form (note-reference "Title" lecture-id page-id), as found in -.crs files
; against the information in the aux files. The list of all information in the aux files must be passed
; as the second parameter to this function. The function all-page-info returns this information.
; Return either #t or #f
(define (check-a-note-reference note-ref aux-info)
  (let ((res (member-by-predicate note-ref aux-info
                                  (lambda (nr aux)
                                    (let ((nr-lecture (cross-ref-lecture-id nr))
                                          (nr-page (cross-ref-page-id nr)))
                                      (and (eq? nr-lecture (first aux)) (eq? nr-page (third aux))))))))
   (if res #t #f)))

; Check a specific lecture for cross reference problems regarding note-references.
; Return those entries in a crs files which cause problems (undefined references).
; Only note-references are handled in this version.
; The list of all information in the aux files must be passed as the second parameter to this function.
; The function all-page-info returns this information.
(define (check-crs-file lecture aux-info)
  (let* ((crs-contents (file-read (cross-ref-file-name lecture))) 
         (crs-contents-note-refs (filter (lambda (nr) (eq? 'note-reference (car nr))) crs-contents)))
    (filter
      (lambda (nr)
        (let ((check (check-a-note-reference nr aux-info)))
          (not check)))
      crs-contents-note-refs)))

; Check all lectures in lecture list of note-references, as done by check-crs-file
; Return a list of references which cause problems. Not used.
(define (check-crs-files lecture-list aux-info)
  (accumulate-right
    append
    '()
    (map (lambda (f) (check-crs-file f aux-info)) lecture-list)))

;; Check all note references in the lectures in lecture-sections.
;; Report on problems found on standard output.
;; This function can conveniently be called after the activation of laml-process-all.
;; See directions of use in the neighborhood of laml-process-all.
(define (check-all-note-references)
 (let* ((lectures (map car lecture-sections))
        (aux-info (all-page-info lectures)))
  (display "For each lecture, we return a list of unresolved note-references")
  (display #\newline)
  (for-each
    (lambda (lecture)
       (display (string-append (as-string lecture) ": "))
       (display (as-string (check-crs-file lecture aux-info)))
       (display #\newline) (display #\newline) )
    lectures)))


; ---------------------------------------------------------------------------------------------------------------

; Return text followed with keyboard shortcut information
(define (key-shortcut text shortcut-letter-string)
  (if java-scripting
      (string-append text " -- " (text-choice "Tastaturgenvej: " "Keyboard shortcut: ")
                     (string-it-single shortcut-letter-string))
      text))

; ---------------------------------------------------------------------------------------------------------------

; Help and about page:

(define (make-leno-help-page)
 (let ((kn-email "normark@cs.auc.dk")
       (kn-www "http://www.cs.auc.dk/~normark/")
       (down-shortcuts (font-size 2 "A number in the interval 1..9")))
  (write-text-file
    (page-1 
      "LENO Help Page"
      '()
      (con-par
       (help-page-header)

       (h 1 "The LENO Help and About page")

       (narrow p 750

         "The LENO system is made to support the authoring of lecture notes, organized
         in a number of chapters (or lectures). The top-level page refers to each lecture
         in the material. Each lecture, in turn, refers to a number of pages."
         
         "As an important characteristics of LENO, each page can be shown
         in at least three, and depending on the material, possibly four different views:"
   
         (ul-1 (list       
         "A slide view"
         "An annotated slide view"
         "An aggregated lecture view"
         "A thematic view"
         ))
         
         (con-space "In " (em "slide view") " the primary information on a page is shown using
         a large font, hereby making it attractive to present the information
         from a browser via a projector in an auditorium.")
         
         (con-space "In the " (em "annotated slide view") " the primary information is shown with
         a number of annotations, which are associated with the individual
         constituents of a slide page. As in slide view, a single page is shown
         in isolation from all other pages, but links to the previous and 
         the next page exists.")
         
         (con-space "An " (em "aggregated lecture view") " shows the primary slide information
         and the annotations of all pages in a lecture. Thus, in this view, the pages 
         of a lecture are all aggregated into a single 'lecture page'.
         The information contents of this view is the same as the sum of
         the information of the all annotated slides in a single lecture.")


         (con-space  "The " (em "thematic view") " typically shows most slide elements, including the annotations,
          with additional interleavning text. Typically, but not necessarily, a theme correspond to a section
          of slides. In the theme view of a material, elements such as programs, images, tables, and exercises are numbered
          hierarchically. Themes are not always present in a LENO-based material. The thematic view of the material
          may be available as PDF files via the 'printer icon'.
          It takes extra efforts by the autor to create a thematic view of the material. Use the yellow T icon to navigate to thematic
          expositions, if they are available.")

         (con-space "In addition, LENO supports generation of an automatically progressing slide show with the teacher's speaker sound. 
          For this to work, the author needs to record the speaker sound and prepare the sequencing and timing
          of the individual pages in the show.
          An automatically progressing slide show with speaker sound is started from the lecture overview page,
          with one of the small speaker icons or from
          one of the textually anchored links just above the note page table. The slide show brings you through selected slide pages, 
          programs, and image series. Notice that some of the slide pages my be visited more than once (such as before and
          after an external program page or image series - with different speaker sounds). The speaker sound can come from wav of mp3 files.
          If the material is located on a web server, the sound is streamed to 
          your browser, and currently it relies on RealPlayer from " (a-tag "http://www.real.com") ". For setup see for instance
          " (a-tag "http://video.auc.dk/" "Aalborg University's video pages") ".
         ")
         
         "The navigation icons used in LENO are presented in the table below:"

       )

       (table-3
         1
         (list 100 150 500)
         (list
          (map b (list "Icon" "Keyboard shortcut" "Explanation"))
          (list (img-LN "nav-up.gif" "") "u" "Go up one strutural level towards the top-level page") 
          (list (img-LN "nav-left.gif" "") "p" "Go to the previous page. The icon is also used to navigate
                                                from one lectures to the previous lecture.") 
          (list (img-LN "nav-right.gif" "") "n, CR" 
                                                 (string-append "Go to the next page. The icon is also used to navigate
                                                 from one lectures to the next lecture and other similar purposes."
                                                 (cond ((eq? mouse-advancement 'double-press) 
                                                          "This navigation can also be activated by a double click on the background (Internet Explorer only).")
                                                        ((eq? mouse-advancement 'single-press) 
                                                          "This navigation can also be activated by a single click on the background (Internet Explorer only).")
                                                        (else ""))
                                                 ) )
          (list (img-LN "todisk.gif" "") (em "no shortcut") "Go to a download page, which allows access to
                                             a zip file with all the files making up the set set of lectures.") 
          (list (img-LN "index.gif" "") (em "no shortcut") "Go to a page with an alphabetic index of the lecture notes
                                            covering all lectures.") 
          (list (img-LN "question.gif" "") (em "no shortcut") "Go to this help page") 
          (list (img-LN "nav-home.gif" "") (em "no shortcut") "Go to the page designated as the home page of these lecture notes.") 
          (list (img-LN "nav-aggr.gif" "") (em "no shortcut") "Go to particular page in the aggregated lecture view") 
          (list (img-LN "nav-notes.gif" "") "t" "Go to the annotated slide view from the slide view.") 
          (list (img-LN "nav-slide.gif" "") "t" "Go to the slide view from the annotated slide view.") 
;          (list (img-LN "arrow-down.gif" "") (em "no shortcut") "Go to the bottom of the current page.") 
;          (list (img-LN "arrow-up.gif" "") (em "no shortcut") "Go to the top of the current page.") 
          (list (img-LN  "exclamation.gif" "") (em "no shortcut") "Go to the readers comments on this page. Also allows you to post your own comments.
                                                                   This is a link to an annotation server which is external to LENO.") 
          (list (img-LN  "exclamation-red-ring.gif" "") (em "no shortcut") "As above - but there is at least one existing comment to this page.") 
          (list (img-LN  "no-frames.gif" "") (em "no shortcut") "If the current page is within a frame set, then break out from it. If not, nothing happens.") 


          (list (img-LN (program-icon-name 'english 'small) "") down-shortcuts "Go to a source program") 
          (list (img-LN (exercise-icon-name 'english 'small)  "") down-shortcuts "Go to an exercise") 
          (list (img-LN (image-series-icon-name 'english 'small) "") down-shortcuts "Go to an image series") 
          (list (img-LN (elucidator-icon-name 'english 'small) "") (em "no shortcut") "Go to an elucidator, which explains a program") 
          (list (img-LN (side-track-icon-name 'english 'small) "") (em "no shortcut") 
              "Go to a sidetrack, which is supposed to explain some special topic of the material. The side track is 
               presented in a frameset of its own, which at any time allows you to go back on the main track very easily.") 
          
          (list (img-LN "atomspin.gif" "") "x" "Symbolizes an automatic slide show, possibly with speaker sound.
                                                               By clicking the symbol the automatic slide show stops.") 
          (list (img-LN "atomspin-freeze.gif" "") "x" "Symbolizes a stopped slide show.
                                                                      By clicking this symbol the automatic slide with possible 
                                                                      speaker sound show starts.")
          (list (img-LN "nav-right-red.gif" "") "n" "Go to the next page in an automatically progressing slide show
                                                                    (with speaker sound). Accross a number of pages, 
                                                                     these arrow symbols trace the 'dynamic
                                                                     chain' of a slide show as opposed to the yellow arrow symbols
                                                                     that navigate the static page structures.")

          (list (img-LN "speaker.gif" "") "y" "Plays the speaker sound of a single slide page. You can navigate freely while
                                               listening to the speaker sound. If you start a new speaker sound, the already
                                               playing sound stops. If, however, you start an automatically progressing
                                               slide show with speaker sound, you will experience 'double sound'")
          (list (img-LN "small-speaker.gif" "") (em "no shortcut") "A symbol shown in a lecture index which links to a particular
                                                                    place in an automatic slide show.")

          (list (img-LN "nav-contents-frame.gif" "") "r" "Go to the table of contents of a trail") 
          (list (img-LN "nav-up-frame.gif" "") "q" "Go to the previous slides in a trail") 
          (list (img-LN "nav-current-frame.gif" "") (em "no shortcut") "Go to the current slide in a trail") 
          (list (img-LN "nav-down-frame.gif" "") "w" "Go to the next slide in a trail")

          (list (img-LN "side-track-back.gif" "") (em "no shortcut") "Go back on the main track from a sidetrack")

          (list (img-LN "nav-book.gif" "") "v" (con "Go to the corresponding, comprehensive textbook version of the material. 
                                                    This is also known as the thematic view.")) 
                                                
          (list (img-LN "letter-l.gif" "") (em "no shortcut") (con "Go to the " (b "L") "ecture overview from a theme overview"))
          (list (img-LN "print-icon.gif" "") (em "no shortcut") "Go to a page which gives access to PDF files") 

         )
       )

       (narrow p 750

         "The four red icons are used in the LENO trail facility. Using a LENO trail it is
          possible to define a sequence of slides from selected slides in a number of sections. The
          trail determines the selection and the sequence of the slides. This is useful if you want to present
          a subset of your slides for some purpose, and if you do not want to deal with multiple copies (which is usually a bad solution).
          The LENO trail facilities is realized via use of HTML frames. The red icons from above
          are all shown in a frame of its own (in the leftmost margin), the trail navigation frame. Notice that
          you can navigate freely in the slide trail while maintaining the context of the trail."
  
          "The keyboard shortcuts work in Internet Explorer 4, 5, 6 (from Microsoft), and in 
           Mozilla and Netscape 6."
  
          (con-space "The collection of HTML WWW pages, which constitute a set of lecture notes, are generated from an 
           XML-in-LAML source file, written in the programming language Scheme. Scheme is a language
           in the Lisp family of languages. The LENO surface syntax is generated from an XML Document Type Definition (DTD),
           and as such LENO is tightly connected to XML.
           The" (a-tag "http://www.cs.auc.dk/~normark/scheme/distribution/laml/styles/xml-in-laml/lecture-notes/man/lecture-notes.html" "LENO Reference Manual")
           "describes the elements and attributes of the LENO XML language. 
           The underlying system, called LAML
           (Lisp Abstracted Markup Language) is described in further detail in"
           (a-tag "http://www.cs.auc.dk/~normark/laml/" "the LAML home page") ".")

          (con-space "If you consider to make a LENO material yourself we will recommend that
           you start reading the" (a 'href "http://www.cs.auc.dk/~normark/scheme/tutorial/leno/leno.html" "LENO tutorial") ", "
           "which is part of the" (a-tag "http://www.cs.auc.dk/~normark/scheme/tutorial/index.html" "LAML Tutorial")". ")
    
           (con-space "Kurt Nørmark" (br) "Aalborg University" (br) kn-email (br) (a-tag kn-www))

      )

      )

      white black black black 
      #f ; (actual-stylesheets apply-css-styling?)
      (leno-css-class 'help)
   )
   (destination-path "leno-help-and-about"))))


(define (help-page-header)
 (header-banner
  (con
   (if front-index? 
            (a-tag (front-index-url) (img-LN "nav-up.gif" 
                                             (text-choice "Gå til lektionsliste" "Navigate to list of lectures")))
            (img-LN "nav-up-empty.gif" "")) (space 2)

   (a-tag course-home-url (img-LN "nav-home.gif" (text-choice "Kursets hjemmeside" "Course home"))) (space 2)


  )

 ""
 )

)

; ---------------------------------------------------------------------------------------------------
; Pre-scanning of a note page for programs, images, and exercises which requires down-links.

(define (down-links note-page-elements lecture-id page-id presentation-kind)
  (let ((down-elements (down-link-elements note-page-elements)))
    (map 
      (lambda (down-el)
        (let ((kind (car down-el))
              (num (cadr down-el)))
          (local-html-url
           (if (eq? kind 'images)
               (page-element-name lecture-id page-id presentation-kind kind num 1)
               (page-element-name lecture-id page-id presentation-kind kind num)))))
      down-elements)))


; Return a list of image/exercise/program elements found on the note page with note-page-elements.
; The elements of the result is of the form (kind number). Kind is one of the symbols program, images, and exercises.
; Number is a positive integer describing the number of kind on the notepage.
(define (down-link-elements note-page-elements)
 (add-down-link-seq-numbers
   (filter symbol?
    (map down-link-entry note-page-elements))))

(define (add-down-link-seq-numbers lst)
  (reverse (add-down-link-seq-numbers-1 lst 1 1 1 '())))

; Add sequence numbers to the program/exercise/images symbols.
; pn, en, and in are counters (next numbers)
(define (add-down-link-seq-numbers-1 lst pn en in res)
  (cond ((null? lst) res)
        ((eq? (car lst) 'program) (add-down-link-seq-numbers-1 (cdr lst) (+ 1 pn) en in (cons (list 'program pn) res)))
        ((eq? (car lst) 'exercise) (add-down-link-seq-numbers-1 (cdr lst) pn (+ 1 en) in (cons (list 'exercise en) res)))
        ((eq? (car lst) 'images) (add-down-link-seq-numbers-1 (cdr lst) pn en (+ in 1) (cons (list 'images in) res)))
        (else (error (string-append "add-down-link-seq-numbers-1: Unknown kind of tag: " (as-string (car lst)))))))

(define (down-link-entry note-page-element)
 (let ((kind (element-tag note-page-element))
       (contents (element-contents note-page-element))
      )
  (letrec ((external-program? (lambda (c) (eq? 'slide-external (car (fourth c)))))
           (external-images? (lambda (c) (eq? 'slide-external (car (third c)))))
          )
    (cond ((and (eq? kind 'source-program) (external-program? contents)) 'program)
          ((eq? kind 'exercise) 'exercise)
          ((and (eq? kind 'image-series) (external-images? contents)) 'images)
          (else #f)))))
    
; ---------------------------------------------------------------------------------------------------
; Exercise sheet:


(define (make-exercise-index lecture-exercise-list)
  ; write a page with a pr. lecture exercise index
  (write-text-file
    (page-1 
      (con lecture-title " " (con-space (text-choice "Oversigt over opgaver" "Exercise index")))
      '()
      (con 
        (exercise-index-header) (br)
        (h 1 
         (con-space 
           (text-choice "Opgaver" "Exercises") (br)
           lecture-title))

        (if (>= news-flash-level 4) (leno-news-flash 3 500) "")

        (a-tag (html-file (all-exercises-name lecture-id))
               (text-choice "Alle lektionen's opgaver på én side" "All exercises in this lecture on a single page"))
        (if (do-reveal-exercise-solutions?)
            (con (br) 
                 (a-tag (html-file (all-exercises-and-solutions-name lecture-id))
                                   (text-choice "Alle lektionen's opgaver og løsninger på én side"
                                                "All exercises and solutions in this lecture on a single page")))
            "")
             (p)

        (table-3
          1
          (if (do-reveal-exercise-solutions?)  (list 100 400 130)  (list 100 400))
          (map exercise-index-entry (reverse lecture-exercise-list)))

       (p)

       
     (if (and exercise-manager-support?)
         (if (<= (current-time) (lecture-exercise-reveal-time lecture-id))
           (let* ((tm (lecture-exercise-reveal-time lecture-id))
                  (day (weekday tm))
                  (dt (date-time tm))
                  (date (car dt))
                  (time (cadr dt))
                  )
             (font-1 3 red (string-append "Løsningerne til opgaverne i denne lektion vil tidligst blive gjort tilgængelige " day ", " date " klokken " time)))
           "")
         "")  (p)


       (when-generated)
      )
      slide-background-color black blue blue
      (actual-stylesheets apply-css-styling?)
      (leno-css-class 'index)
    )
   (destination-path  (exercise-index-name lecture-id))))

(define (exercise-index-entry entry)
  (let ((ex-id (first entry))
        (title (second entry))
        (page-id (third entry))
        (number-on-page (fourth entry))
        (number-in-lecture (sixth entry))
        (solution (seventh entry))
        (rank (eighth entry))
       )
   (let ((txt (b (string-append (text-choice "Opgave" "Exercise") " " (as-string lecture-number) "." (as-string number-in-lecture))))
         (ex-link (con (a-tag (string-append 
                           ((if exercise-manager-support? exercise-frame-name  exercise-name) lecture-id page-id the-primary-view number-on-page) ".html") title) " " (exercise-rank-presentation rank)))
         (sol-link (if (not (equal? "" solution))
                       (a-tag (html-file (exercise-solution-name lecture-id page-id number-on-page)) (text-choice "Løsning" "Solution"))
                       (em (font-size 2 (text-choice "Ingen tilgængelig løsning" "No solution available"))))))

   (if (do-reveal-exercise-solutions?)         
       (list txt ex-link sol-link)
       (list txt ex-link) ))))




; ---------------------------------------------------------------------------------------------------------------
; The overall exercise index

; Write a page with an overall exercise index, covering all lectures.
; Take the information from the -.exc files in the internal directory.
; The index covers from lecture one to lecture current-lecture.
; Kind is an optional parameter that control the kind of overall index to make.
; The symbol plain is the default value of the optional parameter.
; The symbol secret makes a secret overall exercise index, mainly for the teacher.
; The secret overall exercise index is found on the value of secret-overall-exercise-index-name.
(define (make-overall-exercise-index . optional-parameter-list)
 (let ((kind  (optional-parameter 1 optional-parameter-list 'plain)))
   (let ((external-exercise-entries (lecture-wide-exercise-list))
        )

    (write-text-file
     (page-1 
      (con lecture-title " " (con-space (text-choice "Oversigt over opgaver" "Exercise index")))
      '()
      (con 
       (overall-exercise-index-header) (br)
       (h 1 
          (con-space 
           (text-choice "Opgaver" "Exercises") (br)
           notes-title))

       (if (>= news-flash-level 4) (leno-news-flash 3 690) "")

       (table-3
        1
        (if (or (eq? kind 'secret) (and reveal-all-solutions? (do-reveal-exercise-solutions?))) 
            (list 190 100 400 130)
            (list 190 100 400))
        (map (overall-exercise-index-entry kind) external-exercise-entries))

       (p)

       (font-size 2
        (em
         (text-choice "Evt. løsninger kan findes via den lektionsspecifikke opgave side (første søjle)"
                      "Possible solutions can be found on the lecture specific exercise page (first column)"))) 

       (p)

       (when-generated)
       )
      slide-background-color black blue blue
      (actual-stylesheets apply-css-styling?)
      (leno-css-class 'index)
     )
     (destination-path 
       (cond ((eq? kind 'plain) overall-exercise-index-name)
             ((eq? kind 'secret) secret-overall-exercise-index-name)
             (else (error "make-overall-exercise-index: Erronous optional kind parameter."))))))))


(define (lecture-wide-exercise-list)
  (accumulate-right 
   append 
   '()
   (map (lambda (g) (if (file-exists? g) (file-read g) '()))
        (map (lambda (f) (exercise-idx-file-name f))
             (front-sublist lecture-list current-lecture)))))

; A page with all exercises in a lecture together.
; Good for printing
(define (make-all-exercise-page lecture-list header-danish-english entry-function file-function)
  (write-text-file
    (page-1 
      (con lecture-title " - " (con-space (text-choice "Alle opgaver" "All exercises")))
      '()
      (con 
        (all-exercise-page-header) (br)
        (h 1 
         (con-space 
           (apply text-choice header-danish-english) (br)
           lecture-title)) 
 
        (p) (hr-1) (p)

        (accumulate-right 
          string-append ""
          (map entry-function (reverse lecture-exercise-list)))

       (p)

       (when-generated)
      )
      slide-background-color black blue blue
      (actual-stylesheets apply-css-styling?)
      (leno-css-class 'note 'exercise)
    )
   (destination-path (file-function lecture-id))))

; A single exercise entry, for the aggregation of all exercises in a single page.
(define (exercise-formulation-entry entry)
 (let* ((ex-id (first entry))
        (title (second entry))
        (page-id (third entry))
        (number-on-page (fourth entry))
        (body (fifth entry))
        (number-in-lecture (sixth entry))
        (solution (seventh entry))
        (solution-url (if (not (equal? "" solution)) (exercise-solution-url lecture-id page-id number-on-page) ""))
        (rank (eighth entry))
       )
   (con (font-1 5 blue (b (con (as-string lecture-number) "." (as-string number-in-lecture) (horizontal-space 3) title " " (exercise-rank-presentation rank))))
        (vertical-space 1)
        (indent-pixels 5
          (font-1 3 black body))
        (vertical-space 1)
        (if (and (do-reveal-exercise-solutions?) (not (equal? solution-url ""))) (a-tag solution-url (text-choice "Løsning" "Solution")) "")
        (p) (hr-1) (p))))

; A single exercise entry, for the aggregation of all exercises together with solutions in a single page.
(define (exercise-formulation-solution-entry entry)
 (let* ((ex-id (first entry))
        (title (second entry))
        (page-id (third entry))
        (number-on-page (fourth entry))
        (body (fifth entry))
        (number-in-lecture (sixth entry))
        (solution (seventh entry))
        (rank (eighth entry))
       )
   (con (font-1 5 blue (b (con (as-string lecture-number) "." (as-string number-in-lecture) (horizontal-space 3) title " " (exercise-rank-presentation rank))))
        (vertical-space 1)
        (indent-pixels 5
          (con 
            (font-1 3 black body)))

        (if (not (equal? solution ""))
            (con
               (p)
               (font-1 4 blue (b (text-choice "Løsning" "Solution"))) (br)
               (indent-pixels 5
                  (font-1 3 red solution)))
            "")
        (p) (hr-1) (p))))

; Return a presentation function for the presentation of an exercise in the overall exercise table, covering all lectures.
; Kind is either the symbol plain or secret. In the latter case, always reveal solutions.
(define (overall-exercise-index-entry kind)
 (lambda (entry)
  (let ((ids (first entry))
        (title (second entry))
        (p-numbers (third entry))
        (solution-status (fourth entry))
        (rank (if (>= (length entry) 5) (fifth entry) #f)) 
       )
   (let* ((lec-id (first ids))
          (page-id (second ids))
          (sl-number (fourth ids))
          (main-number (car p-numbers))
          (main-number-1 (if main-number main-number 1))  ; in case lecture-number is set to #f
          (sub-number (cadr p-numbers)))
    (let ((txt (if (= sub-number 1) 
               (let ((lecture-title (list-ref lecture-titles (- main-number-1 1))))
                 (a-tag (html-file (exercise-index-name lec-id)) lecture-title))
               (horizontal-space 1)))
          (ex-num (string-append (text-choice "Opgave" "Exercise") " " (as-string (car p-numbers)) "." (as-string (cadr p-numbers))))
          (ex-titl-link (con (a-tag (html-file ((if exercise-manager-support? exercise-frame-name  exercise-name) lec-id page-id the-primary-view sl-number)) title) " " (exercise-rank-presentation rank)))
          (sol (if (eq? solution-status 'solution)
                   (a-tag (html-file (exercise-solution-name lec-id page-id sl-number)) (text-choice "Løsning" "Solution"))
                   (em (font-size 2 (text-choice "Ingen tilgængelig løsning" "No solution available"))))))
     (if (or (eq? kind 'secret) (and reveal-all-solutions? (do-reveal-exercise-solutions?))) 
         (list txt ex-num ex-titl-link sol)
         (list txt ex-num ex-titl-link)))))))


; ---------------------------------------------------------------------------------------------------------------
; External exercise index on exc files in the internal directory.
; Format: list of ((lecture-id page-id exercise-id number-on-slide) title (lecture-number number-in-lecture))
; The first part contains enough information to form an exercise-url via the function exercise-name.
; (Notice that exercise-id is not used - this is a little strange, but it is a consequence of the
; note page (sub)element naming scheme as implemented by the function page-element-name.

; Return the full path of an exc file for id. 
; This is the file which contains info about exercercises in a given lecture
; Id defaults to the value of the global varible lecture-id
(define (exercise-idx-file-name . id)
 (let ((id-1 (if (null? id) (as-string lecture-id) (as-string (first id)))))
   (string-append note-source-directory "internal/" id-1 "." "exc")))

; Defines the format of a single entry in the saved exercise list
(define (make-external-exercise-entry lecture-exercise-list-entry)
  (let ((ex-id (first lecture-exercise-list-entry))
        (title (second lecture-exercise-list-entry))
        (page-id (third lecture-exercise-list-entry))
        (ex-numb-on-slide (fourth lecture-exercise-list-entry))
        (body (fifth lecture-exercise-list-entry))
        (ex-numb-in-lecture (sixth lecture-exercise-list-entry))
        (solution (seventh lecture-exercise-list-entry))
        (rank (eighth lecture-exercise-list-entry))
       )
   (list (list (as-symbol lecture-id) (as-symbol page-id) (as-symbol ex-id) ex-numb-on-slide)
         title
         (list lecture-number ex-numb-in-lecture)
         (if (equal? "" solution) 'no-solution 'solution)
         rank)))


(define (save-exercise-info lecture-exercise-list)
 (let ((lecture-exercise-list-1 (map make-external-exercise-entry (reverse lecture-exercise-list))))
   (if (file-exists? (exercise-idx-file-name))
       (delete-file (exercise-idx-file-name)))

   (with-output-to-file (exercise-idx-file-name)
       (lambda () (write lecture-exercise-list-1)))))


; return the stored exercise info list (the list stored in a -.crs file).
(define (exercise-info-list lect-id)
  (let ((crs-file (exercise-idx-file-name lect-id)))
    (if (file-exists? crs-file)
        (file-read crs-file)
        '())))


(define (make-exercise-manager-control-center lecture-id)
  (write-text-file
    (page-1 
      (con lecture-title " - " "Exercise Manager Control")
      '()
      (con 
        (h 1 
          (con lecture-title (br) "Exercise Manager Control"))
 
        (p) (hr-1) (p)

       (a-tag (exercise-manager-url
                "exercise-companion-summary.cgi"
                (list 'lecture-id)
                (list lecture-id))
              "Statistik over øvelsesgang") (p)

       (a-tag (exercise-manager-url
                "exercise-companion-status.cgi"
                (list 'lecture-id)
                (list lecture-id))
              "Liste over alle indsendte bidrag") (p)

       (a-tag (exercise-manager-url
                "exercise-companion-status.cgi"
                (list 'lecture-id 'status-kind)
                (list lecture-id "problems"))
              "Liste over alle problemer i alle grupper") (p)

       (a-tag (exercise-manager-url
                "exercise-control-desk.cgi"
                (list 'lecture-id 'exercise-list)
                (list lecture-id  (list-to-string (map (compose as-string first) lecture-exercise-list) (as-string #\$)))
              )
              (b "Exercise Manager Control Desk")) (p)

       (p) (hr-1) (p)

       (when-generated)
      )
      slide-background-color black blue blue
      (actual-stylesheets apply-css-styling?)
      (leno-css-class 'index)
    )
   (destination-path (em-control-name lecture-id))
 ))

; ---------------------------------------------------------------------------------------------------
; Show and speak functions

; Return a list of speak clause specifications.
; lecture-id and page-id is the id of the page which holds the speak clause specification.
; next-page-url is the name of the next page in the show
(define (normalize-speak clause-contents lecture-id page-id next-page-name)
 (let* ((clause-contents-list
          (cond ((and show-and-speak? (one-number-list? clause-contents)) (list (list 'slide 1 (car clause-contents))))
                ((and show-and-speak? (list? clause-contents)) clause-contents)
                (else '())))
        (clause-names (map (lambda (cc) (speak-clause-spec-to-name lecture-id page-id cc)) clause-contents-list))
        (next-clause-names (shift-clause-names clause-names next-page-name))
       )
   (map2
     (lambda (clause-prefix next-name)
       (let ((kind (first clause-prefix))
             (number (second clause-prefix))
             (time (third clause-prefix))
            )
         (make-speak-clause 
             kind number
             (cond ((number? time) (+ time additional-showing-time))
                   ((list? time) (map (lambda (t) (+ t additional-showing-time)) time))
                   (else (laml-error "normalize-speak: time is expected to be either number of list of numbers: " time)))
             next-name
         )
        )
     )
     clause-contents-list next-clause-names)))

; Is x a list with a single number?
(define (one-number-list? x)
 (and (list? x) (= 1 (length x)) (number? (car x))))

; Clause names is a list of either names, or non-empty list of names (image series part names).
; Shift these names in a special way, which reflects the next name in the sequence.
(define (shift-clause-names clause-names final-name)
  (cond ((null? clause-names) '()) ; Is only used if clause-names is empty. Not recursion basis.
        ((and (null? (cdr clause-names)) (string? (car clause-names)))
             (list final-name))    ; @a hereafter: at least two elements
        ((and (null? (cdr clause-names)) (list? (car clause-names)))  ; @i hereafter: at least two elements
             (list (shift-clause-names (car clause-names) final-name)))  
        ((and (string? (car clause-names)) (string? (cadr clause-names))) ; @b
           (cons (cadr clause-names) (shift-clause-names (cdr clause-names) final-name)))
        ((and (string? (car clause-names)) (list? (cadr clause-names)) (not (null? (cadr clause-names)))) ; @c
           (cons (car (cadr clause-names)) (shift-clause-names (cdr clause-names) final-name)))
        ((and (string? (car clause-names)) (list? (cadr clause-names)) (null? (cadr clause-names)))
           (error "shift-clause-names: An empty list encountered, corresponding to empty image series."))
        ((and (list? (car clause-names)) (not (null? (car clause-names))) (string? (cadr clause-names))) ; @d
           (cons (shift-clause-names (car clause-names) (cadr clause-names))
                 (shift-clause-names (cdr clause-names) final-name)))
        ((and (list? (car  clause-names)) (not (null? (car  clause-names))) 
              (list? (cadr clause-names)) (not (null? (cadr clause-names)))) ; @e
           (cons (shift-clause-names (car clause-names) (car (cadr clause-names)))
                 (shift-clause-names (cdr clause-names) final-name)))
        ((and (list? (car  clause-names)) (list? (cadr clause-names)) 
              (or (null? (car clause-names)) (null? (cadr clause-names))))
           (error "shift-clause-names: An empty list encountered, corresponding to empty image series."))
        (else (error "shift-clause-names: Should not happen"))))

           

; Given a speak-clause specification return the name of the page which presents the page and the speak.
; In cause the clause is an image series, return a list of names of all the pages in the series.
(define (speak-clause-spec-to-name lecture-id page-id spec)
 (let ((kind (first spec))
       (number (second spec))
       (time (third spec))
      )
   (cond ((eq? kind 'slide-part) (show-and-speak-slide-name lecture-id page-id number))
         ((eq? kind 'program-part) (program-name lecture-id page-id 'show number))
         ((eq? kind 'exercise-part) (exercise-name lecture-id page-id 'show number))
         ((and (eq? kind 'image-series-part) (list? time))
            (map 
              (lambda (n)
                 (image-name lecture-id page-id 'show number n))
              (number-interval 1 (length time))))
         (else (error (string-append "Problems in speak-clause-spec-to-name: " (as-string kind) ", " (as-string number)
                                     ", " (as-string time)))))))
 
; Speak clause selector functions
(define speak-clause-kind (make-selector-function 2 "speak-clause-kind"))
(define speak-clause-number (make-selector-function 3 "speak-clause-number"))
(define speak-clause-time (make-selector-function 4 "speak-clause-time"))
(define speak-clause-next-name (make-selector-function 5 "speak-clause-next-url"))
(define speak-clause-sub-number (make-selector-function 6 "speak-clause-sub-number"))

; Speak clause contructor function.
; One optional parameter is supported, namely a sub-number of an image series (used for exploded specs only)
(define (make-speak-clause kind number time next-url . optional-parameters)
 (let ((sub-number (optional-parameter 1 optional-parameters #f)))
  (if sub-number
    (list 'speak-clause kind number time next-url sub-number)
    (list 'speak-clause kind number time next-url))))

; Return a speak selector function for part (higher order function).
(define (speak-select part)
  (lambda (speak-clause)
     (eq? (speak-clause-kind speak-clause) part)))

(define (speak-select-number part number)
  (lambda (speak-clause)
     (and 
        (eq? (speak-clause-kind speak-clause) part)
        (= (speak-clause-number speak-clause) number))))

(define (make-show-and-speak-slide slide-spec page-id elements)
 (let ((title-element (title-of-elements elements))
       (long-slide? (find-element 'long-slide elements))
       (element-keywords (extract-note-page-keywords elements))
       (clause-number (speak-clause-number slide-spec))
       (clause-time (speak-clause-time slide-spec))
       (clause-next-name (speak-clause-next-name slide-spec))
      )
  (reset-page-element-numbers)
  (if (and make-faked-show-and-speak-files? (not (zero-time-page? clause-time)))
      (make-faked-show-and-speak-file! (speak-name lecture-id page-id 'slide-part clause-number)))
  (write-text-file
   (show-and-speak-slide
    title-element
    (con
     (background-speaker-sound  (speak-name lecture-id page-id 'slide-part clause-number))
     (as-slide-text
      (apply con
             (map
              (lambda (e)
                (present-element
                 'slide-style
                 (element-tag e)
                 (element-contents e)
                 (element-attributes e)
                ) 
                )
              elements))))
    long-slide?
    element-keywords
    (down-links elements lecture-id page-id 'slide)
    (html-file (as-string clause-next-name))
    clause-time 
    )
   (destination-path (show-and-speak-slide-name lecture-id page-id clause-number)))))

; Make a show and speak LENO by means with Javascript if the variable java-scripting allows it.
(define (show-and-speak-slide title contents long? keywords-to-meta down-links next-link show-time)
 (let* ((meta-contribution (if (null? keywords-to-meta) 
                              '()
                              (list (list 'name "keywords" 
                                  'lang (meta-language language-preference)
                                  'content (comma-separated-string keywords-to-meta)))))
        (prev (if (previous-page) (previous-page) #f))
        (next (if (next-page) (next-page) #f))
        (speak-time-info (speak-playing-time show-time))
        (body (con 
                 (slide-start-marker)
                 (if slide-header-as-frame?
                    "" 
                    (cond ((or (eq? slide-header? 'normal) (eq? slide-header? 'minimal))
                               (con (slide-header 'show-and-speak next-link speak-time-info) (br)))
                          ((eq? slide-header? 'none) "")
                          ; ((eq? slide-header? 'minimal) (left-right-banner "" (minimal-slide-header)))
                          (else (error (string-append "Standard-slide: Unknown slide-header?: " (as-string slide-header?))))))
                 contents
                 (if slide-header-as-frame? "" (if long? 
                                                   (empty-slide-footer)     ; alternatively (long-slide-footer)
                                                   (empty-slide-footer)))
                 (slide-end-marker)))
        (body1 (if (> slide-page-offset 0) (indent-pixels slide-page-offset body) body))
       )
  (if java-scripting
   (timed-page-with-keypress-script
    (as-string title)
    show-time
    next-link
    meta-contribution
    body1

    javascript-loading
    (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
    (js-call "lenoNavigate" 
      (map string-it-single 
         (list ""
               next-link  ; n
               ""
               ""
               (html-file (slide-name lecture-id page-id))  ; x 
               "" ""
              )))

    slide-background-color black black black
    (actual-stylesheets apply-css-styling?)
    (leno-css-class 'slide)
   )
   (error "Use of show and speak slides requires that the variable java-scripting is turned on (value must be true).")
)))

(define (show-and-speak-program context-url title contents next-link show-time program-number)
 (let* ((speak-time-info (speak-playing-time show-time))
        (body  (con (program-header context-url #f #f #f 'slide (key-shortcut (text-choice "Tilbage til slide" "Back to slide") "u")
                                    next-link program-number speak-time-info)
                    (indent-pixels 10 (font-size slide-font-size contents)))))
    (if java-scripting
        (timed-page-with-keypress-script
         (as-string title)
         show-time
         next-link 
         '()
         body

         javascript-loading
         (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
	 (js-call "lenoNavigate" 
		  (map string-it-single 
		       (list ""
			     next-link  ; n
			     ""
			     ""
			     (html-file (program-name lecture-id page-id 'slide program-number)) ; x 
			     "" ""
			     )))

         slide-background-color black black black
         (actual-stylesheets apply-css-styling?)
         (leno-css-class 'slide 'program)
         )
      (error "show-and-speak-program: Use of show and speak slides requires that the variable java-scripting is turned on (value true).")
    )))

(define (show-and-speak-image-series context-url prev-url next-url header title image-file next-link show-time
                                     speak-file-name im-ser-number number-in-series total-number)
 (let* ((speak-time-info (speak-playing-time show-time))
        (body  (con (image-header context-url prev-url next-url next-link im-ser-number
                                  number-in-series speak-time-info total-number)
                    (vertical-space 1)
                    (indent-pixels 10 
                      (con (font-size slide-font-size title) (vertical-space 2)
                           (img-0 (string-append  "." "/" "graphics/" image-file)))))))
    (if java-scripting
      (timed-page-with-keypress-script
        (as-string header) 
        show-time
        next-link
        '()
        (con 
         (background-speaker-sound  speak-file-name)
         body
        )

        javascript-loading
        (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
	(js-call "lenoNavigate" 
		  (map string-it-single 
		       (list ""
			     next-link  ; n
			     ""
			     ""
			     (html-file (image-name lecture-id page-id 'slide im-ser-number number-in-series)) ; x 
			     "" ""
			     )))

        slide-background-color black black black
        (actual-stylesheets apply-css-styling?)
        (leno-css-class 'slide 'image-series)
      )
      (error "show-and-speak-image-series: Use of show and speak slides requires that the variable java-scripting is turned on (value true).")
     )))


(define (save-slide-image-show-and-speak img-file img-text dest-file-name context-url prev-url 
                                         next-url im-ser-number number-in-series total-number)
 (let ((image-series-show-and-speak-clause 
          (filter (speak-select-number 'image-series-part im-ser-number) current-show-and-speak-specification)))

   (if (and show-and-speak? (= 1 (length image-series-show-and-speak-clause)))
       (let* ((the-clause (first image-series-show-and-speak-clause))
              (clause-number (speak-clause-number the-clause))
              (clause-times (speak-clause-time the-clause))
              (clause-time (list-ref clause-times (- number-in-series 1)))
              (clause-next-names (speak-clause-next-name the-clause))
              (clause-next-name (list-ref clause-next-names (- number-in-series 1)))
             )
        (if (and make-faked-show-and-speak-files? (not (zero-time-page? clause-time)))
            (make-faked-show-and-speak-file! (speak-name lecture-id page-id 'image-series-part im-ser-number number-in-series)))

        (write-text-file
         (show-and-speak-image-series
          context-url prev-url next-url
          "Image series"
          img-text
          img-file
          (html-file clause-next-name)
          clause-time
          (speak-name lecture-id page-id 'image-series-part im-ser-number number-in-series)
          im-ser-number number-in-series total-number
          )
         (destination-path (image-name lecture-id page-id 'show clause-number number-in-series)))       
       )
   )
))

(define (end-show-and-speak-page)
  (page-1
     (string-append "End of " lecture-title)
     '()
     (con 
       (vertical-space 5)
       (center

           (font-size 5 (b (con (text-choice "Slut på " "End of ") lecture-title))) (p)

           (a-tag (html-file (index-name lecture-id)) (text-choice "Lektionsindeks" "Lecture index"))

       )
     )
     black white white white
     (actual-stylesheets apply-css-styling?)
     (leno-css-class 'slide)
  )
)

; Return an internal speak entry, to go on the internal -.spk file.
; lecture-id is the id of the lecture
; page-id is the page id of the relevant note page.
; kind is a symbol, one of slide-part, program-part, exercise-part, and image-series-part
; part is the part number 
; show-and-speak-name is the name of the show-and-speak HTML file (excluding extension).
; sound file name is the sound file (excluding extension) in the speak directory.
; explanation is a string explaing the role of the page
(define (make-internal-speak-entry lecture-id page-id kind part show-and-speak-name sound-file-name time explanation)
  (list 'internal-speak (as-symbol lecture-id) (as-symbol page-id) 
                        (as-symbol kind) (as-number part) 
                        (as-string show-and-speak-name) (as-string sound-file-name)
                        (as-number time) (as-string explanation)))

; Internal speak selectors:
(define internal-speak-lecture-id (make-selector-function 2 ""))
(define internal-speak-page-id (make-selector-function 3 ""))
(define internal-speak-kind (make-selector-function 4 ""))
(define internal-speak-part (make-selector-function 5 ""))
(define internal-speak-name (make-selector-function 6 ""))
(define internal-speak-sound (make-selector-function 7 ""))
(define internal-speak-time (make-selector-function 8 ""))
(define internal-speak-explanation (make-selector-function 9 ""))
(define internal-speak-source-file-number (make-selector-function 10 "internal-speak-source-file-number"))


(define (internal-show-and-speak-page internal-speak-list)
 (page-1 
   "Internal speak list"
   '()
   (con 
     (h 1 (con "Show and speak: " lecture-title))

     (general-show-and-speak-info) (p)

     (table-2 1
        (list 20 50 200 20 250 250)
        (make-list 6 white)
        (list "Nr." "Kind" "Title" "Time" "Show and speak page" "Sound file")
        (map2 present-internal-speak-entry internal-speak-list (number-interval 1 (length internal-speak-list)))
     )

     (p)

     (let* ((secs (sum-list (map internal-speak-time internal-speak-list)))
            (time-str (speak-playing-time secs))
           )
       (con (text-choice "Samlet spilletid: " "Total playing time: ") time-str))  (p)

    (when-generated)  

   )
   white black black black
   (actual-stylesheets apply-css-styling?)
   (leno-css-class 'index)
  )
)

; Present and entry in the internal speak file
(define (present-internal-speak-entry entry n)
 (let* ((lec-id (internal-speak-lecture-id entry))
        (pag-id (internal-speak-page-id entry))
        (kind (internal-speak-kind entry))
        (part (internal-speak-part entry))
        (name (internal-speak-name entry))
        (sound (internal-speak-sound entry))
        (time (internal-speak-time entry))
        (expl (internal-speak-explanation entry))
        (sound-exists? (if speak-file-prefix (file-exists-robust? (sound-file-path sound)) #f))
       )
    (list (as-string n)
          (font-size 2 
           (con
            (cond ((eq? kind 'program-part) "Pr")
                  ((eq? kind 'slide-part) (b "Sl"))
                  ((eq? kind 'image-series-part) "Is")
                  ((eq? kind 'exercise-part) "Ex")
                  (else "???"))
            " " (as-string part)))
          (html4:a (font-1 2 (if (and (eq? kind 'slide-part) (= 1 part)) red black) (as-string expl))
                   'href (html-file (slide-name lec-id pag-id)) 'css:text-decoration "none")
          (as-string time)
          (html4:a (font-size 2 name) 'href (html-file name) 'css:text-decoration "none")
          (html4:a (font-1 2 (if sound-exists? red black) sound) 'href (speak-url sound) 'css:text-decoration "none" ))))

; Given a spec, which is a normalized show-and-speak clause specification,
; add appropriate internal-speak-entries to the global variable speak-list-in-this-lecture
(define (add-to-internal-speak-entries spec)
 (let ((exploded-spec (internal-explosion spec)))
   (for-each
     (lambda (spec)
       (set! speak-list-in-this-lecture
         (cons 
           (let ((kind (speak-clause-kind spec))
                 (part (speak-clause-number spec))
                 (time (speak-clause-time spec)))
             (make-internal-speak-entry
                lecture-id page-id kind part 
                (cond ((eq? kind 'slide-part) (show-and-speak-slide-name lecture-id page-id part))
                      ((eq? kind 'program-part) (program-name lecture-id page-id 'show part))
                      ((eq? kind 'exercise-part) "???-not-implemented")
                      ((eq? kind 'image-series-part) (image-name lecture-id page-id 'show part (speak-clause-sub-number spec)))
                      (else "????"))
                (if (eq? kind 'image-series-part) 
                    (speak-name lecture-id page-id kind part (speak-clause-sub-number spec))
                    (speak-name lecture-id page-id kind part))
                time
                (cond ((eq? kind 'slide-part) page-title)
                      ((eq? kind 'program-part) (string-append "Program nummer " (as-string part) " paa " page-title))
                      ((eq? kind 'exercise-part) (string-append "Opgave nummer " (as-string part) " paa " page-title))
                      ((eq? kind 'image-series-part) 
                         (string-append "Billedserie nummer " (as-string part) 
                                        "(" (as-string (speak-clause-sub-number spec)) ")  paa " page-title))
                      (else "???"))))
           speak-list-in-this-lecture)))
      exploded-spec)))

; explode image series in spec-list, hereby flattening the specification
(define (internal-explosion spec-list)
 (flatten
  (map 
    (lambda (spec)
     (cond ((or (eq? (speak-clause-kind spec) 'slide-part)
               (eq? (speak-clause-kind spec) 'program-part)
               (eq? (speak-clause-kind spec) 'exercise-part))
             (list spec))
           ((eq? (speak-clause-kind spec) 'image-series-part)
             (let ((times (speak-clause-time spec)) ; a list of times for image series
                   (nexts (speak-clause-next-name spec)) ; also a list
                   (number (speak-clause-number spec))
                  )
               (map3 (lambda (time next sub-number)
                       (make-speak-clause 'image-series-part number time next sub-number))
                     times nexts (number-interval 1 (length times)))))
           (else (error "internal-explosion: Unknown kind of clause spec"))))
     spec-list)))


; sound name is the pure name of the sound file without prefix path and without extension            
(define (background-speaker-sound sound-name)
  (if (file-exists-robust? (sound-file-path sound-name))
      (itag1 'bgsound (list 'src (speak-url sound-name)))
      "") 
)

; sound name is the pure name of the sound file without prefix path and without extension            
; As a precondition, assume that sound-source is the symbol real-audio.
(define (background-speaker-sound sound-name)
  (if (file-exists-robust? (sound-file-path sound-name))
      (itag1 'embed
        (list 
          'console "Speaker sound"
          'src (speak-url sound-name)
          'controls "ControlPanel"
          'width "0"      ; invisible control - necessary due to interferrence with timing.
          'height "0"
          'type "audio/x-pn-realaudio-plugin"
          'nojava "true"
          'autostart "true"
        )
      )
      "")
)

; Return an informative string that reports on the show-and-speak settings:
(define (general-show-and-speak-info)
 (font-size 2
  (con 
    (b "default-showing-time: ") (as-string default-showing-time) " seconds" (br)
    (b "additional-showing-time: ") (as-string additional-showing-time) " seconds" (br)
    (b "sound-source: ") (as-string sound-source) (br)
    (b "speak-url-prefix: ") (as-string speak-url-prefix) (br)
    (b "speak-file-prefix: ") (as-string speak-file-prefix) (br)
  )))


; Make a sheet which helps the author keep track of the speaker input.
; The produced page is only of internal interest to the author of a LENO material. 
(define (make-show-and-speak-input-sheet)
 (let* ((body (con 
               (h 1 (con lecture-title))              
 
               (table-1 1
                        '(440 70 70 70 70 70)
                        (make-list 6 slide-background-color)
                        (cons (list "Overskrift" "Side" "Lydfil" "Tid" "Andet 1" "Andet 2")
                              (map2 
                               (lambda (e number) ; number not used...
                                 (let ((slide-title (car e))
                                       (slide-id (cadr e))
                                       (slide-section-start? (caddr e)))
                                   (list (con 
                                          (if slide-section-start?
                                              (font-color red (b slide-title))
                                            slide-title))
                                         (con (text-choice "Side " "Page ") (as-string number))
                                         (horizontal-space 1) (horizontal-space 1) (horizontal-space 1) (horizontal-space 1)

                                         )))

                               (reverse note-title-list) (number-interval 1 (length note-title-list))))) (p)

               (when-generated)
               )))
    (write-text-file
       (page-1
        (con lecture-title)
        '()
        body
        white black blue blue
        (actual-stylesheets apply-css-styling?)
        (leno-css-class 'index)
       )
     (destination-path (string-append (as-string lecture-id) "-show-and-speak-sheet")))))

; Return the speak-entry from the .spk file, if it exists,
; If no such entry can be located, return #f
; This function is not actually used - or at least should not be used given the present needs.
(define (previous-speak-entry lecture-id page-id)
  (if previous-speak-list-in-this-lecture 
      (find-in-list 
        (lambda (e) (and (eq? (as-symbol lecture-id) (as-symbol (internal-speak-lecture-id e))) 
                         (eq? (as-symbol page-id) (as-symbol (internal-speak-page-id e)))))
        previous-speak-list-in-this-lecture)
      #f))

; Is there show-and-speak for lecture-id?
(define (is-there-show-and-speak-in-lecture? lecture-id)
  (let ((prev-speak-filename (speak-file-name lecture-id)))
    (if (file-exists? prev-speak-filename)
        (let ((show-and-speak-structure (file-read prev-speak-filename)))
          (not (null? show-and-speak-structure)))
        #f)))


; Return a string of hours, minutes, and seconds given seconds
(define (speak-playing-time seconds)
  (let* ((ti (time-interval seconds))
         (h (fifth ti))
         (m (sixth ti))
         (s (seventh ti))
         (hour-string (lambda (h) (if (> h 1) (text-choice "timer" "hours") (text-choice "time" "hour"))))
         (minute-string (lambda (m) (if (> m 1) (text-choice "minutter" "minutes")(text-choice "minut" "minute"))))
         (second-string (lambda (s) (if (> s 1) (text-choice "sekunder" "seconds")(text-choice "sekund" "second"))))
        )
    (string-append
      (if (> h 0) 
          (string-append (as-string h) " " (hour-string h)    (if (or (> m 0) (> s 0)) ", " "") )
          "")
      
      (if (> m 0) 
          (string-append (as-string m) " " (minute-string m)       (if (> s 0) ", " "") )
          "")

      (if (> s 0) (string-append (as-string s) " " (second-string s)) ""))))

; Use the internal -.spk files to make a total list of all pages in all lectures.
; Similar to the value of the variable speak-list-in-this-lecture
(define (speak-list-of-all-lectures)
 (accumulate-right
   append
   '()
   (map
    (lambda (lecture-id)
        (let* ((spk-file (speak-file-name lecture-id))
               (spk-contents (if (file-exists? spk-file) (file-read spk-file) '()))
              )
      spk-contents))
    (front-sublist lecture-list current-lecture))))


; ---------------------------------------------------------------------------------------------------

; Show a logo img linked to logo-url. The logo image must be in the note-specific-image-files. (Now done automatically).
; In case an empty logo-url is specified, no logo will be shown.
; Thus, there is a strong coupling between logo-url and showing a logo image.
; If no logo is to be shown this procedure returns the empty string.
(define (logo-img . optional-parameter-list)
 (let* ((width (optional-parameter 1 optional-parameter-list #f))
        (img-glyph (if width (img-LN "logo.gif" "" width) (img-LN "logo.gif" "")))
       )
  (cond ((and (file-exists? (string-append note-source-directory "images/logo.gif"))
             (not (empty-string? logo-url)))
          (a-tag logo-url img-glyph))
        ((and (not (file-exists? (string-append note-source-directory "images/logo.gif")))
             (not (empty-string? logo-url)))
          (display-warning "No note-specific logo image in images/. No logo is generated.")
          ""
          )
        (else ""))))



; ---------------------------------------------------------------------------------------------------
; Lisp representation of entire note contents and theme contents

; Return the full path of the lsp file for id.
; A lsp file contains the the entire contents of the lecture notes as a datastructure.
; Id defaults to the value of the global varible lecture-id
(define (lsp-file-name . id)
 (let ((id-1 (if (null? id) (as-string lecture-id) (as-string (first id)))))
   (string-append note-source-directory "internal/" id-1 ".lsp")))

; Return the full path to the theme lsp file for lecture-id.
(define (theme-lsp-file-name lecture-id)
  (string-append note-source-directory "internal/" (theme-source-file-name (as-string lecture-id)) ".lsp"))


; ---------------------------------------------------------------------------------------------------
; Per Madsen:

(define (a-tag-per url anchor accesskey)
  (a 'href url 'accesskey accesskey anchor))

; ---------------------------------------------------------------------------------------------------
; Access to -.lsp representation of Lecture note slides:

; Return element, on the form as constructed by make-element, given
; lecture id, page id, element kind, element id, and further identification.
; The lecture id defaults to the current lecture-id if given as *.
; The page id defaults to the current page id if given as *
; The element kind must be one of the possible LENO element, such as point or items or item.
; The element id can be a symbol, which refers to the id of an element.
; Alternatively, the element id can be a number, say n, which refers to the n'th elemnet of
; the element kind on the page
; Alternatively, element-id must be * (meaning the first encountered in the search).
(define (lookup-element lect-id-0 pg-id-0 element-kind-0 element-id-0)
 (let ((lect-id (as-symbol lect-id-0))
       (pg-id (as-symbol pg-id-0))
       (element-kind (as-symbol element-kind-0))
       (element-id element-id-0)
      )
  (let ((final-lect-id (if (eq? '* lect-id) (as-symbol lecture-id)  lect-id))
        (final-page-id (if (eq? '* pg-id) (as-symbol page-id)  pg-id))
        (final-element-kind element-kind)
        (final-element-id element-id)
       )
    (let ((page-clauses (lookup-page-clause lect-id pg-id)))
      (if page-clauses
          (cond ((symbol? final-element-id) (lookup-element-in-sexpr-by-id page-clauses element-kind element-id))
                ((number? final-element-id) (lookup-element-in-sexpr-by-number page-clauses element-kind element-id))
                (else (laml-error "lookup-element: Unknown type of element-id. Either symbol or number expected: " final-element-id)))
          #f)))))

; Return the lisp representation of the page clause with lecture id lect-id and page id pg-id.
; Return #f if none such exists.
(define (lookup-page-clause lect-id pg-id)
  (let* ((lsp-name (lsp-file-name lect-id))
         (lecture-structure 
            (if (file-exists? lsp-name)
                (file-read lsp-name)
                '()))
         (page-structure (find-in-list (lambda (pg) (eq? (id-of-lisp-page pg) pg-id)) lecture-structure))
        )
    (if page-structure
        (elements-of-lisp-page page-structure)
        #f)))

; precondition: pages-clauses is a list of elements
; a raw Sexpr search for a suitable structure
; Find the first occurence of el-kind, matching el-id (which may be *, acting as wild card).
(define (lookup-element-in-sexpr-by-id sexpr el-kind el-id)
  (cond ((and (pair? sexpr) (leno-element? sexpr))
           (if (and (eq? (element-tag sexpr) el-kind) (if (eq? el-id '*) #t (eq? (element-id sexpr) el-id)))
               sexpr
               (let ((car-res (lookup-element-in-sexpr-by-id (car sexpr) el-kind el-id)))
                  (if car-res
                      car-res
                      (lookup-element-in-sexpr-by-id (cdr sexpr) el-kind el-id)))))
        ((pair? sexpr) 
               (let ((car-res (lookup-element-in-sexpr-by-id (car sexpr) el-kind el-id)))
                  (if car-res
                      car-res
                      (lookup-element-in-sexpr-by-id (cdr sexpr) el-kind el-id))))
        (else #f)))

; precondition: pages-clauses is a list of elements
; a raw Sexpr search for a suitable structure
; Find occurence n of el-kind in sepxr.
(define (lookup-element-in-sexpr-by-number sexpr el-kind number)
 (letrec
   ((n number)  ; state - decremented when encountering a match
    (lookup-element-in-sexpr-by-number-1 
     (lambda (sexpr el-kind number)
       (cond ((and (pair? sexpr) (leno-element? sexpr) (eq? (element-tag sexpr) el-kind) (= n 1))
	      sexpr)
	     ((and (pair? sexpr) (leno-element? sexpr) (eq? (element-tag sexpr) el-kind)  (> n 1)  )
              (begin
                (set! n (- n 1))
	        (let ((cdr-res (lookup-element-in-sexpr-by-number-1 (cdr sexpr) el-kind n)))
		  (if cdr-res
		      cdr-res
		      (lookup-element-in-sexpr-by-number-1 (car sexpr) el-kind n)))))
	     ((pair? sexpr) 
	      (let ((car-res (lookup-element-in-sexpr-by-number-1 (car sexpr) el-kind n)))
		(if car-res
		    car-res
		    (lookup-element-in-sexpr-by-number-1 (cdr sexpr) el-kind n))))
	     (else #f))))
    )
  (lookup-element-in-sexpr-by-number-1 sexpr el-kind n)))


; (define (lookup-element-in-sexpr-by-number-1 sexpr el-kind number)
;   (set! husk (cons sexpr husk))
;   (set! huskn (cons number huskn))
;   (cond ((and (pair? sexpr) (leno-element? sexpr) (eq? (element-tag sexpr) el-kind) (= number 1))
;            sexpr)
;         ((and (pair? sexpr) (leno-element? sexpr) (eq? (element-tag sexpr) el-kind)  (> number 1)  )
;            (let ((cdr-res (lookup-element-in-sexpr-by-number-1 (cdr sexpr) el-kind (- number 1))))
;                   (if cdr-res
;                       cdr-res
;                       (lookup-element-in-sexpr-by-number-1 (car sexpr) el-kind (- number 1)))))
;         ((pair? sexpr) 
;                (let ((car-res (lookup-element-in-sexpr-by-number-1 (car sexpr) el-kind number)))
;                   (if car-res
;                       car-res
;                       (lookup-element-in-sexpr-by-number-1 (cdr sexpr) el-kind number))))
;         (else #f)))

; Do we identify x as a leno element in the Lisp representation of a page.
(define (leno-element? x)
  (turn-into-boolean (and (and (list? x) (= 4 (length x)) (memq (as-symbol (car x)) possible-leno-elements)))))
  
; Return an association list of the sections (and thus, initial themes) of the lecture with id lecture-id.
; Take the information from the -.lsp file in the internal directory.
; Returns ((page-section-id-1 page-id-1-1 page-id-1-2 ... page-id-1-n) .... (page-section-id-m page-id-m-1 page-id-m-2 ... page-id-m-n)).
; In other words: A mapping of section lecture-ids to the list of pages in the section.
(define (leno-sections lect-id)
  (let* ((lsp-name (lsp-file-name lect-id))
         (lecture-structure 
            (if (file-exists? lsp-name)
                (file-read lsp-name)
                '()))
        )
    #f   ; NON FINISHED.
  )    
)

; ---------------------------------------------------------------------------------------------------
; IDAFUS support. Used in case distance-education-support? is #t


; Define an activity in IDAFUS, in the fixed unit idafus-unit-id, corresponding to the exicercise data passed as
; parameter to this procedure.
(define (make-idafus-activity! activity-id title formulation-text solution-text)
 (let* ((data-save-directory (string-append idafus-absolute-cgi-path "teacher-part" "/" "data" "/" (as-string idafus-unit-id) "/"))
        (existing-index (file-read (string-append data-save-directory "all.dat")))
        (cur-time (current-time))
        (dt-tm (date-time cur-time))
       )

  ; updata all.dat if necessary
  (if (not (member (as-string activity-id) existing-index))
      (file-append (string-append data-save-directory "all.dat") (as-string activity-id)))

  ; write the activity alist
  (file-write
    (propertylist-to-alist
      (list 
        'last-modifier idafus-teacher
        'date (car dt-tm)
        'time (cadr dt-tm)
        'second-count cur-time
        'unit-id (as-string idafus-unit-id)
        'activity-id (as-string activity-id)
        'prev-activity-id ""
        'next-activity-id ""
        'primary-activity "false"
        'start-date idafus-start-date
        'start-time ""
        'end-date idafus-end-date
        'end-time ""
        'teacher idafus-teacher
        'activity-title title
        'formulation-format "html"
        'formulation-location "here"
        'activity-formulation formulation-text
        'clarification-format "html"
        'clarification-location "here"
        'activity-clarification solution-text
        'top-level-contribution-types "contribution point-of-view question done"
        'non-top-level-contribution-types "contribution point-of-view question answer disagreement agreement"
        'html-base ""
        'activity-visibility "private"
        'group-members ""
        'password ""))
     (string-append data-save-directory (as-string activity-id))
  )))

; ---------------------------------------------------------------------------------------------------

; Return a news flash.
; The first optional parameter is font-size, and
; the second is a with of the box.
(define (leno-news-flash . optional-parameter-list)
 (let ((size (optional-parameter 1 optional-parameter-list 4))
       (width (optional-parameter 2 optional-parameter-list 800))
      )
  (if (> (string-length news-flash-string) 0)
      (con (p) 
           (table-1 1 (list width) (list yellow) 
                    (list (list 
                           (con (font-1 size black (span news-flash-string _ ". " 
                                 (if news-flash-url (span "See" (a 'href news-flash-url "here") _ ".") "")))))))
           (p))
      "")))


(define (file-exists-robust? f)
 (if (string? f) (file-exists? f) #f))



; ---------------------------------------------------------------------------------------------------
; Utility function for construction of an initial theme source file from an -.lsp file in the internals directory.
; Main function (make-theme-source-file lecture-id).

; A global variable in which to remember the internal theme lsp file contents.
; For use when generating an accurate secondary theme source.
(define existing-theme-lsp-contents #f)

; A global variable for counting the new theme elements relative to the existing theme elements, as represented in the internal file.
(define number-of-new-theme-elements 0)

; Theme element selectors. 
; For the actual purpose, theme elements are located in the internal theme lsp file.
(define theme-element-tag second)
(define theme-element-contents third)
(define theme-element-attributes fourth)

; Make a theme source file from lecture-id. Called by do-end-notes depending on the value of the attribute theme-source.
; theme-source-mode is either the symbol new, overwrite, delta or none.
; OVERWRITING IS POTENTIALLY RISKY, IF YOU HAVE MADE MANUAL CHANGES!
(define (make-theme-source-file lecture-id)
 (let* ((internal-leno-lsp-file-name (lsp-file-name lecture-id))
        (internal-theme-lsp-file-name (theme-lsp-file-name lecture-id))
        (leno-lsp-file-contents (file-read internal-leno-lsp-file-name))
        (theme-lsp-file-contents (if (file-exists? internal-theme-lsp-file-name) (file-read internal-theme-lsp-file-name) '()))
       )
  ; Remember theme-lsp-file-contents in a global variable. Used in make-theme.
  (set! existing-theme-lsp-contents theme-lsp-file-contents)

  (ensure-directory-existence! note-source-directory "theme-templates")
  (let*(
        (theme-file-contents (make-theme lecture-id leno-lsp-file-contents))
        (theme-divided-theme-file-contents (divide-into-themes theme-file-contents))
        (theme-file-name 
          (if (eq? theme-source-mode 'overwrite)
              (theme-source-file-name lecture-id "leno")
              (ensure-non-existing-file-in-dir (theme-source-file-name lecture-id "leno") (string-append note-source-directory "theme-templates/"))))
       )
   (write-text-file
     (string-append
       (read-text-file (string-append leno-software-directory "theme-templates/theme-prefix.scm"))
       (apply string-append (map pp-theme theme-divided-theme-file-contents))
       (read-text-file (string-append leno-software-directory "theme-templates/theme-suffix.scm"))
     )
     (string-append note-source-directory "theme-templates/" theme-file-name))
   (display-message (string-append theme-file-name " has been generated in the directory theme-templates"))
   (if (> number-of-new-theme-elements 0) (display-message "Number of new theme relevant leno elements:" number-of-new-theme-elements)) 

)))
              

(define (divide-into-themes theme-file-contents)
 (letrec ((page-id-of-section-title-clause
            (lambda (theme-list) 
              (fifth (car theme-list)))))
  (map 
   (lambda (theme-list)
     (append 
      (list 'theme 'id (page-id-of-section-title-clause theme-list))
      theme-list))
   (sublist-by-predicate 
    theme-file-contents
    (lambda (cur prev n)
      (equal? (seventh cur) "section-title"))))))

(define (pp-theme theme-list)
 (let ((CR (as-string #\newline))
       (SP " ")
       (quoted (lambda (x) (string-append "'" (as-string x))))
      )
  (string-append 
   "(" (as-string (first theme-list)) SP (quoted  (second theme-list)) SP (string-it (third theme-list)) CR
       (apply string-append 
         (map pp-leno-element (cdddr theme-list))) 
   ")" CR CR
  )))

(define (pp-leno-element leno-element-list)
 (let* ((element-tag (car leno-element-list))
        (output-tag (if (eq? element-tag 'new-leno-element) 'leno-element element-tag))
        (line-prefix (if (eq? element-tag 'new-leno-element) "**" "  "))
        (element-type (get-prop 'element-type (cdr leno-element-list)))
        (CR (as-string #\newline))
        (SP " ")
        (quoted (lambda (x) (string-append "'" (as-string x))))
        (pp-symbol-string-pair (lambda (p) (string-append (quoted (car p)) " " (string-it (cdr p)) " ")))

       )
  (string-append 
   (if (equal? element-type "title") CR "")
   line-prefix "(" (as-string output-tag) SP
       (apply string-append (map pp-symbol-string-pair (propertylist-to-alist (cdr leno-element-list)))) 
   ")" CR)))

; Return the proper name (without extension) of the theme source file for lecture-id.
; With an optional parameter, an extension can be added.
(define (theme-source-file-name lecture-id . optional-parameter-list)
 (let ((file-extension (optional-parameter 1 optional-parameter-list #f)))
   (string-append (as-string lecture-id) "_" "themes" (if file-extension (string-append "." file-extension) ""))))

(define (make-theme lecture-id internal-lsp-structure)
  (letrec ((make-theme-clauses
            (lambda (page-structure)
              (let ((page-id (id-of-lisp-page page-structure)))
                (flatten  ; mainly in order to unfold spliced contributions.
                 (map 
                  (lambda (element)
                    (let ((el-tag (element-tag element)))
                      (if (or (eq? el-tag 'splice-page-with) (eq? el-tag 'splice-page-without))
                          (let ((target-lecture-id (get 'lecture-id (element-attributes element)))
                                (target-page-id (get 'page-id (element-attributes element)))
                                (splice-contr-list (fetch-splice-contribution element)))
                            (map
                              (lambda (splice-contr)
                                 (make-leno-element-source-clause target-lecture-id target-page-id
                                                                  (element-tag splice-contr)
                                                                  (element-id splice-contr))
                              )
                              splice-contr-list)
                          )
                          (list (make-leno-element-source-clause lecture-id page-id (element-tag element) (element-id element)))
                          ))
                    )
                 (elements-of-lisp-page page-structure)))))))
    (flatten (map make-theme-clauses internal-lsp-structure))))

(define (make-leno-element-source-clause lecture-id page-id element-type element-id)
 (let* ((new-clause? (if (and (eq? theme-source-mode 'delta) element-id) 
                         (not (in-existing-theme? (as-string lecture-id) (as-string page-id) 
                                                  (as-string element-type) (as-string element-id)))
                         #f))  ; elements without element-id are never considered as new.
                               ; these are elements with an element-number attribute.
        (leno-element (if new-clause? 'new-leno-element 'leno-element))
       )
  (if new-clause? 
      (set! number-of-new-theme-elements (+ 1 number-of-new-theme-elements)))
  (if element-id
      `(,leno-element lecture-id ,(as-string lecture-id)
                     page-id ,(as-string page-id)
                     element-type ,(as-string element-type)
                     element-id ,(as-string element-id))

      `(,leno-element lecture-id ,(as-string lecture-id)
                     page-id ,(as-string page-id)
                     element-type ,(as-string element-type)
                     element-number "1")
      )))

; Is the theme leno-element with given lecture-id, page-id, element-type and element-id actually part of the
; latest processed theme file, as reflected by the internal theme lsp file?
; The global variable existing-theme-lsp-contents contains the lsp representation of the latest themes.
; The parameters are all strings.
(define (in-existing-theme? lecture-id page-id element-type element-id)
  (let* ((sectioned-elements existing-theme-lsp-contents))        ; just for naming and understanding purposes
    (lid-pid-eltype-elid-in-sections lecture-id page-id element-type element-id sectioned-elements)
  )
)

; Is lecture-id, page-id, element-type, and element-id in one of the elements in sectioned-elements.
; sectioned-elements corresponds to a section in the internal theme lsp file.
(define (lid-pid-eltype-elid-in-sections lecture-id page-id element-type element-id sectioned-elements)
  (if (null? sectioned-elements)
      #f
      (if (lid-pid-eltype-elid-in-single-section lecture-id page-id element-type element-id (car sectioned-elements))
          #t
          (lid-pid-eltype-elid-in-sections lecture-id page-id element-type element-id (cdr sectioned-elements)))))

; Is lecture-id, page-id, element-type, and element-id in one of the elements in the single section section-elements.
; section-elements corresponds to a single section of data in an internal theme lsp file. 
; Search the elements linearly for a match.
; Elements with element-number attributes never matches. 
(define (lid-pid-eltype-elid-in-single-section lecture-id page-id element-type element-id section-elements)
  (let ((elements (elements-of-lisp-page section-elements)))
    (find-in-list 
      (lambda (theme-el)
        (let ((el-tag (theme-element-tag theme-el))
              (attr   (theme-element-attributes theme-el)))
          (if (defaulted-get 'element-id attr #f)   ; there is an element-id, and not an element-number among the attributes
              (and (eq? el-tag 'leno-element)
                   (defaulted-get 'element-id attr #f)      
                   (equal? lecture-id (get 'lecture-id attr))
                   (equal? page-id    (get 'page-id attr))
                   (equal? element-type (get 'element-type attr))
                   (equal? element-id (get 'element-id attr)))
              #f)))
      elements)))


; Check for occurrences of elements with same tag and same id on a single page.
; This can cause problems for the LENO themes.
; Errors are fatal.
(define (check-ids! lsp-page)
 (let* ((elements (elements-of-lisp-page lsp-page))
        (elements-with-id (filter element-id elements))
        (problem-elements 
              (duplicates-by-predicate 
                  elements-with-id
                  (lambda (e1 e2)
                    (and (eq? (as-symbol (element-tag e1)) (as-symbol (element-tag e2)))
                         (eq? (as-symbol (element-id e1)) (as-symbol (element-id e2)))))))
       )
   (for-each 
     (lambda (problem)
       (laml-error
         (string-append "Duplicate (LENO tag, id) discovered: " 
                        (as-string (element-tag problem)) ", " (as-string  (element-id problem)))))
     problem-elements)))

; ---------------------------------------------------------------------------------------------------
; Theme stuff which is used already at LENO kernel level.

; Tells how the theme part of the material is generated.
; In reality, this is not relevant for the slide part of LENO.
; Not used in the LENO software as such, but can be used in LENO files to control variations which affect the theme files.   ...
; It is, however, handy to be able to make the distinction.
; Example: Which version of a source program to include.
; The normal value should be web (symbol).
(define presentation-medium-of-theme 'web)  ; assigned from front-matters attribute


(define (theme-name themes-id name-of-theme)
  (string-append (as-string themes-id) "-" (as-string name-of-theme)))

; Name of aggregated themes file - used for aggreation of themes within a lecture - for the paper version.
(define (aggregated-themes-name original-lecture-id)
  (string-append (as-string original-lecture-id) "_" "themes"))

(define (theme-url themes-id name-of-theme)
  (string-append (theme-name themes-id name-of-theme) ".html"))

(define (theme-name-of lecture-id)
  (string-append (as-string lecture-id) "_" "themes"))

; Return the proper name of the theme front matters page
(define (theme-front-matters-name)
  "theme-front-matter")


; Return an anchor name of an element, such that it can be referred from another 
; place in a theme
(define (theme-ref-anchor lecture-id page-id element-type element-id-or-number)
  (string-append (as-string lecture-id) "_" (as-string page-id) "_" (as-string element-type) "_" (as-string element-id-or-number)))

; Same on the basis if an attribute alist
(define (theme-ref-anchor-from-attributes attr-list)
 (let ((lec-id (get 'lecture-id attr-list))
       (pg-id (get 'page-id attr-list))
       (el-ty (get 'element-type attr-list))
       (el-id (defaulted-get 'element-id attr-list #f))
       (el-nu (defaulted-get 'element-number attr-list #f))
      )
   (theme-ref-anchor lec-id pg-id el-ty (if el-id el-id el-nu))))


; Return the theme URL of the lecture note page characterized by lecture-id page-id.
; At the internal level, search for the title element of (lecture-id, page-id) by looking
; through the internal -_themes.lsp files. In principles, the page may be present
; in any theme of the themes in the material. Most likely, the page is present in the
; themes collection corresponding to the lecture. Return an arbitrary theme URL if
; the page occurs more than once in the themes.
; Return #f if no URL can be located.
; The parameters lecture-id and page-id are assumed to be symbols.
(define (theme-url-of-lecture-id-and-page-id lecture-id page-id)
 (let* ((dest-anchor (theme-ref-anchor lecture-id page-id "title" 1))
        (theme-info (locate-theme-from-lecture-id-and-page-id lecture-id page-id))
        (themes-id (car theme-info))  ; the id of an entire themes file
        (id-of-theme (cdr theme-info)) ; the id of a single theme within a themes files
       )
   (if (and themes-id id-of-theme) 
       (string-append (html-file (theme-name themes-id id-of-theme)) "#" dest-anchor)
       #f)))


; Look through -_themes.lsp files for a particular note page.
; Return a cons pair of (themes-id, theme-id), or a pair of (#f . #f) if not found.
; The parameters lecture-id and page-id are - as a precondition - assumed to be symbols.
; As an optional paramter, pass the name of the theme file to look in.
(define (locate-theme-from-lecture-id-and-page-id lecture-id page-id)
 (let* ((most-relevant-theme-file (theme-name-of lecture-id))
        (likely-res (locate-theme-from-lecture-id-and-page-id-help lecture-id page-id most-relevant-theme-file))
      )
   (if likely-res
       likely-res
       (let* ((all-theme-file-names (map theme-name-of lecture-list))
              (res (global-locate-theme-from-lecture-id-and-page-id lecture-id page-id all-theme-file-names))
                          ; also redoes search already done...
            )
         (if res
             res
             (cons #f #f))))))
  

; As locate-theme-from-lecture-id-and-page-id.
; As an the tird parametrer, pass the name of the theme file to look in.
(define (locate-theme-from-lecture-id-and-page-id-help lecture-id page-id name-of-theme-file)
  (let* ((lfn (lsp-file-name name-of-theme-file))
         (most-likely-lsp-contents (if (file-exists? lfn) (file-read lfn)'()))
         (relevant-lsp-entry (find-title-element lecture-id page-id most-likely-lsp-contents))
        )
    (if relevant-lsp-entry
        (cons name-of-theme-file (id-of-lisp-page relevant-lsp-entry))
        #f)))

; Apply locate-theme-from-lecture-id-and-page-id-help on name-of-theme-file-lst, iteratively.
(define (global-locate-theme-from-lecture-id-and-page-id lecture-id page-id name-of-theme-file-lst)
  (cond ((null? name-of-theme-file-lst) #f)
        (else 
          (let ((res (locate-theme-from-lecture-id-and-page-id-help lecture-id page-id (first name-of-theme-file-lst))))
            (if res 
                res
                (global-locate-theme-from-lecture-id-and-page-id lecture-id page-id (cdr name-of-theme-file-lst)))))))



; Find a title element in lsp entries with for lecture-id and page-id (both symbols).
; Return the located element, or #f if not found.
(define (find-title-element lecture-id page-id lsp-entries)
  (find-in-list
   (lambda (lsp-entry)
    (find-in-list
     (lambda (element)
       (if (eq? (second element) 'leno-element) ; second!!!???
	   (let ((attr-list (element-attributes element)))
	     (and 
	      (or (equal? "title" (get 'element-type attr-list)) (equal? "section-title" (get 'element-type attr-list)))
	      (eq? lecture-id (as-symbol (get 'lecture-id attr-list)))
	      (eq? page-id (as-symbol (get 'page-id attr-list)))))
	   #f)
                   
       )
     (elements-of-lisp-page lsp-entry)
     ))
   lsp-entries))




; ---------------------------------------------------------------------------------------------------     
;;; ECIU requested note view navigator

; Make and return a context navigator intended for the note view.
; The context navigator is always for the current lecture.
; Include n entries (if possible) before and after the current one.
; The current page number is c.
; Use font-size f-size for the entries.
(define (eciu-note-view-context-navigation-table n c f-size)
 (let* ((aux-entries (entries-of-aux-file remembered-old-aux-structure))
	(mx (length aux-entries))
	(aux-entry-selection (list-part (max 1 (- c n)) (min mx (+ c n)) aux-entries))
	(cur-rec (if (< c mx) (list-ref remembered-old-aux-structure c) #f))
        (cur-id (if cur-rec (id-of-aux-entry cur-rec) #f))
	(link-array-list
	 (map
	  (lambda (e)
           (let ((te (title-of-aux-entry e))
                 (se? (section-start?-of-aux-entry e)))
             (a
	      (if (eq? cur-id (id-of-aux-entry e))
		  (b (font-1 f-size red (if se? (string-append te " [section]") te)))
		  (font-size f-size (if se? (string-append te " [section]") te)))
	      'href (html-file (note-name lecture-id (id-of-aux-entry e)))
	      'style "{text-decoration: none;}")))
	 aux-entry-selection))
       )
   (br-list link-array-list)
   ))


(define (eciu-note-view-context-selector current-page-id)
 (let* ((aux-entries (entries-of-aux-file remembered-old-aux-structure))
	(link-anchor-url-list
	 (map
	  (lambda (e)
           (let* ((section-start? (section-start?-of-aux-entry e))
                  (anchor (title-of-aux-entry e))
                  (anchor-1 (if section-start? (string-append "* " anchor) anchor))
                  (url (html-file (note-name lecture-id (id-of-aux-entry e))))
                  (select (equal? (id-of-aux-entry e) current-page-id))
                 )
             (list anchor-1 url select)))
	 aux-entries))
       )
    (select 'name "ContextSelector" 'size "1" 'css:font-size "70%"
      'onchange "if(options[selectedIndex].value) {location = options[selectedIndex].value}"
      (map 
       (lambda (anchor-url)
               (option (if (third anchor-url) (list 'selected "selected") '())
                       'value (second anchor-url) (first anchor-url)))
       link-anchor-url-list))))

  


(define (do-clean-leno-target-directory leno-target-dir)

  (display-message "Rinsing HTML target directory...")
  
  ; Delete HTML files in leno-target-dir
  (let ((target-files (directory-list leno-target-dir)))
    (for-each 
      (lambda (file)
        (let ((full-path-file (string-append leno-target-dir file)))
          (if (equal? "html" (file-name-extension file))
              (delete-file full-path-file))))
      target-files))

  ; Delete graphics files from target dir:
  (let* ((graphics-dir (string-append leno-target-dir "graphics/"))
         (target-files (directory-list graphics-dir))
        )
    (for-each 
      (lambda (file)
        (let ((full-path-file (string-append graphics-dir file)))
          (if (member (file-name-extension file) (list "gif" "jpg" "png" "swf" "svg"))
              (delete-file full-path-file))))
      target-files))


  ; Delete small graphics files:
  (let* ((graphics-dir (string-append leno-target-dir "graphics/small/"))
         (target-files (directory-list graphics-dir))
        )
    (for-each 
      (lambda (file)
        (let ((full-path-file (string-append graphics-dir file)))
          (if (member (file-name-extension file) (list "gif" "jpg" "png"))
              (delete-file full-path-file))))
      target-files))

  ; Delete small image files (LENO icons):
  (let* ((image-dir (string-append leno-target-dir "images/"))
         (target-files (directory-list image-dir))
        )
    (for-each 
      (lambda (file)
        (let ((full-path-file (string-append image-dir file)))
          (if (member (file-name-extension file) (list "gif" "jpg" "png"))
              (delete-file full-path-file))))
      target-files))

  ; lecturer-photos
  (let* ((image-dir (string-append leno-target-dir "lecturer-photos/"))
         (target-files (directory-list image-dir))
        )
    (for-each 
      (lambda (file)
        (let ((full-path-file (string-append image-dir file)))
          (if (member (file-name-extension file) (list "gif" "jpg" "png"))
              (delete-file full-path-file))))
      target-files))

  ; style sheets
  (let* ((style-dir (string-append leno-target-dir "stylesheets/"))
         (target-files (directory-list style-dir))
        )
    (for-each 
      (lambda (file)
        (let ((full-path-file (string-append style-dir file)))
          (if (member (file-name-extension file) (list "css"))
              (delete-file full-path-file))))
      target-files))

  (display-message "Rinsing DONE.")
  

)


(define (on-condition condition x)
 (if condition x ""))


(define (check-primary-view!)
 (cond ((eq? the-primary-view 'slide-view)
	  (if (not make-slide-view?) 
              (laml-error "When the primary view is  slide-view  the front-matters attribute slide-view must be true")))
       ((eq? the-primary-view 'annotated-slide-view)
	  (if (not make-annotated-slide-view?) 
              (laml-error "When the primary view is  annotated-slide-view  the front-matters attribute annotated-slide-view must be true")))
       ((eq? the-primary-view 'aggregated-view)
	  (if (not make-aggregated-view?) 
              (laml-error "When the primary view is  aggregated-view  the front-matters attribute aggregated-view must be true")))
       (else 'nothing))


 (if (and trail-of-lecture? (not make-slide-view?))
     (laml-error "When the front-matters attribute  trail-of-lecture  is true, slide-view must also be true"))

 (if (and show-and-speak? (not make-slide-view?))
     (laml-error "When the front-matters attribute  show-and-speak  is true, slide-view must also be true"))

 )

; ------------------------------------------------------------------------------------------------------------

(define (this-page-source-file-interval)
 (if show-and-speak-recording?
  (let ((source-number-interval
	 (number-interval
	  current-show-and-speak-count
	  (- (+ current-show-and-speak-count (show-and-speak-length current-show-and-speak-specification)) 1))))
    (string-append
     "Sound source file names: "
     (list-to-string (map as-string source-number-interval) ",")))
  ""))

; Counts the number of entries in a show-and-speak list, as referred by current-show-and-speak-specification
; The (only) tricky part is that image-series-part's should be counted in a special way.
(define (show-and-speak-length sap-specification-list)
 (let ((which-part (make-selector-function 2 "which-part"))
       (seconds (make-selector-function 4 "seconds"))
      )
  (cond ((null? sap-specification-list) 0)
        ((eq? 'image-series-part (which-part (car sap-specification-list)))
            (+ (length (seconds  (car sap-specification-list)))  
               (show-and-speak-length (cdr sap-specification-list))))
        (else (+ 1 (show-and-speak-length (cdr sap-specification-list)))))))



; ------------------------------------------------------------------------------------------------------------

; Make a faked show and speak file in speak-file-prefix directory.
; Be careful not to overwrite real sound files.
; name is the proper name of a show and speak file, without path and without extension.
; Controlled by the variable make-faked-show-and-speak-files?
(define (make-faked-show-and-speak-file! name)
  (let ((faked-sound-path (string-append note-source-directory speak-file-prefix name "." (sound-source-extension))))
    (if #t
        (write-text-file
           "faked-sound"
           faked-sound-path))))

; A zero-time-page has additional-showing-time as show time, because of show-and-speak normalization
(define (zero-time-page? clause-time)
  (= (as-number clause-time) (as-number additional-showing-time)))

  
; ------------------------------------------------------------------------------------------------------------

(define (title-author-logo-strip title subtitle author-info date number)
  (table-3 0
	   (list 752 200)
	   (list
	    (list
	     (con 
                 (if number (em (font-size 2 (con (text-choice "LEKTION" "LECTURE") " " (as-string number) ":" (br)))) "")
		 (font-size 6 (b title))
                 (if subtitle (br) "")
                 (if subtitle (font-size 5 (b subtitle)) "") (p)

		 (font-size 4 (b (list-to-string
		  (map (lambda (e) (con e (br)))  (append author-info (list date)) )
		  ""
		  ))))

	     (logo-img)))
	   "middle"))



; Return the presentation of param-list
; param-list a list of cons pairs with name/value strings.
(define (present-applet-parameters param-list)
  (accumulate-right 
    string-append ""
    (map 
     (lambda (par)
      (let ((name (car par))
            (val (cdr par)))
       (param 'name (as-string name) 'value (as-string val))))
     param-list)))


; ---------------------------------------------------------------------------------------------------
; Print page

(define (make-a-print-page!)
  (write-text-file
    (page-1
        "LENO Print page"
        '()

        (con 

          (print-page-header) (br)

          (font-size 6 (b  notes-title)) (br)  (font-size 5  (b "PDF print page")) (vertical-space 1)

          (if pdf-version-as-of
              (let* ((updated-date-list (year-month-day-decode-string pdf-version-as-of))
                     (updated-time (time-encode (first updated-date-list) (second updated-date-list) (third updated-date-list) 0 0 0))
                     (dt (date-time updated-time)))
                 (p (b (text-choice "PDF version fra " "PDF version as of ") (weekday updated-time) _ "," (car dt) _ ".")))
              "")

          (p (em (text-choice 
            "Fra denne side er der adgang til PDF filer af udvalgte dele af materialet."
            "From this page there is access to PDF files of selected parts of the material.")))


          (indent-pixels 10
            (if (>= (length lecture-sections) 1)
		(table-2 1
			 (list 400 150 150)
			 (make-list 3 white)
			 (list (text-choice "Lektion" "Lecture") (text-choice "Normal størrelse" "Normal size") (text-choice "Reduceret størrelse" "Reduced size"))
                         
                         (append 
                           (list 
                             (present-print-entry #f "all" (b (em (text-choice "Hele materialet" "The complete material"))))
                             (present-print-entry #f "front-matter" (text-choice "Titelark" "Front matters"))
                           )
   			   (map 
                             (lambda (ls n) (present-print-entry n (first ls) (second ls))) 
                             lecture-sections (number-interval 1 (length lecture-sections))))

			 )
		"???"))

         (p)

          (table-3 0 (list 180 "*") 
             (list (list  (laml-power-icon "") (when-generated))))

        )


        white black blue blue
        (actual-stylesheets apply-css-styling?)
        (leno-css-class 'index)
       )
    (destination-path (print-page-name))))


(define (present-print-entry n id title)
  (list (string-append (if n (string-append (as-string n) ". ") "") title)
        (normal-size-pdf-link id)
        (reduced-size-pdf-link id)))

(define (normal-size-pdf-link lecture-id)
  (let ((pdf-name (pdf-file (print-name lecture-id)))
        (rel-path (relative-html-destination-pdf-path-fragment)))
    (if (file-exists?  (string-append note-source-directory (relative-source-html-destination-path-fragment) rel-path pdf-name))
        (a 'href (string-append rel-path pdf-name) pdf-name)
        "-")))

(define (reduced-size-pdf-link lecture-id)
  (let ((pdf-name (pdf-file (print-name lecture-id)))
        (rel-path (string-append (relative-html-destination-pdf-path-fragment) "small/")))
    (if (file-exists? (string-append note-source-directory (relative-source-html-destination-path-fragment) rel-path pdf-name))
        (a 'href (string-append rel-path pdf-name) pdf-name)
        "-")))

(define (print-page-header)
 (header-banner
  (con
   (if front-index? 
       (if themes?
           (a-tag (html-file top-level-theme-index-file-name) (img-LN "nav-up.gif" (text-choice "Gå til tematisk indeks" "Navigate to theme index")) )
	   (a-tag (front-index-url) (img-LN "nav-up.gif" (text-choice "Gå til lektionsliste" "Navigate to list of lectures"))))
       (img-LN "nav-up-empty.gif" ""))
   (space 2)

   (a-tag course-home-url (img-LN "nav-home.gif" (text-choice "Kursets hjemmeside" "Course home"))) (space 2)

  )

 (a-tag about-laml-url (img-LN "question.gif" (text-choice "Hjælp om disse noter" "Help page about these notes")))
 )

)

; ---------------------------------------------------------------------------------------------------------------
; Generation of a secondary trail source file

(define (trail-source-file-name name extension)
  (string-append name "." extension))

; Generate a secondary trail source file of list-of-lectures on file-name (without initial path, and without extension).
(define (make-secondary-trail-source-file list-of-lectures file-name) 
 (let* ((trail-file-name 
          (cond ((eq? trail-source-mode 'overwrite)
                  (trail-source-file-name file-name "leno"))
                ((eq? trail-source-mode 'new)
                  (ensure-non-existing-file-in-dir (trail-source-file-name file-name "leno") note-source-directory))
                ((eq? trail-source-mode 'none) #f)
                (else (laml-error "trail-file-name: The value of trail-source-mode must be either overwrite, new, or none. Actual value:" trail-source-mode))))
       )
   (write-text-file
     (con 
       (read-text-file (string-append leno-software-directory "trail-templates/trail-prefix.scm"))
       (total-trail-xml-in-laml list-of-lectures)
       (read-text-file (string-append leno-software-directory "trail-templates/trail-suffix.scm"))
     )
     (string-append note-source-directory trail-file-name) )
   (if (>= lecture-note-verbose-mode 1)
       (display-message (string-append "A trail has been generated in the file  " trail-file-name )))))


; ---------------------------------------------------------------------------------------------------------------
; Front matter info

(define (make-front-matter-info)
  (pair-up
    '(notes-title notes-subtitle note-abstract front-title front-subtitle author affiliation abstract course-home-url author-home-url note-download-url 
      note-contents-description slide-header trail-of-lecture
      language word-index apply-css-styling logo-url news-flash-string news-flash-level quiz-support lecture-note-verbosity-level lecture-type theme-source theme-auto-process trail-source
      make-print-page slide-view annotated-slide-view aggregated-view theme-view primary-view source-destination-delta html-pdf-delta used-exercise-model)
    (list notes-title notes-subtitle note-abstract lecture-title notes-subtitle (car lecture-author-info) (cdr lecture-author-info) lecture-abstract course-home-url author-home-url note-download-url 
          note-contents-description slide-header? trail-of-lecture?
          language-preference word-index? apply-css-styling? logo-url news-flash-string news-flash-level quiz-support? lecture-note-verbose-mode lecture-type theme-source-mode
          theme-auto-process-mode trail-source-mode
          make-print-page? make-slide-view? make-annotated-slide-view? make-aggregated-view? themes? the-primary-view source-destination-delta-path-fragment html-destination-pdf-path-fragment
          used-exercise-model)))


; ---------------------------------------------------------------------------------------------------------------
; Source program index stuff.

; Source program descriptor - constructor and selectors:
(define (make-source-file-descriptor program-file-name initial-sentence kind page-id)
  (list 'source-file-descriptor program-file-name initial-sentence kind page-id))

(define program-file-name-of-source-file-descriptor (make-selector-function 2 "program-file-name-of-source-file-descriptor"))
(define title-of-source-file-descriptor (make-selector-function 3 "title-of-source-file-descriptor"))
(define kind-of-source-file-descriptor (make-selector-function 4 "kind-of-source-file-descriptor"))
(define page-id-of-source-file-descriptor (make-selector-function 5 "page-id-of-source-file-descriptor"))


; Top level source program index procedure.
; New as Feb 1, 2007. I have eliminated long encoded file names. This version creates a deep directory structure within the HTML target dir.
(define (make-source-program-index! lecture-id list-of-source-descriptors)
 (letrec ((program-file-names-equal? (lambda (d1 d2) (equal? (program-file-name-of-source-file-descriptor d1) (program-file-name-of-source-file-descriptor d2)))))
  (let* ((unique-list-of-source-descriptors (remove-duplicates-by-predicate (reverse list-of-source-descriptors) program-file-names-equal?))
	 (unique-list-of-source-files (map program-file-name-of-source-file-descriptor unique-list-of-source-descriptors))
	 (unique-list-of-titles (map title-of-source-file-descriptor unique-list-of-source-descriptors))
        )
   
    ; Make sure that the lecture specific directory for source file exists:
    (ensure-directory-existence! (string-append note-source-directory (relative-source-html-destination-path-fragment) "source-programs/") (as-string lecture-id))

    ; Copy source files into the HTML target directory - categorized after lecture-id.
    ; All source programs - independent of kind - are copied. This includes listings.
    (for-each 
     (lambda (file)
       (let* ((absolute-source-file (if (absolute-file-path? file) file (in-startup-directory file)))
              (max-relative-source-file (but-prefix-part-of-absolute-path absolute-source-file))  ; drop initial absolute path stuff.

              (initial-relative-path (file-name-initial-path max-relative-source-file))
              (file-name-without-extension (file-name-proper max-relative-source-file))
              (file-name-ext (file-name-extension max-relative-source-file))
              (absolute-destination-file (string-append note-source-directory (relative-source-html-destination-path-fragment) "source-programs/"  (as-string lecture-id) "/"
                                                        initial-relative-path file-name-without-extension "." "txt")))
         (ensure-directory-path-existence! (normalize-absolute-file-path (string-append note-source-directory (relative-source-html-destination-path-fragment) "source-programs/"  (as-string lecture-id)))
                                            initial-relative-path)
         (if (file-exists? absolute-destination-file)
             (delete-file absolute-destination-file))
         (copy-program-file absolute-source-file absolute-destination-file)))
     unique-list-of-source-files)


   ; Make the source program index page:
   (letrec ((present-file-name 
	     (lambda (name ext) 
	       (if (empty-string? ext) name (string-append name "." ext))))

	    (relevant-for-index? (lambda (source-file-descriptor)
				   (eq? (kind-of-source-file-descriptor source-file-descriptor) 'source-program)))
           
           )
     ; redefine the three unique-... variables with a more restrictive filtering.
     ; only entries of kind source-program should go into the index.
     ; entries of kind listing should not.
     (let* ((unique-list-of-source-descriptors (remove-duplicates-by-predicate (reverse (filter relevant-for-index? list-of-source-descriptors)) program-file-names-equal?))
	    (unique-list-of-source-files (map program-file-name-of-source-file-descriptor unique-list-of-source-descriptors))
	    (unique-list-of-titles (map title-of-source-file-descriptor unique-list-of-source-descriptors))
            (unique-list-of-page-ids (map page-id-of-source-file-descriptor unique-list-of-source-descriptors))
	   )    
       (make-source-program-index-page!
	lecture-id 
        unique-list-of-source-files
	(map (lambda(path) (present-file-name (file-name-proper path) (file-name-extension path))) unique-list-of-source-files)  ; Before Oct 25, 2007
	(map (lambda(path) (some-suffix-of path)) unique-list-of-source-files)

	(map (lambda(path) 
               (let* ((absolute-path (if (absolute-file-path? path) path (in-startup-directory path)))
                      (max-rel-path (but-prefix-part-of-absolute-path absolute-path)))
		 max-rel-path))
             unique-list-of-source-files)
	unique-list-of-titles
        unique-list-of-page-ids
	)))

    )
 )
)

; A variant of copy-file which eliminates leno doc-comment
; if extract-and-present-end-of-line-doc-comments is true.
(define (copy-program-file source-path target-path)
  (let ((source-prog-text (read-text-file source-path)))
    (write-text-file
       (if extract-and-present-end-of-line-doc-comments
           (extract-program-source source-prog-text)
           source-prog-text) 
       target-path)))

; Encode the full file path into a proper file name.
(define (encode-file-path-in-file-name file-path)
  (transliterate (transliterate file-path #\/ "$") #\: "$"))

; Old. Redefined next
;(define (source-program-url lecture-id encoded-name-ext)
;  (string-append "source-programs/" (as-string lecture-id) "/" (file-name-proper encoded-name-ext) "." "txt"))

(define (source-program-url lecture-id suffix-part-without-extension)
  (string-append "source-programs/" (as-string lecture-id) "/" suffix-part-without-extension "." "txt"))

; 2006
; 2007: Must be corrected:
; A utility function which can be used to refer to source programs given lecture-id, page-id, and source-program-id.
(define (url-of-source-program lecture-id page-id source-program-id)
  (let* ((source-program-clause (lookup-element lecture-id page-id "source-program" (as-symbol source-program-id))))
    (if source-program-clause
        (let* ((full-source-file-path (first (third source-program-clause)))      ; VERY bad programming!
               (abs-file-path (if (absolute-file-path? full-source-file-path) full-source-file-path (in-startup-directory full-source-file-path)))
              )
          (source-program-url lecture-id (file-name-sans-extension (but-prefix-part-of-absolute-path abs-file-path))))
        "???")))


; Return a single entry in the source program index table.
; original-path is the original path to the program source, in authors source directory. 
(define (source-program-index-entry original-path short-file-name-ext full-file-name-ext suffix-path title lecture-id page-id)
 (let ((corresponding-theme-url (theme-url-of-lecture-id-and-page-id (as-symbol lecture-id) (as-symbol page-id)))
       (is-program-annotated? (if extract-and-present-end-of-line-doc-comments
                                  (turn-into-boolean 
                                    (substring-index
                                      (read-text-file  original-path)  0 end-of-line-doc-comment-prefix))
                                  #f))
      )
  (list title
        (html4:a short-file-name-ext 'href (source-program-url lecture-id (file-name-sans-extension suffix-path)) 'title full-file-name-ext)
        (html4:a "Slide context" 'href (html-file (slide-name lecture-id page-id)))
        (if corresponding-theme-url (html4:a "Text book context" 'href corresponding-theme-url) "-")
        (if is-program-annotated? (html4:a "Note context" 'href (html-file (note-name lecture-id page-id))) "-")
  )))

           

; Make the source program index page for lecture-id. 
; short-file-name-ext-list is a list of file names (without path, but with the original extension).
; file-name-ext-list is a list of file possible longer file names
(define (make-source-program-index-page! lecture-id original-source-file-path 
                                         short-file-name-ext-list file-name-ext-list encoded-file-name-list title-list page-id-list)
  (write-text-file
   (let ((prev (if previous-lecture (contents-url (string-append (as-string previous-lecture) "-" "source-programs")) ""))
         (next (if next-lecture (contents-url (string-append (as-string next-lecture) "-" "source-programs")) ""))
         (up (html-file (index-name lecture-id)))
         (down-links '())
        )
    (page-with-keypress-script
      (con lecture-title " " (con-space (text-choice "Kildeprogrammer" "Source Programs")))
      '()
      (con 
        (source-program-index-header lecture-id) (br)
        (h 1 
         (con-space 
           (text-choice "Kildeprogrammer" "Source Programs") (br)
           lecture-title))

        (if (>= news-flash-level 4) (leno-news-flash 3 500) "")

        (p (font-size 2 (em "The Note Context in the rightmost column is only shown in case an annotated program exists. -
                             You can navigate to the annotated program via the annotated slide view (= 'the note context').")))

        (table-3
          1
          (list 370 250 110 150 130)
          (map (lambda (original-path short-file-name-ext file-name-ext suffix-path-with-extension title page-id)
                 (source-program-index-entry original-path short-file-name-ext file-name-ext
                                             suffix-path-with-extension title lecture-id page-id))
                original-source-file-path short-file-name-ext-list file-name-ext-list encoded-file-name-list title-list page-id-list))

       (p)

       (when-generated)
      )

      javascript-loading
      (list (string-append leno-software-directory "javascript/") "lenonavigate.js")
      (js-call "lenoNavigate"   ;@b
                   (append
                     (map string-it-single 
                          (list prev
                                next 
                                up
                                ""
                                "" "" ""))
                     (list (js-string-array down-links))))


      slide-background-color black blue blue
      mouse-advancement
      (actual-stylesheets apply-css-styling?)
      (leno-css-class 'index)
    ))
   (destination-path  (source-program-index-name lecture-id))))
       

; ---------------------------------------------------------------------------------------------------------------
; Administration of separate page elements - note page items such as programs and exercises which are linked to on
; Separate pages.

; Is the element el rendered as a separate element, on its own page, like external programs and exercises.
(define (separate-page-element? el)
  (or (external-program-element? el) (eq? (element-tag el) 'exercise)))

; Is the el an external source program element.
(define (external-program-element? el)
 (let ((attribute-list (element-attributes el)))
  (if (as-boolean (defaulted-get 'drop attribute-list #f)) ; a dropeed element
      #f
      (and (eq? (element-tag el) 'source-program)
           (let* ((contents (element-contents el))
                  (how-to-show (fourth contents)))
             (eq? (car how-to-show) 'slide-external))))))
         

; Construct a separate page item from a LENO element.
; An item is a list of length two of the form (kind number)
(define (make-separate-element-items elements)
  (make-separate-element-items-1 elements 1 1))

(define (make-separate-element-items-1 elements prog-number ex-number)
  (cond ((null? elements) '())
        (else
         (let ((el (car elements)))
          (cond ((eq? (element-tag el) 'source-program)
                 (cons (list 'program prog-number)
                       (make-separate-element-items-1 (cdr elements) (+ 1 prog-number) ex-number)))
                ((eq? (element-tag el) 'exercise)
                 (cons (list 'exercise ex-number)
                       (make-separate-element-items-1 (cdr elements) prog-number (+ 1 ex-number))))
                (else (laml-error "make-separate-element-items-1: Should not happen")))))))

; Selector functions on separate page element items.
(define kind-of-separate-element-item (make-selector-function 1 "kind-of-separate-element-item"))
(define number-of-separate-element-item (make-selector-function 2 "number-of-separate-element-item"))

; Search functions in list of separate page element items.
(define (separate-page-item-before kind number)
  (element-before (list kind number) separate-elements-on-current-page id-1 equal?))

(define (separate-page-item-after kind number)
  (element-after (list kind number) separate-elements-on-current-page id-1 equal?))

; Return a program/exercise/... name depending on kind.
(define (separate-slide-page-item-name lecture-id page-id kind number)
 (cond ((eq? kind 'program)
          (program-name lecture-id page-id 'slide number))
       ((eq? kind 'exercise)
          (exercise-name lecture-id page-id 'slide number))
       (else (laml-error "separate-slide-page-item-item: Unsupported kind" kind))))

(define (separate-note-page-item-name lecture-id page-id kind number)
 (cond ((eq? kind 'program)
          (program-name lecture-id page-id 'note number))
       ((eq? kind 'exercise)
          (exercise-name lecture-id page-id 'note number))
       (else (laml-error "separate-note-page-item-item: Unsupported kind" kind))))

; ---------------------------------------------------------------------------------------------------------------

(define (line-number-text-column n)
  (apply string-append (map (lambda (x) (string-append x CR)) (map as-string (number-interval 1 n)))))

(define (number-of-lines-in-text str)
 (letrec ((number-of-lines-in-text-1
           (lambda (str from res)
             (let ((found-idx (find-in-string str (as-char CR) from)))
               (if found-idx
                   (number-of-lines-in-text-1 str (+ found-idx 1) (+ 1 res))
                   res)))))
  (number-of-lines-in-text-1 str 0 1)))

; ---------------------------------------------------------------------------------------------------------------
; Documentation line support in source programs.

; Disregard the program proper. Return the program documentation on a line-by-line basis.
(define (extract-program-documentation program-txt)
 (let* ((program-txt-lines (string-to-list-of-lines program-txt))
        (split-points (map comment-split-position program-txt-lines))
       )
   (list-of-lines-to-string
     (extract-program-documentation-lines program-txt-lines split-points 0))
 )
)

; Disregard special comments. Return the program proper on a line-by-line basis.
(define (extract-program-source program-txt)
 (let* ((program-txt-lines (string-to-list-of-lines program-txt))
        (split-points (map comment-split-position program-txt-lines))
       )
   (list-of-lines-to-string
     (extract-program-source-lines program-txt-lines split-points))
 )
)

; Return a list of lines of program source contents.
; program-txt-lines split-point-list are list of the same length
(define (extract-program-source-lines program-txt-lines split-point-list)
  (cond ((null? program-txt-lines) '())

        ((and (boolean? (car split-point-list)) (not (car split-point-list)))    ; no comment split in first line
           (cons (car program-txt-lines)
                 (extract-program-source-lines (cdr program-txt-lines) (cdr split-point-list))))

        ((and (number? (car split-point-list)) (= (car split-point-list) 0))     ; split point at start of program line
           (extract-program-source-lines (cdr program-txt-lines) (cdr split-point-list)))

        ((and (number? (car split-point-list)) (> (car split-point-list) 0))     ; split point, not at start of program line
           (cons (substring (car program-txt-lines) 0 (car split-point-list))
                 (extract-program-source-lines (cdr program-txt-lines) (cdr split-point-list))))

        (else (laml-error "extract-program-source-lines: Should not happen."))))

; Return a list of lines of documentation contents.
; program-txt-lines split-point-list are list of the same length
; The parameter skip-lines is used to skip a number of program lines in case there some whole-line comment lines.
(define (extract-program-documentation-lines program-txt-lines split-point-list skip-lines)
  (cond ((null? program-txt-lines) '())

        ((and (boolean? (car split-point-list)) (not (car split-point-list)) (> skip-lines 0))    ; no comment split in first line. Skip some lines
           (extract-program-documentation-lines (cdr program-txt-lines) (cdr split-point-list) (- skip-lines 1)))

        ((and (number? (car split-point-list)) (> (car split-point-list) 0)  (> skip-lines 0))    ; split point, not at start of program line. Skip some lines
           (extract-program-documentation-lines (cdr program-txt-lines) (cdr split-point-list) (- skip-lines 1)))

        ((and (boolean? (car split-point-list)) (not (car split-point-list)))    ; no comment split in first line
           (cons " "
                 (extract-program-documentation-lines (cdr program-txt-lines) (cdr split-point-list) 0)))

        ((and (number? (car split-point-list)) (= (car split-point-list) 0))     ; split point at start of program line
           (cons (substring (car program-txt-lines) (string-length end-of-line-doc-comment-prefix) (string-length (car program-txt-lines)))
                 (extract-program-documentation-lines (cdr program-txt-lines) (cdr split-point-list) (+ skip-lines 1))))

        ((and (number? (car split-point-list)) (> (car split-point-list) 0))     ; split point, not at start of program line
           (cons (substring (car program-txt-lines) (+ (car split-point-list) (string-length end-of-line-doc-comment-prefix)) (string-length (car program-txt-lines)))
                 (extract-program-documentation-lines (cdr program-txt-lines) (cdr split-point-list) 0)))

        (else (laml-error "extract-program-documentation-lines: Should not happen."))))



; Return the postion (string index) of the comment-split point.
; Return #f if no such point can be found.
(define (comment-split-position line-str)
  (substring-index line-str 0 end-of-line-doc-comment-prefix))
 
