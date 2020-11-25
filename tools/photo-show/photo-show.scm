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


;;;; The photo show tool generates a continous presentation of gif or jpg
;;;; pictures from a particular directory. The kernel of the tool
;;;; is a Javascript mechanism which navigates automatically from one HTML page to the other.
;;;; The function make-photo-show generates a set of trivial HTML pages (two per picture).
;;;; The function photos-in-current-directory generates a list photo entries of the
;;;; jpg and gif file in the current directory. As an alternative you can 
;;;; make this list manually. Photos-in-current-directory depends on the
;;;; non-standard Scheme function directory-list (which is supported by MzScheme). <p>
;;;; Tool Usage: Define a simple LAML script in the directory which contains the photos.
;;;; See the examples for concrete information. By LAML processing the script 
;;;; the necessary HTML files are defined in an html sub-directory (made by this tool). <p>
;;;; The index.html file, generated in the current directory, is a natural starting point
;;;; of the photo show. The file index-visual.html is an alternative index. <p>
;;;; Each execution of make-photo-show genererates a file first.lsp, which contains
;;;; the name of the first photo in the collection (a string with no extension). 
;;;; The function first-url-in-directory (which generates a continutation url for
;;;; another collection) uses the first.lsp file of the other directory. Thus, this must
;;;; be taken into consideration when a more complex collection of photos are organized.<p>
;;;; The procedure make-photo-show also generates index-visual-editor.html, which is a page
;;;; that submits a request to a photo service program at www.cs.auc.dk. The server returns
;;;; a zip file with all necessary html stuff for a photo show, including new index files.
;;;; Please notice that this facility is experiment, and not yet meant for general use.<p>
;;;; <b> Obsolete. Use <a href = "../../../styles/xml-in-laml/photo-show/man/photo-show.html">the XML-in-LAML Photo Show Tool</a>.</b>


(load (string-append laml-dir "laml.scm"))

(lib-load "html4.0-loose/basis.scm")
(lib-load "html4.0-loose/surface.scm")
(lib-load "html4.0-loose/convenience.scm")

; Redefinition relative to convenience.scm
(define (keypress-action-setting keypress-action)
  (list 'onload keypress-action))  ; onKeyPress

(lib-load "color.scm")
(lib-load "time.scm")
(lib-load "file-read.scm")

(define photo-show-dir (string-append laml-dir "tools/photo-show/"))
(load (string-append  photo-show-dir "remote-service/photo-zip-lib.scm"))

;;; Example script.
;;; Here follows an example script which makes a photo show. Place it in the directory with you photos.
;;; <pre>
;;; (load (string-append laml-dir "laml.scm"))
;;; (load (string-append laml-dir "tools/photo-show/photo-show.scm"))
;;; 
;;; (define default-upper-caption "Some title to appear above photo")
;;; (define default-upper-size 6)  ; font size - a number between 1 and 6
;;; (define default-lower-caption #f)  ; no lower caption
;;; (define default-lower-size 1)
;;; 
;;; (define photo-show-title "Title of show")
;;; 
;;; (define default-time 2)  ; show each picture 2 seconds
;;; 
;;; (define photo-list (photos-in-current-directory))   ; extract photos from current dir
;;; (define last-url (url-of-first-picture photo-list))  ; show cyclic show
;;; 
;;; (make-photo-show photo-list)
;;; </pre>

; ---------------------------------------------------------------------------------------------------

;; A variable that controls if the tool can generate informative messages.
(define photo-show-verbose #t)

;; The directory in which to write the output html files from this tool.
;; Defaults to (startup-directory).
(define photo-destination-dir (startup-directory))

;;; Default settings.
;;; The default settings apply as defaults for photo-entries.
;;; Individual photo entries overwrites the defaults.

;; A default string for the upper captions of pictures (a string)
(define default-upper-caption #f)

;; A default string for the upper font size of pictures (a number between 1 and 6)
(define default-upper-size 0)

;; A default string for the lower captions of pictures (a string)
(define default-lower-caption #f)

;; A default string for the lower font size of pictures (a number between 1 and 6)
(define default-lower-size 0)

;; A default time (a number, in seconds). Show each picture default-time seconds
(define default-time 8)

;; The default width of a photo. Using #f as value of this variable means that
;; we use the actual width of the photo
(define default-photo-width #f)

;; The default height of a photo. Using #f as value of this variable means that
;; we use the actual height of the photo
(define default-photo-height #f)

;; The width of photos in the visual index
(define visual-index-photo-width 200) ; same constant also in remote-service/photo-zip-lib.scm

;;; Other important variables.

;; The title of the show. 
;; Intended to be redefined after this tool is loaded. 
;; If not redefined, no title is shown.
(define photo-show-title #f)

; The URL prefix of the pictures. Never redefined this variable.
; The default works for pictures in the parent directory of the html directory.
(define picture-url-prefix "../")

;; The relative URL of the continuation of this photo show. You cannot supply an absolute URL.
;; This URL is relative to the directory, in which the picture resides.
;; This variable must be re-defined after this tool is loaded.
;; You can use (url-of-first-picture photo-list) as the value of this variable
;; to make a cyclic show.
;; If #f, this show is terminated by an empty screen in the background color.
(define last-url "")

(define (the-last-url)
 (cond ((and (boolean? last-url) (not last-url)) "end-page.html")
       ((string? last-url) last-url)
       (else (as-string last-url))))

;; A URL which allows us to exit from the photo index. Like last-url, this URL must be relative.
;; It is interpreted as relative to the directory in which the photos reside.
;; This variable can be redefined after photo.scm (this tool) has been loaded.
;; If not redefined, no home url is presented.
(define home-url #f)

; ---------------------------------------------------------------------------------------------------
; CGI program variables - default initialization of

(define photo-dir-name (file-name-proper (startup-directory)))  ; MzScheme specific. Earlier (current-directory)
; Gives "" - remove trailing '/' before using file-name-proper, or find a better way.

; ---------------------------------------------------------------------------------------------------
; Photo entry Selectors
(define photo-file-name-of (make-selector-function 1 "photo-file-name-of"))
(define photo-show-time-of (make-selector-function 2 "photo-show-time-of"))
(define photo-upper-caption-of (make-selector-function 3 "photo-upper-caption-of"))
(define photo-upper-size-of (make-selector-function 4 "photo-upper-size-of"))
(define photo-lower-caption-of (make-selector-function 5 "photo-lower-caption-of"))
(define photo-lower-size-of (make-selector-function 6 "photo-lower-size-of"))

(define (photo-height entry)
  (cond ((>= (length entry) 7) (seventh entry))
        (else default-photo-height)))  

; ---------------------------------------------------------------------------------------------------

(define (make-photo-entry fn tm uc us lc ls)
  (list fn tm uc us lc ls))

(define (make-default-photo-entry photo-file)
  (list photo-file default-time default-upper-caption default-upper-size default-lower-caption default-lower-size))


; next-photo-entry may be #f, in which case the global last-url is used
; n is the number of this photo. Both next-photo-entry and prev-photo-entry may be #f if the next/previous
; entry exists.
(define (make-single-photo-page this-photo-entry next-photo-entry prev-photo-entry n)
 (let ((this-file-name (photo-file-name-of this-photo-entry))
       (this-show-time (photo-show-time-of this-photo-entry))
       (this-upper-caption (photo-upper-caption-of this-photo-entry))
       (this-upper-size (photo-upper-size-of this-photo-entry))
       (this-lower-caption (photo-lower-caption-of this-photo-entry))
       (this-lower-size (photo-lower-size-of this-photo-entry))
       (actual-height (photo-height this-photo-entry))
       (next-file-name (if next-photo-entry (picture-name-to-url (photo-file-name-of next-photo-entry)) (string-append "../" (the-last-url)))) 
       (prev-file-name (if prev-photo-entry (picture-name-to-url (photo-file-name-of prev-photo-entry)) ""))
      )
  (write-timed-photo-page
    this-file-name this-show-time next-file-name prev-file-name this-upper-caption this-upper-size this-lower-caption this-lower-size n
    actual-height)
  (write-stoped-photo-page
    this-file-name this-show-time next-file-name prev-file-name this-upper-caption this-upper-size this-lower-caption this-lower-size n
    actual-height)
 )
)

(define (picture-name-to-url picture-name)
  (string-append (file-name-proper picture-name) "." "html"))

; Show the page contents in term of a picture. The picture, captions and font sizes are given as parameters.
; If stop? is true, a stop link is provided for. If not stop?, a run link bringing us to next-url is presented.
; next-stop-url and prev-stop-url are only used is stop? is false.
(define (show-picture picture-file-name upper-caption upper-size lower-caption lower-size n total-number 
                      stop? next-url next-stop-url prev-stop-url actual-height)
  (center 
    (con
     (left-middle-right-banner 
        (con 
          (html4:a (text-choice "Indhold" "Index") 'href "../index.html" 'css:text-decoration "none") (horizontal-space 8)
          (if stop?
             (html4:a "Stop" 'href (string-append (file-name-proper picture-file-name) "-stop.html") 'css:text-decoration "none")
             (con (html4:a (text-choice "Kør" "Run") 'href next-url 'css:text-decoration "none")  (horizontal-space 8)
                  (html4:a (text-choice "Foregående" "Previous") 'href prev-stop-url 'css:text-decoration "none")  (horizontal-space 8)
                  (html4:a (text-choice "Næste" "Next") 'href next-stop-url 'css:text-decoration "none"))
          )
        )
 
        (string-append 
          (as-string n) " : " (as-string total-number) 
          (horizontal-space 5) "-" (horizontal-space 5) (file-name-proper picture-file-name))
        (if home-url
            (html4:a (text-choice "Hjem" "Home") 'href (string-append "../" home-url) 'css:text-decoration "none")
            "")
     )
     (if upper-caption 
         (con (font-1 upper-size white (b upper-caption)) (p))
         (vertical-space 1)) 
     (cond ((and actual-height default-photo-width)
              (html4:img 'height actual-height 'width default-photo-width 
                         'src (string-append picture-url-prefix picture-file-name) 'alt ""))
           (actual-height
              (html4:img 'height actual-height 'src (string-append picture-url-prefix picture-file-name) 'alt ""))
           (default-photo-width
              (html4:img 'width default-photo-width 'src (string-append picture-url-prefix picture-file-name) 'alt ""))
           (else (html4:img 'src (string-append picture-url-prefix picture-file-name) 'alt "")))

     (p)
     (if lower-caption 
         (con (font-1 lower-size white (b lower-caption)) (p))
         ""))))

(define (meta-tag-clauses)
   (list
     (list 'name "Generator" 'content "LAML")
   )
)


; Write a photo on a html page and provide for a continuation to next-picture.
; This-picture is a jpg or gif file names with extensions, but without initial path.
; Next-picture-url is an URL of the next picture (html extension).
; The remaining parameters are captions and their font sizes.
(define (write-timed-photo-page this-picture show-time next-picture-url prev-picture-url
                                upper-caption upper-size lower-caption lower-size n actual-height) 
 (let* ((this-picture-name (file-name-proper this-picture))
        (next-picture-name (file-name-proper next-picture-url))
        (this-picture-stop-url (string-append this-picture-name "-stop.html"))
        (colors1 (list black white white white))
        (bg-color (first colors1))
        (text-color (second colors1))
        (link-color (third colors1))
        (vlink-color (fourth colors1)))
  (write-text-file
;    (timed-page "Photo Show" show-time next-picture-url '()
;          (show-picture this-picture upper-caption upper-size lower-caption lower-size n photo-list-length #t 
;                       next-picture-url #f #f  actual-height) 
;          bg-color text-color link-color vlink-color)
     (timed-page-with-keypress-script "Photo Show" show-time next-picture-url '()
           (show-picture this-picture upper-caption upper-size lower-caption lower-size n photo-list-length #t 
                        next-picture-url #f #f  actual-height) 
 
           'dynamic
           (list (string-append photo-show-dir "javascript/") "photoNavigate.js")
           (js-call "photoNavigateRunning" 
                     (map string-it-single 
                           (list this-picture-stop-url))
           )
 
           bg-color text-color link-color vlink-color)
    (string-append photo-destination-dir "html/" this-picture-name ".html"))))


; A variant of write-timed-photo-page, which does not apply automatic time out.
(define (write-stoped-photo-page this-picture show-time next-picture-url prev-picture-url
                                 upper-caption upper-size lower-caption lower-size n actual-height) 
 (let* ((this-picture-name (file-name-proper this-picture))
        (next-picture-name (file-name-proper next-picture-url))
        (next-picture-init-path (file-name-initial-path next-picture-url))
        (next-picture-stop-url (if (empty-string? next-picture-url) 
                                   "" 
                                   (string-append next-picture-init-path next-picture-name "-stop" ".html")))
        (prev-picture-name (file-name-proper prev-picture-url))
        (prev-picture-stop-url (if (empty-string? prev-picture-url) "" (string-append prev-picture-name "-stop" ".html")))
        (run-url next-picture-url)
        (colors1 (list black white white white))
        (bg-color (first colors1))
        (text-color (second colors1))
        (link-color (third colors1))
        (vlink-color (fourth colors1)))
  (write-text-file
    (page-with-keypress-script
           "Photo Show"
           '()
          (show-picture this-picture upper-caption upper-size lower-caption lower-size n photo-list-length #f 
                        next-picture-url next-picture-stop-url prev-picture-stop-url actual-height)
          'dynamic
          (list (string-append photo-show-dir "javascript/") "photoNavigate.js")
          (js-call "photoNavigate" 
                    (map string-it-single 
                          (list next-picture-stop-url
                                prev-picture-stop-url
                                run-url
                                "../index.html"
                                ))
                     )

           bg-color text-color link-color vlink-color
           'double-press
         )
    (string-append photo-destination-dir "html/" this-picture-name "-stop" ".html"))))



;;; Top level functions.
;;; The functions in this section are used in the LAML script which generates the photo show.

;; Make the photo-show from photo-list in terms of individual HTML files and an index file.
;; Photo-list may be generated by photos-in-current-directory, or manually.
;; Photo-list is a list of photo-entries, of length at least one.
;; Each photo-entry is of the form ("photo-file.ext" show-time "upper-caption" upper-size "lower-caption" lower-size).
(define (make-photo-show photo-list)
 (let ((html-dir (string-append photo-destination-dir "html")))
 (display-message 
  (string-append 
    "Welcome to the LAML photo show generator, LAML " laml-version "."
  ))
  (set! photo-list-length (length photo-list))
  (if (not (directory-exists? html-dir))
      (make-directory-in-directory photo-destination-dir "html"))
  (make-photo-index photo-list)
  (make-visual-photo-index photo-list)
  (make-visual-photo-editor photo-list)
  (map4
    make-single-photo-page
    photo-list
    (append (cdr photo-list) (list #f))
    (append (list #f) (butlast photo-list))
    (number-interval 1 (length photo-list))
  )

  ; write the first.lsp file
  (if (not (null? photo-list))
   (file-write
    (file-name-proper (photo-file-name-of (first photo-list)))
    (string-append (startup-directory) "first.lsp")))

    (ensure-directory-existence! (string-append photo-destination-dir "html/") "javascript")
    (copy-text-file (string-append photo-show-dir "javascript/" "photoNavigate.js")
                    (string-append photo-destination-dir "html/" "javascript/photoNavigate.js")
                    #t)                  ; overwrite if necessary



  (if photo-show-verbose (display-message (string-append (as-string (length photo-list)) " " "photos pages generated")))
  (if photo-show-verbose (display-message "DONE. Bring index.html or index-visual.html up in you browser to start the show."))))

(define photo-list-length 0) ; assigned in make-photo-show

; Make the photo index file
(define (make-photo-index photo-list)
 (write-text-file
   (page-1 
     (string-append (text-choice "Fotooversigt" "Photo index")
                    (if photo-show-title (string-append  ": " photo-show-title) ""))
     '()
     (con 
       (left-right-banner 
          (html4:a (text-choice "Visuel oversigt" "Visual index") 'href "index-visual.html" 'css:text-decoration "none")
          (if home-url
              (html4:a (text-choice "Hjem" "Home") 'href home-url 'css:text-decoration "none")
              "")
       )
       (h1 (if photo-show-title photo-show-title (text-choice "Fotooversigt" "Photo Index")))
       (ol-1 (map2 photo-index-entry photo-list (number-interval 1 (length photo-list))))
       (p)
       (if last-url (a-tag last-url (text-choice "Fortsættelse" "Continuation of show")) "")
       (vertical-space 2)
       (when-generated)
     )
     black white white white)
   (string-append photo-destination-dir "index.html")))

(define (make-visual-photo-index photo-list)
 (letrec ((bs (lambda (x) (font-size 3 (b x)))))
  (write-text-file
   (page-1
     (string-append (text-choice "Fotooversigt" "Photo index")
                    (if photo-show-title (string-append  ": " photo-show-title) ""))
     '()
     (con 
       (left-right-banner 
          (con (html4:a (text-choice "Tekstuel indhold" "Textual index") 'href "index.html" 'css:text-decoration "none") (horizontal-space 8)
               (html4:a (text-choice "Visuel editor" "Visual editor") 'href "index-visual-editor.html" 'css:text-decoration "none") (horizontal-space 8)
               (html4:a (text-choice "Lav nyt fotoshow" "Make new photo show") 'href "http://www.cs.auc.dk/~normark/cgi-bin/remote-photo-service/start-form.html" 'css:text-decoration "none"))
          (if home-url
              (html4:a (text-choice "Hjem" "Home") 'href home-url 'css:text-decoration "none")
              "")
       )
       (h1 (if photo-show-title photo-show-title (text-choice "Fotooversigt" "Photo index")))
       (indent-pixels 20
        (table-3 0
          (list 50 (+ 30 visual-index-photo-width) 50 300 300 100)
          (append
           (list 
            (map bs 
              (text-choice 
               (list "Nr." "Foto" "Tid" "Øvre overskrift" "Nedre overskrift" "Filnavn")
               (list "Nr." "Photo" "Time" "Upper Caption" "Lower Caption" "File name")))
            (make-list 6 (vertical-space 1))
           )
           (map2 photo-visual-index-entry photo-list (number-interval 1 (length photo-list))))
        )
       )
       (p)
       (if last-url (a-tag last-url (text-choice "Fortsættelse" "Continuation of show")) "")
       (vertical-space 2)
       (when-generated)
     )
     black white white white)
   (string-append photo-destination-dir "index-visual.html"))))



; Make a visual entry in the visual index - a table row
(define (photo-visual-index-entry photo-entry n)
 (letrec ((default-handling 
           (lambda (x) (if (and (boolean? x) (not x)) (em (text-choice "Ingen" "None")) (as-string x))))
         )

  (let ((lower-caption (photo-lower-caption-of photo-entry))
        (upper-caption (photo-upper-caption-of photo-entry))
        (timing (photo-show-time-of photo-entry))
        (photo-file-name (photo-file-name-of photo-entry))
       )
  (list
   (em (as-string n))
   (a-tag
    (string-append "html/" (picture-name-to-url (photo-file-name-of photo-entry)))
    (html4:img 'width (as-string visual-index-photo-width) 'src photo-file-name 'alt "Click to start show from here")
   )
   (as-string timing)
   (default-handling upper-caption)
   (default-handling lower-caption)
   photo-file-name
   )
  )
 )
)

; ---------------------------------------------------------------------------------------------------

; Make a page with a visual photo editor. 
; This procedure can only be used from a remote-service cgi context.
; Uses variables such as default-upper-caption as free variables.
(define (make-visual-photo-editor photo-list)
  (write-text-file
   (page-1 
    "Visual Photo Editor"
    '()
    (form-1 "http://www.cs.auc.dk/~normark/cgi-bin/remote-photo-service/make-photo-zip-refined.cgi"   ; new program
   (con 
     (left-right-banner 
          (html4:a "Index" 'href "index-visual.html" 'css:text-decoration "none")
          (if home-url
              (html4:a "Home" 'href home-url 'css:text-decoration "none")
              "")
     )


     (h 1 "The LAML Photo Show Service")

     (font-1 4 red "This is an experimental facility - do not expect it to be of production quality") (p)

     "Use this page to refine your photo show. You can change the defaults on the upper part of the pages.
      In the lower part you can change the file names, the timing, the upper and lower captions, and the 
      sequencing of the photos."

     (p) (hr) (p)

     (table-3 
       0
       (list 220 30 800)
       (map row-presenter
        (list
         (list "The name of directory in which the photos reside:" (text-line 'photo-dir-name 60 photo-dir-name))
         (list "Overall photo show title: " (text-line 'photo-show-title 60 (as-string-default photo-show-title)))
         (list "Default upper caption shown above each photo: " (text-line 'default-upper-caption 60 (as-string-default default-upper-caption)))
         (list "Font size of upper caption:" (con (text-line 'default-upper-size 1 (as-string default-upper-size)) ))
         (list "Default lower photo caption shown below the picture: " (con (text-line 'default-lower-caption 60 (as-string-default default-lower-caption)) ))
         (list "Font size of lower caption:" (con (text-line 'default-lower-size 1 (as-string default-lower-size))))
         (list "Default number of seconds a photo is shown: " (text-line 'default-time 3 (as-string default-time)))
         (list "Continuation URL: " (con (text-line 'last-url 60 (the-last-url))))
         (list "Home URL: " (con (text-line 'home-url 60 home-url)))

 
       ))
       "top"
     )

    
    (p) (hr) (p)

    (font-size 2 (con "Below you can specify the properties of each photo in the photo show.
                  Only fill in the details if the defaults - shown above - do not apply.
                  The number field in the first column specify the sequencing of the pictures; 
                  The sequence numbers are given as 10, 20, 30 in order to make it easy to put
                  a photo in between others, for instance as 5, 15, 25, 35 etc. In case you 
                  you want to eliminate a photo from the show, just delete contents of the Photo file name field.
                  If you want to add an extra photo, use one of the " (as-string trailing-number-of-empty-entries) 
                  " empty entries at the bottom of the list.")) (p)

    (let ((empty-entries (make-empty-photo-entries trailing-number-of-empty-entries))
         )
     (con 
      (present-visual-editor-photo-list (append photo-list empty-entries))

      (vertical-space 1)

      (hidden-line 'photo-list-length (as-string (+ (length photo-list) trailing-number-of-empty-entries)))
     )
    )

    (p) (hr) (p)

    (submit "Make refined zip file" 'submission-mode)

    (p)
    credits


  ))
     black white white white)
   (string-append photo-destination-dir "index-visual-editor.html")))

; as present-photo-list, but also shows the photo itself.
; The page can only presented from the client file system.
(define (present-visual-editor-photo-list photo-list)
  (table-3 0
    (list 60 200 60 400 100)
   
    (cons 
     (map b (list "Number" "Photo" "Time" "Upper/lower captions" "Photo file name"))

     (map2 present-visual-editor-photo-list-entry 
              (number-interval 1 (length photo-list))
           photo-list))))

(define (present-visual-editor-photo-list-entry n entry)
  (let ((fn (photo-file-name-of entry))
        (tm (photo-show-time-of entry))
        (uc (photo-upper-caption-of entry))
        (lc (photo-lower-caption-of entry))
       )
    (list
      (text-line (make-id "n" n) 3  (as-string (* 10 n)))
      (html4:img 'src fn 'alt "Click to start show from here" 'width (as-string visual-index-photo-width))
      (text-line (make-id "t" n) 3  (as-string-default tm))
      (con (textarea-1 (make-id "u" n) 4 40 (as-string-default uc)) (br)
           (textarea-1 (make-id "l" n) 4 40 (as-string-default lc)))
      (text-line (make-id "f" n) 20 (as-string-default fn))
    )
  ))


; Make a visual editor entry in the visual index - a table row
; (define (photo-visual-editor-entry photo-entry n)
;  (letrec ((default-handling 
;            (lambda (x) (if (and (boolean? x) (not x)) (em "None") (as-string x))))
;          )
; 
;   (let ((lower-caption (photo-lower-caption-of photo-entry))
;         (upper-caption (photo-upper-caption-of photo-entry))
;         (timing (photo-show-time-of photo-entry))
;         (photo-file-name (photo-file-name-of photo-entry))
;        )
;   (list
;    (em (as-string n))
;    (a-tag
;     (string-append "html/" (picture-name-to-url (photo-file-name-of photo-entry)))
;     (html4:img 'width (as-string visual-index-photo-width) 'src photo-file-name 'alt "txt")
;    )
;    (as-string timing)
;    (default-handling upper-caption)
;    (default-handling lower-caption)
;    photo-file-name
;    )
;   )
;  )
; )

; ---------------------------------------------------------------------------------------------------
        


; Take one level off relative-file-name. A generally useful function.
(define (take-one-level-from relative-file-name)
 (let ((forward-slash-pos (find-in-string relative-file-name #\/))
       (backward-slash-pos (find-in-string relative-file-name #\\))
       (lgt (string-length relative-file-name))
      )
   (cond ((and forward-slash-pos backward-slash-pos (<= forward-slash-pos backward-slash-pos))
             (substring relative-file-name (+ 1 forward-slash-pos) lgt))
         ((and forward-slash-pos backward-slash-pos (<= backward-slash-pos forward-slash-pos))
             (substring relative-file-name (+ 1 backward-slash-pos) lgt))
         (forward-slash-pos
             (substring relative-file-name (+ 1 forward-slash-pos) lgt))
         (backward-slash-pos
             (substring relative-file-name (+ 1 backward-slash-pos) lgt))
         (else #f))))

; Make an entry in the photo index
(define (photo-index-entry photo-entry n)
 (let ((caption (photo-lower-caption-of photo-entry))
       (timing (photo-show-time-of photo-entry)))
  (con
   (a-tag
    (string-append "html/" (picture-name-to-url (photo-file-name-of photo-entry)))
    (if caption caption (con (text-choice "Fotonummer" "Photo number") (horizontal-space 1) (as-string n)))
   )
   (horizontal-space 7) (font-size 2 (photo-file-name-of photo-entry)) 
   (horizontal-space 5) "-" (horizontal-space 5) (font-size 2 (con (as-string timing) " " "seconds")))))

(define (photo-filename-leq? fn1 fn2)
  (string<=? (downcase-string fn1) (downcase-string fn1)))
  
;; Return a list of photo-entries, sorted alphabetically, in the LAML startup directory.
;; A photo entry is a list of the form ("photo-file.ext" show-time "upper-caption" upper-size "lower-caption" lower-size)
;; upper-caption and/or lower-caption may be #f in case no caption apply; In that case upper-size and lower-size (integers, seconds)
;; do not matter.
(define (photos-in-current-directory)
  (let* ((dir-list (directory-list (startup-directory)))
         (picture-list
           (filter 
             (lambda (fn) (or (equal? "jpg" (file-name-extension fn)) (equal? "gif" (file-name-extension fn))
                              (equal? "JPG" (file-name-extension fn)) (equal? "GIF" (file-name-extension fn))))
             dir-list))
         (sorted-picture-list (sort-list picture-list photo-filename-leq?))
        )
    (map make-default-photo-entry picture-list)))


;; Return a URL to the first picture in the photo list.
;; This may serve as a reasonable value of last-url.
;; As for last-url, the resulting URL is relative to the picture directory
(define (url-of-first-picture photo-list)
  (if (> (length photo-list) 0)
      (string-append "html/" (file-name-proper (photo-file-name-of (car photo-list))) "." "html")))

;; Return the URL of the first photo page (a html page) in dir. 
;; The URL is relative to the HTML directory of the current collection of images, and
;; it will normally address the first html file in the next collection. Thus the return value
;; is a legal value of last-url, just like the value of url-of-first-picture.
;; The directory dir is given relative to the photo directory of current collection of images.
;; This function relies on the file first.lsp, which is generated by the photo show system in
;; each picture directory upon processing. If no first.lsp file exists, the function returns 
;; the empty string.
(define (first-url-in-directory dir)
  (let* ((abs-dir (string-append (startup-directory) dir))
         (first-path (string-append abs-dir "first.lsp")))
    (if (file-exists? first-path)
        (string-append 
           dir    ; and to paremeter directory
           "html/"  ; and into its html directory
           (file-read first-path) ".html")
        "")))
             

(define (end-page)
  (page
    "End page"
    (center
      (con 
       (vertical-space 4)
       (font-size 6 (b (text-choice "Slut" "The End")))
      ))
    black white white white))

;; Return a sublist of photo-input-list, namely the photo entries in number-list.
;; The first photo is number 1.
(define (take-subset-by-numbers number-list photo-input-list)
   (if (null? number-list)
       '()
       (let ((actual-photo (get-photo-number (car number-list) photo-input-list)))
         (cons actual-photo (take-subset-by-numbers (cdr number-list) photo-input-list)))))
 
 (define (get-photo-number i phlst)
   (list-ref phlst (- i 1)))

(write-text-file (end-page) (string-append photo-destination-dir "end-page.html"))
(write-text-file (end-page) (string-append photo-destination-dir "end-page-stop.html"))


