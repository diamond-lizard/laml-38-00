;;;; The course plan document style supports the generation of a course home page with
;;;; references to course plans for a number of lectures, course introduction, lecture overviews, calendars, and exercises. 
;;;; All the generated WWW pages are organized
;;;; in a single directory, the  <em>course html directory</em> - for convenient bundling of all the generated pages.


(lib-load "xml-in-laml/xml-in-laml.scm")

; The course plan action procedure:
(define (course-plan! ast)
  (do-course-plan-xml-in-laml-bridging! ast)
  
  ; Saving the AST - for laml-forms experiments:
  (file-write ast (string-append course-dir "internal/" (source-filename-without-extension) ".ast"))

  (end-laml)
    
)

(laml-style "xml-in-laml/course-plan/mirror/course-plan-1-mirror")


; The ordering of the loading of the XHTML mirrors are important. The transitional mirror 'wins',
; but the frameset stuff is available.

(lib-load "xml-in-laml/mirrors/xhtml10-frameset-mirror.scm")
(lib-load "xml-in-laml/mirrors/xhtml10-transitional-mirror.scm")
(set! xml-check-language-overlap? #f)  ; too much overlap between XHTML 1.0 transitional and framework.

(lib-load "xhtml1.0-convenience.scm")

; ---------------------------------------------------------------------------------------------------
; Constants

(define course-plan-verbose? #t)

; Are the HTML pages written in raw or pretty printed mode.
; Values: raw or pp (symbols).
(define course-plan-html-rendering-mode 'pp)

; The mode of editing. A symbol.
; laml: Editing a laml source file.
; laml-form: Editing via a web laml-form interface.
; Normal use: Use the default value laml.
(define course-plan-editing-mode 'laml)


; The URL prefix to the cgi LAML forms interface.
; Does only apply if course-plan-editing-mode is laml-form.
(define laml-forms-web-interface-url-prefix "http://www.cs.auc.dk/~normark/cgi-bin/course-plan/")

; The File path prefix to the calendar area in the cs.auc.dk LAML calendars.
; Does only apply if course-plan-editing-mode is laml-form.
(define laml-calendar-file-prefix "/user/normark/.public_html/cgi-bin/calendar/")

; ---------------------------------------------------------------------------------------------------
; The Original course-plan addapted to XHTML1.0-transitional


(define course-plan-version "version 2, XML-in-LAML, October 2002")

; The directory in which the course plan software is located
(define course-plan-software-directory (string-append laml-dir "styles/xml-in-laml/course-plan/"))

; ===============================================================================================================
;;; The course description. 
;;; The definitions in this section must be initialized from the course-info part of the course plan.

;; A list of subject records.
;; A subject record is a list of (subject-id title subject and possible external url).
;; The default is the empty list. Is allowed to be longer or shorter than lecture-description-list.
;; Entries are looked up via the subject-id.
(define course-plan-subject-list '())

;; A list of lecture records. 
;; A lecture record is the central description of a single lecture in the course, in terms of
;; a lecture-id, a subject-id (referring to a record in subject-list), exercise time info, plenum time info, and room info.
;; The list must be of the same length as course-plan-time-list.
;; Lecture record description:
;;   Lecture-id: A symbol identifying a lecture. -
;;   Subject-id: A symbol identifying a subject in subject list. -
;;   Exercise-info: A list of (start-displacement length). Displacement relative to lecture start in course-plan-time-list. Length in seconds. -
;;   Plenum-info: A list of (start-displacement length). Displacement relative to lecture start in course-plan-time-list. Length in seconds. -
;;   Room: A room id string.
;;   And more...
(define lecture-description-list '())

;; A list of times for the individual lectures in the course.
;; Each time is an integer (the number of seconds since jan. 1, 1970).
;; Use the function time-encode from the LAML time library to return such a number.
;; Must be of the same length as course-plan-lecture-list.
(define course-plan-time-list '())

;; An id of the course - used mainly in the laml-form interface
(define course-id #f)

;; The title of the course (a string).
;; Must be defined in the course description file.
(define course-title #f)

;; A brief, abbreviated course title (a string).
;; Must be defined in the course description file.
(define brief-course-title #f)

;; Course semester (a string).
;; Must be defined in the course description file.
(define course-semester #f)

;; Brief course semester (a string).
;; Must be defined in the course description file.
(define brief-course-semester #f)

;; The name of the teacher(s). Defaults to "The teacher"
(define teacher-name "The teacher")

;; The absolute path of the course directory.
;; The internal directory will be a subdirectory of the course-dir,
;; and it is natural to place the LAML XML-in-LAML source of the course plan in the course-dir.
;; Ends in a slash.
(define course-dir #f)

;; The prefix of the course URL, pointing at the HTML course directory.
;; Ends in a slash.
;; Must be defined in the course description file.
(define course-url-prefix #f)

; The full, absolute URL of the author.
(define author-home-url #f)

; Are the course plan overview pages generated in the current run?
(define make-overview-pages? #f)

; The exercise model.
(define actual-exercise-model #f)
(define floating-exercises #f)

; Year and month information of the course calendar - all natural numbers
(define course-plan-calendar-start-year #f)
(define course-plan-calendar-start-month #f)
(define course-plan-calendar-number-of-months #f)

; If url-of-external-calendar is a string, take it as an (absolute) URL of an external calendar.
; This calendar is used instead of the integrated calendar of the course plan system.
; Rationale: Merge the course-plan generated calendar with other calendars, and provide the URL to the
; merged calendar.
(define url-of-external-calendar #f)

; A number of extra calendar entries, in the format of the LAML calendar facility, added to the course plan calendar entries.
(define extra-calendar-entries '())

; Can be redefined after loading to accomodate another calendar structuring.
(define course-plan-number-of-calendar-divisions 2)
(define course-plan-calendar-division-of-day
  (let* ((seconds-in-a-day (* 24 60 60))
         (noon-time (/ seconds-in-a-day 2)))
    (list (list 0 (- noon-time 1)) (list noon-time (- seconds-in-a-day 1)))))


; How many days is an element marked as new or updated. Assigned in
(define new-or-updated-n-days-attr #f)



; A function returning the full path of the lecture description list file in the internal directory of the course directory.
; In this file we store lecture-description-list in order for other tools to access the information in this data structure.
(define (lecture-list-file)
  (string-append course-dir "internal/lecture-list.lsp"))

; A list of link tripples to be displayed at the bottom of a course plan page.
(define course-plan-bottom-links '())

; A list of link tripples to appear under the course plan navigation links in the left frame
(define course-plan-index-links '())

(define (bottom-link-contribution) 
  (letrec ((between-links (lambda () (con (space 5) "|" (space 5)))))
    (center
     (brl (map 
            (compose (lambda (x) (font-size 2 x)) link-tripple-contribution)
             course-plan-bottom-links)))))

(define (link-tripple-contribution link-tripple)
  (let ((url (url-of-link-tripple link-tripple))
        (target (target-of-link-tripple link-tripple))
        (anchor (anchor-of-link-tripple link-tripple))
       )
   (a 'href url 'target target anchor)))


; Link tripple selectors:
(define url-of-link-tripple (make-selector-function 1 "url-of-link-tripple"))
(define anchor-of-link-tripple (make-selector-function 2 "anchor-of-link-tripple"))
(define target-of-link-tripple (make-selector-function 3 "target-of-link-tripple"))


;; A function which returns a list of links which go into the overview frame (the leftmost frame of the frameset) of the course home page.
;; Each element of the list must be a list of the format ("url" "anchor-name" "target"). The target may be "main" (for the main, rightmost
;; window of the frameset) or "_top".
;; Thus this function must return a list of lists.
;; The links given here are added to some automatically generated links to overviews, calendar, and introduction.
;; If you want additional general links, redefine the function in the course description file.
;; As the default, this function returns the empty list.
(define (general-overview-links) course-plan-index-links)

;; A function which returns trace information. Must return the emtpy string of an HTML comment.
;; The LAML function html:page uses and expects this function.
;; You can use the LAML function html-comment when implementing the function. The default implementation returns the empty string.
(define (tracing-comment)
  "")

;; A function which returns meta information, inserted by html:page.
;; The LAML function html:page uses and expects this function.
;; Must return a list of property lists of meta information.
;; The default implementation returns the empty list (no meta information).
(define (meta-tag-clauses)
  '())

;; The last lecture-number which we want presented now.
;; Using this variable we can progressively reveal more and more lectures in the course.
;; Must be defined in the course description file.
(define last-lecture-number #f)

;; The pixel width of the overview frame (leftmost frame of course home page).
;; Default 320.
(define overview-pixel-width 320)

;; The total pixel width of a calendar. OK for one semester, but problematic for other course spans.
(define preferred-calendar-pixel-width 880)

;; The time (second-count) of the first lecture in the course.
;; Must be defined in the course description file.
(define course-start #f)

;; Given the records of this lecture and the next-lecture, return an interval of the exercise time.
;; Return #f if there are no exercises.
;; Depends on the model: forskudte øvelser til næste øvelsesgang.
;; This function may return #f in case there is no lectures.
;; next-lecture-rec may be #f in which case this function also returns #f.
;; Must be redefined in the course-description-file.
(define (actual-exercise-interval this-lecture-rec next-lecture-rec)
  (error "You have not redefined actual-exercise-internal in the course-description-file to reflect the course exercise model"))

(define (actual-exercise-interval this-lecture-rec next-lecture-rec)
  (lecture-exercise-interval this-lecture-rec))

(define (actual-exercise-interval this-lecture-rec next-lecture-rec)
 (if (eq? actual-exercise-model 'this-exercise-slot)
     (lecture-exercise-interval this-lecture-rec)
     (if next-lecture-rec
         (lecture-exercise-interval next-lecture-rec)
         #f)))


(define relative-source-destination-path #f)

; The fragment of a file path which distinguishes the course-dir and the HTML destination directory.
; As default, the html files are placed in a subdirectory of the course-dir named 'html'.
; You can redefine this function after the course-plan software is loaded in order to provide for another html destination.
(define (relative-source-html-destination-path-fragment)
  relative-source-destination-path)


; The news flash string to display on certain course plan pages.
; An empty newflash will not cause any news flashing.
(define news-flash-string "")

; The URL associated with a non-empty news-flash-string.
(define news-flash-url #f)

; The news flash level determines the scope the newsflash (on how many pages news-flash-string will appear).
; A low number gives little of no news flash.
; Level 0: None
; Level 1 or higher: Main page: main overview page of all lectures.
; Level 2 or higher: Level 1 + each lecture overview page.
(define news-flash-level 0)

; Whether or not to use an -.ico image for visual identification of these course plan web pages.
; Boolean value
(define shortcut-icon #f)

; END OF PRE-DEFINITIONS OF COURSE DESCRIPTIONS.
; ===============================================================================================================

; ---------------------------------------------------------------------------------------------------------------
;;; Top level course plan functions. 
;;; The top level functions used in individual lecture pages (pages describing a single lecture plan in the course)
;;; are init-course-plan and lecture-plan. Besides these, the exercise functions described later are typically found
;;; in a lecture plan file.

;; The init-course-plan function must be called after all loading is completed, and before any pages are generated.
(define (init-course-plan)

  (if course-plan-verbose? (laml-welcome))
  (if course-plan-verbose? (course-plan-welcome))
  
  (check-redefinitions)

  ; Define effective course-plan-lecture-list by merging the lecture-description-list and the course-plan-time-list
  (set! course-plan-lecture-list 
     (map2 
       (lambda (ld-rec time) (list (first ld-rec)
                                   (second ld-rec)
                                   time
                                   (third ld-rec)
                                   (fourth ld-rec)
                                   (fifth ld-rec)
                                   (sixth ld-rec)    ; new as of
                                   (seventh ld-rec)  ; updated as of
                             )
       )
       lecture-description-list
       course-plan-time-list))

  ; Ensure that the html and html/images directories exists in course-plan-dir
  (if (equal? (relative-source-html-destination-path-fragment) "html/") (ensure-directory-existence! course-dir "html"))   ; else, the course plan user must make the dest. dir himself or herself.
  (ensure-directory-existence! (string-append course-dir (relative-source-html-destination-path-fragment)) "images")

  ; Copy images - as they come with the Course Plan tool - to html target directory
  (copy-files 
      course-plan-image-files
      (string-append course-plan-software-directory "images/")
      (string-append course-dir (relative-source-html-destination-path-fragment) "images/") )

  ; Copy images - as they appear in the images directory of the source directory - to html target directory
  (let ((image-source-dir (string-append course-dir "images/")))
   (if (directory-exists? image-source-dir)
    (copy-files 
      (directory-list image-source-dir)
      image-source-dir
      (string-append course-dir (relative-source-html-destination-path-fragment) "images/"))))


  ; Save course-plan-lecture-list in the internal directory: lecture-list.lsp

  (ensure-directory-existence! (string-append course-dir) "internal")

   (if (and (eq? scheme-system 'scm) (not (directory-exists? (string-append course-dir "internal/"))))
      (error (string-append "You must make a directory name 'internal' in the course directory: "
                              course-dir 
                              ". We save the value of lecture-description-list in a file in this directory")))

   (if (file-exists? (lecture-list-file))
       (delete-file (lecture-list-file)))

   (with-output-to-file (lecture-list-file)
       (lambda () (write course-plan-lecture-list)))

)

(define warning-counter 0)

(define (display-warning-1 message)
  (if course-plan-verbose? (display-warning message))
  (set! warning-counter (+ 1 warning-counter)))  

(define (check-redefinitions)
  (if (null? course-plan-subject-list) (display-warning-1 "You should define the variable course-plan-subject-list"))
  (if (null? lecture-description-list) (display-warning-1 "You should define the variable lecture-description-list"))
  (if (null? course-plan-time-list) (display-warning-1 "You should define the variable course-plan-time-list"))
  (if (not course-title) (display-warning-1 "You MUST define the variable course-title"))
  (if (not course-semester) (display-warning-1 "You MUST define the variable course-semester"))
  (if (not brief-course-semester) (display-warning-1 "You MUST define the variable brief-course-semester"))
  (if (not course-dir) (display-warning-1 "You MUST define the variable course-dir"))
  ; (if (not course-url-prefix) (display-warning-1 "You MUST define the variable course-url-prefix"))
  (if (not last-lecture-number) (display-warning-1 "You MUST define the variable last-lecture-number"))
  (if (not course-start) (display-warning-1 "You MUST define the variable course-start"))
  (if (not actual-exercise-interval) (display-warning-1 "You MUST define the function actual-exercise-interval"))
  (if (not (= (length course-plan-time-list) (length lecture-description-list)))
                         (display-warning "The course-plan-time-list and the lecture-description-list must be lists of equal lengths"))

  (if (and course-plan-verbose? (= 0 warning-counter)) (begin (display "Redefinitions in course description file OK") (newline)))
)

(define lecture-id-from-lecture-plan #f)

;; Write the plan of a given lecture to a html file.
;; The id parameter is mandatory. The keyword parameters are each of the form
;; (key value), where key is one of literature, guide, exercises, references, and misc.
;; The function writes the plan to an HTML file in the course html directory. The name of the
;; files will be identical to the name of the LAML file, in which the source (with the lecture-plan form) is located.
;; This function the central function for generation of individual lecture plans for lectures in the cource.
;; (Renamed in XML-in-LAML version)
(define (course-plan-lecture-plan id . keyword-parameters)
 (check-lecture-plan id)
 (set! lecture-id-from-lecture-plan id)
 (let ((the-lit (keyword-parameter 'literature keyword-parameters))
       (the-guide (keyword-parameter 'guide keyword-parameters))
       (the-ex  (keyword-parameter 'exercises keyword-parameters))
       (the-ref (keyword-parameter 'references keyword-parameters))
       (the-misc (keyword-parameter 'misc keyword-parameters)))
   (course-plan-lecture-plan-1 id the-lit the-guide the-ex the-ref the-misc)))


(define (check-lecture-plan id)
  (if (not (find-lecture id course-plan-lecture-list)) (display-error "The lecture-id (first parameter of course-plan-lecture-plan) is not found in the lecture description list"))
)


; End of top level functions.
; ----------------------------------------------------------------------------------------------------------------


; Subject selectors and locators related to course-plan-subject-list

(define subject-id car)
(define subject-title cadr)
(define subject-description caddr)

; Return a manually supplied URL of the subject structure - for linking to an external place
; A selector on a subject structure
(define subject-external-url 
  (lambda (x) (if (>= (length x) 4) (fourth x) #f)))

; Lookup a subject record with some given subject id. Return a record (a tripple list, or four element list).
(define (find-subject id subject-list)
  (cond ((null? subject-list) #f)
        ((eq? id (subject-id (car subject-list))) (car subject-list))
        (else (find-subject id (cdr subject-list)))))

; ---------------------------------------------------------------------------------------------------
; Lecture records selectores and locates, related to course-plan-lecture-list

; given an id, return a lecture record
(define (find-lecture id lecture-list)
  (cond ((null? lecture-list) #f)
        ((eq? id (lecture-id (car lecture-list))) (car lecture-list))
        (else (find-lecture id (cdr lecture-list)))))

; shortcut to find-lecture
(define (lec id)
  (find-lecture id course-plan-lecture-list))

; Return record number n of lecture-list.
; The first is assigned number one.
; If n is out of range, return #f
(define (find-lecture-by-number n lecture-list)
  (if (and (>= n 1) (<= n (length lecture-list)))
      (list-ref lecture-list (- n 1))
      #f))


; Selectors and locators are defined on the derived course-plan-lecture-list, see below.
; ---------------------------------------------------------------------------------------------------


; LOADING

(lib-load "color.scm")
(lib-load "time.scm")
(lib-load "file-read.scm")

(load (string-append laml-dir "tools/calendar/xml-in-laml/calendar.scm"))
; (load (string-append laml-dir "tools/calendar/calendar-lib.scm"))

; -----------------------------------------------------------------------------

(define target-extension "html")

; -----------------------------------------------------------------------------
; Called after loading of style and course specific information:

; The effective course-plan-lecture-list which merges lecture-description-list and course-plan-time-list
; Assigned by init-course-plan
(define course-plan-lecture-list '())


(define (course-plan-welcome)
  (display (string-append "The course plan style " course-plan-version))
  (newline))

; selectors of single lecture record in course-plan-lecture-list
(define (lecture-id rec) (list-ref rec 0))
(define (lecture-subject-id rec) (list-ref rec 1))
(define (lecture-absolute-start-time rec) (list-ref rec 2))  ; start på halvdag
(define (lecture-exercise-start-displacement rec) (car (list-ref rec 3)))
(define (lecture-exercise-length rec) (cadr (list-ref rec 3)))
(define (lecture-plenum-start-displacement rec) (car (list-ref rec 4)))
(define (lecture-plenum-length rec) (cadr (list-ref rec 4)))
(define (lecture-room rec) (list-ref rec 5))
(define (lecture-new-as-of rec) (list-ref rec 6))
(define (lecture-updated-as-of rec) (list-ref rec 7))

; Derived selectors:

(define (find-subject-by-lecture-id lecture-id course-plan-subjects)
  (let ((lecture-record (find-lecture lecture-id course-plan-lecture-list)))
    (if lecture-record
        (let ((subject-id (lecture-subject-id lecture-record)))
          (find-subject subject-id course-plan-subjects))
        (laml-error "find-subject-by-lecture-id: Cannot locate a lecture with lecture-id:" lecture-id))))
         

(define (lecture-plenum-start-time rec) 
  (+ (lecture-absolute-start-time rec) (lecture-plenum-start-displacement rec)))

(define (lecture-exercise-start-time rec) 
  (+ (lecture-absolute-start-time rec) (lecture-exercise-start-displacement rec))) 

; extended selectors, accessing static information:

(define (lecture-title lecture-rec)
  (let ((static-record (find-subject (lecture-subject-id lecture-rec) course-plan-subject-list)))
    (subject-title static-record)))

(define (lecture-description lecture-rec)
  (let ((static-record (find-subject (lecture-subject-id lecture-rec) course-plan-subject-list)))
    (subject-description static-record)))

(define (lecture-number lecture-rec)
  ; calculate lecture-rec's number in the sequence given of the course-plan-lecture-list
  (lecture-number-help (lecture-id lecture-rec) course-plan-lecture-list 1))

(define (lecture-number-help lec-id lecture-list n)
  (cond ((null? lecture-list) "??")
        ((eq? (lecture-id (car lecture-list)) lec-id) n)
        (else (lecture-number-help lec-id (cdr lecture-list) (+ n 1)))))

; time interval functions:
(define (lecture-lecture-interval lecture-rec)
  (let* ((start-time (lecture-plenum-start-time lecture-rec))
         (end-time (+ start-time (lecture-plenum-length lecture-rec))))
    (list start-time end-time)))

(define (lecture-exercise-interval lecture-rec)
  (let* ((start-time (lecture-exercise-start-time lecture-rec))
         (end-time (+ start-time (lecture-exercise-length lecture-rec))))
    (list start-time end-time)))


; -----------------------------------------------------------------------------
;;; Colors.
;;; The graphical idea of the course home page is to show the left frame in a dark color
;;; with white text, and the right large main frame in a light color with black text.
;;; The course plan document style supports color styles in terms of related colors and (currently) a single gif image file.
;;; You can make a new color scheme, and you can apply an existing color scheme.

; Concrete colors
(define yellow-1 (make-color 255 255 160))
(define purple-1 (make-color 204 126 250)) 
(define purple-2 (make-color 183 73 248))
(define dark-red (make-color 240 0 0))
(define yellow-3 (make-color 255 255 117))

; The dark color.
; Background color of the left frame.
(define the-dark-color purple-1)

; The light color.
; Background color of the right frame.
(define the-light-color yellow-1)

; The dark text color, used in the right main frame.
(define the-dark-text-color purple-2)

;; The color used to mark course entries in the calendar.
(define course-calendar-color yellow)

; The colors used by the course-plan software
(define overview-frame-bg the-dark-color)
(define overview-frame-text white)
(define overview-frame-link white)
(define overview-frame-vlink white)

(define contents-frame-bg the-light-color)
(define contents-frame-text black)
(define contents-frame-link black)  
(define contents-frame-vlink black)

(define actual-triangle "triangle.gif")

;; Return a color scheme in terms of a dark color (background of left frame), dark text color,
;; light color (background of large right frame) and a gif triangle file name (usually in the dark color).
(define (make-color-scheme dark dark-text light triangle-image)
  (list dark dark-text light triangle-image))

;; Apply a given color scheme
(define (apply-course-plan-color-scheme color-scheme)
 (let ((dark (car color-scheme))
       (dark-text (cadr color-scheme))
       (light (caddr color-scheme))
       (tri (cadddr color-scheme)))
  (set! the-dark-color dark)   (set! the-light-color light)
  (set! the-dark-text-color dark-text)
  (set! overview-frame-bg dark)
  (set! contents-frame-bg light)
 
  (set! actual-triangle tri)
))

;; A sample color scheme: Purple and yellow
(define course-plan-color-scheme-1 (make-color-scheme purple-1 purple-2 yellow-1 "triangle.gif"))

;; Another sample color scheme: Read and yellow.
(define course-plan-color-scheme-2 (make-color-scheme dark-red dark-red yellow-3 "triangle-99.gif"))

;; Another sample color scheme: Black and white.
(define course-plan-color-scheme-3 (make-color-scheme black black white "triangle-black.gif"))

;; Another sample color scheme:  Blue and grey.
(define course-plan-color-scheme-4 (make-color-scheme (make-color 0 64 128) (make-color 0 64 128) (make-color 222 222 222)  "triangle-00.gif"))

;; Another sample color scheme:  fall-colors
(define course-plan-color-scheme-5 (make-color-scheme (make-color 0 100 100) (make-color 0 100 100) (make-color 216 216 177)  "triangle-00a.gif"))

;; Another sample color scheme: 
(define course-plan-color-scheme-6 (make-color-scheme (make-color 153 83 72) (make-color 134 72 64) (make-color 255 223 191)  "triangle-red_brown.gif"))

;; Another sample color scheme: 
(define course-plan-color-scheme-7 (make-color-scheme (make-color 225 113 0) (make-color 225 113 0) (make-color 173 214 214)  "triangle-dark_orange.gif"))

;; Another sample color scheme: 
(define course-plan-color-scheme-8 (make-color-scheme (make-color 225 113 0) (make-color 196 98 0) (make-color 201 201 148)  "triangle-dark_orange.gif"))

(define course-plan-color-scheme-9 (make-color-scheme (make-color 113 113 0) (make-color 100 100 0) (make-color 255 255 180)  "triangle-olive_yellow.gif"))

(define course-plan-color-scheme-10 (make-color-scheme (make-color 1 133 156) (make-color 1 125 148) (make-color 255 226 198)  "triangle-blue-creme.gif"))

(define course-plan-color-scheme-11 (make-color-scheme (make-color 1 133 156) (make-color 1 125 148) (make-color 255 255 255)  "triangle-blue-creme.gif"))


; Map a friendly color scheme name (a string) to a color scheme (a list structures of colors etc).
; Frindly color names: "purple-yellow", "red-yellow", "black-white", "blue-grey", "green-brown". 
; The default is the color scheme corresponding to "green-brown"
(define (get-color-scheme friedly-scheme-name)
  (cond ((equal? (as-string friedly-scheme-name) "purple-yellow") course-plan-color-scheme-1)
        ((equal? (as-string friedly-scheme-name) "red-yellow") course-plan-color-scheme-2)
        ((equal? (as-string friedly-scheme-name) "black-white") course-plan-color-scheme-3)
        ((equal? (as-string friedly-scheme-name) "blue-grey") course-plan-color-scheme-4)
        ((equal? (as-string friedly-scheme-name) "green-brown") course-plan-color-scheme-5)
        ((equal? (as-string friedly-scheme-name) "brownish-yellow") course-plan-color-scheme-6)
        ((equal? (as-string friedly-scheme-name) "orange-blue") course-plan-color-scheme-7)
        ((equal? (as-string friedly-scheme-name) "orange-brownish") course-plan-color-scheme-8)
        ((equal? (as-string friedly-scheme-name) "olive-yellow") course-plan-color-scheme-9)
        ((equal? (as-string friedly-scheme-name) "blue-creme") course-plan-color-scheme-10)
        ((equal? (as-string friedly-scheme-name) "blue-white") course-plan-color-scheme-11)
        (else course-plan-color-scheme-5)))



; -----------------------------------------------------------------------------
; IMAGES

; A list of all course-plan relevant images, which need to be copied to the html directory
(define course-plan-image-files 
  (list "triangle.gif" "triangle-99.gif" "triangle-black.gif" "triangle-00.gif" "triangle-00a.gif" "triangle-red_brown.gif" "triangle-dark_orange.gif" "triangle-olive_yellow.gif"
        "triangle-blue-creme.gif"
        "new.gif" "updated.gif" "ny.gif" "opdateret.gif"))

; -----------------------------------------------------------------------------
; Misc functionality

; Return same time next week
(define (week+ n)
  (* n (* 7 seconds-in-a-day)))

(define (last-update)
 (let* ((tm (current-time))
        (day (weekday tm))
        (dt (date-time tm))
        (date (car dt))
        (time (cadr dt)))
  (font 'size "1" 'color (rgb-color-encoding black) (text-choice "Genereret:" "Generated:") (capitalize-string-nd day) date _ ","  time 
                     (generating-system))))

(define (laml-course-plan-notice)
  (font-1 1 black
     (text-choice
       (con "Programmeret af Kurt Nørmark med brug af " (a 'href "http://www.cs.auc.dk/~normark/laml/" "LAML") "teknologi")
       (con "Programmed by Kurt Nørmark using" (a 'href "http://www.cs.auc.dk/~normark/laml/" "LAML") "technology"))))

;; Return a string which tells about the generating computer system.
;; Default: the empty string.
(define (generating-system)
 "")

; Return the duration of the time interval time-int in seconds.
(define (clock-interval-duration time-int)
    (let ((dt1 (car time-int))
          (dt2 (cadr time-int)))
      (- dt2 dt1)))

; Return a clock interval string given a list of two time numbers (second counts).
(define (clock-interval time-int)
  (let ((dt1 (date-time-1 (car time-int)))
        (dt2 (date-time-1 (cadr time-int))))
    (string-append (cadr dt1) " - " (cadr dt2))))

; Return a clock interval string given a list of two time numbers (second counts).
(define (extended-clock-interval time-int)
  ; includes weekday of (first time-int), date, and hour/minute interval.
  ; Returns a string in a particular format.
  (let ((dt1 (date-time-1 (car time-int)))
        (dt2 (date-time-1 (cadr time-int))))
    (string-append (weekday (car time-int)) " " (car dt1) (text-choice " klokken " " at ") (cadr dt1) " - " (cadr dt2))))

; variant of day-time from time.scm:
(define (date-time-1 second-count)
 "Return a list of two strings (date time)"
 (let ((time-list (time-decode second-count)))
  (let* ( (y (first time-list))
          (year (modulo y 100))
          (month (second time-list))
          (day (third time-list))
          (hours (fourth time-list))
          (minutes (fifth time-list))
          (seconds (sixth time-list)))
    (list 
      (if (eq? language-preference 'danish)
          (string-append (number->string day) "."
                         (number->string month) "."
                         (number->string y))
          (string-append (get-month-name month) " "
                         (number->string day) ", "
                         (number->string y)))
      (string-append (zero-pad-string (number->string  hours)) "."
                     (zero-pad-string (number->string  minutes))
                     )))))

; 98: triangle.gif
; 99: triangle-99.gif
(define (triangle-ruler)
  (table-1 0 '(39 3000) (make-list 2 contents-frame-bg) 
    (list (list (img-0 (string-append "images/" actual-triangle)) (hr))) "middle"))


; Return a slice for the lecture-plan 
; If not condition, return an empty slice (empty list)
(define (lecture-plan-slice condition left right)
  (if condition
      (list 
        (list (font-1 4 the-dark-text-color (b left))
              (div right (p))))
      '()))

(define (course-plan-lecture-plan-1 id lit guide exerc refs misc)
 (let ((empty-contribution? null?)
       (rec       (find-lecture id course-plan-lecture-list))
      )
  (if rec
      (let* (  
             (n         (lecture-number rec))
             (next-rec  (find-lecture-by-number (+ n 1) course-plan-lecture-list))
             (titl      (lecture-title rec))
             (descr     (lecture-description rec))
             (lec-int   (lecture-lecture-interval rec))
             (ex-int    (actual-exercise-interval rec next-rec))
             (dt        (date-time-1 (car lec-int)))
             (room      (lecture-room rec))
             (nothing-to-insert '())
             )

        (write-course-html-file
         (course-page
          (con (text-choice "Lektion " "Session ") (as-string n))
          (table-1 0 '(175 600) (make-list 2 contents-frame-bg) 
                   (append
                    (lecture-plan-slice #t 
                                        (text-choice "Emne" "Subject")
                                        (con titl (p) (div 'css:font-style "italic" descr)))

                    (lecture-plan-slice #t
                                        (text-choice "Dato" "Date")
                                        (con (capitalize-string-nd (weekday (car lec-int))) " " (car dt)))

                    (if (> (clock-interval-duration lec-int) 0)
                        (lecture-plan-slice #t
                                            (text-choice "Plenum" "Plenum Session")
                                            (con (text-choice "Klokken " " ") (clock-interval lec-int) (text-choice " i " " in ") room))
                        nothing-to-insert)


                    (lecture-plan-slice (not (empty-contribution? lit))
                                        (text-choice "Litteratur" "Literature")
                                        (con lit (br)))

                    (lecture-plan-slice  (not (empty-contribution? guide))
                                         ""
                                         (i (font-size 2 (con (text-choice "Læsevejledning: " "Reading guide: ")  guide ))))

                    (if (and ex-int (> (clock-interval-duration ex-int) 0))  ; no exercises presented  if ex-int is #f - forskudte øvelser sidste lektion.
                        (lecture-plan-slice (not (empty-contribution? exerc))
                                            (text-choice "Øvelser" "Exercises")
                                            (con (if ex-int 
                                                     (if floating-exercises 
                                                         exerc
                                                         (con (text-choice "Øvelserne til denne lektion afholdes " 
                                                                       "The exercises of this session take place ")
                         
                                                          (decapitalize-string-nd (extended-clock-interval ex-int)) (p) exerc))
                                                     exerc
                                                     )
                                                 )
                                            )
                        nothing-to-insert)

                    (lecture-plan-slice (not (empty-contribution? refs))
                                        (text-choice "Referencer" "References")
                                        (con refs))

                    (lecture-plan-slice (not (empty-contribution? misc))
                                        (text-choice "Diverse" "Miscellaneous")
                                        (con misc))
                    ))
          '() '() 2
          )
         (string-append (as-string id) "-lecture")
         ))
   (laml-error "course-plan-lecture-plan-1:" "Cannot find lecture with lecture-id:" id))))
     


; -----------------------------------------------------------------------------
; Keyword handling

; (define (lecture-identification id)
;  (cons 'lecture-identification id))
;
; (define (literature lit)
;   (cons 'literature lit))
; 
; (define (exercises ex)
;   (cons 'exercises ex))
; 
; (define (references ref)
;   (cons 'references ref))
; 
; (define (guide g)
;   (cons 'guide g))
; 
; (define (misc m)
;   (cons 'misc m))



; A function for which returns keyword specific information. 
; If no relevant keyword parameter is present, this function returns an empty string
(define (keyword-parameter key par-list)
 (let ((res (assq key par-list)))
   (if res (cdr res) "")))

;;; Course overviews.
;;; This sections contains a single function which generates convenient overviews of a course, including
;;; the 'home page' of the course. A call of this function is typically made by the LAML file which produces
;;; the course introduction, which must have the name "course-introduction".

; ===============================================================================================================
; COURSE HOME PAGE

; Generates and returns a course home page with two frames named overview and main.
(define (course-home-page overview-page contents-page)
 (let ((xhtml-frameset:frameset (xhtml10-frameset 'frameset))
       (xhtml-frameset:frame    (xhtml10-frameset 'frame))
       (xhtml-frameset:html     (xhtml10-frameset 'html))
       (xhtml-frameset:title    (xhtml10-frameset 'title))
       (xhtml-frameset:head    (xhtml10-frameset 'head))
       (xhtml-frameset:link    (xhtml10-frameset 'link))
      )
  (let ((shortcut-clause
	 (if shortcut-icon
	     (xhtml-frameset:link 'rel "SHORTCUT ICON" 'href (string-append "images/shortcut-icon.ico"))
	     '())))
    (xhtml-frameset:html 
     (xhtml-frameset:head (xhtml-frameset:title (string-append course-title " - " course-semester))
                          shortcut-clause
			  )
     (xhtml-frameset:frameset 
      (con (xhtml-frameset:frame 'name "overview" 'src (string-append overview-page ".html") 'scrolling "auto")
           (xhtml-frameset:frame 'name "main" 'src (string-append contents-page ".html") 'scrolling "auto")
	   )
      'cols (string-append (as-string overview-pixel-width) ",*"))))))

; Writes a course home page to a file in the course html directory.
(define (write-course-home-page file-name overview-page contents-page)
 (write-course-html-file
      (course-home-page overview-page contents-page)
      file-name))


; ---------------------------------------------------------------------------------------------------
; COURSE OVERVIEW PAGE

; Generate and return a course overview in terms of lecture number, weekday, date, time, and subject.
(define (course-overview)
  (table-2 1 (list 50 50 120 50 60 300) (make-list 6 contents-frame-bg)
    (list (text-choice "Kursusgang" "Session number") 
          (text-choice "Ugedag" "Weekday")
          (text-choice  "Dato" "Date")
          (text-choice  "Klokken" "Time")
          (text-choice  "Lokale" "Room")
          (text-choice  "Emne" "Subject"))
    (map overview-entry course-plan-lecture-list)))

(define (overview-entry lecture-rec)
 (let ((d-t (date-time-1 (lecture-absolute-start-time lecture-rec))))
  (list (as-string (lecture-number lecture-rec))
        (capitalize-string-nd (weekday (lecture-absolute-start-time lecture-rec)))
        (car d-t)
        (cadr d-t)
        (lecture-room lecture-rec)
        (lecture-title lecture-rec))))

; Write a course overview (as generated by course-overview) to a file in the course html directory.
(define (write-course-overview file-name)
  (write-course-html-file
    (course-page
      (text-choice "Kursusoversigt" "Course overview")
      (course-overview)
      '() '() 2
    )
    file-name))


; ---------------------------------------------------------------------------------------------------
; COURSE SUMMARY PAGE

; Generate and return a course summary in terms of lecture number, title and description (taken from course-plan-subject-list).
(define (course-description-summary)
  (table-2 1 (list 50 150 450) (make-list 3 contents-frame-bg)
    (list (text-choice "Kursusgang" "Lecture number")
          (text-choice  "Emne" "Subject")
          (text-choice  "Beskrivelse" "Description"))
    (map description-entry course-plan-lecture-list)))

(define (description-entry lecture-rec)
 (let* ((st (lecture-absolute-start-time lecture-rec))
        (dt (date-time st))
        (wd (capitalize-string-nd (weekday st))))
  (list (con (as-string (lecture-number lecture-rec)) (br)
             (font-1 1 grey1 (con wd " " (car dt))))
        (lecture-title lecture-rec)
        (div 'css:font-size "75%" (lecture-description lecture-rec)))))

; Write a course summary (as generated by course-description-summary) to a file in the course html directory.
(define (write-course-description-summary file-name)
  (write-course-html-file
    (course-page
      (text-choice "Lektionsbeskrivelser" "Lecture Descriptions")
      (course-description-summary)
      '() '() 2
    )
    file-name))

; -----------------------------------------------------------------------------
; COURSE OVERVIEW FRAME

(define (standard-overview-links)
  (list 
     (list "course-introduction.html" (text-choice "Introduktion" "Introduction") "main")
     (list "course-overview.html"     (text-choice "Kursusoversigt" "Course overview") "main")
     (list "course-summary.html"      (text-choice "Lektionsbeskrivelser" "Lecture descriptions") "main")
     (list (if url-of-external-calendar url-of-external-calendar "course-calendar.html") (text-choice "Kursuskalender" "Course calendar") "main")))

(define overview-url (make-selector-function 1 "overview-url"))
(define overview-anchor-text (make-selector-function 2 "overview-anchor-text"))
(define overview-target (make-selector-function 3 "overview-target"))
(define (new-as-of lst)
  (if (>= (length lst) 4) (fourth lst) #f))
(define (updated-as-of lst)
  (if (>= (length lst) 5) (fifth lst) #f)) 


; Return a course overview frame, to be presented as the leftmost navigation bar in a frame set.
; n: reveal n links in the lecture list.
; Uses the function general-overview-links which is assumed to be defined in the course-description file.
(define (course-overview-frame n)
  (let ((anchor-p-list (append (make-list n #t) (make-list (- (length course-plan-lecture-list) n) #f)))
        (ttl (text-choice "Indhold" "Contents"))
        (body-color-attributes 
         (list 'bgcolor (rgb-color-encoding overview-frame-bg) 'text (rgb-color-encoding overview-frame-text)
	       'link (rgb-color-encoding overview-frame-link) 'vlink (rgb-color-encoding overview-frame-vlink)))
        (laml-generation-meta (meta 'name "Generator" 'content "LAML"))
       )

     (html 
      (head laml-generation-meta (title ttl))
      (body body-color-attributes 
        (con 
	    (b (con brief-course-title " - " brief-course-semester)) (br)

	    (ul (li-mapper
	     (map (lambda (link) (span (a-tag-target (overview-url link) (font-1 2 overview-frame-text (overview-anchor-text link)) (overview-target link))
                                       (new-and-or-updated-icon-if-appropriate (new-as-of link) (updated-as-of link) new-or-updated-n-days-attr)))
		  (append (standard-overview-links) (general-overview-links)))))

	    (p)
          
	    (b (text-choice "Lektioner:" "Lectures:")) (br)
	    (ol (li-mapper
	     (map2 course-overview-frame-entry course-plan-lecture-list anchor-p-list)))

            (if (and (eq? course-plan-editing-mode 'laml-form) course-id)
                (con 
                  (vertical-space 2)
                  (a 'target "_top" 
                     'href (string-append laml-forms-web-interface-url-prefix "present-form.cgi" "?" "course-plan-name" "=" course-id) (font-size 2 "Edit Course Plan")))
                '())

)))))


; Is there an external URL from lecture-id.
; I.e., an URL given in the subject record.
(define (external-url-from? lecture-id)
 (let ((subject-descr (find-subject lecture-id course-plan-subject-list)))
   (and subject-descr (subject-external-url subject-descr))))


; Return a relative URL to a us lecture page given the lecture id.
; If the subject record contains a fourth external URL field, use this instead.
(define (us-relative-url-perhaps-external lecture-id)
 (let ((subject-descr (find-subject-by-lecture-id lecture-id course-plan-subject-list)))
   (if (and subject-descr (subject-external-url subject-descr))
       (subject-external-url subject-descr)
       (us-relative-url lecture-id))))

(define (course-overview-frame-entry lecture-rec anchor-p)
 (span
   (if anchor-p
       (a-tag-target (us-relative-url-perhaps-external (lecture-id lecture-rec))  (font-1 2 overview-frame-text (lecture-title lecture-rec)) "main")
       (font-1 2 overview-frame-text (lecture-title lecture-rec)))
   (horizontal-space 2) 
   (new-and-or-updated-icon-if-appropriate (lecture-new-as-of lecture-rec) (lecture-updated-as-of lecture-rec) new-or-updated-n-days-attr)))


; Write left frame at top level via the function course-overview-frame.
; The file is written to the course html directory.
; file-name is without extension or path information.
; reveal n links in the lecture list.
(define (write-course-overview-frame file-name n)

  ; The exernal version:
  (write-course-html-file
      (course-overview-frame n)
      file-name)
 
  ; The internal version
  (write-course-html-file
      (course-overview-frame (length lecture-description-list))
      (string-append file-name "-" "internal"))
)

; ---------------------------------------------------------------------------------------------------------------
; Course calendar functions.

(define calendar-events '()) ; assigned by course-calendar

; Generate and return a calendar in which the lectures in the course are plotted in.
; As parameter we pass  number-of-months from from-year, from-month with some given calendar title.
; It is also possible to pass external calendar entries in the format required by the LAML calendar tools
; (see the documentation of calendar-events in this tool).
(define (course-calendar from-year from-month number-of-months calendar-title external-calendar-entries)
  (set! calendar-events 
    (append (map course-calendar-entry course-plan-lecture-list) external-calendar-entries))
  (set! calendar-division-of-day course-plan-calendar-division-of-day)
  (calendar from-year from-month number-of-months #f course-plan-number-of-calendar-divisions))

; Write a calendar via course-calendar on the file in the course html directory.
(define (write-course-calendar file-name from-year from-month number-of-months calendar-title external-entries css-link-clause css-embedded-style-clause)
  (write-course-html-file
    (course-page
      calendar-title
      (course-calendar from-year from-month number-of-months calendar-title external-entries)
      css-link-clause
      css-embedded-style-clause
    )
    file-name))

;; Export this course's calendar entries to to-file.
;; Only this course's entries are exported. The export format is the 'native' calendar format,
;; as defined by the interactive, server-based LAML calendar tool. URLs to lectures are made absolute via the exporting.
;; If to-file exists, overwrite it.
(define (export-course-calendar-entries to-file)
  (file-write
    (map export-course-calendar-entry course-plan-lecture-list)
    to-file))


; Returns a native calendar entry from rec.
; This variant includes a relative url to lectures
(define (course-calendar-entry rec)
 (let ((n (lecture-number rec)))
  (list (lecture-absolute-start-time rec)
        (- (+ (lecture-absolute-start-time rec) (lecture-plenum-length rec)) 1) ;  (- x 1): just keep it within the half day  ; earlier: (* 2 seconds-in-an-hour)
        (string-append brief-course-title " " (as-string n))
        ""
        course-calendar-color
        (if (<= n last-lecture-number) (us-relative-url-perhaps-external (lecture-id rec)) ""))))

; Returns a native calendar entry from rec.
; This variant includes an absolute url to lectures. This is the only difference between this function and course-calendar-entry
; Precondition: course-url-prefix must be non #f.
(define (export-course-calendar-entry rec)
 (let ((n (lecture-number rec)))
  (list (lecture-absolute-start-time rec)
        (- (+ (lecture-absolute-start-time rec) (lecture-plenum-length rec)) 1)  ; (- x 1): just keep it within the half day; earlier: (* 2 seconds-in-an-hour)
        (string-append brief-course-title " " (as-string n))
        (string-append brief-course-title " " (as-string n))  ; identical brief and long descriptions
        course-calendar-color
        (if (<= n last-lecture-number)
            (if (external-url-from? (lecture-id rec))
                (us-relative-url-perhaps-external (lecture-id rec))
                (string-append course-url-prefix (us-relative-url (lecture-id rec))))
            ""))))

;; Make course overview pages, including the course home page, and the course calendar.
;; home-page-name is the name of the home page file, without extension and without path information.
;; cal-year, cal-month, and cal-number-of-months determine the calendar range.
;; cal-title is used to define the title of the calendar.
;; external-calendar-entries is used to pass additional calendar entries to the course calendar (a list of entries).
;; The entries must be in the 'calendar-events' format, as documented in the LAML calendar tool.
(define (make-course-overview-pages home-page-name cal-year cal-month cal-number-of-months cal-title external-calendar-entries)
  (write-course-overview "course-overview")
  (write-course-description-summary "course-summary")
  (write-course-overview-frame "course-contents-frame" last-lecture-number)

  (write-course-home-page home-page-name "course-contents-frame" "course-introduction")  ; the external one
  (write-course-home-page (string-append home-page-name "-" "internal") "course-contents-frame-internal" "course-introduction")  ; the internal one

  (set! month-width (ensure-min-max (quotient preferred-calendar-pixel-width cal-number-of-months) 120 250))
  (set! calendar-background-color white)
  (write-course-calendar
     "course-calendar" cal-year cal-month cal-number-of-months
      cal-title external-calendar-entries
      '() ; (link 'href "calendar.css" 'rel "stylesheet" 'type "text/css")
      (style 'type "text/css" "TABLE.calendar-table TD { font-size: 7pt; }")
  )

)

; Return x if within the interval [a..b].
; If x is larger than b, return b. If x is smaller than a, return a.
; Precond: a <= b.
(define (ensure-min-max x a b)
 (cond ((<= x a) a)
       ((>= x b) b)
       (else x)))
  


; -----------------------------------------------------------------------------
;;; Course plan exercise functions.
;;; The course-exercise function may, for instance, be called after the lecture-plan function in order to
;;; define an excercise for the lecture. Each exercise is written on a separate HTML file. The URL function 
;;; course-exercise-ref can be used to refer to the exercises from the lecture-plan.

;; Define an exercise with a certain id (symbol), a number, and a body.
;; The exercise belongs to lecture-id, which is optional in cases the course-exercise follows a lecture-plan clause.
;; id is a symbol identifying the execise. 
;; number is the execise number or letter.
(define (course-exercise id number body . lecture-id)
  (let* ((lecture-id-1 (if (null? lecture-id) lecture-id-from-lecture-plan (car lecture-id))))
   (if (not lecture-id-1) (error "course-exercise: Cannot locate lecture-id. Probably because of no previous lecture-plan form."))
   (let* ((lecture-name (as-string lecture-id-1))
          (lec-rec (find-lecture lecture-id-1 course-plan-lecture-list))
          (lec-title (lecture-title lec-rec)))
    (write-course-html-file
      (course-page 
        (string-append (con lec-title (br) (text-choice "Opgave " "Exercise ") (as-string number)))
        body)
      (string-append (exercise-name id lecture-name))))))


(define (exercise-name id lecture-id)
  (string-append (as-string lecture-id) "-" "exercise" "-" (as-string id)))


;; Return an URL to an exercise.
;; The URL is relative, and only works from HTML files in the course html directory.
;; id is a symbol identifying the execise. 
;; number is the execise number, a letter or an integer.
;; lecture-id identifies the lecture, to which the exercise belongs.
;; The function return a link to the exercise, and it is
;; only applicable from a lecture plan page. See the function exercise-url for an absolute url.
(define (course-exercise-ref id number lecture-id)
  (a-tag (string-append (exercise-name id lecture-id) ".html")
         (string-append (text-choice "Opgave " "Exercise ") (as-string number))))


; -----------------------------------------------------------------------------
;;; URL functions.
;;; These functions define a few useful URL functions. The user is encouraged to write more such
;;; functions in the course description file.

;; Return a relative URL to a particular lecture page (as generated by the lecture-plan function).
(define (us-relative-url id)
  (string-append  (as-string id) "-lecture" ".html"))

;; Return an absolute URL to a particular exercise.
;; Precondition: course-url-prefix must be non #f.
(define (exercise-url lecture-id exercise-id)
  ; absolute exercise url
  (string-append course-url-prefix (relative-source-html-destination-path-fragment) (exercise-name exercise-id lecture-id) ".html"))

;; Return an absolute URL to lecture number n in this course.
;; Precondition: course-url-prefix must be non #f.
(define (lecture-number-url n)
  ; absolute lecture us url given a lecture number n
  (let ((lectureid (lecture-id (list-ref course-plan-lecture-list  (- n 1)))))
    (string-append course-url-prefix (relative-source-html-destination-path-fragment)  (as-string lectureid) "-lecture" ".html")))


; ---------------------------------------------------------------------------------------------------------------
;;; Internal functions of interest.
;;; Here we wil document a few internal functions which may turn out to be useful in some contexts.

;; Return a course page (with standard graphical elements, header, body and some footer stuff (bottom-links)).
;; This functions returns an HTML value (AST).
;; Use this function together with write-course-html-file if you want to make additional course plan files.
;; The title parameter is without course name.
;; If the global variable news-flash-level >= news-flash-sensitity (the 3rd optional parameter) then show the newsflash.
(define (course-page the-title the-body . optional-parameter-list)
 (let ((css-link-clause (optional-parameter 1 optional-parameter-list '()))
       (css-embedded-style-clause (optional-parameter 2 optional-parameter-list '()))
       (shortcut-clause
          (if shortcut-icon
              (link 'rel "SHORTCUT ICON" 'href (string-append "images/shortcut-icon.ico"))
              '()))
       (news-flash-sensitivity (optional-parameter 3 optional-parameter-list 10)) ; 10 is so high that the newsflash-string never is shown pr. default.
       (laml-generation-meta (meta 'name "Generator" 'content "LAML"))
      )
  (let ((body-color-attributes 
         (list 'bgcolor (rgb-color-encoding contents-frame-bg) 'text (rgb-color-encoding contents-frame-text)
	       'link (rgb-color-encoding contents-frame-link) 'vlink (rgb-color-encoding contents-frame-vlink)))
       )
    (html
     (head 
       laml-generation-meta
       (title (con course-title the-title))
       shortcut-clause
       css-link-clause css-embedded-style-clause)
     (body 
      (h1 (font 'color (rgb-color-encoding the-dark-text-color) (con course-title (br) the-title))) 

      (if (>= news-flash-level news-flash-sensitivity) (course-plan-news-flash 4 500) (p))

      (triangle-ruler) (p)

      the-body

      (p) (hr) (p)

      (table-1 0 '(250 450) (make-list 2 contents-frame-bg) 
	       (list (list (con teacher-name (p) (last-update) (br) (laml-course-plan-notice)) (bottom-link-contribution))))

      body-color-attributes
      )))))

;; Write the contents in the course's html file.
;; The contents is typcically an activation of course-page.
;; File is without extension and without any path information.
;; This function adds an html extension.
;; Use this function together course-page if you want to make additional course plan files.
(define (write-course-html-file contents file)
  (write-html (list course-plan-html-rendering-mode 'prolog)
    contents
    (string-append course-dir (relative-source-html-destination-path-fragment) file ".html")))


;;; Utility functions.
;;; Here are some functions which are useful when producing course home pages.

;; Return a list of times, length n, starting with start, and with the period period.
;; This function can be used to produce the course-plan-time-list in case of a regular course rythm.
(define (periodic start n period)
  (cond ((= n 0) '())
        ((> n 0) (cons start (periodic  (+ start period) (- n 1) period)))
        ((< n 0) (error "Periodic: negative counter"))))


; The location of calendar facility's input data
(define calendar-facility-data-location "/user/normark/.public_html/cgi-bin/calendar/data/")

; Return the calendar event entries from a calendar made by
; my calendar facility, as present in the calendar-facility-data-location directory.
(define (calendar-facility-entries key)
  (let ((entries (file-read (string-append calendar-facility-data-location key))))
    (map 
      make-calendar-event  ; from calendar-lib
      (cdr (assoc "entries" entries)))))


; Make an empty index file in the hmtl directory.
(define (write-empty-index-file)
  (write-course-html-file
  ""
  "index"))

; Ensure that the file with path (string-append prefix-dir dir) exists.
; Create it if necessary.
(define (ensure-directory-existence! prefix-dir dir)
  (if (not (directory-exists? (string-append prefix-dir dir)))
      (make-directory-in-directory prefix-dir dir)))

(define gmap (curry-generalized map))
(define li-mapper (gmap li))




; ---------------------------------------------------------------------------------------------------
; XML-in-LAML bridge part


; Take data from the course-plan-ast (the overall AST) and transfer it to the variables of
; the original LAML course-plan facility.
(define (do-course-plan-xml-in-laml-bridging! course-plan-ast)
 (let* (
        (course-info-ast (traverse-and-collect-first-from-ast course-plan-ast (ast-of-type? 'element-name "course-info") id-1))
        (course-info-alist (propertylist-to-alist (ast-attributes course-info-ast)))

        (lecture-list-ast (traverse-and-collect-first-from-ast course-info-ast (ast-of-type? 'element-name "lecture-list") id-1))
        (lecture-asts (traverse-and-collect-all-from-ast lecture-list-ast (ast-of-type? 'element-name "lecture") id-1))
        (lectures (traverse-and-collect-all-from-ast lecture-list-ast (ast-of-type? 'element-name "lecture") transform-lecture-ast-to-original))

        (time-list-ast (traverse-and-collect-first-from-ast course-info-ast (ast-of-type? 'element-name "time-list") id-1))
        (times (if time-list-ast
                   (traverse-and-collect-all-from-ast time-list-ast (ast-of-type? 'element-name "time") transform-time-ast-to-original)  ; and check that all lectures have a time
                   (map transform-time-ast-to-original (collect-times-from-lectures lecture-list-ast))))

        (cut-length (min (length times) (length lectures)))
        (times-abbreviated (list-prefix times cut-length))
        (lectures-abbreviated (list-prefix lectures cut-length))
        (lecture-asts-abbreviated (list-prefix lecture-asts cut-length))

        (times-from-lectures (map (lambda (lecture-ast) (traverse-and-collect-first-from-ast lecture-ast (ast-of-type? 'element-name "time") transform-time-ast-to-original)) lecture-asts-abbreviated))

        (subject-list-ast (traverse-and-collect-first-from-ast course-info-ast (ast-of-type? 'element-name "subject-list") id-1))
        (subjects (if subject-list-ast
                      (traverse-and-collect-all-from-ast subject-list-ast (ast-of-type? 'element-name "subject") transform-subject-ast-to-original)
                      #f))

        (index-links-ast (traverse-and-collect-first-from-ast course-info-ast (ast-of-type? 'element-name "index-links") id-1))
        (index-links (traverse-and-collect-all-from-ast index-links-ast (ast-of-type? 'element-name "link-entry") transform-link-asts-to-original))

        (bottom-links-ast (traverse-and-collect-first-from-ast course-info-ast (ast-of-type? 'element-name "bottom-links") id-1))
        (bottom-links (traverse-and-collect-all-from-ast bottom-links-ast (ast-of-type? 'element-name "link-entry") transform-link-asts-to-original))

        (course-intro-ast (traverse-and-collect-first-from-ast course-plan-ast (ast-of-type? 'element-name "course-intro") id-1))

        (lecture-plan-par-lists (traverse-and-collect-all-from-ast course-plan-ast (ast-of-type? 'element-name "lecture-plan") transform-lecture-plan-ast-to-original))
       )

   ; Either the course-plan-subject-list is formed from the "subject" elements, or they are generated from the "lecture elements"
   ; If one or more "subject" elements exits in a subject-list, use these. Else generated them from the "lecture"s.
   ; It is NOT possible to mix "subjects" and inlined subjects in "lecture"s.
   (set! course-plan-subject-list
         (if subjects
             subjects
             (make-subject-list-from-lecture-asts lecture-asts)))

   (set! lecture-description-list lectures-abbreviated)
   (set! course-plan-time-list
      (map (lambda (t-timelist t-lecturelist) (if t-lecturelist t-lecturelist t-timelist)) times-abbreviated times-from-lectures)) 

   (set! course-plan-bottom-links bottom-links)
   (set! course-plan-index-links index-links)

   (set! course-start (if (null? times-abbreviated) 0 (apply min course-plan-time-list)))

   (set! language-preference (as-symbol (defaulted-get 'language-preference course-info-alist "english")))
   (set! new-or-updated-n-days-attr (as-number (defaulted-get 'new-or-updated-n-days course-info-alist "3")))
   (set! course-title (get 'course-title course-info-alist))
   (set! course-id (if (bound? 'course-plan-name) course-plan-name #f)) ; cource-plan-name bound in LAML forms contexts
   (set! brief-course-title (get 'brief-course-title course-info-alist))
   (set! course-semester (get 'course-semester course-info-alist))
   (set! brief-course-semester (get 'brief-course-semester course-info-alist))
   (set! teacher-name (get 'teacher-name course-info-alist))
   (set! course-dir (defaulted-get 'course-directory course-info-alist (startup-directory)))
   (set! course-url-prefix (if (and (eq? course-plan-editing-mode 'laml-form) course-id)
                               (string-append laml-forms-web-interface-url-prefix "data/" course-id "/")
                               (defaulted-get 'course-url-prefix course-info-alist #f)))
   (set! author-home-url (get 'author-home-url course-info-alist))
   (set! make-overview-pages? (as-boolean (defaulted-get 'make-overview-pages course-info-alist #f)))
   (apply-course-plan-color-scheme (get-color-scheme (defaulted-get 'color-scheme course-info-alist "course-plan-color-scheme-5")))
   (set! last-lecture-number (as-number (defaulted-get 'last-lecture-number course-info-alist (length lecture-description-list))))
   (set! relative-source-destination-path (defaulted-get 'relative-source-destination-path course-info-alist "html/"))

   (set! course-plan-calendar-start-year (as-number (defaulted-get 'calendar-start-year course-info-alist (calculate-calendar-start-year course-plan-time-list))))
   (set! course-plan-calendar-start-month (as-number (defaulted-get 'calendar-start-month course-info-alist (calculate-calendar-start-month course-plan-time-list))))
   (set! course-plan-calendar-number-of-months (as-number (defaulted-get 'calendar-number-of-months course-info-alist (calculate-calendar-number-of-months course-plan-time-list))))
   (set! url-of-external-calendar (defaulted-get 'url-of-external-calendar course-info-alist #f))

   (set! actual-exercise-model (as-symbol (defaulted-get 'exercise-model course-info-alist "this-exercise-slot")))
   (set! floating-exercises (as-boolean (defaulted-get 'floating-exercises course-info-alist "false")))
   (set! news-flash-string (defaulted-get 'news-flash-string course-info-alist ""))
   (set! news-flash-url (defaulted-get 'news-flash-url course-info-alist #f))
   (set! news-flash-level (as-number (defaulted-get 'news-flash-level course-info-alist 0)))

   (set! shortcut-icon (as-boolean (defaulted-get 'shortcut-icon course-info-alist #f)))

   (init-course-plan)
   
   ; Make the course plan overview pages:
   (if make-overview-pages? 
       (make-course-overview-pages (source-filename-without-extension) 
                                   course-plan-calendar-start-year course-plan-calendar-start-month course-plan-calendar-number-of-months
                                   (text-choice "Kursuskalender" "Course calendar") extra-calendar-entries))

   ; Make course intro:
   (if course-intro-ast
       (let ((intro-contents (ast-subtrees course-intro-ast)))
         (write-html 
	  (list course-plan-html-rendering-mode 'prolog)
	  (course-page (text-choice "Introduktion" "Introduction") intro-contents '() '() 1)
	  (string-append course-dir (relative-source-html-destination-path-fragment) "course-introduction" ".html"))))

   ; Make the lecture pages (Danish term: ugesedler)
   (for-each 
      (lambda (lecture-plan-1-par-list)
         (apply course-plan-lecture-plan-1 lecture-plan-1-par-list))
      lecture-plan-par-lists)


  ; Export calendar if we work via LAML forms, and if the course id is known.
  (if (and (eq? course-plan-editing-mode 'laml-form) course-id)
      (export-course-calendar-entries (string-append laml-calendar-file-prefix "data/" course-id "." "import")))


 )
)

(define (transform-lecture-plan-ast-to-original lecture-plan-ast)
 (let* ((attributes (propertylist-to-alist (ast-attributes lecture-plan-ast)))
        (lecture-id (get 'lecture-id attributes))
        (literature-ast (traverse-and-collect-first-from-ast lecture-plan-ast (ast-of-type? 'element-name "literature") id-1))
        (literature-contribution (if literature-ast (ast-subtrees literature-ast) '()))

        (reading-guide-ast (traverse-and-collect-first-from-ast lecture-plan-ast (ast-of-type? 'element-name "reading-guide") id-1))
        (reading-guide-contribution (if reading-guide-ast (ast-subtrees reading-guide-ast) '()))

        (exercises-ast (traverse-and-collect-first-from-ast lecture-plan-ast (ast-of-type? 'element-name "exercises") id-1))
        (exercises-contribution (if exercises-ast (ast-subtrees exercises-ast) '()))

        (references-ast (traverse-and-collect-first-from-ast lecture-plan-ast (ast-of-type? 'element-name "references") id-1))
        (references-contribution (if references-ast (ast-subtrees references-ast) '()))

        (misc-ast (traverse-and-collect-first-from-ast lecture-plan-ast (ast-of-type? 'element-name "misc") id-1))
        (misc-contribution (if misc-ast (ast-subtrees misc-ast) '()))

       )
   (list (as-symbol lecture-id) literature-contribution reading-guide-contribution exercises-contribution references-contribution misc-contribution)))

(define (transform-link-asts-to-original link-ast)
 (let* ((attributes (propertylist-to-alist (ast-attributes link-ast)))
        (url (get 'href attributes))
        (target (defaulted-get 'target attributes "main"))
        (anchor (ast-subtrees link-ast))
        (new-as-of (defaulted-get 'new-as-of attributes #f))
        (updated-as-of (defaulted-get 'updated-as-of attributes #f))
       )
   (list (as-string url) anchor (if (equal? (as-string target) "top") "_top" target) new-as-of updated-as-of)))

(define (transform-time-ast-to-original time-ast)
 (let* ((attributes (propertylist-to-alist (ast-attributes time-ast)))
        (year (get 'year attributes))
        (month (get 'month attributes))
        (day (get 'day attributes))
        (hour (get 'hour attributes))
        (minute (defaulted-get 'minute attributes 0))
        (second (defaulted-get 'second attributes 0))
       )
  (apply time-encode (map as-number (list year month day hour minute second))) ))


(define (transform-subject-ast-to-original subject-ast)
 (let* ((attributes (propertylist-to-alist (ast-attributes subject-ast)))
        (id (get 'id attributes))
        (title (get 'title attributes))
        (href (defaulted-get 'href attributes #f))
       
        (contents (ast-subtrees (traverse-and-collect-first-from-ast subject-ast (ast-of-type? 'element-name "description") id-1)))  ; A list of XHTML contents
       )
   (list (as-symbol id) (as-string title) contents href)))


(define (transform-lecture-ast-to-original lecture-ast)
 (let* ((attributes (propertylist-to-alist (ast-attributes lecture-ast)))
        (lecture-id (get 'lecture-id attributes))
        (subject-id (defaulted-get 'subject-id attributes #f))
        (plenum-start (get 'plenum-start attributes))
        (plenum-length (get 'plenum-length attributes))
        (exercise-start (get 'exercise-start attributes))
        (exercise-length (get 'exercise-length attributes))
        (room (get 'room attributes))
        (time-ast (traverse-and-collect-first-from-ast lecture-ast (ast-of-type? 'element-name "time") id-1))  ; ???
        (new-as-of (defaulted-get 'new-as-of attributes #f))
        (updated-as-of (defaulted-get 'updated-as-of attributes #f))
       )
   (list (as-symbol lecture-id)
         (if subject-id   ; non-inlined subject
             (as-symbol subject-id)
             (as-symbol lecture-id)  ; faked subject-id
         )
         (list (* 60 (as-number exercise-start)) (* 60 (as-number exercise-length)))
         (list (* 60 (as-number plenum-start)) (* 60 (as-number plenum-length)))
         (as-string room)
         new-as-of
         updated-as-of
      )))


; Calculate the minimum year, as represented in a list of times (second counts).
(define (calculate-calendar-start-year time-list)
 (year-of-time (time-decode (apply min time-list))))

; Calculate the minimum month as represented in a list of times (second counts).
(define (calculate-calendar-start-month time-list)
 (month-of-time (time-decode (apply min time-list))))

; Calculate the month span (approximate value) of time list - the number of months
(define (calculate-calendar-number-of-months time-list)
  (let ((min-time (apply min time-list))
        (max-time (apply max time-list)))
    (+ (quotient (- max-time min-time) (* seconds-in-a-day 30)) 2)))


(define (make-subject-list-from-lecture-asts lecture-ast-list)
  (map make-subject-from-lecture-ast lecture-ast-list))

(define (make-subject-from-lecture-ast lecture-ast)
 (let ((descr-ast (ast-subtree lecture-ast "description")))
  (if (ast-attribute lecture-ast 'subject-id #f)
      (laml-error "You inline subject information in the lecture elements. You are not supposed to have subject-id attributes in your lecture elements.")) 
  (list
    (as-symbol (ast-attribute lecture-ast 'lecture-id))  ; use the lecture-id as the subject-id
    (ast-attribute lecture-ast 'title "???") 
    (list (ast-text descr-ast))
    (ast-attribute lecture-ast 'href #f))))

(define (collect-times-from-lectures lecture-list-ast)
  (let ((lecture-list (traverse-and-collect-all-from-ast lecture-list-ast (ast-of-type? 'element-name "lecture") id-1))
        (time-list (traverse-and-collect-all-from-ast lecture-list-ast (ast-of-type? 'element-name "time") id-1)))
    (if (not (= (length time-list) (length lecture-list)))   ; some lecture without a time clause
        (laml-error "When you inline time in a lecture, all lecture elements must have a time clause." (length res) (length lecture-ast-list)))
    time-list))
       



; End XML-in-LAML bridge part
; ---------------------------------------------------------------------------------------------------


; Return a news flash.
; The first optional parameter is font-size, and
; the second is a with of the box.
(define (course-plan-news-flash . optional-parameter-list)
 (let ((size (optional-parameter 1 optional-parameter-list 4))
       (width (optional-parameter 2 optional-parameter-list 800))
      )
  (if (and (> (string-length news-flash-string) 0) (> news-flash-level 0))
      (div  
           (center (table-1 1 (list width) (list yellow) 
                    (list (list 
                           (font-1 size black 
                             (con news-flash-string
                                  (if news-flash-url
				      (text-choice
				       (span "Se" (a 'href news-flash-url "her") _ ".")
				       (span "See" (a 'href news-flash-url "here") _ "."))
                                     "")
                              )
                           ))))))
      '())))


; Return an aggregation (span) of a new image and/or an updated image if appropriate.
; Format of new-year-month-day-string and updated-year-month-day-string: "y-m-d".
; new-n-days is a number of days an item is considered new.
(define (new-and-or-updated-icon-if-appropriate new-year-month-day-string updated-year-month-day-string new-n-days)
  (span (new-icon-if-appropriate new-year-month-day-string new-n-days)
        (updated-icon-if-appropriate updated-year-month-day-string new-n-days)))

; Return a small  new image URL in case year-month-day-string is in the neighborhood of the current time.
; Format of year-month-day-string: "y-m-d".
; new-n-days is a number of days an item is considered new.
(define (new-icon-if-appropriate year-month-day-string new-n-days)
 (if (and year-month-day-string (string? year-month-day-string))
     (let* ((cur-time (current-time))
	    (year-month-day-list (year-month-day-decode-string year-month-day-string))
	    (item-time (time-encode (first year-month-day-list) (second year-month-day-list) (third year-month-day-list) 0 0 0))
	    (is-new? (< cur-time (+ item-time (* seconds-in-a-day new-n-days))))
            (explanation-string (string-append (text-choice "Nyhed fra" "New as of") " "  (first (date-time item-time))))
           )
       (if is-new? (span (horizontal-space 1) (img 'src (if (eq? language-preference 'english) "images/new.gif" "images/ny.gif") 'title explanation-string 'alt "")) ""))
     ""))

; Return a small updated image URL in case year-month-day-string is in the neighborhood of the current time.
; Format of year-month-day-string: "y-m-d".
; new-n-days is a number of days an item is considered new.
(define (updated-icon-if-appropriate year-month-day-string new-n-days)
 (if (and year-month-day-string (string? year-month-day-string))
     (let* ((cur-time (current-time))
	    (year-month-day-list (year-month-day-decode-string year-month-day-string))
	    (item-time (time-encode (first year-month-day-list) (second year-month-day-list) (third year-month-day-list) 0 0 0))
	    (is-new? (< cur-time (+ item-time (* seconds-in-a-day new-n-days))))
            (explanation-string (string-append (text-choice "Opdateret" "Updated as of") " "  (first (date-time item-time))))
           )
       (if is-new? (span (horizontal-space 1) (img 'src (if (eq? language-preference 'english) "images/updated.gif" "images/opdateret.gif") 'title explanation-string 'alt "")) ""))
     ""))


; ---------------------------------------------------------------------------------------------------------------

; A form to be used in additional auxiliary pages which use the course plan layout.
; Such pages must be sibling pages to the main laml course page file, because it relies on information 
; from the internal course plan directory.
(define (a-course-plan-page)
  (let* ((this-dir (startup-directory))
         (internal-dir-list (directory-list (string-append this-dir "internal/")))
         (ast-files (filter (lambda (x) (equal? (file-name-extension x) "ast")) internal-dir-list)))
   (cond ((= (length ast-files) 1)
           (let* ((course-plan-ast (file-read (string-append this-dir "internal/" (first ast-files))))
                  (course-info-ast (ast-subtree course-plan-ast "course-info"))
                  (empty-lecture-plan-list (make-ast "lecture-plan-list" '() '() 'double "course-plan-1" '()))
                  (faked-course-plan-ast (make-ast "course-plan" (list course-info-ast empty-lecture-plan-list) '() 'double "course-plan-1" '())))
	      (do-course-plan-xml-in-laml-bridging! faked-course-plan-ast)  ; in order to initialize 
           ))
         ((> (length ast-files) 1)
           (laml-error "Ambiguous AST file in internal dir"))
         ((= (length ast-files) 0)
           (laml-error "NO AST file in internal dir")))))


(end-laml-loading)