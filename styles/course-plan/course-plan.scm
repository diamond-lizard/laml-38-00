; An LAML lecture-plan style.
; The LAML library and programs are written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999  Kurt Normark, normark@cs.auc.dk.

; This style assumes that course-description file is loaded just after the loading of
; this file.


;;;; The course plan document style supports the generation of a course home page with
;;;; references to 
;;;; course plans for a number of lectures, course introduction, lecture overviews, calendars, and exercises. All the generated WWW pages are organized
;;;; in a single directory, the  <em>course html directory</em> - for convenient bundling of all the generated pages.
;;;; The variable language-preference determines the language of the fixed texts in the system. Possible values are the symbols danish and english.
;;;; This system is now obsolete. We have put an XML-in-LAML interface on the course plan system. The new software is found
;;;; in styles/xml-in-laml/course-plan/. As of now, there is no manual of of this style, but it will come soon.

;;; Overview and organization.
;;; The course plan document style relies on a complete description of a course plan in terms of lecture lists,
;;; time lists, subject lists, and a number of other parameters. The course description is represented in a single
;;; central Lisp file, the <em>course description file</em>.<p>
;;; Besides the course description file there is one central LAML file which generates the introduction, the overviews, and
;;; the HTML frame pages. The is caled the <em>LAML course file.</em> Besides these two files there exists a file pr. lecture,
;;; typically (but not necessarily) organized in a directory of their own. Finally, there may be other course files with additional
;;; text, which are produced with the course file look using the functions course-page write-course-html-file.
;;; 
;;; The course plan document styles features a very convenient handling of time. If the course runs perodically and
;;; regularly the determination of the individual lecture times is almost made automatically, see time-list below.
;;; The temporal power of the system is used to generate overviews and calendars (by means of the LAML calandar tool).
;;; We use a separate and simple time-list, external from the other lists, to provide for very easy rescheduling of lectures.<p>
;;; 
;;; We recommend the following directory and file organization when using the course plan document style:
;;; <pre>
;;;   course-dir
;;;    html (*)
;;;      images  (*)
;;;    internal (*)
;;;    lecture-pages
;;;    course-description.scm
;;;    course.laml
;;; </pre>
;;; All but course.laml and course-description.scm are directories. Only those directories marked by (*) need to have the mentioned names.
;;; 
;;; We will now describe the role of each of the directories and files:
;;; <ul>
;;;   <li> course-dir: The home directory of the course. May contain many other items. Named by the variable course-dir.
;;;   <li> html: The directory in which all generated HTML files are located. Create this directory yourself.
;;;   <li> html/images: The directory where all gif files will be placed. The necessary gif files will be copied to this directory automatically.
;;;        However, you must create this directory yourself.
;;;   <li> internal: A directory in which some internal files are written.
;;;   <li> lecture-pages: The directory in which to place the individual LAML lecture descriptions.
;;;   <li> course-description.scm: The course description file which must be loaded just after the course-plan style file. You are yourself responsible
;;;        for loading this file.
;;;   <li> course.laml: The LAML file which produces all the overall plans and "infrastructure" pages, including the course home page itself.
;;;        This file should also produce the course intoduction file, which must be named course-introduction.
;;; </ul>
;;; The html, html/images, and the internal directories are created automatically by the course-plan system.
;;; In a later section we will show examples and templates of the directories and files.

(define course-plan-version "version 1, June 18, 1999")

; The directory in which the course plan software is located
(define course-plan-software-directory (string-append laml-dir "styles/course-plan/"))

; The laml source file. Not used in this program, but may be useful in the source file itself
(define laml-source-file-name-without-extension (source-filename-without-extension)) 

; ===============================================================================================================
;;; The course description. 
;;; The definitions in this section must be redefined in the course-description file
;;; which should be located in course-dir, which is defined below. The course description file
;;; must be loaded just after the style loading clause. The central description structures (lists)
;;; are time-list, lecture-description-list, and subject-list.

;; A list of subject records.
;; A subject record is a list of (subject-id title subject).
;; The default is the empty list. Is allowed to be longer or shorter than lecture-description-list.
;; Entries are looked up via the subject-id.
(define subject-list '())

;; A list of lecture records. 
;; A lecture record is the central description of a single lecture in the course, in terms of
;; a lecture-id, a subject-id (referring to a record in suject-list), exercise time info, plenum time info, and room info.
;; The list must be of the same length as time-list.
;; Lecture record description:
;;   Lecture-id: A symbol identifying a lecture. -
;;   Subject-id: A symbol identifying a subject in subject list. -
;;   Exercise-info: A list of (start-displacement length). Displacement relative to lecture start in time-list. Length in seconds. -
;;   Plenum-info: A list of (start-displacement length). Displacement relative to lecture start in time-list. Length in seconds. -
;;   Room: A room id string.
(define lecture-description-list '())

;; A list of times for the individual lectures in the course.
;; Each time is an integer (the number of seconds since jan. 1, 1970).
;; Use the function time-encode from the LAML time library to return such a number.
;; Must be of the same length as lecture-list.
(define time-list '())



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
;; Ends in a slash.
;; Must be defined in the course description file.
(define course-dir #f)

;; The prefix of the course URL, pointing at the course directory.
;; Ends in a slash.
;; Must be defined in the course description file.
(define course-url-prefix #f)

; A function returning the full path of the lecture description list file in the internal directory of the course directory.
; In this file we store lecture-description-list in order for other tools to access the information in this data structure.
(define (lecture-list-file)
  (string-append course-dir "internal/lecture-list.lsp"))

;; A HTML string returning function with standard links from course-plan pages. 
;; As default, no standard links are defined.
(define (standard-links) "")

;; A function which returns a list of links which go into the overview frame (the leftmost frame of the frameset) of the course home page.
;; Each element of the list must be a list of the format ("url" "anchor-name" "target"). The target may be "main" (for the main, rightmost
;; window of the frameset) or "_top".
;; Thus this function must return a list of lists.
;; The links given here are added to some automatically generated links to overviews, calendar, and introduction.
;; If you want additional general links, redefine the function in the course description file.
;; As the default, this function returns the empty list.
(define (general-overview-links) '())

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

;; Return a list with course meta information to be put on any course page.
;; Similar to the function meta-tag-clauses.
;; The result of this function is passed more directly to the html:page function via the course-plan document style.
;; As default no meta descriptions are provided.
(define (meta-course-contribution) '())

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

; END OF PRE-DEFINITIONS OF COURSE DESCRIPTIONS.
; ===============================================================================================================

; ---------------------------------------------------------------------------------------------------------------
;;; Top level course plan functions. 
;;; The top level functions used in individual lecture pages (pages describing a single lecture plan in the course)
;;; are init-course-plan and lecture-plan. Besides these, the exercise functions described later are typically found
;;; in a lecture plan file.

;; The init-course-plan function must be called after all loading is completed, and before any pages are generated.
(define (init-course-plan)

  (laml-welcome)
  (course-plan-welcome)
  
  ; Define effective lecture-list by merging the lecture-description-list and the time-list

  (check-redefinitions)

  (set! lecture-list 
     (map2 
       (lambda (ld-rec time) (list (first ld-rec)
                                   (second ld-rec)
                                   time
                                   (third ld-rec)
                                   (fourth ld-rec)
                                   (fifth ld-rec)
                             )
       )
       lecture-description-list
       time-list))

  ; Ensure that the html and html/images directories exists in course-plan-dir
  (ensure-directory-existence! course-dir "html")
  (ensure-directory-existence! (string-append course-dir "html/") "images")

  ; Copy images to html directory
  (copy-files 
      course-plan-image-files
      (string-append course-plan-software-directory "images/")
      (string-append course-dir "html/images/") )

  ; Save lecture-list in the internal directory: lecture-list.lsp

  (ensure-directory-existence! (string-append course-dir) "internal")

   (if (and (eq? scheme-system 'scm) (not (directory-exists? (string-append course-dir "internal/"))))
      (error (string-append "You must make a directory name 'internal' in the course directory: "
                              course-dir 
                              ". We save the value of lecture-description-list in a file in this directory")))

   (if (file-exists? (lecture-list-file))
       (delete-file (lecture-list-file)))

   (with-output-to-file (lecture-list-file)
       (lambda () (write lecture-list)))

)

(define warning-counter 0)

(define (display-warning-1 message)
  (display-warning message)
  (set! warning-counter (+ 1 warning-counter)))  

(define (check-redefinitions)
  (if (null? subject-list) (display-warning-1 "You should define the variable subject-list"))
  (if (null? lecture-description-list) (display-warning-1 "You should define the variable lecture-description-list"))
  (if (null? time-list) (display-warning-1 "You should define the variable time-list"))
  (if (not course-title) (display-warning-1 "You MUST define the variable course-title"))
  (if (not course-semester) (display-warning-1 "You MUST define the variable course-semester"))
  (if (not brief-course-semester) (display-warning-1 "You MUST define the variable brief-course-semester"))
  (if (not course-dir) (display-warning-1 "You MUST define the variable course-dir"))
  (if (not course-url-prefix) (display-warning-1 "You MUST define the variable course-url-prefix"))
  (if (not last-lecture-number) (display-warning-1 "You MUST define the variable last-lecture-number"))
  (if (not course-start) (display-warning-1 "You MUST define the variable course-start"))
  (if (not actual-exercise-interval) (display-warning-1 "You MUST define the function actual-exercise-interval"))
  (if (not (= (length time-list) (length lecture-description-list)))
                         (display-warning "The time-list and the lecture-description-list must be lists of equal lengths"))

  (if (= 0 warning-counter) (begin (display "Redefinitions in course description file OK") (newline)))
)

(define lecture-id-from-lecture-plan #f)

;; Write the plan of a given lecture to a html file.
;; The id parameter is mandator. The keyword parameters are each of the form
;; (key value), where key is one of literature, guide, exercises, references, and misc.
;; The function writes the plan to an HTML file in the course html directory. The name of the
;; files will be identical to the name of the LAML file, in which the source (with the lecture-plan form) is located.
;; This function the central function for generation of individual lecture plans for lectures in the cource.
(define (lecture-plan id . keyword-parameters)
 (check-lecture-plan id)
 (set! lecture-id-from-lecture-plan id)
 (let ((the-lit (keyword-parameter 'literature keyword-parameters))
       (the-guide (keyword-parameter 'guide keyword-parameters))
       (the-ex  (keyword-parameter 'exercises keyword-parameters))
       (the-ref (keyword-parameter 'references keyword-parameters))
       (the-misc (keyword-parameter 'misc keyword-parameters)))
   (lecture-plan-1 id the-lit the-guide the-ex the-ref the-misc)))


(define (check-lecture-plan id)
  (if (not (find-lecture id lecture-list)) (display-error "The lecture-id (first parameter of lecture-plan) is not found in the the lecture description list"))
)


; End of top level functions.
; ----------------------------------------------------------------------------------------------------------------


; Subject selectors and locators related to subject-list

(define subject-id car)
(define subject-title cadr)
(define subject-description caddr)

; Return a manually supplied URL of the subject structure - for linking to an external place
; A selector on a subject structure
(define subject-external-url 
  (lambda (x) (if (>= (length x) 4) (fourth x) #f)))

; Lookup a subject record with some given id. Return a record (a tripple list).
(define (find-subject id subject-list)
  (cond ((null? subject-list) #f)
        ((eq? id (subject-id (car subject-list))) (car subject-list))
        (else (find-subject id (cdr subject-list)))))

; ---------------------------------------------------------------------------------------------------
; Lecture records selectores and locates, related to lecture-list

; given an id, return a lecture record
(define (find-lecture id lecture-list)
  (cond ((null? lecture-list) #f)
        ((eq? id (lecture-id (car lecture-list))) (car lecture-list))
        (else (find-lecture id (cdr lecture-list)))))

; shortcut to find-lecture
(define (lec id)
  (find-lecture id lecture-list))

; Return record number n of lecture-list.
; The first is assigned number one.
; If n is out of range, return #f
(define (find-lecture-by-number n lecture-list)
  (if (and (>= n 1) (<= n (length lecture-list)))
      (list-ref lecture-list (- n 1))
      #f))


; Selectors and locators are defined on the derived lecture-list, see below.
; ---------------------------------------------------------------------------------------------------


; LOADING

; general.scm is loaded by laml.scm
(lib-load "cgi.scm")
(lib-load "html.scm")
(lib-load "html-v1.scm")
(lib-load "hex.scm")
(lib-load "time.scm")
(lib-load "file-read.scm")
(load (string-append laml-dir "tools/calendar/calendar.scm"))
(load (string-append laml-dir "tools/calendar/calendar-lib.scm"))

; -----------------------------------------------------------------------------

(define target-extension "html")

; -----------------------------------------------------------------------------
; Called after loading of style and course specific information:

; The effective lecture-list which merges lecture-description-list and time-list
; Assigned by init-course-plan
(define lecture-list '())


(define (course-plan-welcome)
  (display (string-append "The course plan style " course-plan-version))
  (newline))

; selectors of single lecture record in lecture-list
(define (lecture-id rec) (list-ref rec 0))
(define (lecture-subject-id rec) (list-ref rec 1))
(define (lecture-absolute-start-time rec) (list-ref rec 2))  ; start på halvdag
(define (lecture-exercise-start-displacement rec) (car (list-ref rec 3)))
(define (lecture-exercise-length rec) (cadr (list-ref rec 3)))
(define (lecture-plenum-start-displacement rec) (car (list-ref rec 4)))
(define (lecture-plenum-length rec) (cadr (list-ref rec 4)))
(define (lecture-room rec) (list-ref rec 5))

; Derived selectors:
(define (lecture-plenum-start-time rec) 
  (+ (lecture-absolute-start-time rec) (lecture-plenum-start-displacement rec)))

(define (lecture-exercise-start-time rec) 
  (+ (lecture-absolute-start-time rec) (lecture-exercise-start-displacement rec))) 

; extended selectors, accessing static information:

(define (lecture-title lecture-rec)
  (let ((static-record (find-subject (lecture-subject-id lecture-rec) subject-list)))
    (subject-title static-record)))

(define (lecture-description lecture-rec)
  (let ((static-record (find-subject (lecture-subject-id lecture-rec) subject-list)))
    (subject-description static-record)))

(define (lecture-number lecture-rec)
  ; calculate lecture-rec's number in the sequence given of the lecture-list
  (lecture-number-help (lecture-id lecture-rec) lecture-list 1))

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
(define yellow-1 '(255 255 160))
(define purple-1 '(204 126 250)) 
(define purple-2 '(183 73 248))
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

;; Another sample color scheme:  Blue and grey.
(define course-plan-color-scheme-5 (make-color-scheme (make-color 0 100 100) (make-color 0 100 100) (make-color 216 216 177)  "triangle-00a.gif"))

  

; -----------------------------------------------------------------------------
; IMAGES

; a list of all course-plan relevant images, which need to be copied to the html directory
(define course-plan-image-files 
  (list "triangle.gif" "triangle-99.gif" "triangle-black.gif" "triangle-00.gif" "triangle-00a.gif"))

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
  (font 1 black (con (text-choice "Genereret: " "Generated: ") (capitalize-string-nd day) " " date ", " time (br)
                     (generating-system)))))

;; Return a string which tells about the generating computer system.
;; Default: the empty string.
(define (generating-system)
 "")


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
  (table-1 0 '(39 1000) (make-list 2 contents-frame-bg) 
    (list (list (img (con  "images/" actual-triangle)) (hr))) "middle"))


; Return a slice for the lecture-plan 
; If not condition, return an empty slice (empty list)
(define (lecture-plan-slice condition left right)
  (if condition
      (list 
        (list (font 4 the-dark-text-color (b left))
              (con right (p))))
      '()))

(define (lecture-plan-1 id lit guide exerc refs misc)
 (let* ((rec       (find-lecture id lecture-list))  
        (n         (lecture-number rec))
        (next-rec  (find-lecture-by-number (+ n 1) lecture-list))
        (titl      (lecture-title rec))
        (descr     (lecture-description rec))
        (lec-int   (lecture-lecture-interval rec))
        (ex-int    (actual-exercise-interval rec next-rec))
        (dt        (date-time-1 (car lec-int)))
        (room      (lecture-room rec)))

  (write-course-html-file
    (course-page
     (con (text-choice "Lektion " "Session ") (as-string n))
     (table-1 0 '(175 600) (make-list 2 contents-frame-bg) 
	      (append
		(lecture-plan-slice #t 
                   (text-choice "Emne" "Subject")
                   (con titl (p) (i descr) ))

		(lecture-plan-slice #t
                   (text-choice "Dato" "Date")
                   (con (capitalize-string-nd (weekday (car lec-int))) " " (car dt)))

		(lecture-plan-slice #t
                    (text-choice "Forelæsning" "Talk")
                    (con (text-choice "Klokken " " ") (clock-interval lec-int) (text-choice " i lokale " " in room ") room))

		(lecture-plan-slice (not (empty-string? lit))
                    (text-choice "Litteratur" "Literature")
                    (con lit (br)))

		(lecture-plan-slice  (not (empty-string? guide))
                    ""
                    (i (font-size 2 (con (text-choice "Læsevejledning: " "Reading guide: ")  guide ))))

		(lecture-plan-slice (not (empty-string? exerc))
                    (text-choice "Øvelser" "Exercises")
                    (con (if ex-int 
                            (con (text-choice "Øvelserne til denne lektion afholdes " 
                                      "The exercises of this session takes place ")
                         
                                      (decapitalize-string-nd (extended-clock-interval ex-int)) (p) exerc)
                            exerc
                         )
                    )
                )

		(lecture-plan-slice (not (empty-string? refs))
                    (text-choice "Referencer" "References")
                    (con refs))

                (lecture-plan-slice (not (empty-string? misc))
                    (text-choice "Diverse" "Miscellaneous")
                    (con misc))
     )))
     (string-append (as-string id) "-lecture")
  )))


; -----------------------------------------------------------------------------
; Keyword handling

(define (lecture-identification id)
  (cons 'lecture-identification id))

(define (literature lit)
  (cons 'literature lit))

(define (exercises ex)
  (cons 'exercises ex))

(define (references ref)
  (cons 'references ref))

(define (guide g)
  (cons 'guide g))

(define (misc m)
  (cons 'misc m))

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
 (html:html 
  (con 
   (html:title (string-append course-title " - " course-semester))
   (html:frameset 
      (con (html:frame "" 'name "overview" 'src (string-append overview-page ".html") 'scrolling "auto")
           (html:frame "" 'name "main" 'src (string-append contents-page ".html") 'scrolling "auto")
      )
      'cols (string-append (as-string overview-pixel-width) ",*")))))

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
    (map overview-entry lecture-list)))

(define (overview-entry lecture-rec)
 (let ((d-t (date-time-1 (lecture-absolute-start-time lecture-rec))))
  (list (lecture-number lecture-rec)
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
      (course-overview))
    file-name))


; ---------------------------------------------------------------------------------------------------
; COURSE SUMMARY PAGE

; Generate and return a course summary in terms of lecture number, title and description (taken from subject-list).
(define (course-description-summary)
  (table-2 1 (list 50 150 450) (make-list 3 contents-frame-bg)
    (list (text-choice "Kursusgang" "Lecture number")
          (text-choice  "Emne" "Subject")
          (text-choice  "Beskrivelse" "Description"))
    (map description-entry lecture-list)))

(define (description-entry lecture-rec)
 (let* ((st (lecture-absolute-start-time lecture-rec))
        (dt (date-time st))
        (wd (capitalize-string-nd (weekday st))))
  (list (con (as-string (lecture-number lecture-rec)) (br)
             (font 1 grey1 (con wd " " (car dt))))
        (lecture-title lecture-rec)
        (font-size 2 (lecture-description lecture-rec)))))

; Write a course summary (as generated by course-description-summary) to a file in the course html directory.
(define (write-course-description-summary file-name)
  (write-course-html-file
    (course-page
      (text-choice "Lektionsbeskrivelser" "Lecture Descriptions")
      (course-description-summary))
    file-name))

; -----------------------------------------------------------------------------
; COURSE OVERVIEW FRAME

(define (standard-overview-links)
  (list 
     (list "course-introduction.html" (text-choice "Introduktion" "Introduction") "main")
     (list "course-overview.html"     (text-choice "Kursusoversigt" "Course overview") "main")
     (list "course-summary.html"      (text-choice "Lektionsbeskrivelser" "Lecture descriptions") "main")
     (list "course-calendar.html"     (text-choice "Kursuskalender" "Course calendar") "main")))

(define overview-url (make-selector-function 1 "overview-url"))
(define overview-anchor-text (make-selector-function 2 "overview-anchor-text"))
(define overview-target (make-selector-function 3 "overview-target"))


; Return a course overview frame, to be presented as the leftmost navigation bar in a frame set.
; n: reveal n links in the lecture list.
; Uses the function general-overview-links which is assumed to be defined in the course-description file.
(define (course-overview-frame n)
  (let ((anchor-p-list (append (make-list n #t) (make-list (- (length lecture-list) n) #f))))
     (page (con-space  (text-choice "Indhold" "Contents") course-title brief-course-semester)
	   (con 

	    (b (con brief-course-title " - " brief-course-semester)) (br)

	    (ul
	     (map (lambda (link) (a-tag-target (overview-url link) (font 2 overview-frame-text (overview-anchor-text link)) (overview-target link)))
		  (append (standard-overview-links) (general-overview-links))))

	    (p)
          
	    (b (text-choice "Lektioner:" "Lectures:")) (br)
	    (ol
	     (map2 course-overview-frame-entry lecture-list anchor-p-list)))
	   overview-frame-bg overview-frame-text overview-frame-link overview-frame-vlink)))    


; Is there an external URL from lecture-id.
; I.e., an URL given in the subject record.
(define (external-url-from? lecture-id)
 (let ((subject-descr (find-subject lecture-id subject-list)))
   (and subject-descr (subject-external-url subject-descr))))


; Return a relative URL to a us lecture page given the lecture id.
; If the subject record contains an fourth external URL field, use this instead.
(define (us-relative-url-perhaps-external lecture-id)
 (let ((subject-descr (find-subject lecture-id subject-list)))
   (if (and subject-descr (subject-external-url subject-descr))
       (subject-external-url subject-descr)
       (us-relative-url lecture-id))))

(define (course-overview-frame-entry lecture-rec anchor-p)
 (if anchor-p
   (a-tag-target (us-relative-url-perhaps-external (lecture-id lecture-rec))  (font 2 overview-frame-text (lecture-title lecture-rec)) "main")
   (font 2 overview-frame-text (lecture-title lecture-rec))))


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
    (append (map course-calendar-entry lecture-list) external-calendar-entries))
  (calendar from-year from-month number-of-months #f))

; Write a calendar via course-calendar on the file in the course html directory.
(define (write-course-calendar file-name from-year from-month number-of-months calendar-title external-entries)
  (write-course-html-file
    (course-page
      calendar-title
      (course-calendar from-year from-month number-of-months calendar-title external-entries)
    )
    file-name))

;; Export this course's calendar entries to to-file.
;; Only this course's entries are exported. The export format is the 'native' calendar format,
;; as defined by the interactive, server-based LAML calendar tool. URL's to lectures are made absolute via the exporting.
;; If to-file exists, overwrite it.
(define (export-course-calendar-entries to-file)
  (file-write
    (map export-course-calendar-entry lecture-list)
    to-file))


; Returns a native calendar entry from rec.
; This variant includes a relative url to lectures
(define (course-calendar-entry rec)
 (let ((n (lecture-number rec)))
  (list (lecture-absolute-start-time rec)
        (+ (lecture-absolute-start-time rec) (* 2 seconds-in-an-hour)) ; just keep it within the half day
        (string-append brief-course-title " " (as-string n))
        ""
        course-calendar-color
        (if (<= n last-lecture-number) (us-relative-url-perhaps-external (lecture-id rec)) ""))))

; Returns a native calendar entry from rec.
; This variant includes an absolute url to lectures. This is the only difference between this function and course-calendar-entry
(define (export-course-calendar-entry rec)
 (let ((n (lecture-number rec)))
  (list (lecture-absolute-start-time rec)
        (+ (lecture-absolute-start-time rec) (* 2 seconds-in-an-hour))  ; just keep it within the half day
        (string-append brief-course-title " " (as-string n))
        ""
        course-calendar-color
        (if (<= n last-lecture-number)
            (if (external-url-from? (lecture-id rec))
                (us-relative-url-perhaps-external (lecture-id rec))
                (string-append course-url-prefix "html/" (us-relative-url (lecture-id rec))))
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

  (set! month-width (quotient preferred-calendar-pixel-width cal-number-of-months))
  (set! calendar-background-color white)
  (write-course-calendar "course-calendar" cal-year cal-month cal-number-of-months cal-title external-calendar-entries))
  


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
          (lec-rec (find-lecture lecture-id-1 lecture-list))
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
(define (exercise-url lecture-id exercise-id)
  ; absolute exercise url
  (string-append course-url-prefix "html/" (exercise-name exercise-id lecture-id) ".html"))

;; Return an absolute URL to lecture number n in this course.
(define (lecture-number-url n)
  ; absolute lecture us url given a lecture number n
  (let ((lectureid (lecture-id (list-ref lecture-list  (- n 1)))))
    (string-append course-url-prefix "html/"  (as-string lectureid) "-lecture" ".html")))


; ---------------------------------------------------------------------------------------------------------------
;;; Internal functions of interest.
;;; Here we wil document a few internal functions which may turn out to be useful in some contexts.

;; Return a course page (with standard graphical elements, header, body and some footer stuff (standard-links)).
;; This functions returns an HTML string.
;; Use this function together with write-course-html-file if you want to make additional course plan files.
(define (course-page title body)
  ; title without course name
   
   (html:page
    (con course-title title)
    (meta-course-contribution)
    (con 
      (h 1 (font-color the-dark-text-color (con course-title (br) title))) (p)

      (triangle-ruler) (p)

      body

      (p) (hr) (p)

      (table-1 0 '(250 450) (make-list 2 contents-frame-bg) 
        (list (list (con teacher-name (p) (last-update)) (standard-links))))
      
    )
   contents-frame-bg contents-frame-text contents-frame-link contents-frame-vlink))

;; Write the contents in the course's html file.
;; The contents is typcically an activation of course-page.
;; File is without extension and without any path information.
;; This function adds an html extension.
;; Use this function together course-page if you want to make additional course plan files.
(define (write-course-html-file contents file)
  (write-text-file
    contents
    (string-append course-dir "html/" file ".html")))


;;; Utility functions.
;;; Here are some functions which are useful when producing course home pages.

;; Return a list of times, length n, starting with start, and with the period period.
;; This function can be used to produce the time-list in case of a regular course rythm.
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
