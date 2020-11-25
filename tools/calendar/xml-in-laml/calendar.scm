;=>man/calendar.laml

;;;; This is a tool for generation of calendars, which can be shown in an Internet Browser. 
;;;; The stuff in this file relies on the XHTML 1.0 transitional mirror together with the XHTML
;;;; convenience library. Thus, in order to use this calendar tool, you should load libraries in the following way (besides the fundamental LAML library):
;;;; <pre>
;;;;   (laml-style "simple-xhtml1.0-transitional-validating") 
;;;;   (lib-load "xhtml1.0-convenience.scm") <br>
;;;;   (load (string-append laml-dir "tools/calendar/xml-in-laml/calendar.scm")) <br>
;;;; </pre> <p>
;;;;
;;;; The main function of the tool is <kbd>calendar</kbd>. The function <kbd>week-calendar</kbd> is also a top-level function.
;;;; Besides these two functions, it is important to understand the input format of appointments in the calendar.
;;;; Also, there are a number of constants, which affect the appearance of the calendar. <p>
;;;;
;;;; This tools requires input in terms of calendar events, which are assumed to be hold the the Scheme
;;;; variabel <kbd>calendar-events</kbd>. In other words, you must define this variable to a value which represents a list of calendar event.
;;;; Below, in section 1, we explain the calendar event format. <p>
;;;;
;;;; Besides the general LAML stuff (laml.scm) and the XHTML mirror libraries (mentioned above) this tools requires the libraries
;;;; <a href = "../../../../lib/man/color.html">color.scm</a>, <a href="../../../../lib/man/time.html">time.scm</a> and <a href="../../../../lib/man/file-read.html">file-read.scm</a> (all from directory lib in the LAML distribution).
;;;; The required libraries are not loaded by the tool itself. Notice however, that the loading example given above also includes
;;;; loading of color, time and file-read (because they are loaded by <a href="../../../../styles/man/simple-xhtml1.0-transitional-validating.html">simple-xhtml1.0-transitional-validating</a>). <p>
;;;;
;;;; The font size of the calendar entries is controlled by CSS.
;;;; You can either link to a CSS file, or embed a CSS clause in the head of your HTML element.
;;;; The following CSS clauses is an example: 
;;;; <pre><kbd>  (style 'type "text/css" "TABLE.calendar-table TD { font-size: 7pt; }")</kbd></pre> <p>
;;;; A clause like this should be a constituent of the HTML head element.<p>
;;;; 
;;;; Please take look at the <a href = "../../../../examples/calendar-xml/index.html">calendar examples</a>.<p>
;;;; 
;;;; At www.cs.aau.dk we support a <a href="http://www.cs.aau.dk/~normark/cgi-bin/calendar/calendar-entry.cgi">LAML web calendar service</a> which will generate a calendar for you based on web input.
;;;; You are welcome to try it out.

;;;; .title Reference Manual of the LAML Calendar Tool - XHTML

; The LAML library and programs are written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 2002  Kurt Normark, normark@cs.auc.dk.
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


; LOG:
; 21.12.98:
; Changed such that multiple appointments on a single day get their own colors.
; boil-together has been changed to return color-frames instead of a simple text.
; The color-function in month-column has been changed to use background colors now that each appointment has its color-frame


; 5.2.98: We introduced a new week separation which looks real good in Internet Explorer 4.0, but not as good in Netscape 4.01.

; 4.10.02: Converted to XHTML

; 16.8.2005: Generalized to more than two columns (before and after noon).


;;; About the format of calendar input. 
;;; The calendar input must be present in a variable named <kbd>calendar-events</kbd>.
;;; <kbd>calendar-events</kbd> must be defined externally to the calendar tool.
;;; <kbd>calendar-events</kbd> is a list of appointments.
;;; A single appointment is, in turn, a list. Thus <kbd>calendar-events</kbd> is a list of lists.<p>
;;; 
;;; The detailed format of an appointment is the following: <br>
;;; 
;;; <pre>   (start-time end-time brief-description long-description color url) </pre>
;;; 
;;; Start-time and end-time are integers (a number of seconds since january 1, 1970).
;;; The function time-encoding from the <a href="../../../../lib/man/time.html">time library</a> is useful for construction of these
;;; numbers.<p>
;;; 
;;; Brief-description is a string, the contents of which goes into the calendar.
;;; Long-description is a longer description of the event. The long description is supposed to contain the brief description as well.
;;; The long description is currently used as a popup title text of a calendar event presentation. 
;;; In addition, the long description is shown instead of the brief description in a week calendar. <p>
;;; 
;;; Color is a RGB color.
;;; The color can be given in all formats accepted by the function <a href = "../../../../lib/man/color.html#rgb-color-encoding">rgb-color-encoding</a> from the LAML color library.
;;; You can also use the color constants of the <a href = "../../../../lib/man/color.html">LAML color library</a>. <p>
;;; 
;;; Finally, url is an URL, to which we link from the calendar entry. If it is the empty string, no link will be made.



; ---------------------------------------------------------------------------------------------------------------

;;; Event selection functions. 
;;; The event selector functions define accessors into a calendar appointment.

;; Return the start time of a calendar appointment. A second count integer.
(define (event-start-time e) (list-ref e 0))

;; Return the end time of a calendar appointment. A second count integer.
(define (event-end-time e) (list-ref e 1))

;; Return the brief textual description of a calendar appointment.
(define (event-brief-text e) (list-ref e 2))

;; Return the long textual description of a calendar appointment.
(define (event-long-text e) (list-ref e 3))

;; Return the  color of a calendar appointment.
(define (event-color e) (list-ref e 4))

;; Return the url of a calendar appointment.
(define (event-url e) (list-ref e 5))



(define (event-interval e) (list (event-start-time e) (event-end-time e)))


; ---------------------------------------------------------------------------------------------------------------

;;; Calendar parameters. 
;;; In this section we find a number of variables which control the calendar appearance.

; The calendar font size. Default value is 1.
;  Not used any more. The font size is controlled by a CSS clause, external to the calendar fragment. 
(define calendar-font-size 1)

;; Week separators: Whether thin og thick (symbols).
(define week-separator-of-calendar 'thin) ; alternative: 'thick

(define (week-sep) 
  (cond ((eq? week-separator-of-calendar 'thin) "")
        ((eq? week-separator-of-calendar 'thick) (hr))
        (else (error "Calendar: week separator problems"))))

;; The width of a month. Default value is 150.
(define month-width 150)

;; A list of second count intervals that divide a day and night (24 hours = 86400 seconds) into disjoint intervals.
;; The list must at least contain one interval. There is no fixed upper limit of the number of intervals.
;; Minimum second count is 0 and highest possible second count is seconds-in-a-day.
;; It is allowed that the start second count of one interval is equal to the end of the second count of the previous interval.
;; The intervals are not required to span all seconds of the 24 hours.
;; The value of this variable is used to determine the meaning of the columns in a LAML calendar.
;; The default value divides at noon.
;; Example of a legal value (the default value, in fact):   '((0 43200) (43200 86399))
(define calendar-division-of-day
  (let ((noon-time (/ seconds-in-a-day 2)))
    (list (list 0 (- noon-time 1)) (list noon-time (- seconds-in-a-day 1)))))


; ---------------------------------------------------------------------------------------------------------------

;; The background color of the calendar.
;; A color can be constructed by use of the function make-color from the LAML color library. 
;; The default background color is white.
(define calendar-background-color white)

; 

; is there an overlap between the intervals, each of which is a list (x y), x <= y.
; function of general interest
(define (interval-overlap? interval-1 interval-2)
  (let ((i1-start (car interval-1))
        (i1-end (cadr interval-1))
        (i2-start (car interval-2))
        (i2-end (cadr interval-2)))
   (or (and (<= i2-start i1-end) (<= i1-end i2-end))
       (and (<= i1-start i2-end) (<= i2-end i1-end))
       ; embedding:
       (and (<= i2-start i1-start) (<= i1-end i2-end))
       (and (<= i1-start i2-start) (<= i2-end i1-end)))))

; Return a second count interval from calendar-division-of-day, as of year, month and day.
; The first interval is numbered 1. 
; The size of n is coupled to the length of calendar-division-of-day.
(define (part-of-day-interval year month day n)
 (let* ((start (second-count year month day 0 0 0))
        (division-interval (list-ref calendar-division-of-day (- n 1)))
       )
   (list (+ start (first division-interval)) (+ start (second division-interval))))) 


; Return an interval of first half day.  From 00:00 to 12:00
(define (first-half-day-interval year month day)
 (let ((start (second-count year month day 0 0 0)))
    (list start (- (+ start (quotient seconds-in-a-day 2)) 1))))

; Return an interval of second half day. From 12:00 to 24:00
(define (second-half-day-interval year month day)
 (let ((start (second-count year month day 0 0 0)))
    (list (+ start (quotient seconds-in-a-day 2)) (- (+ start seconds-in-a-day) 1))))

; Return a list of the form (y . m). There are number-of-months entries in the list:
(define (month-list from-month year number-of-months)
 (let ((y-m-l (year-month-list year from-month number-of-months)))
  (map (lambda (y-m) (con (get-month-name (cdr y-m)) " " (as-string (car y-m))))
    y-m-l)))

; enumerate the second counts of month in year:
(define (make-second-counts month year)
  (map (lambda (day)
          (second-count year month day 0 0 0))  ; !!
    (number-interval 1 (days-in-month month year))))


(define (cal-table border cell-width-list cell-color-function weekend? cell-font-size list-of-list)
  (let ((bdr (list 'border (as-string border))))
   (table 'class "calendar-table" bdr
    (tbody
     (if (> (length calendar-division-of-day) 1) (calendar-division-presentation) '())
     (map 
       (lambda (row)
         (tr 
           (map (lambda (cell width color-1)
                  (td 'bgcolor (rgb-color-encoding color-1)
		      (div 'css:width (string-append (as-string width) "px") (present-cell cell cell-font-size))
		      (if (weekend? row) (hr) '())))
                row cell-width-list (cell-color-function row))))
       list-of-list)))))

(define (calendar-division-presentation)
  (tr (td) (td) (map (lambda (i) (td (center (font 'size "1" (hour-minute-interval-presentation (list-ref calendar-division-of-day (- i 1))))))) (number-interval 1 (length calendar-division-of-day)))))

(define (hour-minute-interval-presentation from-to-second-list)
  (let* ((from-second-count (first from-to-second-list))
         (to-second-count (second from-to-second-list))
         (from-hour-minute (hours-minutes-seconds-decode from-second-count))
         (to-hour-minute (hours-minutes-seconds-decode to-second-count))
        )
    (list (two-ciffers (first from-hour-minute)) _ ":" _ (two-ciffers (second from-hour-minute)) "-" (two-ciffers (first to-hour-minute)) _ ":" _ (two-ciffers (second to-hour-minute)))))

; Return a string of (at least) two characters given a non-negative integer n. Pad with '0'.
(define (two-ciffers n)
  (cond ((< n 10) (string-append "0" (as-string n)))
        (else (as-string n))))

; Jeg har taget (as-string) væk rundt om cell - det giver problemer på uge numre...
(define (present-cell cell size)
  cell) ; earlier (font-size size cell) - does not validate. Cannot have font around table structure

; return all elements of list which satiesfy the predicate p
; general:
(define (find-all p lst)
  (find-all-help p lst '()))

(define (find-all-help p lst res)
  (cond ((null? lst) (reverse res))
        ((p (car lst)) (find-all-help p (cdr lst) (cons (car lst) res)))
        (else (find-all-help p (cdr lst) res))))


; Return the text of a day in a calendar, given year, month, day and day-part. 
; Searches in calendar-events.
; part-of-day addresses a (relative) time interval in the calendar-division-of-day, and it is an integer (1 or larger).
; In the original version of this tool (before Aug 16, 2005) part-of-day was either 1 or 2 (before or after noon).
(define (tasks year month day part-of-day)
 (let* ((cal-interval (part-of-day-interval year month day part-of-day))
        (matching-events 
          (find-all
             (lambda (e) (interval-overlap? cal-interval (event-interval e)))
             calendar-events))
        (wnc (week-number-contribution year month day part-of-day)))
  (if (equal? "" wnc)
      (boil-together matching-events "")
      (list (i wnc) (boil-together matching-events "")))))



(define (week-number-contribution year month day part-of-day)
  (if (= 1 part-of-day)
      (let* ((sc (second-count year month day 0 0 0))
             (day-number (weekday-number sc)))
        (if (= 1 day-number) ; monday
            (let ((week-num (danish-week-number sc)))
              (as-string week-num))
            ""))
      ""))


; aggreates the matching event to a single string, use separator sep between entries.
(define (boil-together matching-events sep)
  (if (null? matching-events)
      (space 1)
      (let ((res  (map (lambda (s)
			 (con s sep))
		       (map (lambda (e) 
                             (let* ((brief-txt (event-brief-text e))
                                    (al (level-of-attention brief-txt))
                                    (unattensioned-brief-txt (substring brief-txt 0 (- (string-length brief-txt) al)))
                                  )
                              (if  (> (string-length (event-url e)) 0) 
				   (color-frame  (attention-deco al (a 'href (event-url e) 'title (event-long-text e) unattensioned-brief-txt)) (event-color e))
				   (color-frame  (attention-deco al (span unattensioned-brief-txt 'title (event-long-text e))) (event-color e)) )))
			    (sort-list matching-events event-leq?)))))
	res)))

(define (level-of-attention str)
  (let* ((len (string-length str))
         (c (- len 1))
         (d (- len 2))
         (e (- len 3)))
    (cond ((and (>= len 3) (eqv? #\! (string-ref str c)) (eqv? #\! (string-ref str d)) (eqv? #\! (string-ref str e))) 3)
          ((and (>= len 2) (eqv? #\! (string-ref str c)) (eqv? #\! (string-ref str d))) 2)
          ((and (>= len 1) (eqv? #\! (string-ref str c))) 1)
          (else 0))))

(define (attention-deco level x)
  (cond ((= level 0) x)
        ((= level 1) (span x (b (font 'color (rgb-color-encoding 255 0 0) 'size "2" "!"))))
        ((= level 2) (span x (b (font 'color (rgb-color-encoding 255 0 0) 'size "3" "!!"))))
        ((> level 2) (span (b x (font 'color (rgb-color-encoding 255 0 0) 'size "4" "!!!")) 'css:text-decoration "blink"))))

(define (attention-deco-week level x)
  (cond ((= level 0) x)
        ((= level 1) (span x (b (font 'color (rgb-color-encoding 255 0 0) 'size "4" "!"))))
        ((= level 2) (span x (b (font 'color (rgb-color-encoding 255 0 0) 'size "5" "!!"))))
        ((> level 2) (span (b x (font 'color (rgb-color-encoding 255 0 0) 'size "6" "!!!")) 'css:text-decoration "blink"))))
 

; Is calendar event e1 less than or equal to calendar event e2
(define (event-leq? e1 e2)
  (<= (event-start-time e1) (event-start-time e2)))
    

; Given a particular calendar interval, cal-interval, which is day-number (redundant),
; return the color asked for in the event-list
(define (the-event-color cal-interval day-number)
  (let ((matching-events 
          (find-all
             (lambda (e) (interval-overlap? cal-interval (event-interval e)))
             calendar-events)))
    (if (null? matching-events)
        (if (even? day-number) green1 green2)
        (event-color (car matching-events)))))  


; Return a HTML table for month in year.
; if today-info? is true, emphasize today in red.
; number-of-divisions is an integer that gives the number of calendar divisions of a single 24 hour period (a day and a night).
(define (month-column month year today-info? number-of-divisions)
 (let ((days (days-in-month month year))
       (half-column-width (quotient (- month-width 40) 2))  ; not used any more
       (other-columns-width (quotient (- month-width 40) number-of-divisions))
      )
  (cal-table 0 
    ; width list
    (append (list 20 20) (make-list number-of-divisions other-columns-width))
    
    ; background color function:
    (lambda (row)
     (if (equal? (car row) (week-sep))
      (make-list (+ number-of-divisions 2) (if (eq? week-separator-of-calendar 'thin) grey1 black))         ; WEEK SEPARATOR
      (let* ((day (as-number (cadr row)))
             (start-time (second-count year month day 0 0 0))
             (end-time (+ start-time seconds-in-a-day))
             (today (and today-info? (>= (current-time) start-time) (<= (current-time) end-time)))
             (event-color-1 (the-event-color (first-half-day-interval year month day)  day))       ; not used in this version
             (event-color-2 (the-event-color (second-half-day-interval year month day) day))      ; not used in this version
            )
        (background-color-list number-of-divisions (even? day) today))))

    ; week separator predicate: NOT USED ANY MORE. SEPARATORS AFTER SUNDAY HOW HAS THEIR OWN ROW. THUS ALWAYS FALSE
    (lambda (row) #f)

    ; font-size:
    calendar-font-size

    (month-list-with-week-separators (make-second-counts month year) days year month number-of-divisions)

)))

(define (background-color-list number-of-divisions even? today?)
  (append 
    (cond (today? (list red red))
          (even? (list green1 green1))
          (else  (list green2 green2)))

    (if even? (make-list number-of-divisions green1)  (make-list number-of-divisions green2))))


; Given the second-count-list for a month, return a list of (second-count brief-weekday day tasks-1 tasks-2 ... ) for each day in month if year.
; The parameter month is supposed to have days number of days.
; Week separators are returned as (0 "" "" "" "" ...), made by the function make-week-separator.
(define (month-list-with-week-separators second-count-list days year month number-of-divisions)
  (let* ((entries  (map2 (lambda (day sec-count)
                            (append 
                              (list sec-count (brief-weekday sec-count) (as-string day))
                              (map (lambda (n) (tasks year month day n)) (number-interval 1 number-of-divisions))  ; tasks of the number-of-divisions periods
                            )
                         )
                         (number-interval 1 days)
                         second-count-list
                        ))
         (entries-with-sep (week-separator-extend entries number-of-divisions))
        )
   (map cdr entries-with-sep)))

; Introduce week separators into entries
(define (week-separator-extend entries number-of-divisions)
  (cond ((null? entries) '())
        ((= 7 (weekday-number (caar entries)))
          (cons (car entries)
                (cons (make-week-separator number-of-divisions)
                      (week-separator-extend (cdr entries) number-of-divisions))))
        (else (cons (car entries) (week-separator-extend (cdr entries) number-of-divisions)))))

(define (make-week-separator number-of-divisions)
  (cons 0 (make-list (+ 2 number-of-divisions) (week-sep))))

; enumerate years and months, starting from start-year and start-month. The entries in the list
; are of the form (y . m). The length of the list is number-of-months
(define (year-month-list start-year start-month number-of-months)
  (cond ((= 0 number-of-months) '())
        ((= start-month 12) (cons (cons start-year start-month) (year-month-list (+ 1 start-year) 1 (- number-of-months 1))))
        ((< start-month 12) (cons (cons start-year start-month) (year-month-list start-year (+ 1 start-month) (- number-of-months 1)))) 
        (else (error "year-month-list problem"))))

;;; The main calendar function. 
;;; In this section we find the most important function at all, the calendar function which generates a calendar.

;; Return a HTML calendar for year, from-months, and running through number-of-months. 
;; This is the main function of the tool. Thus, the LAML calendar tool IS this function.
;; As an example, (calendar 1999 2 3) will produce a calendar of February, March, and April in 1999.
;; If the optional parameter show-today is #t, the current date is shown in red.
;; .pre-condition The variable calendar-events must be defined prior to the call of this function. number-of-divisions = (length calendar-division-of-day).
;; .internal-references "calendar events" "SECTION1"
;; .parameter year The year of the calendar. An integer. Example 2005.
;; .parameter from-month The first month of the calendar. An integer from 1 to 12.
;; .parameter number-of-months The number of months in this calendar. A non-negative integer.
;; .parameter show-today? If true, mark the calendar generation day of with a red color. Defaults to #f.
;; .parameter number-of-divisions The number of columns (besides the fixed weekday and day columns) in this calendar. Defaults to 2.
;; .form (calendar year from-month number-of-months [show-today? number-of-divisions])
(define (calendar year from-month number-of-months . optional-parameter-list)
 (let ((show-today? (optional-parameter 1 optional-parameter-list #f))
       (number-of-divisions (optional-parameter 2 optional-parameter-list 2))
      )
  (if (not (= number-of-divisions (length calendar-division-of-day)))
      (laml-error "calendar: the value of number-of-divisions must be equal to the length of the list calendar-division-of-day" number-of-divisions (length calendar-division-of-day)))
  (table-2 1 (make-list number-of-months month-width)
             (make-list number-of-months calendar-background-color)
             (month-list from-month year number-of-months)
     (list 
        (map (lambda (y-m) (month-column (cdr y-m) (car y-m) show-today? number-of-divisions)) (year-month-list year from-month number-of-months))))))

(define xml-error-truncation-length 1000)

; part-of-day addresses a (relative) time interval in the calendar-division-of-day, and it is an integer (1 or larger).
(define (week-tasks year month day part-of-day)
 (let* ((cal-interval (part-of-day-interval year month day part-of-day))
        (matching-events 
          (find-all
             (lambda (e) (interval-overlap? cal-interval (event-interval e)))
             calendar-events))
        (wnc ""))
  (div (if (equal? "" wnc) "" (em wnc)) (boil-together-week matching-events ""))))

; aggreates the matching event to a single string, use separator sep between entries.
; A variant of boil-together.
(define (boil-together-week matching-events sep)
  (if (null? matching-events)
      (space 1)
      (let ((res  (map (lambda (s)
			 (con s sep))
		       (map 
                          (lambda (e) 
                           (let* ((brief-txt (event-brief-text e))
                                  (al (level-of-attention brief-txt))
                                  (unattensioned-brief-txt (substring brief-txt 0 (- (string-length brief-txt) al)))
                                 )
                            (if  (> (string-length (event-url e)) 0) 
				 (color-frame  (attention-deco-week al (a 'href (event-url e)  (event-long-text e))) (event-color e))
				 (color-frame  (attention-deco-week al (span (event-long-text e) )) (event-color e)) )))
			    (sort-list matching-events event-leq?) ))))
	res)))


;; Make a week calendar for the week that contains the time t0 (a second count).
;; The week calendar has number-of-divisions columns (besides the fixed weekday and day columns), relative to the value of the variable calendar-division-of-day.
;; .parameter t0 A time (a second-count) contained in the desired week.
;; .parameter number-of-divisions The number of columns (besides the fixed weekday and day columns) of this week calendar. Defaults to 2.
;; .parameter week-cal-column-width The pixel width of columns in the week calendar. Defaults to 200. 
;; .form (week-calendar t0 [number-of-divisions week-cal-column-width])
(define (week-calendar t0 . optional-parameter-list)
 (let ((number-of-divisions (optional-parameter 1 optional-parameter-list 2))
       (week-cal-column-width (optional-parameter 2 optional-parameter-list 200))
      )
  (let* ((dec-t0 (time-decode t0))
         (wdn (weekday-number t0))
         (day-start (time-encode (year-of-time dec-t0) (month-of-time dec-t0) (day-of-time dec-t0) 0 0 0))
         (week-start (- day-start (* (- wdn 1) seconds-in-a-day)))
         (decoded-week-start (time-decode week-start))
         (week-end (- (+ (* 7 seconds-in-a-day) week-start) 1))
         (week-start-time-list (map (lambda (i) (+ week-start (* i seconds-in-a-day))) (number-interval 0 6)))
         (weekday-list (if (eq? language-preference 'english)
                           (list "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")
                           (list "Mandag" "Tirsdag" "Onsdag" "Torsdag" "Fredag" "Lørdag" "Søndag")
                       ))
         (weekday-assoc-list (pair-up weekday-list week-start-time-list))
        )
   (div
     (text-choice "Ugestart: " "Start of week: ") (car (date-time week-start)) (br)
     (text-choice "Ugenummer: " "Week number: ") (as-string (danish-week-number week-start)) (p)

     (let ((other-columns-width week-cal-column-width))     
      (cal-table
       1 

       ; width list
       (append (list 100 50) (make-list number-of-divisions other-columns-width))

       ; background color function:
       (lambda (row)
         (let* ((weekday (car row))
                (start-time (cdr (assoc weekday weekday-assoc-list)))
                (cur-time (current-time)))
          (cond ((and (<= start-time cur-time) (<= cur-time (+ start-time seconds-in-a-day)))
                    (append (list red red) (make-list number-of-divisions green2)))
                (else (make-list (+ 2 number-of-divisions) green2)))))

       (lambda (row) #f) ; not relevant

       3  ; font size

       ; content list
       (map2 (lambda (weekday daystart-time)
               (let* ((decoded-daystart-time (time-decode daystart-time))
                      (day (day-of-time decoded-daystart-time))
                      (month (month-of-time decoded-daystart-time))
                      (year (year-of-time decoded-daystart-time))
                     )
                 (append 
                   (list weekday (as-string day))
                   (map (lambda (n) (week-tasks year month day n)) (number-interval 1 number-of-divisions))  ; week-tasks of the number-of-divisions periods
                 )
               )
             )
             weekday-list  week-start-time-list)))))))
