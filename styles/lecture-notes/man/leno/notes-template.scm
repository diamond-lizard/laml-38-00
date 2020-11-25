(define note-abstract
  "ABSTRACT AT TOP LEVEL")


; The directory in which this file is located.
; Must be ended with a slash
(define note-source-directory "DIRECTORY")             ; The directory where the note sources are located. 
                                                       ; Full, absolute path. Must be terminated by a slash.

(define notes-title "NOTE TITLE")                      ; The top level title of these notes.
 
(define current-lecture N)                             ; The number of the last section processed


(define lecture-sections                               ; A list with one entry per lecture section
  (list
    (list  "CHAPTER-1-ID"   "CHAPTER ONE TITLE")
    (list  "CHAPTER-2-ID"   "CHAPTER TWO TITLE")
    ...
   )
)
                                                       ; A parameter-less function returns a list of meta-tag contribution                                     
(define (meta-tag-clauses)
   (list (list 'HTTP-EQUIV "Content-Type" 'content "text/html; charset = ISO-8859-1")  
         (list 'name "Generator" 'content "laml")
         (list 'name "description" 
                     'lang (meta-language language-preference)
                     'content "CONTENTS DESCRIPTION")
   )
)


(define course-home-url "URL")                       ; The full course home URL.

(define author-home-url "URL")                       ; The full author's home URL.

(define note-download-url "URL")                     ; The url from which to download the set of notes. If empty string no download

(define (cross-reference-location-hints location-list)  ; No location hints 
  "")


; --------------------------------------------------------------------------------------------------------

; Here it is possible to define useful scheme functions,
; for instance functions which abbreviate URL's to outside
; material. Such functions are highly recommended.

