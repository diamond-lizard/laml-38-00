; The LAML library and programs written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999  Kurt Normark.
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


;;;; URLs are encoded in order to avoid special characters causing problems in an Internet adresss. 
;;;; This library provides functions to encode and decode parts of an URL. The main functions are
;;;; encode-a-list and string-decode (also known as extract-attributes). The library also supports
;;;; decoding of the multipart/form-data format, which can be used for file uploading. The main function
;;;; in this area is multipart-decode.
;;;; Encode-a-list takes an association list and encodes it to a string with '=' and '&' representation.
;;;; string-decode (extract-attributes) takes a string, such as produced by encode-a-list, and returns an association list.
;;;; Thus, each of the two functions are inverse version of the other function.<p>
;;;; This library requires the <a href = "color.html"> color library</a>  to be loaded (not for coloring purpose, but in order to get access to the 
;;;; the function that convert hexadecimal numbers to decimal numbers). In case multipart-decode is used, also the <a href="collect-skip.html">collect-skip library</a>
;;;; is required.<p>
;;;; This library is used by the accompanying <a href = "cgi.html">CGI library</a>.<p>
;;;; .title Reference Manual of the URL Encode and Decode Library

;;; Encoding stuff. 

;; A vector which controls the working of encode-string and encode-a-list (and others).
;; The vector contains 128 entries. Entry n contains the encoding of character n.
;; All % encodings must be strings of lenght exactly 3.
;; Chars outside the range (chars between 128 and 255) are intended always to be encoded.

(define encode-vector
  (list->vector 
    '("%00" "%01" "%02" "%03" "%04" "%05" "%06" "%07" "%08" "%09" "%0a" "%0b" "%0c" "%0d" "%0e" "%0f" "%10" "%11" "%12" "%13" "%14" "%15" "%16" "%17" "%18" "%19" "%1a" "%1b" "%1c" "%1d" "%1e" "%1f" "%20" "%21" "%22" "%23" "%24" "%25" "%26" "%27" "%28" "%29" "%2a" "%2b" "%2c" "%2d" "%2e" "%2f" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "%3a" "%3b" "%3c" "%3d" "%3e" "%3f" "%40" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "%5b" "%5c" "%5d" "%5e" "%5f" "%60" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "%7b" "%7c" "%7d" "%7e" "%7f")))

(define (encode-char char)
  (let* ((n (char->integer char)))
   (if (and (>= n 0) (<= n 128))
       (vector-ref encode-vector n)
       (string-append "%" (number-in-base n 16)))))

;; Encode the string str, thus protecting a number of special characters.
;; The encoding is controlled by the list encode-vector.   
(define (encode-string str)
  (encode-string-help str 0 "") )

(define (encode-string-help str i res)
  (if (= i (string-length str))
      res
      (encode-string-help str (+ i 1)
        (string-append res (encode-char (string-ref str i))))))

;; Encode an a-list (both keys and values). Return a string in whichs the encoded keys and values are puted together as:
;; <pre> key1 = val1 & key2 = val2 </pre>.
(define (encode-a-list a-list)
  (let ((res (encode-a-list-1 a-list)))
    (if (not (null? a-list))  ; remove traling &
        (substring res 0 (- (string-length res) 1))
        res)))

; helping operation to encode-a-list, doing the real work
(define (encode-a-list-1 a-list)
  (if (null? a-list)
      ""
      (let ((key (car (car a-list)))
            (val (cdr (car a-list))))
        (string-append (encode-string (as-string key))
                       "="
                       (encode-string (as-string val))
                       "&"
                       (encode-a-list-1 (cdr a-list)))))) 
         
  

; ================================================================================================================

;;; Decoding stuff. 
;;; Because spaces and certain special characters may not appear in URLs, an
;;; encoding scheme is used when it is necessary to generate a URL that
;;; includes them:  Each space is converted to a plus sign, and each special
;;; character is replaced by a triplet of characters consisting of a percent
;;; sign and two hexadecimal digits, which together identify the ASCII code
;;; for the character.

; An internal string in which the individual strings of the alist are represented.
(define decode-out-string "")

;; Decodes and extract the attributes from the string str-a-list and return an association list.
;; Assume that str-a-list is url encoded (application/x-www-form-urlencoded).
;; In CGI programming, a query string consists of a sequence of
;; equations separated by ampersands, with the name of some attribute on
;; the left-hand side of each equation and the value of that attribute on
;; the right-hand side.  
;; This function returns an association list from the string str-a-list, which represents
;; an association list. In the string, the character '=' separates keys and values. 
;; Similarly, the character '&' separate key-value pairs. In addition, the '+'
;; character represents a blank space.
;; .parameter str-a-list The part a URL that follows the question mark.
;; .internal-references "alias function" "extract-attributes"
(define (string-decode str-a-list)
  (set! decode-out-string (make-string (string-length str-a-list) #\space))
  (let ((res (decode-string-alist-1 str-a-list 0 (string-length str-a-list) decode-out-string 0 '() "" 'in-key-or-value)))
    (cond ((and (= 1 (length res)) (empty-string? (car res))) '())
          (else (propertylist-to-alist (reverse res))))
))

;; Decodes and extract the attributes from the string str-a-list and return an association list.
;; Just and alias of string-decode.
;; .form (extract-attributes str-a-list)
;; .parameter str-a-list The part a URL that follows the question mark.
;; .internal-references "alias function" "string-decode"
(define extract-attributes string-decode)

; The helping function of decode-string-alist, which drives the underlying state machine
(define (decode-string-alist-1 instr inptr inlength outstr outptr prop-list collected current-state)
  (if (= inptr inlength)

      (cond 
            ((eq? current-state 'in-key-or-value) (cons (substring outstr 0 outptr) prop-list))  ; include the last string
            ((eq? current-state 'hex3) (cons (substring outstr 0 outptr) prop-list))             ; also here
            ((eq? current-state 'equal-accepted) (cons "" prop-list))                            ; include a trailing empty string
            ((eq? current-state 'ampersand-accepted) prop-list)                                  ; just return prop-list
            (else (error "decode-string-a-list-1: Strange end of string input")))
                
      (let* ((inch (string-ref instr inptr))
             (trans-res (decode-string-transition current-state inch collected))
             (next-state (car trans-res))
             (next-collected (cdr trans-res))
            )

       (cond 
          ((and (eq? next-state 'in-key-or-value) (eq? inch #\+)) (string-set! outstr outptr #\space))  ; handle '+'
          ((eq? next-state 'in-key-or-value) (string-set! outstr outptr inch))                          ; normal case
          ((eq? next-state 'hex3) (string-set! outstr outptr next-collected))                           ; insert hex conversion
       )

       (decode-string-alist-1 instr (+ 1 inptr) inlength
                              outstr 
                              (cond ((eq? next-state 'equal-accepted) 0)
                                    ((eq? next-state 'ampersand-accepted) 0)
                                    ((eq? next-state 'hex1) outptr)
                                    ((eq? next-state 'hex2) outptr)
                                    (else  (+ outptr 1)))
                              (if (or (eq? next-state 'equal-accepted) (eq? next-state 'ampersand-accepted))
                                  (cons (substring outstr 0 outptr) prop-list)
                                  prop-list)
                              next-collected
                              next-state)
   )))



; STATES in underlying state machine
; equal-accepted:          Just accepted a = character
; ampersand-accepted:      Just accepted a & character
; in-key-or-value:         Just accepted a non hex char in a key or value
; hex1, hex2, hex3:        Just accepted first, second and third char of a hex decoding

(define hex1-state (cons 'hex1 ""))
(define equal-accepted-state (cons 'equal-accepted ""))
(define ampersand-accepted-state (cons 'ampersand-accepted ""))
(define in-key-or-value-state (cons 'in-key-or-value ""))


; The state machine transition function.
; The third parameter is only used when reading %xy. We collect the x and y characters in this parameter.
(define (decode-string-transition in-state ch hex-collect)
 (let ((char (as-string ch)))
   (cond 
         ((eq? in-state 'in-key-or-value)
            (cond 
                  ((eqv? ch #\%)                                           hex1-state)
                  ((eqv? ch #\=)                                           equal-accepted-state)
                  ((eqv? ch #\&)                                           ampersand-accepted-state)
                  (else                                                   in-key-or-value-state)
             ))

         ((eq? in-state 'hex1)
                                                                          (cons 'hex2 (as-string ch)))

         ((eq? in-state 'hex2)
                                                                          (cons 'hex3
                                                                                (two-digit-hex-to-char
                                                                                   (string-append hex-collect (as-string ch)))))

         ((eq? in-state 'hex3)
             (cond 
                   ((eqv? ch #\&)                                          ampersand-accepted-state)
                   ((eqv? ch #\=)                                          equal-accepted-state)
                   ((eqv? ch #\%)                                          hex1-state)
                   (else                                                  in-key-or-value-state)
             ))

         ((eq? in-state 'ampersand-accepted)
            (cond 
                  ((eqv? ch #\%)                                           hex1-state)
                  ((eqv? ch #\=)                                           equal-accepted-state)
                  ((eqv? ch #\&)                                           ampersand-accepted-state)
                  (else                                                   in-key-or-value-state)
             ))

         ((eq? in-state 'equal-accepted)
            (cond 
                  ((eqv? ch #\%)                                           hex1-state)
                  ((eqv? ch #\=)                                           equal-accepted-state)
                  ((eqv? ch #\&)                                           ampersand-accepted-state)
                  (else                                                   in-key-or-value-state)
             ))



         (else                                                            (error (string-append 
                                                                                    "decode-string-transition: Unknown state: "
                                                                                    (as-string in-state))))

  )))
    

; A specialized two digit hex to char conversion function.
(define (two-digit-hex-to-char two-char-string)
  (if (= 2 (string-length two-char-string))
      (let ((c1 (hex-ciffer->decimal-ciffer (string-ref two-char-string 0)))
            (c2 (hex-ciffer->decimal-ciffer (string-ref two-char-string 1)))
           )
       (integer->char (+ (* c1 16) c2)))
      (error "two-digit-hex-to-char: First parameter must be a string of length two")))


; ================================================================================================================================
;;; Multipart form decoding (file uploading).
;;; The main function in this part is multipart-decode, which is able to extract keyword value pairs,
;;; as done by string-decode. In addition, multipart-decode will save uploaded files in a particular
;;; directory on the server. The directory is given as a hidden field of the accompanying
;;; HTML form, called multipart-form.

;; Return the association list of decoded keyword value pairs from a multipart encoded 
;; standard input. As a side effect, write uploaded files into the servers file system.
;; Information about the uploaded file is stored in a four-tuple (list of length 4) under a keyname corresponding to the parameter called name
;; in the file-upload form. The four-tuple is (file-path-at-client file-path-at-server contents-type server-directory-url).
;; file-path-at-client is the full file path of the file on the client.
;; file-path-at-server is the full file path of the file on the server. The function upload-target-filename can be redefined in order to 
;; determine the exact name on the server. From file-path-at-server the file name proper on the server can easily be extracted.
;; The constituent contents-type is the contents type, such as 'image/bmp'. 
;; Finally, server-directory-url is the URL prefix of the directory in which the uploaded files reside. This information is useful
;; if you are going to link to the uploaded file.
;; cur-time is the current time (an integer second count) used to unique naming of the target file in the servers file system
;; together with initial path, which is passed in a hidden field.
;; .reference "accompanying function" "multipart-form" "../html4.0-loose/man/convenience.html#multipart-form"
;; .reference "accompanying function" "file-upload" "../html4.0-loose/man/convenience.html#file-upload"
;; .reference "accompanying function" "file-upload" "../html4.0-loose/man/convenience.html#file-upload"
;; .internal-references "determination of file name on server" "upload-target-filename"
(define (multipart-decode cur-time)
 (let* ((c-and-b (content_type-and-boundary))
        (content_type (car c-and-b))
        (boundary (string-append "--" (cdr c-and-b))))  ; HACK!!!  ; boundary is wrong - not long enough - read it from first line.
  (display-mes-if-debugging boundary)
  (if (equal? content_type "multipart/form-data")
      (begin
        (pass-next-boundary! boundary) ; now right after the boundary
        (multipart-decode-1! cur-time boundary)
      )
      (error (string-append "multipart-decode: content_type must be 'multipart-decode'")))))

(define debugging-multiform-decode #f) ; normally #f

(define debug-port
  (if debugging-multiform-decode
      (open-output-file "/user/aabudd/.public_html/cgi-bin/debug-info")
      #f))

(define (display-mes-if-debugging mes)
  (if debugging-multiform-decode
      (display (string-append (as-string mes) (as-string #\newline)) debug-port)))

; In this variable we collect key value pairs
(define collected-form-alist '())

; In this variable we catch the file path information from a (hidden) HTML field.
(define multiform-file-path #f)

; In this variable we catch the directory URL information from a (hidden) HTML field.
(define multiform-dir-url #f)

; Precondition:we have just passed a boundary
(define (multipart-decode-1! cur-time boundary)
 (if (not (at-end-of-form-input)) ; @c
  (let ((content-disposition (read-content-disposition))
        (name (read-name))
        (file-name (read-possible-filename))  ; full path on client's file system
       )
    (cond ((and file-name (not (blank-string? file-name))) ; @a
            (let* ((possible-content-type (read-possible-content-type)) 
                   (extension (file-name-extension file-name))
                   (proper-filename (file-name-proper file-name))
                   (target-file-path (string-append multiform-file-path (upload-target-filename proper-filename extension cur-time)))
                   (op (open-output-file target-file-path))
                   )
              (set! collected-form-alist (cons (cons name (list file-name target-file-path possible-content-type multiform-dir-url)) collected-form-alist))
              (read-a-string 4)         ; CR LF CR LF - interspacing before file content
              (pass-uploaded-file! op boundary) ; now boundary has just been passed
              (close-output-port op)
              (multipart-decode-1! cur-time boundary) ; ITERATE
              ))
        ((and file-name (blank-string? file-name))  ; @d drop name, skip possible file until and including next boundary.
            (skip-until-string boundary #t) ; include boundary
            (multipart-decode-1! cur-time boundary) ; ITERATE
        )
        (else
          (let ((value (read-value boundary)))
            (begin
              (set! collected-form-alist (cons (cons name value) collected-form-alist))
              (catch-possible-file-path! name value) ; @b
              (catch-possible-directory-url! name value)
              (pass-next-boundary! boundary)
              (multipart-decode-1! cur-time boundary) ; ITERATE
            )
          )
        )
    )
  )
  (reverse collected-form-alist)  ; returns the form alist.
 )
)


(define (pass-uploaded-file! op boundary)
  (pass-uploaded-file-1! op boundary 0 (string-length boundary))
)  

; we have matched match-pos chars in boundary. I.e., all charters from position
; 0 through match-pos - 1 match the boundary prefix
(define (pass-uploaded-file-1! op boundary match-pos boundary-lgt)
 (if (= boundary-lgt match-pos) ; @a
     'done
     (let ((ch (read-a-char))
	   (match-ch (string-ref boundary match-pos))
	   )
       (cond ((eqv? ch match-ch)
	      (display-mes-if-debugging (string-append "Matches " (as-string ch) " match-pos: " (as-string (+ match-pos 1))))
	      (pass-uploaded-file-1! op boundary (+ match-pos 1) boundary-lgt))
	     ((and (not (eqv? ch match-ch)) (> match-pos 0))
	      (display-mes-if-debugging (string-append "Writing " (substring boundary 0 match-pos) "to op"))
	      (write-string-to-port (substring boundary 0 match-pos) op) ; (@b) write matched part of boundary
	      (write-char ch op)
	      (pass-uploaded-file-1! op boundary 0 boundary-lgt))
	     ((not (eqv? ch match-ch)) 
	      (display-mes-if-debugging (string-append "Passing " (as-string ch) " through"))
	      (write-char ch op)
	      (pass-uploaded-file-1! op boundary 0 boundary-lgt))
	     ))))


(define (pass-next-boundary! boundary)
  (display-mes-if-debugging "pass-next-boundary")
  (skip-string boundary "Boundary expected"))  

; first skip two line shifts and then return the string before next boundary
(define (read-value boundary)
  (display-mes-if-debugging "read-value")
  (read-a-string 4) ; CR LF CR LF
  (let* ((val (collect-until-string boundary))
         (lgt (string-length val)))
    (if (>= lgt 2)
        (substring val 0 (- lgt 2))  ; exclude trailing CR LF
        val)))

; return a Content-Type, or #f if there is no such clause
(define (read-possible-content-type)
  (display-mes-if-debugging "read-possible-content-type")
  (ensure-look-ahead 16)
  (if (substring-index (look-ahead-prefix 16) 0 "Content-Type") ; is there a content type ahead
      (begin 
        (skip-while is-white-space?)
        (skip-string "Content-Type" "Content-Type expected")
        (skip-while is-white-space?)
        (skip-while (char-predicate #\:))
        (skip-while is-white-space?)
        (let ((contenttype (collect-until is-white-space?))) ; now at first white char after the content type
          contenttype)
      )
      #f))

(define (is-white-space-or-semicolon? ch)
  (or (is-white-space? ch) (eqv? #\; ch)))

; return a filename path if there, or #f if there is no such clause
(define (read-possible-filename)
  (display-mes-if-debugging "read-possible-filename")
  (skip-while (char-predicate #\;))
  (ensure-look-ahead 9)
  (if (equal? (look-ahead-prefix 9) " filename")  ;; for saarbar. antager kun eet space foer filename
      (begin
        (skip-while is-white-space?)
        (skip-string "filename" "filename expected")
        (skip-while is-white-space?)
        (skip-while (char-predicate #\=))
        (skip-while is-white-space?)
        (skip-string (as-string #\") "String quote expected after 'filename='")
        (let ((filename (collect-until (char-predicate #\"))))
          (skip-string (as-string #\") "String quote expected after 'name=...'")
          filename)
      )
      #f))

; at end if there are two trailing dashes RIGHT AFTER the boundary (always correct?)
(define (at-end-of-form-input)
  (display-mes-if-debugging "at-end-of-form-input")
  (ensure-look-ahead 2)
  (equal? (look-ahead-prefix 2) "--"))
 

; read a content-disposition clause and return its value (normally "form-data").
; leaves the position right after the trailing semicolon.
(define (read-content-disposition)
  (display-mes-if-debugging "read-content-disposition")
  (skip-until-string "Content-Disposition:" #t)
  (skip-while is-white-space?)
  (let ((val (collect-until (char-predicate #\;))))
    (skip-string ";" "Semicolon expected after content-Disposition form data")
    val))

(define (read-name)
  (display-mes-if-debugging "read-name")
  (skip-until-string "name=" #t)
  (skip-while is-white-space?)
  (skip-string (as-string #\") "String quote expected after 'name='")
  (let ((val (collect-until (char-predicate #\"))))
    (skip-string (as-string #\") "String quote expected after 'name=...'")
    val))

;; This function determines the proper name and extension of a downloaded file (no initial path information must be given) as represented in the server's file system.
;; It is essential in most situations to ensure uniqueness of the file on the server. Therefore it is recommended that
;; cur-time (the current time in seconds elapsed since January 1, 1970) is used in the file file. This function can be
;; redefined if you want to define the upload target filename in a special way. The default is (string-append proper-name "-" cur-time "." extension),
;; or if the extension is empty, (string-append proper-name "-" cur-time).
(define (upload-target-filename proper-name extension cur-time)
  (string-append proper-name "-" (as-string cur-time)
                 (if (not (empty-string? extension)) "." "") extension)) 

; return the cons pair of contents type and boundary string from environment variable CONTENT_TYPE.
(define (content_type-and-boundary)
 (let* ((content-type (getenv "CONTENT_TYPE"))
        (pos (find-in-string content-type #\; 0)))
   (if pos
     (let* ((real-content-type (substring content-type 0 pos))
            (pos1 (skip-chars-in-string content-type white-space-char-list (+ pos 1)))
            (pos2 (find-in-string content-type #\= pos1)))
       (if pos2 
         (let ((boundary (substring content-type (+ 1 pos2) (string-length content-type))))
           (cons real-content-type boundary))
         (error (string-append "multipart-decode: unexpected content type - case 1: " content-type)))
     )
     (error (string-append "multipart-decode: unexpected content type - case 2: " content-type))
   )))

(define (catch-possible-file-path! name value)
  (display-mes-if-debugging "catch-possible-file-path!")
  (if (equal? name "target-directory!!!")
      (set! multiform-file-path value)))


(define (catch-possible-directory-url! name value)
  (display-mes-if-debugging "catch-possible-directory-url!")
  (if (equal? name "target-directory-url!!!")
      (set! multiform-dir-url value)))
