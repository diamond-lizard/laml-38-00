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


;;;; This is a collection of Scheme functions used for CGI programming purposes.
;;;; We use the so-called POST method, according to which input to a CGI program
;;;; is read from standard input.<p>
;;;; 
;;;; The function extract-form-input reads the necessary
;;;; amounts of input (as determined by the environment variable CONTENT_LENGT) and
;;;; returns a Lisp association list.<p>
;;;; 
;;;; The function extract-url-parameters returns and decodes the url parameters.
;;;; The url parameters are the part of an URL found after the question mark.
;;;; Using the CGI interface, these are passed via the environment variable QUERY_STRING.
;;;; Like extract-form-input, the result of this function is also a Lisp association list.<p>
;;;; 
;;;; The input functions (extract-form-input and extract-url-parameters) take a parameter, which
;;;; is used for testing purposes in the situation where we are not connected to a WWW server.
;;;; The global variable cgi-testing controls whether this parameter is used.
;;;; The variable cgi-testing must be defined in the context of this library.<p>
;;;;
;;;; The function time-extend extends an association list with time and date information.
;;;; This time stamping is quite useful when we extract data from an input form.<p>
;;;; 
;;;; On the output side, the function write-page writes a page to standard output.
;;;; If cgi-testing, the page is written to a local test-file.<p>
;;;; The variable test-file must be defined in the context of this library.<p>
;;;; 
;;;; The function make-url-parameters produces and encodes url paramters. This is used
;;;; if we make an URL which passes parameters to another CGI program.
;;;; The function make-url makes and returns an URL.<p>
;;;;
;;;; The CGI library depends on the accompanying <a href = "encode-decode.html">URL encode and decode library</a>.<p>
;;;;
;;;; There exists a simple <a href="../../tutorial/cgi-programming/cgi-programming.html">tutorial CGI example</a> in elucidative style.
;;;; .title Reference Manual of the CGI library


;;; CGI output functions. 

;; Write output (a text or a LAML AST) to standard output, or if cgi-testing, to test-file.
;; .parameter output The text to be written to std output (a string) or the LAML AST to be linearized to standard output.
(define (cgi-write output)
    (if cgi-testing 
        (begin
          (if (file-exists? test-file) (delete-file test-file))
          (cond 
              ((string? output)
                 (save-on-file output test-file))
              ((ast? output)
                 (save-on-file (xml-render output) test-file))
          )
        )
        (cond 
              ((string? output)
                 (writeln "Content-type: text/html") (writeln)
                 (display  output)  ; sending it to standard output
              )

              ((ast? output)
                 (writeln "Content-type: text/html") (writeln)
                 (render-to-output-port output (current-output-port) 'prolog))

              (else (laml-error "cgi-write: You should write either string or AST"))
        )
    ))

; Render the HTML ast to standard output.
; This procedure corresponds to cgi-write. Use this procedure if you make use of AST-based mirrors.
; Obsolete. Use cgi-write instead this procedure.
(define (cgi-write-html-ast ast)
 (let ((content-type-string (string-append "Content-type: text/html" (as-string #\newline) (as-string #\newline))))  
  (render-to-output-port ast (current-output-port) content-type-string)))

  

(define (write-page title body . color-list)
;; Writes an HTML page to standard output. 
;; The page is described in terms of title, body, and an optional color-list.
;; These three parameters are passed through the HTML function page.
;; If cgi-testing is true, write the page to test-file (a global variable) instead of to standard output.
  (let ((the-output (apply page (append (list title body) color-list))))
    (cgi-write the-output)))

(define writeln
;; Writes a newline on standard output
;; .form (writeln . args)
  (lambda args
    (for-each display args)
    (newline)))


;; Ends and exit the CGI program. 
;; Normally issued as the last command in a cgi program.
;; When we are cgi-testing, this is the empty command.
(define (end)
  (if (not cgi-testing) (exit)))


;;; CGI input functions. 

; Read n chars from standard input and return as string (OBSOLETE).
; Use the much more efficient version read-std-input-1
;(define (read-std-input n)
;  (read-n n ""))

;; Read n chars from standard input and return as string.
(define (read-std-input n)
  (read-n-1 0 n (make-string n #\space)))

; Reads n characters from standard input, and return result.
; The parameter is used for a representation of the result, while reading.
; Initially it should be some arbitrary string of length n.
(define (read-n-1 i n str)
 (if (> n 0)
   (let ((ch (read-char )))
     (string-set! str i ch)
     (read-n-1 (+ i 1) (- n 1) str))  
   str))

(define (length-of-std-input)
;; Return the number of chars on standard input
  (let* ((n-as-str (getenv "CONTENT_LENGTH")))
    (string->number n-as-str)))


;; Return the url input (a string) also known as the query string. 
;; This functions does not kind of decoding.
;; If testing, a parameter is required; the parameter plays the role of the input source (an URL encoded string).
(define (cgi-url-input . the-input)
 (if cgi-testing
     (car the-input)
     (getenv "QUERY_STRING")))


(define (read-n n str)
; Reads n characters from standard input, and return result (OBSOLETE - too slow).
; The parameter is used for accumulating the string iteratively.
; Initially it should be empty
 (if (> n 0)
   (let ((ch (read-char)))
     (read-n (- n 1) (string-append str (make-string 1 ch))))  ; string-append potentially very inefficient!
   str))


(define (extract-form-input . testfile)
;; Extract form input from std input, and return the decoded data as an association list. 
;; Assumes that the input is application/x-www-form-urlencoded (which is the default encoding of an HTML form).
;; The optional parameter must be supplied if testing. In that case input
;; is taken from the file instead of std input.  The file must contain an a-list.
    (if cgi-testing
        (file-read (car testfile))
        (let* ((input (read-std-input (length-of-std-input))))
           (extract-attributes input))))

;; Extend the the a-list with date, time and second-cound fields (all strings).
(define (time-extend a-list second-count)
 (let* ((td (date-time second-count))
        (tm (cadr td))
        (dt (car td))
        (extended-a-list
             (cons (cons "date" dt) (cons (cons "time" tm) (cons (cons "second-count" (number->string second-count)) a-list)))))
  extended-a-list))


;;; Other CGI functions.

;; Make the last part of an url - the part after the question mark.
;; The partial url is made from a list of keys and a list of values. They are expected to be of the same length.
;; Make and encode the keys and values in key-list and val-list, respectively.
;; Returns a string.
;; This is the original version used before August 23, 2011.
(define (make-url-parameters-and-encode key-list val-list)
  (encode-a-list (map2 cons key-list val-list)))

;; Make the last part of an url - the part after the question mark.
;; The partial url is made from a list of keys and a list of values. They are expected to be of the same length.
;; A variant which does not URL encode keys and values.
;; Returns a string.
;; New as of August 23, 2011.
(define (make-url-parameters key-list val-list)
  (let* ((url-par-a-list (map2 cons key-list val-list))
         (url-par-string-list (map (lambda (pair) (string-append (as-string (car pair)) "=" (as-string (cdr pair)))) url-par-a-list)))
    (list-to-string url-par-string-list "&")))
    

;; Return a decoded URL input, as taken from the QUERY_STRING and decoded appropritely to an association list.
;; The optional parameter the-input is used in case of cgi-testing, namely as the encoded url parameter.
;; If not testing, the function is called without parameters.
;; In that case, the parameters are taken from an environment variable, QUERY_STRING.
(define (extract-url-parameters . the-input)
 (map symbolize-key
  (if (not cgi-testing)
      (extract-attributes (cgi-url-input))
      (extract-attributes  (car the-input)))))

(define (activity-url cgi-program key-list val-list)
  (string-append activity-url-prefix cgi-program "?" (make-url-parameters key-list val-list)))

;; Return an URL from the parameters.
;; The URL includes URL parameters (the part which follows the question mark).
;; The two last parameters are passed into make-url-parameters after a question mark
(define (make-url url-prefix cgi-program key-list val-list)
 ; a more general version of activity-url
  (string-append url-prefix cgi-program "?" (make-url-parameters key-list val-list)))


(define (save-registrations a-list f g)
;; A special purpose function which saves the a-list in two files f and g.
;; The a-list is the registered data. Save this structure in file f, appart from
;; the contents field with key 'contributin' of a-list, which is to be saved in file g.
;; No file deletion nor directory updating is done in this function. 
 (let ((contr (get 'contribution a-list))
       (a-list-but-contents 
         (filter (lambda (a) (not (eq? (car a) 'contribution))) a-list)))
   (save-a-list a-list-but-contents f)
   (write-text-file contr g)
))


;; Return a list of the file name components separated by underscore.
;; This function is useful in case we save various kinds of transactions on a
;; file whose name represents part of the file contents. The components of the
;; file name is separated by the underscore character.
(define (split-file-name filename)
 (let ((split-points (split-point-list filename #\_)))
   (split-string filename split-points )))

(define (split-point-list str split-char)
  ; return a list of positions of the split-char as found in str.
 (let ((str-lgt (string-length str)))
  (let loop
     ((pos (- str-lgt 1))
      (res '()) )
   (cond ((and (= 0 pos) (eqv? (string-ref str pos) split-char)) (cons pos res))
         ((= 0 pos) res)
         ((eqv? (string-ref str pos) split-char)
            (loop (- pos 1) (cons pos res)))
         (else (loop (- pos 1) res))))))

(define (split-string str split-list)
  ; str is splitted in components, separated by points in split-list
  ; return the list of components.
 (let ((split-list-1 (cons -1 (append split-list (list (string-length str))))))
  (map2
   (lambda(f t)
      (substring str (+ 1 f) t))
   split-list-1 (cdr split-list-1))))


; ---------------------------------------------------------------------------------------------------------------
;;; Multipart input.

;; Extract form input from std input, and return the decoded data as an association list. 
;; Assumes that the input is multipart/form-data, which is the encoding that provides for file uploading.
;; The parameter cur-time is used as part of the file name of uploaded files - in order to ensure unique naming.
;; The optional parameter must be supplied if testing. In that case input
;; is taken from the file instead of std input.  The file is assumed to contain the raw textual contents.
(define (extract-multipart-form-input cur-time . testfile)
 (let ((input-port (if cgi-testing (open-input-file (car testfile)) (current-input-port))))
   (set! ip input-port) ; defined in the library collect-skip
   (let ((res (multipart-decode cur-time)))
     (close-input-port input-port)
     res
   )))


; ---------------------------------------------------------------------------------------------------

; (define opg #f)
; 
; (define (do-load)
;  (load "cgi.scm")
;  (load "encode-decode.scm")
;  (load "collect-skip.scm")
;  (load "file-read.scm")
;  (set! cgi-testing #t)
;  (set! content_type-and-boundary (lambda () (cons "multipart/form-data" "-----------------------------7d03bb315033c")))
;  (set! collected-form-alist '())
;  (set! multiform-file-path #f)
; 
; )





