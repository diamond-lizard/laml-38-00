;;;; This library provides http access to WWW servers from Scheme via
;;;; MzScheme's net library.  As such this library depends on MzScheme, and
;;;; it will only work with MzScheme. Notice that this is an exception seen
;;;; in relation to the other LAML libraries, styles and tool. <p>
;;;; Using this library it is possible in a straightforward way to read and return a
;;;; page from the Internet. We use an association list for the resulting
;;;; information. The library also provides a number of functions for
;;;; traversing a network (graph) of pages - in a simple and rather
;;;; primitive way. There is more work to be done on this in order to get a
;;;; real WEB crawling program. But the stuff in this library is good
;;;; enough as a starting point, at least.<p>
;;;; Most URL parameters to functions in this library are represented as strings.
;;;; In some situations, however, we use the MzScheme URL structure (record). Some functions
;;;; accept both representations. In the ideal situation, all functions should accept both formats.<p>
;;;; This library loads MzSchemes 'url.ss' and the LAML 'collect-skip.scm' library.<p>
;;;; <b><em>Please notice that this library is still incomplete, in progress, and experimental.</em></b>
;;;; .title Reference Manual of the URL Reading Library

(lib-load "collect-skip.scm")
(define buffer-length 1000000)
(require (lib "url.ss" "net"))

; ----------------------------------------------------------------------------------------
; Queue Datatype

; Make and return a queue
(define (make-queue init-lst)
  (cons 'queue init-lst))

; Is the queue empty
(define (empty-queue? queue)
  (null? (cdr queue)))

; Return the oldest element in the queue, and take it out of the queue.
; Mutates queue.
; Precondition: (not (empty-queue? queue))
(define (get-queue! queue)
  (let ((res (cadr queue)))
    (set-cdr! queue (cddr queue))
    res))

; Put el in queue as the youngest element.
; Muates queue.
; Complexity: O(length queue).
(define (put-queue! queue el)
  (let* ((last-entry (last-pair queue))
         (new-entry (cons el '())))
    (set-cdr! last-entry new-entry)))

; Return the last cons cell in lst.
; Precondition: (not (null? lst))
(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

; ----------------------------------------------------------------------------------------

;;; Page readers.
;;; The functions in this section read pages from the Internet.    


;; Return the HTML document (a string) referred by url-string.
;; Precondition: The URL points to an existing and accessible WWW page.
;; This function does NOT compensates for (re-read) moved pages.
(define (read-text-file-from-url url-string)
  (let ((text #f))
    (call/input-url
      (string->url url-string) 
      get-impure-port
      (lambda (ip)
        (set! text (read-text-file-from-input-port ip))
        (close-input-port ip)))
    text))

(define (page-moved? http-structure)
 (equal? (get 'status http-structure) "301"))

;; Read the page referred by url-string.
;; Return an association list including status and a contents key value pair.
;; The value of the contents key is probably the most interesting. 
;; The first element returned is the status key value pair.
;; The remaining elements are http header key value pairs.
;; Status "0" is returned in case of severe problems accessing the page, due to for instance
;; a malformed url.
;; This function compensates (re-reads) moved pages.
(define (read-http-alist url-string)
  (with-handlers ((exn? (lambda (exn) (list (cons 'status "0")))))
    (let ((real-url (string->url url-string)))
      (if (and (url-scheme real-url) (url-host real-url))
          (let* ((current-look-ahead-buffer (get-look-ahead-buffer))
                 (res (read-http-alist-1 url-string))
                 (res1 (if (page-moved? res)
                           (let ((new-location-url (get 'Location res)))
                             (if (not (equal? new-location-url url-string))
                                 (read-http-alist new-location-url)
                                 res))
                           res)))
              (put-look-ahead-buffer current-look-ahead-buffer)
              res1
            )
          (list (cons 'status "0"))))))

(define (read-http-alist-1 url-string)
  (let ((contents #f)
        (status #f)
        (props #f)
        (small-cap-key (lambda (pair) (cons (downcase-string (car pair)) (cdr pair))))
       )
    (set! buffer-length 10000000)
    (call/input-url
      (string->url url-string) 
      get-impure-port
      (lambda (ipp)
        (reset-look-ahead-buffer)
        (set! ip ipp)

        ; extract status
        (skip-while is-white-space?)
        (skip-while (negate is-white-space?)) ; skipping HTTP...
        (skip-while is-white-space?)
        (set! status (collect-until is-white-space?))

        (skip-while (negate end-of-line?))
        (skip-while end-of-line?)
        (set! props (read-http-properties))
        (skip-while is-white-space?)

        (set! contents (collect-until eof?))

        (close-input-port ipp)
      )
    )
    (append
     (list (cons 'status status))
     (map symbolize-key (map small-cap-key props))
     (list (cons 'contents contents)))))
       

(define (read-http-property)
 (let ((key (collect-until (char-predicate #\:))))
   (read-a-char)
   (skip-while is-white-space?)
   (let ((value (collect-until end-of-line?)))
     (cons key value))))

(define double-end-of-line-1 (list->string (list #\newline #\newline)))
(define double-end-of-line-2 (list->string (list #\return #\newline #\return #\newline)))

(define (read-http-properties)
  (ensure-look-ahead 4)
  (if (or (match-look-ahead? double-end-of-line-1) (match-look-ahead? double-end-of-line-2))
      '()
      (begin
       (skip-while is-white-space?) 
       (cons (read-http-property)
             (read-http-properties)))))


; url-target-exists? has been moved to the LAML compatibility functions, due 
; to the desire to check absolute URLs. 



; ---------------------------------------------------------------------------------------------------
;;; WWW crawling functions.  
;;; In this section we present WWW crawling, including a number of useful
;;; building block functions.

;; Check url, and recursively breadth first search all urls fulfilling restriction, up to max pages.
;; Restriction is a predicate on URLs.
;; This procedure is currently the main WWW crawler in this library.
(define (check-page url restriction max-pages)
 (set! end-queue '())
 (call-with-current-continuation
  (lambda (exit)
    (let ((url-queue (make-queue (list url))))
    (iterate-page-check url-queue '() restriction exit max-pages)))))

  
;; Return the first href value after character position start.
;; Returns a cons pair (href-value . position), or #f if not found.  
;; position is the position of the located href
;; Notice: this function should also be able to handle urls outside quotes.
;; Thus, the function is too simple in its current implementation.
(define (locate-href html-doc-str start)
  (let* ((href-index (substring-index html-doc-str start "href")))
    (if href-index
        (let* ((quote-start-index (find-in-string html-doc-str #\" href-index))
               (quote-end-index (find-in-string html-doc-str #\" (+ 1 quote-start-index)))
             )
          (if (and quote-start-index quote-end-index)
              (cons (substring html-doc-str (+ 1 quote-start-index) quote-end-index) href-index)
              #f))
        #f)))

; Return the list of all href values in html-doc-str
(define (locate-hrefs html-doc-str)
  (locate-hrefs-1 html-doc-str 0 '()))

(define (locate-hrefs-1 html-doc-str start res-list)
 (let ((first-href (locate-href html-doc-str start)))
   (if first-href
       (let ((href-val (car first-href))
             (href-pos (cdr first-href)))
         (locate-hrefs-1 html-doc-str (+ href-pos 1) (cons href-val res-list)))
       (reverse res-list))))

;; Is url an absolute http url
(define (absolute-http-url? url)
 (let ((url-str (if (url? url) (url->string url) url)))
   (and
     (>= (string-length url-str) 5)
     (equal? "http:" (substring url-str 0 5)))))

;; Is url a relative http url
(define (relative-http-url? url)                               ; important not to confuse it with LAML's relative-url? function
  (with-handlers ((exn? (lambda (exn) #f)))
    (let ((real-url (if (url? url) url (string->url url))))
      (let ((scheme (url-scheme real-url))
            (host (url-host real-url)))
        (and (not scheme) (not host))))))

;; Return the base URL of absolute-url. Does not always work.
(define (base-url-of-absolute-url absolute-url)
  (file-name-initial-path absolute-url))
   
;; Return a function that canonicalizes an url-str relative
;; to base-url. A canonicalized url is absolute and reasonable sound. Canonicaization either returns
;; an absolute http url or #f, in case of problems.
;; Precondition: base-url-str is an absolute-http-url.
;; base-url is either an url structure or a string.
(define (canonicalize-url base-url)
  (lambda (url-str)
    (with-handlers ((exn? (lambda (exn) #f)))
      (cond ((absolute-http-url? url-str) url-str)
            ((relative-http-url? url-str) 
              (combine-url/relative (if (url? base-url) base-url (string->url base-url)) url-str))  ; Feb 4, 2005: url-str??
            (else #f)))))


; Return the subset of canonicalized urls in url-list.
; base-url is either an url structure or a string.
(define (canonicalize-urls url-list base-url)
 (letrec ((canonicalizer (canonicalize-url base-url)))
   (filter (lambda (x) x) (map canonicalizer url-list))))

;; Does url refer to a page with "200" status?
(define (page-ok? url)
 (let* ((url-string (if (url? url) (url->string url) url)))
  (let ((http-structure (read-http-alist url-string)))
    (equal? "200" (get 'status http-structure)))))

;; A higher order function that returns a predicate which checks whether a URL
;; is in the same domain (on the same host) as as-url.
(define (url-in-domain as-url)
 (let* ((as-real-url (string->url as-url))
        (as-host (url-host as-real-url)))
  (lambda (url)
    (let ((real-url (string->url url)))
      (equal? (url-host real-url) as-host)))))



(define (report-page! url page-struct)
 (let ((status (get 'status page-struct)))
   (if (equal? status "200")
       (display-message url)
       (display-message
         (string-append "STATUS " status " " url)))))

;; A variable that is assigned to the queue at the end of iteration, as implemented by check-page
(define end-queue '())

(define (iterate-page-check url-queue visited-urls restriction exit max-pages)
  (cond ((= max-pages 0) (set! end-queue (cdr url-queue)) (exit 'max-limit-reached))
        ((empty-queue? url-queue) (exit 'finished))
        (else
         (let* ((front-page-url (get-queue! url-queue))   ; first url in queue
                (url-struct (read-http-alist front-page-url))  ; first page alist in queue
               )
            (if (not (member front-page-url visited-urls))
                (begin
                  (report-page! front-page-url url-struct)

                  (if (equal? "200" (get 'status url-struct))
                      (let* ((contents (get 'contents url-struct))
                             (links (locate-hrefs contents))
                             (canonical-links (canonicalize-urls links front-page-url))
                             (restricted-canonical-links (filter restriction canonical-links))
                            )
                         (for-each 
                           (lambda (suburl)
                             (put-queue! url-queue suburl))
                           restricted-canonical-links)))

                  (iterate-page-check url-queue (cons front-page-url visited-urls) restriction exit (- max-pages 1)))
                (iterate-page-check url-queue visited-urls restriction exit (- max-pages 1)))))


   )
)

(define (normark-restriction url)
 (with-handlers ((exn? (lambda (exn) #f)))
  (let ((url-str (string->url url)))
    (and (equal? (url-scheme url-str) "http")
         (equal? (url-host url-str) "www.cs.auc.dk")
         (equal? "~normark" (substring (url-path url-str) 1 9))))))



; ---------------------------------------------------------------------------------------------------

;;; Miscellaneous functions and procedures

;; The list of bookmarks that do not work due to the check performed by the procedure check-bookmark-list.
;; This variable is defined by the procedure check-bookmark-list.
(define problem-bookmarks '())

;; Check a bookmark list for page existence. The bookmark list is a list of
;; entries. An entry can be a bookmark of the form ("description" "url") or just an URL (a string).
;; The procedure assigns the variable problem-bookmarks to the bookmarks that experience access problems.
(define (check-bookmark-list bml)
 (set! problem-bookmarks '())
 (for-each 
   (lambda (bm)
    (let ((url (cond ((and (list? bm) (= 2 (length bm))) (cadr bm))
                     ((string? bm) bm)
                     (else (error (string-append "Bookmark is not an URL string or two tuple list: " (as-string bm))))))
          (txt (cond ((and (list? bm) (= 2 (length bm)) (string? (car bm)) (not (blank-string? (car bm)))) (car bm))
                     ((string? bm) bm)
                     ((list? bm) (cadr bm))
                     (else "???")))
         )
     (display txt)
     (newline)
     (if (url-target-exists? url)
         (begin (display "OK") (newline) (newline))
         (begin (display "NOT OK") (set! problem-bookmarks (cons bm problem-bookmarks)) (newline) (newline)))))
   bml)
   (set! problem-bookmarks (reverse problem-bookmarks))
)
