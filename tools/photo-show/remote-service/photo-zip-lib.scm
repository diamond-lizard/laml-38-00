(define photo-service-background-color green1)
(define visual-index-photo-width 200) ; same constant in ../photo-show.scm
(define trailing-number-of-empty-entries 3)

(define (photo-number-interval n m width)
  (map 
    (lambda (i)
       (pad-integer (as-string i) width))
    (number-interval n m)))

; Given the positive integer i (as a string or integer) return a string with i and sufficient leading zeros
; such that the string length of the result becomes width
(define (pad-integer i width)
 (let ((is (as-string i)))
  (cond ((< (string-length is) width) (pad-integer (string-append "0" is) width))
        ((>= (string-length is) width) is))))

(define (make-photo-entry-1 prefix-part number-part suffix-part)
 (let ((photo-file (string-append prefix-part number-part suffix-part)))
  (list photo-file default-time default-upper-caption default-upper-size default-lower-caption default-lower-size)))

(define (strip-trailing-char str)
 (if (> (string-length str) 0)
     (substring str 0 (- (string-length str) 1))
     str))


(define (cleanup-files-and-dirs base-dir photo-list)
 (let ((index-file (string-append base-dir "index.html"))
       (visual-index-file (string-append base-dir "index-visual.html"))
       (visual-editor-file (string-append base-dir "index-visual-editor.html"))
       (first-file (string-append base-dir "first.lsp")) 
      )
  (if (file-exists? index-file) (delete-file index-file))
  (if (file-exists? visual-index-file) (delete-file visual-index-file))
  (if (file-exists? visual-editor-file) (delete-file visual-editor-file))
  (if (file-exists? first-file) (delete-file first-file))

  (for-each 
    (lambda (file)
     (let ((file-proper-name (file-name-proper file)))
      (delete-file (string-append base-dir "html/" file-proper-name ".html"))
      (delete-file (string-append base-dir "html/" file-proper-name "-stop.html"))
     )
    )
    (map first photo-list))

  (delete-directory (string-append base-dir "html"))
  (delete-directory (strip-trailing-char base-dir))))


; ----------------------------------------------------------------------------------------------------------- 
; Used in start forms:

(define (row-presenter row)
  (list (font-size 2 (car row)) "" (con (cadr row) (p))))

(define font-advice-message "1 = very small, 3 = normal, 6 = very large")
(define continuation-advice "Relative to the photo dir. If empty the photo show will be cyclic.")
(define home-advice "A home URL relative to the photo dir.")
(define empty-advice "This field can be left empty.")

(define credits 
   (font-size 2 (con "This photo service system is designed and programmed by Kurt Nørmark (c), Aalborg University,
                      Denmark using " (a-tag "http://www.cs.auc.dk/~normark/laml/" (font-color black "LAML")) " technology.")))

(define (tm x) (con (horizontal-space 3) (em (font-size 2 x))))
(define (sc x) (font-size 2 (kbd x)))

; ----------------------------------------------------------------------------------------------------------- 

(define (as-string-default x)
  (cond ((string? x) x)
        ((and (boolean? x) (not x)) "")
        (else (as-string x))))





(define (present-photo-list photo-list)
  (table-3 0
    (list 60 120 60 300 300)
   
    (cons 
      (map b (list "Number" "Photo file name" "Time" "Upper caption" "Lower caption"))

     (map2 present-photo-list-entry 
              (number-interval 1 (length photo-list))
           (map enforce-defaults-in-photo-entry photo-list)))))

;; Return a modified photo entry in which captions and font sized as defaulted (#f).
(define (enforce-defaults-in-photo-entry photo-entry)
  (let ((fn (photo-file-name-of photo-entry))
        (tm (photo-show-time-of photo-entry))
        (uc (photo-upper-caption-of photo-entry))
        (lc (photo-lower-caption-of photo-entry))
       )
    (make-photo-entry fn #f #f #f #f #f)))
  

(define (number-interval-with-factor factor n m)
  (map 
    (lambda (i) (* i factor))
    (number-interval n m)))

(define (make-id str n)
  (as-symbol (string-append str (as-string n))))

(define (present-photo-list-entry n entry)
  (let ((fn (photo-file-name-of entry))
        (tm (photo-show-time-of entry))
        (uc (photo-upper-caption-of entry))
        (lc (photo-lower-caption-of entry))
       )
    (list
      (text-line (make-id "n" n) 3  (as-string (* 10 n)))
      (text-line (make-id "f" n) 20 (as-string-default fn))
      (text-line (make-id "t" n) 3  (as-string-default tm))
      (textarea-1 (make-id "u" n) 3 40 (as-string-default uc))
      (textarea-1 (make-id "l" n) 3 40 (as-string-default lc))
    )
  )
)



; Make n empty entries
(define (make-empty-photo-entries n)
 (map
   (lambda (n)
     (make-photo-entry #f #f #f #f #f #f))
   (number-interval 1 n)))





