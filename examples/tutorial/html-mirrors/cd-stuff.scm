;;; cd entry subclauses.
(define (cd-number n)
  (tag-data 'cd-number (as-string n)))

(define (cd-artist a)
  (tag-data 'cd-artist (as-string a)))

(define (cd-title t)
  (tag-data 'cd-title (as-string t)))

(define (cd-playing-time min sec)
  (tag-data 'cd-playing-time 
    (string-append (as-string min) ":"
    (as-string sec))))


;; Present a cd entry
(define (cd-entry . subentries-0)
 (let ((subentries (flatten subentries-0))) ; to form a property list
  (let ((number				; @a
         (defaulted-get-prop 'cd-number subentries "?"))
	(artist 
         (defaulted-get-prop 'cd-artist subentries "No artist"))
	(title
         (defaulted-get-prop 'cd-title subentries "No title"))
	(play-time 
         (defaulted-get-prop 'cd-playing-time subentries 
	   "Unknown playing time"))
	)
    (table 'border "2"			; @b
	   (tr (tdb "Number") (td number))
	   (tr (tdb "Artist") (td artist))
	   (tr (tdb "Title") (td title))
	   (tr (tdb "Playing time") (td play-time))
	   ))))

;; Present a cd archive
(define (cd-archive . cd-entries)
  (html
   (head (title page-title))

   (body
     (h1 page-title)         ; @d
     
     (map spacy cd-entries)  ; @e
   )
  )
)



;;; Other definitions:

; Tag string-data with tag 
(define (tag-data tag string-data)
  (list (as-symbol tag) string-data))

(define (spacy x) (list x (p)))

(define (tdb x) (td (b x)))
