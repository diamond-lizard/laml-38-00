;; This is the portion of the notes.scm file which must
;; be loaded after the lecture notes style has been loaded.

;; In the long run, all the material in notes.scm should go here.
;; But there are some technical difficulties which must be overcome before
;; it can happen.

; On which system have we generated LENO. The variable computer-system may be defined
; in laml.scm at the basic LAML level.
(define (generating-system)
  (con (text-choice "Genereret for: " "on the system ") (as-string computer-system)))


(set-language 'english)


