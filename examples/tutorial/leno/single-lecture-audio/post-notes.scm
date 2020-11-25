; This file will be read after the LENO software is loaded.
; Here you can redefine a number of LENO variables.

(define laml-tutorial-dir (in-startup-directory "../../"))

(define (in-tutorial-dir suffix)
  (string-append 
    laml-tutorial-dir suffix))
