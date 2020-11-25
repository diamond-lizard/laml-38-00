(define (get-baseline project-name n)
  (list
    (td (read-text-file (in-startup-directory project-name "/" "baseline" "-" (as-string n) "-" "explanation")))
    (td (a 'href (string-append project-name "/" "baseline" "-" (as-string n) "." "midl") 'type "text/plain" "midl source file"))
    (td (a 'href (string-append project-name "-" "midi" "/" "baseline" "-" (as-string n) "." "mid")  "midi file"))))

; Take midi-file from midi-temp
(define (get-baseline-mt project-name n)
  (list
    (td (read-text-file (in-startup-directory project-name "/" "baseline" "-" (as-string n) "-" "explanation")))
    (td (a 'href (string-append project-name "/" "baseline" "-" (as-string n) "." "midl") 'type "text/plain" "midl source file"))
    (td (a 'href (string-append "midi-temp/" (as-string project-name) "." "mid")  "midi file"))))

(define (a-self str)
  (a 'href str str))

