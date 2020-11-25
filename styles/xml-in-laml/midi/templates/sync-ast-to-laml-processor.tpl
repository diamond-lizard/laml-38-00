(load (string-append laml-dir "laml.scm"))
(load (string-append laml-dir "styles/xml-in-laml/midi/midi.scm"))
(load (string-append laml-dir "styles/xml-in-laml/midi/midi-laml-processing-lib.scm"))

(let* ((ast-file-full-path "FULL-PATH-TO-AST-FILE")
       (laml-file-full-path "FULL-PATH-TO-LAML-FILE")
      )

  (let ((read-ast (file-read ast-file-full-path))) 
    ; Do necessary transformation (stable-sorting, info regeneration, ....) here
    (make-midi-laml-file read-ast laml-file-full-path))

)

(end-laml)