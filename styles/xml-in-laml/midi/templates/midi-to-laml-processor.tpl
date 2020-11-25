(load (string-append laml-dir "laml.scm"))
(load (string-append laml-dir "styles/xml-in-laml/midi/midi.scm"))
(load (string-append laml-dir "styles/xml-in-laml/midi/midi-laml-processing-lib.scm"))

(let* ((midi-file-full-path "FULL-PATH-TO-MIDI-FILE")
       (processing-mode "THE-PROCESSING-MODE")
       (counter-transposition "THE-COUNTER-TRANSPOSITION")
       (track-separation "THE-TRACK-SEPARATION")
       (midi-file-name-initial-path (file-name-initial-path midi-file-full-path))
       (midi-file-name-proper (file-name-proper midi-file-full-path)))
  (midi-file-to-laml midi-file-full-path 
                     (string-append midi-file-name-initial-path 
                                    midi-file-name-proper ".midl")
                     (as-symbol processing-mode)
                     (as-number counter-transposition)
                     (as-boolean track-separation) 
  )
)

(end-laml)