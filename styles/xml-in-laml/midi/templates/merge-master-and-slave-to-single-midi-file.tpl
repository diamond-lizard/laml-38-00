; Merge a master and a slave midi files to a single midi file.
; Assumes that the three file path strings are substituted before execution.

(load (string-append laml-dir "laml.scm"))
(laml-style "xml-in-laml/midi/midi")
(load (string-append laml-dir "styles/xml-in-laml/midi/midi-laml-processing-lib.scm"))

(let* ((master-ast (midi-file-to-laml-ast "FILE-PATH-1" 'absTime 0 #f))
       (slave-ast (midi-file-to-laml-ast "FILE-PATH-2" 'absTime 0 #f))
       
       (master-header (ast-subtree master-ast "MidiHeader"))
       (master-events (ast-subtrees (ast-subtree master-ast "MidiTrack")))
       (slave-events  (filter channel-message? 
                                 (ast-subtrees (ast-subtree slave-ast "MidiTrack"))))
      )

  (write-text-file
   (standard-midi-file-ast-to-bin
    (StandardMidiFile 'internal:run-action-procedure "false"
       master-header
       (MidiTrack
          (abs-merge-two-lists master-events slave-events))))
    "FILE-PATH-3"))
  
  

