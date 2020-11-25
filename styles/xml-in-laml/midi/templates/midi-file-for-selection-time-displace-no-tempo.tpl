(load (string-append laml-dir "laml.scm"))
(laml-style "xml-in-laml/midi/midi")
(load (string-append laml-dir "styles/xml-in-laml/midi/midi-laml-processing-lib.scm"))

(StandardMidiFile
   THE-HEADER
   (MidiTrack
      THE-CONTEXT

      (time-displace THE-TIME
       (filter-messages (lambda (mes) (not (Meta? mes 81)))  
        (select-channels THE-CHANNELS
          THE-SELECTION
        )
       )
      )

      (Meta 'deltaTime "100000" 'type "47" "")
   )
)

