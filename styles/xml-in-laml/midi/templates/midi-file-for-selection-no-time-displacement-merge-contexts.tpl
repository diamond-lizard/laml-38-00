(load (string-append laml-dir "laml.scm"))
(laml-style "xml-in-laml/midi/midi")
(load (string-append laml-dir "styles/xml-in-laml/midi/midi-laml-processing-lib.scm"))

(StandardMidiFile
   THE-HEADER
   (MidiTrack

      (delta-merge
         (list THE-FIRST-CONTEXT)
         (select-channels THE-CHANNELS
           THE-SECOND-CONTEXT
           THE-SELECTION
         )
      )

      (Meta 'deltaTime "100000" 'type "47" "")
   )
)

