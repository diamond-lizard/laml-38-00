(load (string-append laml-dir "laml.scm"))
(laml-style "xml-in-laml/midi/midi")
(load (string-append laml-dir "styles/xml-in-laml/midi/midi-laml-processing-lib.scm"))

(StandardMidiFile
   THE-HEADER
   (MidiTrack
      THE-CONTEXT

      

      (clean-for-sectional-playing TIME-MODE 10 USE-VOICE-CONTEXT 
        (select-channels THE-CHANNELS
          (abs-merge
             (get-fixed-part-from-to-abstime THE-FIXED-PART-PATH THE-FIRST-ABS-TIME THE-LAST-ABS-TIME)
             THE-SELECTION
          )
        )
      )

      (end-sectional-playing (* 4 480))

   )
)

