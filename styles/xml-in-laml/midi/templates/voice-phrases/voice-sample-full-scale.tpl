(load (string-append laml-dir "laml.scm"))
(laml-style "xml-in-laml/midi/midi")
(load (string-append laml-dir "styles/xml-in-laml/midi/midi-laml-processing-lib.scm"))
(define ppqn 480)
(define ch 16)

(StandardMidiFile
   (MidiHeader 'format "0" 'numberOfTracks "1" 'pulsesPerQuarterNote ppqn 'mode "deltaTime")
   (MidiTrack
      (voice ch *MSB* *LSB* *PC*)
      (map
        (lambda (n)
          (NoteOn 'deltaTime (/ ppqn 2) 'channel ch 'note n 'velocity "100" 'duration (/ ppqn 2))
        )
        (number-interval 0 127)
      )
      (Meta 'deltaTime ppqn 'type "47" "")   ; End of track
   )
)

