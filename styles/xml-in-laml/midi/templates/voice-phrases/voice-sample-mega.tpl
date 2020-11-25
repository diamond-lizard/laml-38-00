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
        (lambda (vel)
         (list
          (NoteOn 'deltaTime (/ ppqn 8) 'channel ch 'note 60 'velocity  vel 'duration (/ ppqn 2))
          (NoteOn 'deltaTime (/ ppqn 8) 'channel ch 'note 64 'velocity  vel 'duration (/ ppqn 2))
          (NoteOn 'deltaTime (/ ppqn 8) 'channel ch 'note 67 'velocity  vel 'duration (/ ppqn 2))
          (midi-comment-delta-time (* (/ ppqn 8) 5) "Pause rest of this beat")
         )          
        )
        (list 19 39 59 74 89 104 119 126)
      )
      (Meta 'deltaTime ppqn 'type "47" "")   ; End of track
   )
)

;;; 828