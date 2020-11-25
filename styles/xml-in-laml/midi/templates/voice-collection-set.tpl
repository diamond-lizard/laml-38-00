(load (string-append laml-dir "laml.scm"))
(laml-style "xml-in-laml/midi/midi")
(load (string-append laml-dir "styles/xml-in-laml/midi/midi-laml-processing-lib.scm"))
(define ppqn 480)

(StandardMidiFile
   (MidiHeader 'format "0" 'numberOfTracks "1" 'pulsesPerQuarterNote ppqn 'mode "deltaTime" 'counterTransposition "0")
   (MidiTrack
      VOICES
      (Meta 'deltaTime (* 8 ppqn) 'type "47" "")
   )
)

