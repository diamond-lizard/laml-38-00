(load (string-append laml-dir "laml.scm"))
(laml-style "xml-in-laml/midi/midi")
(load (string-append laml-dir "styles/xml-in-laml/midi/midi-laml-processing-lib.scm"))

(StandardMidiFile
   (MidiHeader 'format "0" 'numberOfTracks "1" 'pulsesPerQuarterNote "480" 'mode "deltaTime")
   (MidiTrack
     (Meta 'deltaTime "0"  'type "81" "09 27 C0")


     (Meta 'deltaTime "1920" 'type "47" "")
   )
)
