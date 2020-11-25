(load (string-append laml-dir "laml.scm"))
(laml-style "xml-in-laml/midi/midi")
(load (string-append laml-dir "styles/xml-in-laml/midi/midi-laml-processing-lib.scm"))

(TyrosMultiPad
   'mode "deltaTime" 'image "S910"
   'name-1   "Bank name 1"   'repeat-1   "false"   'chord-match-1   "false"   'image-1   "S910"
   'name-2   "Bank name 2"   'repeat-2   "false"   'chord-match-2   "false"   'image-2   "S910"
   'name-3   "Bank name 3"   'repeat-3   "false"   'chord-match-3   "false"   'image-3   "S910"
   'name-4   "Bank name 4"   'repeat-4   "false"   'chord-match-4   "false"   'image-4   "S910"
   (MidiTrack
      (Meta 'deltaTime "0" 'type "47" ""))
   (MidiTrack
      (Meta 'deltaTime "0" 'type "47" ""))
   (MidiTrack
      (Meta 'deltaTime "0" 'type "47" ""))
   (MidiTrack
      (Meta 'deltaTime "0" 'type "47" ""))
)

