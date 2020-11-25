(load (string-append laml-dir "laml.scm"))
(laml-style "xml-in-laml/midi/midi")
(load (string-append laml-dir "styles/xml-in-laml/midi/midi-laml-processing-lib.scm"))

(TyrosStyle 'mode "absTime" 'image "S713" 'pulsesPerQuarterNote "1920"
   (MidiSection
      (StyleInit
         (Meta 'absTime "0" 'info "0:0:0 Time signature 4:4" 'type "88" "04 02 18 08")
         (Meta 'absTime "0" 'info "0:0:0 Tempo: 508474 microsec. per quarter note = 118 BPM. Setter: (tempo= BPM)" 'type "81" (tempo= 120))
         (Meta 'absTime "0" 'info "0:0:0 Copyright" 'type "2" "(C)Kurt Noermark."))
      (StyleId
         (Meta 'absTime "0" 'info "0:0:0 Sequence/track name" 'type "3" "StyleName")
      )

      (StyleSetup
      )

      (MainA 

      )

      (MainB
      
      )

      (MainC
     
      )

      (MainD 
      
      )

      (FillInAA
      
      )

      (FillInBB
  
      )

      (FillInCC
 
      )

      (FillInDD

      )

      (IntroA

      )

      (IntroB

      )

      (IntroC

      )

      (EndingA

      )

      (EndingB

      )

      (EndingC

      )

      (FillInBA

      )
   )

   (NonMidiSuffix  ; Joergen Soerensens default CASM section.  
                   ; This default CASM section uses "MIDI source channel = Keyboard output channel" -layout.
                   ; It also uses identical settings for all style parts. You can use this CASM for further editing.

     "43 41 53 4D 00 00 01 B8 43 53 45 47 00 00 01 B0 53 64 65 63 00 00 00 90 4D 61 69 6E 20 41 2C 4D 61 69 6E 20 42 2C 4D 61 69 6E 20 43 2C 4D 61 69 6E 20 44 2C 46 69 6C 6C 20 49 6E 20 41 41 2C 46 69 6C 6C 20 49 6E 20 42 42 2C 46 69 6C 6C 20 49 6E 20 43 43 2C 46 69 6C 6C 20 49 6E 20 44 44 2C 46 69 6C 6C 20 49 6E 20 41 42 2C 46 69 6C 6C 20 49 6E 20 42 41 2C 49 6E 74 72 6F 20 41 2C 49 6E 74 72 6F 20 42 2C 49 6E 74 72 6F 20 43 2C 45 6E 64 69 6E 67 20 41 2C 45 6E 64 69 6E 67 20 42 2C 45 6E 64 69 6E 67 20 43 43 74 61 62 00 00 00 1B 08 30 39 52 68 79 53 75 62 08 01 0F FF 07 FF FF FF FF 00 00 01 00 07 00 7F 01 00 43 74 61 62 00 00 00 1B 09 31 30 52 68 79 4D 61 69 09 01 0F FF 07 FF FF FF FF 00 00 01 00 07 00 7F 01 00 43 74 61 62 00 00 00 1B 0A 31 31 42 61 73 73 20 20 0A 01 0F FF 03 FF FF FF FF 00 00 00 01 03 1C 7F 04 00 43 74 61 62 00 00 00 1B 0B 31 32 43 68 6F 72 64 31 0B 01 0F FF 03 FF FF FF FF 00 02 00 02 07 00 7F 01 00 43 74 61 62 00 00 00 1B 0C 31 33 43 68 6F 72 64 32 0C 01 0F FF 03 FF FF FF FF 00 02 00 02 07 00 7F 01 00 43 74 61 62 00 00 00 1B 0D 31 34 50 61 64 20 20 20 0D 01 0F FF 03 FF FF FF FF 00 02 00 01 07 00 7F 01 00 43 74 61 62 00 00 00 1B 0E 31 35 50 68 72 61 73 31 0E 01 0F FF 03 FF FF FF FF 00 02 00 01 07 00 7F 01 00 43 74 61 62 00 00 00 1B 0F 31 36 50 68 72 61 73 32 0F 01 0F FF 03 FF FF FF FF 00 02 00 01 07 00 7F 01 00")

)
