; GNU Emacs support for MIDI LAML processing.
; Kurt Normark, Aalborg University (c), 2010.
; Email: normark@cs.aau.dk.

(provide 'laml-midi-support)

; ---------------------------------------------------------------------------------------------------------------
; CONFIGURATION PART.
; Reassign variables - with (setq ...) -  in your .emacs file just AFTER loading midi.el.
; The values of the variables below serve as examples and documentation 
; of file and directory paths. You MUST set them to values that make sense in your setup.

(defvar div-midi-utilities-dir "c:/programs/midi-utilities/bin/" "Full path to directory holding DIV MIDI command line MIDI tools. Important for MIDI LAML.")
(defvar cygwin-shell "C:/cygwin/bin/sh.exe" "The file full path to an appropriate shell program in cygwin. Important for MIDI LAML.")

(defvar recording-dir "c:/users/kurt/normark/Media/midi/recording/" "Full path to a recording dir. Arbitrary, existing directory.")
(defvar step-recording-dir "c:/users/kurt/normark/Media/midi/step-recording/" "Full path to a step recording dir. Arbitrary, existing directory.")
(defvar midl-project-dir "c:/users/kurt/Media/midi-laml-projects/" "Full path to the Midi LAML Project directory. Full path to an existing directory meant for collections of MIDI LAML files.")

(defvar winamp-player-exc "c:/program files/Winamp/winamp.exe" "The full path to your interactive midi player - preferable winamp. Not necessary, but nice to have around.")
(defvar abc-dir "c:/programs/abcmidi/" "The directory that holds abc2midi.exe and midi2abc.exe")
(defvar abc-ps-dir "c:/Programs/abcps/abcm2ps-5.9.13/" "The directory that holds abcm2ps.exe")
(defvar gs-dir "c:/Program Files (x86)/Ghostgum/gsview/" "The directory that hold gsview32.exe, or similar" )

(defvar data-list-dir (concat laml-dir "styles/xml-in-laml/midi/data/") "The directory in which the voice list files are found")

; END CONFIGURATION PART
; ---------------------------------------------------------------------------------------------------------------


; Functions, instead of variables, to provide for late binding (assignment of variables in .emacs file).
(defun abc2midi-exc() (concat abc-dir "abc2midi.exe"))   ; The full path to abc2midi.exe
(defun midi2abc-exc() (concat abc-dir "midi2abc.exe"))   ; The full path to midi2abc.exe
(defun abcm2ps-exc()  (concat abc-ps-dir "abcm2ps.exe")) ; The full path to abcm2ps.exe
(defun gsview-exc()   (concat gs-dir "gsview32.exe"))    ; The full path to ghostview (32 bit version).


; More specialized configuration (KN relevant - Others: Ignore these).
; Tentative instrument naming - should be assigned finally in .emacs file after loading of this file.
(defvar pc-sound-card-string-id "A: SB Live" "The prefix string that identifies the PC sound card midi output port. Revealed by M-x show-midi-out-ports") 
(defvar tyros-string-id "TYROS-1" "The prefix string that identifies the Yamaha Tyros output port. Revealed by M-x show-midi-out-ports") 
(defvar motif-xs-8-string-id "Yamaha MOTIF XS8-1" "The prefix string that identifies Yamaha Motif XS-8 midi output port. Revealed by M-x show-midi-out-ports") 

(defvar motif-host-name "Motif_xs-26146d" "The host name of the Motif keyboard, as taken from the Utility Network screen image. Only relevant to Kurt Normark.")

(defvar midi-path-on-motif-xs "//Motif_xs-25cb60/media/usb1-1/Midi/" 
   "The full path to the MIDI directory on a drive connected to the Motif XS. Instrument specific. Not important for general use of MIDI LAML.")
(defvar midi-illustrator-exc "c:/Program Files/Rallentando Software/MidiIllustrator 2/MI.exe" 
    "The full path to a midi illustrator - score presentation program. Commercial, NON-free software.")

; ---------------------------------------------------------------------------------------------------------------

; GLOBAL VARIABLES 

(defvar midi-player 'command-line-player "Determines which midi player to use for everything but single-note playing. One of winamp or command-line-player. A symbol.")

(defvar midi-out-port 0 "A port number - the midi out port number. A non-negative integer.")
(defvar midi-in-port 0 "A port number - the midi in port number. A non-negative integer.")

(defvar midi-software-dir (concat laml-dir "styles/xml-in-laml/midi/"))

; File to be put on find-file-hook.
(defun find-midl-file-actions ()
  "Establish consistent midl stack if the major mode is midi-laml-mode"
  (if (and (boundp 'major-mode) (eq major-mode 'midi-laml-mode))
      (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack)))
)

(setq find-file-hook (cons 'find-midl-file-actions find-file-hook))


(defvar path-to-midi-error-file (concat laml-dir "styles/xml-in-laml/midi/" "midi/error.mid")
        "A path to a midi error file, which is played if no mirror file can be generated")  


(defvar laml-temp-file-path (concat laml-dir "temp/") "The LAML temp directory. Assumed to be writable.")

(defvar show-single-note-midi-file-cache-in-menu nil
   "A boolean variale that controls if Single Note Caching options are presented in the menu.
This variale is independent of the variable use-single-note-midi-file-cache, which controls if single note chaching is in use.
In the current version of MIDI LAML, single note caching is not recommended (it represents an early attempt in the development of the system).")

; ---------------------------------------------------------------------------------------------------------------
; MIDI LAML MODE STUFF and key/menu bidings.
; The mode is backed up by the file extension "midl".

; Redefine if necessary - with defvar - before loading this file:
(defvar tyros-support nil "Is there specific support for Yamaha Tyros instruments")
(defvar motif-xs-support nil "Is there specific support for Yamaha Motif XS instruments")
(defvar midi-illustrator-support nil "Is there support for Midi Illustrator - A commercial MIDI sheet music notation program")


(define-derived-mode 
        midi-laml-mode laml-mode "MidiLaml" 
  "The MidiLaml mode is a specialization of Laml mode which supports programmed MIDI solutions
in a particular MIDI XML-in-LAML language.
The LAML MIDI processor is a part of the LAML distribution.
LAML means 'Lisp Abstracted Markup Language'. LAML makes HTML and XML languages available as 
Scheme functions."

  ; actions-here

  ; Should not really be local to the mode function. But it does not work if I move them outside. Will have to be researched...
  (make-variable-buffer-local 'local-midl-mode-info)
  (make-variable-buffer-local 'attribute-to-be-modified)
  (make-variable-buffer-local 'play-after-condition)
  (make-variable-buffer-local 'channels-to-play)
  (make-variable-buffer-local 'relative-tempo-percentage-to-play)
  (make-variable-buffer-local 'play-residual-from-fixed-part) 
  (make-variable-buffer-local 'shadow-overlay) 
  (make-variable-buffer-local 'kind-of-shadow) 
  (make-variable-buffer-local 'kind-of-shadow) 
  (make-variable-buffer-local 'number-of-current-shadow) 

  (show-midl-mode-status-string)

  (setq mode-line-format 
     '("%e"
       #("-" 0 1
         (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
       mode-line-mule-info mode-line-modified mode-line-frame-identification mode-line-buffer-identification
       #("   " 0 3
         (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
       mode-line-position
       (vc-mode vc-mode)
       #("  " 0 2
         (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
       mode-line-modes
       (which-func-mode
        ("" which-func-format
         #("--" 0 2
           (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))))
       (:eval (midl-mode-status-string))
       (global-mode-string
        (#("--" 0 2
           (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
         global-mode-string))
       #("-%-" 0 3
         (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))))

)

(setq auto-mode-alist 
      (cons '("\\.midl" . midi-laml-mode)
            auto-mode-alist))

(defun size-of-current-buffer ()
   (point-max))

; Midi LAML keymap:
(defvar midi-laml-mode-map (make-sparse-keymap "MidiLaml"))

(define-key midi-laml-mode-map [menu-bar midi]
    (cons "MIDI" (make-sparse-keymap "MIDI")))

(define-key midi-laml-mode-map [menu-bar midi-laml-stack]
    (cons "MIDL Stack" (make-sparse-keymap "MIDL Stack")))

(define-key midi-laml-mode-map [menu-bar midi-laml-comments]
    (cons "MIDL Comments" (make-sparse-keymap "MIDL Comments")))

(define-key midi-laml-mode-map [menu-bar midi-laml]
    (cons "MIDL" (make-sparse-keymap "MIDL")))

(define-key-after midi-laml-mode-map [menu-bar midi-laml-baselines]                         ; controlling the order of this entry in the menu bar with define-key-after
    (cons "MIDL Baselines" (make-sparse-keymap "MIDL Baselines")) 'midi-laml-stack)


; (laml-define-key midi-laml-mode-map "\C-i" 'goto-next-input-field)



; Contributions to the tools menu: Recording

(laml-define-key global-map [menu-bar tools menu-tools-sep-2]
    '("----"))

(laml-define-key  global-map [menu-bar tools dired-recordings]
      '("Show Recorded MIDI Files" . dired-recordings))

(laml-define-key  global-map [menu-bar tools stop-recording]
      '("Stop Recording" . stop-recording))

(laml-define-key  global-map [menu-bar tools record-from-midi-port]
      '("Record from MIDI in port" . record-from-midi-port))

(laml-define-key global-map [menu-bar tools menu-tools-sep-1]
    '("----"))


(laml-define-key  midi-laml-mode-map [menu-bar midi stop-midi-playing]
      '("Stop MIDI Playing" . stop-midi-playing))

(laml-define-key  midi-laml-mode-map [menu-bar midi stop-midi-processes]
      '("Stop MIDI Processes" . stop-midi-processes))

(laml-define-key midi-laml-mode-map [menu-bar midi menu-midi-sep-3]
    '("----"))


;(laml-define-key midi-laml-mode-map [menu-bar midi menu-midi-sep-1]
;    '("----"))

(laml-define-key  midi-laml-mode-map [menu-bar midi set-midi-input-port]
      '("Set MIDI input port" . set-midi-input-port))

(laml-define-key  midi-laml-mode-map [menu-bar midi set-midi-output-port]
      '("Set MIDI output port" . set-midi-output-port))

(laml-define-key midi-laml-mode-map [menu-bar midi menu-midi-sep-2]
    '("----"))

(laml-define-key  midi-laml-mode-map [menu-bar midi show-midi-in-ports]
      '("Show MIDI input ports" . show-midi-in-ports))

(laml-define-key  midi-laml-mode-map [menu-bar midi show-midi-out-ports]
      '("Show MIDI output ports" . show-midi-out-ports))

(laml-define-key midi-laml-mode-map [menu-bar midi menu-midi-sep-2]
    '("----"))


; Tools menu contributions:

(laml-define-key global-map [menu-bar tools midi-ports]
    (cons "MIDI Ports" (make-sparse-keymap "Midi Ports")))

(laml-define-key  global-map [menu-bar tools midi-ports set-midi-input-port]
      '("Set MIDI input port..." . set-midi-input-port))

(laml-define-key  global-map [menu-bar tools midi-ports set-midi-output-port]
      '("Set MIDI output port..." . set-midi-output-port))

(laml-define-key global-map [menu-bar tools midi-ports menu-tools-midi-sep]
    '("----"))

(laml-define-key  global-map [menu-bar tools midi-ports show-midi-in-ports]
      '("Show MIDI input ports" . show-midi-in-ports))

(laml-define-key  global-map [menu-bar tools midi-ports show-midi-out-ports]
      '("Show MIDI output ports" . show-midi-out-ports))

(laml-define-key global-map [menu-bar tools instrument]
    (cons "Instrument" (make-sparse-keymap "Instrument")))

(laml-define-key global-map [menu-bar tools set-midi-player]
    '("Set MIDI Player..." . set-midi-player))

(laml-define-key  global-map [menu-bar tools instrument instrument-info]
      '("Instrument Info" . instrument-info))

(laml-define-key  global-map [menu-bar tools instrument set-instrument]
      '("Set Instrument..." . set-instrument))

(laml-define-key global-map [menu-bar tools instrument menu-tools-midi-sep-30]
    '("----"))

(if tyros-support
    (laml-define-key  global-map [menu-bar tools instrument set-instrument-tyros]
      '("Set to Tyros" . set-instrument-tyros)))

(if motif-xs-support
    (laml-define-key  global-map [menu-bar tools instrument set-instrument-motif-xs]
      '("Set to Motif XS" . set-instrument-motif-xs)))

(laml-define-key  global-map [menu-bar tools instrument set-instrument-pc-sound-card]
      '("Set to PC Soundcard - General MIDI" . set-instrument-pc-sound-card))


(laml-define-key midi-laml-mode-map [menu-bar midi-laml help-menu]
    (cons "Help" (make-sparse-keymap "Help")))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml help-menu help-on-ghost-fragments]
      '("On Ghost Fragments" . (lambda () (interactive) (help-from-text-file "ghost-fragments.txt"))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml  help-menu help-on-function-keys]
      '("On Function Keys" . (lambda () (interactive) (help-from-text-file "function-keys.txt"))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml  help-menu help-on-markers]
      '("On Markers" . (lambda () (interactive) (help-from-text-file "marker-channel.txt"))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml  help-menu help-on-voices]
      '("On Voices" . (lambda () (interactive) (help-from-text-file "about-voices.txt"))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml  help-menu help-on-status-line]
      '("On MIDL buffer status line " . (lambda () (interactive) (help-from-text-file "about-midl-status-line.txt"))))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml browse-menu]
    (cons "Browse" (make-sparse-keymap "Browse")))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml check-midi-laml-setup]
    '("Check MIDL LAML Setup" . check-midi-laml-setup))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml browse-menu midi-laml-home-page]
      '("MIDI LAML Home Page" . (lambda () (interactive) (browse-url-default-browser "http://www.cs.aau.dk/~normark/midi-laml/"))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml browse-menu midi-laml-manual]
      '("MIDI LAML Manual" . (lambda () (interactive) (open-midi-html-documentation "midi-mirror.html"))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml browse-menu midi-laml-function-manual]
      '("MIDI Function Library Manual" . (lambda () (interactive) (open-midi-html-documentation "midi-laml-processing-lib.html"))))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml menu-midi-laml-sep-11]
    '("----"))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml insert-template]
    (cons "Insert template" (make-sparse-keymap "Insert template")))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml menu-midi-laml-sep-10]
    '("----"))


(laml-define-key  midi-laml-mode-map [menu-bar midi-laml insert-template tyros-multi-pad]
      '("Tyros multipad" . 
          (lambda () (interactive)
            (insert-file-contents (concat laml-dir "styles/xml-in-laml/midi/" "templates/" "tyros-multipad" "." "tpl")))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml insert-template tyros-style]
      '("Tyros style - absTime" . 
          (lambda () (interactive)
            (insert-file-contents (concat laml-dir "styles/xml-in-laml/midi/" "templates/" "tyros-style-abs-time" "." "tpl")))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml insert-template simple-format-1-16]
      '("Format 1 midi file - 16 tracks" . 
          (lambda () (interactive)
            (insert-file-contents (concat laml-dir "styles/xml-in-laml/midi/" "templates/" "simple-midi-format-1-16" "." "tpl")))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml insert-template simple-format-1-2]
      '("Format 1 midi file - 2 tracks" . 
          (lambda () (interactive)
            (insert-file-contents (concat laml-dir "styles/xml-in-laml/midi/" "templates/" "simple-midi-format-1-2" "." "tpl")))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml insert-template simple-format-0]
      '("Format 0 midi file" . 
          (lambda () (interactive)
            (insert-file-contents (concat laml-dir "styles/xml-in-laml/midi/" "templates/" "simple-midi-format-0" "." "tpl")))))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml settings]
    (cons "Settings" (make-sparse-keymap "Settings")))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml settings toggle-syntax-highlighting]
      '("Toggle syntax highlighting" . toggle-syntax-highlighting))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml settings toggle-playing-fixed-part]
      '("Toggle fixed part playing" . toggle-playing-fixed-part))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml settings set-use-of-end-note-anchoring]
      '("Set use of end note anchoring " . set-use-of-end-note-anchoring))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml settings set-use-of-voice-context]
      '("Set use of voice context" . set-use-of-voice-context))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml settings set-mode-of-playing-single-notes]
      '("Set mode of playing single notes" . set-mode-of-playing-single-notes))





(laml-define-key midi-laml-mode-map [menu-bar midi-laml shadow]
    (cons "Shadow" (make-sparse-keymap "Shadow")))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml shadow message-about-current-shadow]
      '("Info about current shadow" . message-about-current-shadow))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml shadow menu-midi-laml-sep-1]
    '("----"))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml shadow set-kind-of-shadow-none]
      '("No shadow" . set-kind-of-shadow-none))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml shadow set-kind-of-shadow-until-end-of-current-bar]
      '("Until end of current bar" . set-kind-of-shadow-until-end-of-current-bar))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml shadow set-kind-of-shadow-number-of-quarter-notes]
      '("A number of quarter notes..." . set-kind-of-shadow-number-of-quarter-notes))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml shadow set-kind-of-shadow-number-of-seconds]
      '("A number of seconds..." . set-kind-of-shadow-number-of-seconds))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml shadow set-kind-of-shadow-duration-of-current-event]
      '("Duration of current event" . set-kind-of-shadow-duration-of-current-event))




(laml-define-key midi-laml-mode-map [menu-bar midi-laml live-cursor]
    (cons "Live Cursor" (make-sparse-keymap "Live Cursor")))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml live-cursor info-live-cursor]
      '("About" . (lambda () (interactive) (help-from-text-file "live-cursor.txt"))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml live-cursor live-cursor-info]
      '("Info" . live-cursor-info))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml live-cursor menu-midi-laml-sep]
    '("----"))


(laml-define-key  midi-laml-mode-map [menu-bar midi-laml live-cursor faster-live-cursor]
      '("Faster" . faster-live-cursor))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml live-cursor slower-live-cursor]
      '("Slower" . slower-live-cursor))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml live-cursor live-cursor-auto-set-startup-latency]
      '("Auto Set Startup Latency" . live-cursor-auto-set-startup-latency))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml live-cursor live-cursor-set-startup-latency]
      '("Set Startup Latency..." . live-cursor-set-startup-latency))


(if tyros-support
    (laml-define-key  midi-laml-mode-map [menu-bar midi-laml format-attributes-of-multi-pad]
      '("Format attributes of multipad form" . format-attributes-of-multi-pad)))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml menu-midi-laml-sep-9]
    '("----"))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml insert-marker-midi-event]
      '("Insert marker..." . insert-marker-midi-event))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml make-marker-region-browser-new-marker-letter]
      '("Marker browser with letter..." . make-marker-region-browser-new-marker-letter))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml make-marker-region-browser]
      '("Marker browser" . make-marker-region-browser))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml menu-midi-laml-sep-12]
    '("----"))


(laml-define-key  midi-laml-mode-map [menu-bar midi-laml display-length-of-midi-list]
      '("Display length of selection" . display-length-of-midi-list))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml menu-midi-laml-sep-8]
    '("----"))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml reset-navigation-predicate]
      (cons "Reset navigation predicate" (function reset-navigation-predicate)))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml define-navigation-predicate]
      (cons "Define navigation predicate..." (function define-navigation-predicate)))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml goto-abs-time]
      '("Goto time..." . goto-abs-time))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml goto-bar]
      '("Goto bar..." . goto-bar))



(laml-define-key midi-laml-mode-map [menu-bar midi-laml menu-midi-laml-sep-7]
    '("----"))

(if show-single-note-midi-file-cache-in-menu
 (progn
  (laml-define-key midi-laml-mode-map [menu-bar midi-laml single-note-cache]
    (cons "Single note cache" (make-sparse-keymap "Single note cache")))

  (laml-define-key  midi-laml-mode-map [menu-bar midi-laml single-note-cache delete-single-noteon-cache]
      '("Delete cache" . delete-single-noteon-cache))

  (laml-define-key  midi-laml-mode-map [menu-bar midi-laml single-note-cache toggle-use-of-single-note-cache]
      '("Toggle use of cache" . toggle-use-of-single-note-cache))

  (laml-define-key  midi-laml-mode-map [menu-bar midi-laml single-note-cache single-note-cache-info]
      '("Info about cache" . single-note-cache-info))

  (laml-define-key midi-laml-mode-map [menu-bar midi-laml single-note-cache menu-midi-laml-sep-17]
    '("----"))

  (laml-define-key  midi-laml-mode-map [menu-bar midi-laml single-note-cache cache-selected-track]
      '("Cache by track" . cache-selected-track))

  (laml-define-key  midi-laml-mode-map [menu-bar midi-laml single-note-cache cache-selected-bars-in-laml-file]
      '("Cache by bars..." . cache-selected-bars-in-laml-file))

  (laml-define-key  midi-laml-mode-map [menu-bar midi-laml single-note-cache cache-selection-in-laml-file]
      '("Cache by selection" . cache-selection-in-laml-file))

  (laml-define-key midi-laml-mode-map [menu-bar midi-laml menu-midi-laml-sep-16]
    '("----"))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml outdent-channel]
      '("Outdent channel of track..." . outdent-channel))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml outdent-multiple-channels]
      '("Outdent multiple channels of track..." . outdent-multiple-channels))

 (laml-define-key midi-laml-mode-map [menu-bar midi-laml menu-midi-laml-sep-6]
    '("----"))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml rename-midl-project]
      '("Rename MIDL project..." . rename-midl-project))



(laml-define-key midi-laml-mode-map [menu-bar midi-laml-comments lower-c]
    (cons "Lower C" (make-sparse-keymap "Lower C")))



; (laml-define-key midi-laml-mode-map [menu-bar midi-laml score-comment]
;    (cons "Score Comments" (make-sparse-keymap "Score Comments")))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml-comments lower-c set-c]
    (cons "Set lower C" (make-sparse-keymap "Set lower C for score comments")))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments lower-c report-lower-c]
      '("What is lower C" . report-lower-c))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments lower-c set-c set-c-to-c7]
      '("Set lower C to C7 (96)" . (lambda () (interactive) (set-c-value 96))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments lower-c set-c set-c-to-c6]
      '("Set lower C to C6 (84)" . (lambda () (interactive) (set-c-value 84))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments lower-c set-c set-c-to-c5]
      '("Set lower C to C5 (72)" . (lambda () (interactive) (set-c-value 72))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments lower-c set-c set-c-to-c4]
      '("Set lower C to C4 (60)" . (lambda () (interactive) (set-c-value 60))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments lower-c set-c set-c-to-c3]
      '("Set lower C to C3 (48)" . (lambda () (interactive) (set-c-value 48))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments lower-c set-c set-c-to-c2]
      '("Set lower C to C2 (36)" . (lambda () (interactive) (set-c-value 36))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments set-c set-c-to-c1]
      '("Set lower C to C1 (24)" . (lambda () (interactive) (set-c-value 24))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments set-c set-c-to-c0]
      '("Set lower C to C0 (12)" . (lambda () (interactive) (set-c-value 12))))


;(laml-define-key midi-laml-mode-map [menu-bar midi-laml-comments menu-midi-laml-sep-3]
;    '("----"))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml-comments remove-score-comments-menu]
    (cons "Remove Score Comments" (make-sparse-keymap "Remove Score Comments")))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments remove-score-comments-menu remove-score-comments-from-bars]
      '("From bars..." . remove-score-comments-from-bars))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments remove-score-comments-menu remove-score-comments-from-region]
      '("From region" . remove-score-comments-from-region))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments remove-score-comments-menu remove-score-comments-from-form]
      '("From form" . remove-score-comments-from-form))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml-comments add-score-comments-menu]
    (cons "Add Score Comments" (make-sparse-keymap "Add Score Comments")))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments add-score-comments-menu add-score-comments-to-bars-of-channel]
      '("To to bars of channel..." . add-score-comments-to-bars-of-channel))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments add-score-comments-menu add-score-comments-to-bars]
      '("To bars..." . add-score-comments-to-bars))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments add-score-comments-menu add-score-comments-to-region-of-channel]
      '("To region of channel..." . add-score-comments-to-region-of-channel))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments add-score-comments-menu add-score-comments-to-region]
      '("To region" . add-score-comments-to-region))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments add-score-comments-menu add-score-comments-to-form-of-channel]
      '("To form of channel..." . add-score-comments-to-form-of-channel))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments add-score-comments-menu add-score-comments-to-form]
      '("To form" . add-score-comments-to-form))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml-comments set-kind-of-midl-score]
    '("Set kind of score comment..." . set-kind-of-midl-score))




(laml-define-key midi-laml-mode-map [menu-bar midi-laml-comments menu-midi-laml-sep-2]
    '("----"))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments add-comment-to-midl-file]
      '("Add midl file comment..." . add-comment-to-midl-file))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments display-comment-of-midl-file]
      '("Show midl file comment" . display-comment-of-midl-file))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml-comments menu-midi-laml-sep-4]
    '("----"))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments remove-bar-comments-from-current-track]
      '("Remove bar comments from current track" . remove-bar-comments-from-current-track))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-comments add-bar-comments-to-current-track]
      '("Add bar comments to current track" . add-bar-comments-to-current-track))





(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack help-on-midl-stack]
      '("Help on Midl Stack" . (lambda () (interactive) (help-from-text-file "midl-stack.txt"))))

; (laml-define-key midi-laml-mode-map [menu-bar midi-laml midl-stack]
;    (cons "Midl Stack" (make-sparse-keymap "Midl Stack")))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack midl-source-file-stack-info]
      '("Info about midl stack" . midl-source-file-stack-info))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml-stack menu-midi-laml-sep-14]
    '("----"))

; (laml-define-key midi-laml-mode-map [menu-bar midi-laml baseline]
;    (cons "Baseline" (make-sparse-keymap "Baseline")))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack help-on-ghost-fragments]
      '("Help on ghost fragments" . (lambda () (interactive) (help-from-text-file "ghost-fragments.txt"))))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack reset-ghost-fragments]
      '("Reset ghost fragments" . reset-ghost-fragments))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack add-project-wide-ghost-fragments-of-midl-stack]
      '("Add project-wide ghost fragments and browse" . add-project-wide-ghost-fragments-of-midl-stack))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack add-all-ghost-fragments-of-midl-stack]
      '("Add all ghost fragments and browse" . add-all-ghost-fragments-of-midl-stack))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack browse-existing-ghost-fragments]
      '("Browse existing ghost fragments" . browse-existing-ghost-fragments))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml-stack menu-midi-laml-sep-18]
    '("----"))


(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack get-rid-of-ghost-elements]
      '("Get rid of ghost elements" . get-rid-of-ghost-elements))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack reset-and-empty-midl-stack]
      '("Clear All" . reset-and-empty-midl-stack))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack reset-midl-stack]
      '("Reset midl stack..." . reset-midl-stack))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml-stack menu-midi-laml-sep-15]
    '("----"))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack undo-all-pops-of-midl-stack]
      '("To top of stack" . undo-all-pops-of-midl-stack))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack do-all-pops-of-midl-stack]
      '("To bottom of stack" . do-all-pops-of-midl-stack))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack undo-pop-midl-stack]
      '("Undo pop" . undo-pop-midl-stack))


; Done implicitly, when necessary:
; (laml-define-key  midi-laml-mode-map [menu-bar midi-laml recover-midl-source-file-stack]
;       '("Recover midl stack" . recover-midl-source-file-stack))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack merge-two-top-stack-entries]
      '("Pop splitted entries, merge and push result" . merge-two-top-stack-entries))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack pop-and-reestablish-midl-file]
      '("Pop and reestablish source" . pop-and-reestablish-midl-file))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml-stack menu-midi-laml-sep-13]
    '("----"))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml-stack push]
    (cons "Push" (make-sparse-keymap "Push")))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack push push-latest-temporary-selection-on-midl-stack]
      '("Latest temp selection" . push-latest-temporary-selection-on-midl-stack))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack push push-baseline-on-midl-stack]
      '("Baseline file on Midl stack - undo all pops..." . push-baseline-on-midl-stack))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack push push-external-usb-midi-file-on-midl-stack]
      '("MIDI file from USB stick..." . push-external-usb-midi-file-on-midl-stack))

(if motif-xs-support
    (laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack push push-external-motif-xs-midi-file-on-midl-stack]
      '("MIDI file from Motif XS..." . push-external-motif-xs-midi-file-on-midl-stack)))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack push push-external-midi-file-on-midl-stack]
      '("External MIDI file..." . push-external-midi-file-on-midl-stack))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack push push-external-midl-file-on-midl-stack]
      '("External Midl file..." . push-external-midl-file-on-midl-stack))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack push replicate-current-buffer-and-undo-all-pops]
      '("Replication of current buffer - undo all pops" . replicate-current-buffer-and-undo-all-pops))



(laml-define-key midi-laml-mode-map [menu-bar midi-laml-stack process-and-push]
    (cons "Process and Push" (make-sparse-keymap "Process and Push")))

; (laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack process-and-push split-and-replicate-midl-file-by-channel]
;      '("Splits by channel..." . split-and-replicate-midl-file-by-channel))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack process-and-push process-laml-file-save-and-push-with-counter-transposition]
      '("Counter transposition..." . process-laml-file-save-and-push-with-counter-transposition))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack process-and-push process-laml-file-save-and-push-toggle-processing-mode]
      '("New top mode" . process-laml-file-save-and-push-toggle-processing-mode))

;(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack process-and-push process-laml-file-save-and-push-via-ast]
;      '("Via AST" . process-laml-file-save-and-push-via-ast))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack process-and-push process-laml-file-all-mes-save-unzoom-and-zoom-on-channel]
      '("Zoom on channel - after total unzoom..." . process-laml-file-all-mes-save-unzoom-and-zoom-on-channel))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack process-and-push process-laml-file-save-unzoom-and-zoom-on-channel]
      '("Zoom on notes in channel - after total unzoom..." . process-laml-file-save-unzoom-and-zoom-on-channel))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack process-and-push process-laml-file-save-and-push-unzoom]
      '("Unzoom" . process-laml-file-save-and-push-unzoom))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack process-and-push process-laml-file-save-zoom-and-push]
      '("Zoom with predicate..." . process-laml-file-save-zoom-and-push))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack process-and-push process-laml-file-save-and-push]
      '("Regular" . process-laml-file-save-and-push))




(laml-define-key midi-laml-mode-map [menu-bar midi-laml-stack menu-midi-laml-sep-22]
    '("----"))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-stack stack-browser]
      '("Browse MIDL stack" . stack-browser))


(laml-define-key midi-laml-mode-map [menu-bar midi-laml menu-midi-laml-sep-2]
    '("----"))

; (laml-define-key  midi-laml-mode-map [menu-bar midi-laml next-similar-midi-step]
;      '("Next similar midi step" . next-similar-midi-step))

; (laml-define-key  midi-laml-mode-map [menu-bar midi-laml midi-step]
;      '("Midi step" . midi-step))

; (laml-define-key  midi-laml-mode-map [menu-bar midi-laml process-selected-laml-file-and-play-again]
;      '("Process and Play selection again" . process-selected-laml-file-and-play-again))



(laml-define-key  midi-laml-mode-map [menu-bar midi-laml process-and-make-channel-stat]
      '("Channel stat browser" . process-and-make-channel-stat))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml make-voice-collection-from-midl-file]
      '("Voice collection browser " . make-voice-collection-from-midl-file))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml new-voice-collection]
      '("Empty voice collection browser" .  new-voice-collection))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml menu-midi-laml-sep-19]
    '("----"))

(laml-define-key midi-laml-mode-map [menu-bar midi-laml process-and-save-menu]
    (cons "Process and Save" (make-sparse-keymap "Process and Play")))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml process-and-save-menu process-laml-file-and-save-in-mid-dir-on-memory-stick]
      '("To USB stick file..." . process-laml-file-and-save-in-mid-dir-on-memory-stick))

(if motif-xs-support
    (laml-define-key  midi-laml-mode-map [menu-bar midi-laml process-and-save-menu process-laml-file-and-save-in-mid-dir-on-motif-xs]
      '("To Motif XS file..." . process-laml-file-and-save-in-mid-dir-on-motif-xs)))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml process-and-save-menu process-laml-file-and-save]
      '("To MIDI file ..." . process-laml-file-and-save))



(laml-define-key midi-laml-mode-map [menu-bar midi-laml process-and-illustrate-menu]
    (cons "Process and Illustrate" (make-sparse-keymap "Process and Illustrate")))

(if (and (file-exists-p abc-dir) (file-exists-p (abc2midi-exc)) (file-exists-p abc-ps-dir) (file-exists-p (abcm2ps-exc)))
  (laml-define-key  midi-laml-mode-map [menu-bar midi-laml process-and-illustrate-menu process-laml-file-and-illustrate-via-abc]
      '("Via ABC - few notes only" . process-laml-file-and-illustrate-via-abc)))

(if midi-illustrator-support
   (laml-define-key  midi-laml-mode-map [menu-bar midi-laml process-and-illustrate-menu process-selected-laml-file-certain-channels-and-illustrate]
      '("Selection of channels..." . process-selected-laml-file-certain-channels-and-illustrate)))

(if midi-illustrator-support
   (laml-define-key  midi-laml-mode-map [menu-bar midi-laml process-and-illustrate-menu process-bars-laml-file-certain-channels-and-illustrate]
      '("Bars of channels..." . process-bars-laml-file-certain-channels-and-illustrate)))

(if midi-illustrator-support
    (laml-define-key  midi-laml-mode-map [menu-bar midi-laml process-and-illustrate-menu process-selected-track-and-illustrate]
      '("Track" . process-selected-track-and-illustrate)))

(if midi-illustrator-support
    (laml-define-key  midi-laml-mode-map [menu-bar midi-laml process-and-illustrate-menu process-laml-file-and-illustrate]
      '("Entire Midl File" . process-laml-file-and-illustrate)))



(laml-define-key midi-laml-mode-map [menu-bar midi-laml process-and-play-menu]
    (cons "Process and Play" (make-sparse-keymap "Process and Play")))

; (laml-define-key  midi-laml-mode-map [menu-bar midi-laml process-and-play-menu process-selected-laml-file-certain-channels-and-play]
;       '("Selection of channels..." . process-selected-laml-file-certain-channels-and-play))
; 
; (laml-define-key  midi-laml-mode-map [menu-bar midi-laml process-and-play-menu process-bars-laml-file-certain-channels-and-play]
;       '("Bars of channels..." . process-bars-laml-file-certain-channels-and-play))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml process-and-play-menu process-selected-laml-file-and-play]
      '("Selection" . process-selected-laml-file-and-play))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml process-and-play-menu process-selected-bars-laml-file-and-play]
      '("Bars..." . process-selected-bars-laml-file-and-play))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml process-and-play-menu process-selected-track-and-play]
      '("Track" . process-selected-track-and-play))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml process-and-play-menu process-laml-file-and-play]
      '("Midl File" . process-laml-file-and-play))



(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-baselines baseline-present-info]
      '("Info about baselines" . baseline-present-info))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-baselines create-baseline-of-current-midl-source-file]
      '("Create baseline file..." . create-baseline-of-current-midl-source-file))

(laml-define-key  midi-laml-mode-map [menu-bar midi-laml-baselines browse-baselines]
      '("Browse baselines" . browse-baselines))



; Keyboard bindings - traditional

(laml-define-key midi-laml-mode-map (list "\C-o" "\C-c\C-o")     'process-laml-file-and-play)
(laml-define-key midi-laml-mode-map (list "\C-c\C-c" "\C-c\C-c") 'process-selection-or-shadow-and-play)
(laml-define-key midi-laml-mode-map (list "\C-cB" "\C-cB") 'process-selected-bars-laml-file-and-play)
(laml-define-key midi-laml-mode-map (list "\C-c\C-t" "\C-c\C-t") 'process-selected-track-and-play)
(laml-define-key midi-laml-mode-map (list "\C-c\C-k" "\C-c\C-k") 'process-selected-laml-file-and-play-again)
; (laml-define-key midi-laml-mode-map (list "\C-c\C-m" "\C-c\C-m") 'midi-step)

(laml-define-key midi-laml-mode-map (list "\C-c\C-u" "\C-c\C-u") 'next-similar-midi-step)
(laml-define-key midi-laml-mode-map (list "\C-c\C-s" "\C-c\C-s") 'process-laml-file-and-save)
(laml-define-key midi-laml-mode-map (list "\C-c\C-s" "\C-c\C-s") 'process-laml-file-and-save)
(laml-define-key midi-laml-mode-map (list "\C-cS" "\C-cS")       'process-laml-file-and-save-in-mid-dir-on-memory-stick)
(laml-define-key midi-laml-mode-map (list "\C-c\C-l" "\C-c\C-l") 'process-laml-file-and-illustrate)

(laml-define-key midi-laml-mode-map (list "\C-c\C-p" "\C-c\C-p") 'process-laml-file-save-and-push)
(laml-define-key midi-laml-mode-map (list "\C-cp" "\C-cp")       'process-laml-file-save-and-push-toggle-processing-mode)
(laml-define-key midi-laml-mode-map (list "\C-cz" "\C-cz")       'process-laml-file-save-zoom-and-push)
(laml-define-key midi-laml-mode-map (list "\C-cq" "\C-cq")       'process-laml-file-save-and-push-unzoom)
(laml-define-key midi-laml-mode-map (list "\C-ch" "\C-ch")       'process-laml-file-save-unzoom-and-zoom-on-channel)
(laml-define-key midi-laml-mode-map (list "\C-cH" "\C-cH")       'process-laml-file-all-mes-save-unzoom-and-zoom-on-channel)
(laml-define-key midi-laml-mode-map (list "\C-cP" "\C-cP")       'process-laml-file-save-and-push-with-counter-transposition)
;(laml-define-key midi-laml-mode-map (list "\C-cs" "\C-cs")       'split-and-replicate-midl-file-by-channel)
(laml-define-key midi-laml-mode-map (list "\C-c\C-o" "\C-c\C-o") 'pop-and-reestablish-midl-file)
(laml-define-key midi-laml-mode-map (list "\C-c\C-y" "\C-c\C-y") 'undo-pop-midl-stack)
(laml-define-key midi-laml-mode-map (list "\C-c\y" "\C-c\y")     'undo-all-pops-of-midl-stack)
(laml-define-key midi-laml-mode-map (list "\C-c\o" "\C-c\o")     'do-all-pops-of-midl-stack)
(laml-define-key midi-laml-mode-map (list "\C-c\C-i" "\C-c\C-i") 'midl-source-file-stack-info)
(laml-define-key midi-laml-mode-map (list "\C-ct" "\C-ct")       'push-latest-temporary-selection-on-midl-stack)
(laml-define-key midi-laml-mode-map (list "\C-cL" "\C-cL")       'push-external-midl-file-on-midl-stack)
(laml-define-key midi-laml-mode-map (list "\C-cM" "\C-cM")       'push-external-midi-file-on-midl-stack)
(laml-define-key midi-laml-mode-map (list "\C-cR" "\C-cR")       'replicate-current-buffer-and-undo-all-pops)
(laml-define-key midi-laml-mode-map (list "\C-cG" "\C-cG")       'get-rid-of-ghost-elements)
(laml-define-key midi-laml-mode-map (list "\C-cu" "\C-cu")       'push-external-usb-midi-file-on-midl-stack)
(laml-define-key midi-laml-mode-map (list "\C-cx" "\C-cx")       'push-external-motif-xs-midi-file-on-midl-stack)
(laml-define-key midi-laml-mode-map (list "\C-c\C-w" "\C-c\C-w") 'display-comment-of-midl-file)
; (laml-define-key midi-laml-mode-map (list "\C-cA" "\C-cA")       'reset-midl-stack)
(laml-define-key midi-laml-mode-map (list "\C-c\C-a" "\C-c\C-a") 'add-comment-to-midl-file)

(laml-define-key midi-laml-mode-map (list "\C-c\C-b" "\C-c\C-b") 'create-baseline-of-current-midl-source-file)
(laml-define-key midi-laml-mode-map (list "\C-ci" "\C-ci")       'baseline-present-info)
(laml-define-key midi-laml-mode-map (list "\C-cb" "\C-cb")       'browse-baselines)
(laml-define-key midi-laml-mode-map (list "\C-ca" "\C-ca")       'push-baseline-on-midl-stack)

(laml-define-key midi-laml-mode-map (list "\C-cm" "\C-cm")       'make-marker-region-browser)
(laml-define-key midi-laml-mode-map (list "\C-c\C-m" "\C-c\C-m") 'make-marker-region-browser-new-marker-letter)
(laml-define-key midi-laml-mode-map (list "\C-cl" "\C-cl")       'display-length-of-midi-list)

(laml-define-key midi-laml-mode-map (list "\C-c\C-x" "\C-c\C-x") 'stop-midi-playing)
(laml-define-key midi-laml-mode-map (list "\C-c\C-g" "\C-c\C-g") 'goto-bar)  
(laml-define-key midi-laml-mode-map (list "\C-\M-m" "\C-\M-m")   'insert-marker-midi-event)  
(laml-define-key midi-laml-mode-map (list "\C-cg" "\C-cg")       'browse-existing-ghost-fragments)  

(laml-define-key midi-laml-mode-map (list "\C-cs" "\C-cs")       'stack-browser)
(laml-define-key midi-laml-mode-map (list "\C-cc" "\C-cc")       'process-and-make-channel-stat)
(laml-define-key midi-laml-mode-map (list "\C-cv" "\C-cv")       'make-voice-collection-from-midl-file)

; Playing with mouse events - learning:
; (define-key midi-laml-mode-map [down-mouse-1] (function (lambda (e) (interactive "e") (message "1111") (mouse-drag-region e))))
; (define-key midi-laml-mode-map [drag-mouse-1] (function (lambda (e) (interactive "e") (message "2222"))))


; Function keys: 

; (define-key midi-laml-mode-map [f4] 'keyboard-quit)

; F5-F8: States and toggles: 
(define-key  midi-laml-mode-map [f5] 'set-relative-tempo)   
(define-key  midi-laml-mode-map [C-f5] 'reset-relative-tempo)   
(define-key  midi-laml-mode-map [S-f5]   (function (lambda () (interactive) (add-to-relative-tempo -10))))   
(define-key  midi-laml-mode-map [C-M-f5] (function (lambda () (interactive) (add-to-relative-tempo 10))))   
(define-key  midi-laml-mode-map [M-f5] (function (lambda () (interactive) (add-to-relative-tempo 10))))   

(define-key  midi-laml-mode-map [f6] 'toggle-play-after-condition)
(define-key  midi-laml-mode-map [C-f6] 'play-after-condition)

(define-key  midi-laml-mode-map [f7] 'set-channels-to-play)
(define-key  midi-laml-mode-map [C-f7] 'set-channels-all)
(define-key  midi-laml-mode-map [M-f7] 'toggle-use-voice-context)

(define-key  midi-laml-mode-map [f8] 'toggle-fixed-tempo)

; (define-key  midi-laml-mode-map [f11]  (function (lambda () (interactive) (message "Use Enter"))))  ; f11 is not used - KP Enter is used instead.


; Wheel playing:
(define-key  midi-laml-mode-map [wheel-down] 'process-from-point-by-wheel)
(define-key  midi-laml-mode-map [left-fringe wheel-down] 'process-from-point-by-wheel)
(define-key  midi-laml-mode-map [M-wheel-down] 'next-midi-message-and-play)
(define-key  midi-laml-mode-map [left-fringe M-wheel-down] 'next-midi-message-and-play)
(define-key  midi-laml-mode-map [wheel-up] 'back-midi-playing-by-wheel)
(define-key  midi-laml-mode-map [left-fringe wheel-up] 'back-midi-playing-by-wheel)
(define-key  midi-laml-mode-map [M-wheel-up] 'previous-midi-message-and-play)
(define-key  midi-laml-mode-map [left-fringe M-wheel-up] 'previous-midi-message-and-play)

(define-key  midi-laml-mode-map [C-wheel-down] 'goto-next-bar-and-process-current-bar-and-play)
(define-key  midi-laml-mode-map [C-wheel-up] 'goto-previous-bar-and-process-current-bar-and-play)
(define-key  midi-laml-mode-map [left-fringe C-wheel-down] 'goto-next-bar-and-process-current-bar-and-play)
(define-key  midi-laml-mode-map [left-fringe C-wheel-up] 'goto-previous-bar-and-process-current-bar-and-play)
(define-key  midi-laml-mode-map [C-mouse-2] 'process-current-bar-and-play)
(define-key  midi-laml-mode-map [left-fringe C-mouse-2] 'process-current-bar-and-play)

(define-key  midi-laml-mode-map [S-wheel-down] 'goto-next-chord-process-and-play)
(define-key  midi-laml-mode-map [S-wheel-up] 'goto-previous-chord-process-and-play)
(define-key  midi-laml-mode-map [left-fringe S-wheel-down] 'goto-next-chord-process-and-play)
(define-key  midi-laml-mode-map [left-fringe S-wheel-up] 'goto-previous-chord-process-and-play)
(define-key  midi-laml-mode-map [S-mouse-2] 'process-current-chord-and-play)
(define-key  midi-laml-mode-map [left-fringe S-mouse-2] 'process-current-chord-and-play)

(define-key  midi-laml-mode-map [M-C-wheel-down] 'goto-next-marked-section-and-process-it-and-play)
(define-key  midi-laml-mode-map [M-C-wheel-up] 'goto-previous-marked-section-and-process-it-and-play)
(define-key  midi-laml-mode-map [left-fringe M-C-wheel-down] 'goto-next-marked-section-and-process-it-and-play)
(define-key  midi-laml-mode-map [left-fringe M-C-wheel-up] 'goto-previous-marked-section-and-process-it-and-play)
(define-key  midi-laml-mode-map [M-C-mouse-2] 'process-current-marked-region-and-play)
(define-key  midi-laml-mode-map [left-fringe M-C-mouse-2] 'process-current-marked-region-and-play)

(define-key  midi-laml-mode-map [S-C-wheel-down] 'process-from-current-point-to-end-and-play)
(define-key  midi-laml-mode-map [left-fringe S-C-wheel-down] 'process-current-marked-region-and-play)

(define-key  midi-laml-mode-map [mouse-2] 'play-single-noteon-maybe-cached)
(define-key  midi-laml-mode-map [left-fringe mouse-2] 'play-single-noteon-maybe-cached)

; End wheel 

; Up/Down/End - as alternatives to mouse wheel
; Closely connected to wheel bindings such that they can be used on machines without a mouse with a wheel.

(define-key  midi-laml-mode-map [down] 'process-from-point-by-wheel)
(define-key  midi-laml-mode-map [M-down] 'next-midi-message-and-play)
(define-key  midi-laml-mode-map [up] 'back-midi-playing-by-wheel)
(define-key  midi-laml-mode-map [M-up] 'previous-midi-message-and-play)
(define-key  midi-laml-mode-map [C-down] 'goto-next-bar-and-process-current-bar-and-play)
(define-key  midi-laml-mode-map [C-up] 'goto-previous-bar-and-process-current-bar-and-play)
(define-key  midi-laml-mode-map [C-end] 'process-current-bar-and-play)
(define-key  midi-laml-mode-map [S-down] 'goto-next-chord-process-and-play)
(define-key  midi-laml-mode-map [S-up] 'goto-previous-chord-process-and-play)
(define-key  midi-laml-mode-map [S-end] 'process-current-chord-and-play)
(define-key  midi-laml-mode-map [M-C-down] 'goto-next-marked-section-and-process-it-and-play)
(define-key  midi-laml-mode-map [M-C-up] 'goto-previous-marked-section-and-process-it-and-play)
(define-key  midi-laml-mode-map [M-C-end] 'process-current-marked-region-and-play)
(define-key  midi-laml-mode-map [S-C-down] 'process-from-current-point-to-end-and-play)
(define-key  midi-laml-mode-map [end] 'play-single-noteon-maybe-cached)

; End Up/Down/End

; F9 - F12
(define-key  midi-laml-mode-map [f9]   'add-score-comments-to-current-track-or-page-piano-roll-duration)
(define-key  midi-laml-mode-map [S-f9]   'add-score-comments-to-current-track-or-page-piano-roll-velocity)
(define-key  midi-laml-mode-map [C-f9]  'remove-score-comments-from-current-track-or-page)
(define-key  midi-laml-mode-map [f10]  'toggle-shadow-setting)  ; earlier: process-current-bar-and-play
(define-key  midi-laml-mode-map [f11]  'process-current-page-and-play)
(define-key  midi-laml-mode-map [C-f11] 'process-current-page-and-play-mouse-speed-control)
(define-key  midi-laml-mode-map [S-f11] 'process-from-point-and-current-page-and-play)
(define-key  midi-laml-mode-map [M-f11] 'process-from-current-point-to-end-and-play)


(define-key  midi-laml-mode-map [f12]  'process-current-blank-separated-region-and-play)
(define-key  midi-laml-mode-map [S-f12]  'process-from-point-and-current-blank-separated-region-and-play)


; KEY PAD:
; Play single notes. Edit and play:
(define-key  midi-laml-mode-map [kp-space] 'play-single-noteon-maybe-cached)  ; Key pad: 5     
(define-key  midi-laml-mode-map [kp-up] 'kp-up-action)        
(define-key  midi-laml-mode-map [kp-down] 'kp-down-action)
(define-key  midi-laml-mode-map [kp-left] 'kp-left-action)        
(define-key  midi-laml-mode-map [kp-right] 'kp-right-action)

; Movement/edit without playing:
(define-key  midi-laml-mode-map [S-kp-space] 'kp-shift-space-action)  ; Shift Key pad: 5     
(define-key  midi-laml-mode-map [S-kp-up]   'kp-shift-up-action)
(define-key  midi-laml-mode-map [S-kp-down] 'kp-shift-down-action)
(define-key  midi-laml-mode-map [S-kp-left] 'kp-shift-left-action)

(define-key  midi-laml-mode-map [C-kp-up]   'kp-control-up-action)
(define-key  midi-laml-mode-map [C-kp-down] 'kp-control-down-action)
(define-key  midi-laml-mode-map [C-kp-space] 'kp-control-home-action)

(define-key  midi-laml-mode-map [S-kp-right] 'kp-shift-right-action)
(define-key  midi-laml-mode-map [C-S-kp-up] 'kp-control-shift-up-action)
(define-key  midi-laml-mode-map [C-S-kp-down] 'kp-control-shift-down-action)

; Bar and marked sections:
;(define-key  midi-laml-mode-map [C-kp-up] 'goto-previous-bar-and-process-current-bar-and-play)    ; Now shadow playing
;(define-key  midi-laml-mode-map [C-kp-down] 'goto-next-bar-and-process-current-bar-and-play)
(define-key  midi-laml-mode-map [M-C-kp-up] 'goto-previous-marked-section-and-process-it-and-play)
(define-key  midi-laml-mode-map [M-C-kp-down] 'goto-next-marked-section-and-process-it-and-play)

; Kp enter:
(define-key  midi-laml-mode-map [kp-enter]  'process-current-page-and-play)
(define-key  midi-laml-mode-map [C-kp-enter] 'process-current-page-and-play-mouse-speed-control)
(define-key  midi-laml-mode-map [S-kp-enter] 'process-from-point-and-current-page-and-play)
(define-key  midi-laml-mode-map [M-kp-enter] 'process-from-current-point-to-end-and-play)

(define-key  midi-laml-mode-map [C-home] 'process-current-bar-and-play)    ; Earlier C-kp-space, which now is used for playing of current shadow.
(define-key  midi-laml-mode-map [M-C-kp-space] 'process-current-marked-region-and-play)    ; Key pad 5, M-C-kp-5

; END KEY PAD:

; Left and right arrows:
(define-key  midi-laml-mode-map [left] '(lambda () (interactive) "Subtract from attribute - level 1" (do-subtract-from-attribute-of-current-form 1)))
(define-key  midi-laml-mode-map [C-left] '(lambda () (interactive) "Subtract from attribute - level 2" (do-subtract-from-attribute-of-current-form 2)))
(define-key  midi-laml-mode-map [C-kp-left] '(lambda () (interactive) "Subtract from attribute - level 2" (do-subtract-from-attribute-of-current-form 2)))
(define-key  midi-laml-mode-map [C-M-left] '(lambda () (interactive) "Subtract from attribute - level 3" (do-subtract-from-attribute-of-current-form 3)))
(define-key  midi-laml-mode-map [C-M-kp-left] '(lambda () (interactive) "Subtract from attribute - level 3" (do-subtract-from-attribute-of-current-form 3)))

(define-key  midi-laml-mode-map [right] '(lambda () (interactive) "Add to attribute - level 1"(do-add-to-attribute-of-current-form 1)))
(define-key  midi-laml-mode-map [C-right] '(lambda () (interactive) "Add to attribute - level 2" (do-add-to-attribute-of-current-form 2)))
(define-key  midi-laml-mode-map [C-kp-right] '(lambda () (interactive) "Add to attribute - level 2" (do-add-to-attribute-of-current-form 2)))
(define-key  midi-laml-mode-map [C-M-right] '(lambda () (interactive)  "Add to attribute - level 3" (do-add-to-attribute-of-current-form 3)))
(define-key  midi-laml-mode-map [C-M-kp-right] '(lambda () (interactive)  "Add to attribute - level 3" (do-add-to-attribute-of-current-form 3)))

; Other function keys:
(define-key  global-map [pause] 'stop-midi-playing)                  ; Pause/Break                               NOTICE global-map
(define-key  global-map [C-pause] 'again-play-midi-file)             ;                                           NOTICE global-map

(define-key  midi-laml-mode-map [kp-add] (function faster-live-cursor))
(define-key  midi-laml-mode-map [kp-subtract] (function slower-live-cursor))
(define-key  midi-laml-mode-map [kp-delete] (function toggle-use-live-cursor-movement))
(define-key  midi-laml-mode-map [kp-divide] (function toggle-playing-fixed-part))

(define-key midi-laml-mode-map [kp-multiply] (function midl-documentation-of-key))
(define-key midi-laml-mode-map [home] (function midl-documentation-of-key))         ; Key pad independent alias

(define-key  midi-laml-mode-map [next]  'process-next-page-and-play)
(define-key  midi-laml-mode-map [prior]  'process-previous-page-and-play)


; End Key Pad

; Other Mouse bindings:

(define-key  midi-laml-mode-map [C-down-mouse-1] '(lambda () (interactive) (set-attribute-or-form-for-interaction)))

; ---------------------------------------------------------------------------------------------------------------------------------------

; CONVENIENT INTERACTIVE EMACS LIST FUNCTIONS: M-x ...
; If you do not need them, or if they conflict with other functions of the same names, you can delete them without doing any harm.

(defun make-voice-browser ()
  (interactive)
  "Make a voice browser of the current MIDL file."
  (make-voice-collection-from-midl-file))

(defun make-channel-browser ()
  (interactive)
  "Make a channel browser of the current MIDL file."
  (process-and-make-channel-stat))

(defun goto-time (tm)
  (interactive "nAbsolute time: ")
  "From within a track, goto the midi event closest to absTime time. Works only in absTime mode."
  (goto-abs-time tm))
 

; -----------------------------------------------------------------------------------------------------------------------

; Emacs tools menu contributions:

(laml-define-key global-map [menu-bar tools midi-laml-menu]
    (cons "Make MIDI Laml File" (make-sparse-keymap "Midi Laml")))

(if tyros-support
    (laml-define-key  global-map [menu-bar tools midi-laml-menu new-multi-pad]
      '("Make tyros multipad..." . (lambda () (interactive) (make-new-midl-file "tyros-multipad")))))

(if tyros-support
    (laml-define-key  global-map [menu-bar tools midi-laml-menu new-abs-time-style]
      '("Make tyros absTime style..." . (lambda () (interactive) (make-new-midl-file "tyros-style-abs-time")))))

(laml-define-key  global-map [menu-bar tools midi-laml-menu new-format-1-16]
      '("Make format 1 midi file - 16 tracks..." . (lambda () (interactive) (make-new-midl-file "simple-midi-format-1-16"))))

(laml-define-key  global-map [menu-bar tools midi-laml-menu new-format-1-2]
      '("Make format 1 midi file - 2 tracks..." . (lambda () (interactive) (make-new-midl-file "simple-midi-format-2"))))

(laml-define-key  global-map [menu-bar tools midi-laml-menu new-format-0-midi]
      '("Make format 0 midi file..." . (lambda () (interactive) (make-new-midl-file "simple-midi-format-0"))))

; -----------------------------------------------------------------------------------------------------------------------------------

; Keypad actions:

(defun kp-up-action ()
  (interactive)
  (previous-midi-message-and-play)
  (mark-shadow-of-current-event))

(defun kp-down-action ()
  (interactive)
  (next-midi-message-and-play)
  (mark-shadow-of-current-event)
)

(defun kp-left-action ()
  "Subtract from attribute - level 1" 
  (interactive)
  (let ((play-after-condition 'after-navigate-and-edit))
    (do-subtract-from-attribute-of-current-form 1)))

(defun kp-right-action ()
  "Add to attribute - level 1" 
  (interactive)
  (let ((play-after-condition 'after-navigate-and-edit))
    (do-add-to-attribute-of-current-form 1)))

(defun kp-shift-space-action ()
  "Hightlight current MIDI event. With shadow."
  (interactive)
  (beginning-of-line)
  (highlight-this-line)
  (mark-shadow-of-current-event))

(defun kp-shift-up-action ()
  "Move to previous midi event and highlight. With shadow." 
  (interactive)
  (let ((play-after-condition 'never)
        (target-form-of-navigation "NoteOn"))
    (previous-midi-message-and-play)
    (mark-shadow-of-current-event)))

(defun kp-control-up-action ()
  "Move to previous midi event and highlight. With shadow." 
  (interactive)
  (message "It is not possible to play shadows backward. Use 'the other' Control Up Arrow to play previous bar"))

(defun kp-control-down-action ()
  "Move past the current shadow, form a new similar shadow, and play it. Leave cursor at end." 
  (interactive)
  (if shadow-overlay
      (let ((end-p (overlay-end shadow-overlay)))
        (goto-char end-p)
        (forward-midi 1) (backward-midi 1)  ;  weird - but a convenient way to ensure correct starting of next playing
        (highlight-this-line)
        (mark-shadow-of-current-event)
        (process-shadow-and-play nil)
      )
      (message "Cannot. No shadow exists.")))

(defun kp-control-home-action ()
  "Move past the current shadow, form a new similar shadow, and play it. Leave cursor at end." 
  (interactive)
  (if shadow-overlay
      (process-shadow-and-play nil)
      (message "Cannot. No shadow exists.")))

(defun kp-shift-down-action ()
  "Move to next midi event and highlight. with shadow." 
  (interactive)
  (let ((play-after-condition 'never)
        (target-form-of-navigation "NoteOn"))
    (next-midi-message-and-play)
    (mark-shadow-of-current-event)))

(defun kp-control-shift-up-action ()
  "Move to previous midi event and highlight. No shadow moving. Keep exiting shadow as long as the point is inside it." 
  (interactive)
  (let ((play-after-condition 'never)
        (target-form-of-navigation t)
       )
    (previous-midi-message-and-play)

    ; Reset shadow if point moves outside the shadow:
    (if shadow-overlay   
        (if (or (< (point) (overlay-start shadow-overlay)) (> (point) (overlay-end shadow-overlay)))
            (reset-shadow-overlay)))
  )
)

(defun kp-control-shift-down-action ()
  "Move to next midi event and highlight. No shadow moving. Keep exiting shadow as long as the point is inside it." 
  (interactive)
  (let ((play-after-condition 'never)
        (target-form-of-navigation t)
       )
    (next-midi-message-and-play)

    ; Reset shadow if point moves outside the shadow:
    (if shadow-overlay   
        (if (or (< (point) (overlay-start shadow-overlay)) (> (point) (overlay-end shadow-overlay)))
            (reset-shadow-overlay)))
  )
)


(defun kp-shift-left-action ()
  "Subtract from attribute - level 1" 
  (interactive)
  (let ((play-after-condition 'never))
    (do-subtract-from-attribute-of-current-form 1)))

(defun kp-shift-right-action ()
  "Add to attribute - level 1" 
  (interactive)
  (let ((play-after-condition 'never))
    (do-add-to-attribute-of-current-form 1)))

; -----------------------------------------------------------------------------------------------------------------------------------




(defun buffer-reset-undo ()
  "Discard undo information in the current buffer"
  (buffer-disable-undo)
  (buffer-enable-undo))



(defun toggle-syntax-highlighting ()
  "Toggle syntax highlighting"
  (interactive)
  (font-lock-mode nil)
)


(defun make-new-midl-file (template-name)
 (let ((use-dialog-box nil))  ; Prevents Windows Explorer Dialog - Per dynamic binding!!!
  (let* ((dir0 (call-interactively (function (lambda (dir) (interactive "DMake a midi laml file in which directory: ") dir))))
         (name (call-interactively (function (lambda (name) (interactive "sName of midi laml file (without extension): ") name))))
         (dir (ensure-trailing-slash (expand-file-name dir0)))
         (new-buffer (create-file-buffer (concat dir name "." "midl"))))
    (set-buffer new-buffer)
    (write-file (concat dir name "." "midl"))
    (insert-file-contents (concat laml-dir "styles/xml-in-laml/midi/" "templates/" template-name "." "tpl"))
    (midi-laml-mode)
    (set-window-buffer (selected-window) new-buffer)
    new-buffer)))



; ---------------------------------------------------------------------------------------------------------------

(defvar dired-refresh-count 0 "Global variable that limits the number of times timer-based dired refreshing is attempted.")

(defun midi-to-laml (processing-mode counter-transposition &optional dont-ask-for-track-separation) 
  "From dired, process the selected midi file to the corresponding LAML file. Create a midi-temp directory for
temporary files."
 (interactive (list 
                (completing-read "Processing mode: " '(("absTime" . nil) ("absTime-on-off" . nil) ("deltaTime" . nil) ("raw" . nil)) nil nil "absTime")
                (completing-read "Counter transposition: " '() nil nil "0")))
  
 (let ((ask-for-track-separation (if dont-ask-for-track-separation nil t)))
 
                                        ; Ensure that the midi-temp directory exists
   (if (not (file-exists-p (concat (current-directory) "midi-temp/"))) ; directory existence
       (make-directory (concat (current-directory) "midi-temp/")))
 
   (let ((selected-files (dired-get-marked-files nil)))
     (if (and (= 1 (length selected-files)) (file-regular-p (car selected-files)))
         (let* ((full-path-midi-file (car selected-files))
                (file-name (file-name-proper (file-name-nondirectory full-path-midi-file)))
                (midl-file-name (concat file-name "." "midl"))
                (ext (file-name-extension full-path-midi-file)))
           (if (and ask-for-track-separation (midi-extension-p ext))
               (let ((track-separation (ask-user "Should the midi file be track separated [yes/no]?" "no")))
                 (midi-file-to-laml-file processing-mode counter-transposition track-separation full-path-midi-file))
             (midi-file-to-laml-file processing-mode counter-transposition nil full-path-midi-file))
           (other-window 1) ; now in dired window
           (setq dired-refresh-count 0)
           (run-at-time 1 nil (function refresh-dired-do-focus-midl-file) midl-file-name)
           )
       (progn
         (beep)
         (message "Cannot process. You should only select a single midi file."))))))

(defun refresh-dired-do-focus-midl-file (midl-file-name)
  (revert-buffer)
  (let ((p0 (point)))
    (goto-char (point-min))
    (let ((res (search-forward midl-file-name nil t)))
      (if res 
          (progn
            (search-backward midl-file-name)
            (setq dired-refresh-count 0))
          (progn
            (setq dired-refresh-count (+ 1 dired-refresh-count))
            (if (< dired-refresh-count 10) (run-at-time 1 nil (function refresh-dired-do-focus-midl-file) midl-file-name)))))))


; Convenient shortcuts bound to convenient key shortcuts:
(defun midi-to-laml-abstime-zero-counter-transposition ()
  "From dired, process the selected midi file to the corresponding LAML file. Makes midi file in absTime mode with no counter transposition."
  (interactive)
  (midi-to-laml "absTime" "0" t))

(defun midi-to-laml-deltatime-zero-counter-transposition ()
  "From dired, process the selected midi file to the corresponding LAML file. Makes midi file in deltaTime mode with no counter transposition."
  (interactive)
  (midi-to-laml "deltaTime" "0" t))


(defun midi-extension-p (x)
  (or (equal (downcase x) "mid") (equal (downcase x) "midi")))


; Asynchronous midi file to laml file processing via laml temp dir.
(defun midi-file-to-laml-file (processing-mode counter-transposition track-separation full-path-midi-file) 
  (let ((laml-temp-processing-dir (concat laml-dir "temp/"))) 
    (make-a-file-from-laml-template 
       "temp-processing-program.laml"
       laml-temp-processing-dir
       "midi-to-laml-processor"
       'laml-mode
       (list
        (list "FULL-PATH-TO-MIDI-FILE" full-path-midi-file)
        (list "THE-PROCESSING-MODE" processing-mode)
        (list "THE-COUNTER-TRANSPOSITION" counter-transposition)
        (list "THE-TRACK-SEPARATION" (if track-separation "true" "false"))
        )
       (concat laml-dir "styles/xml-in-laml/midi/" "templates/"))  
    (laml-process-file  (concat laml-temp-processing-dir "temp-processing-program.laml"))))

; Synchronous version with more parameters.
(defun sync-midi-file-to-laml-file (processing-mode counter-transposition track-separation full-path-midi-file full-path-laml-file) 
  (let* ((laml-temp-processing-dir (concat laml-dir "temp/")) 
         (buf (make-a-file-from-laml-template 
               "temp-processing-program.laml"
               laml-temp-processing-dir
               "sync-midi-to-laml-processor"
               'laml-mode
               (list
                (list "FULL-PATH-TO-MIDI-FILE" full-path-midi-file)
                (list "FULL-PATH-TO-LAML-FILE" full-path-laml-file)
                (list "THE-PROCESSING-MODE" processing-mode)
                (list "THE-COUNTER-TRANSPOSITION" counter-transposition)
                (list "THE-TRACK-SEPARATION" (if track-separation "true" "false"))
                )
               (concat laml-dir "styles/xml-in-laml/midi/" "templates/"))))
    (kill-buffer buf) ; Cleans up. Buffer not used - only the written file. 
    (laml-sync-process-file (concat laml-temp-processing-dir "temp-processing-program.laml") 'silent)))

(defun sync-ast-file-to-laml-file (full-path-ast-file full-path-laml-file) 
  (let ((laml-temp-processing-dir (concat laml-dir "temp/"))) 
    (make-a-file-from-laml-template 
       "temp-processing-program.laml"
       laml-temp-processing-dir
       "sync-ast-to-laml-processor"
       'laml-mode
       (list
        (list "FULL-PATH-TO-AST-FILE" full-path-ast-file)
        (list "FULL-PATH-TO-LAML-FILE" full-path-laml-file)
       )
       (concat laml-dir "styles/xml-in-laml/midi/" "templates/"))  
    (laml-sync-process-file (concat laml-temp-processing-dir "temp-processing-program.laml") 'silent)))


; The function midi-to-laml is bound to a dired menu entry in the function laml-enable-dired-mode of
; the file emacs-support/laml-key-menu-bindings.el.
; The file laml-key-menu-bindings.el is part of the LAML distribution.

; ---------------------------------------------------------------------------------------------------------------

(defun outdent-channel (n)
 "Outdent all midi messages that belong to channel n"
 (interactive "nOutdent channel: ")
 (message (concat "Outdenting channel " (number-to-string n) "..."))
 (save-excursion
  (if (not (looking-at "(MidiTrack"))
      (backward-up-list 1))
  (let ((limit (point))
        (done nil))
    (forward-sexp 1) (backward-char 1)
    
    (while (not done)
      (if (re-search-backward 
           (regular-expression 
            `(concat "'channel" (one-or-more " ") (concat "\"") ,(int-to-string n) (concat "\"")))
           limit t)
          (progn
            (beginning-of-line 1)
            (indent-amount -2)
          )
          (setq done t)))))
  (message "DONE"))

(defun outdent-multiple-channels (str-int-lst)
  "Outdent all midi messages that belong to a number of channels."
  (interactive "sOutent which channels (numbers separated by space): ")
  (message (concat "Outdenting channels " str-int-lst "..."))
  (save-excursion
    (let ((n-list (car (read-from-string (concat "(" str-int-lst ")")))))
      (mapcar (function outdent-channel) n-list)))
  (message "DONE"))
    


(defun indent-amount (a)
  (if (> a 0)
      (insert (make-string a 32))
      (let ((count 0))
        (while (and (< count (abs a)) (looking-at " "))
           (delete-char 1)
           (setq count (+ count 1))))))



; ---------------------------------------------------------------------------------------------------------------

(defun format-attributes-of-multi-pad ()
  "Reformat the attributes of a TyrosMulitiPad form."
  (interactive)

  (save-excursion
    (goto-char (point-min))
    (let ((res (search-forward "'name-1" nil t)))
       (if res
           (progn
             (beginning-of-line 1)

             (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)
             (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)   (end-of-line)
             (forward-char 1)

             (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)
             (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)   (end-of-line)
             (forward-char 1)

             (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)
             (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)   (end-of-line)
             (forward-char 1)

             (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)
             (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)   (end-of-line 1) (delete-char 1)   (end-of-line)
             (forward-char 1))
           (progn
             (beep)
             (message "You can only use this command on a buffer with multipad content"))))))

; ---------------------------------------------------------------------------------------------------------------
; OVERLAY STUFF. Not used.

(defvar overlayable-forms nil)

(setq overlayable-forms
  (regular-expression
    '(concat "(" (one-or-more " ")
       (or "ProgramChange" "ControlChange" "NoteOn" "NoteOff" "SysEx" "Meta")   ; more
    )))

(defun prepare-overlaying ()
 (interactive)
 (remove-overlays-from-current-buffer)
 (setq buffer-invisibility-spec nil)
 (let ((count 0))
 (save-excursion
  (goto-char (point-min))
  (let ((done nil))
    (while (not done)
      (if (re-search-forward overlayable-forms nil t)
        (let* ((kind (type-of-overlay-under (point)))
               (ext (extension-of-overlay-under (point)))
               (overlay (make-overlay (car ext) (cdr ext) (current-buffer))))
          (overlay-put overlay 'invisible kind)
          (overlay-put overlay 'intangible nil)   ; ???
          (setq count (+ count 1))
          (message (number-to-string count))
        )
        (setq done t)))))))

(defun remove-overlays-from-current-buffer()
  (let ((overlay-list (overlays-in (point-min) (point-max))))
    (mapcar (function (lambda (o) (delete-overlay o))) overlay-list)))

; Return the type of the overlay of the form under point. 
; Point is assumed to be inside the form (a XML-in-LAML midi message).
(defun type-of-overlay-under (p)
  (save-excursion 
    (backward-up-list 1)
    (forward-char)
    (let ((name-of-form (string-to-symbol (name-under-point))))
      (backward-up-list 1)
      (let ((p0 (point)))
        (forward-sexp 1)
        (let ((p1 (point)))
          (backward-sexp 1)
          (let ((channel-p (search-forward "'channel" p1 t)))
            (if channel-p
                (int-value-of-string-ahead)
              name-of-form)))))))

(defun int-value-of-string-ahead ()
  (progn
    (search-forward "\"")
    (let ((p2 (point)))
      (search-forward "\"")
      (string-to-number (buffer-substring p2 (- (point) 1))))))

; Return a cons pair of start and end position of the overlay of the form under point. 
; Point is assumed to be inside the form (a XML-in-LAML midi message).
(defun extension-of-overlay-under (p)
  (save-excursion 
    (backward-up-list 1)
    (let ((p0 (point)))
      (forward-sexp 1)
      (end-of-line 1) 
      (while (looking-at-chars (point) white-space-char-list) (forward-char 1))
      (cons p0 (point)))))

; delete?
(defvar the-visible-channel t)

(defun show-only-channel (n)
  "Show only channel n"
  (interactive "nShow only channel: ")
  (show-all-channels)
  (add-to-invisibility-spec 'SysEx)   (add-to-invisibility-spec 'Meta)
  (setq the-visible-channel n)
  (mapcar 
   (lambda (ch) 
     (if (not (= n ch))
       (add-to-invisibility-spec ch)))
   (number-interval 1 16)))

(defun show-all-channels ()
  "Show all channels"
  (interactive)
  (setq buffer-invisibility-spec nil))

(defun next-visible-line ()
  (let* ((overlays (overlays-at (point))))
    (if (not (null overlays))
        (let ((overlay-props (overlay-get (car overlays) 'invisible)))
          (next-line 1)
          (if (not (eq overlay-props the-visible-channel))
              (next-visible-line)))
        (next-line 1))))


; ---------------------------------------------------------------------------------------------------------------
; Midi playing

(defvar midi-file-playing nil "The full path to the midi file currently playing")
(defvar midi-play-process-name "MIDI play")

(defun play-midi-file (midi-file-path &optional given-midi-output-port dont-remember-file)
  "Play asynchronously the midi file in midi-file-path with a midi player selected via the variable midi-player. Return the process.
Play on given-midi-output-port if provide (and if supported by the midi player) else (per default) on midi-out-port (the value of a global variable).
Relies on Cygwin for command line playing."
  (interactive "f:Play MIDI file: ")
  (if (not dont-remember-file) (setq midi-file-playing midi-file-path))
  (let ((midi-port (if given-midi-output-port given-midi-output-port midi-out-port))
        (SP " "))
    (cond ((eq midi-player 'winamp)
             (start-process midi-play-process-name nil winamp-player-exc midi-file-path)
          )
          ((eq midi-player 'command-line-player)
            (let ((rest (concat div-play-smf SP "--out" SP (as-string midi-port) SP "--" SP "'" midi-file-path "'"))
                  (process-name midi-play-process-name))

             ; stop an existing playing. In some contexts, the stopping has been done already due to synchronization with a live cursor.
             (if (get-process process-name)
                 (progn
                   (delete-process process-name)
                   (sit-for 0.1) ; from empirical evidence, it is useful to make this pause. If not done, the play acts as sync playing
                 ))

             (start-process process-name nil cygwin-shell "-c" rest)
            )
          )
          (t (error (concat "Unknown value of the variable midi-player: " (as-string midi-player)))))))

(defun again-play-midi-file ()
  "Play the most recently played midi file again."
  (interactive)
  (stop-midi-playing t)
  (message "Playing midi file again.")
  (play-midi-file midi-file-playing)
)

(defun play-midi-file-sync (midi-file-path &optional given-midi-output-port)
  "Play synchronously (if using command-line-play) the midi file in midi-file-path with a midi player selected via the variable midi-player. Return the process.
Play on given-midi-output-port if provided (and if supported by the midi player) else (per default) on midi-out-port (the value of a global variable).
Relies indirectly on Cygwin for command line playing."
  (interactive "f:Play MIDI file: ")
  (let ((midi-port (if given-midi-output-port given-midi-output-port midi-out-port))
        (SP " "))
    (cond ((eq midi-player 'winamp)
             (start-process midi-play-process-name nil winamp-player-exc midi-file-path)
;             (call-process winamp-player-exc nil nil nil midi-file-path)  ; ???
          )
          ((eq midi-player 'command-line-player)
            (let ((rest (concat div-play-smf SP "--out" SP (as-string midi-port) SP "--" SP "'" midi-file-path "'"))
                  (process-name midi-play-process-name))

             ; stop an existing playing:
             (if (get-process process-name)
                  (progn (delete-process process-name))
                )

            (call-process cygwin-shell nil nil t "-c" rest))
          )
          (t (error (concat "Unknown value of the variable midi-player: " (as-string midi-player)))))))


(defun illustrate-midi-file (midi-file-path)
  "Illustrate midi file in midi-file-path with a midi illustrator selected via the variable midi-illustrator-exc"
  (interactive "f:Illustrate MIDI file: ")
  (start-process "MIDI illustrate" nil midi-illustrator-exc midi-file-path))

(defvar last-kind-of-play-from-dired nil "mid or another file extension, signaling the kind of playing last initiated by play-form-dired")

(defun play-from-dired ()
  "Play a sound file (midi, mp3, wav) from dired. Works also on directories. Directories are bulk played by WinAmp. 
Midi files are played via the function play-midi-file, which in turn may use either
a command oline midi player or winamp (depending on the variable midi-player). Other sound files are always played with winamp."
  (interactive)
  (let ((selected-files (dired-get-marked-files nil)))
    (cond ((and (= 1 (length selected-files)) (file-regular-p (car selected-files)))
             (let* ((full-path-sound-file (car selected-files))
                    (ext (file-name-extension full-path-sound-file))
                    )
               (cond ((equal (downcase ext) "mid")
                      (setq last-kind-of-play-from-dired "mid")
                      (play-midi-file full-path-sound-file)
                      )
                     (t (setq last-kind-of-play-from-dired ext)
                        (start-process "Sound play" nil winamp-player-exc (concat  full-path-sound-file)))
                     )
               )
          )
          ((and (= 1 (length selected-files)) (file-directory-p (car selected-files)))       ; directory playing - non-midi win winamp
             (let* ((full-path-sound-file (car selected-files)))
                (start-process "Sound play" nil winamp-player-exc (concat  full-path-sound-file)) 
             )
          )
          (t 
           (beep)
           (message "Cannot play. You should only select a single file.")))))

(defun stop-playing-from-dired ()
  "Stop playing from dired."
  (interactive)
  (cond ((equal last-kind-of-play-from-dired "mid") 
            (stop-midi-playing)
            (setq last-kind-of-play-from-dired nil))
        ((and (not (null last-kind-of-play-from-dired)) (get-process "Sound play"))
            (delete-process "Sound play")
            (setq last-kind-of-play-from-dired nil))
        (t "Nothing to stop...")))

(defun illustrate-midi-file-from-dired ()
  "Illustrate a MIDI file"
  (interactive)
  (let ((selected-files (dired-get-marked-files nil)))
    (if (and (= 1 (length selected-files)) (file-regular-p (car selected-files)))
      (let* ((full-path-midi-file (car selected-files)))
         (illustrate-midi-file full-path-midi-file))
      (progn
        (beep)
        (message "Cannot illustrate. You should only select a single midi file.")))))

(defun process-laml-file-and-play ()
  "Process and play the current MIDI LAML file - all of it - without interactive cursor movement"
  (interactive)
  (let* ((laml-file-name (current-file))
         (proper-laml-file-name (file-name-proper laml-file-name))   ; with possible double name (icon name: x.icon)
         (midi-file-name (concat proper-laml-file-name ".mid"))
         (midi-file-path (concat (current-directory) "midi-temp/" midi-file-name))
        )
    (ensure-directory-existence (current-directory) "midi-temp/")
    (copy-file path-to-midi-error-file midi-file-path t) 
    (laml-sync-process-current-buffer)    ; playing with (laml-sync-process-current-buffer "laml-source")
    (message "A MIDI file has been generated in midi-temp. Now playing...")
    (cond ((file-exists-p midi-file-path)   ; with possible double name
            (play-midi-file midi-file-path))
          (t (error "Process-laml-file-and-play: Should not happen:")))))

(defun process-laml-file-and-illustrate ()
  "Process and illustrate the current MIDI LAML file - all of it"
  (interactive)
  (let* ((laml-file-name (current-file))
         (proper-laml-file-name (file-name-proper laml-file-name))   ; with possible double name (icon name: x.icon)
         (midi-file-name (concat proper-laml-file-name ".mid"))
         (midi-file-path (concat (current-directory) "midi-temp/" midi-file-name))
        )
    (ensure-directory-existence (current-directory) "midi-temp/")
    (copy-file path-to-midi-error-file midi-file-path t) 
    (laml-sync-process-current-buffer)  
    (message "A MIDI file has been generated in midi-temp. Now illustrating...")
    (cond ((file-exists-p midi-file-path)   ; with possible double name
            (illustrate-midi-file midi-file-path))
          (t (error "Process-laml-file-and-play: Should not happen:")))))


(defun process-laml-file-and-save (target-file-name)
  "Process midi laml file of the current buffer and save it in the file target-file-name.
target-file-name is the proper name (without path and without extension) of the target file name."
  (interactive "sName of target midi file (without extension): ")
  (let* ((kind (kind-of-midl-file-in-current-buffer)))
    (cond ((eq 'midi kind)      (process-laml-midi-file-and-save      target-file-name (current-directory)))
          ((eq 'multi-pad kind) (process-laml-multi-pad-file-and-save target-file-name (current-directory)))
          ((eq 'style kind)     (process-laml-style-file-and-save     target-file-name (current-directory)))
          (t (error (concat "process-laml-file-and-save: Unknown kind of midl file: " (symbol-to-string kind)))))))

(defvar default-icon-name "S910")

(defun process-laml-midi-file-and-save (target-file-name the-target-dir)
  (let* ((laml-file-name (current-file))
         (proper-laml-file-name (file-name-proper laml-file-name))   
         (temp-source-file-path (concat (current-directory) "midi-temp/" proper-laml-file-name "." "mid"))
         (target-file-path (concat the-target-dir target-file-name "." "mid"))
        )
    (offer-baselining-of-current-midl-source-file "Upon MIDI saving")
    (ensure-directory-existence (current-directory) "midi-temp/")
    (laml-sync-process-current-buffer)
    (copy-file temp-source-file-path target-file-path t)
    (message (concat "The file "  target-file-path  " has been created."))
))


; Modified October 29, 2009. Not tested yet. Introduced the-target-dir (before just (current-directory)).
(defun process-laml-multi-pad-file-and-save (target-file-name the-target-dir)
  (let* ((laml-file-name (current-file))
         (proper-laml-file-name (file-name-proper laml-file-name))
         (icon-name (file-name-extension proper-laml-file-name))  ; because laml-file-name has double extension - icon in the middle.
         (temp-source-file-path (concat (current-directory) "midi-temp/" proper-laml-file-name "." "mid"))
         (target-file-path 
             (if icon-name
                 (concat the-target-dir target-file-name "." icon-name "." "pad")
                 (concat the-target-dir target-file-name "." default-icon-name "." "pad"))))
    (ensure-directory-existence (current-directory) "midi-temp/")
    (laml-sync-process-current-buffer)
    (copy-file temp-source-file-path target-file-path t)
    (message (concat "The file "  target-file-path  " has been created."))))

; Modified October 29, 2009. Not tested yet. Introduced the-target-dir (before just (current-directory)).
(defun process-laml-style-file-and-save (target-file-name the-target-dir)
  (let* ((laml-file-name (current-file))
         (cur-dir (current-directory))
         (proper-laml-file-name (file-name-proper laml-file-name))
         (icon-name (file-name-extension proper-laml-file-name))  ; because laml-file-name has double extension - icon in the middle.
         (temp-source-file-path (concat cur-dir "midi-temp/" proper-laml-file-name "." "mid"))
         (target-file-path ; not used
             (if icon-name
                 (concat cur-dir target-file-name "." icon-name "." "sty")
                 (concat cur-dir target-file-name "." default-icon-name "." "sty")))
         (extension-path (concat cur-dir "midi-temp/" "extensions"))
        )
    (ensure-directory-existence cur-dir "midi-temp/")
    (laml-sync-process-current-buffer)  ; also makes the extensions file in midi-temp
    (let* ((explicit-extensions (file-read extension-path)) 
           (explicit-target-file-path (concat the-target-dir target-file-name "." explicit-extensions)))
      (copy-file temp-source-file-path explicit-target-file-path t)
      (delete-file extension-path)
      (message (concat "The file "  explicit-target-file-path  " has been created."))) ))


(defun process-laml-file-and-save-in-mid-dir-on-memory-stick ()
  "Process midi laml file of the current buffer and save it directly in the midi dir on a memory stick.
Prompt for the target file name: A proper name, without path and without extension."
  (interactive)

  (let* ((drive-letter (usb-memory-stick-drive-letter midi-dir-in-memory-stick))  ; Maybe dialogue
         (target-file-name-0 (read-from-minibuffer (format "Write to MIDI file name (with or without extension) in %s:/%s: " drive-letter midi-dir-in-memory-stick)))
         (target-file-name (file-name-sans-extension target-file-name-0))
         (kind (kind-of-midl-file-in-current-buffer))
         (target-dir (concat drive-letter ":" "/" midi-dir-in-memory-stick))
        )
    (cond ((eq 'midi kind)      (process-laml-midi-file-and-save      target-file-name target-dir))
          ((eq 'multi-pad kind) (process-laml-multi-pad-file-and-save target-file-name target-dir))
          ((eq 'style kind)     (process-laml-style-file-and-save     target-file-name target-dir))
          (t (error (concat "process-laml-file-and-save-on-mid-dir-on-memory-stick: Unknown kind of midl file: " (symbol-to-string kind)))))))

(defun process-laml-file-and-save-in-mid-dir-on-motif-xs ()
  "Process the midi laml file of the current buffer and save it directly in Midi dir on drive connected to a motif-xs via the ethernet connection.
A Motif-XS specific function. Prompt for the target file name: A proper name, without path and without extension."
  (interactive)
  (if (not (file-directory-p midi-path-on-motif-xs))
      (message "There is no connection to your Motif-XS.")
      (let* ((target-file-name-0 (read-from-minibuffer (format "Write to MIDI file name (with or without extension) on the Motif-XS: ")))
             (target-file-name (file-name-sans-extension target-file-name-0))
             (kind 'midi)               ; fixed kind!
             (target-dir midi-path-on-motif-xs)
             )
        (cond ((eq 'midi kind)      (process-laml-midi-file-and-save      target-file-name target-dir))
              (t (error (concat "process-laml-file-and-save-on-mid-dir-on-memory-stick: Unknown kind of midl file: " (symbol-to-string kind))))))))

(defvar midi-dir-in-memory-stick "Midi/" "The directory name of midi files, as used in memory sticks")

(defun process-midl-file-source-target (source-file-path target-file-path)
  "Process the midi laml file in source-file-path and write the resulting midi file to target-file-path.
Both are full file paths. The processing is done via the laml temp directory, in order not to affect the directory 
that holds the source file."
  (let* ((file-name-ext (file-name-nondirectory source-file-path))
         (proper-file-name (file-name-proper file-name-ext))
         (source-file-path-in-temp-dir (concat laml-temp-file-path file-name-ext)))
    (copy-file source-file-path source-file-path-in-temp-dir t)
    (let* ((generated-midi-file-path (concat laml-temp-file-path "midi-temp/" proper-file-name "." "mid")) )
      (ensure-directory-existence laml-temp-file-path "midi-temp/")
      (copy-file path-to-midi-error-file generated-midi-file-path  t) 
      (laml-sync-process-file source-file-path-in-temp-dir 'silent)
      (delete-file source-file-path-in-temp-dir)
      (copy-file generated-midi-file-path target-file-path t)
      'done)))

; An ad hoc midl example processing function.
; The following functions, process-all-midl-files, generalizes it in a useful way.
(defun process-midl-example-files ()
  "Process all midl files in the current directory (such as a dired directory)
and write the resulting midi files to a specialized directory in the parent of the current directory.
Just right for the organization of the example files."
  (interactive)
  (let* ((dir (current-directory))
         (dir-path-list (file-path-to-path-list dir))
         (dir-name (car (last dir-path-list))) 
         (midl-files (filter (function (lambda (f) (equal (file-name-extension f) "midl"))) (directory-files dir t)))  ; full paths
         (target-dir (concat dir "../" dir-name "-midi" "/")))
    (ensure-directory-existence (concat dir "../") (concat dir-name "-midi"))
    (mapcar2
     (function
      (lambda (sfp n)
        (message "Processing file number %d: %s" n (file-name-nondirectory sfp))
        (process-midl-file-source-target sfp (concat target-dir (file-name-proper (file-name-nondirectory sfp)) "." "mid"))))
     midl-files (number-interval 1 (length midl-files)))
    (message "Processed %d midl files" (length midl-files))))

(defun process-all-midl-files (target-dir)
  "Process all midl files in the current directory (such as a dired directory)
and write the derived midi files in target-dir." 
  (interactive "DTarget directory: ")
  (let* ((dir (current-directory))
         (dir-path-list (file-path-to-path-list dir))
         (dir-name (car (last dir-path-list))) 
         (midl-files (filter (function (lambda (f) (equal (file-name-extension f) "midl"))) (directory-files dir t)))  ; full paths
        )
    (mapcar2
     (function
      (lambda (sfp n)
        (message "Processing file number %d: %s" n (file-name-nondirectory sfp))
        (process-midl-file-source-target sfp (concat target-dir (file-name-proper (file-name-nondirectory sfp)) "." "mid"))))
     midl-files (number-interval 1 (length midl-files)))
    (message "Processed %d midl files" (length midl-files))))


; ---------------------------------------------------------------------------------------------------
; Midl source stack
; Invariant: The Midl stack (of size n+1) is formed by the current buffer at the top, and
; a number of files 0.midl, ... n-1.midl in the "project directory".
; midl-source-file-stack is a stack (list) of the file-paths n-1.midl ... 0.midl, where 0.midl is the bottom of the stack.
; The ghost stack is formed by files named j-ghost.midl, for j = n, ...
;
; EXAMPLE
; 0.midl   (bottom of stack)
; 1.midl
; 2.midl
; 3.midl
; 4.midl
; CURRENT  (in buffer and file - TOP of stack).
; 5-ghost.midl
; 6-ghost.midl


(defvar midl-source-file-stack '())
(make-variable-buffer-local 'midl-source-file-stack)

(defvar midl-source-file-ghost-stack '())
(make-variable-buffer-local 'midl-source-file-ghost-stack)

(defun pop-and-reestablish-midl-file ()
  "Pop the MIDL file stack and reestablish the MIDL file one level below it on the stack - Go down the stack"  
  (interactive)
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (if (not (null midl-source-file-stack))
      (let* ((file-path (car midl-source-file-stack))
             (ghost-file-path (make-ghost-file-path file-path))
            )
        ; Write buffer to ghost file:
        (register-current-point (point))
        (write-region (point-min) (point-max) ghost-file-path)

        ; Update buffer from top of stack
        (erase-buffer)
        (insert-file-contents file-path)
        (buffer-reset-undo)
        (let ((registered-point (extract-registered-point)))
           (if registered-point
               (goto-char registered-point)))
        (save-buffer)

        ; Delete file corresponding to top of stack:
        (delete-file file-path)
        
        ; Update stack lists:
        (setq midl-source-file-ghost-stack (cons ghost-file-path midl-source-file-ghost-stack))
        (setq midl-source-file-stack (cdr midl-source-file-stack))

        ; Info:
;        (if (= (length midl-source-file-stack) 0)
;               (message (concat "The stack has been popped, and it has now reached its minimal height. "
;                                "Ghost files: %d.")  (length midl-source-file-ghost-stack))
;               (message "The stack has been popped. Remaining entries on the stack (including this buffer): %d. Ghost files: %d."
;                        (+ (length midl-source-file-stack) 1) (length midl-source-file-ghost-stack)))
        (message (concat "After pop: %d element(s) + %d ghost(s). " (comment-of-midl-file))
                        (+ (length midl-source-file-stack) 1) (length midl-source-file-ghost-stack))
      )
      (error (concat "The midl source file stack only contains the basis buffer, which cannot be popped. Nothing done"))))

(defun make-ghost-file-path (file-path)
  "Return the ghost-file-path of file-path.
A ghost file is a specially named file that is only saved for pop-undo purposes."
  (let ((proper-name-part (file-name-proper (file-name-nondirectory file-path)))
        (directory-file-path (file-name-directory file-path)))
    (concat directory-file-path proper-name-part "-" "ghost" "." "midl")))

(defun undo-pop-midl-stack ()
  "Undo the pop - hereby going up the stack"
  (interactive)
  (if (>= (length midl-source-file-ghost-stack) 1)
      (let* ((ghost-file-path (car midl-source-file-ghost-stack))
             (file-path (make-non-ghost-file-path ghost-file-path)))

        ; Establish a new file from buffer - numbering comes derived top ghost file name:
        (write-region (point-min) (point-max) file-path)

        ; Establish buffer from ghost file:
        (erase-buffer)
        (insert-file-contents ghost-file-path)
        (buffer-reset-undo)
        (let ((registered-point (extract-registered-point)))
           (if registered-point
               (goto-char registered-point)))
        (save-buffer)

        ; Delete ghost file:
        (delete-file ghost-file-path)

        ; Update stack list:
        (setq midl-source-file-ghost-stack (cdr midl-source-file-ghost-stack))
        (setq midl-source-file-stack (cons file-path midl-source-file-stack))

        ; Info:
        (message (concat "After undo pop: %d element(s) + %d ghost(s). " (comment-of-midl-file))
                        (+ (length midl-source-file-stack) 1) (length midl-source-file-ghost-stack))
      )
      (message "Nothing to undo. Nothing done.")))

; (defun replicate-and-undo-all-pops ()
;   "Replicate current buffer of the absolute top of the stack, turning all ghost elements
; into real elements (undoing all pops)."
;   (interactive)
;   (while (not (null midl-source-file-ghost-stack))
;      (let* ((ghost-path (car midl-source-file-ghost-stack))
;             (non-ghost-path (make-non-ghost-file-path ghost-path)))
;        (rename-file ghost-path non-ghost-path t)      
;        (setq midl-source-file-ghost-stack (cdr midl-source-file-ghost-stack))
;        (setq midl-source-file-stack (cons non-ghost-path midl-source-file-stack))
;      )       
;   ))

(defun replicate-current-buffer-and-undo-all-pops ()
  "Replicate current buffer to the absolute top of the stack, turning all ghost elements into real elements (undoing all pops)."
  (interactive)
  (let ((content-to-replicate (buffer-substring-no-properties (point-min) (point-max)))
        (current-project-name (file-name-proper (file-name-proper (current-file))))
        (p-remember (point))
        (level-before-undo-pops (+ (length midl-source-file-stack) 1))
       )

    ; Do as many undo-pops as we have elements on the ghost stack:
    (while (not (null midl-source-file-ghost-stack))
       (undo-pop-midl-stack))
    
    ; Push content-to-replicate on stack:
    (push-current-midl-buffer current-project-name (current-buffer))
    (erase-buffer)
    (insert content-to-replicate)
    (add-comment-to-midl-file (concat "Replicated from level " (as-string level-before-undo-pops)))
    (goto-char p-remember)
    (buffer-reset-undo)
    (save-buffer)
 
    ; Info:
    (message "Current buffer has been replicated to top of stack. Current height: %d. Number of ghosts: %d."
             ( + (length midl-source-file-stack) 1) (length midl-source-file-ghost-stack))
  )
)


(defun make-non-ghost-file-path (ghost-file-path)
  "Return the non-ghost file-path of ghost-file-path.
A ghost file is a specially named file that is only saved for pop-undo purposes."
  (let* ((proper-name-part (file-name-proper (file-name-nondirectory ghost-file-path)))
         (i (find-in-string proper-name-part ?-))       
         (directory-file-path (file-name-directory ghost-file-path)))
    (concat directory-file-path (substring proper-name-part 0 i) "." "midl")))


(defun push-current-midl-buffer (project-name buf)
 (save-excursion
  (set-buffer buf)
  (ensure-directory-existence (current-directory) (concat project-name "/") )
  (let ((temp-midl-file (concat (current-directory) (concat project-name "/") (int-to-string (length midl-source-file-stack)) "." "midl")))
    (register-current-point (point))
    (write-region (point-min) (point-max) temp-midl-file nil 0)
    (setq midl-source-file-stack (cons temp-midl-file midl-source-file-stack)))))

(defun midl-source-file-stack-info ()
  "Provide information about the midl source file stack"
  (interactive)
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (cond ((null midl-source-file-stack)
           (message "The midl stack only contains the basis source file. Stack height: 1. Ghost elements: %d" 
                    (length midl-source-file-ghost-stack)))
        (t (message "Height of midl source file stack: %d: This buffer and %d elements below it. Ghost elements: %d"
                     (+ 1 (length midl-source-file-stack))
                     (length midl-source-file-stack)
                     (length midl-source-file-ghost-stack)))))

(defun add-comment-to-midl-file (comment)
  "Add a comment to a midl file. The comment is a one-line comment stored as a Scheme comment at the first line of the file."
  (interactive (list (read-from-minibuffer "Midl file comment: " (comment-of-midl-file))))
  (set-comment-of-midl-file comment)
)

(defun display-comment-of-midl-file ()
  "Show the MIDL file comment - the one in the top of the file."
  (interactive)
  (let ((comment (comment-of-midl-file)))
    (if (empty-string-p comment)
        (message "No comment of this midl file. Add one with M-x add-comment-to-midl-file or  C-c C-a")
        (message comment))))
        

(defun is-midl-stack-inconsistent()
  "Is the midl stack of the current project (implicitly determined from proper file name) inconsistent in relation to the
stack files in the project directory."
  (let* ((laml-file-name (current-file))
         (proper-laml-file-name (file-name-proper laml-file-name))
         (project-name (file-name-proper proper-laml-file-name))  ; removing possible double extension 
         (project-dir (concat (current-directory) project-name "/")))
    (and (null midl-source-file-stack)
         (or (file-exists-p (concat project-dir "/" "0" "." "midl")) 
             (file-exists-p (concat project-dir "/" "0-ghost" "." "midl")))))) 

(defun recover-midl-source-file-stack ()
  "Take the information in the project directory, and establish midl stack from it. 
This defines the variabales midl-source-file-stack and midl-source-file-ghost-stack.
Useful if you return to a source file for which in another sessions have left information on the stack."
;  (interactive)
  (let ((current-project-name (file-name-proper (file-name-proper (current-file)))))
    (recover-middle-stack-from-project-directory current-project-name)))

(defun recover-middle-stack-from-project-directory (project-name)
  (let* ((project-dir (concat (current-directory) project-name "/")))
    (if (file-exists-p project-dir)
        (if (null midl-source-file-stack)
            (let* ((stack-files (filter 
                                  (function (lambda (x) 
                                            (let ((y (file-name-proper (file-name-nondirectory x))))
                                              (numeric-string y))))
                                  (directory-files (concat (current-directory) project-name "/"))))
                   (sorted-stack-files (sort stack-files (function (lambda (s1 s2)
                                                                     (let ((s1-num (as-number (file-name-proper s1)))
                                                                           (s2-num (as-number (file-name-proper s2))))
                                                                       (<= s1-num s2-num))))))

                   (ghost-files (filter 
                                  (function (lambda (x) 
                                            (let ((y (file-name-proper (file-name-nondirectory x))))
                                              (ghost-name-p y))))
                                  (directory-files (concat (current-directory) project-name "/"))))
                   (sorted-ghost-files (sort ghost-files (function (lambda (s1 s2)
                                                                     (let ((s1-num (get-ghost-number s1))
                                                                           (s2-num (get-ghost-number s2)))
                                                                       (<= s1-num s2-num))))))
                  )
              (setq midl-source-file-stack 
                    (mapcar (function (lambda (f) (concat (current-directory) project-name "/" f))) (reverse sorted-stack-files)))
              (setq midl-source-file-ghost-stack 
                    (mapcar (function (lambda (f) (concat (current-directory) project-name "/" f))) sorted-ghost-files))
              (message "The midl stack has been reestablished. Current height (including current buffer): %s."
                       (+ 1 (length midl-source-file-stack))))
          (message "The stack already exists. Height (including current buffer) %s. Nothing done." (+ 1 (length midl-source-file-stack))))
      (message "Nothing to recover. Nothing done."))))

(defun reset-midl-stack ()
  "Get rid of earlier pushed midl files on the stack. That is, clear the stack entirely.
Deletes stack files in the project directory. Keep the baselines."
  (interactive)
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (if (ask-user "You are about to delete all stack entries below the top, and all ghost elements. Keeping just the current buffer. Proceed?")
      (let ((current-project-name (file-name-proper (file-name-proper (current-file)))))
        (get-rid-of-ghost-stack)    
        (reset-midl-stack-of-project-name current-project-name))
      (message "Nothing Done.")))

(defun reset-and-empty-midl-stack ()
  "Get rid of earlier pushed midl files on the stack. That is, clear the stack entirely. In addition, clear the current entry such that the stack is entirely empty.
Keep the baselines."
  (interactive)
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (if (ask-user "You are about to delete all stack entries below the top, all ghost elements, And The Current Midl File. Proceed?")
      (let ((current-project-name (file-name-proper (file-name-proper (current-file)))))
        (get-rid-of-ghost-stack)    
        (reset-midl-stack-of-project-name current-project-name)
        (erase-buffer)
      )
      (message "Nothing Done.")))

(defun get-rid-of-ghost-elements ()
  "Get rid all ghost elements."
  (interactive)
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (get-rid-of-ghost-stack)
  (show-midl-mode-status-string)
  (message "After removing ghost stack elements: Midl stack height (including this buffer): %d. Ghost files: %d."
           (+ (length midl-source-file-stack) 1) (length midl-source-file-ghost-stack))
)
    

(defun reset-midl-stack-of-project-name (project-name)
  (let* ((project-dir (concat (current-directory) project-name "/")))
    (setq midl-source-file-stack nil)
    (if (file-exists-p project-dir)
        (let ((stack-files (filter 
                                (function (lambda (x) 
                                            (let ((y (file-name-proper (file-name-nondirectory x))))
                                              (numeric-string y))))
                                (directory-files (concat (current-directory) project-name "/")))))
          (cond ((null stack-files) (message "No stack files in existing project directory. Nothing done."))
                ((not (null stack-files))
                   (mapcar (function 
                            (lambda (proper-file-name)
                              (delete-file (concat (current-directory) project-name "/" proper-file-name))))
                           stack-files)
                   (message "%s stack file(s) have been deleted. The midl stack now only consists of the current buffer."
                           (length stack-files)))))
        (message "Cannot find any project directory. Nothing done."))))


(defun push-copy-of-existing-midl-stack-entry (i)
  (interactive
    (list (if (= (length midl-source-file-stack) 0)  ; only possibility is current buffer = 0
              0
              (string-to-number
               (read-from-minibuffer 
                (format "Which entry do you wish to replicate: (0 = bottom, ..., %d = top = current buffer): " 
                        (length midl-source-file-stack)))))))
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (let* ((laml-file-name (current-file))
         (proper-laml-file-name (file-name-proper laml-file-name))
         (project-name (file-name-proper proper-laml-file-name))) ; eliminate possible double extension
    (cond ((= i (length midl-source-file-stack)) 
                   (push-current-midl-buffer project-name (current-buffer))
                   (message (concat "The existing buffer has been replicated on the midl stack.")))
          ((and (< i (length midl-source-file-stack)) (>= i 0))
              (push-current-midl-buffer project-name (current-buffer))
              (erase-buffer)
              (insert-file-contents (concat (current-directory) project-name "/" (int-to-string i) ".midl"))
              (buffer-reset-undo)
              (message "Element number %d from the stack has been copied to the top." i))
          (t (message "Illegal copying. Provide a number between 0 and %d. Nothing done." (length midl-source-file-stack))))))

(defun push-latest-temporary-selection-on-midl-stack ()
  "Push the most recent temporary selection onto the MIDL stack. Useful if you want to elaborate the most recent selection as a MIDI LAML file."
  (interactive)
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))

  (if (file-exists-p (concat (current-directory) "midi-temp"))
      (if (file-exists-p (concat (current-directory) "midi-temp/" "sel-temp.midl"))
          (let ((current-project-name (file-name-proper (file-name-proper (current-file)))))
            (get-rid-of-ghost-stack)
            (push-current-midl-buffer current-project-name (current-buffer))
            (erase-buffer)
            (insert-file-contents (concat (current-directory) "midi-temp/" "sel-temp.midl"))
            (buffer-reset-undo)
            (save-buffer)

            ; Info:
            (message "The latest temporary selection has been pushed on midl stack. Current height: %d. Number of ghosts: %d."
                     ( + (length midl-source-file-stack) 1) (length midl-source-file-ghost-stack))
          )
          (message "The temporary file sel-temp.midl does not exists in the midi-temp directory. Nothing done."))
      (message "The midi-temp directory does not exists. Nothing done."))
)

(defun push-external-midl-file-on-midl-stack (midl-file-path)
  "Push an externally, existing MIDI LAML file onto the MIDL stack."
  (interactive "fPush which existing midl file: ")
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))

  (if (file-exists-p midl-file-path)
      (let ((current-project-name (file-name-proper (file-name-proper (current-file)))))
        (get-rid-of-ghost-stack)
        (push-current-midl-buffer current-project-name (current-buffer))
        (erase-buffer)
        (insert-file-contents midl-file-path)
        (buffer-reset-undo)
        (save-buffer)

                                        ; Info:
        (message "An external file has been pushed on midl stack. Current height: %d. Number of ghosts: %d."
                 ( + (length midl-source-file-stack) 1) (length midl-source-file-ghost-stack))
      )
    (message "The midl file does not exist. Nothing done."))
)

(defun push-external-midi-file-on-midl-stack (midi-file-path)
  "Parse and push an external MIDI file onto the MIDL stack" 
  (interactive "fParse and push which existing MIDI file: ")
  (push-external-midi-file-on-midl-stack-internal midi-file-path)
  (add-comment-to-midl-file (concat "From MIDI file: " midi-file-name))
)

(defun push-external-usb-midi-file-on-midl-stack ()
  "Push an existing MIDI file from the Midi directory of a connected USB memory stick to the Midl stack." 
  (interactive)
  (let* ((drive-letter (usb-memory-stick-drive-letter midi-dir-in-memory-stick))  ; Maybe dialogue
         (midi-file-name (completing-read 
                           (format "Parse and push midi file from %s:%s: " drive-letter midi-dir-in-memory-stick)
                           (directory-files (concat drive-letter ":" "/" midi-dir-in-memory-stick)) nil t))
         (midi-file-path (concat drive-letter ":" "/" midi-dir-in-memory-stick midi-file-name))
        )
    (push-external-midi-file-on-midl-stack-internal midi-file-path)
    (add-comment-to-midl-file (concat "From USB memory stick: " midi-file-name))
  )
)

(defun push-external-motif-xs-midi-file-on-midl-stack ()
  "Push an existing MIDI file from the Motif-XS Midi directory to the Midl file stack. This is a Motif-XS specific function."
  (interactive)
  (let* (
         (midi-file-name (completing-read 
                           (format "Parse and push midi file from your Motif-XS (space for completion): ")
                           (directory-files midi-path-on-motif-xs) nil t))
         (midi-file-path (concat midi-path-on-motif-xs midi-file-name))
        )
    (push-external-midi-file-on-midl-stack-internal midi-file-path)
    (add-comment-to-midl-file (format "From Motif XS: %s at %s" midi-file-name (midl-time-string (current-time)) ))
  )
)

(defun midl-time-string (elisp-time-list)
  "Return a formatted time string of elisp-time-list, which is the time format returned by, for instance, current-time. It is a list of two or three integers."
  (let ((time-list (decode-time elisp-time-list)))
    (format "%02d-%02d-%4d %2d:%02d" (nth 3 time-list) (nth 4 time-list)  (nth 5 time-list)  (nth 2 time-list) (nth 1 time-list))))

  
(defun push-external-midi-file-on-midl-stack-internal (midi-file-path)
  (if (is-current-buffer-blank)
      (progn
         ; A way to initiate an empty MIDL LAML project:
         ; The user have just made an empty (blank) midl file. Next, the user pushes an existing MIDI file.
         ; Parse midi file and put the corresponding MIDI LAML file into this buffer as the initial frame on the MIDL Stack.
        (if (file-exists-p midi-file-path)
            (let* ((current-project-name (file-name-proper (file-name-proper (current-file))))
                   (laml-temp-processing-dir (concat laml-dir "temp/"))
                   (full-path-laml-file (concat laml-temp-processing-dir "pusing-external-midi.midl"))
                   (processing-mode "deltaTime")  ; fixed
                   (counter-transposition "0")    ; fixed
                   (buf (current-buffer))           
                  )
              (if (file-exists-p full-path-laml-file) (delete-file full-path-laml-file))
              (message "Parsing %s MIDI file with counter transposition %s." processing-mode counter-transposition)
              (sync-midi-file-to-laml-file processing-mode counter-transposition nil midi-file-path full-path-laml-file) 

              (set-buffer buf)
              (erase-buffer)
              (insert-file-contents full-path-laml-file)
              (buffer-reset-undo)
              (save-buffer)
              (if (file-exists-p full-path-laml-file) (delete-file full-path-laml-file))

              ; Info:
              (message "An external MIDI file makes up the initial frame on the stack.")
             )
          (message "The MIDI file does not exist. Nothing done."))
      )
      (progn
        (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
        (if (eq 'midi (kind-of-midl-file-in-current-buffer))
            (if (file-exists-p midi-file-path)
                (let* ((current-project-name (file-name-proper (file-name-proper (current-file))))
                       (laml-temp-processing-dir (concat laml-dir "temp/"))
                       (full-path-laml-file (concat laml-temp-processing-dir "pusing-external-midi.midl"))
                       (processing-mode (find-processing-mode-of-current-buffer))
                       (counter-transposition (find-counter-transposition-of-current-buffer))
                       (buf (current-buffer))           
                       )
                  (if (file-exists-p full-path-laml-file) (delete-file full-path-laml-file))
                  (message "Parsing %s MIDI file with counter transposition %s." processing-mode counter-transposition)
                  (sync-midi-file-to-laml-file processing-mode counter-transposition nil midi-file-path full-path-laml-file) 

                  (set-buffer buf)
                  (get-rid-of-ghost-stack)
                  (push-current-midl-buffer current-project-name (current-buffer))
                  (erase-buffer)
                  (insert-file-contents full-path-laml-file)
                  (buffer-reset-undo)
                  (save-buffer)
                  (if (file-exists-p full-path-laml-file) (delete-file full-path-laml-file))

                  ; Info:
                  (message "An external MIDI file has been parsed and pushed on midl stack. Current height: %d. Number of ghosts: %d."
                           ( + (length midl-source-file-stack) 1) (length midl-source-file-ghost-stack))
                  )
              (message "The MIDI file does not exist. Nothing done."))
          (message "It is only possible to handle standard MIDI files with this function. Consider extensions to multi-pads and styles.")))))


; ------------------------------------------------------------------
; Moved to general el June 1, 2011

(defvar ciffer-list (list 48 49 50 51 52 53 54 55 56 57) "The list of chars that are ciffers: 0-9")

(defun numeric-string (str)
  "Is the string str numeric"
  (let* ((str-lst (string-to-list str))
         (non-numeric-elements (filter (function (lambda (ch) (not (member ch ciffer-list)))) str-lst)))
    (and (> (length str) 0) (null non-numeric-elements))))
; ------------------------------------------------------------------


(defun ghost-name-p (str)
  "Is str the name of ghost file in a project dir. str is a proper name (without extension and initial path)"
  (and (>= (length str) 6)
       (equal "-ghost" (substring str (- (length str) 6) (length str)))))

(defun stack-name-p (str)
  "Is str the name of stack file in a project dir. str is a proper name (without extension and initial path)"
  (numeric-string str))
  

(defun get-rid-of-ghost-stack ()
  (while (not (null midl-source-file-ghost-stack))
    (delete-file (car midl-source-file-ghost-stack))
    (setq midl-source-file-ghost-stack (cdr midl-source-file-ghost-stack))))

(defun process-laml-file-save-and-push ()
  "Process the MIDI LAML file, save it and push it onto the stack.
If you are not at the top of the stack you loose the ghost top entries!"
  (interactive)
  (ensure-directory-existence (current-directory) "midi-temp/")
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (get-rid-of-ghost-stack)
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (let* ((kind (kind-of-midl-file-in-current-buffer)))
    (cond ((eq 'midi kind)      (process-laml-midi-file-save-and-push t))
          ((eq 'multi-pad kind) (process-laml-multi-pad-file-save-and-push))
          ((eq 'style kind) (process-laml-style-file-save-and-push))
          (t (error (concat "process-laml-file-save-and-push: Unknown kind of midl file" (symbol-to-string kind)))))))

(defun process-laml-file-save-and-push-unzoom ()
  "Process the MIDI LAML file, save it and push it onto the stack. Unzoom the fixed-part.
If you are not at the top of the stack you loose the ghost top entries!"
  (interactive)
  (ensure-directory-existence (current-directory) "midi-temp/")
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (get-rid-of-ghost-stack)
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (let* ((kind (kind-of-midl-file-in-current-buffer)))
    (cond ((eq 'midi kind)      (process-laml-midi-file-save-and-push nil))
          ((eq 'multi-pad kind) (error "Unzoom not yet support on multi pads"))
          ((eq 'style kind) (error "Unzoom not yet support on styles"))
          (t (error (concat "process-laml-file-save-and-push: Unknown kind of midl file" (symbol-to-string kind)))))))



(defun process-laml-file-save-and-push-via-ast ()
  "A slightly optimized process, save and push via an AST. Experimental. Do not use..."
  (interactive)
  (ensure-directory-existence (current-directory) "midi-temp/")
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (get-rid-of-ghost-stack)
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (let* ((kind (kind-of-midl-file-in-current-buffer)))
    (cond ((eq 'midi kind)      (process-laml-midi-file-save-and-push-via-ast))
          ((eq 'multi-pad kind) (error "Not yet supported - it is experimental"))
          ((eq 'style kind) (error "Not yet supported - it is experimental"))
          (t (error (concat "process-laml-file-save-and-push-via-ast: Unknown kind of midl file" (symbol-to-string kind)))))))


(defun process-laml-file-save-and-push-with-counter-transposition (ct)
  "Process, save and push the current MIDL file. Introduce another counter transposition"
  (interactive "nCounter transposition: ")
  (ensure-directory-existence (current-directory) "midi-temp/")
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (get-rid-of-ghost-stack)
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (let* ((kind (kind-of-midl-file-in-current-buffer)))
    (cond ((eq 'midi kind)      (process-laml-midi-file-save-and-push t (number-to-string ct) ))
          ((eq 'multi-pad kind) (process-laml-multi-pad-file-save-and-push (number-to-string ct)))
          ((eq 'style kind) (error "Cannot push with counter transposition on styles."))
          (t (error (concat "process-laml-file-save-and-push-with-counter-transposition: Unknown kind of midl file" 
                            (symbol-to-string kind)))))))

(defun process-laml-file-save-zoom-and-push (lambda-exp-filter-string)
  "Process, save and push the current MIDL file and zoom in on those MIDI LAML messages that satisfy the lambda expression predicate in lambda-exp-filter-string.
Those messsage that do not satisfy the lambda expresion predicate are passed to a new residual file in the fixed-part directory of the project dir."
  (interactive "sLambda expression zoom predicate: ")

  (ensure-directory-existence (current-directory) "midi-temp/")
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (get-rid-of-ghost-stack)
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (if (equal "absTime" (find-processing-mode-of-current-buffer))
      (let* ((kind (kind-of-midl-file-in-current-buffer)))
        (cond ((eq 'midi kind)
                  (if (= (format-of-midi-file-in-current-buffer) 0)
                      (process-laml-file-save-zoom-and-push-midi-kind-abstime lambda-exp-filter-string)
                      (message "Zooming is only available in format 0 MIDI files")))
              ((eq 'multi-pad kind) (error "Cannot zoom on multipads"))
              ((eq 'style kind) (error "Cannot zoom on styles"))
              (t (error (concat "process-laml-file-save-zoom-and-push: Unknown kind of midl file" 
                                (symbol-to-string kind))))))
      (message "Zooming is only available in absTime mode.")))


(defun process-laml-file-save-unzoom-and-zoom-on-channel (channel)
  "A variant of process-laml-file-save-zoom-and-push which unzooms and subsequently zooms in on the Notes in a given channel together with a few other control-related messages.
Essentially,  NoteOn messages will be left after zooming."
  (interactive "nShow only notes in channel: ")

  (ensure-directory-existence (current-directory) "midi-temp/")
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (get-rid-of-ghost-stack)
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (if (equal "absTime" (find-processing-mode-of-current-buffer))
      (let* ((kind (kind-of-midl-file-in-current-buffer)))
        (cond ((eq 'midi kind)
                  (if (= (format-of-midi-file-in-current-buffer) 0)
                      (let ((lambda-exp-filter-string (format "(make-zoom-on-channel-predicate %d)" channel) ))   ; Earlier: (format "(lambda (m) (and (NoteOn? m) (= %d (midi 'channel m))))" channel)
                         (process-laml-file-save-zoom-out-and-zoom-and-push-midi-kind-abstime lambda-exp-filter-string channel) 
                      )
                      (message "Zooming is only available in format 0 MIDI files")))
              ((eq 'multi-pad kind) (error "Cannot zoom on multipads"))
              ((eq 'style kind) (error "Cannot zoom on styles"))
              (t (error (concat "process-laml-file-save-zoom-and-push: Unknown kind of midl file" 
                                (symbol-to-string kind))))))
      (message "Zooming is only available in absTime mode."))
)

; A variant of process-laml-file-save-unzoom-and-zoom-on-channel which shows all messages in the given channel - not just NoteOn messages
(defun process-laml-file-all-mes-save-unzoom-and-zoom-on-channel (channel)
  "A variant of process-laml-file-save-unzoom-and-zoom-on-channel which unzooms and subsequently zooms in on messages in a given channel. Both NoteOn and ControlMessages (and other) messages will be present after zooming."
  (interactive "nShow only notes as well as control messages in channel: ")

  (ensure-directory-existence (current-directory) "midi-temp/")
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (get-rid-of-ghost-stack)
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (if (equal "absTime" (find-processing-mode-of-current-buffer))
      (let* ((kind (kind-of-midl-file-in-current-buffer)))
        (cond ((eq 'midi kind)
                  (if (= (format-of-midi-file-in-current-buffer) 0)
                      (let ((lambda-exp-filter-string (format "(make-zoom-on-channel-predicate-all %d)" channel)))   ; earlier: (format "(lambda (m) (and (channel-message? m) (= %d (midi 'channel m))))" channel)
                         (process-laml-file-save-zoom-out-and-zoom-and-push-midi-kind-abstime lambda-exp-filter-string channel) 
                      )
                      (message "Zooming is only available in format 0 MIDI files")))
              ((eq 'multi-pad kind) (error "Cannot zoom on multipads"))
              ((eq 'style kind) (error "Cannot zoom on styles"))
              (t (error (concat "process-laml-file-save-zoom-and-push: Unknown kind of midl file" 
                                (symbol-to-string kind))))))
      (message "Zooming is only available in absTime mode."))
)

(defun process-laml-file-save-and-push-toggle-processing-mode ()
  "Process, save and push the current MIDL file. 
If in deltaTime mode shift to absTime mode. If in absTime mode switch to deltaTime mode."
  (interactive)
;   (interactive 
;      (list (completing-read "Processing mode for new top: " 
;                    '(("absTime" . nil) ("deltaTime" . nil)) nil t
;                     (let ((pm (find-processing-mode-of-current-buffer)))  ; initial input - automatic toggle
;                        (if (equal pm "absTime") "deltaTime" "absTime")) )))

  (let* ((existing-mode (find-processing-mode-of-current-buffer))
         (mode (if (equal existing-mode "absTime") "deltaTime" "absTime")))
    (ensure-directory-existence (current-directory) "midi-temp/")
    (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
    (get-rid-of-ghost-stack)
    (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
    (let* ((kind (kind-of-midl-file-in-current-buffer)))
      (cond ((eq 'midi kind)      (progn
                                     (process-laml-midi-file-save-and-push t nil mode )
                                     (add-comment-to-midl-file (concat "Toggle to " mode))
                                  )
            )
            ((eq 'multi-pad kind) (process-laml-multi-pad-file-save-and-push nil mode))
            ((eq 'style kind) (process-laml-style-file-save-and-push mode))
            (t (error (concat "process-laml-file-save-and-push-toggle-processing-mode: Unknown kind of midl file" 
                              (symbol-to-string kind))))))))

; If provided, explicit-counter-transposition is a string
; If provided, new-processing-mode is either the the string "absTime" or "deltaTime"
; keep-zoom is a boolean information
(defun process-laml-midi-file-save-and-push (keep-zoom &optional explicit-counter-transposition new-processing-mode)
  (let* ((cur-buf (current-buffer))
         (laml-file-name (current-file))
         (proper-laml-file-name (file-name-proper laml-file-name))
         (project-name proper-laml-file-name) 
         (temp-midi-source-path (concat (current-directory) "midi-temp/" proper-laml-file-name "." "mid"))
         (temp-laml-target-path (concat (current-directory) "midi-temp/" "temp" "." "laml"))
         (processing-mode (if new-processing-mode new-processing-mode (find-processing-mode-of-current-buffer)))
         (counter-transposition (if explicit-counter-transposition explicit-counter-transposition (find-counter-transposition-of-current-buffer)))
         (existing-fixed-part-name (find-fixed-part-of-first-track-of-current-buffer))
        )

    (if (file-exists-p temp-laml-target-path) (delete-file temp-laml-target-path))  ; should not exist under normal circumstances
    (copy-file (concat laml-dir "styles/xml-in-laml/midi/" "templates/error-mid.tpl") temp-laml-target-path t)  ; rewritten if everything is OK.

    ; to avoid getting a wrong midi file later on:
    (if (file-exists-p temp-midi-source-path) (delete-file temp-midi-source-path))

    (message "Generating MIDI file from LAML file...")

    ; Process with eliminated target-format
    (let ((buf (current-buffer))
          (target-format (target-format-of-current-buffer)))

      ; Temporarily eliminate target-format form buffer - and the MidiTrack fixed-part if requested
      (if target-format (eliminate-target-format-of-current-buffer))
      (if (and keep-zoom existing-fixed-part-name) (eliminate-fixed-part-name))
      (laml-sync-process-current-buffer) ; makes a midi file, in path temp-midi-source-path, in the midi-temp directory.
      (set-buffer buf)
      (if (and keep-zoom existing-fixed-part-name) (insert-fixed-part-name existing-fixed-part-name))
      (if target-format (insert-target-format-in-current-buffer target-format))

      (push-current-midl-buffer project-name cur-buf)
      (message "Reprocessing  MIDI file in order to produce a fresh LAML (midl) file...")
      (sync-midi-file-to-laml-file processing-mode counter-transposition nil temp-midi-source-path temp-laml-target-path)
      (set-buffer cur-buf)
      (erase-buffer)
      (insert-file-contents temp-laml-target-path)
      (buffer-reset-undo)
      (if target-format (insert-target-format-in-current-buffer target-format))   ; insert original target-format in pushed buffer
      (if (and keep-zoom existing-fixed-part-name) (insert-fixed-part-name existing-fixed-part-name))
      (if (not keep-zoom) (add-comment-to-midl-file "Unzoomed"))
      (delete-file temp-laml-target-path)
      (let ((buf (get-buffer "temp-processing-program.laml")))
        (if buf (kill-buffer buf))))
    
    (message "An evaluation of the buffer has been pushed. New stack height (including this buffer): %s."   (+ 1 (length midl-source-file-stack)))
))

; A variant of the function process-laml-midi-file-save-and-push which provides for 'zoom in'.
(defun process-laml-file-save-zoom-and-push-midi-kind-abstime (lambda-exp-filter-string)
  (let* ((cur-buf (current-buffer))
         (laml-file-name (current-file))
         (proper-laml-file-name (file-name-proper laml-file-name))
         (project-name proper-laml-file-name) 
         (temp-midl-file-name-proper (concat proper-laml-file-name "-" "xyz-copy"))
         (temp-midl-copy-source-path (concat (current-directory) temp-midl-file-name-proper  "." "midl"))
         (temp-midi-source-path (concat (current-directory) "midi-temp/" temp-midl-file-name-proper "." "mid"))   ; Notice temp-midl-file-name-proper 
         (temp-laml-target-path (concat (current-directory) "midi-temp/" "temp" "." "laml"))
        )

    (if (file-exists-p temp-laml-target-path) (delete-file temp-laml-target-path))  ; should not exist under normal circumstances
    (copy-file (concat laml-dir "styles/xml-in-laml/midi/" "templates/error-mid.tpl") temp-laml-target-path t)  ; rewritten if everything is OK.

    ; to avoid getting a wrong midi file later on:
    (if (file-exists-p temp-midi-source-path) (delete-file temp-midi-source-path))

    (message "Generating MIDI file from LAML file...")

    ; Process with eliminated target-format
    (let* ((buf (current-buffer))
           (copy-buf (get-buffer-create (concat proper-laml-file-name "-temp-copy")))
           (fixed-part-abs-dir (concat (current-directory) project-name "/" "fixed-parts" "/"))
           (next-fixed-part-number (calculate-next-fixed-part-number fixed-part-abs-dir))
           (next-fixed-part-name (concat "fp" (as-string next-fixed-part-number)))
           (existing-fixed-part-name (find-fixed-part-of-first-track-of-current-buffer))
           (counter-transposition-str (find-counter-transposition-of-current-buffer))
          )

      ; Temporarily process a copy of buf with an appropriate filtering in the first track.
      (set-buffer copy-buf)
      (insert-buffer buf)
      (midi-laml-mode)       ; needed for proper navigation with forward-sexp
      (if existing-fixed-part-name (eliminate-fixed-part-name))                     ; The fixed part is taken into account by the inserted filter-messages-keep-residual-form form
      (insert-filter-messages-keep-residual-form "filter-messages-keep-residual-and-accumulate!" lambda-exp-filter-string fixed-part-abs-dir next-fixed-part-name existing-fixed-part-name)
      (set-visited-file-name temp-midl-copy-source-path)
      (save-buffer temp-midl-copy-source-path)
      (ensure-directory-existence (concat (current-directory) project-name "/") "fixed-parts/")
      (laml-sync-process-current-buffer)        ; makes a midi file, in path temp-midi-source-path, in the midi-temp directory.
      (delete-file temp-midl-copy-source-path)
      (kill-buffer copy-buf)

      ; Reprocessing and inserting fresh contents into the buffer.
      (set-buffer buf)  ; returns to the original buffer, on which the processing occurs.
      (push-current-midl-buffer project-name cur-buf)
      (message "Reprocessing  MIDI file in order to produce a fresh LAML (midl) file...")
      (sync-midi-file-to-laml-file "absTime" counter-transposition-str nil temp-midi-source-path temp-laml-target-path)
      (set-buffer cur-buf)
      (erase-buffer)
      (insert-file-contents temp-laml-target-path)
      (buffer-reset-undo)

      ; insert fixed-part attribute
      (insert-fixed-part-attribute-on-first-track next-fixed-part-name)
      (add-comment-to-midl-file (concat "The result of zooming with the predicate" "  " lambda-exp-filter-string))

      (delete-file temp-laml-target-path)
      (let ((buf (get-buffer "temp-processing-program.laml")))
        (if buf (kill-buffer buf))))
    
    (message "An evaluation of the buffer has been pushed. New stack height (including this buffer): %s."   (+ 1 (length midl-source-file-stack)))
))

; Another variant of the function process-laml-midi-file-save-and-push which provides for total zoom out followed by a zoom in. Good for shifting of channel. 
; Only very little difference from process-laml-file-save-zoom-and-push-midi-kind-abstime.
(defun process-laml-file-save-zoom-out-and-zoom-and-push-midi-kind-abstime (lambda-exp-filter-string channel)
  (let* ((cur-buf (current-buffer))
         (laml-file-name (current-file))
         (proper-laml-file-name (file-name-proper laml-file-name))
         (project-name proper-laml-file-name) 
         (temp-midl-file-name-proper (concat proper-laml-file-name "-" "xyz-copy"))
         (temp-midl-copy-source-path (concat (current-directory) temp-midl-file-name-proper  "." "midl"))
         (temp-midi-source-path (concat (current-directory) "midi-temp/" temp-midl-file-name-proper "." "mid"))   ; Notice temp-midl-file-name-proper 
         (temp-laml-target-path (concat (current-directory) "midi-temp/" "temp" "." "laml"))
        )

    (if (file-exists-p temp-laml-target-path) (delete-file temp-laml-target-path))  ; should not exist under normal circumstances
    (copy-file (concat laml-dir "styles/xml-in-laml/midi/" "templates/error-mid.tpl") temp-laml-target-path t)  ; rewritten if everything is OK.

    ; to avoid getting a wrong midi file later on:
    (if (file-exists-p temp-midi-source-path) (delete-file temp-midi-source-path))

    (message "Generating MIDI file from LAML file...")

    ; Process with eliminated target-format
    (let* ((buf (current-buffer))
           (copy-buf (get-buffer-create (concat proper-laml-file-name "-temp-copy")))
           (fixed-part-abs-dir (concat (current-directory) project-name "/" "fixed-parts" "/"))
           (next-fixed-part-number (calculate-next-fixed-part-number fixed-part-abs-dir))
           (next-fixed-part-name (concat "fp" (as-string next-fixed-part-number)))
           (existing-fixed-part-name (find-fixed-part-of-first-track-of-current-buffer))
           (counter-transposition-str (find-counter-transposition-of-current-buffer))
          )

      ; Temporarily process a copy of buf with an appropriate filtering in the first track.
      (set-buffer copy-buf)
      (insert-buffer buf)
      (midi-laml-mode)       ; needed for proper navigation with forward-sexp
      (if existing-fixed-part-name (eliminate-fixed-part-name))                     ; The fixed part is taken into account by the inserted filter-messages-keep-residual-form form
      (insert-filter-messages-keep-residual-form "filter-messages-keep-residual-and-reprocess!" lambda-exp-filter-string fixed-part-abs-dir next-fixed-part-name existing-fixed-part-name)  ; !!
      (set-visited-file-name temp-midl-copy-source-path)
      (save-buffer temp-midl-copy-source-path)
      (ensure-directory-existence (concat (current-directory) project-name "/") "fixed-parts/")
      (laml-sync-process-current-buffer)        ; makes a midi file, in path temp-midi-source-path, in the midi-temp directory.
      (delete-file temp-midl-copy-source-path)
      (kill-buffer copy-buf)

      ; Reprocessing and inserting fresh contents into the buffer.
      (set-buffer buf)  ; returns to the original buffer, on which the processing occurs.
      (push-current-midl-buffer project-name cur-buf)
      (message "Reprocessing  MIDI file in order to produce a fresh LAML (midl) file...")
      (sync-midi-file-to-laml-file "absTime" counter-transposition-str nil temp-midi-source-path temp-laml-target-path)
      (set-buffer cur-buf)
      (erase-buffer)
      (insert-file-contents temp-laml-target-path)
      (buffer-reset-undo)

      ; insert fixed-part attribute
      (insert-fixed-part-attribute-on-first-track next-fixed-part-name)
      (if existing-fixed-part-name
          (add-comment-to-midl-file (concat (format "The result of switching to channel %d after a total unzoom" channel)))
          (add-comment-to-midl-file (concat (format "The result of switching to channel %d" channel))))

      (delete-file temp-laml-target-path)
      (let ((buf (get-buffer "temp-processing-program.laml")))
        (if buf (kill-buffer buf))))
    
    (message "An evaluation of the buffer has been pushed. New stack height (including this buffer): %s."   (+ 1 (length midl-source-file-stack)))
))

(defun insert-filter-messages-keep-residual-form (scheme-filter-fn-name lambda-exp-filter-string fixed-part-abs-dir next-fixed-part-name existing-fixed-part-name)
  (goto-char (point-min))
  (let ((search-res (search-forward "(MidiTrack" nil t)))
    (if search-res
        (let ((p0 (point)))

           ; Insert end parenthesis at end of MidiTrack:
          (backward-up-list 1)
          (forward-sexp 1) (backward-char 1) (insert ")")

          (goto-char p0)
          (while (looking-at-white-space (point)) (forward-char 1))

          ; Now just after MidiTrack name:
          ; proceed through the attributes of MidiTrack:
          (while (looking-at-chars (point) (list ?'))
            (forward-sexp 2)
            (while (looking-at-white-space (point)) (forward-char 1)))

          (insert (format "(%s %s %s %s " 
                          scheme-filter-fn-name
                          lambda-exp-filter-string
                          (concat "\"" fixed-part-abs-dir next-fixed-part-name "\"")
                          (if existing-fixed-part-name (concat "\"" fixed-part-abs-dir existing-fixed-part-name "\"") "#f")))
          )
      (error "insert-filter-messages-keep-residual-form: Cannot find a MidiTrack form"))))


(defun calculate-next-fixed-part-number (fixed-part-abs-dir)
  "Return the next fp<n> file number in the absolute directory path fixed-part-abs-dir"
  (let* ((dir-lst (eliminate-dot-and-dot-dot (directory-files fixed-part-abs-dir)))
         (fp-dir-lst (filter (lambda (fn) (and (>= (length fn) 2) (equal "fp" (substring fn 0 2)))) dir-lst)))
    (+ (length fp-dir-lst) 1)))

(defun insert-fixed-part-attribute-on-first-track (fixed-part-name)
  (goto-char (point-min))
  (let ((search-res (search-forward "(MidiTrack" nil t)))
    (if search-res
        (insert (format " 'fixed-part \"%s\" "fixed-part-name))
        (error "insert-fixed-part-attribute-on-first-track: Cannot find a MidiTrack"))))

(defun eliminate-fixed-part-name ()
  (goto-char (point-min))
  (let ((search-res (search-forward "(MidiTrack" nil t)))
     (if search-res      
         (progn
            (backward-up-list 1)
            (forward-sexp 1)
            (let ((pn (point)))
              (forward-sexp -1)
              (let ((p1 (search-forward "'fixed-part" pn t)))
                 (backward-sexp 1) ; now just in front of 'fixed-part
                 (kill-sexp 2))))
         (error "eliminate-fixed-part-name: Cannot find MidiTrack form"))))

(defun insert-fixed-part-name (fixed-part-name)
  (goto-char (point-min))
  (let ((search-res (search-forward "(MidiTrack" nil t)))
     (if search-res      
         (insert (format " 'fixed-part \"%s\" " fixed-part-name))
         (error "insert-fixed-part-name: Cannot find MidiTrack form"))))



(defun process-laml-midi-file-save-and-push-via-ast ()
  (let* ((cur-buf (current-buffer))
         (laml-file-name (current-file))
         (proper-laml-file-name (file-name-proper laml-file-name))
         (project-name proper-laml-file-name) 
         (temp-source-path (concat (current-directory) "midi-temp/" proper-laml-file-name "." "src"))
        )

    (copy-file (concat laml-dir "styles/xml-in-laml/midi/" "templates/error-mid.tpl") temp-source-path t)  ; rewritten if everything is OK.
    (message "Generating source file from LAML file...")

    (let ((buf (current-buffer)))
          
      ; Notice use of LAML program parameter - causes installation of alternative action procedures: 
      (laml-sync-process-current-buffer "laml-source") ; makes and saves a laml source file file, in path temp-source-path, in the midi-temp directory.
      (push-current-midl-buffer project-name cur-buf)
      (set-buffer cur-buf)
      (erase-buffer)
      (insert-file-contents temp-source-path)
      (buffer-reset-undo)

      ; Clean up:
      (delete-file temp-source-path)
    )
    
    (message "An evaluation of the buffer has been pushed. New stack height (including this buffer): %s."   (+ 1 (length midl-source-file-stack)))
))

(defun split-and-replicate-midl-file-by-channel (ch)
  (interactive "nChannel number (1..16): ") 

  (ensure-directory-existence (current-directory) "midi-temp/")
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (get-rid-of-ghost-stack)
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (let* ((kind (kind-of-midl-file-in-current-buffer)))
    (cond ((eq 'midi kind)     
             (let ((format-0-file-p (current-buffer-is-standard-midifile-of-format 0))   ; bool
                   (mode (mode-of-standard-midi-file-current-buffer))) ; symbol
               (if (and format-0-file-p (eq mode 'absTime))
                      (split-and-replicate-midl-format0-abstime-file-by-channel ch))))
          ((eq 'multi-pad kind) (error "Cannot split a multipad file"))
          ((eq 'style kind) (error "Cannot split a style file"))
          (t (error (concat "split-and-replicate-midl-file-by-channel: Unknown kind of midl file" 
                            (symbol-to-string kind)))))))

(defun split-and-replicate-midl-format0-abstime-file-by-channel (ch)
  "Push two new entries on the stack. One disregard channel ch, and the other focussing on channel ch"
  (let* ((cur-buf (current-buffer))
         (laml-file-name (current-file))
         (proper-laml-file-name (file-name-proper laml-file-name))
         (project-name proper-laml-file-name)
         (temp-midl-file-name "_temp")   ; The name of temporary file
         (temp-midl-file-path  (concat (current-directory) temp-midl-file-name ".midl"))   ; The path to the temporary file in the current directory
         (temp-midi-source-path (concat (current-directory) "midi-temp/" temp-midl-file-name "." "mid"))
         (temp-laml-target-path (concat (current-directory) "midi-temp/" "temp" "." "laml"))
         (processing-mode "absTime")
         (counter-transp (find-counter-transposition-of-current-buffer))
         (current-buffer-contents (buffer-substring-no-properties (point-min) (point-max)))
        )

    (if (file-exists-p temp-laml-target-path) (delete-file temp-laml-target-path)) ; should not exist under normal circumstances
    (copy-file (concat laml-dir "styles/xml-in-laml/midi/" "templates/error-mid.tpl") temp-laml-target-path t) ; rewritten if everything is OK.

                                        ; to avoid getting a wrong midi file later on:
    (if (file-exists-p temp-midi-source-path) (delete-file temp-midi-source-path))

    (message "Generating MIDI files from LAML file...")
                                        ; Process with eliminated target-format

    ; Make and push split with all but channel ch:    
    (if (file-exists-p temp-midl-file-path) (delete-file temp-midl-file-path))
    (make-other-channels-file-from-buffer-string current-buffer-contents ch temp-midl-file-path)  ; makes a source with appropriate channel filtering in temp-midl-file-path
    (message "Pushing file with all channels but %s" ch)
    (laml-sync-process-file temp-midl-file-path 'silent)                   ; produces a midi file in temp-midi-source-path
    (push-current-midl-buffer project-name cur-buf)
    (sync-midi-file-to-laml-file processing-mode counter-transp nil 
                                 temp-midi-source-path temp-laml-target-path)   ; parse to laml, and place laml file in temp-laml-target-path
    (set-buffer cur-buf)
    (erase-buffer)
    (insert (format ";;;; SPLIT: All channels but channel %s" ch)) (insert CR)
    (insert-file-contents temp-laml-target-path)
    (buffer-reset-undo)
    (delete-file temp-laml-target-path)
    (delete-file temp-midl-file-path)

    ; Make and push split with channel ch:
    (make-channel-file-from-buffer-string current-buffer-contents ch temp-midl-file-path)    ; makes a source with appropriate channel filtering in temp-midl-file-path
    (message "Pushing file with channel %s only" ch)
    (laml-sync-process-file temp-midl-file-path 'silent)              ; produces a midi file in temp-midi-source-path
    (push-current-midl-buffer project-name cur-buf)
    (sync-midi-file-to-laml-file processing-mode counter-transp nil 
                                 temp-midi-source-path temp-laml-target-path)   ; parse to laml, and place laml file in temp-laml-target-path
    (set-buffer cur-buf)
    (erase-buffer)
    (insert (format ";;;; SPLIT: Only channel %s" ch)) (insert CR)
    (insert-file-contents temp-laml-target-path)
    (buffer-reset-undo)
    (delete-file temp-laml-target-path)
    (delete-file temp-midl-file-path)

    (message "The buffer has been split by channel %s. Two new entries have been pushed. New stack height (including this buffer): %s."  
             ch  (+ 1 (length midl-source-file-stack)))
    ))

(defun make-channel-file-from-buffer-string (buffer-string ch target-path)
  "Make a file in target-path with filtering to channel ch. Return t if successful, else nil"
  (let* ((temp-buffer-name " midl-split")
         (buf (get-buffer-create temp-buffer-name))
        )
    (set-buffer buf)
    (erase-buffer)
    (insert buffer-string)

    (goto-char (point-min))
    (let ((res1 (search-forward "(StandardMidiFile" nil t)))
      (if res1
          (let ((res2 (search-forward "(MidiTrack" nil t)))
            (if res2
                (progn
                  (search-backward "(MidiTrack" nil t)
                  (forward-sexp 1)
                  (insert ")")
                  (goto-char res2)
                  (insert " ")
;                  (insert (format "(filter-messages (lambda (x) (and (ast? x) (or (non-channel-message? x) (= %s (midi 'channel x)))))" ch))
                  (insert (format "(filter-messages (lambda (x) (and (ast? x) (or (Meta? x) (equal? %s (midi 'channel x)))))" ch))

                  (if (file-exists-p target-path) (delete-file target-path))
                  (write-file target-path)
                  (kill-buffer buf)
                  t)
              nil))
            nil))))

(defun make-other-channels-file-from-buffer-string (buffer-string ch target-path)
  "Make a file in target-path with filtering to all other channels than ch. Return t if successful, else nil"
  (let* ((temp-buffer-name " midl-split")
         (buf (get-buffer-create temp-buffer-name))
        )
    (set-buffer buf)
    (erase-buffer)
    (insert buffer-string)

    (goto-char (point-min))
    (let ((res1 (search-forward "(StandardMidiFile" nil t)))
      (if res1
          (let ((res2 (search-forward "(MidiTrack" nil t)))
            (if res2
                (progn
                  (search-backward "(MidiTrack" nil t)
                  (forward-sexp 1)
                  (insert ")")
                  (goto-char res2)
                  (insert " ")
                  (insert (format "(filter-messages (lambda (x) (and (ast? x) (or (non-channel-message? x) (not (= %s (midi 'channel x))))))" ch))

                  (if (file-exists-p target-path) (delete-file target-path))
                  (write-file target-path)
                  (kill-buffer buf)
                  t)
              nil))
            nil))))
  
(defun merge-two-top-stack-entries ()
  "Merge the two stack entries which both are assumed to be the result of a split"
  (interactive)
  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (if (not (null midl-source-file-stack))
      (let* ((file-path (car midl-source-file-stack)))
        (if (is-split-situation file-path (current-buffer))
            (let* ((cur-buf (current-buffer))
                   (laml-file-name (current-file))
                   (proper-laml-file-name (file-name-proper laml-file-name))
                   (project-name proper-laml-file-name) 
                   (temp-midi-source-path (concat (current-directory) "midi-temp/" proper-laml-file-name "." "mid"))
                   (temp-midi-source-path-1 (concat (current-directory) "midi-temp/" "_temp-1" "." "mid"))
                   (temp-midi-source-path-2 (concat (current-directory) "midi-temp/" "_temp-2" "." "mid"))
                   (processing-mode (find-processing-mode-of-current-buffer))
                   (counter-transposition (find-counter-transposition-of-current-buffer))
                   (temp-laml-target-path (concat (current-directory) "midi-temp/" "temp" "." "laml"))
                  )
              (get-rid-of-ghost-stack)

              ; Process top of stack - the slave (with events belonging to a single channel):
              (laml-sync-process-current-buffer) ; makes a midi file, in path temp-midi-source-path, in the midi-temp directory.
              (message "Make midi file from the top of the stack...")
              (rename-file temp-midi-source-path temp-midi-source-path-1 t)

              ; Process entry just under top - the master:
              (set-buffer cur-buf)
              (erase-buffer)
              (insert-file-contents file-path)
              (laml-sync-process-current-buffer)  ; makes a midi file, in path temp-midi-source-path, in the midi-temp directory.
              (message "Make midi file from the entry just below the top of the stack...")

              (merge-midi-files temp-midi-source-path-1 temp-midi-source-path temp-midi-source-path-2) ; merges two first midi files to a midi file in temp-midi-source-path-2
              (message "Merging two midi files...")

              (sync-midi-file-to-laml-file processing-mode counter-transposition nil temp-midi-source-path-2 temp-laml-target-path)
              (message "Making resulting Midi LAML file...")
 
              (set-buffer cur-buf)
              (erase-buffer)
              (insert-file-contents temp-laml-target-path)
              (buffer-reset-undo)
              (save-buffer)

              ; Delete file corresponding to top of stack:
              (delete-file file-path)
              (setq midl-source-file-stack (cdr midl-source-file-stack))

              ; Cleaning up:
              (delete-file temp-midi-source-path-1)
              (delete-file temp-midi-source-path)
              (delete-file temp-midi-source-path-2)

              (message (concat "After merge: %d element(s) + %d ghost(s). " (comment-of-midl-file))
                       (+ (length midl-source-file-stack) 1) (length midl-source-file-ghost-stack)))
          (message "The top and the entry just below the top seem NOT to be a result of a split"))
      )
      (message (concat "The midl source file stack only contains the basis buffer, which cannot be popped. Nothing done"))))

(defun is-split-situation (master-file-path slave-buffer) 
  t)

(defun merge-midi-files (slave-path master-path output-path)
   (make-a-file-from-laml-template
       "temp-processing-program.laml"
       laml-temp-file-path
       "merge-master-and-slave-to-single-midi-file"
       'midi-laml-mode
       (list
        (list "FILE-PATH-1" master-path)
        (list "FILE-PATH-2" slave-path)
        (list "FILE-PATH-3" output-path)
       )
       (concat laml-dir "styles/xml-in-laml/midi/" "templates/"))  
    (laml-sync-process-file (concat laml-temp-file-path "temp-processing-program.laml") 'silent))


(defun target-format-of-current-buffer ()
 (save-excursion
   (goto-char (point-min))
   (let* ((target-format-reg-exp 
             (regular-expression  '(concat "'target-format" (one-or-more (char-set tab newline space return)) (concat "\"" (char-set "0" "1" "2") "\"") )))
          (target-format-match (re-search-forward target-format-reg-exp nil t)))
     (if target-format-match
         (match-string-no-properties 0)
         nil))))

(defun eliminate-target-format-of-current-buffer()
  (save-excursion
   (goto-char (point-min))
   (let* ((target-format-reg-exp 
             (regular-expression  '(concat "'target-format" (one-or-more (char-set tab newline space return)) (concat "\"" (char-set "0" "1" "2") "\"") )))
          (target-format-match (re-search-forward target-format-reg-exp nil t)))
     (if target-format-match
         (replace-match "")
         nil))))

(defun insert-target-format-in-current-buffer (target-format-string)
  (save-excursion
   (goto-char (point-min))
   (let* ((format-reg-exp 
             (regular-expression  '(concat "'format" (one-or-more (char-set tab newline space return)) (concat "\"" (char-set "0" "1" "2") "\"") )))
          (format-match (re-search-forward format-reg-exp nil t)))
     (if format-match
         (insert (concat " " target-format-string " ")) 
         (error "Could not locate a format attribute of current midl buffer.")))))

; If provided, explicit-counter-transposition is a string
; If provided, new-processing-mode is either the the string "absTime" or "deltaTime"
(defun process-laml-multi-pad-file-save-and-push (&optional explicit-counter-transposition new-processing-mode)
  (let* ((cur-buf (current-buffer))
         (laml-file-name (current-file))
         (proper-laml-file-name (file-name-proper laml-file-name))
         (project-name (file-name-proper proper-laml-file-name))  ; eliminate possible double extension
         (temp-midi-source-path-0 (concat (current-directory) "midi-temp/" proper-laml-file-name "." "mid"))
         (temp-midi-source-path (concat (current-directory) "midi-temp/" proper-laml-file-name "." "pad"))
         (temp-laml-target-path (concat (current-directory) "midi-temp/" "temp" "." "laml"))
         (processing-mode (if new-processing-mode new-processing-mode (find-processing-mode-of-current-buffer)))
         (counter-transposition (if explicit-counter-transposition explicit-counter-transposition (find-counter-transposition-of-current-buffer)))
        )
    (if (file-exists-p temp-laml-target-path) (delete-file temp-laml-target-path))  ; should not exist under normal circumstances
    (copy-file (concat laml-dir "styles/xml-in-laml/midi/" "templates/error-pad.tpl") temp-laml-target-path)  ; rewritten if everything is OK.

    ; to avoid getting a wrong midi file later on:
    (if (file-exists-p temp-midi-source-path) (delete-file temp-midi-source-path))

    (message "Generating MIDI multi-pad file from LAML file...")
    (laml-sync-process-current-buffer) ; gives -.mid file
    (rename-file temp-midi-source-path-0 temp-midi-source-path t)   ; now pad name. Important for subsequent midi-to-laml processing.
    (push-current-midl-buffer project-name cur-buf)
    (message "Reprocessing  PAD file in order to produce a fresh LAML (midl) file...")
    (sync-midi-file-to-laml-file processing-mode counter-transposition nil temp-midi-source-path temp-laml-target-path)
    (set-buffer cur-buf)
    (erase-buffer)
    (insert-file-contents temp-laml-target-path)
    (buffer-reset-undo)
    (delete-file temp-laml-target-path)
    (let ((buf (get-buffer "temp-processing-program.laml")))
      (if buf (kill-buffer buf)))
    
    (message "An evaluation of the buffer has been pushed. New stack height (including this buffer): %s."   (+ 1 (length midl-source-file-stack)       )) ))

; If provided, explicit-counter-transposition is a string
; If provided, new-processing-mode is either the the string "absTime" or "deltaTime"
(defun process-laml-style-file-save-and-push (&optional new-processing-mode)
  (let* ((cur-buf (current-buffer))
         (laml-file-name (current-file))
         (proper-laml-file-name (file-name-proper laml-file-name))
         (project-name (file-name-proper proper-laml-file-name))  ; eliminate possible double extension
         (temp-midi-source-path-0 (concat (current-directory) "midi-temp/" proper-laml-file-name "." "mid"))
         (temp-midi-source-path (concat (current-directory) "midi-temp/" proper-laml-file-name "." "sty"))
         (temp-laml-target-path (concat (current-directory) "midi-temp/" "temp" "." "laml"))
         (processing-mode (if new-processing-mode new-processing-mode (find-processing-mode-of-current-buffer)))
        )
    (if (file-exists-p temp-laml-target-path) (delete-file temp-laml-target-path))  ; should not exist under normal circumstances
    (copy-file (concat laml-dir "styles/xml-in-laml/midi/" "templates/error-style.tpl") temp-laml-target-path)  ; rewritten if everything is OK.

    ; to avoid getting a wrong midi file later on:
    (if (file-exists-p temp-midi-source-path) (delete-file temp-midi-source-path))

    (message "Generating MIDI style file from LAML file...")
    (laml-sync-process-current-buffer) ; gives -.mid file
    (rename-file temp-midi-source-path-0 temp-midi-source-path t)   ; now sty name. Important for subsequent midi-to-laml processing.
    (push-current-midl-buffer project-name cur-buf)
    (message "Reprocessing STYLE file in order to produce a fresh LAML (midl) file...")
    (sync-midi-file-to-laml-file processing-mode "0" nil temp-midi-source-path temp-laml-target-path)
    (set-buffer cur-buf)
    (erase-buffer)
    (insert-file-contents temp-laml-target-path)
    (buffer-reset-undo)
    (delete-file temp-laml-target-path)
    (let ((buf (get-buffer "temp-processing-program.laml")))
      (if buf (kill-buffer buf)))
    
    (message "An evaluation of the buffer has been pushed. New stack height (including this buffer): %s."   (+ 1 (length midl-source-file-stack)       )) ))

; Alternative version - avoiding backward navigation over entire StandardMidiFile form (or similar form), which may be expensive in case of heavy commenting (piano roll).
; Relies on searching (for regular expression) in stead of list navigation.
; Old version follows this function (commented out of the source).
(defun kind-of-midl-file-in-current-buffer ()
 (save-excursion
  (goto-char (point-min))
  (let ((outer-form-reg-exp (regular-expression '(or "(StandardMidiFile" "(TyrosMultiPad" "(TyrosStyle"))))
    (if (re-search-forward outer-form-reg-exp nil t)
        (progn
          (re-search-backward outer-form-reg-exp nil t)
          (cond ((looking-at "(StandardMidiFile") 'midi)
                ((looking-at "(TyrosMultiPad") 'multi-pad)
                ((looking-at "(TyrosStyle") 'style)
                (t nil)))
        nil))))

; (defun kind-of-midl-file-in-current-buffer ()
;   "By looking at the midi laml source in the current buffer, determine its kind.
; Return midi for StandardMidiFiles, multi-pad for Tyros Multipads, and style for Tyros styles. Returns a symbol.
; Return nil for unknown type."
;   (save-excursion
;     (goto-char (point-max))
;  
;     ; Compensate for registered point at the end of the buffer:
;     (beginning-of-line 1)
;     (if (looking-at ";;; ")
;         (backward-char 1)
;         (goto-char (point-max)))
;  
;     (backward-sexp 1)
;     (kind-of-midl-file-in-current-buffer-real)))


; (defun kind-of-midl-file-in-current-buffer-real()
;  (save-excursion
;   (condition-case nil
;       (cond ((looking-at "StandardMidiFile") 'midi)
;             ((looking-at "TyrosMultiPad") 'multi-pad)
;             ((looking-at "TyrosStyle") 'style)
;             (t (progn
;                  (down-list 1)
;                  (kind-of-midl-file-in-current-buffer-real))))
;     (error nil))))

; A somewhat risky variant of the version above. 
; Introduced to be able to recognize a StandardMidiFile through a function, in which the standard midi file AST is not the first argument.
(defun kind-of-midl-file-in-current-buffer-real ()
 (save-excursion
  (condition-case nil
      (cond ((looking-at "StandardMidiFile") 'midi)
            ((looking-at "TyrosMultiPad") 'multi-pad)
            ((looking-at "TyrosStyle") 'style)
            (t (progn
                 (down-list 1)
                 (kind-of-midl-file-in-current-buffer-real))))
    (error (backward-up-list 1)  ; look forward for one of the kind names
           (forward-sexp 1)
           (kind-of-midl-file-in-current-buffer-real)))))

              

; --------------------------------------------------------------------------------------------------------------
; Base lining, project directory, and midi stack

; IMPORTANT INVARIANT: The baseline files must always be numbered from 1 to n.
; No holes can exist among baseline files. Do not manually delete a baseline file.

(define-derived-mode 
        baseline-browser-mode fundamental-mode "BaselineBrowser" 
  "A line-oriented browser of baselines of the current MIDI LAML project."

 (make-variable-buffer-local 'baseline-project-name)
 (make-variable-buffer-local 'baseline-directory)

)  

(defvar baseline-browser-mode-map (make-sparse-keymap "BaselineBrowser"))
(define-key baseline-browser-mode-map "p" 'bb-push-baseline-on-midel-stack)
(define-key baseline-browser-mode-map "g" 'refresh-baseline-browser)
(define-key baseline-browser-mode-map "e" 'edit-baseline-explanation)
(define-key baseline-browser-mode-map [down] 'next-line-and-highlight-line)   
(define-key baseline-browser-mode-map [up] 'previous-line-and-highlight-line)

(laml-define-key baseline-browser-mode-map [menu-bar baseline-browser]
    (cons "Baseline Browser" (make-sparse-keymap "Baseline Browser")))

(laml-define-key baseline-browser-mode-map [menu-bar baseline-browser refresh-baseline-browser]
      '("Refresh" . refresh-baseline-browser))

(laml-define-key baseline-browser-mode-map [menu-bar baseline-browser edit-baseline-explanation]
      '("Edit explanation" . edit-baseline-explanation))

(laml-define-key baseline-browser-mode-map [menu-bar baseline-browser bb-push-baseline-on-midel-stack]
      '("Push baseline - undo pops before" . bb-push-baseline-on-midel-stack))


(defun create-baseline-of-current-midl-source-file (explanation)
  "Create a new baseline (snapshot) of the current midl source file and save it permanently in the implicitly determined project directory."
  (interactive (list (read-from-minibuffer "Role of baseline: " (comment-of-midl-file))))
  (let ((next-baseline-number (create-baseline-of-current-midl-source-file-silent explanation)))
    (message "The file baseline-%d bas been created in the project directory. Never delete it again!"  next-baseline-number)))

(defun create-baseline-of-current-midl-source-file-silent (explanation)
 "Create a baseline file of the current midl source file with explanation and return the base line number."
 (let ((current-project-name (file-name-proper (file-name-proper (current-file)))))
   (ensure-directory-existence (current-directory) (concat current-project-name "/") )
   (let* ((existing-base-line-files (find-existing-baseline-files (current-directory) current-project-name))
          (next-baseline-number (+ (length existing-base-line-files) 1)))
     (let ((target-path (concat (current-directory) current-project-name "/" "baseline" "-" 
                                (number-to-string next-baseline-number) "." "midl"))
           (target-explanation-path
            (concat (current-directory) current-project-name "/" "baseline" "-" 
                    (number-to-string next-baseline-number) "-" "explanation" )))
       (write-region (point-min) (point-max) target-path nil 0)
       (if (> (length explanation) 0)
           (write-file-with-initial-contents explanation target-explanation-path)))
     next-baseline-number)))
  

(defun write-file-with-initial-contents (str path)
  "Create a file with str as initial contents. The path of the file is path."
  (let ((new-buffer (create-file-buffer path)))
    (set-buffer new-buffer)
    (erase-buffer)
    (insert str)
    (write-file path)
    (kill-buffer new-buffer)))

(defun find-existing-baseline-files (current-dir project-name)
  (filter 
   (function (lambda (x) 
               (let ((fnp (file-name-proper (file-name-nondirectory x)))
                     (fne (file-name-extension (file-name-nondirectory x))))
                 (and 
                  (not (or (equal fnp ".") (equal fnp "..")))
                  (> (length fnp) 8) 
                  (equal "baseline" (substring fnp 0 8))
                  (equal "midl" fne)))))
   (directory-files (concat current-dir project-name "/"))))

(defun push-baseline-on-midl-stack (baseline-number)
  "Push an existing baseline file onto the midl stack. First, undo all pops."
  (interactive
    (let* ((current-project-name (file-name-proper (file-name-proper (current-file))))
           (count (length (find-existing-baseline-files (current-directory) current-project-name))))
     (list (cond ((= count 1) ; only one possibility
                    (string-to-number
                     (read-from-minibuffer 
                      "There is only one baseline: " "1")))
                  ((> count 1)
                     (string-to-number
                      (read-from-minibuffer 
                       (format "Which baseline entry between 1 and %d do you wish to push: " 
                               count ))))
                  (t 0)))))
  (if (> baseline-number 0)
      (do-push-baseline-on-midle-stack baseline-number)
      (message "There are no base lines. Nothing done.")))

(defun do-push-baseline-on-midle-stack (baseline-number &optional explanation-of-baseline) 
  "With the midl file as the current buffer, push baseline file with baseline-number onto the midel stack. 
Unto all pops before pushing"

  ; Do as many undo-pops as we have elements on the ghost stack:
  (while (not (null midl-source-file-ghost-stack))
    (undo-pop-midl-stack))
       
  (let* ((current-project-name (file-name-proper (file-name-proper (current-file))))
         (baseline-source-path 
          (concat (current-directory) current-project-name "/" "baseline" "-" (number-to-string baseline-number) ".midl"))
         )
  (if (is-current-buffer-blank) 
      (if (and (null midl-source-file-stack) (null midl-source-file-ghost-stack))
          (progn
            ; A way to initiate an empty MIDL LAML project:
            ; The user have just made an empty (blank) midl file. Next, the user pushes an existing Midi file.
            (insert-file-contents baseline-source-path)
            (buffer-reset-undo)
            (add-comment-to-midl-file (concat "Pushed from baseline" " " (as-string baseline-number) ". " (if explanation-of-baseline explanation-of-baseline "") ))
            (message "Baseline file number %d has been pushed as the initial contents on the empty Midl stack" baseline-number))
          (message "You must empty the Midl stack, using for instance M-x reset-midl-stack or M-x reset-and-empty-midl-stack before you can push into an emtpy buffer.") 
      )
      (if (file-exists-p baseline-source-path)
          (progn
            (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
            (push-current-midl-buffer current-project-name (current-buffer))
            (erase-buffer)
            (insert-file-contents baseline-source-path)
            (buffer-reset-undo)
            (add-comment-to-midl-file (concat "Pushed from baseline" " " (as-string baseline-number) ". " (if explanation-of-baseline explanation-of-baseline "") ))
            (message "Baseline file number %d has been pushed on the midl stack" baseline-number))
        (message "The file baseline-%d.midl does not exist in the project directory %s." baseline-number current-project-name)))))


(defun undo-all-pops-of-midl-stack ()
  (interactive)
  (while (not (null midl-source-file-ghost-stack))
         (undo-pop-midl-stack))
  (message "Now at top of stacks. All pop operations have been undone."))

(defun do-all-pops-of-midl-stack ()
  (interactive)
  (while (not (null midl-source-file-stack))
         (pop-and-reestablish-midl-file))
  (message "Now at beginning of stack. The stack has been popped as much as possible."))

(defun baseline-present-info ()
  (interactive)
  (let ((info-buffer (if (get-buffer "*BaselineInfo*") (get-buffer "*BaselineInfo*") (generate-new-buffer "*BaselineInfo*")))
        (current-project-name (file-name-proper (file-name-proper (current-file))))
        (current-dir (current-directory))  ; bound before info buffer is created.
       )
    (delete-other-windows)
    (split-window-vertically)
    (set-window-buffer (other-window 1) info-buffer)
    (set-buffer info-buffer)
    (toggle-read-only -1)
    (erase-buffer)

    (insert "BASELINE FILE INFO.") (insert CR) (insert CR)
    (insert "Baseline files are snapshots of midl files, which have been saved permanently in the project directory.") (insert CR) 
    (insert (concat "The project directory is located in: " current-dir current-project-name "/" )) (insert CR) (insert CR) 

    (let ((base-line-files (find-existing-baseline-files current-dir current-project-name)))
      (cond ((and (file-exists-p (concat current-dir current-project-name "/")) (not (null base-line-files)))
               (mapcar 
                (function
                 (lambda (blf)
                   (insert blf) (insert ": ")
                   (let ((expl-path (concat current-dir current-project-name "/" (file-name-proper blf) "-" "explanation"))) 
                     (if (file-exists-p expl-path)
                         (progn (let ((res (insert-file-contents expl-path))) ; point is located before insertion. Therefore forward-char ...
                                  (forward-char (cadr res)))
                                (insert CR))
                       (progn (insert "No explanation available.") (insert CR))))))
                base-line-files ))
            ((and (file-exists-p (concat current-dir current-project-name "/")) (null base-line-files))
              (insert (concat "There are no baseline files in the project directory " current-project-name)))

            (t (insert (concat "There is no directory for the project " current-project-name))))) 

    (toggle-read-only 1)
    (set-buffer-modified-p nil)
    (other-window 1)))

(defun browse-baselines ()
  (interactive)
  (let* ((current-dir (current-directory))  ; bound before info buffer is created.         
         (midl-buf-name (file-name-proper (buffer-name (current-buffer))))
         (browser-buffer-name (concat "baseline-browser-" midl-buf-name))
         (browser-buffer (if (get-buffer browser-buffer-name) (get-buffer browser-buffer-name) (generate-new-buffer browser-buffer-name)))
         (current-project-name (file-name-proper (file-name-proper (current-file))))

         (base-line-files (find-existing-baseline-files current-dir current-project-name))
        )
    (if (and (file-exists-p (concat current-dir current-project-name "/")) (not (null base-line-files)))
        (progn
          (delete-other-windows)
          (split-window-vertically)
          (set-window-buffer (selected-window) browser-buffer)
          (set-buffer browser-buffer)

          (insert-baseline-browser-contents browser-buffer current-project-name current-dir base-line-files) 

          ; adjust size of browser:
           (let* ((lines-max (/ (frame-height) 2))
                  (actual-number-of-lines (length base-line-files))
                  (extra-lines 4)  ; 3 exta lines is too few. They simpley disappear. Set it to 4 or 5.
                  (required-number-of-lines (+ extra-lines actual-number-of-lines))
                  )
             (if (< required-number-of-lines lines-max)
                 (enlarge-window (- required-number-of-lines lines-max))) ; a negative number, thus shrinks the window size
           )
        )
        (progn
           (cond ((not (file-exists-p (concat current-dir current-project-name "/"))) 
                   (message "Cannot browse baselines. No project directory is present."))
                 ((null base-line-files) 
                   (message "Cannot browse baselines. No baselines have been defined."))
                 (else (message "Cannot.")))
        ) 
)))

; (defvar baseline-project-name nil "The name of the project associated with a baseline browser")
; (defvar baseline-directory nil "The full path of the directory associated with a baseline browser")

(defun insert-baseline-browser-contents (browser-buffer current-project-name midl-file-dir base-line-files)
 (set-buffer browser-buffer)
 (toggle-read-only -1)
 (erase-buffer)

 (baseline-browser-mode)

 (setq baseline-project-name current-project-name)
 (setq baseline-directory (concat midl-file-dir current-project-name "/"))

 (mapcar 
  (function
   (lambda (blf)
     (let* ((blf-full-path (concat midl-file-dir current-project-name "/" blf))
            (blf-created-time (nth 5 (file-attributes blf-full-path)))
            )
       (insert blf) (insert ":   ")
       (insert "[")  (insert (midl-time-string blf-created-time))  (insert "]   ")
       (let ((expl-path (concat midl-file-dir current-project-name "/" (file-name-proper blf) "-" "explanation"))) 
         (if (file-exists-p expl-path)
             (progn (let ((res (insert-file-contents expl-path))) ; point is located before insertion. Therefore forward-char ...
                      (forward-char (cadr res)))
                    (insert CR))
           (progn (insert "No explanation available.") (insert CR)))))))
  base-line-files )

  (goto-char (point-min))
  (highlight-this-line)

  (toggle-read-only 1)
  (set-buffer-modified-p nil)
)

(defun refresh-baseline-browser ()
  "From within a baseline browser, refresh the contents of the browser"
  (interactive)
  (insert-baseline-browser-contents 
     (current-buffer)
     baseline-project-name
     (parent-directory baseline-directory)
     (find-existing-baseline-files (parent-directory baseline-directory) baseline-project-name)
  )
)

(defun bb-push-baseline-on-midel-stack ()
  "Initiated from baseline browser buffer"
  (interactive)
  (let* ((n (selected-base-line-number))
         (explanation (get-baseline-explanation n)))
    (other-window 1)
    (do-push-baseline-on-midle-stack n explanation)
    (other-window 1)))

(defun edit-baseline-explanation (new-explanation)
  "From a baseline browser, edit the baseline file explanation."
;  (interactive "sNew explanation: ")
  (interactive (list (read-from-minibuffer "New explanation: " (get-baseline-explanation (selected-base-line-number)))))
  (let* ((n (selected-base-line-number))
         (explanation-path (concat baseline-directory "baseline" "-" (as-string n) "-" "explanation"))
         (buf (current-buffer))
        )
    (if (file-exists-p explanation-path) (delete-file explanation-path))
    (write-file-with-initial-contents new-explanation explanation-path)
    (set-buffer buf)
    (refresh-baseline-browser)
    (goto-line n)
    (highlight-this-line)
  )
)

(defun get-baseline-explanation (base-line-number)
  "Must be called from base-line browser directory, because it relies on a buffer-local variable, namely baseline-directory"
  (read-text-file-if-exists (concat baseline-directory "baseline" "-" (as-string base-line-number) "-" "explanation"))
)

(defun read-text-file-if-exists (file-path)
  (if (file-exists-p file-path)
      (read-text-file file-path)
      ""))

(defun selected-base-line-number()
  "Return the number (an integer) of the currently selected baseline in a baseline browser."
  (save-excursion
    (beginning-of-line 1)
    (while (not (looking-at-chars (point) (list ?-))) (forward-char 1))
    (forward-char 1)
    (let ((p0 (point)))
      (while (not (looking-at-chars (point) (list ?.))) (forward-char 1))
      (as-number (buffer-substring-no-properties p0 (point))))))

(defun offer-baselining-of-current-midl-source-file (prefix-explanation)
  (save-window-excursion
   (let ((baselining (ask-user "Do you want to baseline the MIDL source file?")))
     (if baselining 
         (create-baseline-of-current-midl-source-file-silent 
          (concat "[" prefix-explanation "]" " " (comment-of-midl-file)))))))


; ---------------------------------------------------------------------------------------------------------------------------------------------
; SCORE COMMENT DRAWING.
; In this part of the program, "score comment" refer to traditional sheet musis scores, but also to other possible notations such as piano roll notation.
; Top level of score drawing is either add-score-comments-to-region, do-add-score-comments-of-region-channel and additional similar functions. 

(defvar score-counter-transpostion 0 "Global variable holding the score-counter-transpostion for score comment drawing.")
(make-variable-buffer-local 'score-counter-transpostion)

(defvar note-on-reg-exp (regular-expression '(concat "(" (zero-or-more " ") "NoteOn")))

(defvar score-comment-line-reg-exp (regular-expression '(or "; " "; o" "; <" )))

; Decorate midi messages with sores between the points p and m.
; When called interactively, p and m is supposed to be point and mark.
(defun add-score-comments-to-region (p m &optional mes)
 "Decorate the selected region with score comments. All channels are decorated."
 (interactive "r")
 (display-message-or-buffer "Adding scores to all channels of the selected region...")
 (fix-score-start-column-and-piano-roll-interval) ; assign score-start-column
 (save-excursion
   (let* ((p0 (min p m))
          (pn (max p m)))
     (setq score-counter-transpostion (string-to-number (find-counter-transposition-of-current-buffer)))
     (goto-char pn) (beginning-of-line 1)
     (re-search-backward note-on-reg-exp p0 t) (beginning-of-line 1) ;  bør måske fjernes ...
     (while (and (> (point) 1) (>= (point) p0))
       (message (concat "Progress: " (progress pn p0 (point))))
       (decorate-midi-message-line-with-score t) (beginning-of-line 1)  ; !
       (let ((back-search (re-search-backward note-on-reg-exp p0 t)))
         (if back-search
             (beginning-of-line 1)
             (goto-char (- p0 1)))))))
 (message (if mes mes "Scores added to region.")))

(defun add-score-comments-to-current-track-or-page-piano-roll-duration ()
  (interactive)
  (if (looking-at "(MidiTrack")
      (add-score-comments-to-form)
    (let* ((start-p (max (window-start) (current-track-pos-before-first-midi-event)))
           (end-p   (min (window-end)   (current-track-pos-after-last-midi-event)))
           (kind-of-midl-score 'piano-roll-note-and-duration) ; dynamic binding !!
          )
      (add-score-comments-to-region start-p end-p "Score comments added to page - numbers show the durations"))))

(defun add-score-comments-to-current-track-or-page-piano-roll-velocity ()
  (interactive)
  (if (looking-at "(MidiTrack")
      (add-score-comments-to-form)
    (let* ((start-p (max (window-start) (current-track-pos-before-first-midi-event)))
           (end-p   (min (window-end)   (current-track-pos-after-last-midi-event)))
           (kind-of-midl-score 'piano-roll-note-and-velocity) ; dynamic binding !!
          )
      (add-score-comments-to-region start-p end-p "Score comments added to page - numbers show the velocities"))))


(defun add-score-comments-to-bars (start-end-bar-string)
  "Add score comments to a number of bars"
  (interactive "sFrom bar number to bar number (sep. by space), one bar number, or the current bar: ")
  (let* ((start-bar-number-0 (first-number-of-string start-end-bar-string))
         (start-bar-number (if start-bar-number-0 start-bar-number-0 (current-bar-number)))
         (end-bar-number (last-number-of-string start-end-bar-string))
         (start-p (locate-initial-bar start-bar-number))
         (end-p   (if end-bar-number (locate-final-bar end-bar-number) (locate-final-bar start-bar-number)))
        )
    (add-score-comments-to-region start-p end-p)))


; Decorate midi messages with scores between the points p and m.
; When called interactively, p and m are supposed to be point and mark.
; If channel is t, add scores for all channels. Else only for a specific channel.
(defun do-add-score-comments-of-region-channel (channel p m)
 "Decorate the selected region, and the given channel with score comments"
 (save-excursion
   (fix-score-start-column-and-piano-roll-interval) ; assign score-start-column
   (let* ((p0 (min p m))
          (pn (max p m)))
     (setq score-counter-transpostion (string-to-number (find-counter-transposition-of-current-buffer)))
     (goto-char pn) (beginning-of-line 1)
     (re-search-backward note-on-reg-exp p0 t) (beginning-of-line 1)
     (while (and (> (point) 1) (>= (point) p0))
       (message (concat "Progress: " (progress pn p0 (point))))
       (decorate-midi-message-line-with-score channel) (beginning-of-line 1)
       (let ((back-search (re-search-backward note-on-reg-exp p0 t)))
         (if back-search
             (beginning-of-line 1)
             (goto-char (- p0 1))))))))

(defun add-score-comments-to-form ()
  "Decorate the selected form with score comments. You are supposed to select a form by placing point at the initial parenthesis."
  (interactive)
  (message "Adding scores to all channels in form...")
  (save-excursion
    (let ((p0 (point)))
      (forward-sexp 1)
      (let ((pn (point)))
        (add-score-comments-to-region p0 pn))))
  (message "Scores added to form"))

(defun add-score-comments-to-form-of-channel (ch)
  "Decorate the selected form with score comments. You are supposed to select a form by placing point at the initial parenthesis."
  (interactive "nAdd score to which channel in form: ")
  (message "Adding scores to channel %s in form..." (number-to-string ch))
  (save-excursion
    (let ((p0 (point)))
      (forward-sexp 1)
      (let ((pn (point)))
        (do-add-score-comments-of-region-channel ch p0 pn))))
  (message "Scores added to form"))

(defun add-score-comments-to-region-of-channel (ch)
  "Decorate a given channel in the selected region with score comments."
  (interactive "nAdd score to which channel in form: ")
  (message "Adding scores to channel " (number-to-string ch) " in form...")
  (save-excursion
    (do-add-score-comments-of-region-channel ch (point) (mark)))
  (message "Scores added to form"))

(defun add-score-comments-to-bars-of-channel (start-end-bar-string ch)
  "Add score comments to a number of bars, only a single channel"
  (interactive "sFrom bar number to bar number (sep. by space), one bar number, or the current bar: 
nAdd score to which channels:  ")
  (let* ((start-bar-number-0 (first-number-of-string start-end-bar-string))
         (start-bar-number (if start-bar-number-0 start-bar-number-0 (current-bar-number)))
         (end-bar-number (last-number-of-string start-end-bar-string))
         (start-p (locate-initial-bar start-bar-number))
         (end-p   (if end-bar-number (locate-final-bar end-bar-number) (locate-final-bar start-bar-number)))
        )
    (do-add-score-comments-of-region-channel ch start-p end-p)))

(defun has-current-line-score-comment ()
 (save-excursion
   (beginning-of-line 1)
   (let ((p0 (point)))
     (end-of-line 1)
     (let ((pn (point)))
        (goto-char p0)
        (let ((search-res (re-search-forward score-comment-line-reg-exp pn t)))
          search-res)))))

(defun remove-score-comment-of-current-line ()
 (save-excursion
   (beginning-of-line 1)
   (let ((p0 (point)))
     (end-of-line 1)
     (let ((search-res (re-search-backward score-comment-line-reg-exp p0 t)))
       (if search-res
           (kill-line-not-cr))))))


(defun remove-score-comments-from-region (p m)
 "Remove the score comments of the selected region"
 (interactive "r")
 (save-excursion
   (let* ((p0 (min p m))
          (pn (max p m)))
     (goto-char pn)
     (while (> (point) p0)
       (message (concat "Progress: " (progress pn p0 (point))))
       (let ((search-res (re-search-backward score-comment-line-reg-exp p0 t)))
          (if search-res 
              (kill-line-not-cr)
              (goto-char p0)))))))

(defun remove-score-comments-from-current-track-or-page ()
  (interactive)
  (if (looking-at "(MidiTrack")
      (remove-score-comments-from-form)
      (let* ((start-p (max (window-start) (current-track-pos-before-first-midi-event)))
             (end-p   (min (window-end)   (current-track-pos-after-last-midi-event)))
             (start-p-1 start-p)
             (end-p-1 end-p)
            )
        (condition-case nil
            (save-excursion
              (goto-char start-p)
              (forward-sexp -5)
              (setq start-p-1 (point)))
          (save-excursion
            (goto-char end-p)
            (forward-sexp 5)
            (setq end-p-1 (point)))
          (error nil))

        (remove-score-comments-from-region start-p-1 end-p-1))))

(defun remove-score-comments-from-bars (start-end-bar-string)
  (interactive "sFrom bar number to bar number (sep. by space), one bar number, or the current bar: ")
  (let* ((start-bar-number-0 (first-number-of-string start-end-bar-string))
         (start-bar-number (if start-bar-number-0 start-bar-number-0 (current-bar-number)))
         (end-bar-number (last-number-of-string start-end-bar-string))
         (start-p (locate-initial-bar start-bar-number))
         (end-p   (if end-bar-number (locate-final-bar end-bar-number) (locate-final-bar start-bar-number)))
        )
    (remove-score-comments-from-region start-p end-p)))

(defun kill-line-not-cr ()
  (while (not (looking-at-chars (point) '(10 13))) (delete-char 1)))

(defun remove-score-comments-from-form ()
  "Remove the score comments from form. You are supposed to select a form by placing point at the initial parenthesis."
  (interactive)
  (save-excursion
    (let ((p0 (point)))
      (forward-sexp 1)
      (let ((pn (point)))
        (remove-score-comments-from-region p0 pn))))
  (message "Removed"))

; Decorate NoteOn message current line with score comment.
; Assume as a precondition that there is a NoteOn message on the current line.
; Assume also that point is at the beginning of the line.             
; Channel can be a number, or t for all channels.
(defun decorate-midi-message-line-with-score (channel)
 (save-excursion
  (let ((channel-of-mes (channel-of-message-on-current-line)))
     (if (or (eq channel t) (and channel-of-mes (= channel channel-of-mes)))
         (let* ((attr-props (attributes-of-current-form))
                (note-number (plist-get attr-props 'note))  ; (note-number-on-current-line))
                (velocity (plist-get attr-props 'velocity))
                (duration (plist-get attr-props 'duration)))
           (insert-comment-at-column-number score-start-column (draw-score-line (as-number note-number) (as-number velocity) (as-number duration))))))))

(defvar info-reg-exp (regular-expression '(concat (one-or-more (char-set tab space)) "'info" (one-or-more (char-set tab space)))))

; Assume that the current line is a NoteOn message with an info attribute.
; Delete the existing note info name (presumably outdated) and insert a new one read from the note attribute value
; Compensate for changed spacing on the current line.
(defun renew-note-info-name-of-current-line ()
 (save-excursion
  (let* ((note-number (note-number-on-current-line))
         (note-name-of-note-number (note-number-to-note-name note-number score-counter-transpostion))  ; the new one - the one to print
         (ins-count (length note-name-of-note-number))
        )
    (end-of-line 1)
    (let ((pn (point)))
      (beginning-of-line 1)
      (let ((search-res (re-search-forward info-reg-exp pn t)))
        (if (and search-res (looking-at "\""))
            (let ((del-count 0))

              ; delete existing note info name:
              (progn
                (forward-sexp 1)  ; now at end of info attribute value
                (backward-char 1)
                (while (not (looking-at " ")) (backward-char 1)) 
                (forward-char 1)
                (while (not (looking-at "\"")) (delete-char 1) (setq del-count (+ 1 del-count)))

              ; add new note info name:
              (progn
                (insert note-name-of-note-number))

              ; Compensate space:
              ; Growing (- ins-count del-count)
              (do-attribute-mutate-space-compensation-after-attr-value (- ins-count del-count) 'info)  ))))))))
   
    
(defun note-number-to-note-name (n0 counter-transposition)
 (let ((n (+ n0 counter-transposition)))
  (let ((root-number (mod n 12))
        (octave-number (- (/ n 12) 2))  ; yamaha convention
       )
   (concat (name-of-note root-number) (as-string octave-number) 
        (if (not (= counter-transposition 0))   ; display counter transposition
            (if (> (- counter-transposition) 0)
                (concat "+" (as-string (- counter-transposition)))
                (as-string (- counter-transposition)))
            "")))))

(defun name-of-note (n)
  (cond ((= n 0) "C")
        ((= n 1) "C#")
        ((= n 2) "D")
        ((= n 3) "D#")
        ((= n 4) "E")
        ((= n 5) "F")
        ((= n 6) "F#")
        ((= n 7) "G")
        ((= n 8) "G#")
        ((= n 9) "A")
        ((= n 10) "A#")
        ((= n 11) "B")
        (t (error "name-of-note: Invalid number: %d" n))))

(defvar lower-c-note-number 60)
(make-variable-buffer-local 'lower-c-note-number)
; (setq lower-c-note-number 60)    ; c4 = 60

(defun set-c-value (c-value)
 (interactive)
  (let ((before lower-c-note-number))
    (setq lower-c-note-number c-value)
    (message (concat "Lower C value changed from " (int-to-string before) " to " (int-to-string c-value)))))

(defun report-lower-c ()
  (interactive)
  (message (concat "Lower C in score comment has value " (int-to-string lower-c-note-number))))

(defvar default-score-start-column 132 "The default start column of score comments")

(defvar score-start-column  125 "The start column of the score comment. Does also affect the length of bar comments.
   Assigned in do-add-score-comments-of-region-channel and add-score-comments-to-region via the p")
(make-variable-buffer-local 'score-start-column)

(defun fix-score-start-column-and-piano-roll-interval ()
 "Set the score-start-column and the piano-roo-min-note and piano-roll-max-note based on the the width of the selected window"
 (let ((ww (window-width (selected-window))))
  (cond ((memq kind-of-midl-score '(piano-roll-note-only piano-roll-note-and-velocity piano-roll-note-and-duration))
            (setq score-start-column default-score-start-column)              ; too simple...
            (let ((piano-interval (- ww score-start-column)))
               (if (< piano-interval 128)
                   (progn
                      (setq piano-roll-min-note (+ (/ (- 127 piano-interval) 2) 3))
                      (setq piano-roll-max-note (- (- 127 (/ (- 127 piano-interval) 2)) 3)))
                   (progn
                      (setq piano-roll-min-note 0)
                      (setq piano-roll-max-note 127))))
        )
        ((eq kind-of-midl-score 'five-line-sheet-music) 
             (setq score-start-column (cond ((>= ww (+ 137 27)) ; broad enough
                                             137)               ; preferred start column

                                            ((>= ww (+ 117 27)) ; broad enough
                                             117)               ; preferred start column

                                            (t (- ww 27))       ; 27 positions from right edge - may be problematic
                                            ))))

  (setq bar-comment-string (make-string score-start-column ?-))
  score-start-column))

; ---------------------------------------------------------------------------------------------------------------
; SCORE LINE DRAWING FUNCTIONS:

(defvar kind-of-midl-score 'piano-roll-note-and-duration "Controls the kind of score comment to draw. Either five-line-sheet-music, piano-roll-note-only, piano-roll-note-and-velocity, 
                                        or piano-roll-note-and-duration")

(defun set-kind-of-midl-score (new-val)
   (interactive (list 
                  (completing-read (format "New kind of MIDI LAML scores (is now: %s): " kind-of-midl-score)
                                          '((piano-roll-note-only . nil) (piano-roll-note-and-velocity . nil) (piano-roll-note-and-duration . nil) (five-line-sheet-music . nil))
                                           nil nil)))
   (setq kind-of-midl-score (as-symbol new-val))
   (message "The kind of MIDI LAML score presentation is now %s." kind-of-midl-score))

(defun draw-score-line (note-number velocity duration)
  (cond ((memq kind-of-midl-score '(piano-roll-note-only piano-roll-note-and-velocity piano-roll-note-and-duration)) (draw-piano-roll-line note-number velocity duration))
        ((eq kind-of-midl-score 'five-line-sheet-music) (draw-five-line-sheet-music-score-line note-number))
        (else (error "draw-score-line: Unknown value of the global variable kind-of-midl-score. " kind-of-midl-score))))


; Return a score line for note-number.
(defun draw-five-line-sheet-music-score-line (note-number)
  (let ((ct-note-number (+ score-counter-transpostion note-number)))   ; counter-transposed node number
    (concat
      (draw-dot       (+ lower-c-note-number -10) ct-note-number  'min) ; d, d#
      (draw-between-1 (+ lower-c-note-number -8) ct-note-number)   ; e    
      (draw-dot       (+ lower-c-note-number -7) ct-note-number)   ; f, f#
      (draw-between   (+ lower-c-note-number -5) ct-note-number)   ; g, g#    
      (draw-dot       (+ lower-c-note-number -3) ct-note-number)   ; a, a#
      (draw-between-1 (+ lower-c-note-number -1) ct-note-number)   ; h      

      (draw-line      (+ lower-c-note-number 0) ct-note-number)    ; c, c#
      (draw-between   (+ lower-c-note-number 2) ct-note-number)    ; d, d#
      (draw-line-1    (+ lower-c-note-number 4) ct-note-number)    ; e
      (draw-between   (+ lower-c-note-number 5) ct-note-number)    ; f, f#
      (draw-line      (+ lower-c-note-number 7) ct-note-number)    ; g, g#
      (draw-between   (+ lower-c-note-number 9) ct-note-number)    ; a, a#
      (draw-line-1    (+ lower-c-note-number 11) ct-note-number)   ; h
      (draw-between   (+ lower-c-note-number 12) ct-note-number)   ; c, c#
      (draw-line      (+ lower-c-note-number 14) ct-note-number)   ; d, d#
      (draw-between-1 (+ lower-c-note-number 16) ct-note-number)   ; e

      (draw-dot       (+ lower-c-note-number 17) ct-note-number)    ; f, f#
      (draw-between   (+ lower-c-note-number 19) ct-note-number)    ; g, g#
      (draw-dot       (+ lower-c-note-number 21) ct-note-number)    ; a, a#
      (draw-between-1 (+ lower-c-note-number 23) ct-note-number)   ; h
      (draw-dot       (+ lower-c-note-number 24) ct-note-number 'max) ; c, c#
    )))

(defun draw-line (low-value note-number &optional limit)
  (cond ((and (eq limit 'min) (< note-number low-value)) "<")
        ((= low-value note-number) "o")
        ((= (+ low-value 1) note-number) "=")
        ((and (eq limit 'max) (> note-number (+ low-value 1))) ">")
        (t "|")))

(defun draw-line-1 (low-value note-number &optional limit)
  (cond ((= low-value note-number) "o")
        ((and (eq limit 'max) (> note-number (+ low-value 1))) ">")
        (t "|")))

(defun draw-dot-1 (low-value note-number &optional limit)
  (cond ((= low-value note-number) "o")
        ((and (eq limit 'max) (> note-number low-value)) ">")
        (t ".")))

(defun draw-dot (low-value note-number &optional limit)
  (cond ((and (eq limit 'min) (< note-number low-value)) "<")
        ((= low-value note-number) "o")
        ((= (+ low-value 1) note-number) "=")
        ((and (eq limit 'max) (> note-number (+ low-value 1))) ">")
        (t ".")))

(defun draw-between (low-value note-number &optional limit)
  (cond ((= low-value note-number) "o")
        ((= (+ low-value 1) note-number) "=")
        ((and (eq limit 'max) (> note-number (+ low-value 1))) ">")
        (t " ")))

(defun draw-between-1 (low-value note-number &optional limit)
  (cond ((= low-value note-number) "o")
        ((and (eq limit 'max) (> note-number low-value)) ">")
        (t " ")))

; End Score drawing function.

; ---------------------------------------------------------------------------------------------------------------
; PIANO ROLL DRAWING FUNCTIONS:

(defvar piano-roll-min-note 20 "The minimum (leftmost) note shown in a piano roll line")
(defvar piano-roll-max-note 127 "The maximum (rightmost) note shown in a piano roll line")

(defun piano-roll-note-char(velocity duration)
  "Return the note character (in elisp an integer) for a note with the given velocity and duration"
  (cond ((eq kind-of-midl-score 'piano-roll-note-only) ?x) ; 'x'
        ((eq kind-of-midl-score 'piano-roll-note-and-velocity) ; A velocity char between 0 (weak) and 9 (strong)
         (+ 48                                                 ;  ?0
            (/ velocity 13)                                    ;  between 0 and 9
            ))
        ((eq kind-of-midl-score 'piano-roll-note-and-duration) ; A duration char between 0 (short) and 9 (long)
         (+ 48                                                 ;  ?0
            (min (/ duration 120) 9)                           ;  between 0 and 9
            ))))

(defun draw-piano-roll-line (note-number-0 velocity duration)
  (let ((note-number (+ score-counter-transpostion note-number)))   ; actually draw the counter-transposed node number
    (char-list-to-string
      (append (list 
                (cond ((< note-number piano-roll-min-note) ?<)
                      ((= note-number piano-roll-min-note) (piano-roll-note-char velocity duration))
                      ((= (mod piano-roll-min-note 12) 0) ?|)
                      (t 32))
              )
              (mapcar (function (lambda (n)
                                  (cond ((= n note-number) (piano-roll-note-char velocity duration))
                                        ((= (mod n 12) 0) ?|)
                                        (t 32))                      
                                  ))
                      (number-interval (+ piano-roll-min-note 1) (- piano-roll-max-note 1)))
              (list 
                (cond ((> note-number piano-roll-max-note) ?>)
                      ((= note-number piano-roll-max-note) (piano-roll-note-char velocity duration))
                      ((= (mod piano-roll-max-note 12) 0) ?|)
                      (t 32))
              )
      )
    )
  )
)

(defun char-list-to-string (char-lst)
  (apply (function string) char-lst))



; ---------------------------------------------------------------------------------------------------------------


; Insert comment-txt a column number cn at the current line
(defun insert-comment-at-column-number (cn txt)
  (delete-existing-comment-at-line)
  (end-of-line 1)
  (let ((eol-pos (column-number-of (point))))
    (cond ((<= eol-pos cn)
              (insert (make-string (- cn eol-pos) 32))
              (insert "; ")
              (insert txt))
          (t  (insert "")))))

(defun delete-existing-comment-at-line ()
 (save-excursion
  (end-of-line)
  (while (is-inside-comment)
     (backward-delete-char-untabify 1))
  (backward-char 1)
  (while (looking-at-chars (point) '(32))
     (delete-char 1) (backward-char 1))))


; point in front of message form.
; Return the kind of midi message at the current line. A string or nil.
(defun kind-of-message-on-current-line ()
  (save-excursion
    (let ((p0 (point))
          (eol-p (progn (end-of-line 1) (point))))
      (goto-char p0)
      (if (eobp)
          nil
          (progn
            (while (and (not (eobp)) (not (looking-at-chars (point) '(40)))) (forward-char 1)) ; goto start paren
            (if (or (eobp) (> (point) eol-p))
                nil
              (progn
                (forward-char 1)
                (if (looking-at-chars (point) '(40)) ; another start paren.
                    nil
                  (name-under-point)))))))))

; point in front of message form.
; Return the channel of message at the current line. A number or nil
(defun channel-of-message-on-current-line ()
  (save-excursion
    (let ((p0 (point))
          (eol-p (progn (end-of-line 1) (point))))
      (goto-char p0)
      (if (eobp)
          nil
          (let ((res (search-forward "'channel" eol-p t)))
            (if res
                (progn
                  (while (looking-at-chars (point) '(32 34))   ; space, quote
                     (forward-char 1))
                  (let ((channel-string (name-under-point)))
                    (string-to-number channel-string)))
                nil))))))


; point in front of NoteOn message form.
(defun note-number-on-current-line ()
 (save-excursion
   (search-forward "'note" nil t)
   (while (not (looking-at-chars (point) '(34))) (forward-char 1))  ; goto "
   (forward-char 1)
   (let ((str (name-under-point)))
     (string-to-number str))))

; ---------------------------------------------------------------------------------------------------------------
; Progress feedback.

; actual value goes from from-value to to-value. Display percentage done progress.
(defun progress (from-value to-value actual-value)
  (concat (number-to-string (truncate (* (/ (float (abs (- actual-value from-value))) (abs (- from-value to-value))) 100))) "%% done..."))

; ---------------------------------------------------------------------------------------------------------------

(defun find-processing-mode-of-current-buffer()
  "Return a string. Defaults to absTime."
  (save-excursion
    (goto-char (point-min))
    (let ((search-res (search-forward "'mode" nil t)))
      (if search-res
          (progn
            (search-forward "\"" nil t)
            (name-under-point))
          "absTime"))))

(defun guess-processing-mode (start-pos end-pos)
  "Guess the processing mode from the first midi event in the [start-pos end-pos] interval. 
start and end are buffer positions. Return a string, either absTime or deltaTime. Defaults to deltaTime."
  (save-excursion
    (let ((delta-search (progn (goto-char start-pos)  (search-forward "deltaTime" end-pos t)))
          (abs-search   (progn (goto-char start-pos) (search-forward "'absTime" end-pos t)))
         )
      (cond ((and delta-search abs-search)
               (if (< delta-search abs-search) "deltaTime" "absTime"))
            (delta-search "deltaTime")
            (abs-search "absTime")
            (t "deltaTime")))))


(defun find-tempo-of-current-buffer()
  "Return the tempo of the current buffer. An integer measured in BPM (= number of quarter notes per minute).
Take the information from the info part of a Meta type 81 event (which - in the ideal world - is NOT the best approach).
An alternative is (hex-string-to-int (find-global-tempo-value)) which returns the number of micro seconds pr. 
Return nil if tempo cannot be found."
  (cond ((or (current-buffer-is-standard-midifile) (current-buffer-is-style-format))
            (save-excursion
              (goto-char (point-min))
              (let ((search-res (search-forward "microsec. per quarter note = " nil t)))
                (if search-res
                    (progn
                      (string-to-number (name-under-point)))
                  nil))))
        ((current-buffer-is-multipad-format)
            (error "No tempo assigned to multipad"))
        (t (error "Unknow format of midi laml file: Should be StandardMidiFile, TyrosMultiPad, or TyrosStyle"))))

(defun find-pulses-per-quarter-note-of-current-buffer()
  "Return the number of pulses per quarter note of the current buffer. An integer.
Return nil if pulses per quarter note cannot be found."
  (cond ((or (current-buffer-is-standard-midifile) (current-buffer-is-style-format))
            (save-excursion
              (goto-char (point-min))
              (let ((search-res (search-forward "'pulsesPerQuarterNote" nil t)))
                (if search-res
                    (progn
                      (search-forward "\"" nil t)
                      (string-to-number (name-under-point)))
                  nil))))
        ((current-buffer-is-multipad-format)
            1920)
        (t (error "Unknow format of midi laml file: Should be StandardMidiFile, TyrosMultiPad, or TyrosStyle"))))

(defun find-counter-transposition-of-current-buffer()
  "Return a string. Defaults to \"0\"."
  (save-excursion
    (goto-char (point-min))
    (let ((search-res (search-forward "'counterTransposition" nil t)))
      (if search-res
          (progn
            (search-forward "\"" nil t)
            (name-under-point))
          "0"))))

(defun find-number-of-tracks-of-current-buffer ()
  "Find the number of midi tracks of the current buffer (a non-negative integer). Return 0 for non-StandardMidiFiles."
  (save-excursion
    (if (eq (kind-of-midl-file-in-current-buffer) 'midi) ; StandardMidiFile
        (progn
          (goto-char (point-min))
          (re-search-forward (regular-expression '(concat "(" (zero-or-more (char-set tab newline space return)) "StandardMidiFile")) nil t)
          (count-matches (regular-expression '(concat "(" (zero-or-more (char-set tab newline space return)) "MidiTrack"))))
        0)))

; Slow if there are large amount of score comments (piano roll comments) in the track:
;(defun find-fixed-part-of-first-track-of-current-buffer ()
;  "Find the value of the fixed-part attribute of the first (and presumably only) MidiTrack of the current MIDI LAML buffer. A string.
;Returns the attribute value, or nil of the attribute is not present."
;  (save-excursion
;    (goto-char (point-min))
;    (let ((search-res (search-forward "(MidiTrack" nil t)))
;      (if search-res      
;          (progn
;            (backward-up-list 1)
;            (forward-sexp 1)
;            (let ((pn (point)))
;              (forward-sexp -1)  ; may be slow in particular
;              (let ((p1 (search-forward "'fixed-part" pn t)))
;                (if (and p1 (or (looking-at-white-space (point)) (looking-at-chars (point) (list 34))))
;                    (progn
;                      (while (looking-at-chars (point) (list 32 10 13 9 34))   ; white space and double quote
;                             (forward-char 1))
;                      (name-under-point))
;                    nil))))
;          nil))))

(defun find-fixed-part-of-first-track-of-current-buffer ()
  "Find the value of the fixed-part attribute of the first (and presumably only) MidiTrack of the current MIDI LAML buffer. A string.
Returns the attribute value, or nil of the attribute is not present."
  (save-excursion
    (goto-char (point-min))
    (let ((search-res (search-forward "(MidiTrack" nil t)))
      (if search-res      
          (progn
            (backward-char 10) ; Now looking at start parenthesis of '(MidiTrack ...)
            (let ((midi-track-attributes (attributes-of-current-form)))
              (plist-get midi-track-attributes 'fixed-part)))
          nil))))
               

(defun enter-track-of-standard-midi-file (&optional n)
  "Enter track n of a standard midi file, where n defaults to 1. Leave the cursor point just after the name MidiTrack.
Assume as a precondition that the current buffer is a standard midi file with at least n tracks. 
You can use the function find-number-of-tracks-of-current-buffer to find the number of trakcs."
  (let ((n1 (if n n 1)))
    (goto-char (point-min))
    (re-search-forward (regular-expression '(concat "(" (zero-or-more (char-set tab newline space return)) "StandardMidiFile")) nil t)
    (dotimes (i 100) 
      (re-search-forward (regular-expression '(concat "(" (zero-or-more (char-set tab newline space return)) "MidiTrack")) nil t))))

(defun go-inside-nearest-midi-track ()
  "Relocate point inside the nearest MidiTrack, either forward or backward. Return t is successful, nil otherwise.
Useful if point is at the preamble stuff of the midl file, or at the very end of the midl file."
  (let ((p0 (point)))
    (let ((forward-res (search-forward "(MidiTrack" nil t)))
       (if forward-res 
           (progn
             (while (looking-at-chars (point) white-space-char-list) (forward-char 1))
             t)
           (let ((backward-res (search-backward "(MidiTrack" nil t)))
              (if backward-res
                  (progn
                    (down-list 1) (forward-sexp 1)
                    (while (looking-at-chars (point) white-space-char-list) (forward-char 1))
                    (goto-char (current-track-pos-after-last-midi-event))
                    t)
                  (progn
                     (goto-char p0)
                     nil)))))))

; expensive if score comments are shown:
(defun is-inside-track-of-standard-midi-file ()
  "Is the cursor point inside a MidiTrack for a StandardMidiFile. 
This function ruturns nil if point is at the initial parenthesis of a MidiTrack (but it, unfortunately, returns t if inside the symbol
MidiTrack or at the end parenthesis of a MidiTrack)."
  (save-excursion
    (if (eq (kind-of-midl-file-in-current-buffer) 'midi) ; StandardMidiFile
        (condition-case nil
            (progn
              (if (looking-at "(MidiTrack")
                  nil
                  (progn
                     (if (is-inside-string) (while (not (looking-at "\"")) (backward-char 1)))  ; not entire safe - but good enough in 
                                                                                                ; this context
                     (while (not (looking-at "(MidiTrack")) (backward-up-list 1))
                     (looking-at "(MidiTrack"))))
          (error nil)))))

; ---------------------------------------------------------------------------------------------------------------
; Selective Midi Playing. Format 0 stuff.

(defvar selection-begin nil "The start point of the previous selection")   ; not used right now
(defvar selection-end nil "The end point of the previous selection")       ; not used right now
(defvar last-temporary-midi-file-path nil "The full path to the previous midi file selection")
(defvar previous-midi-step-length nil "The length of the previous midi step. An integer.") 

(make-variable-buffer-local 'selection-begin)
(make-variable-buffer-local 'selection-end)
(make-variable-buffer-local 'last-temporary-midi-file-path)
(make-variable-buffer-local 'previous-midi-step-length)

(defun process-selected-track-and-play ()
  "Play the implicitly selected MIDI track. Point is assumed to be at the initial parenthesis of a MidiTrack form.
Wrap the selected track of a midl file into a proper context, process it, and play it. Sensitive to channel selection and relative tempo."
  (interactive)
  (do-selected-track-and-play 'play)
)

(defun process-selected-track-and-illustrate ()
  (interactive)
  (do-selected-track-and-play 'illustrate)
)


(defun do-selected-track-and-play (command)
  "Point is assumed to be at the initial parenthesis of a MidiTrack form.
Wrap the selected track of a midl file into a proper context, process it, and do it.
Command is either the symbol play or illustrate.
Works for format 0 as well as format 1 files."

  (if (looking-at "(MidiTrack")
      (save-excursion
      (let ((start-p (first-noteon-in-track))
            (end-p (last-noteon-in-track)))
        (setq selection-begin start-p) (setq selection-end end-p) 

        (goto-char start-p)   ; to allow process-laml-file-and-present-from-to to locate the appropriate context from this position
        (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p end-p command)
      ))
      (progn
        (beep)
        (message "You must select the initial parenthesis of a MidiTrack in order to use this command."))))



; Assume point is at initial parenthesis in MidiTrack form
(defun first-noteon-in-track ()
  (save-excursion
    (search-forward "(NoteOn" nil t)
    (beginning-of-line 1)
    (point)))

; Assume point is at initial parenthesis in MidiTrack form
(defun last-noteon-in-track ()
  (save-excursion
    (forward-sexp 1)
    (search-backward "(NoteOn" nil t)
    (beginning-of-line 1) (forward-sexp 1)
    (point)))
  

(defun process-selected-laml-file-and-play ()
  "Wrap the selected part of a midl file into a proper context, process it, and play it.
Works for format 0 as well as format 1 files. Sensitive to channel selection and relative tempo."
  (interactive)
  (let ((start-p (min (point) (mark)))
        (end-p (max (point) (mark))))
    (setq selection-begin start-p) (setq selection-end end-p) 
    (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p end-p 'play)))

(defun process-selection-or-shadow-and-play ()
  "Dispatch to either process and play of the current selection or the current shadow"
  (interactive)
  (if (and (single-line-selection-p) (not (null shadow-overlay)))
      (process-shadow-and-play t)
      (process-selected-laml-file-and-play)))

(defun single-line-selection-p()
  "Does the current point and mark select a single line"
  (let* ((start-p (min (point) (mark)))
         (end-p (max (point) (mark)))
         (start-pos-of-next-line (save-excursion (goto-char start-p) (next-line 1) (point))))
   (< end-p start-pos-of-next-line)))

(defun process-shadow-and-play (go-back-to-start)
  "Process and play the current shadow. If the boolean parameter go-back-to-start is true, go back to start-p, else leave the point at the end point."
  (interactive)
  (let ((start-p (overlay-start shadow-overlay))
        (end-p (overlay-end shadow-overlay)))
    (setq selection-begin start-p) (setq selection-end end-p) 
    (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p end-p 'play nil t)
    (if go-back-to-start (goto-char start-p))
    (highlight-this-line)
  )
)

(defun process-selected-bars-laml-file-and-play (start-end-bar-string)
  "Wrap the selected bars of a midl file into a proper context, process it, and play it.
Works for format 0 files as well as format 1 files. Sensitive to channel selection."
  (interactive 
     (list (read-from-minibuffer "Play from bar number to bar number (sep. by space) or only one bar number: " (as-string (current-bar-number)))))
  (let* ((start-bar-number-0 (first-number-of-string start-end-bar-string))
         (start-bar-number (if start-bar-number-0 start-bar-number-0 (current-bar-number)))
         (end-bar-number (last-number-of-string start-end-bar-string))
         (start-p (locate-initial-bar start-bar-number))
         (end-p   (if end-bar-number (locate-final-bar end-bar-number) (locate-final-bar start-bar-number)))
        )
;    (goto-char start-p)
    (setq selection-begin start-p) (setq selection-end end-p) 
    (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p end-p 'play)))

(defun process-current-bar-and-play (number-of-bars-0)
  "Wrap the current bar, and depending of the prefix argument a number of additional bars,
of a midl file into a proper context, process it, and play it.
Works for format 0 files as well as format 1 files."
  (interactive "p")
  (let* ((number-of-bars (if number-of-bars-0 number-of-bars-0 1))
         (start-bar-number (current-bar-number))
         (end-bar-number (+ (current-bar-number) (- number-of-bars 1)))
         (start-p (locate-initial-bar start-bar-number))
         (end-p   (if end-bar-number (locate-final-bar end-bar-number) (locate-final-bar start-bar-number)))
        )
    (if (= start-bar-number end-bar-number)
        (message "Playing %s in bar number %d..." (channel-info-string channels-to-play) start-bar-number)
        (message "Playing %s from bar %d to bar %d..." (channel-info-string channels-to-play) start-bar-number end-bar-number))
    (setq selection-begin start-p) (setq selection-end end-p) 
    (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p end-p 'play)
    ))

(defun process-from-current-bar-to-end-and-play ()
  (interactive)
  (let* ((max-bar (find-max-bar-number))
         (remaining-bar-numbers (+ (- max-bar (current-bar-number)) 1))
        )
    (process-current-bar-and-play remaining-bar-numbers)  ; process and play from current bart number and to end 
  )
)

(defun process-current-page-and-play ()
  "Process and play the current page (the window in Emacs terms). Sensitive to play channels and tempo"
  (interactive)

  (set-mark nil)
;  (if (not (is-inside-track-of-standard-midi-file))   ; Too slow
;      (go-inside-nearest-midi-track))

  (if t ;   too slow (is-inside-track-of-standard-midi-file)
      (progn
        (message "Playing current (visible) page in %s." (channel-info-string channels-to-play))
        (let* ((start-p (max (window-start) (current-track-pos-before-first-midi-event)))
               (end-p   (min (window-end)   (current-track-pos-after-last-midi-event)))
              )
          (setq selection-begin start-p) (setq selection-end end-p) 
          (setq avoid-last-line-scrolling nil)
          (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p end-p 'play)
          ))
    (message "Nothing to play")))

(defun process-next-page-and-play ()
  (interactive)
  (scroll-up)
  (goto-char (window-start)) (set-mark nil)
  (forward-midi 1 channels-to-play)
  (sit-for 0.1) ; necessary - why?
  (process-from-point-and-current-page-and-play)
)

(defun process-previous-page-and-play ()
  (interactive)
  (scroll-down) 
  (goto-char (window-start)) (set-mark nil)
  (sit-for 0.1) ; necessary - why?
  (process-current-page-and-play)
)

(defun process-from-point-and-current-page-and-play ()
  "Process and play from point and the rest of the current page (the window in Emacs terms). Sensitive to play channels and tempo"
  (interactive)
    (message "Playing from point and the rest of current (visible) page in %s." (channel-info-string channels-to-play))

    (if nil ; (not (is-inside-track-of-standard-midi-file))  ; very expensive in case of piano roll or score comments - therefore eliminated
        (go-inside-nearest-midi-track))

    (if t ;  (is-inside-track-of-standard-midi-file)         ; similary very expensive. Therefore eliminated
        (progn
          (beginning-of-line 1)
          (let* ((start-p (point))
                 (end-p   (min (window-end)   (current-track-pos-after-last-midi-event)))
                )
            (setq selection-begin start-p) (setq selection-end end-p) 
            (setq avoid-last-line-scrolling nil)
            (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p end-p 'play)
            ))
        (message "Nothing to play")))

(defun process-from-current-point-to-end-and-play ()
  "Process and play from point and the rest of the current track. Sensitive to play channels and tempo"
  (interactive)
    (message "Playing from point and the rest of the current track in %s." (channel-info-string channels-to-play))

    (if (not (is-inside-track-of-standard-midi-file))
        (go-inside-nearest-midi-track))

    (if (is-inside-track-of-standard-midi-file)
        (progn
          (beginning-of-line 1)
          (let* ((start-p (point))
                 (end-p   (current-track-pos-after-last-midi-event))
                 )
            (setq selection-begin start-p) (setq selection-end end-p) 
            (setq avoid-last-line-scrolling nil)
            (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p end-p 'play)
            ))
         (message "Nothing to play")))


(defun process-current-page-and-play-mouse-speed-control ()
  "Process and play the channels-top-play the current page (the window in Emacs terms). The horizontal position of the cursor
overrules the ordinary speed control."
  (interactive)

  (if (not (is-inside-track-of-standard-midi-file))
      (go-inside-nearest-midi-track))

  (if (is-inside-track-of-standard-midi-file)
      (let* ((x-frame-position (cadr (mouse-pixel-position))) 
             (speed (relative-tempo x-frame-position)))
        (message "Playing current (visible) page in %s without moving point. Speed: %d %%." (channel-info-string channels-to-play) speed)
        (let* ((start-p (max (window-start) (current-track-pos-before-first-midi-event)))
               (end-p   (min (window-end)   (current-track-pos-after-last-midi-event)))
               )
          (setq selection-begin start-p) (setq selection-end end-p) 
          (process-laml-file-and-present-from-to channels-to-play speed start-p end-p 'play)
          ))
      (message "Nothing to play")))

; Not entirely accurate, but probably good enough
(defun current-track-pos-before-first-midi-event()
  "Return the position just before the first midi event of the currently (implicitly) selected track. Return nil if not inside a track."
  (save-excursion
    (beginning-of-line 1)
    (backward-up-list 1)
    (if (looking-at "(MidiTrack") 
        (progn 
           (forward-char 10) 
           (while (looking-at-chars (point) '(9 10 13 32)) ; white space
             (forward-char 1))
           (if (looking-at "'fixed-part")
               (progn
                 (forward-sexp 2) 
                 (while (looking-at-chars (point) '(9 10 13 32)) ; white space
                   (forward-char 1))))
           (point))
        nil)))

(defun current-track-pos-after-last-midi-event()
  "Return the position just after the last midi event of the currently (implicitly) selected track. Return nil if not inside a track."
  (save-excursion
    (beginning-of-line 1)
    (backward-up-list 1)
    (if (looking-at "(MidiTrack") 
        (progn 
           (forward-sexp 1) 
           (backward-char 2)                                ; passing back over end parenthesis

;  Earlier
;           (while (looking-at-chars (point) '(9 10 13 32))  ; white space
;             (backward-char 1))

;  Now - april 3, 2011:
            (while (not (looking-at-chars (point) '(41)))    ; not looking at end parenthesis
                   (backward-char 1))
            
           (+ (point) 1))
        nil)))

(defun process-current-blank-separated-region-and-play ()
  "Process and play the current page (the window in Emacs terms)"
  (interactive)
    (message "Playing current blank separated region in %s." (channel-info-string channels-to-play))
    (let ((p1 (pos-of-previous-blank-line-before-point))
          (p2 (pos-of-next-blank-line-after-point)))
      (let* ((start-p (if p1 (max p1 (current-track-pos-before-first-midi-event)) (current-track-pos-before-first-midi-event)))
             (end-p   (if p2 (min p2 (current-track-pos-after-last-midi-event)) (current-track-pos-after-last-midi-event)))
            )
        (setq selection-begin start-p) (setq selection-end end-p) 
        (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p end-p 'play)
        )))

(defun process-from-point-and-current-blank-separated-region-and-play ()
  "Process and play the current page (the window in Emacs terms)"
  (interactive)
    (message "Playing from point to end of current blank separated region in %s." (channel-info-string channels-to-play))
    (beginning-of-line 1)    
    (let ((p1 (point))
          (p2 (pos-of-next-blank-line-after-point)))
      (let* ((start-p (if p1 (max p1 (current-track-pos-before-first-midi-event)) (current-track-pos-before-first-midi-event)))
             (end-p   (if p2 (min p2 (current-track-pos-after-last-midi-event)) (current-track-pos-after-last-midi-event)))
            )
        (setq selection-begin start-p) (setq selection-end end-p) 
        (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p end-p 'play)
        )))




(defvar blank-line-regexp (regular-expression '(concat (zero-or-more (char-set tab space)) newline)))

(defun at-blank-line (p)
  "Is the position p at a blank line"
  (save-excursion
    (goto-char p) 
    (beginning-of-line 1)
    (looking-at blank-line-regexp)))

(defvar midi-track-line-regexp (regular-expression '(concat (zero-or-more (char-set tab space)) "(MidiTrack")))

(defun at-midi-track-line(p)
  "Is the position p at a  (MidiTrack ... line"
  (save-excursion
    (goto-char p) 
    (beginning-of-line 1)
    (looking-at midi-track-line-regexp)))

(defun pos-of-previous-blank-line-before-point()
  "Return the char position of the previous blank line, or nil if no such line exists in this track."
  (save-excursion 
    (beginning-of-line 1)
    (while (not (or (at-blank-line (point)) (at-midi-track-line (point))))
      (previous-line 1))
    (if (at-blank-line (point)) (point) nil)))

(defun pos-of-next-blank-line-after-point()
  "Return the char position of the previous blank line, or nil if no such line exists in this track."
  (save-excursion 
    (beginning-of-line 1)
    (while (not (or (at-blank-line (point)) (at-end-parenthesis-line (point))))
      (next-line 1))
    (if (at-blank-line (point)) (point) nil)))

(defvar end-parenthesis-line-regexp (regular-expression '(concat (zero-or-more (char-set tab space)) ")")))

(defun at-end-parenthesis-line (p)
  "Is the position p at an end of parenthesis line"
  (save-excursion
    (goto-char p) 
    (beginning-of-line 1)
    (looking-at end-parenthesis-line-regexp)))


(defun cache-selection-in-laml-file ()
  "Wrap the selected part of a midl file into a proper context, process it, generate (cache) single-note midi files.
Sensitive to channel selection."
  (interactive)
  (message "Caching in progress for %s ..." (channel-info-string channels-to-play))
  (let ((start-p (min (point) (mark)))
        (end-p (max (point) (mark))))
    (if (eq (mode-of-standard-midi-file-current-buffer) 'absTime)
        (progn
          (setq selection-begin start-p) (setq selection-end end-p) 
          (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p end-p 'none "laml-single-notes")
          (message (concat "Caching done for " (channel-info-string channels-to-play))))
        (message "Can only be used in absTime mode. Nothing done."))))


(defun cache-selected-track ()
  "Point is assumed to be at the initial parenthesis of a MidiTrack form.
Wrap the selected track of a midl file into a proper context, process it, and chache the NoteOn in the track.
Sensitive to channel selection. Works for format 0 as well as format 1 files."
  (interactive)
  (message "Caching in progress for %s ..." (channel-info-string channels-to-play))
  (if (looking-at "(MidiTrack")
      (save-excursion
        (let ((start-p (first-noteon-in-track))
              (end-p (last-noteon-in-track)))
          (if (eq (mode-of-standard-midi-file-current-buffer) 'absTime)
              (progn
                (setq selection-begin start-p) (setq selection-end end-p) 

                (goto-char start-p) ; to allow process-laml-file-and-present-from-to to locate the appropriate context from this position

                (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p end-p 'none "laml-single-notes")
                (message (concat "Caching done for " (channel-info-string channels-to-play))))
              (message "Can only be used in absTime mode. Nothing done."))
        )
      )
      (progn
        (beep)
        (message "You must select the initial parenthesis of a MidiTrack in order to use this command."))))

(defun cache-selected-bars-in-laml-file (start-end-bar-string)
  "Wrap the selected bars of a midl file into a proper context, process it, and cache the single NoteOn messages.
Sensitive to channel selection. Works for format 0 files as well as format 1 files."
  (interactive "sCache notes from bar numbers (sep. by space), single, or current bar: ")
  (message "Caching in progress for %s ..." (channel-info-string channels-to-play))
  (let* ((start-bar-number-0 (first-number-of-string start-end-bar-string))
         (start-bar-number (if start-bar-number-0 start-bar-number-0 (current-bar-number)))
         (end-bar-number (last-number-of-string start-end-bar-string))
         (start-p (locate-initial-bar start-bar-number))
         (end-p   (if end-bar-number (locate-final-bar end-bar-number) (locate-final-bar start-bar-number)))
        )
    (if (eq (mode-of-standard-midi-file-current-buffer) 'absTime)
        (progn
          (setq selection-begin start-p) (setq selection-end end-p) 
          (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p end-p 'none "laml-single-notes")
          (message (concat "Caching done for " (channel-info-string channels-to-play))))
        (message "Can only be used in absTime mode. Nothing done."))))

(defun delete-single-noteon-cache ()
  (interactive)
  (message "Deleting single note cache...")
  (let* ((dir (concat (current-directory) "midi-temp/" "single-notes/"))
         (dir-files (list-of-files dir)))
    (if (file-exists-p dir)
        (if (> (length dir-files) 0)
            (progn 
              (mapcar (lambda (f) (delete-file (concat dir f))) dir-files)
              (delete-directory-recursively dir)
              (message "%s single note files has been deleted, and single-notes dir has been removed" (length dir-files)))
            (progn
              (delete-directory dir)
              (message "Nothing cached. The single-notes directory has been deleted.")
            ))
        (message "No single-note cache exists. NOTHING DONE."))))

(defun toggle-use-of-single-note-cache ()
  (interactive)
  (setq use-single-note-midi-file-cache (not use-single-note-midi-file-cache))
  (message (if use-single-note-midi-file-cache "Single note cache is now in use" "Single note cache is now deactivated")))

(defun single-note-cache-info ()
  (interactive)
  (message (if use-single-note-midi-file-cache "Single note cache is currently in use." "Single note cache is not activated at this moment.")))

(defun make-voice-collection-from-midl-file ()
  "Extract the voices from the current MIDI LAML file, organize these in voice collection file, and show it as a voice browser 'in the other window'. A voice collection file has extension 'voices'.
If the voice collection file already exists, add a numeric prefix."
  (interactive)
  (if (fboundp 'open-voice-collection-from-browser-other-window)
      (let* ((orig-buf (current-buffer))
	     (proper-voice-file-name (file-name-sans-extension (buffer-name orig-buf)))
	     (cur-dir (current-directory))
	     (unique-proper-voice-file-name (make-unique cur-dir proper-voice-file-name "voices"))
	     (voice-file-with-path (concat cur-dir unique-proper-voice-file-name "." "voices"))
	     )
	(laml-sync-process-current-buffer "voice-collection-extract")
	(copy-file (concat laml-temp-file-path "voices.voices") voice-file-with-path)
	(open-voice-collection-from-browser-other-window voice-file-with-path)
	(message "Done. The voice file is  %s"  voice-file-with-path))
      (error "This feature is not supported in the current version of MIDI LAML.")))

(defun process-and-make-channel-stat ()
  "Present channel statistics of the current midl file. Present the info in 'the other' buffer."
  (interactive)
   (let ((stat-file-path (concat laml-temp-file-path "channel-stat.txt")))
    (if (file-exists-p stat-file-path) (delete-file stat-file-path))      
    (laml-sync-process-current-buffer "channel-stat")  ; makes file in stat-file-path
    (if (file-exists-p stat-file-path)
        (let* ((orig-buf (current-buffer))
               (info-buffer (if (get-buffer "*ChannelInfo*") (get-buffer "*ChannelInfo*") (generate-new-buffer "*ChannelInfo*")))
              )
          (delete-other-windows)
          (split-window-vertically)
          (set-window-buffer (selected-window) info-buffer)
          (set-buffer info-buffer)
          (toggle-read-only -1)
          (erase-buffer)
          (insert-file-contents (concat laml-temp-file-path "channel-stat.txt"))
          (toggle-read-only 1)
          (set-buffer-modified-p nil)
          (adjust-size-of-window 17 3)
          (other-window 1)
          (message "Done."))
        (error "process-and-make-channel-stat: No stat file has been generated."))))

(defun adjust-size-of-window (required-number-of-lines extra-lines)
 "Adjust the size of the current window to be required-number-of-lines + extra-lines"
 (let* ((lines-max (/ (frame-height) 2))
        (total-number-of-lines (+ required-number-of-lines extra-lines))
       )
   (if (< total-number-of-lines lines-max)
       (enlarge-window (- total-number-of-lines lines-max)) ; a negative numer, thus shrinks the window size
   )
 )
)



(defun goto-next-bar-and-process-current-bar-and-play (n)
  "Goto next bar. Wrap that bar into a proper context, process it, and play number-of-bars-0 bars (defaults to a single bar).
With a prefix argument, play n bars. Works for format 0 files as well as format 1 files."
  (interactive "p")
  (set-mark nil)
  (let ((start-bar-number (current-bar-number)))
      (goto-bar-internal (+ start-bar-number 1))
      (if (member play-after-condition '(after-navigate after-navigate-and-edit))
         (progn 
           (if (= n 1)
               (message "Now at start of bar %d - playing %s in bar number %d..."
                        (+ start-bar-number 1) (channel-info-string channels-to-play) (+ start-bar-number 1))
               (message "Now at start of bar %d - playing %s from bar number %d to bar number %d..."  
                        (+ start-bar-number 1) (channel-info-string channels-to-play) (+ start-bar-number 1) (+ start-bar-number 1 (- n 1)) ))
           (let* ((start-bar-number (current-bar-number))
                  (end-bar-number (current-bar-number))
                  (start-p (locate-initial-bar start-bar-number))
                  (end-p   (if end-bar-number (locate-final-bar (+ end-bar-number (- n 1))) (locate-final-bar start-bar-number)))
                 )
             (setq selection-begin start-p) (setq selection-end end-p) 
             (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p end-p 'play)
             ))
         (message "Now at the beginning of bar %d." (+ start-bar-number 1)))))
         

(defun goto-previous-bar-and-process-current-bar-and-play ()
  "Goto previous bar. Wrap that bar into a proper context, process it, and play it.
Does not support a prefix argument. Works for format 0 files as well as format 1 files."
  (interactive)
  (set-mark nil)
  (let ((start-bar-number (current-bar-number)))
      (goto-bar-internal (- start-bar-number 1))

      (if (member play-after-condition '(after-navigate after-navigate-and-edit))
         (progn 
           (message "Now at start of bar %d - playing %s in bar number %d..." 
                    (- start-bar-number 1) (channel-info-string channels-to-play) (- start-bar-number 1))
           (let* ((start-bar-number (current-bar-number))
                  (end-bar-number (current-bar-number))
                  (start-p (locate-initial-bar start-bar-number))
                  (end-p   (if end-bar-number (locate-final-bar end-bar-number) (locate-final-bar start-bar-number)))
                  )
             (setq selection-begin start-p) (setq selection-end end-p) 
             (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p end-p 'play)))
         (message "Now at the beginning of bar %d." (- start-bar-number 1)))))


; specialized function of process-selected-bars-laml-file-and-play
(defun first-number-of-string (str)
  (let ((p1 (find-in-string str 32)))
    (if p1 
        (string-to-number (substring str 0 p1))
        (string-to-number str))))

(defun current-bar-number ()
  (save-excursion
    (find-next-bar-number)))

; specialized function of process-selected-bars-laml-file-and-play
(defun last-number-of-string (str)
  (let ((p1 (find-in-string str 32)))
    (if p1 
        (string-to-number (substring str p1 (length str)))
        nil)))

; (defun locate-initial-bar (bar-number)
;  (save-excursion
;    (goto-char (point-min))
;    (let ((pos (search-forward (concat "\"" (number-to-string bar-number) ":") nil t)))
;      (beginning-of-line 1)
;      (point))))
; 
; (defun locate-final-bar (bar-number)
;  (save-excursion
;    (goto-char (point-max))
;    (let ((pos (search-backward (concat "\"" (number-to-string bar-number) ":") nil t)))
;      (end-of-line 1)
;      (point))))

(defun locate-initial-bar (bar-number)
 "Return start point of bar-number"
 (save-excursion
   (beginning-of-line 1) (backward-up-list 1)  ; now at beginning of the current MidiTrack
   (let ((pos (search-forward (concat "\"" (number-to-string bar-number) ":") nil t)))
     (beginning-of-line 1)
     (point))))

(defun locate-final-bar (bar-number)
 "Return end point of bar-number"
 (save-excursion
   (beginning-of-line 1) (backward-up-list 1) (forward-sexp 1)  ; now at end of the current MidiTrack
   (let ((pos (search-backward (concat "\"" (number-to-string bar-number) ":") nil t)))
     (beginning-of-line 1) (forward-sexp 1)    ; if (end-of-line 1) is used it conflicts with a possible score comment
     (point))))

(defun process-selected-laml-file-certain-channels-and-play (channels)
  "Wrap the selected part of a midl file into a proper context, process it, and play it.
Works only for format 0 files."
  (interactive "sPlay which channels: (space-separated channel numbers): ")
  (let ((start-p (min (point) (mark)))
        (end-p (max (point) (mark)))
        (channel-numbers (mapcar (function as-number ) (string-to-list-separated-by 32 channels)))
       )
    (setq selection-begin start-p) (setq selection-end end-p) 
    (process-laml-file-and-present-from-to channel-numbers 100 start-p end-p 'play)))

(defun process-selected-laml-file-certain-channels-and-illustrate (channels)
  "Wrap the selected part of a midl file into a proper context, process it, and illustrate it."
  (interactive "sPlay which channels: (space-separated channel numbers): ")
  (let ((start-p (min (point) (mark)))
        (end-p (max (point) (mark)))
        (channel-numbers (mapcar (function as-number ) (string-to-list-separated-by 32 channels)))
       )
    (setq selection-begin start-p) (setq selection-end end-p) 
    (process-laml-file-and-present-from-to channel-numbers 100 start-p end-p 'illustrate)))

(defun process-bars-laml-file-certain-channels-and-play (channels start-end-bar-string)
  "Wrap certain chanenls of some bars of midl file into a proper context, process it, and play it."
  (interactive "sPlay which channels: (space-separated channel numbers): 
sFrom bar number to bar number (sep. by space), one bar number, or the current bar: ")
  (let* ((start-bar-number-0 (first-number-of-string start-end-bar-string))
         (start-bar-number (if start-bar-number-0 start-bar-number-0 (current-bar-number)))
         (end-bar-number (last-number-of-string start-end-bar-string))
         (start-p (locate-initial-bar start-bar-number))
         (end-p   (if end-bar-number (locate-final-bar end-bar-number) (locate-final-bar start-bar-number)))
         (channel-numbers (mapcar (function as-number ) (string-to-list-separated-by 32 channels)))
        )
    (setq selection-begin start-p) (setq selection-end end-p) 
    (process-laml-file-and-present-from-to channel-numbers 100 start-p end-p 'play)))

(defun process-bars-laml-file-certain-channels-and-illustrate (channels start-end-bar-string)
  "Wrap certain channels of some bars of midl file into a proper context, process it, and play it."
  (interactive "sPlay which channels: (space-separated channel numbers): 
sFrom bar number to bar number (sep. by space), one bar number, or the current bar: ")
  (let* ((start-bar-number-0 (first-number-of-string start-end-bar-string))
         (start-bar-number (if start-bar-number-0 start-bar-number-0 (current-bar-number)))
         (end-bar-number (last-number-of-string start-end-bar-string))
         (start-p (locate-initial-bar start-bar-number))
         (end-p   (if end-bar-number (locate-final-bar end-bar-number) (locate-final-bar start-bar-number)))
         (channel-numbers (mapcar (function as-number ) (string-to-list-separated-by 32 channels)))
        )
    (setq selection-begin start-p) (setq selection-end end-p) 
    (process-laml-file-and-present-from-to channel-numbers 100 start-p end-p 'illustrate)))


(defun midi-step (n)
  "Play the next n midi events, and advance point through these events to the beginning of the next midi event.
It is assumed that the points is at the start parenthesis of the first midi event. 
Leave mark at the beginning of the section."
  (interactive "p")
  (setq previous-midi-step-length n)
  (save-excursion
    (let ((p1 (point)))
       (forward-sexp n)
       (setq selection-begin p1) (setq selection-end (point)) 
       (process-laml-file-and-present-from-to t 100 selection-begin selection-end 'play)
  ))
  (set-mark (point))
  (forward-sexp n)
  (while (looking-at-chars (point) '(9 10 13 32))   ; white space
         (forward-char 1)))

(defvar channels-to-play t "The boolean true (for all channels) or a list of integers in the interval [1..16].
  Boolean nil means all midi messages (assuming in practice that all events are NoteOn messages.
  The single and barwise play operations take the value of this variable into account.")

(defun set-channels-to-play (channel-list-str)
  "Set the channel(s) to play. Either all, space separated list of channel numbers, or a single channel"
  (interactive 
    (list (read-string (concat "Currently playing/navigating " (channel-info-string channels-to-play) ". After this command play "
               "(a space separated list of numbers or the symbol all): "))))
  (cond ((equal channel-list-str "all") 
           (setq channels-to-play t)
           (show-midl-mode-status-string) 
           (message "All channels will play."))
        (t (setq channels-to-play (mapcar (function string-to-number) (string-to-list-separated-by 32 channel-list-str)))
           (show-midl-mode-status-string)
           (message (concat "The following channels will play: " channel-list-str)))))

(defun set-channels-all ()
  "Reset channel selection such that all channels play"
  (interactive)
  (setq channels-to-play t)
  (show-midl-mode-status-string)
  (message "Resetting: All channels will play.")
)

(defun set-forms-to-play (form-name)
  "Set target-form-of-navigation based on the parameter form-name (an element name, or all) of type string."
  (interactive 
     (list
        (completing-read
            (if (equal target-form-of-navigation t)
                (format "Currently playing/navigating all forms. New name of MIDI LAML element, or all (space for completions): ") 
                (format "Currently playing/navigating the form: %s. New name of MIDI LAML element, or all (space for some completions): " 
                    (if (equal target-form-of-navigation t) "all" target-form-of-navigation)))
            (list "all" "NoteOn" "ControlChange" "ProgramChange" "ChannelKeyPressure" "PitchBendChange"
                  "SysEx" "Meta") nil nil)))
  (setq target-form-of-navigation
        (if (equal form-name "all") t form-name))
  (if (equal target-form-of-navigation t)
      (message "Navigating and playing all MIDI LAML elements.")
      (message "Navigating and playing the following MIDI LAML elements: %s" target-form-of-navigation)))
   

(defvar play-after-condition 'after-navigate 
  "A variable that controls if we actually play the current note/bar after navigating to it. Possible values: after-navigate, after-navigate-and-edit, never.")




(defun toggle-play-after-condition ()
  "Toggle play after navigation. Either PLAY after both navigation and editing, play only after navigation, or disabeling of all playing"
  (interactive)
  (setq play-after-condition (cond ((eq play-after-condition 'after-navigate) 'after-navigate-and-edit)
                                   ((eq play-after-condition 'after-navigate-and-edit) 'never)
                                   ((eq play-after-condition 'never) 'after-navigate)))
  (cond ((eq play-after-condition 'after-navigate) (message "Now play after navigation, but NOT after editing."))
        ((eq play-after-condition 'after-navigate-and-edit) (message "Now play after navigation and editing"))
        ((eq play-after-condition 'never) (message "Navigate and edit only. NO PLAYING.")))

  (show-midl-mode-status-string))

(defun play-after-condition ()
  "Reset playing condition to play only after navigation (not after editing)."
  (interactive)
  (setq play-after-condition 'after-navigate)
  (message "Resetting: Now play after navigation, but NOT after editing.")
  (show-midl-mode-status-string))

(defvar attribute-to-be-modified "note" "The name of the attribute to be modified by arrow left and arrow right")

; Not used - and not up to date:
(defvar possible-attribute-names (list  "note" "velocity" "duration" "absTime" "deltaTime" "pressure" "value" "number") 
  "All possible attribute names of all possible midi LAML form names")

; not used any more
(defun select-attribute-to-be-modified (attr-name-str)
 (interactive
   (list 
     (completing-read (concat "Current attribute to be modified: " 
                              attribute-to-be-modified ". "
                              "New attribute to be modified (space for possibilities): ")
                      '(("note" . nil) ("velocity" . nil) ("duration" . nil)))))
 (setq attribute-to-be-modified attr-name-str))


(defun toggle-attribute-to-be-modified ()
  (interactive)
  (if t
      (message "Disabled. Select attribute with Control Mouse Left")
      (progn  ; old code
        (let ((remaining (member attribute-to-be-modified possible-attribute-names)))
          (if remaining                 ; expect always true
              (let ((lgt-remaining (length remaining)))
                (cond ((= lgt-remaining 1)
                       (setq attribute-to-be-modified (car possible-attribute-names))
                       (show-midl-mode-status-string))
                      ((> lgt-remaining 1)
                       (setq attribute-to-be-modified (cadr remaining))
                       (show-midl-mode-status-string))
                      (t (error "toggle-attribute-to-be-modified: Should not happen"))))
            (error "toggle-attribute-to-be-modified: Should not happen")))
        (message "Now modifying attribute   %s" attribute-to-be-modified))))

(defvar midi-laml-forms
  (list"NoteOn" "NoteOff" "PolyKeyPressure" "ControlChange" "ProgramChange"
       "ChannelKeyPressure" "PitchBendChange"
       "AllSoundOff" "ResetAllControllers" "LocalControl" "AllNotesOff"
       "OmniOff"   "OmniOn"   "MonoMode"   "PolyMode"
       "SysEx"   "MTCQuarterFrame"   "SongPositionPointer" "SongSelect"
       "TuneRequest"  "TimingClock" "Start" "Continue" "Stop" "ActiveSensing" "Meta")
  "The list of all possible MIDI LAML forms")

(defun set-attribute-or-form-for-interaction  ()
  "Set attribute-to-be-modified or target-form-of-navigation to the selected attribute/form name. Afterwards, the
surrounding form will be selected."
  (interactive)
  (mouse-drag-region last-command-event)   ; sets point position first
  (let* ((moused-attribute-name (name-under-point))
         (is-attribute (= (aref moused-attribute-name 0) ?')) ; if not, assume it is a form-name, which affect target-form-of-navigation
         (unquoted-moused-attribute-name 
             (if is-attribute (substring moused-attribute-name 1) moused-attribute-name)))
     
     ; The primary effect:
     (if is-attribute 
         (setq attribute-to-be-modified unquoted-moused-attribute-name)
         (if (equal unquoted-moused-attribute-name "MidiTrack")
             (setq target-form-of-navigation t)
             (if (member unquoted-moused-attribute-name midi-laml-forms)
                 (setq target-form-of-navigation unquoted-moused-attribute-name)
                 (error "Invalid form name has been selected"))))

     ; deltaTime attribute speciality:
     (if (and is-attribute (equal attribute-to-be-modified "deltaTime"))
            (let ((answer (completing-read "deltaTime Mode (simple or compensating): " (list "simple" "compensating") nil t )))
              (cond ((equal answer "simple") (setq delta-time-add-subtract-mode 'simple))
                    ((equal answer "compensating") (setq delta-time-add-subtract-mode 'compensating))
                    (t (error "set-attribute-to-be-modified. Should not happen")))))

     (show-midl-mode-status-string)
     
     ; Minibuffer feedback:
     (if is-attribute
         (message "Now the left and right arrow keys will modify the  %s  attribute %s"
              unquoted-moused-attribute-name
              (if (equal attribute-to-be-modified "deltaTime")
                  (if (eq delta-time-add-subtract-mode 'simple)
                       "- affecting the time of all successor"
                       "- NOT affecting the time of any successor")
                  ""))
         (message "Now the up and down arrows navigate between  %s  forms" 
                  (if (equal target-form-of-navigation t) "ALL" target-form-of-navigation)))
                  
     (backward-up-list 1)))
     

(defun string-to-list-separated-by (sep-char str)
  "Return the list of strings in str separated by one or more sep-char occurrences. 
str is not supposed to start with sep-char."
  (if (= (length str) 0)
      nil
      (filter (function (lambda (x) (not (empty-string-p x)))) 
        (string-to-list-separated-by-1 sep-char (eliminate-string-prefix (list sep-char) str)))))

(defun empty-string-p (x)
 (and (stringp x) (equal x "")))
 
(defun string-to-list-separated-by-1 (sep-char str)
  (let ((p (find-in-string str sep-char))
        (lgt (length str)))
    (if p
        (cons (substring str 0 p)
              (string-to-list-separated-by-1 sep-char (eliminate-string-prefix (list sep-char) (substring str p lgt))))
        (cons str nil))))

(defun eliminate-string-prefix (char-lst str)
  "Eliminate all prefix characters that belong to char-lst. 
Return the suffix list."
  (let ((i 0)
        (lgt (length str)))
    (while (and (< i lgt) (memq (aref str i) char-lst))
           (setq i (+ i 1)))
    (substring str i lgt)))

(defun channel-list-as-string (channel-list)
  "Transform channel-list (a list of integers) to a space separated string (without parentheses).
Pass the boolean input t through this function."
  (cond ((eq channel-list t) t)
        ((listp channel-list) (list-to-string (mapcar (function int-to-string) channel-list) " "))
        (t (error "channel-list-as-string: Expects either boolean t, or a list of integers."))))

; if mode is compact, a slightly short channel info string is returned. Good for status bar.
(defun channel-info-string (channel-list &optional mode)
  (if (eq mode 'compact)
      (cond ((eq channel-list t) "all")
            ((and (listp channel-list) (= (length channel-list) 1)) 
                   (concat (list-to-string (mapcar (function int-to-string) channel-list) " ")))
            ((and (listp channel-list) (> (length channel-list) 1))
                  (concat  (list-to-string (mapcar (function int-to-string) channel-list) " ")))
            (t "???"))
      (cond ((eq channel-list t) "all channels")
            ((and (listp channel-list) (= (length channel-list) 1))
                  (concat "channel" " " (list-to-string (mapcar (function int-to-string) channel-list) " ")))
            ((and (listp channel-list) (> (length channel-list) 1))
                  (concat "channels" " " (list-to-string (mapcar (function int-to-string) channel-list) " ")))
            (t "???"))))

(defun play-noteon-n (n)
  "Play the current noteOn midi message and in total n noteOn midi events. Do not move point.
It is assumed that point is located at the start parenthesis of a NoteOn message, or just before it."
  (interactive "p")
  (save-excursion
    (let ((p1 (point)))
       (setq selection-begin p1)
       (if (eq channels-to-play t) 
           (forward-noteon n)
           (forward-noteon n channels-to-play))
       (if (= n 1)
           (message (concat "Playing a single note in " (channel-info-string channels-to-play) " without moving point"))
           (message (concat "Playing %d notes in " (channel-info-string channels-to-play) " without moving point") n))
       (setq selection-end (point)) 
       (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) selection-begin selection-end 'play)))
)

(defvar use-single-note-midi-file-cache nil "A boolean variable that controls possible use of the single note midi file cache")
(defvar single-note-playing-via-shell-script t
   "A boolean variale that control how to play single notes. If t, play via shell script (presumably fast).
If false, generate a MIDI file with a single note. The latter approach can be used together with voice contexts (applying the correct voice on state-less sound cards).")

(defun toggle-single-note-playing-via-shell-script ()
  (interactive)
  (setq single-note-playing-via-shell-script (not single-note-playing-via-shell-script))
  (if single-note-playing-via-shell-script
      (message "Now playing single notes via special shell script")
      (message "Now playing a single note the same way as playing multiple notes (via a MIDI file)")))

(defun set-mode-of-playing-single-notes ()
  (interactive)
  (let ((answer (ask-user 
                  (concat (if single-note-playing-via-shell-script "Currently single notes are played via special shell script. "
                                                                   "Currently single notes are played as a MIDI file. ")
                          "Toggle? (yes/no): "))))
    (if answer 
       (toggle-single-note-playing-via-shell-script)
       (message (if single-note-playing-via-shell-script "Single notes are still played via special shell script "
         "Single notes are still played as a MIDI file ")))))

(defun play-single-noteon-maybe-cached ()
  "Assume that point is located at the initial parenthesis of a NoteOn message. Do not move point.
Play it either via play-note-on, or if possible, via chached midi files in midi-temp/single-notes"
  (interactive)
  (if single-note-playing-via-shell-script
      (progn
        (beginning-of-line 1)
        (highlight-this-line)
        (let ((dur (attribute-of-current-form 'duration)))
          (if use-single-note-midi-file-cache
              (let* ((abs-time (attribute-of-current-form 'absTime))
                     (channel  (attribute-of-current-form 'channel))
                     (note     (attribute-of-current-form 'note))
                     (vel      (attribute-of-current-form 'velocity))
                                        ; dur, see at outer level
                     (midi-file-path (concat (current-directory) "midi-temp/single-notes/" (single-midi-note-file-name abs-time channel note vel dur) "." "mid"))
                     )
                (if (file-exists-p midi-file-path)
                    (let ((midi-play-process (get-process midi-play-process-name)))
                      (if midi-play-process
                          (interrupt-process midi-play-process))
                      (play-midi-file-sync midi-file-path)
                      (redisplay)
                                        ; (sleep-for 1.0)  ; use together with async play-midi-file
                      )
                  (execute-single-midl-event))) ; before: (play-noteon-n 1)
            (execute-single-midl-event))

          ; Now deal with stopping the note playing again.
          ; Compute the duration, given assumptions about tempo and PPQN rate.
          ; The latency of the involved MIDI player is also estimated. 
          (let* ((ppqn 480)       ; more generally, take from context.
                 (bpm 120)        ; beats = quarter notes per minutes (= tempo). More generally, take from context. 
                 (tempo-mikro-sek-per-qn (/ 60000000.0 bpm))
                 (time-mikro-sek-per-tick (/ tempo-mikro-sek-per-qn ppqn))
                 (duration-seconds (/ (* (as-number dur) time-mikro-sek-per-tick) (float 1000000)))
                 (midi-player-latency (/ command-line-player-startup-latency 2))
                )
            (run-at-time (+ midi-player-latency duration-seconds) nil
                         (function (lambda () 
                                     (if pending-off-args
                                         (start-process "PlayOff" nil cygwin-shell "-c" (concat div-send-midi-exe SP pending-off-args )))
                                     (setq pending-off-args nil)))))
          ))
      (process-single-note-and-play)))


(defun process-single-note-and-play () 
  "Assume that the cursor point is located at the initial parenthesis of a NoteOn message.
Process and play it. Alternative to play-single-noteon-maybe-cached, which uses a specialized shell script to play a single note.
This function creates a full midi file with a single note."
  (interactive)
  (let* ((start-p (point))
         (end-p (progn (forward-sexp 1) (point)))
        )
    (setq selection-begin start-p) (setq selection-end end-p) 
    (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p end-p 'play) 
    (highlight-this-region start-p end-p)
  )
)
      

(defun single-midi-note-file-name (abs-time channel note vel dur)
  "Form an return a single note file name. All paramters are assumed to be strings."
  (concat channel "-" note "-" vel "-" dur))

; Not used anymore. Replaced by next-midi-message-and-play.
; (defun next-noteon-and-play ()
;   "Go to next noteOn message and play it."
;   (interactive)
;   
;   ; forwarding: Takes into consideration if point already is on a channel-relevant NoteOn message:
;   (if (not (and (member (current-form-name) (list "NoteOn")) 
;                 (appropriate-current-channel channels-to-play)))
;       (if (eq channels-to-play t) 
;           (forward-noteon 0)
;           (forward-noteon 0 channels-to-play))
;       (if (eq channels-to-play t) 
;           (forward-noteon 1)
;           (forward-noteon 1 channels-to-play)))
; 
;   (highlight-this-line)
; 
;   (if play-after-condition
;       (progn 
;         (message (concat "Playing a single NoteOn in " 
;                          (channel-info-string channels-to-play)
;                          (if use-single-note-midi-file-cache " via the single note chache" " (non-cached)")))
;         (if (not (looking-at ")"))      ; end of track
;             (play-single-noteon-maybe-cached)   
;           (progn (beep) (message "At end of track")))))
; )

; Not used anymore. Replaced by previous-midi-message-and-play.
; (defun previous-noteon-and-play ()
;   "Go to previous noteOn message and play it."
;   (interactive)
; 
;   ; backwarding: Takes into consideration if point already is on a channel-relevant NoteOn message:
;   (if (not (and (member (current-form-name) (list "NoteOn")) 
;            (appropriate-current-channel channels-to-play)))
;       (if (eq channels-to-play t) 
;           (backward-noteon 0) 
;         (backward-noteon 0 channels-to-play))
;       (if (eq channels-to-play t) 
;           (backward-noteon 1) 
;         (backward-noteon 1 channels-to-play)))
; 
;   (highlight-this-line)
; 
;   (if play-after-condition
;      (progn 
;        (message (concat "Playing a single NoteOn in "
;                         (channel-info-string channels-to-play)
;                         (if use-single-note-midi-file-cache " via single note chache" " (non-cached)")))
;        (if (not (looking-at "MidiTrack")) ; beginning of track
;            (play-single-noteon-maybe-cached)
;          (progn (beep) (message "At beginning of track")))))
; )



(defvar target-form-of-navigation "NoteOn"
   "The name of forms which we navigate between using the commands next-midi-message-and-play and previous-midi-message-and-play.
A MIDI LAML element name (a string), or the value t (which means all possible forms).")


(defun define-navigation-predicate ()
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  (other-window 1)
  (find-file (concat laml-dir "styles/xml-in-laml/midi/custom-filter-sort/midi-navigation-predicates.el"))
  (goto-char (point-min)))

(defun reset-navigation-predicate ()
  (interactive)
;  (load-file (concat laml-dir "styles/xml-in-laml/midi/custom-filter-sort/reset-navigation-predicate.el"))
  (fset 'form-navigation-predicate (function (lambda () t)))
  (message "Navigation predicate is now constant true. Form-name = %s. Channel = %s."
            (if (equal target-form-of-navigation t) "ALL" target-form-of-navigation)  (channel-info-string channels-to-play))
)


(defun next-midi-message-and-play ()
  "Go to next noteOn message and play it.
If the cursour is located at a blank line, move the blank line instead of playing."
  (interactive)

  (if (at-blank-line (point)) 
      (move-blank-line 1)
      (if (not (looking-at end-context))
          (progn

            (if (looking-at "(MidiTrack") (end-of-line 1)) ; to get started after going as far back as possible.

            ; forwarding: Takes into consideration if point already is on a channel-relevant NoteOn message:
            (if (not (and (appropriate-current-form target-form-of-navigation) 
                          (appropriate-current-channel channels-to-play target-form-of-navigation)))
                (if (eq channels-to-play t) 
                    (forward-midi 0)
                  (forward-midi 0 channels-to-play))
              (if (eq channels-to-play t) 
                  (forward-midi 1)
                (forward-midi 1 channels-to-play)))

            (highlight-this-line)

            (if (and (equal target-form-of-navigation "NoteOn") (member play-after-condition '(after-navigate after-navigate-and-edit)))
                (progn  
                  ; (message (concat "Playing a single NoteOn in " 
                  ;                (channel-info-string channels-to-play)
                  ;                 (if use-single-note-midi-file-cache " via the single note chache" "")))
                  (if (not (looking-at ")")) ; end of track
                      (play-single-noteon-maybe-cached)   
                    (progn (beep) (message "At end of track")))

                )
            )

            (message "")
     ))
  )
)


(defun previous-midi-message-and-play ()
  "Go to previous noteOn message and play it.
If the cursour is located at a blank line, move the blank line instead of playing."
  (interactive)

  (if (at-blank-line (point)) 
      (move-blank-line -1)
      (if (not (looking-at "(MidiTrack"))
          (progn
            ; backwarding: Takes into consideration if point already is on a channel-relevant NoteOn message:
            (if (not (and (appropriate-current-form target-form-of-navigation)  
                          (appropriate-current-channel channels-to-play target-form-of-navigation)))
                (if (eq channels-to-play t) 
                    (backward-midi 0) 
                  (backward-midi 0 channels-to-play))
              (if (eq channels-to-play t) 
                  (backward-midi 1) 
                (backward-midi 1 channels-to-play)))

            (highlight-this-line)

            (if (and (equal target-form-of-navigation "NoteOn") (member play-after-condition '(after-navigate after-navigate-and-edit)))
                (progn 
                  (message (concat "Playing a single NoteOn in "
                                   (channel-info-string channels-to-play)
                                   (if use-single-note-midi-file-cache " via single note chache" "")))
                  (if (not (looking-at "MidiTrack")) ; beginning of track
                      (play-single-noteon-maybe-cached)
                    (progn (beep) (message "At beginning of track")))))
            (message "")))
   )
)

(defun move-blank-line (n)
   "Move the currently selected blank line n positions. n may be negative, meaning move backward.
Precondition: The cursor point is at a blank line."
   (kill-line 1)
   (forward-line n)
   (insert CR)
   (previous-line 1)
)

(defvar use-form-navigation-predicate t
   "A boolean that controls if the form navigation predicate is actually called.
In some contexts it is necessary, temporarily, to disable use of the form navigation predicate.")


; Default predicate - can be redefined "interactively" in MIDI LAML: 
(defun form-navigation-predicate () t)

(defvar end-context (regular-expression '(concat (zero-or-more (char-set tab newline space return)) ")")))

(defun forward-midi (n &optional channels)
  "Move forward through n midi messages of a particular type, determined by the global variable target-form-of-navigation.
That is, pass over n channel-relevant midi messages, and find the next channel-relevant midi message.
If channels is provided (a list of channel integers) only move to a message that belongs to the given channels.
It is assumed that the point is located at the start parenthesis of a midi message, or just in front of it.
In this version, assume a strict line orientation (one midi message per line); This speeds up the forward navigation dramatically.
Display the progress of the navigation in the minibuffer."
  (interactive "p")
  (let ((cnt n)
        (at-end-of-track nil)
        (p-count 0) ; progress-count
       )
     (transient-mark-mode 0)   ; disable during backward navigation

     (advance-through-white-space-not-starting-inside-comment) (if (looking-at end-context) (setq at-end-of-track t))

     ; Go past n relevant NoteOn messages:
     (while (and (not at-end-of-track) (> cnt 0))
       (if (and (appropriate-current-form target-form-of-navigation) 
                (appropriate-current-channel channels target-form-of-navigation)
                (if use-form-navigation-predicate (form-navigation-predicate) t))
           (setq cnt (- cnt 1)))
       (if (not at-end-of-track) (forward-sexp 1)) 

       ; (advance-through-white-space)   ; TOO expensive
       ; (end-of-line 1) (forward-char 2) ; assume one midi message per line     THIS IS TOO SIMPLE in case of bar comments etc.
       (end-of-line 1) (advance-through-white-space-not-starting-inside-comment)

       (setq p-count (+ 1 p-count))       
       (display-navigation-progress p-count)
 
       (if (looking-at end-context) (setq at-end-of-track t))
     )

     ; move to next relevant NoteOn
     (while (and (not at-end-of-track) (not (and (appropriate-current-form target-form-of-navigation) 
                                                 (appropriate-current-channel channels target-form-of-navigation)
                                                 (if use-form-navigation-predicate (form-navigation-predicate) t)
                                                 )))
            (if (not at-end-of-track) (forward-sexp 1))

            ; (advance-through-white-space)   ; TOO expensive
            ; (end-of-line 1) (forward-char 2) ; assume one midi message per line   THIS IS TOO SIMPLE in case of bar comments etc.
            (end-of-line 1) (advance-through-white-space-not-starting-inside-comment)

            ; progress indicator
            (setq p-count (+ 1 p-count))
            (display-navigation-progress p-count)

            (if (looking-at end-context) (setq at-end-of-track t)))
     (transient-mark-mode 1)
   )
)



; easier than forward-midi, because (backward-sexp ...) lands on the start paren
; channels is a list of integers, or boolean t (all channels).
(defun backward-midi (n &optional channels)
  "Move backward through n midi messages of a particular type, determined by the global variable target-form-of-navigation.
If channels is provided (a list of channel integers) only move to a message that belongs to the given channels.
It is assumed that the point is located at the start parenthesis of a midi message, or just in front of it."
  (interactive "p")
  (let ((cnt n)
        (at-beginning-of-track (looking-at "MidiTrack"))
        (p-count 0) ; progress-count
       )
     (transient-mark-mode 0) ; disable during backward navigation

     (while (and (not at-beginning-of-track) (> cnt 0))
       (if (and (appropriate-current-form target-form-of-navigation) 
                (appropriate-current-channel channels target-form-of-navigation) 
                (if use-form-navigation-predicate (form-navigation-predicate) t)
                )
           (setq cnt (- cnt 1)))
       (if (not at-beginning-of-track) (backward-sexp 1))
       (setq p-count (+ 1 p-count))       
       (display-navigation-progress p-count)
       (if (looking-at "MidiTrack") (setq at-beginning-of-track t))
     )

     (while (and (not at-beginning-of-track) (not (and (appropriate-current-form target-form-of-navigation)
                                                       (appropriate-current-channel channels target-form-of-navigation) 
                                                       (if use-form-navigation-predicate (form-navigation-predicate) t)
                                                       )))
            (if (not at-beginning-of-track) (backward-sexp 1))

            (setq p-count (+ 1 p-count))
            (display-navigation-progress p-count)

            (if (looking-at "MidiTrack") (setq at-beginning-of-track t)))

     (transient-mark-mode 1)

     ; move to previous NoteOn
))

(defvar navigation-progress-granularity 80)

(defvar use-navigation-progress t "A boolean that controls if navigation progress is being used.")

(defun display-navigation-progress (p-count)
   (if (and use-navigation-progress (= 0 (mod p-count navigation-progress-granularity))) 
       (progn
         (message (concat 
                    (if (equal t target-form-of-navigation) "ALL" target-form-of-navigation)
                     ", " (channel-info-string channels-to-play)
                    (if t
                        (concat " (dot = " (as-string navigation-progress-granularity) ")")
                        "")
                    ": "
                    (make-string (/ p-count navigation-progress-granularity) ?.)
                  ))
         (redisplay))
     ))


; channels is a list of integers, or boolean t (all channels).
; forward-midi is a more general variant of this function. 
(defun forward-noteon (n &optional channels)
  "Move forward through n NoteOn midi messages. 
That is, pass over n channel-relevant NoteOn midi messages, and find the next channel-relevant NoteOn midi message.
If channels is provided (a list of channel integers) only move to a NoteOn that belongs to the given channels.
It is assumed that the point is located at the start parenthesis of a midi message, or just in front of it.
forward-midi is a more general similar function."
  (interactive "p")
  (let ((cnt n)
        (at-end-of-track nil)
       )
     (advance-through-white-space-not-starting-inside-comment) (if (looking-at ")") (setq at-end-of-track t))

     ; Go past n relevant NoteOn messages:
     (while (and (not at-end-of-track) (> cnt 0))
       (if (and (member (current-form-name) (list "NoteOn")) 
                (appropriate-current-channel channels target-form-of-navigation))  
           (setq cnt (- cnt 1)))
       (if (not at-end-of-track) (forward-sexp 1)) (advance-through-white-space-not-starting-inside-comment)
       (if (looking-at ")") (setq at-end-of-track t))
     )

     ; move to next relevant NoteOn
     (while (and (not at-end-of-track) (not (and (member (current-form-name) (list "NoteOn")) 
                                                 (appropriate-current-channel channels target-form-of-navigation))))
            (if (not at-end-of-track) (forward-sexp 1)) (advance-through-white-space-not-starting-inside-comment)
            (if (looking-at ")") (setq at-end-of-track t)))))


; easier than forward-noteon, because (backward-sexp ...) lands on the start paren
; channels is a list of integers, or boolean t (all channels).
(defun backward-noteon (n &optional channels)
  "Move forward through n NoteOn midi messages. 
If channels is provided (a list of channel integers) only move to a NoteOn that belongs to the given channels.
It is assumed that the point is located at the start parenthesis of a midi message, or just in front of it.
backward-midi is a more general similar function."
  (interactive "p")
  (let ((cnt n)
        (at-beginning-of-track (looking-at "MidiTrack"))
       )

     (while (and (not at-beginning-of-track) (> cnt 0))
       (if (and (member (current-form-name) (list "NoteOn")) 
                (appropriate-current-channel channels target-form-of-navigation))
           (setq cnt (- cnt 1)))
       (if (not at-beginning-of-track) (backward-sexp 1))
       (if (looking-at "MidiTrack") (setq at-beginning-of-track t))
     )

     (while (and (not at-beginning-of-track) (not (and (member (current-form-name) (list "NoteOn"))
                                                       (appropriate-current-channel channels target-form-of-navigation))))
            (if (not at-beginning-of-track) (backward-sexp 1))
            (if (looking-at "MidiTrack") (setq at-beginning-of-track t)))

     ; move to previous NoteOn
))

(defun forward-midi-message (&optional n)
  "Navigate forward through the next midi  messages, and position point at initial parenthesis. 
Return t if success, nil otherwise. If nil is returned the position of the cursor is undefined."
  (let ((n1 (if n n 1)))
    (condition-case nil
     (progn
       (forward-sexp n1)
       (advance-through-white-space-not-starting-inside-comment)
       (looking-at "("))
     (error nil))))

(defun backward-midi-message (&optional n)
  "Navigate backward through the next n midi messages, and position point at initial parenthesis.
Return t if success, nil otherwise. If nil is returned the position of the cursor is undefined."
  (let ((n1 (if n n 1)))
    (condition-case nil
     (progn
      (backward-sexp n1)
      (looking-at "("))
    (error nil))))


(defun appropriate-current-channel (channels target-form-of-navigation)
  "A helping function of forward-noteon and backward-noteon
channels is a list of integers, or boolean t (all channels).
Return if the channel of the current message member of channels?
target-form-of-navigation is a element name, or t (for all element names)."
  (let ((check-channels (if (null channels) nil t)))
    (if check-channels 
        (if (eq channels t)
            t
            (if (and (not (equal target-form-of-navigation t)) (member target-form-of-navigation (list "Meta" "SysEx")))
                t
                (if (looking-at end-context)
                    nil
                    (let ((ch (attribute-of-current-form 'channel)))
                      (if (null ch)
                          nil 
                        (member (string-to-number ch) channels))))
            )
        )
        t)))

(defun appropriate-current-form (target-form-of-navigation)
  "Point is just in front of current form.
Is this form appropriate relative to target-form-of-navigation."
  (if (looking-at end-context)
      nil
      (or (equal t target-form-of-navigation)
          (member (current-form-name) (list target-form-of-navigation)))))
 
; ---------------------------------------------------------------------------------------------------
; Interactive attribute modification.

(defvar delta-time-add-subtract-mode 'simple "Controls the behavior of do-add-to-attribute-of-current-form and do-subtract-from-attribute-of-current-form with deltaTime. Either simple or compensating (symbols). The value simple is simple adding to deltaTime. The value compensating moves the message without disturbing the tail.")

(defvar end-note-anchor nil 
   "Is the end of the note postion anchored.  If true, the end of the note is fixed.
Only the beginning of the note is moved in time. This may cause a 0 duration if the note is moved forward in time.")

(defun do-add-to-attribute-of-current-form (level)
  (interactive)
  (setq score-counter-transpostion (string-to-number (find-counter-transposition-of-current-buffer)))

  (let* ((attribute-name (string-to-symbol attribute-to-be-modified))
         (attribute-value (as-number (attribute-of-current-form attribute-name)))
         (form-name (string-to-symbol (current-form-name)))
         (delta (get-attribute-delta-of-level level form-name attribute-name))                 ;  level -> delta
         (space-delta (calculate-space-delta attribute-value (+ attribute-value delta)))       ;  growing or shrinking text on line - signed integer
        )
    (if (and (eq attribute-name 'deltaTime) (eq delta-time-add-subtract-mode 'compensating))
                                                              ; adding to deltaTime, reducing deltaTime of next message, etc.
        (progn                ; not yet good together with score comments.
          (add-delta-time-to-current-midi-message delta)

          (if end-note-anchor
              (add-to-attribute-of-current-form 'duration (- delta)))

          (highlight-this-line)
          (if (and (member play-after-condition '(after-navigate-and-edit)) (equal form-name 'NoteOn))
              (execute-single-midl-event))
          )
 
        (let ((done (add-to-attribute-of-current-form attribute-name delta)) ; The actual addition to the note attribute value - with boundary checks.
             )

          ; Space compensation due to potential change of note attribute value:
          (if done (do-attribute-mutate-space-compensation-after-attr-value space-delta attribute-name))

          ; Changing the duration due to end note anchoring:
          (if (and end-note-anchor (or (eq attribute-name 'absTime) (eq attribute-name 'deltaTime)))
              (let* ((existing-duration-value (as-number (attribute-of-current-form 'duration)))
                     (new-duration-value (+ existing-duration-value (- delta)))
                     (duration-space-delta (calculate-space-delta existing-duration-value new-duration-value)))
                (add-to-attribute-of-current-form 'duration (- delta))
                (do-attribute-mutate-space-compensation-after-attr-value duration-space-delta 'duration)))

          ; absTime moving:
          (if (eq attribute-name 'absTime)
              (while 
                  (let ((next-abs-time (abs-time-of-next-message attribute-name)))
                    (and attribute-value next-abs-time (> (+ attribute-value delta) next-abs-time)))
                (move-midi-message-forward)))

          ; Piano roll comment updating:
          (if (and done (or (eq attribute-name 'note)
                            (and (eq kind-of-midl-score 'piano-roll-note-and-velocity) (eq attribute-name 'velocity))
                            (and (eq kind-of-midl-score 'piano-roll-note-and-duration) (eq attribute-name 'duration))
                        )
                        (has-current-line-score-comment))
              (save-excursion
                (remove-score-comment-of-current-line)
                (decorate-midi-message-line-with-score t)
                (renew-note-info-name-of-current-line)))

          ; Update info attribute - and space compensate
          (if (and done (eq attribute-name 'note))
              (renew-note-info-name-of-current-line))

          (highlight-this-line) 

          ; Playing:
          (if (and done (member play-after-condition '(after-navigate-and-edit)) (equal form-name 'NoteOn))
              (play-single-noteon-maybe-cached))  ; earlier: (execute-single-midl-event)
  ))))


(defun do-subtract-from-attribute-of-current-form (level)
  "Subtract an amount from a numeric attribute. 
Point is assumed to be at - or just before the initial parenthesis of the form." 
  (interactive)
  (setq score-counter-transpostion (string-to-number (find-counter-transposition-of-current-buffer)))

  (let* ((attribute-name (string-to-symbol attribute-to-be-modified))
         (attribute-value (as-number (attribute-of-current-form attribute-name)))
         (form-name (string-to-symbol (current-form-name)))
         (delta (get-attribute-delta-of-level level form-name attribute-name))
         (space-delta (calculate-space-delta attribute-value (max 0 (+ attribute-value (- delta)))))
        )

    (if (and (eq attribute-name 'deltaTime) (eq delta-time-add-subtract-mode 'compensating))
                                          ; subtracting from deltaTime, reducing deltaTime of next message, etc.

        (progn                                                       ; not good together with score comments.
          (subtract-delta-time-from-current-midi-message delta)

          (if end-note-anchor
              (add-to-attribute-of-current-form 'duration delta))

          (highlight-this-line)
          (if (and (member play-after-condition '(after-navigate-and-edit)) (equal form-name 'NoteOn))
              (execute-single-midl-event))
        )

        (let ((done (add-to-attribute-of-current-form attribute-name (- delta)))) ; with boundary checks

          ; Space compensation due to potential change of note attribute value:
          (if (and done (> attribute-value 0))
              (do-attribute-mutate-space-compensation-after-attr-value space-delta attribute-name))

          ; Changing the duration due to end note anchoring:
          (if (and end-note-anchor (or (eq attribute-name 'absTime) (eq attribute-name 'deltaTime)))
              (let* ((existing-duration-value (as-number (attribute-of-current-form 'duration)))
                     (new-duration-value (+ existing-duration-value delta))
                     (duration-space-delta (calculate-space-delta existing-duration-value new-duration-value)))
                (add-to-attribute-of-current-form 'duration delta)
                (do-attribute-mutate-space-compensation-after-attr-value duration-space-delta 'duration)))

          ; absTime moving:
          (if (eq attribute-name 'absTime)
              (while 
                  (let ((prev-abs-time (abs-time-of-previous-message attribute-name)))
                    (and attribute-value prev-abs-time (< (+ attribute-value (- delta)) prev-abs-time)))
                (move-midi-message-backward)))

          ; Piano roll comment updating:
          (if (and done (or (eq attribute-name 'note)
                            (and (eq kind-of-midl-score 'piano-roll-note-and-velocity) (eq attribute-name 'velocity))
                            (and (eq kind-of-midl-score 'piano-roll-note-and-duration) (eq attribute-name 'duration))
                        )
                        (has-current-line-score-comment))
              (progn
                (remove-score-comment-of-current-line)
                (decorate-midi-message-line-with-score t)))

          ; Update info attribute - and space compensate
          (if (and done (eq attribute-name 'note))
              (renew-note-info-name-of-current-line))

          (highlight-this-line)

          ; Playing:
          (if (and done (member play-after-condition '(after-navigate-and-edit)) (equal form-name 'NoteOn))
              (play-single-noteon-maybe-cached))  ; earlier: (execute-single-midl-event)
          ))))

(defun toggle-end-note-anchoring ()
  (interactive)
  (setq end-note-anchor (not end-note-anchor))
  (if end-note-anchor 
      (message "Now end of note anchoring: When changing the time (abs or delta) or a note, the end of note position is anchored.")
      (message "Now NO end of not anchoring (normal).")))

(defun set-use-of-end-note-anchoring ()
  (interactive)
  (let ((answer (ask-user 
                  (concat (if end-note-anchor "Currently end note anchoring is used. "
                                              "Currently notes are not end anchored (normal). ")
                          "Toggle? (yes/no): "))))
    (if answer
       (toggle-end-note-anchoring)
       (message (if end-note-anchor 
           "End note anchoring is still used. "
           "Notes are still not end anchored (this is normal mode). ")))))

(defun add-delta-time-to-current-midi-message (delta-time)
 (let ((next-delta-time (attribute-of-next-message 'deltaTime)))
    (if next-delta-time
        (if (<= delta-time next-delta-time)
            (progn             ; add and subtract - maintain invariant
              (add-to-attribute-of-current-form 'deltaTime delta-time)
              (save-excursion
                (forward-midi-message 1)
                (add-to-attribute-of-current-form 'deltaTime (- delta-time)))
              )
          (progn ; add and subtract as much as possible (= next-delta-time) - maintain invariant
            (add-to-attribute-of-current-form 'deltaTime next-delta-time)
            (save-excursion
              (forward-midi-message 1)
              (add-to-attribute-of-current-form 'deltaTime (- next-delta-time)) ; becomes zero
              ) 
            (move-midi-message-forward) ; Exchange 
            (let ((av (as-number (attribute-of-current-form 'deltaTime))))  
              (set-attribute-of-current-form 'deltaTime 0)
              (save-excursion
                (backward-midi-message 1)
                (set-attribute-of-current-form 'deltaTime av)
                ))
            (add-delta-time-to-current-midi-message (- delta-time next-delta-time)) ; Recur
            ))
         (add-to-attribute-of-current-form 'deltaTime delta-time))))

(defun subtract-delta-time-from-current-midi-message (delta-time)
 (let ((this-delta-time (attribute-of-current-form 'deltaTime 'number)))
    (if (>= this-delta-time delta-time)
        (progn                 ; subtract and add - maintain invariant
          (add-to-attribute-of-current-form 'deltaTime (- delta-time))
          (save-excursion
            (forward-midi-message 1)
            (add-to-attribute-of-current-form 'deltaTime delta-time))
          )
      (progn ; subtract and add as much as possible (= next-delta-time) - maintain invariant
        (add-to-attribute-of-current-form 'deltaTime (- this-delta-time))
        (save-excursion
          (forward-midi-message 1)
          (add-to-attribute-of-current-form 'deltaTime this-delta-time) 
        ) 
        (let ((avp (attribute-of-previous-message 'deltaTime)))
         (if avp
             (progn
               (move-midi-message-backward) ; Exchange 
               (set-attribute-of-current-form 'deltaTime avp)
               (save-excursion
                 (forward-midi-message 1)
                 (set-attribute-of-current-form 'deltaTime 0)
                 ))
              (message "Encountered beginning of track."))
         (if avp (subtract-delta-time-from-current-midi-message (- delta-time this-delta-time))) ; Recur
        )))))

(defun attribute-of-next-message (attr-name)
 "Return the attribute value of attr-name (a symbol) of the following midi message.
Or nil, if no such message exists."
 (save-excursion
   (let ((forward-res (forward-midi-message 1)))
     (if forward-res
         (let ((res-string (attribute-of-current-form attr-name)))
           (if res-string (as-number res-string) nil))
         nil))))

(defun attribute-of-previous-message (attr-name)
 "Return the attribute value of attr-name (a symbol) of the previous midi message.
Or nil, if no such message exists."
 (save-excursion
   (let ((backward-res (backward-midi-message 1)))
     (if backward-res
         (let ((res-string (attribute-of-current-form attr-name)))
           (if res-string (as-number res-string) nil))
         nil))))
 

; Return the attribute value of attribute-name (a symbol) of the next midi message
(defun abs-time-of-next-message (attribute-name)
  (save-excursion
    (forward-midi-message 1)
    (let ((attr-val (attribute-of-current-form attribute-name)))
       (if attr-val (as-number attr-val) nil))))

; Return the attribute value of attribute-name (a symbol) of the previous midi message
(defun abs-time-of-previous-message (attribute-name)
  (save-excursion
    (let ((backward-res (backward-midi-message 1)))
       (if backward-res
           (let ((attr-val (attribute-of-current-form attribute-name)))
             (if attr-val (as-number attr-val) nil))
           nil))))

; A specialized procedure which inserts/deletes white space AFTER the currently selected form,
; depending on the value of d.
(defun do-attribute-mutate-space-compensation (d)
 (save-excursion
  (if (not (= d 0))
      (progn
        (forward-sexp 1)
        (cond ((< d 0) (insert (make-string (- d) 32)))
              ; ((> d 0) (delete-char d))   ; earlier version
              ((> d 0)
                 (let ((count d))
                   (while (> count 0)
                      (if  (not (looking-at-chars (point) (list 10 13))) (delete-char 1))
                      (setq count (- count 1)))))
              (t 'do-nothing))))))

(defun do-attribute-mutate-space-compensation-after-attr-value (d attribute-name)
 "Compensate the d extra character spaces are needed for the value of the attribute named attribtue-name. 
d can also be negative, in which case d fewer character spaces are needed."
 (save-excursion
    (end-of-line 1)
    (let ((pn (point)))
       (beginning-of-line 1)
       (let ((attr-found (search-forward (concat "'" (as-string attribute-name)) nil t)))
         (if attr-found  ; true under normal circumstances
             (progn
                (forward-sexp 1)  ; forward through attribute value
                (cond ((< d 0) (insert (make-string (- d) 32)))
                      ((> d 0)
                       (let ((count d))
                         (while (> count 0)
                           (if  (looking-at-chars (point) (list 32)) (delete-char 1))   ; earlier: (not (looking-at-chars (point) (list 32 10 13)))
                           (setq count (- count 1)))))
                      (t 'do-nothing))))))))

      

; The integer number n is changed to m.
; Calculate the extra amount of space needed. 
; Zero, positive or negative number.
(defun calculate-space-delta (n m)
  (let ((ciffers-n (ciffer-numbers n))
        (ciffers-m (ciffer-numbers m)))
    (- ciffers-m ciffers-n)))

; How many ciffers occur in n. If n is negative, the minus sign counts for one extra ciffer.
(defun ciffer-numbers (n)
  (let ((n1 (if (< n 0) (- n) n)))
    (if (= n1 0) 
        1
       (+ 1 (truncate (log10 n1)) (if (< n 0) 1 0)))))

; Move the current midi message one step backward, and ensure that it is still the currently selected mid message.
; Assume as a precondition that the current message is not the first one.
(defun move-midi-message-backward ()
  (if (looking-at "(") (backward-char 1))
  (while (looking-at " ") (backward-char 1))
  (forward-char 1)
  (let* ((p0 (point)))
      (end-of-line 1)
      (let* ((pn (point))
             (cur-line (buffer-substring-no-properties p0 pn)))
        (goto-char p0)
        (let ((kill-whole-line t))
          (delete-line 1))
        (backward-midi-message 1)
        (beginning-of-line 1)
        (insert cur-line)
        (insert CR)
        (backward-sexp 1))))

; Move the current midi message one step foward and ensure that it is still the currently selected mid message.
; Assume as a precondition that the current message is not the last one.
(defun move-midi-message-forward ()
  (if (looking-at "(") (backward-char 1))
  (while (looking-at " ") (backward-char 1))
  (forward-char 1)
  (let* ((p0 (point)))
      (end-of-line 1)
      (let* ((pn (point))
             (cur-line (buffer-substring-no-properties p0 pn)))
        (goto-char p0)
        (let ((kill-whole-line t))
          (delete-line 1))
        (forward-midi-message 1)
        (beginning-of-line 1)
        (insert cur-line)
        (insert CR)
        (backward-sexp 1))))

; Add delta to the value of attribute-name of current form.
; attribute-name is a symbol.
; Returns t if sucessful, else nil.
(defun add-to-attribute-of-current-form (attribute-name delta)
  (let* ((form-name (string-to-symbol (current-form-name)))
         (current-attribute-value-str (attribute-of-current-form attribute-name))
         (min-value (get-min-attribute-value form-name attribute-name))
         (max-value (get-max-attribute-value form-name attribute-name))
        )
    (if (or (not min-value) (not max-value))  ; invalid attribute-name
        (progn
           (beep)
           (message "Cannot set the attribute  %s  of a  %s  form" (symbol-to-string attribute-name) (current-form-name))
           nil)
        (if current-attribute-value-str
            (let* ((current-attribute-value (string-to-number current-attribute-value-str))
                   (new-value
                    (cond ((> delta 0) (min-maybe-unlimited max-value (+ delta current-attribute-value)))
                          ((< delta 0) (max-maybe-unlimited min-value (+ delta current-attribute-value)))
                          (t current-attribute-value))))
              (set-attribute-of-current-form attribute-name new-value)
              t)
            nil))))

(defun min-maybe-unlimited (t-or-number number)
  (if (eq t t-or-number) number (min t-or-number number)))

(defun max-maybe-unlimited (t-or-number number)
  (if (eq t t-or-number) number (max t-or-number number)))

; If nil is returned: attr-name invalid in form-name
; If t is returned: attr-name is unlimited minimum wise.
; If a number is returned: The number is the minimum.
(defun get-min-attribute-value (form-name attr-name)
  (cond ((eq form-name 'NoteOn)
           (cond ((eq attr-name 'note) 0)
                 ((eq attr-name 'channel) 1)
                 ((eq attr-name 'velocity) 0)
                 ((eq attr-name 'duration) 0)
                 ((eq attr-name 'deltaTime) 0)
                 ((eq attr-name 'absTime) 0)
                 (t nil)))
        ((eq form-name 'PitchBendChange)
           (cond ((eq attr-name 'channel) 1)
                 ((eq attr-name 'value) 0)
                 ((eq attr-name 'deltaTime) 0)
                 ((eq attr-name 'absTime) 0)
                 (t nil))) 
        (t (cond ((eq attr-name 'note) 0)
                 ((eq attr-name 'channel) 1)
                 ((eq attr-name 'velocity) 0)
                 ((eq attr-name 'duration) 0)
                 ((eq attr-name 'deltaTime) 0)
                 ((eq attr-name 'absTime) 0)
                 ((eq attr-name 'value) 0)
                 ((eq attr-name 'number) 0)
                 ((eq attr-name 'control) 0)
                 ((eq attr-name 'pressure) 0)
                 (t t)  ; earlier nil, but now unlimited minimum wise
           ))))

; If nil is returned: attr-name invalid in form-name
; If t is returned: attr-name is unlimited minimum wise.
; If a number is returned: The number is the minimum.
(defun get-max-attribute-value (form-name attr-name)
  (cond ((eq form-name 'NoteOn)
           (cond ((eq attr-name 'note) 127)
                 ((eq attr-name 'channel) 16)
                 ((eq attr-name 'velocity) 127)
                 ((eq attr-name 'duration) t)
                 ((eq attr-name 'deltaTime) t)
                 ((eq attr-name 'absTime) t)
                 (t nil)))
        ((eq form-name 'PitchBendChange)
           (cond ((eq attr-name 'channel) 16)
                 ((eq attr-name 'value) 16383)
                 ((eq attr-name 'deltaTime) t)
                 ((eq attr-name 'absTime) t)
                 (t nil))) 
        (t (cond ((eq attr-name 'note) 127)
                 ((eq attr-name 'channel) 16)
                 ((eq attr-name 'velocity) 127)
                 ((eq attr-name 'duration) t)
                 ((eq attr-name 'deltaTime) t)
                 ((eq attr-name 'absTime) t)
                 ((eq attr-name 'value) 127)
                 ((eq attr-name 'number) 127)
                 ((eq attr-name 'control) 127)
                 ((eq attr-name 'pressure) 127)
                 (t t)  ; earlier nil, but now unlimited minimum wise
              ))))

; ---------------------------------------------------------------------------------------------------

(defun get-attribute-delta-of-level (level form-name attribute-name)
  (cond ((and (eq form-name 'NoteOn) (eq attribute-name 'note))
           (cond ((= level 1) 1)
                 ((= level 2) 12)
                 ((= level 3) 24)
                 (t (error "get-delta-of-level: Unknown level of attribute %s of form %s" 
                           (symbol-to-string attribute-name) (symbol-to-string form-name)))))
        ((and (eq form-name 'NoteOn) (eq attribute-name 'velocity))
           (cond ((= level 1) 1)
                 ((= level 2) 10)
                 ((= level 3) 20)
                 (t (error "get-delta-of-level: Unknown level of attribute %s of form %s" 
                           (symbol-to-string attribute-name) (symbol-to-string form-name)))))
        ((and (eq form-name 'NoteOn) (eq attribute-name 'duration))
           (cond ((= level 1) 1)
                 ((= level 2) 100)
                 ((= level 3) 1000)
                 (t (error "get-delta-of-level: Unknown level of attribute %s of form %s" 
                           (symbol-to-string attribute-name) (symbol-to-string form-name)))))
        ((and (eq form-name 'PitchBendChange) (eq attribute-name 'value))
           (cond ((= level 1) 1)
                 ((= level 2) 100)
                 ((= level 3) 1000)
                 (t (error "get-delta-of-level: Unknown level of attribute %s of form %s" 
                           (symbol-to-string attribute-name) (symbol-to-string form-name)))))
        ((and (or (eq attribute-name 'deltaTime) (eq attribute-name 'absTime)))
          (let ((ppqn (find-pulses-per-quarter-note-of-current-buffer)))
           (cond ((= level 1) 1)
                 ((= level 2) 10)
                 ((= level 3) (if ppqn ppqn 100))
                 (t (error "get-delta-of-level: Unknown level of attribute %s of form %s"
                           (symbol-to-string attribute-name) (symbol-to-string form-name))))))
        ((eq attribute-name 'channel)
           (cond ((= level 1) 1)
                 ((= level 2) 8)
                 (t 8)))
        ((eq attribute-name 'pressure)
           (cond ((= level 1) 1)
                 ((= level 2) 10)
                 (t 20)))
        (t (cond ((= level 1) 1)
                 ((= level 2) 10)
                 (t 100)))))


; ---------------------------------------------------------------------------------------------------

; May be quite inefficient due to use of is-inside-comment. See more efficient version next.
(defun advance-through-white-space()   
  (while (or (looking-at-chars (point) '(9 10 13 32 59)) (is-inside-comment))   ; white space
         (forward-char 1)))

; More efficient than advance-through-white-space. Assumes, as a precondition that point is outside a comment when starting.
(defun advance-through-white-space-not-starting-inside-comment()   
  (let ((done (not (looking-at-chars (point) '(9 10 13 32 59)))))
  (while (not done)
     (if (looking-at ";") (end-of-line 1) (forward-char 1))
     (setq done (not (looking-at-chars (point) '(9 10 13 32 59)))))))

(defun next-similar-midi-step ()
  "Play the next previous-midi-step-length midi steps, continuing the previous midi step.
   Assume that the previous midi step left point at the beginning of a midi event."
  (interactive)
  (if previous-midi-step-length
      (progn 
        (save-excursion
          (let ((p1 (point)))
            (forward-sexp previous-midi-step-length)
            (setq selection-begin p1) (setq selection-end (point)) 
            (process-laml-file-and-present-from-to t 100 p1 (point) 'play)))
        (set-mark (point))
        (forward-sexp previous-midi-step-length)
        (while (not (looking-at-chars (point) '(40))) ; start parenthesis
          (forward-char 1)))
      (message "Cannot repeat previous step.")))

(defun process-selected-laml-file-and-play-again ()
  "Play previous selection again"
  (interactive)
  (if last-temporary-midi-file-path
      (play-midi-file last-temporary-midi-file-path)  ; Alternatively, but slower: (process-laml-file-and-present-from-to selection-begin selection-end)
      (message "Cannot play again.")))

(defvar extra-context "" "An extra context which can be assigned imperatively. Only used in format 0 and format 1 midi files.")

(defvar use-voice-context nil "If true, process-laml-file-and-present-from-to extracts and uses voice context (PC and CC messages). 
   If false, no voice context is inserted. As of now, voice context is used for multipads and styles.")

(defun toggle-use-voice-context ()
  "The voice context may be provided in the beginning of the MIDI file. Toggle whether to take the voice context into consideration when playing" 
  (interactive)
  (setq use-voice-context (not use-voice-context))
  (show-midl-mode-status-string)
  (if use-voice-context
      (message "Now using voice context when playing parts of a MIDI LAML file.")
      (message "Now disregarding the voice context when playing parts of a MIDI LAML file.")))

(defun set-use-of-voice-context ()
  (interactive)
  (let ((answer (ask-user 
                  (concat (if use-voice-context "Currently the voice context is used. " "Currently the voice context is NOT used. ")
                          "Toggle? (yes/no): "))))
    (if answer
        (toggle-use-voice-context)
        (message (if use-voice-context "The voice context is still used. " "The voice context is still NOT used. ")))))


(defvar start-play-time 0 "Global variable used by stop-midi-playing to find stopping location. Assigned by process-laml-file-and-present-from-to")

(defvar play-residual-from-fixed-part nil "A flag that tells if we want to play the residual part, as represented in a possible fixed part. Only applies to SMF0 in absTime mode. 
   Best and most useful if no voice context is used and needed (because we do not yet attempt to find voice information in the residual file).")

(defun toggle-playing-fixed-part ()
  "Switch between playing fixed parts or not."
  (interactive)
  (setq play-residual-from-fixed-part (not play-residual-from-fixed-part))
  (show-midl-mode-status-string)
  (if play-residual-from-fixed-part
      (message "Playing includes the fixed residual part.")
      (message "Only the notes shown are played. The fixed residual part is NOT played."))
)

(defun process-laml-file-and-present-from-to (channels relative-tempo p1 p2 action &optional laml-action-procedures-0 do-not-reset-shadow)
  "Play or do other action from position p1 to p2. 
channels is a list of channel numbers (integers between 1 and 16) or boolean t (meaning all channels).
The parameter relative-tempo is a percentage number, 100 meaning play in natural tempo. Applies only to MIDI format 0 and format 1 files.
action is either the symbol play, illustrate, or none.
Depending on the value of the variable use-voice-context, this function can locate the relevant midi context and wraps it arround the selection.
laml-action-procedure-0 is a string which determines the set of Midi LAML action procedures.
If laml-action-procedure is not provided (or if nil) the standard action procedures are used."
  (interactive) 

  (show-midl-mode-status-string)
  (if (not do-not-reset-shadow) (reset-shadow-overlay))

  (if (< p1 p2)
   (let* ((cur-point (point))
          (midl-buf (current-buffer))
          (laml-action-procedures (if (null laml-action-procedures-0) "standard" laml-action-procedures-0))
          (selected-message-txt (buffer-substring-no-properties p1 p2))
          (first-time-of-selection (find-first-abs-time-selection p1 p2))
          (ppqn (find-pulses-per-quarter-note-of-current-buffer))
         )
     (cond
            ((current-buffer-is-standard-midifile-of-format 0)
              (let* ((header (header-of-format-0-from-current-buffer))
                     (mode (mode-of-standard-midi-file-current-buffer))
                     (hex-tempo-value (find-global-tempo-value))  ; The hex string in textual contents of the type 81 Meta event.
                     (scaled-tempo-value (scale-hex-tempo hex-tempo-value relative-tempo))
                     (scaled-tempo-context (format "(Meta 'deltaTime \"0\" 'type \"81\" \"%s\")" (int-to-hex-string scaled-tempo-value t)))  
                     (context (cond ((not use-voice-context) "")
                                    ((eq mode 'absTime)
                                       (find-local-voice-context-abs-time-and-reset-to 5))  ; earlier: (context-of-format-0-from-current-buffer)
                                    ((eq mode 'deltaTime) 
                                       (find-local-voice-context-delta-time))
                                    (t (error "process-laml-file-and-present-from-to: Unknown mode:" mode))))
 ;                    (last-time-of-context (if use-voice-context 1920 10))   ; earlier: (find-last-abs-time-of context)
                     (temp-file-path (concat (current-directory) "midi-temp/" "sel-temp.midl"))
                     (midi-file-path (concat (current-directory) "midi-temp/" "midi-temp/" "sel-temp.mid"))
                     (fixed-part (find-fixed-part-of-first-track-of-current-buffer))
                    )
               (setq last-temporary-midi-file-path midi-file-path)
               (ensure-directory-existence (current-directory) "midi-temp/")
               (ensure-directory-existence (concat (current-directory) "midi-temp/") "midi-temp/")
               (if (file-exists-p temp-file-path) (delete-file temp-file-path))
               (cond ((and (eq mode 'absTime) fixed-part play-residual-from-fixed-part first-time-of-selection )               ; new as of april 2, 2011.
                        (let* ((last-time-of-selection (find-last-abs-time-selection p1 p2))
                               (current-project-name (file-name-proper (file-name-proper (current-file))))
                               (fixed-path-full-path (concat (current-directory) current-project-name "/" "fixed-parts/" fixed-part))
                               (buf (make-a-file-from-laml-template 
                                    "sel-temp.midl"
                                    (concat (current-directory) "midi-temp/")
                                    "midi-file-with-cleaning-and-channel-selection-and-fixed-part"
                                    'midi-laml-mode
                                    (list
                                     (list "THE-FIRST-ABS-TIME"(as-string  first-time-of-selection))
                                     (list "THE-LAST-ABS-TIME" (as-string last-time-of-selection))
                                     (list "THE-FIXED-PART-PATH" (string-it fixed-path-full-path))
                                     (list "THE-HEADER" header)
                                     (list "THE-CONTEXT" (concat scaled-tempo-context CR context CR extra-context))
                                     (list "THE-SELECTION" selected-message-txt)
                                     (list "THE-CHANNELS" (if (and (symbolp channels) (eq channels t)) "#t" (concat "'(" (channel-list-as-string channels) ")")))
                                     (list "USE-VOICE-CONTEXT" (if use-voice-context "#t" "#f"))
                                     (list "TIME-MODE" "'abs-time")
                                    )
                                    (concat laml-dir "styles/xml-in-laml/midi/" "templates/"))))
                           (kill-buffer buf)))

                      ((and (eq mode 'absTime) first-time-of-selection)
                        (let ((buf (make-a-file-from-laml-template 
                                    "sel-temp.midl"
                                    (concat (current-directory) "midi-temp/")
                                    "midi-file-with-cleaning-and-channel-selection"
                                    'midi-laml-mode
                                    (list
                                     (list "THE-HEADER" header)
                                     (list "THE-CONTEXT" (concat scaled-tempo-context CR context CR extra-context))
                                     (list "THE-SELECTION" selected-message-txt)
                                     (list "THE-CHANNELS" (if (and (symbolp channels) (eq channels t)) "#t" (concat "'(" (channel-list-as-string channels) ")")))
                                     (list "USE-VOICE-CONTEXT" (if use-voice-context "#t" "#f"))
                                     (list "TIME-MODE" "'abs-time")
                                    )
                                    (concat laml-dir "styles/xml-in-laml/midi/" "templates/"))))
                           (kill-buffer buf)))
 
                      ((eq mode 'absTime)
                         (error "Could, of some reason not find the first time of selection"))
 
                      ((and (eq mode 'deltaTime))
                        (let ((buf (make-a-file-from-laml-template 
                                    "sel-temp.midl"
                                    (concat (current-directory) "midi-temp/")
                                    "midi-file-with-cleaning-and-channel-selection" 
                                    'midi-laml-mode
                                    (list
                                     (list "THE-HEADER" header)
                                     (list "THE-CONTEXT" (concat scaled-tempo-context CR context CR extra-context))
                                     (list "THE-CHANNELS" (if (and (symbolp channels) (eq channels t)) "#t" (concat "'(" (channel-list-as-string channels) ")")))
                                     (list "USE-VOICE-CONTEXT" (if use-voice-context "#t" "#f"))
                                     (list "TIME-MODE" "'delta-time")
                                     (list "THE-SELECTION" selected-message-txt)
                                     )
                                    (concat laml-dir "styles/xml-in-laml/midi/" "templates/"))))
                           (kill-buffer buf)))
 
                      (t (error "Not made yet"))
 
                )
                (copy-file path-to-midi-error-file midi-file-path t)   ; error midi file copying
                (laml-sync-process-file temp-file-path 'silent laml-action-procedures)
 
                (cond ((file-exists-p midi-file-path) ; with possible double name
                         (cond ((eq action 'play)
                                  (cond ((and (eq mode 'absTime) (eq midi-player 'command-line-player))
                                          (if use-live-cursor-movement
                                              (let ((use-navigation-progress nil)        ; DYNAMIC BINDING
                                                    (use-form-navigation-predicate nil)  ; DYNAMIC BINDING
                                                   )
                                                (let ((pause-times (live-noteon-pause-list-abs-time midl-buf p1 p2 channels))
                                                      )
                                                  (setq start-play-time (float-time))
                                                  (if (get-process midi-play-process-name) (progn (delete-process midi-play-process-name) (sit-for 0.1)))  ; sit-for: seems necessary for live cursor movement...
                                                  (play-midi-file midi-file-path)
                                                  (live-cursor-noteon-move pause-times midl-buf p1 p2 channels
                                                                           ppqn scaled-tempo-value)
                                                  ))
                                              (play-midi-file midi-file-path))
                                        )
 
                                        ((and (eq mode 'deltaTime) (eq midi-player 'command-line-player))
                                          (if use-live-cursor-movement
                                              (let ((use-navigation-progress nil)        ; DYNAMIC BINDING
                                                    (use-form-navigation-predicate nil)  ; DYNAMIC BINDING
                                                   )
                                                (let ((pause-times (live-noteon-pause-list-delta-time midl-buf p1 p2 channels))
                                                      )
                                                  (setq start-play-time (float-time))
                                                  (if (get-process midi-play-process-name) (progn (delete-process midi-play-process-name) (sit-for 0.1)))
                                                  (play-midi-file midi-file-path)
                                                  (live-cursor-noteon-move pause-times midl-buf p1 p2 channels
                                                                           ppqn scaled-tempo-value)
                                                  ))
                                              (play-midi-file midi-file-path))
                                        )
 
                                        (t   (play-midi-file midi-file-path))))
 
                               ((eq action 'illustrate) (illustrate-midi-file midi-file-path))
 
                               ((eq action 'none) 'do-nothing)
 
                               (t (error "process-laml-file-and-present-from-to: Unknown action"))
                              ))
                      (t (error "Process-laml-file-and-play: Should not happen:")))
 
 
           ))
 
           ((current-buffer-is-standard-midifile-of-format 1)               ; Ikke endnu tilpasset til ny cleaned-for-sectional-playing, og ny skabelon.
              (let* ((header (header-of-format-1-from-current-buffer))
                     (mode (mode-of-standard-midi-file-current-buffer))
                     (context-1 "")          ; earlier  (global-context-of-format-1-from-current-buffer-first-bar)
                     (context-2 (cond ((not use-voice-context) "")
                                      ((eq mode 'absTime)
                                       (find-local-voice-context-abs-time-and-reset-to 5)) ; earlier: (context-of-format-0-from-current-buffer)
                                      ((eq mode 'deltaTime) 
                                       (find-local-voice-context-delta-time))
                                      (t (error "process-laml-file-and-present-from-to: Unknown mode: %s" mode))))
                     ; (tempo-context (find-global-tempo-event))
                     (hex-tempo-value (find-global-tempo-value))  ; The hex string in textual contents of the type 81 Meta event.
                     (scaled-tempo-value (scale-hex-tempo hex-tempo-value relative-tempo))
                     (scaled-tempo-context (format "(Meta 'deltaTime \"0\" 'type \"81\" \"%s\")" (int-to-hex-string scaled-tempo-value t)))  
 ;                    (last-time-of-context 1920) ; earlier  (find-last-abs-time-of context-2)
                     (temp-file-path (concat (current-directory) "midi-temp/" "sel-temp.midl"))
                     (midi-file-path (concat (current-directory) "midi-temp/" "midi-temp/" "sel-temp.mid"))
                    )
               (setq last-temporary-midi-file-path midi-file-path)
               (ensure-directory-existence (current-directory) "midi-temp/")
               (ensure-directory-existence (concat (current-directory) "midi-temp/") "midi-temp/")
               (if (file-exists-p temp-file-path) (delete-file temp-file-path))
               (cond ((and (eq mode 'absTime) first-time-of-selection)
                        (let ((buf (make-a-file-from-laml-template 
                                    "sel-temp.midl"
                                    (concat (current-directory) "midi-temp/")
                                    "midi-file-with-cleaning-and-channel-selection"
                                    'midi-laml-mode
                                    (list
                                     (list "THE-HEADER" header)
                                     (list "THE-CONTEXT" (concat scaled-tempo-context CR context-1 CR context-2 CR extra-context))
                                     (list "THE-SELECTION" selected-message-txt)
                                     (list "THE-CHANNELS" (if (and (symbolp channels) (eq channels t)) "#t" (concat "'(" (channel-list-as-string channels) ")")))
                                     (list "USE-VOICE-CONTEXT" (if use-voice-context "#t" "#f"))
                                     (list "TIME-MODE" "'abs-time")
                                    )
                                    (concat laml-dir "styles/xml-in-laml/midi/" "templates/"))))
                           (kill-buffer buf)))
 
                      ((eq mode 'absTime)
                         (error "Could, of some reason not find the last time of context or the first time of selection"))
 
                      ((and (eq mode 'deltaTime))             
                        (let ((buf (make-a-file-from-laml-template 
                                    "sel-temp.midl"
                                    (concat (current-directory) "midi-temp/")
                                    "midi-file-with-cleaning-and-channel-selection" 
                                    'midi-laml-mode
                                    (list
                                     (list "THE-HEADER" header)
                                     (list "THE-CONTEXT" (concat scaled-tempo-context CR context-2 CR extra-context))
                                     (list "THE-CHANNELS" (if (and (symbolp channels) (eq channels t)) "#t" (concat "'(" (channel-list-as-string channels) ")")))
                                     (list "USE-VOICE-CONTEXT" (if use-voice-context "#t" "#f"))
                                     (list "TIME-MODE" "'delta-time")
                                     (list "THE-SELECTION" selected-message-txt)
                                     )
                                    (concat laml-dir "styles/xml-in-laml/midi/" "templates/")))
                             )
                           (kill-buffer buf)))
 
                      (t (error "Not made yet")))
                (copy-file path-to-midi-error-file midi-file-path t)   ; error midi file copying
                (laml-sync-process-file temp-file-path 'silent laml-action-procedures)
 
 
                (cond ((file-exists-p midi-file-path) ; with possible double name
                         (cond ((eq action 'play)
                                  (cond ((and (eq mode 'absTime) (eq midi-player 'command-line-player))
                                          (if use-live-cursor-movement
                                              (let ((use-navigation-progress nil)        ; DYNAMIC BINDING
                                                    (use-form-navigation-predicate nil)  ; DYNAMIC BINDING
                                                   )
                                                (let ((pause-times (live-noteon-pause-list-abs-time midl-buf p1 p2 channels))
                                                      )
                                                  (setq start-play-time (float-time))
                                                  (if (get-process midi-play-process-name) (progn (delete-process midi-play-process-name) (sit-for 0.1)))
                                                  (play-midi-file midi-file-path)
                                                  (live-cursor-noteon-move pause-times midl-buf p1 p2 channels
                                                                           ppqn scaled-tempo-value)
                                                  ))
                                              (play-midi-file midi-file-path))
                                        )
 
                                        ((and (eq mode 'deltaTime) (eq midi-player 'command-line-player))
                                          (if use-live-cursor-movement
                                              (let ((use-navigation-progress nil)        ; DYNAMIC BINDING
                                                    (use-form-navigation-predicate nil)  ; DYNAMIC BINDING
                                                   )
                                                (let ((pause-times (live-noteon-pause-list-delta-time midl-buf p1 p2 channels))
                                                      )
                                                  (setq start-play-time (float-time))
                                                  (if (get-process midi-play-process-name) (progn (delete-process midi-play-process-name) (sit-for 0.1)))
                                                  (play-midi-file midi-file-path)
                                                  (live-cursor-noteon-move pause-times midl-buf p1 p2 channels
                                                                           ppqn scaled-tempo-value)
                                                  ))
                                              (play-midi-file midi-file-path))
                                        )
 
                                        (t   (play-midi-file midi-file-path))))
 
                               ((eq action 'illustrate) (illustrate-midi-file midi-file-path))
 
                               ((eq action 'none) 'do-nothing)
 
                               (t (error "process-laml-file-and-present-from-to: Unknown action"))
                              ))
                      (t (error "Process-laml-file-and-play: Should not happen:")))
 
              )
           )
 
           ((current-buffer-is-multipad-format)
              (let* ((header (header-of-multipad-from-current-buffer))
                     (mode (mode-of-multipad-current-buffer))
                     (context (cond ((eq mode 'absTime)
                                       (find-local-voice-context-abs-time-and-reset-to 1920))  ; earlier: (context-of-multipad-from-current-buffer)
                                    ((eq mode 'deltaTime) 
                                       (find-local-voice-context-delta-time))
                                    (t (error "process-laml-file-and-present-from-to: Unknown mode: %s" mode))))
                     (last-time-of-context 1)  ; earlier: (find-last-abs-time-of context)
                     (temp-file-path (concat (current-directory) "midi-temp/" "sel-temp.midl"))
                     (midi-file-path (concat (current-directory) "midi-temp/" "midi-temp/" "sel-temp.mid"))
                    )
               (setq last-temporary-midi-file-path midi-file-path)
               (ensure-directory-existence (current-directory) "midi-temp/")
               (ensure-directory-existence (concat (current-directory) "midi-temp/") "midi-temp/")
               (if (file-exists-p temp-file-path) (delete-file temp-file-path))
               (cond ((and (eq mode 'absTime) first-time-of-selection last-time-of-context)
                        (let ((buf (make-a-file-from-laml-template 
                                    "sel-temp.midl"
                                    (concat (current-directory) "midi-temp/")
                                    "midi-file-for-selection-time-displace"
                                    'midi-laml-mode
                                    (list
                                     (list "THE-HEADER" header)
                                     (list "THE-CONTEXT" context)
                                     (list "THE-SELECTION" selected-message-txt)
                                     (list "THE-CHANNELS" (if (and (symbolp channels) (eq channels t)) "#t" (concat "'(" (channel-list-as-string channels) ")")))
                                     (list "THE-TIME" (int-to-string (- (- (- first-time-of-selection last-time-of-context) 1)))) ; negative time displacement...
                                     )
                                    (concat laml-dir "styles/xml-in-laml/midi/" "templates/"))))
                           (kill-buffer buf)))
 
                      ((eq mode 'absTime)
                         (error "Could, of some reason not find the last time of context or the first time of selection"))
 
                      ((and (eq mode 'deltaTime))   
                        (let ((buf (make-a-file-from-laml-template 
                                    "sel-temp.midl"
                                    (concat (current-directory) "midi-temp/")
                                    "midi-file-for-selection-no-time-displacement"
                                    'midi-laml-mode
                                    (list
                                     (list "THE-HEADER" header)
                                     (list "THE-CONTEXT" context)
                                     (list "THE-CHANNELS" (if (and (symbolp channels) (eq channels t)) "#t" (concat "'(" (channel-list-as-string channels) ")")))
                                     (list "THE-SELECTION" selected-message-txt)
                                     )
                                    (concat laml-dir "styles/xml-in-laml/midi/" "templates/"))))
                           (kill-buffer buf)))
 
                      (t (error "Not made yet")))
                (copy-file path-to-midi-error-file midi-file-path t)   ; error midi file copying
                (laml-sync-process-file temp-file-path 'silent laml-action-procedures)
 ;               (message (concat "A MIDI file has been generated in midi-temp. " (if (eq action 'play) "Now playing." "Now illustrating.")))
                (cond ((file-exists-p midi-file-path) ; with possible double name
                       (cond ((eq action 'play) (play-midi-file midi-file-path))
                             ((eq action 'illustrate) (illustrate-midi-file midi-file-path))
                             ((eq action 'none) 'do-nothing)
                             (t (error "process-laml-file-and-present-from-to: Unknown action"))))
                      (t (error "Process-laml-file-and-play: Should not happen:")))))
 
           ((current-buffer-is-style-format)
              (let* ((header (header-of-style-from-current-buffer))
                     (mode (mode-of-style-current-buffer))
                     (context (cond ((eq mode 'absTime)
                                       (find-context-in-style-setup-section))
                                    ((eq mode 'deltaTime) 
                                       (error "As of now, TyrosStyle only supports absTime mode"))
                                    (t (error "process-laml-file-and-present-from-to: Unknown mode: %s" mode))))
                     (last-time-of-context 0)
                     (temp-file-path (concat (current-directory) "midi-temp/" "sel-temp.midl"))
                     (midi-file-path (concat (current-directory) "midi-temp/" "midi-temp/" "sel-temp.mid"))
                    )
               (setq last-temporary-midi-file-path midi-file-path)
               (ensure-directory-existence (current-directory) "midi-temp/")
               (ensure-directory-existence (concat (current-directory) "midi-temp/") "midi-temp/")
               (if (file-exists-p temp-file-path) (delete-file temp-file-path))
               (cond ((and (eq mode 'absTime) first-time-of-selection last-time-of-context)
                        (let ((buf (make-a-file-from-laml-template 
                                    "sel-temp.midl"
                                    (concat (current-directory) "midi-temp/")
                                    "midi-file-for-selection-time-displace"
                                    'midi-laml-mode
                                    (list
                                     (list "THE-HEADER" header)
                                     (list "THE-CONTEXT" context)
                                     (list "THE-SELECTION" selected-message-txt)
                                     (list "THE-CHANNELS" (if (and (symbolp channels) (eq channels t)) "#t" (concat "'(" (channel-list-as-string channels) ")")))
                                     (list "THE-TIME" (int-to-string (- (- (- first-time-of-selection last-time-of-context) 1)))) ; negative time displacement...
                                     )
                                    (concat laml-dir "styles/xml-in-laml/midi/" "templates/"))))
                           (kill-buffer buf)))
 
                      ((eq mode 'absTime)
                         (error "Could, of some reason not find the last time of context or the first time of selection"))
 
                      ((and (eq mode 'deltaTime))  ; no yet relevant - but kept because it may be useful in the future
                        (let ((buf (make-a-file-from-laml-template 
                                    "sel-temp.midl"
                                    (concat (current-directory) "midi-temp/")
                                    "midi-file-for-selection-no-time-displacement"
                                    'midi-laml-mode
                                    (list
                                     (list "THE-HEADER" header)
                                     (list "THE-CONTEXT" context)
                                     (list "THE-CHANNELS" (if (and (symbolp channels) (eq channels t)) "#t" (concat "'(" (channel-list-as-string channels) ")")))
                                     (list "THE-SELECTION" selected-message-txt)
                                     )
                                    (concat laml-dir "styles/xml-in-laml/midi/" "templates/"))))
                           (kill-buffer buf)))
 
                      (t (error "Not made yet")))
                (copy-file path-to-midi-error-file midi-file-path t)   ; error midi file copying
                (laml-sync-process-file temp-file-path 'silent laml-action-procedures)
 ;               (message (concat "A MIDI file has been generated in midi-temp. " (if (eq action 'play) "Now playing." "Now illustrating.")))
                (cond ((file-exists-p midi-file-path) ; with possible double name
                       (cond ((eq action 'play) (play-midi-file midi-file-path))
                             ((eq action 'illustrate) (illustrate-midi-file midi-file-path))
                             ((eq action 'none) 'do-nothing)
                             (t (error "process-laml-file-and-present-from-to: Unknown action"))))
                      (t (error "Process-laml-file-and-play: Should not happen:")))))
 
           (t (error "Can only select, process and play a standard midi file of format 0 or 1, a Tyros Multipad, or a Tyros Style"))))))

;(defun scale-hex-tempo (hex-tempo-value scaling-factor)
;  (let ((tempo-uspqn (hex-string-to-int hex-tempo-value)))    ; the number of microseconds per quarter note.
;    (round (* scaling-factor (/ (float tempo-uspqn) 100)))))

(defun scale-hex-tempo (hex-tempo-value scaling-factor)
  "Return the scaled tempo, measured in 'number of microseconds per quarter note'. Scaling-factor is in percent, thus 100 means scale with 1.0"
  (let ((tempo-uspqn (hex-string-to-int hex-tempo-value)))    ; the number of microseconds per quarter note.
     (round (* (float tempo-uspqn) (/ 100.0 scaling-factor)))))

(defvar using-fixed-tempo t "Fixed tempo relies on the tempo setting in the variable relative-tempo-percentage-to-play.
   Non-fixed tempo (variable tempo) relies on the x-position of the cursor")
(defvar relative-tempo-percentage-to-play 100 "The relative tempo, in percent. Controls the playing tempo when sequences of midi events are played interactively")

(defun relative-percentage-playing-tempo ()
  (if using-fixed-tempo
      relative-tempo-percentage-to-play
      (centered-relative-tempo (current-mouse-pixel-position))))

(defun set-relative-tempo (new-relative-tempo-percentage)
  "Set the tempo - a percentage number. 100% is normal tempo as specified in the MIDL file."
  (interactive (list (read-from-minibuffer (concat "New relative tempo percentage (was " (as-string relative-tempo-percentage-to-play) "): "))))
  (if (>= (as-number new-relative-tempo-percentage) 5)
      (progn
        (setq relative-tempo-percentage-to-play (as-number new-relative-tempo-percentage))
        (show-midl-mode-status-string)
        (message "New relative tempo percentage: %d." relative-tempo-percentage-to-play))
      (message "The relative tempo must at least be 5%%. Nothing done.")))

(defun reset-relative-tempo ()
  "Reset the tempo to 100% - the tempo as specified in the MIDL file."
  (interactive)
  (setq relative-tempo-percentage-to-play 100)
  (show-midl-mode-status-string)
  (message "Resetting: Relative tempo percentage reset to %d." relative-tempo-percentage-to-play))

(defun toggle-fixed-tempo ()
  "Switch between fixed and variable tempo. Variable tempo is controlled by the x-position of the cursor arrow."
  (interactive)
  (setq using-fixed-tempo (not using-fixed-tempo))
  (show-midl-mode-status-string)
  (if using-fixed-tempo
      (message "Tempo is now fixed - independent of x-position of cursor")
      (message  "Tempo is now variable - it depends on the x-position of cursor"))
)

(defun add-to-relative-tempo (dt)
 (let ((new-relative-tempo-percentage-to-play (+ relative-tempo-percentage-to-play dt)))
  (if (>= new-relative-tempo-percentage-to-play 5)
      (progn
         (setq relative-tempo-percentage-to-play new-relative-tempo-percentage-to-play)
         (show-midl-mode-status-string)
         (message "New relative tempo percentage: %d." relative-tempo-percentage-to-play))
      (message "The relative tempo must at least be 5%%. Current releative tempo is %d." relative-tempo-percentage-to-play ))))

(defun goto-global-tempo-event()
   "Place the cursor point at the start parenthesis of a the first tempo Meta event in the buffer.
Return t if successful, else retur nil."
   (goto-char (point-min))
   (let ((search-res (search-forward " Tempo: " nil t)))
     (if search-res
         (progn
            (beginning-of-line 1)
            (while (looking-at-white-space (point)) (forward-char 1))
            (looking-at "(Meta"))
         nil)))

(defun find-global-tempo-event()
 "Return a text string with the global tempo event."
 (save-excursion
     (let ((res (goto-global-tempo-event)))
     (if res
         (progn
            (beginning-of-line 1)
            (let ((p0 (point)))
               (forward-sexp 1)
               (buffer-substring-no-properties p0 (point))))
         ""))))

(defun find-global-tempo-value ()
  "Return the global tempo as a hex encoded string. If the tempo event is not found, return the hex string corresponding to 120 BPM."
  (save-excursion
    (let ((res (goto-global-tempo-event)))
      (if res
          (textual-contents-of-current-form)
          "07 A1 20"))))


(defun find-first-abs-time-selection (p1 p2)
  (save-excursion
    (goto-char p1)
    (forward-sexp 1)
    (let ((pn (point)))
      (goto-char p1)
      (if (search-forward "'absTime" pn t)
          (progn
             (search-forward "\"")
             (string-to-number (name-under-point)))
          nil))))

(defun find-last-abs-time-selection (p1 p2)
  (save-excursion
    (goto-char p2)
    (beginning-of-line 1)  ; maybe not correct when at the end of the MIDL file.
    (backward-sexp 1)
    (if (search-forward "'absTime" p2 t)
        (progn
          (search-forward "\"")
          (string-to-number (name-under-point)))
      nil)))

(defun find-first-delta-time-selection (p1 p2)
  (save-excursion
    (goto-char p1)
    (forward-sexp 1)
    (let ((pn (point)))
      (goto-char p1)
      (if (search-forward "'deltaTime" pn t)
          (progn
             (search-forward "\"")
             (string-to-number (name-under-point)))
          nil))))             


(defun current-buffer-is-standard-midifile ()
  "Is the current buffer a standard midi file (as opposed to a tyros multipad)."
  (save-excursion
    (goto-char (point-min))
    (if (and (search-forward "(StandardMidiFile" nil t)
             (search-forward "(MidiHeader" nil t))
        t
        nil)))

; Is the current buffer a format f standard midi file.
; f is an integer number, such as 0, 1 or 2.
; It is assumed that the format is given as a text string of the format attribute in the MidiHeader.
; A TyrosMultiPad is not, in this context, a standard midi file.
(defun current-buffer-is-standard-midifile-of-format (f)
  (save-excursion
    (goto-char (point-min))
    (if (and (search-forward "(StandardMidiFile" nil t)
             (search-forward "(MidiHeader" nil t))
        (progn
          (backward-up-list 1)
          (let ((p1 (point))
                (p2 (progn (forward-sexp 1) (point))))
            (backward-sexp 1)
            (if (and (search-forward "'format" p2 t) (search-forward "\""))
                (let ((format-char (char-under-point)))
                  (= format-char (+ ?0 f)))
                nil)))
        nil)))

(defun header-of-format-0-from-current-buffer ()
  (save-excursion
    (goto-char (point-min))
    (if (and (search-forward "(StandardMidiFile" nil t)
             (search-forward "(MidiHeader" nil t))
        (progn
          (backward-up-list 1)
          (let ((p0 (point)))
             (forward-sexp 1)
             (buffer-substring-no-properties p0 (point))))
        nil)))

(defun context-of-format-0-from-current-buffer ()
  (save-excursion
    (goto-char (point-min))
    (if (and (search-forward "(StandardMidiFile" nil t)
             (search-forward "(MidiTrack" nil t))
      (let ((p0 (point)))
        (search-forward "(NoteOn" nil t)
        (backward-up-list 1)
        (buffer-substring-no-properties p0 (point)))
      nil)))




(defun find-last-abs-time-of (fragment)
  "Return the abs-time of the last midi event in fragment (a string). Returns an integer, or nil."
  (let ((search-res (string-search-backward fragment "'absTime")))
    (if search-res
        (let* ((p1 (find-in-string fragment 34 search-res))    ; 34 = '"'
               (p2 (find-in-string fragment 34 (+ p1 1))))
           (string-to-number (substring fragment (+ p1 1) p2)))
        nil)))

  
(defun mode-of-standard-midi-file-current-buffer ()
  "Return the mode (absTime, deltaTime), a symbol, of the current buffer, which
as a precondition is assumed to hold a StandardMidiFile of arbitrary format (0, 1)."
  (save-excursion
    (goto-char (point-min))
    (if (and (search-forward "(StandardMidiFile" nil t)
             (search-forward "(MidiHeader" nil t))
        (progn
          (backward-up-list 1)
          (let ((p0 (point)))
             (forward-sexp 1)
             (let ((p1 (point)))
                (goto-char p0)
                (if (search-forward "'mode" p1 t)
                    (progn
                       (search-forward "\"")
                       (string-to-symbol (name-under-point)))
                    nil))))
         nil)))

; --------------------------------------------------------------------------------------------------
; Format 1 stuff.

; Return a midi file format 0 header corresponding to the format 1 melody of the current buffer.
; A string.
(defun header-of-format-1-from-current-buffer ()
 (let ((mode-string (symbol-to-string (mode-of-standard-midi-file-current-buffer)))
       (ppqn-str (number-to-string (find-pulses-per-quarter-note-of-current-buffer)))
      )
  (concat 
    "(" "MidiHeader" " " "'format" " " "\"0\"" " " "'numberOfTracks" " " "\"1\"" " "
        "'pulsesPerQuarterNote" " " (string-it ppqn-str) " "
        "'mode" " " (string-it mode-string) ")"
        )) )

; Not used
(defun context-of-format-1-from-current-buffer ()
  (concat 
    (global-context-of-format-1-from-current-buffer-first-bar) CR
    (local-context-of-format-1-from-current-buffer-first-bar)
  )
)

; Return the entire first track
(defun global-context-of-format-1-from-current-buffer ()
  (save-excursion
    (goto-char (point-min))
    (if (and (search-forward "(StandardMidiFile" nil t)
             (search-forward "(MidiTrack" nil t))
      (let ((p0 (point)))
        (backward-up-list 1) 
        (forward-sexp 1)  ; through the entire MidiTrack
        (backward-char 1)
        (forward-sexp -1) ; back over end of track meta event
        (buffer-substring-no-properties p0 (- (point) 1)))
      nil)))




; If possible, return the first bar of first track. 
; If not possible, return entire contents of first track
(defun global-context-of-format-1-from-current-buffer-first-bar ()
  (save-excursion
    (goto-char (point-min))
    (if (and (search-forward "(StandardMidiFile" nil t)
             (search-forward "(MidiTrack" nil t))
        (let ((p0 (point)))
          (let ((res (search-forward "\"1:"))) ; start of bar 1
            (if res
                (progn
                  (beginning-of-line)
                  (buffer-substring-no-properties p0 (point)))
              (global-context-of-format-1-from-current-buffer))))
        "")))


(defun local-context-of-format-1-from-current-buffer ()
  (save-excursion
    (if (search-backward "(MidiTrack" nil t)
      (progn
         (search-forward "(MidiTrack" nil t)
         (let ((p0 (point)))
           (search-forward "(NoteOn" nil t)
           (backward-up-list 1)
           (buffer-substring-no-properties p0 (point))))
      "")) )


; (defun find-local-voice-context-abs-time-and-reset-to (new-time)
;   "Find and return the closest ProgramChange and corresponding ControlChange context in the current MidiTrack.
; Reset the abs-time to new-time (an integer).
; Operates relative to point.
; Returns a relatively brief string. Returns an empty string if an appropriate context cannot be found." 
;   (save-excursion
;     (if (search-backward "(MidiTrack" nil t)
;         (let ((track-start (point)) (track-end nil) (p1 nil) (p2 nil) (p-program-change nil))
;           (forward-sexp 1) 
;           (setq track-end (point))   ; end of track position
;           (backward-sexp 1)      ; now at start of MidiTrack
;           (search-forward "(MidiTrack" nil t)
;           
;           (if (search-forward "(ProgramChange" track-end t)
;               (progn
;                 (search-backward "(ProgramChange" nil t)
;                 (setq p-program-change (point))
;                 (let ((abs-time-program-change (abs-time-current-form)))
;                   (while (and (member (current-form-name) (list "ControlChange" "ProgramChange")) (= abs-time-program-change (abs-time-current-form)))
;                     (setq p1 (point))
;                     (backward-sexp 1))
;                   (goto-char p-program-change)
;                   (while (and (member (current-form-name) (list "ControlChange" "ProgramChange")) (= abs-time-program-change (abs-time-current-form)))
;                     (forward-sexp 1)
;                     (setq p2 (point))
;                     (while (not (looking-at "(")) (forward-char 1)))
;                   (reset-context-from-to-time (buffer-substring-no-properties p1 p2) abs-time-program-change new-time)))
;             "")
;           )
;           "")))

(defun find-local-voice-context-abs-time-and-reset-to (new-time)
  "Find and return the closest ProgramChange and corresponding ControlChange context in the current MidiTrack.
Reset the abs-time to new-time (an integer).
Operates relative to point.
Returns a relatively brief string. Returns an empty string if an appropriate context cannot be found." 
  (save-excursion
    (let ((p-cur (point)))
      (if (search-backward "(MidiTrack" nil t)
          (let ((track-start (point)) (track-end nil) (p1 nil) (p2 nil) (p-program-change nil))
            (forward-sexp 1) 
            (setq track-end (point))    ; end of track position
            (goto-char p-cur)
          
            (if (search-backward "(ProgramChange" track-start t)
                (progn
                  (setq p-program-change (point))
                  (let ((abs-time-program-change (abs-time-current-form)))
                    (while (and (member (current-form-name) (list "ControlChange" "ProgramChange")) (= abs-time-program-change (abs-time-current-form)))
                      (setq p1 (point))
                      (backward-sexp 1))
                    (goto-char p-program-change)
                    (while (and (member (current-form-name) (list "ControlChange" "ProgramChange")) (= abs-time-program-change (abs-time-current-form)))
                      (forward-sexp 1)
                      (setq p2 (point))
                      (while (not (looking-at "(")) (forward-char 1)))
                    (reset-context-from-to-time (buffer-substring-no-properties p1 p2) abs-time-program-change new-time)))
              "")
            )
        ""))))


(defun find-local-voice-context-delta-time ()
  "Find and return the closest ProgramChange and corresponding ControlChange context in the current MidiTrack.
Operates relative to point.
Returns a relatively brief string. Returns an empty string if an appropriate context cannot be found." 
  (save-excursion
    (if (search-backward "(MidiTrack" nil t)
        (let ((track-start (point)) (track-end nil) (p1 nil) (p2 nil) (p-program-change nil))
          (forward-sexp 1) 
          (setq track-end (point))   ; end of track position
          (backward-sexp 1)      ; now at start of MidiTrack
          (search-forward "(MidiTrack" nil t)
          
          (if (search-forward "(ProgramChange" track-end t)
              (progn
                (search-backward "(ProgramChange" nil t)
                (setq p-program-change (point))
                (let ((delta-time-program-change (delta-time-current-form)))
                  (while (and (member (current-form-name) (list "ControlChange" "ProgramChange")) (= delta-time-program-change (delta-time-current-form)))
                    (setq p1 (point))
                    (backward-sexp 1))
                  (goto-char p-program-change)
                  (while (and (member (current-form-name) (list "ControlChange" "ProgramChange")) (= delta-time-program-change (delta-time-current-form)))
                    (forward-sexp 1)
                    (setq p2 (point))
                    (while (not (looking-at "(")) (forward-char 1)))
                  (buffer-substring-no-properties p1 p2)))
            "")
          )
          ""))
)  

(defun abs-time-current-form ()
  "Return the absTime of the current form. Point must be a initial parenthesis."
  (save-excursion
     (let ((p1 (point)))
       (forward-sexp 1)
       (let ((p2 (point)))
          (find-first-abs-time-selection p1 p2)))))

(defun delta-time-current-form ()
  "Return the deltaTime of the current form. Point must be a initial parenthesis."
  (save-excursion
     (let ((p1 (point)))
       (forward-sexp 1)
       (let ((p2 (point)))
          (find-first-delta-time-selection p1 p2)))))


(defun current-form-name()
  "Return the name of the current form. Point must be pointing at the initial parenthesis, or just in front of it."
  (save-excursion
    (while (or (looking-at "(") (looking-at " ")) (forward-char 1))
    (name-under-point)))

(defun attribute-of-current-form (attribute-name &optional return-type)
   "Return the attribute named attribute-name of the current form.
If return-type is string (a symbol) return a string. If return-type is number (a symbol) return a number. The optional parameter return-type defaults to string.
Return nil if attribute is not found.
Point is assumed to be at - or just before the initial parenthesis of the form." 
 (let ((effective-return-type (cond ((null return-type) 'string)
                                    ((eq return-type 'number) 'number)
                                    ((eq return-type 'string) 'string)
                                    (t (error "attribute-of-current-form: Optional return-type must be number or string (a symbol).")))))
  (save-excursion
    (while (looking-at-chars (point) white-space-char-list) (forward-char 1))
    (let ((p0 (point)))
      (end-of-line 1) 
      (let* ((p1 (point))
             (res (ast-list-attribute (car (read-from-string (buffer-substring-no-properties p0 p1))) attribute-name)))
        (if res
            (cond ((eq effective-return-type 'number) (string-to-number res))
                  ((eq effective-return-type 'string) res))
            nil))))))

(defun attributes-of-current-form ()
   "Return the list of the attributes of the current form. Returns a property list. 
Point is assumed to be at - or just before the initial parenthesis of the form." 
  (save-excursion
   (let ((p0 (point))
         (res nil)
        )
     (forward-sexp 1)
     (condition-case nil
      (let ((pn (point)))
        (goto-char p0)
        (while (looking-at-chars (point) white-space-char-list) (forward-char 1)) ; initial white space
        (forward-char 1)  ; initial paren
        (forward-sexp 1)   ; Form name
        (while (looking-at-chars (point) white-space-char-list) (forward-char 1)) ; white space after form name
        (while (<= (point) pn)
           (if (looking-at-chars (point) (list 39))   ; at a single quote
               (let ((attr-name(substring (name-under-point) 1)))
                  (forward-sexp 1)
                  (while (looking-at-chars (point) white-space-char-list) (forward-char 1))   ; white space
                  (let* ((nup (name-under-point))
                         (attr-val (substring nup 1 (- (length nup) 1))))
                    (forward-sexp 1)
                    (while (looking-at-chars (point) white-space-char-list) (forward-char 1))
                    (setq res (cons (as-symbol attr-name) (cons attr-val res)))))
               (progn
                  (forward-sexp 1)
                  (while (looking-at-chars (point) white-space-char-list) (forward-char 1))))))
        (error 'do-nothing))
     res)))

(defun textual-contents-of-current-form ()
  "Return the textual contents of the current form, which is selected (by start parenthesis or just in front of it).
Assume as a precondition, the the textual contents is the last constituent of the selected form. 
Assume also that this text string exists in the form."  
  (save-excursion
    (forward-sexp 1)
    (let ((pn (point)))
      (backward-char 1) (backward-sexp 1) ; now at beginning of textual contents
      (let ((p1 (+ (point) 1)))  ; beginning position of text to be extracted.
         (goto-char pn) (backward-char 1)
         (while (not (looking-at-char-number 34))  ; "
                (backward-char 1)) 
         (buffer-substring-no-properties p1 (point))))))

(defun set-textual-contents-of-current-form (contents-string)
  "In the current form (selected by point at the start parenthesis or just in front of it) set the textuel contents to contents-string.
Assume as a precondition, the the textual contents is the last constituent of the selected form. 
Assume also that this text string exists in the form"
  (save-excursion
    (forward-sexp 1)
    (let ((p1 (point)))
      (goto-char p1) (backward-char 1) (backward-sexp 1) ; now at beginning of textual contents
      (kill-sexp 1)
      (insert (format "\"%s\"" contents-string)))))

(defun attribute-of-next-form (attribute-name &optional return-type)
 "Return the attribute named attribute-name of the next form. Similar to attribute-of-current-form."
 (let ((effective-return-type (cond ((null return-type) 'string)
                                    ((eq return-type 'number) 'number)
                                    ((eq return-type 'string) 'string)
                                    (t (error "attribute-of-next-form: Optional return-type must be number or string (a symbol).")))))
   (save-excursion
     (forward-sexp 1) (while (looking-at-chars (point) white-space-char-list) (forward-char 1))
     (attribute-of-current-form attribute-name return-type))))

(defun attribute-of-previous-form (attribute-name &optional return-type)
 "Return the attribute named attribute-name of the previous form. Similar to attribute-of-current-form."
 (let ((effective-return-type (cond ((null return-type) 'string)
                                    ((eq return-type 'number) 'number)
                                    ((eq return-type 'string) 'string)
                                    (t (error "attribute-of-previous-form: Optional return-type must be number or string (a symbol).")))))
   (save-excursion
     (backward-sexp 1)
     (attribute-of-current-form attribute-name return-type))))



(defun set-attribute-of-current-form (attr-name new-value)
  "In the current form, set the value of the attribute named attr-name (a symbol) to new-value (an integer).
A fatal error occurs if attr-name is not present in the current form."
  (let ((attr-name-str (symbol-to-string attr-name))
        (new-value-str (int-to-string new-value)))
   (save-excursion
     (let ((p0 (point)))
       (forward-sexp 1)
       (let ((p1 (point)))
         (backward-sexp 1)
         (let ((search-res (search-forward (concat "'" attr-name-str) p1 t)))
           (if search-res
               (progn
                 (while (not (looking-at "\"")) (forward-char 1)) (forward-char 1) ; now point is at first ciffer in attr value
                 (while (not (looking-at "\"")) (delete-char 1)) ; delete existing attribute value
                 (insert new-value-str))
               (error "Cannot find and change the value of attribute %s to %d in form %s." attr-name-str new-value (current-form-name)))))))))
                
  

(defun reset-context-from-to-time (context-string from-time to-time)
  "Be sure that all events in context-string starts at time time (an integer). Return the reset context-string"
   (let* ((temp-buf-name "*replace-buffer*")
          (temp-buf (if (get-buffer temp-buf-name)
                        (get-buffer temp-buf-name)
                        (generate-new-buffer temp-buf-name)))
          (cur-buffer (current-buffer))
          (from-time-string (if (numberp from-time) (int-to-string from-time) from-time))
          (to-time-string (if (numberp to-time) (int-to-string to-time) to-time))
          (abstime-clause-reg-exp
             (regular-expression  `(concat "'absTime" (one-or-more (char-set tab newline space return)) "\"" ,(number-to-string from-time) "\""))
          )
         )
    (set-buffer temp-buf)
    (erase-buffer)
    (insert context-string) (goto-char (point-min))
    (while (re-search-forward abstime-clause-reg-exp nil t)
       (replace-match  (concat "'absTime" " "  (concat "\"" to-time-string "\"")) nil nil))
    (let ((res (buffer-string)))
       (set-buffer cur-buffer)
       res)))

(defun local-context-of-format-1-from-current-buffer-first-bar ()
  (save-excursion
    (if (search-backward "(MidiTrack" nil t)
      (progn
         (search-forward "(MidiTrack" nil t)
         (let ((p0 (point)))
           (let ((res (search-forward "\"1:"))) ; start of bar 1
             (if res
                 (progn
                   (beginning-of-line)
                   (buffer-substring-no-properties p0 (point)))
               (local-context-of-format-1-from-current-buffer)))))
       "")))





; ---------------------------------------------------------------------------------------------------
; Multipad stuff.

(defun current-buffer-is-multipad-format ()
  (save-excursion
    (goto-char (point-min))
    (if (and (search-forward "(TyrosMultiPad" nil t)
             (search-forward "(MidiTrack" nil t))
        t
        nil)))

; Relative to the current position, find and return the context
(defun context-of-multipad-from-current-buffer ()
  (save-excursion
    (if (search-backward "(MidiTrack" nil t)
      (progn
         (search-forward "(MidiTrack" nil t)
         (let ((p0 (point)))
           (search-forward "(NoteOn" nil t)
           (backward-up-list 1)
           (buffer-substring-no-properties p0 (point))))
      nil)) )

(defun header-of-multipad-from-current-buffer ()
 "Return a midi file format 0 header corresponding to the multi pad of the current buffer. A string."
 (let ((mode-string (symbol-to-string (mode-of-multipad-current-buffer))))
  (concat 
    "(" "MidiHeader" " " "'format" " " "\"0\"" " " "'numberOfTracks" " " "\"1\"" " "
        "'pulsesPerQuarterNote" " " "\"1920\"" " "
        "'mode" " " (string-it mode-string) ")"
        )))

(defun mode-of-multipad-current-buffer ()
  "Returns the mode of the multipad buffer. A symbol."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "(TyrosMultiPad" nil t)
        (progn
          (backward-up-list 1)
          (let ((p0 (point)))
             (forward-sexp 1)
             (let ((p1 (point)))
                (goto-char p0)
                (if (search-forward "'mode" p1 t)
                    (progn
                       (search-forward "\"")
                       (string-to-symbol (name-under-point)))
                    nil))))
         nil)) )

; ---------------------------------------------------------------------------------------------------------------
; Tyros style stuff.

(defun current-buffer-is-style-format ()
  (save-excursion
    (goto-char (point-min))
    (if (and (search-forward "(TyrosStyle" nil t)
             (search-forward "(MidiSection" nil t))
        t
        nil)))

; Returns a symbol.
(defun mode-of-style-current-buffer ()
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "(TyrosStyle" nil t)
        (progn
          (backward-up-list 1)
          (let ((p0 (point)))
             (forward-sexp 1)
             (let ((p1 (point)))
                (goto-char p0)
                (if (search-forward "'mode" p1 t)
                    (progn
                       (search-forward "\"")
                       (string-to-symbol (name-under-point)))
                    nil))))
         nil)) )

(defun find-context-in-style-setup-section ()
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "(TyrosStyle" nil t)
        (if (search-forward "(MidiSection" nil t)
            (let ((p0 (search-forward "(StyleSetup" nil t)))
               (if p0
                   (progn
                     (backward-up-list 1)
                     (forward-sexp 1)
                     (let ((pn (point)))
                       (goto-char p0)
                       (forward-through-white-space)
                       (buffer-substring-no-properties (point) (- pn 1))))
                   ""))
             "")
         "")))
        
(defun header-of-style-from-current-buffer ()
 "Return a midi file format 0 header corresponding to the style of the current buffer. A string."
 (let ((mode-string (symbol-to-string (mode-of-style-current-buffer))))
  (concat 
    "(" "MidiHeader" " " "'format" " " "\"0\"" " " "'numberOfTracks" " " "\"1\"" " "
        "'pulsesPerQuarterNote" " " "\"1920\"" " "
        "'mode" " " (string-it mode-string) ")"
        )))

; ---------------------------------------------------------------------------------------------------------------
; Midi list length function

(defun display-length-of-midi-list (&optional extra-info)
  "Displays information about the length of the selected midi list (an integer) in the mini buffer.
Works in both (clean and exclusive) absTime and deltaTime modes.
Does not include the first deltaTime if applied on a selection of deltaTime midi messages.
extra-info is some info, which is displayed as a prefix, if provided. Presentation only."
  (interactive)
  (let* ((first-lgt (length-of-first-message-of-current-buffer))
         (lgt (length-of-midi-list))
         (mode (guess-processing-mode  (min (point) (mark)) (max (point) (mark))))  ; a string: "absTime" or "deltaTime"   
         (ppqn (find-pulses-per-quarter-note-of-current-buffer))  ; an integer
         (number-of-quater-notes (/ lgt ppqn))
         (rest-lgt (mod lgt ppqn))
         (exta-info-txt (if extra-info extra-info ""))
        )
    (cond 
          ((and (equal mode "absTime") (>= lgt ppqn))
             (message "%sThe length is %d pulses = %d quarter notes + %d pulses (ppqn=%d)." exta-info-txt lgt number-of-quater-notes rest-lgt ppqn))

          ((equal mode "absTime")
             (message "%sThe length is %d pulses." exta-info-txt lgt))

          ((and (equal mode "deltaTime") (>= lgt ppqn))
             (message "%sThe length is %d pulses = %d quarter notes + %d pulses (ppqn=%d).  If first event is included %d pulses = %d quarter notes + %d pulses."
                      exta-info-txt  lgt number-of-quater-notes rest-lgt  ppqn (+ lgt first-lgt) (/ (+ lgt first-lgt) ppqn) (mod (+ lgt first-lgt) ppqn)  ))

          ((and (equal mode "deltaTime") (>= (+ lgt first-lgt) ppqn))
             (message "%sThe length is %d pulses.  If first event is included %d quarter notes + %d pulses (ppqn=%d)."
                      exta-info-txt lgt (/ (+ lgt first-lgt) ppqn) (mod (+ lgt first-lgt) ppqn) ppqn  ))

          ((equal mode "deltaTime")
             (message "%sThe length is %d pulses: ppqn=%d.  If first event is include: %d pulses."
                      exta-info-txt lgt ppqn (+ lgt first-lgt)))

          (t (error "display-length-of-midi-list. Unknown mode. Should either be absTime or deltaTime (string)")))))


(defun length-of-midi-list ()
  "Return the length of the selected midi list (an integer).
Works in both (clean and exclusive) absTime and deltaTime modes.
Does not include the first deltaTime if applied on a selection of deltaTime midi messages."
  (let* ((selected-txt (buffer-substring-no-properties (min (point) (mark)) (max (point) (mark))))
         (selected-lst (car (read-from-string (concat "(" selected-txt ")"))))
         (mode (guess-processing-mode  (min (point) (mark)) (max (point) (mark)))) ; a string: "absTime" or "deltaTime"   
        )
    (cond ((equal mode "absTime")
            (let ((time-of-first (string-to-number (ast-list-attribute (car selected-lst) 'absTime)))
                  (time-of-last  (string-to-number (ast-list-attribute (car (last selected-lst)) 'absTime))))
              (- time-of-last time-of-first))              
          )
          ((equal mode "deltaTime")
              (if (> (length selected-lst) 1)
                  (accumulate-right (function +) 0 (mapcar (function (lambda (ast-lst) (string-to-number (ast-list-attribute ast-lst 'deltaTime)))) (cdr selected-lst)))
                  0))
          (t (error "length-of-midi-list: Unknown mode. Should be either absTime or deltaTime")))))


(defun length-of-first-message-of-current-buffer ()
  "Return the length of the first event of the selected midi list (an integer).
Works in both (clean and exclusive) absTime and deltaTime modes."
  (let* ((selected-txt (buffer-substring-no-properties (min (point) (mark)) (max (point) (mark))))
         (mode (guess-processing-mode  (min (point) (mark)) (max (point) (mark)))) ; a string: "absTime" or "deltaTime"
        )
    (cond ((equal mode "absTime")
             (string-to-number (ast-list-attribute (car (read-from-string (concat selected-txt))) 'absTime)))
          ((equal mode "deltaTime")
             (string-to-number (ast-list-attribute (car (read-from-string (concat selected-txt))) 'deltaTime)))
          (t (error " length-of-first-message-of-current-buffer: Unknown mode. Should be either absTime or deltaTime")))))

(defun ast-list-attribute (ast-lst attribute-name) 
  "Primitive version of the Scheme function ast-attribute. 
ast-lst is the emacs list form of midi message.
Return the attribute value (a string) of attribute-name (a symbol).
Return nil if attribute-name is not found in the ast-lst."
  (if (not (null ast-lst))
      (ast-list-attribute-1 (cdr ast-lst) attribute-name)))

(defun ast-list-attribute-1 (ast-lst attribute-name)
  (cond ((null ast-lst) nil)                                                                              ; empty list
        ((and (not (null (cdr ast-lst))) (symbolp (car ast-lst)) (eq (car ast-lst) attribute-name))      ; 'an "val"
            (cadr ast-lst))
        ((and (not (null (cdr ast-lst))) (listp (car ast-lst))                                            ; (quote an) "val2
              (= (length (car ast-lst)) 2) (eq (caar ast-lst) 'quote) (eq (cadr (car ast-lst)) attribute-name))
              (cadr ast-lst))
        (t                                                                                                ; everything else...
              (ast-list-attribute-1 (cdr ast-lst) attribute-name))))

; Use set-attribute-of-current-form instead. I forgot that I already made this function ...

; (defun set-ast-attribute (attr-name new-value)
;   "The initial parenthesis of an AST midi message is supposed to be selected.
; Change the value of the attribute named attr-name (a symbol or string) to new-value (a string or integer).
; Return t if successful, else nil. Leave point unchanged compared to its initial position."
;   (let ((p0 (point))
;         (attr-name-str (if (stringp attr-name) attr-name (symbol-to-string attr-name)))
;         (new-val-str (if (stringp new-value) new-value (int-to-string new-value))))
;     (forward-sexp 1)
;     (let ((pn (point)))
;        (goto-char p0)
;        (let ((res (search-forward (concat "'" attr-name-str) pn t)))
;           (if res
;               (progn
;                  (while (looking-at-chars (point) white-space-char-list) (forward-char 1))
;                  (if (looking-at "\"")
;                   (progn
;                      (kill-sexp 1)
;                      (insert "\"") (insert new-val-str) (insert "\"") (goto-char p0)
;                      t)
;                   (progn
;                      (goto-char p0)
;                      nil)))
;                (progn
;                      (goto-char p0)
;                      nil))))))
      
 
; ---------------------------------------------------------------------------------------------------------------
; Bar drawing.

; Internal global variable of bar drawing:
(defvar previous-bar-number nil) 

(defun add-bar-comments-to-current-track ()
  "Draw bar comments of the current MidiTrack.
The point is supposed to be located at the start parenthesis of a MidiTrack, or inside a MidiTrack form (depth 1 or 2, not inside a string).
This function relies on the info attributes of the MIDI message elements."
  (interactive)
  (fix-score-start-column-and-piano-roll-interval) ; assign score-start-column
  (setq previous-bar-number nil)
  (save-excursion
    (let ((p0 (point)))
      (if (looking-at "(MidiTrack")
          (progn (draw-bars-of-midi-track) "Done")
        (progn
          (goto-char p0)
          (backward-up-list 1)
          (if (looking-at "(MidiTrack")
              (progn (draw-bars-of-midi-track)
                     (message "Done")
                     "Done")
            (progn
              (goto-char p0)
              (backward-up-list 2)
              (if (looking-at "(MidiTrack")
                  (progn (draw-bars-of-midi-track)
                          (message "Done")
                          "Done"
                  )
                  (message "You should locate point at or inside a MidiTrack  (outside strings, max depth 2). Nothing done.")))))))))

; Precondition: The start parenthesis of a MidiTrack form is selected
(defun draw-bars-of-midi-track ()
  (let ((p0 (point)))
    (forward-sexp 1)
    (let ((pn (point)))
      (goto-char pn)
      (let ((next-bar-number (find-previous-bar-number)))
        (while (and next-bar-number (> (point) p0))
          (if (not (eq next-bar-number previous-bar-number))
              (progn
                (beginning-of-line 1) (next-line 1)
                (if (null previous-bar-number) (setq previous-bar-number (+ next-bar-number 1)))  ; only first time - at the end of the track
                (draw-bar-line previous-bar-number) 
                (beginning-of-line 1) (previous-line 1)
;                (end-of-line 1)
                (setq previous-bar-number next-bar-number)
                (setq next-bar-number (find-previous-bar-number)))
            (setq next-bar-number (find-previous-bar-number))))))))
       
(defun find-next-bar-number ()
  "Starting from point, find the next bar number from an info attribute. Moves point to a position after the info attribute name.
   Returns nil if no next bar number can be found."
  (if (and (search-forward "'info" nil t) (search-forward "\"" nil t))
      (let ((p0 (point))
            (pn (search-forward ":" nil t)))
        (string-to-number (buffer-substring-no-properties p0 (- pn 1))))
      nil))

(defun find-previous-bar-number ()
  "Starting from point, find the previous bar number from an info attribute. Moves point to a position before the info attribute name.
   Returns nil if no previous bar number can be found."
  (if (and (search-backward "'info" nil t) (search-forward "\"" nil t))
      (let ((p0 (point))
            (pn (search-forward ":" nil t)))
        (search-backward "'info" nil t)
        (string-to-number (buffer-substring-no-properties p0 (- pn 1))))
      nil))

(defun find-max-bar-number ()
  (save-excursion
    (beginning-of-line)  ; to exit strings 
    (backward-up-list 1) ; looking at MidiTrack
    (forward-sexp 1)     ; at end of MidiTrack
    (find-previous-bar-number)
  )
)
    

; precondition: point is at the beginning of the first line of the new bar
(defun draw-bar-line (bar-number)
  (let* ((offset (cond ((< bar-number 10) 2)
                       ((< bar-number 100) 1)
                       (t 0)))
         (bar-line-length (+ score-start-column offset 18))
        ) 
    (insert (concat "; "
                    (number-to-string bar-number) " "
                    (make-string bar-line-length ?-)
                    CR))))



(defun remove-bar-comments-from-current-track ()
  (interactive)
  (save-excursion
    (let ((p0 (point)))
      (if (looking-at "(MidiTrack")
          (progn (remove-bars-from-midi-track) "Done")
        (progn
          (goto-char p0)
          (backward-up-list 1)
          (if (looking-at "(MidiTrack")
              (progn (remove-bars-from-midi-track) "Done")
            (progn
              (goto-char p0)
              (backward-up-list 2)
              (if (looking-at "(MidiTrack")
                  (progn (remove-bars-from-midi-track) "Done")
                  (message "You should locate point at or inside a MidiTrack  (outside strings, max depth 2). Nothing done.")))))))))

(defun remove-bars-from-midi-track ()
  (while (search-forward "; " nil t)
    (if (is-point-inside-bar-comment-p)
        (remove-current-bar-comment))))

(defvar bar-comment-string (make-string score-start-column ?-))

(defun is-point-inside-bar-comment-p ()
  (save-excursion
    (beginning-of-line)
    (let ((p0 (point)))
      (end-of-line)
      (let ((pn (point)))
        (goto-char p0)
        (if (search-forward "; " pn t)
            (if (looking-at-chars (point) (list ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
                (if (search-forward bar-comment-string pn t)
                    t
                  nil)
              nil)
          nil)))))

(defun remove-current-bar-comment ()
  (beginning-of-line)
  (delete-line 1))


; ---------------------------------------------------------------------------------------------------------------

(defun goto-bar (bar-number)
  "Goto bar number bar-number. Relies on the text in the info attribute.
In that way, it works in both absTime and deltaTime mode. If necessaryl, goes into a MidiTrack."
  (interactive "nGoto bar number: ")
  (if (not (is-inside-track-of-standard-midi-file))
      (go-inside-nearest-midi-track))
  (set-mark nil)
  (beginning-of-line 1)
  (goto-bar-internal bar-number)
)

(defun goto-bar-internal (bar-number)
  "Goto bar number bar-number. Relies on the text in the info attribute.
In that way, it works in both absTime and deltaTime mode."
  (let ((p0 (point)))
    (if (looking-at-track-like-form)
        (goto-bar-1 bar-number p0) 
        (progn
          (goto-char p0)
          (backward-up-list 1)
          (if (looking-at-track-like-form)
              (goto-bar-1 bar-number p0)
              (progn
                (goto-char p0)
                (backward-up-list 2)
                (if (looking-at-track-like-form)
                    (goto-bar-1 bar-number p0)
                    (message "Cannot goto a bar number from this context."))))))))

(defun goto-abs-time (time)
  "From with a track, goto the midi event closest to absTime time. Works only in absTime mode."
  (interactive "nAbsolute time: ")
  (beginning-of-line 1)
  (if (equal "absTime" (find-processing-mode-of-current-buffer))
      (let ((pos (binary-search-for-abs-time time)))
        (if pos
            (goto-char pos)
          (message "Not possible. Are you located within a MidiTrack of an absTime mode StandardMidiFile?")))
      (message "This function can only be used in absTime mode.")))
  

(defvar track-like-form-names 
  (list "MidiTrack" "MainA" "MainB" "MainC" "MainD" "FillInAA" "FillInBB" "FillInCC" "FillInDD"  "FillInBA" "FillInAB" 
        "IntroA" "IntroB" "IntroC" 
        "EndingA" "EndingB" "EndingC" ))

(defun looking-at-track-like-form ()
  (save-excursion
    (while (looking-at-chars (point) (list 40           ; start-parenthesis
                                           9 10 13 32   ; white space
                                     )) (forward-char 1))
    (member (name-under-point) track-like-form-names )))
        

(defun goto-bar-1 (bar-number p0)
  (let ((bn (find-next-bar-number)))
    (while (and bn (not (eq bn bar-number)))
      (setq bn (find-next-bar-number)))
    (if (eq bar-number bn)
        (beginning-of-line 1)
        (progn (goto-char p0)
               (message (concat "Cannot goto bar " (number-to-string bar-number)))))))

; ---------------------------------------------------------------------------------------------------------------
; Capturing stuff

(defvar capture-dir "c:/users/kurt/Media/midi/Cantabile/")
(defvar capturing-program "Cantabile")

(defun cantabile-help ()
  (interactive)
  (let ((help-buffer (if (get-buffer "*Help*") (get-buffer "*Help*") (generate-new-buffer "*Help*"))))
    (delete-other-windows)
    (split-window-vertically)
    (set-buffer help-buffer)
    (set-window-buffer (other-window 1) help-buffer)
    (toggle-read-only -1)
    (erase-buffer)
    (insert-file-contents (concat laml-dir "styles/xml-in-laml/midi/" "help/" "cantabile-help.txt"))
    (toggle-read-only 1)
    (set-buffer-modified-p nil)
    (other-window 1)))

(defun capture-midi-file()
  "Watch and wait for a new file in capture-dir. If or when it appears, parse it with midi-to-laml and show it in a buffer"
  (interactive)
  (let ((initial-file-list (directory-files capture-dir))
        (current-file-list (directory-files capture-dir))
        (i 0)
       )
    (while (= (length initial-file-list) (length current-file-list))
      (sleep-for 2) 
      (setq i (+ i 1))
      (message (make-string i ?.))
      (setq current-file-list (directory-files capture-dir)))

    (let ((new-files (list-difference current-file-list initial-file-list)))
       (if (= (length new-files) 1)    
           (progn
             (sync-midi-file-to-laml-file "deltaTime" "0" nil (concat capture-dir (car new-files)) (concat capture-dir (file-name-proper (car new-files)) "." "midl"))
             (find-file (concat capture-dir (file-name-proper (car new-files)) "." "midl"))
           )
           (error "Could not locate new midi file")))))
    


(defun midl-mode-status-string ()
;  (if (is-midl-stack-inconsistent) (recover-midl-source-file-stack))
  (concat

     (if step-recording-p
         (concat "Step: 1/" (as-string (get-quantification-from-mouse-x-position)) ".")
         "")
 
     " Marker: " current-marker-letter "  "

     ; Midl stack:
     (if (and (null midl-source-file-stack) (null midl-source-file-ghost-stack))
         (concat "Stack: *")
       (concat "Stack: " (midl-stack-image midl-source-file-stack midl-source-file-ghost-stack))) "  "

     ; State:
     "Attribute: " (pad-string-to-length 9 attribute-to-be-modified) 

     (if use-voice-context " + " " - ")

     (cond ((eq play-after-condition 'after-navigate) "Play")
           ((eq play-after-condition 'after-navigate-and-edit) "PLAY")
           ((eq play-after-condition 'never) "No Play")
           (t "???")) 

     (if use-live-cursor-movement " > " "   ")

     "Channels: " (channel-info-string channels-to-play 'compact) (if play-residual-from-fixed-part " +FP. "
                                                                                                    ".     ") 
     "Tempo: " (as-string (relative-percentage-playing-tempo)) "%%" (if using-fixed-tempo "(Static) " "(Variable) " )
     "Shadow: " (brief-info-string-about-current-shadow) ". "
     "Midi I/O Ports: " (as-string midi-in-port) "/" (as-string midi-out-port)
  )
)

; An incomplete experiment:
;   `(:propertize " - " local-map ,menu-bar-keymap)
; (defvar menu-bar-keymap (make-sparse-keymap "MidiLamlMenuBar"))
; (define-key menu-bar-keymap [mouse-1] 'toggle-use-voice-context)

(defun midl-stack-image (midl-source-file-stack midl-source-file-ghost-stack)
  (concat
     (make-string (length midl-source-file-stack) ?-)
     "*"
     (make-string (length midl-source-file-ghost-stack) ?.))) 
  

(defun pad-string-to-length (lgt str &optional justification pad-char)
 "Pad the string str to a total length. Pad with pad-char.
justification is either left or right (a symbol), and it defaults to 'left"
 (let ((just (if (not justification) 'left justification))
       (pad-char-1 (if pad-char pad-char 32))
      )
  (let ((str-lgt (length str)))
   (if (>= lgt str-lgt)
    (cond ((eq just 'left) (concat str (make-string (- lgt str-lgt) pad-char-1)))
          ((eq just 'right) (concat (make-string (- lgt str-lgt) pad-char-1) str))
          (t (error "pad-string-to-length: Unknown justification: %s" just)))
    (cond ((eq just 'left)  (substring str 0 lgt))
          ((eq just 'right) (substring str 0 lgt))
          (t (error "pad-string-to-length: Unknown justification: %s" just)))))))

(defun show-midl-mode-status-string ()
; (setq global-mode-string (midl-mode-status-string))
  (setq local-midl-mode-info (midl-mode-status-string))
  (force-mode-line-update))
    

(defun help-from-text-file (file)
  "Display help from file in help directory of midi stuff."
  (let ((help-buffer (if (get-buffer "*Help*") (get-buffer "*Help*") (generate-new-buffer "*Help*"))))
    (set-window-buffer (other-window 1) help-buffer)
    (set-buffer help-buffer)
    (toggle-read-only -1)
    (erase-buffer)
    (insert-file-contents (concat laml-dir "styles/xml-in-laml/midi/help/" file))
    (toggle-read-only 1)
    (set-buffer-modified-p nil)
    (other-window 1)))

(defun open-midi-html-documentation (doc-file)
  (start-process "Windows Application" nil (concat laml-dir "bin/call.bat") (concat laml-dir "styles/xml-in-laml/midi/man/" doc-file)))

; ---------------------------------------------------------------------------------------------------------------
; Support of marked sections (by use of meta markers).
; Markers are hosted as textual contetns in type 6 meta events. Format: "<letter>-i<space>free-text" where i is an integer number.

(defun locate-initial-point-of-marked-section (marker-number)
 "Return the start point of the section that starts with a marker numbered with marker-number.
Return nil if the given marker-number cannot be found.
The marker number is an integer, or a string which represents an integer.
This function works relative to the current track."
   (find-marker-position marker-number))

(defun locate-final-point-of-marked-section (marker-number)
 "Return the end point of the section that starts with a marker numbered with marker-number.
The marker number is an integer, or a string which represents an integer.
This function works relative to the current track."
 (save-excursion
   (let ((p-ini (find-marker-position marker-number)))
     (if p-ini
         (progn
           (goto-char p-ini) (end-of-line 1)
           (find-next-marker-position)
         )
         nil))))


(defun find-marker-position (n)
 "Return the position of the meta form with marker n (an integer or string which represents the integer). 
Operates within the currently, implicitly selected track"
 (save-excursion
  (beginning-of-line 1)
  (while (not (looking-at "(MidiTrack")) (backward-up-list 1))
  ; now at beginning of the current MidiTrack
  (forward-sexp 1)
  (let ((limit (point)))
     (backward-sexp 1)
     (let* ((n-str (if (integerp n) (int-to-string n) n)))
       (find-marker-string-start-at (point) n-str limit)))))


(defun find-next-marker-position (&optional track-limit)   ; use of track-limit makes the function much more efficient
  "Relative the current point, return the position of the meta form with the next occuring mark in this track."
  (save-excursion
   (if track-limit  
       (find-marker-string-start-at (point) nil track-limit)
       (let ((cur-point (point)))
         (beginning-of-line 1) (backward-up-list 1) ; now at beginning of the current MidiTrack
         (forward-sexp 1)
         (let ((limit (point)))
           (goto-char cur-point)
           (find-marker-string-start-at cur-point nil limit))))))

(defun find-previous-marker-position (&optional track-limit) ; use of track-limit makes the function much more efficient
  "Relative the current point, return the position of the meta form with the previous occuring mark in this track."
  (save-excursion
   (if track-limit
       (find-previous-marker-string-start-at (point) track-limit)
       (let ((cur-point (point)))
         (beginning-of-line 1) (backward-up-list 1) ; now at beginning of the current MidiTrack
         (let ((limit (point)))
           (goto-char cur-point)
           (find-previous-marker-string-start-at cur-point limit))))))

(defun find-previous-marker-string-start-at (p limit)
  "Find the position of previous meta marker (of arbitrary number) between position p and limit. (Thus, search backward from p towards limit).
Return the found position.
Return nil if the marker cannot be located."
  (goto-char p)
  (let ((pos (search-backward (concat current-marker-letter "-") limit t)))
    (if pos
        (progn
          (backward-char 1) (backward-up-list 1)
          (if (looking-at "(Meta")
              (point)
              (find-previous-marker-string-start-at (- pos 1) limit)))
        nil)))

(defun find-marker-string-start-at (p n-str limit)
  "Find the marker string n-str, such as \"5\", between position p and limit. Return the found position.
If n-str is nil, find the first marker following p.
Return nil if the marker cannot be located."
  (goto-char p)
  (let* ((marker-str (concat current-marker-letter "-" (if (null n-str) "" (concat n-str ""))))
         (pos (search-forward marker-str limit t)))
    (if pos
        (progn
          (backward-char (+ (length marker-str) 1))
          (backward-up-list 1)
          (if (looking-at "(Meta")
              (point)
              (find-marker-string-start-at (+ pos 1) n-str limit)))
        nil)))

(defun start-position-of-current-track ()
 "Return the position just before the first event of the current track."
 (save-excursion
   (beginning-of-line 1) 
   (backward-up-list 1) ; now at beginning of the current MidiTrack   
   (if (looking-at "(MidiTrack")
       (progn
         (forward-char 1)
         (forward-sexp 2)
         (backward-sexp 1)
         (point))
       nil)))


(defun end-position-of-current-track ()
  "Return the position just after the last event of the current track."
 (save-excursion
   (beginning-of-line 1) 
   (backward-up-list 1) ; now at beginning of the current MidiTrack   
   (if (looking-at "(MidiTrack")
       (progn
         (forward-sexp 1)
         (backward-char 1)
         (point))
       nil)))

(defun process-current-marked-region-and-play ()
  (interactive)
  (let* ((start-p (find-previous-marker-position))
         (end-p   (find-next-marker-position))
         (start-p-1 (if start-p start-p (start-position-of-current-track))) 
         (end-p-1 (if end-p end-p (end-position-of-current-track))) 
         (current-marker (marker-number-of-current-section))
        )
    (if (and start-p-1 end-p-1)  ; expected always to be true
        (progn
          (setq selection-begin start-p-1) (setq selection-end end-p-1) 
          (message "Playing section starting with the marker %s-%s" current-marker-letter current-marker)
          (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p-1 end-p-1 'play)
        )
        (message "Cannot process and play marked region."))))

(defun goto-next-marked-section-and-process-it-and-play ()
  (interactive)
  (let ((start-p (find-next-marker-position)))
    (if start-p
        (progn
          (goto-char start-p)
          (forward-midi-message 1)
          (if (member play-after-condition '(after-navigate after-navigate-and-edit))
              (let* ((current-marker (marker-number-of-current-section))
                     (end-p (find-next-marker-position))
                     (end-p-1 (if end-p end-p (end-position-of-current-track))) )
                (if end-p-1
                    (progn
                      (setq selection-begin start-p) (setq selection-end end-p-1) 
                      (message "Playing section starting with the marker %s-%s" current-marker-letter current-marker)
                      (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p end-p-1 'play)
                      )
                  (message "Cannot play marked section")))
              (message "Now at section starting with the marker %s-%s" current-marker-letter (marker-number-of-current-section))))               
         (message "At End."))))


(defun goto-previous-marked-section-and-process-it-and-play ()
  (interactive)
  (let ((end-p (find-previous-marker-position)))
    (if end-p
        (progn
          (goto-char end-p)
          (let* ((current-marker (marker-number-of-current-section))
                 (start-p (find-previous-marker-position))
                 (start-p-1 (if start-p start-p (start-position-of-current-track))) )
             (if start-p-1
                 (progn
                   (goto-char start-p-1)
                   (forward-midi-message 1)
                   (if (member play-after-condition '(after-navigate after-navigate-and-edit))
                       (progn
                         (setq selection-begin start-p-1) (setq selection-end end-p) 
                         (message "Playing section starting with the marker %s-%s" current-marker-letter (if current-marker current-marker "Begin"))
                         (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p-1 end-p 'play))
                       (if current-marker
                          (message "Now at section starting with the marker %s-%s" current-marker-letter current-marker)
                          (message "Now at beginning"))))
                 (message "Cannot move to marked section"))))
         (message "At Beginning."))))


(defun extract-marker-text ()
  "Assume that the point is located at the start parenthesis of a Meta marker midi event with some <letter>-i marking.
Extract the full marker text. A string. Return nil if not possible."
 (save-excursion
  (forward-sexp 1)
  (let ((limit (point)))
    (backward-sexp 1)
    (let ((start-p (search-forward (concat "\"" current-marker-letter "-")  limit t)))
      (if start-p
          (progn
            (goto-char (+ start-p 1))
            (let ((end-p (search-forward "\"" limit t)))
               (if end-p
                   (buffer-substring-no-properties (- start-p 2) (- end-p 1))
                   nil)))
          nil)))))

(defun marker-number-of-current-section ()
  "Return the marker number as a string (the marker number i of the previous meta marker <letter>-i) of the current section.
A section goes from one meta marker to the next. Returns a string."
 (save-excursion
  (let ((p (find-previous-marker-position)))
    (if p
        (progn
           (goto-char p)
           (let ((res (search-forward (concat "\"" current-marker-letter "-") nil t)))
             (if res
                 (let ((marker-name (name-under-point)))
                   (substring marker-name 2 (length marker-name)))
               nil)))
        nil))))

(defun collect-marker-list-of-track ()
 "Given the implicitly selected track, collect and retun a list of all marker texts, of the current marker letter, in this track."
 (save-excursion
   (beginning-of-line 1) (backward-up-list 1) ; now at beginning of the current MidiTrack
   (forward-sexp 1)
   (let ((limit (point))
         (pos nil)
         (res nil)
         (i 0)
        )
     (forward-sexp -1)
     (while (setq pos (find-next-marker-position limit))
        (goto-char pos)
        (setq res (cons (extract-marker-text) res))
        (setq i (+ i 1))
;        (message "Collecting marker %d" i)
        (forward-sexp 1)  ; progress
     )
     (reverse res))))

(defun collect-all-markers-of-track ()
 "Given the implicitly selected track, collect and retun a list of all marker texts, independent of marker letter, in this track."
 (save-excursion
   (beginning-of-line 1) (backward-up-list 1) ; now at beginning of the current MidiTrack
   (forward-sexp 1)
   (let ((limit (point))
         (pos nil)
         (res nil)
         (i 0)
        )
     (forward-sexp -1)
     (while (setq pos (find-next-marker-position limit))
        (goto-char pos)
        (setq res (cons (extract-marker-text) res))
        (setq i (+ i 1))
        (message "Collecting marker %d" i)
        (forward-sexp 1)  ; progress
     )
     (reverse res))))

; ---------------------------------------------------------------------------------------------------------------
; Marker browser

(defvar current-marker-letter "M" "The letter currently used for the marker browser. A string of length 1.")

(defun make-marker-region-browser ()
  "From inside a MidiTrack, or from inside the nearest MidiTrack, make a marker browser of the implictly selected track. 
Return the number of marks of the browser."
  (interactive)

  (if (not (is-inside-track-of-standard-midi-file))
      (go-inside-nearest-midi-track))

  (if (is-inside-track-of-standard-midi-file)
      (let ((marker-text-list (filter (function (lambda (x) (not (null x)))) (collect-marker-list-of-track)))) 
        (if (> (length marker-text-list) 0)
            (progn
              (message "Marker Browser with %d markers, using letter \"%s\"." (length marker-text-list) current-marker-letter)
              (delete-other-windows)
              (split-window-vertically)
              (let* ((midl-buf-name (file-name-proper (buffer-name (current-buffer)))) 
                     (browser-buf-name (concat "marker-browser" "-" midl-buf-name))
                     (browser-buf (get-buffer-create browser-buf-name)) ; ealier (get-buffer-create (generate-new-buffer-name browser-buf-name))
                     )
                (set-buffer browser-buf)
                (toggle-read-only -1)
                (erase-buffer)
                (set-window-buffer (selected-window) browser-buf)
                (mapcar (function (lambda (mt) (insert mt) (insert CR))) marker-text-list)
                (goto-char (point-min))
                (highlight-this-line)
                (marker-browser-mode)
      
                ; adjust size of browser:
                (let* ((lines-max (/ (frame-height) 2))
                       (actual-number-of-lines (length marker-text-list))
                       (extra-lines 3)
                       (required-number-of-lines (+ extra-lines actual-number-of-lines))
                       )
                  (if (< required-number-of-lines lines-max)
                      (enlarge-window (- required-number-of-lines lines-max)) ; a negative numer, thus shrinks the window size
                    ))

                (toggle-read-only 1)
                ) 
                (length marker-text-list)
              )
          (progn
            (message "There are no \"%s\" markers in this midl file. Noting done." current-marker-letter)
            0)))
   (progn
     (message "No MidiTrack in sight. Nothing done.") 
     0)
))   

(defun make-marker-region-browser-new-marker-letter (marker-letter)
  "From inside a MidiTrack, make a marker browser of the implictly selected track. Ask for marker letter."
  (interactive "sMarker Browser with letter: ")
  (let ((old-marker-letter current-marker-letter))
    (setq current-marker-letter marker-letter)
    (let ((marker-count (make-marker-region-browser)))
      (if (= marker-count 0)
          (setq current-marker-letter old-marker-letter) ; such that if no markers exists, the current marker letter does not change.
      ))))
         

      
(define-derived-mode 
        marker-browser-mode fundamental-mode "MarkerBrowser" 
  "A line-oriented browser of selected Meta markers of a MIDI LAML file."

)  

(defvar marker-browser-mode-map (make-sparse-keymap "MarkerBrowser"))
(define-key marker-browser-mode-map "f" 'mb-forward-marker)
(define-key marker-browser-mode-map "b" 'mb-backward-marker)
(define-key marker-browser-mode-map "a" 'mb-add-ticks-to-marker)
(define-key marker-browser-mode-map "g" 'mb-goto-meta-marker)
(define-key marker-browser-mode-map "p" 'mb-play-marked-region)
(define-key marker-browser-mode-map "c" 'mb-add-comment-to-marker)
(define-key marker-browser-mode-map "s" 'mb-save-to-kill-ring)
(define-key marker-browser-mode-map  "l" 'mb-display-length-of-marked-region)
(define-key marker-browser-mode-map  "e" 'mb-enclose-marked-region-in-midi-region)
(define-key marker-browser-mode-map  "A" 'mb-play-around-mark)
(define-key marker-browser-mode-map  "d" 'mb-delete-marker)
(define-key marker-browser-mode-map  "D" 'mb-delete-marked-region)

(laml-define-key marker-browser-mode-map [menu-bar marker-browser]
    (cons "Marker Browser" (make-sparse-keymap "Marker Browser")))



; Replicating some function keys (related to tempo and channel selection) from the marker browser. 
; In order for them to have effect on buffer local variables, carry them out in 'the other window'.

(define-key marker-browser-mode-map [f5] (function (lambda () (interactive) (do-in-other-window (call-interactively 'set-relative-tempo)))))
(define-key marker-browser-mode-map [C-f5] (function (lambda () (interactive) (do-in-other-window (call-interactively 'reset-relative-tempo)))))
(define-key marker-browser-mode-map [S-f5] (function (lambda () (interactive) (do-in-other-window (call-interactively  
                                                                                                      (function (lambda () (interactive) (add-to-relative-tempo -10))))))))
(define-key marker-browser-mode-map [C-M-f5] (function (lambda () (interactive) (do-in-other-window (call-interactively 
                                                                                                       (function (lambda () (interactive) (add-to-relative-tempo 10))))))))
(define-key marker-browser-mode-map [M-f5] (function (lambda () (interactive) (do-in-other-window (call-interactively
                                                                                                       (function (lambda () (interactive) (add-to-relative-tempo 10))))))))

(define-key marker-browser-mode-map [f7] (function (lambda () (interactive) (do-in-other-window (call-interactively 'set-channels-to-play)))))
(define-key marker-browser-mode-map [C-f7] (function (lambda () (interactive) (do-in-other-window (call-interactively 'set-channels-all)))))
(define-key marker-browser-mode-map [M-f7] (function (lambda () (interactive) (do-in-other-window (call-interactively 'toggle-use-voice-context)))))

(define-key marker-browser-mode-map [kp-delete] (function (lambda () (interactive) (do-in-other-window (call-interactively 'toggle-use-live-cursor-movement)))))
(define-key marker-browser-mode-map [kp-add] (function (lambda () (interactive) (do-in-other-window (call-interactively 'faster-live-cursor)))))
(define-key marker-browser-mode-map [kp-subtract] (function (lambda () (interactive) (do-in-other-window (call-interactively 'slower-live-cursor)))))

(define-key marker-browser-mode-map [down] 'next-line-and-highlight-line)   
(define-key marker-browser-mode-map [up] 'previous-line-and-highlight-line)



(laml-define-key  marker-browser-mode-map [menu-bar marker-browser help-on-markers-from-browser]
      '("Help on Markers" . (lambda () (interactive) (help-from-text-file "marker-channel.txt"))))

(laml-define-key marker-browser-mode-map [menu-bar marker-browser marker-browser-sep-1]
   '("----"))

(laml-define-key  marker-browser-mode-map [menu-bar marker-browser mb-enclose-marked-region-in-drum-fills-by-bars]
      '("Enclose section in drum fill form" . mb-enclose-marked-region-in-drum-fills-by-bars))

(laml-define-key  marker-browser-mode-map [menu-bar marker-browser mb-enclose-marked-region-in-time-displacement]
      '("Enclose section in a time displace form" . mb-enclose-marked-region-in-time-displacement))

(laml-define-key  marker-browser-mode-map [menu-bar marker-browser mb-enclose-marked-region-in-velocity-scale]
      '("Enclose section in a velocity scale form" . mb-enclose-marked-region-in-velocity-scale))

(laml-define-key  marker-browser-mode-map [menu-bar marker-browser mb-enclose-marked-region-in-volume-scale]
      '("Enclose section in volume-scale" . mb-enclose-marked-region-in-volume-scale))

(laml-define-key  marker-browser-mode-map [menu-bar marker-browser mb-enclose-marked-region-in-tempo-scale]
      '("Enclose section in tempo-scale" . mb-enclose-marked-region-in-tempo-scale))

(laml-define-key  marker-browser-mode-map [menu-bar marker-browser mb-enclose-marked-region-in-midi-region]
      '("Enclose section in midi-region" . mb-enclose-marked-region-in-midi-region))

(laml-define-key marker-browser-mode-map [menu-bar marker-browser marker-browser-sep-2]
   '("----"))

(laml-define-key  marker-browser-mode-map [menu-bar marker-browser mb-normalize-markers]
      '("Normalize markers" . mb-normalize-markers))

(laml-define-key  marker-browser-mode-map [menu-bar marker-browser mb-add-ticks-to-marker]
      '("Add time to marker..." . mb-add-ticks-to-marker))

(laml-define-key  marker-browser-mode-map [menu-bar marker-browser mb-backward-marker]
      '("Move marker backward" . mb-backward-marker))

(laml-define-key  marker-browser-mode-map [menu-bar marker-browser mb-forward-marker]
      '("Move marker forward" . mb-forward-marker))

(laml-define-key  marker-browser-mode-map [menu-bar marker-browser mb-delete-marker]
      '("Delete marker" . mb-delete-marker))

(laml-define-key marker-browser-mode-map [menu-bar marker-browser marker-browser-sep-3]
   '("----"))

(laml-define-key  marker-browser-mode-map [menu-bar marker-browser mb-save-to-kill-ring]
      '("Save copy of section to kill ring" . mb-save-to-kill-ring))

(laml-define-key  marker-browser-mode-map [menu-bar marker-browser mb-display-length-of-marked-region]
      '("Display length of section" . mb-display-length-of-marked-region))



(laml-define-key  marker-browser-mode-map [menu-bar marker-browser mb-add-comment-to-marker]
      '("Add comment to marker" . mb-add-comment-to-marker))

(laml-define-key  marker-browser-mode-map [menu-bar marker-browser mb-goto-meta-marker]
      '("Goto marker" . mb-goto-meta-marker))

(laml-define-key marker-browser-mode-map [menu-bar marker-browser marker-browser-sep-4]
   '("----"))

(laml-define-key  marker-browser-mode-map [menu-bar marker-browser mb-play-around-mark]
      '("Play around marker" . mb-play-around-mark))

(laml-define-key  marker-browser-mode-map [menu-bar marker-browser mb-delete-marked-region]
      '("Delete marked region" . mb-delete-marked-region))

(laml-define-key  marker-browser-mode-map [menu-bar marker-browser mb-play-marked-region]
      '("Play marked region" . mb-play-marked-region))



(defun marker-number-of-browser-line ()
  "Return the number - as a string - of the currently selected marker line in a marker browser."
  (save-excursion
   (beginning-of-line)
   (let ((start-p (search-forward (concat current-marker-letter "-") nil t)))
    (if start-p
        (progn
          (forward-word 1)
          (let ((end-p (point)))
             (buffer-substring-no-properties start-p end-p)))
        nil  ; not expectable
    ))))

(defun marker-comment-of-browser-line ()
  "Return the comment - as a string - of the currently selected marker line in a marker browser."
  (save-excursion
   (beginning-of-line)
   (let ((start-p (search-forward (concat current-marker-letter "-") nil t)))
    (if start-p
        (progn
          (forward-word 1)
          (let ((start-p (point)))
             (end-of-line 1)
             (delete-string-prefix (list 32 42) (buffer-substring-no-properties start-p (point)))))
        nil  ; not expectable
    ))))


(defun mb-goto-meta-marker ()
  (interactive)
  (let ((marker-number-text (marker-number-of-browser-line)))
    (other-window 1)
    (if (not (is-inside-track-of-standard-midi-file)) (go-inside-nearest-midi-track))
    (let ((pos (find-marker-position marker-number-text)))
      (if pos
          (goto-char pos)
          (message "Cannot find marker"))
      (other-window 1))))


(defun mb-play-marked-region ()
  (interactive)
  (save-excursion
    (if (and transient-mark-mode mark-active) 

        ; Highlighed selection in browser buffer:
        (let ((mb-start-p (min (point) (mark)))
              (mb-end-p (max (point) (mark))))
          (goto-char mb-start-p)
          (let ((from-marker-number-text (marker-number-of-browser-line)))
            (goto-char mb-end-p)
            (let ((to-marker-number-text (marker-number-of-browser-line)))
              (other-window 1)
              (if (not (is-inside-track-of-standard-midi-file)) (go-inside-nearest-midi-track))
              (let* ((start-p (locate-initial-point-of-marked-section from-marker-number-text))
                     (end-p   (locate-final-point-of-marked-section to-marker-number-text))
                     (start-p-1 (if start-p start-p (start-position-of-current-track))) 
                     (end-p-1 (if end-p end-p (end-position-of-current-track))) 
                     (current-marker (marker-number-of-current-section))
                     )
                (if (and start-p-1 end-p-1) ; expected always to be true
                    (progn
                      (setq selection-begin start-p-1) (setq selection-end end-p-1) 
                      (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p-1 end-p-1 'play)
                      )
                  (message "Cannot process and play marked region.")))
;               (other-window 1)   ; if other-window, it is problematic to stop midi playing
        )))

        ; Single browser line - implicit selection
        (let ((marker-number-text (marker-number-of-browser-line)))
          (if marker-number-text
              (progn
                (other-window 1)
                (if (not (is-inside-track-of-standard-midi-file)) (go-inside-nearest-midi-track))
                (let* ((start-p (locate-initial-point-of-marked-section marker-number-text))
                       (end-p   (locate-final-point-of-marked-section marker-number-text))
                       (start-p-1 (if start-p start-p (start-position-of-current-track))) 
                       (end-p-1 (if end-p end-p (end-position-of-current-track))) 
                       (current-marker (marker-number-of-current-section))
                       )
                  (if (and start-p-1 end-p-1) ; expected always to be true
                      (progn
                        (setq selection-begin start-p-1) (setq selection-end end-p-1) 
                        (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p-1 end-p-1 'play)
                        )
                    (message "Cannot process and play marked region.")))
;                (other-window 1)   ; if other-window, it is problematic to stop midi playing
              )
            (message "Cannot??!!"))))))

(defun mb-play-around-mark (&optional before-and-after-length-0)
  "Play an interval around the mark. The interval defaults to 4 quarter notes before and after.
Play a loud sound instead at the place of the marker.
If a parameter is passed, it designates the tick length before and after the marker to be played."
  (interactive)
  (save-excursion
   (condition-case nil
    (let* ((marker-number-text (marker-number-of-browser-line)))
      (if marker-number-text
          (progn
            (other-window 1)
            (if (not (is-inside-track-of-standard-midi-file)) (go-inside-nearest-midi-track))
            (let* ((pos (find-marker-position marker-number-text))
                   (before-and-after-length (if before-and-after-length-0 before-and-after-length-0 (* (find-pulses-per-quarter-note-of-current-buffer) 4)))
                   (start-p pos)
                   (start-p-1 (if start-p start-p (start-position-of-current-track)))
                   (buffer (current-buffer)) 
                  )
              (goto-char pos)
              (let ((at (attribute-of-current-form 'absTime 'number))
                    (meta-form-string (current-form-string)))
                
                (kill-sexp 1)     ; delete meta event
                (insert (marker-bell-event at 16)) (backward-sexp 1)
                (setq extra-context (read-text-file (concat laml-dir "styles/xml-in-laml/midi/" "templates/" "marker-bell-context.tpl")))
                (move-to-abs-time (+ at before-and-after-length))
                (let ((end-p-1 (point)))
                  (goto-char pos)
                  (move-to-abs-time (max 0 (- at before-and-after-length)))
                  (let ((start-p-1 (point)))
                    (setq selection-begin start-p-1) (setq selection-end end-p-1) 
                    (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p-1 end-p-1 'play)
                    ; (set-buffer buffer) ; necessary of some reason. RESEARCH IT. Due to buffer problems in process-laml-file-and-present-from-to. Solved
                    (setq extra-context "")
                    (goto-char pos)
                    (kill-sexp 1) 
                    (insert meta-form-string) (backward-sexp 1))
                  )))
              (other-window 1))
        (message "Cannot??!!")))
   (error (progn
             (setq extra-context "")
          )))))

; Not used
(defun find-play-around-limits (middle-pos before-and-after-length)
 "From middle-pos find an interval around middle pos of before-and-after-length (measured in a number of ticks).
Return a cons pair of (from . to) positions."
 (save-excursion
   (goto-char middle-pos)
   (let ((abs-time-start (attribute-of-current-form 'absTime 'number)))
      (move-to-abs-time (+ abs-time-start before-and-after-length))
      (let ((end-p (point)))
        (goto-char middle-pos)
        (move-to-abs-time (max 0 (- abs-time-start before-and-after-length)))
           (cons (point) end-p)))))
         

(defun marker-bell-event (abs-time channel)
  (concat "(" "NoteOn" " "
             "'absTime" " " (string-it (int-to-string abs-time)) " "
             "'channel" " " (string-it (int-to-string channel)) " "
             "'note" " " (string-it (int-to-string 40)) " "
             "'velocity" " " (string-it (int-to-string 127)) " "
             "'duration" " " (string-it (int-to-string 50))  ")"))


(defun current-form-string ()
 (save-excursion
  (let ((p0 (point)))
    (forward-sexp 1)
    (buffer-substring-no-properties p0 (point)))))

(defun move-to-abs-time (abs-time-to)
  "Move forward or backward close to absTime abs-time-to (a number). Return a position.
Protected for overrun in forward direction.
In backward direction, no underrun protection is done. In backward direction movement can easily be stopped at absTime 0.
Precondition: Point selects a form with absTime."
  (let ((cur-abs-time (attribute-of-current-form 'absTime 'number))
        (is-at-end nil))
    (cond ((> abs-time-to cur-abs-time)  ; move forward
            (while (and (not is-at-end) (< (attribute-of-current-form 'absTime 'number) abs-time-to))
              (let ((res (forward-midi-message)))
                (setq is-at-end (not res))))
            (point))
          ((= abs-time-to cur-abs-time) (point))
          ((< abs-time-to cur-abs-time)  ; move backward
            (while (> (attribute-of-current-form 'absTime 'number) abs-time-to)
              (backward-midi-message))
            (point)))))
  

(defun mb-add-comment-to-marker ()
  "Add a comment (promp for it). Delete a possible existing comment both in browser and in midl file."
  (interactive)
  (let ((comment (read-from-minibuffer "Comment of this marker: " (marker-comment-of-browser-line))))
   (let ((marker-number-text (marker-number-of-browser-line)))
     (other-window 1)
     (if (not (is-inside-track-of-standard-midi-file)) (go-inside-nearest-midi-track))
     (let ((pos (find-marker-position marker-number-text)))
       (if pos
           (progn
             (goto-char pos)
             (forward-sexp 1)
             (let ((limit (point)))
               (backward-sexp 1)
               (let ((res (search-forward (concat "\"" current-marker-letter "-") limit t))
                     (p0 (point))
                    )
                 (if res
                     (progn
                       (search-forward "\"" limit t)
                       (goto-char p0) (forward-word 1)
                       (insert " ") 
                       
                       ; kill existing comment:
                       (while (not (looking-at-chars (point) (list 34))) (delete-char 1))

                       (insert comment))
                   (error "Cannot??!! (1)")))))
         (error "Cannot??!! (2)")))

     (other-window 1)

                                        ; now back in browser buffer
     (end-of-line 1)
     (toggle-read-only)
     (beginning-of-line 1) (search-forward (concat current-marker-letter "-") nil t) (forward-word 1)

     ; kill existing comment:
     (while (not (looking-at-chars (point) (list 10))) (delete-char 1))

     (insert " ") (insert comment)
     (toggle-read-only)
     (beginning-of-line 1))))

(defun mb-delete-marker ()
  "Delete the marker, both in the browser and in the midi laml file."
  (interactive)
  (let ((marker-number-text (marker-number-of-browser-line)))
     (other-window 1)
     (if (not (is-inside-track-of-standard-midi-file)) (go-inside-nearest-midi-track))
     (let ((pos (find-marker-position marker-number-text)))
       (if pos
           (progn
             (goto-char pos)
             (delete-sexp 1)
             (beginning-of-line 1)
             (delete-line 1)
           )
           (error "Cannot delete??!!")))

     (other-window 1)

                                        ; now back in browser buffer
     (end-of-line 1)
     (toggle-read-only)
     (beginning-of-line 1)
     (delete-line 1)
     (toggle-read-only)
     (beginning-of-line 1)))

(defun mb-delete-marked-region ()
  "Delete the marked region, both in the browser and in the midi laml file."
  (interactive)
  (let ((marker-number-text (marker-number-of-browser-line)))
    (if marker-number-text
     (progn
      (other-window 1)
      (if (not (is-inside-track-of-standard-midi-file)) (go-inside-nearest-midi-track))
      (let* ((initial-p0 (locate-initial-point-of-marked-section marker-number-text))
             (initial-p (if initial-p0 initial-p0 (start-position-of-current-track)))
             (final-p0 (locate-final-point-of-marked-section marker-number-text))
             (final-p (if final-p0 final-p0 (end-position-of-current-track))))
        (delete-region initial-p final-p)
        (other-window 1)
                                        ; now back in browser buffer
        (end-of-line 1)
        (toggle-read-only)
        (beginning-of-line 1)
        (delete-line 1)
        (toggle-read-only)
        (beginning-of-line 1)))
     (message "Cannot"))))



(defun mb-display-length-of-marked-region ()
  (interactive)
  (if (and transient-mark-mode mark-active) 

      ; Highlighed selection in browser buffer:
      (let ((mb-start-p (min (point) (mark)))
            (mb-end-p (max (point) (mark))))
        (goto-char mb-start-p)
        (let ((from-marker-number-text (marker-number-of-browser-line)))
          (goto-char mb-end-p)
          (let ((to-marker-number-text (marker-number-of-browser-line)))
            (if (and from-marker-number-text to-marker-number-text)
                (progn
                  (other-window 1)
                  (if (not (is-inside-track-of-standard-midi-file)) (go-inside-nearest-midi-track))
                  (let* ((p0 (point))
                         (initial-p0 (locate-initial-point-of-marked-section from-marker-number-text))
                         (initial-p (if initial-p0 initial-p0 (start-position-of-current-track)))
                         (final-p0 (locate-final-point-of-marked-section to-marker-number-text))
                         (final-p (if final-p0 final-p0 (end-position-of-current-track)))) 
                    (set-mark initial-p)
                    (goto-char final-p)
                    (display-length-of-midi-list)
                    (goto-char p0)
                    (other-window 1)))
              (message "Cannot")))))

    (let ((marker-number-text (marker-number-of-browser-line))
         )
      (if marker-number-text
          (save-excursion
            (other-window 1)
            (if (not (is-inside-track-of-standard-midi-file)) (go-inside-nearest-midi-track))
            (let* ((p0 (point))
                   (initial-p0 (locate-initial-point-of-marked-section marker-number-text))
                   (initial-p (if initial-p0 initial-p0 (start-position-of-current-track)))
                   (final-p0 (locate-final-point-of-marked-section marker-number-text))
                   (final-p (if final-p0 final-p0 (end-position-of-current-track)))) 
              (set-mark initial-p)
              (goto-char final-p)
              (display-length-of-midi-list (concat "Section starting at marker " marker-number-text ": "))
              (goto-char p0)
              (other-window 1)))
        (message "Cannot")))))

(defun mb-save-to-kill-ring ()
  (interactive)
  (if (and transient-mark-mode mark-active) 
      (let ((mb-start-p (min (point) (mark)))
            (mb-end-p (max (point) (mark))))
        (goto-char mb-start-p)
        (let ((from-marker-number-text (marker-number-of-browser-line)))
          (goto-char mb-end-p)
          (let ((to-marker-number-text (marker-number-of-browser-line)))
            (if (and from-marker-number-text to-marker-number-text)
                (progn
                  (other-window 1)
                  (if (not (is-inside-track-of-standard-midi-file)) (go-inside-nearest-midi-track))
                  (let* ((initial-p0 (locate-initial-point-of-marked-section from-marker-number-text))
                         (initial-p (if initial-p0 initial-p0 (start-position-of-current-track)))
                         (final-p0 (locate-final-point-of-marked-section to-marker-number-text))
                         (final-p (if final-p0 final-p0 (end-position-of-current-track)))) 
                    (copy-region-as-kill initial-p final-p)
                    (message "Region saved in kill ring") 
                    (other-window 1)))
              (message "Cannot")))))

    (let ((marker-number-text (marker-number-of-browser-line)))
      (if marker-number-text
          (save-excursion
            (other-window 1)
            (if (not (is-inside-track-of-standard-midi-file)) (go-inside-nearest-midi-track))
            (let* ((initial-p0 (locate-initial-point-of-marked-section marker-number-text))
                   (initial-p (if initial-p0 initial-p0 (start-position-of-current-track)))
                   (final-p0 (locate-final-point-of-marked-section marker-number-text))
                   (final-p (if final-p0 final-p0 (end-position-of-current-track))))
              (copy-region-as-kill initial-p final-p)
              (message "Region saved in kill ring") 
              (other-window 1)))
        (message "Cannot")))))

(defun mb-enclose-marked-region-in-midi-region ()
  "Enclose the marked region in a midi-region form."
  (interactive)
  (mb-enclose-marked-region-given-prefix "(midi-region"))

(defun mb-enclose-marked-region-in-tempo-scale ()
  "Enclose the marked region in a tempo-scale form."
  (interactive)
  (mb-enclose-marked-region-given-prefix 
   (function
    (lambda ()
      (let ((tempo (find-tempo-of-current-buffer)))
        (concat "(tempo-scale" " " (if tempo (number-to-string tempo) "??") " " "(make-scale-function-by-xy-points (from-percent-points '((0 100) (100 100))))"))))))

(defun mb-enclose-marked-region-in-volume-scale ()
  "Enclose the marked region in a  volume-scale-multi-channel form."
  (interactive)
  (mb-enclose-marked-region-given-prefix 
   (function
    (lambda ()
      (let ((volume (find-tempo-of-current-buffer)))
        (concat "(volume-scale-multi-channel" " " "(list (list <channel> <start-vol>" " " 
                "(make-scale-function-by-xy-points (from-percent-points '((0 100) (100 100))))"
                " ))"))))))

(defun mb-enclose-marked-region-in-velocity-scale ()
  "Enclose the marked region in a velocity scaling form."
  (interactive)
  (mb-enclose-marked-region-given-prefix 
    "(scale-attribute-of-channel  'velocity (make-scale-function-by-xy-points (from-percent-points '((0 100) (100 100))))"))

(defun mb-enclose-marked-region-in-time-displacement ()
  "Enclose the marked region in a time-displace-channels form."
  (interactive)
  (mb-enclose-marked-region-given-prefix 
    "(time-displace-channels '(<ch1>...) 150"))


(defun mb-enclose-marked-region-in-drum-fills-by-bars ()
  "Enclose the marked region in drum fill form"
  (interactive)
  (mb-enclose-marked-region-given-prefix 
    "(substitute-section-by-bar '(9 10)
      (list
          (list <bar> 1 fill-aa)
      )"))

(defun mb-enclose-marked-region-given-prefix (prefix-form)
  "Enclose the marked region with the given prefix-form.
prefix-form is either a string or a parameterless function that produces the string.
The latter provides for 'as late as possible' generation of the string (from the context of the midl buffer).
The string is supposed to start - but not end - with a parenthesis."
  (if (and transient-mark-mode mark-active) 

      ; Highlighed selection in browser buffer:
      (let ((mb-start-p (min (point) (mark)))
            (mb-end-p (max (point) (mark))))
        (goto-char mb-start-p)
        (let ((from-marker-number-text (marker-number-of-browser-line)))
          (goto-char mb-end-p)
          (let ((to-marker-number-text (marker-number-of-browser-line)))
            (if (and from-marker-number-text to-marker-number-text)
                (progn
                  (other-window 1)
                  (if (not (is-inside-track-of-standard-midi-file)) (go-inside-nearest-midi-track))
                  (let* ((initial-p0 (locate-initial-point-of-marked-section from-marker-number-text))
                         (initial-p (if initial-p0 initial-p0 (start-position-of-current-track)))
                         (final-p0 (locate-final-point-of-marked-section to-marker-number-text))
                         (final-p (if final-p0 final-p0 (end-position-of-current-track))))
                     (goto-char final-p) (backward-char 1)
                     (while (looking-at-white-space (point)) (backward-char 1))
                     (insert ")")

                     (goto-char initial-p)
                     (let ((indentation (current-indentation)))
                       ; (forward-sexp 1)     
                       (insert CR)
                       (insert (make-string (- indentation 1) 32))
                       (insert (concat (if (stringp prefix-form) prefix-form (apply prefix-form nil)) " "))
                       (insert CR)
                       (insert (make-string indentation 32))
                       (backward-up-list 1)
                     )
                     (other-window 1)))
              (message "Cannot")))))

      (let ((marker-number-text (marker-number-of-browser-line)))
        (if marker-number-text
            (save-excursion
              (other-window 1)
              (if (not (is-inside-track-of-standard-midi-file)) (go-inside-nearest-midi-track))
              (let* ((initial-p0 (locate-initial-point-of-marked-section marker-number-text))
                     (initial-p (if initial-p0 initial-p0 (start-position-of-current-track)))
                     (final-p0 (locate-final-point-of-marked-section marker-number-text))
                     (final-p (if final-p0 final-p0 (end-position-of-current-track))))
                (goto-char final-p) (backward-char 1)
                (while (looking-at-white-space (point)) (backward-char 1))
                (insert ")") 

                (goto-char initial-p)
                (let ((indentation (current-indentation)))
                  ; (forward-sexp 1)     
                  (insert CR)
                  (insert (make-string (- indentation 1) 32)) 
                  (insert (concat (if (stringp prefix-form) prefix-form (apply prefix-form nil)) " "))
                  (insert CR)
                  (insert (make-string indentation 32)) 
                  (backward-up-list 1)
                )

                (other-window 1)))
          (message "Cannot")))))

(defun mb-forward-marker ()
  "Move the marker forward one position. Only possible in absTime mode" 
  (interactive)
  (let ((marker-number-text (marker-number-of-browser-line)))
    (other-window 1)
    (if (eq 'absTime (mode-of-standard-midi-file-current-buffer))
        (let ((pos (find-marker-position marker-number-text)))
          (if pos
              (progn
                (goto-char pos)
                (forward-sexp 1) (while (looking-at-white-space (point)) (forward-char 1)) ; now at form following the meta marker 
                (let* ((at (attribute-of-current-form 'absTime))
                       (at-number (string-to-number at)))
                  (transpose-sexps 1) (backward-sexp 1)
                  (set-attribute-of-current-form 'absTime at-number)))
            (message "Cannot")))
        (error "Can only be done in absTime mode."))
    (other-window 1)))
       

(defun mb-backward-marker ()
  "Move the marker backward one position. Only possible in absTime mode" 
  (interactive)
  (let ((marker-number-text (marker-number-of-browser-line)))
    (other-window 1)  
    (if (eq 'absTime (mode-of-standard-midi-file-current-buffer))
        (let ((pos (find-marker-position marker-number-text)))
          (if pos
              (progn
                (goto-char pos)
                (backward-sexp 1) ; now at form before the meta marker 
                (let* ((at (attribute-of-current-form 'absTime))
                       (at-number (string-to-number at)))
                  (goto-char pos)      
                  (transpose-sexps 1) (backward-sexp 2)
                  (set-attribute-of-current-form 'absTime at-number)))
            (message "Cannot")))
        (error "Can only be done in absTime mode."))
    (other-window 1)))

(defun mb-add-ticks-to-marker (ticks)
  "Add ticks to marker, and reposition it accordingly relative to its sibling events. Only possible in absTime mode" 
  (interactive "nAdd how many time ticks (positive or negative): ")
  (let ((marker-number-text (marker-number-of-browser-line)))
    (other-window 1)  
    (if (eq 'absTime (mode-of-standard-midi-file-current-buffer))
        (let ((pos (find-marker-position marker-number-text)))
          (if pos
              (progn
                (goto-char pos)
                (set-attribute-of-current-form 'absTime (+ (attribute-of-current-form 'absTime 'number) ticks))
                (cond ((> ticks 0)
                       (while (> (attribute-of-current-form 'absTime 'number) (attribute-of-next-form 'absTime 'number))
                         (swap-sexp-with-next)))
                      ((< ticks 0)
                       (while (< (attribute-of-current-form 'absTime 'number) (attribute-of-previous-form 'absTime 'number))
                         (swap-sexp-with-previous)))
                      (t 'do-nothing)))
            'do-nothing))
        (error "Can only be done in absTime mode."))
    (other-window 1)))

(defun mb-normalize-markers ()
  "Normalize the numering of markers, starting from 0. Must be called from a marker browser.
This affects the textual contents of all markers (Meta event of type 6) in the implicitly selected track, and it
affects the marker browser (which is automatically renewed)."
  (interactive)
  (other-window 1)  ; now in relevant track of midl buffer
  (let ((p0 (point))
        (marker-numbers (collect-marker-number-list-of-track))
        (pos nil)
       )
    (if (not (null marker-numbers))
        (progn
          (beginning-of-line 1) (backward-up-list 1) ; now at beginning of the current MidiTrack
          (forward-sexp 1)
          (let ((limit (point))
                (next-marker-number 0))
            (forward-sexp -1) ; now at beginning of track again
            (while (setq pos (find-next-marker-position limit))
               (goto-char pos)
               (let ((full-text-of-marker (textual-contents-of-current-form)))
                 (set-textual-contents-of-current-form
                    (format "%s-%s %s" current-marker-letter next-marker-number (marker-text-of-marker-text full-text-of-marker)))
                 (setq next-marker-number (+ 1 next-marker-number))
                 (goto-char pos)
                 (forward-sexp 1))))))
     (goto-char p0)
     (make-marker-region-browser) ; renew the browser
   )

   ; now back in marker browser
   (message "Done")
)

(defun collect-marker-number-list-of-track ()
  "Collect a list of marker numbers (a list of integers) of the markers in the implicitly selected track.
The implicitly selected track is the track that contains the cursor point."
  (let ((marker-texts (collect-marker-list-of-track)))
    (filter (function (lambda (x) (not (null x))))
            (mapcar (function marker-number-of-marker-text) marker-texts))))


(defun marker-number-of-marker-text (marker-text)
  "Return the number, of type integer, of marker-text (a text string)."
  (let* ((p1 (find-in-string marker-text ?- 0))
         (p2 (if p1 (find-in-string marker-text 32 p1) nil)))
    (if (and p1 p2)
        (let ((candidate-string (substring-no-properties marker-text (+ p1 1) p2)))
           (if (numeric-string candidate-string)
               (as-number candidate-string)
               nil))
        nil)))

(defun marker-text-of-marker-text (marker-text)
  "Return the text, of type string, of marker-text (a text string).
This is the part of marker-text that follows '<letter>-<number> '."
  (let* ((p1 (find-in-string marker-text ?- 0))
         (p2 (if p1 (find-in-string marker-text 32 p1) nil)))
    (if (and p1 p2)
        (substring-no-properties marker-text (+ p2 1) (length marker-text))
        nil)))

(defun insert-marker-midi-event (marker-text &optional marker-number)
  "Insert a marker event - a Meta event of type 6 with the given marker text.
If not provided, the marker number is the successor of the maximal marker number in the current track.
The marker event is in front of the following event. Works in both deltaTime and absTime mode."
  (interactive "sMarker text: ")
  (let* ((collected-numbers (if marker-number '() (collect-marker-number-list-of-track)))
         (marker-number-1 (cond (marker-number marker-number)
                                ((not (null collected-numbers)) (+ (apply (function max) collected-numbers) 1))
                                (t 0)))
         (p0 (point))
         (p1 (save-excursion (forward-sexp 1) (point)))
         (mode (guess-processing-mode p0 p1))
       )
    (while (looking-at-chars (point) white-space-char-list) (forward-char 1))
    (let ((left-margin (current-indentation)))
      (open-line 1)
      (cond ((equal mode "deltaTime")
               (insert (format "(Meta 'deltaTime \"0\" 'type \"6\" \"%s-%s %s\")" current-marker-letter marker-number-1 marker-text)))
            ((equal mode "absTime")
               (let ((cur-abs-time (attribute-of-current-form 'absTime 'number)))
                (insert (format "(Meta 'absTime \"%s\" 'type \"6\" \"%s-%s %s\")" cur-abs-time current-marker-letter marker-number-1 marker-text))))
            (t (error "insert-marker-midi-event: Should not happen.")))
      (backward-sexp 1))))



; ---------------------------------------------------------------------------------------------------------------
; Midl file comments:

(defun comment-of-midl-file ()
  "Return the current midl file comment"
  (save-excursion
     (goto-char (point-min))
     (if (looking-at ";;;; ")
         (progn
           (forward-char 5)
           (let ((p (point)))
             (end-of-line 1)
             (buffer-substring-no-properties p (point))))
         "")))

(defun set-comment-of-midl-file (comment-str)
  "Arrange that the first line of the current midl file becomes a four semicolon Scheme comment with comment-str as contents.
comment-str must be without CRs"
  (save-excursion
    (goto-char (point-min))
    (cond ((looking-at ";;;;")  ; killing existing fours semicolon comment
            (delete-line 1)
            (insert (concat ";;;; " comment-str CR)))
          (t (insert (concat ";;;; " comment-str CR))))))


(defun extract-registered-point ()
  "Extract the registered point - at the very end of the buffer"
  (save-excursion
    (goto-char (point-max))
    (let ((limit (point)))
       (beginning-of-line 1)
       (let ((registered-point (search-forward ";;; " limit t)))
         (if registered-point
             (string-to-number (name-under-point))
             nil)))))


(defun register-current-point (p)
  "Register point p at the very end of the buffer"
  (save-excursion
    (goto-char (point-max))
    (beginning-of-line 1)
    (if (looking-at ";;; ")
        (progn
           (beginning-of-line 1)
           (delete-line 1)))
    (insert (concat ";;; " (int-to-string p)))))

; ---------------------------------------------------------------------------------------------------------------

(defun midi-load ()
  (interactive)
  (insert "(laml-style \"xml-in-laml/midi/midi\")") (comint-send-input)
  (insert "(load (string-append laml-dir \"styles/xml-in-laml/midi/midi-laml-processing-lib.scm\"))") (comint-send-input)
)

; ---------------------------------------------------------------------------------------------------------------
; Experimental. Intended for a faster forward-noteon. NOT USED YET.
(defun advance-point-to-noteon-in-channels (channels &optional limit)
 "Advance point to a NoteOn in channel. 
If channels is t, any channel will do.
Postcondition: If found, point will be located at start parenthesis of the located NoteOn message. In that case, return t.
If not found, return nil." 
 (debug)
 (let ((ch-reg-exp (if (eq channels t)
                       (regular-expression "'channel")
                       (regular-expression `(concat "'channel" (zero-or-more (char-set tab newline space return)) 
                                                               "\"" 
                                                               (or ,@(mapcar (function number-to-string) channels))
                                                               "\""))))
       (found nil))
  (catch 'res
   (while (not found)
     (let ((p1 (search-forward "(NoteOn" limit t)))
       (if p1
           (if (eq channels t)
               (progn
                  (setq found t)
                  (goto-char (- p1 7)))
               (let ((p0 (- p1 7)))     ; at beginning of NoteOn form
                 (goto-char p0)
                 (forward-sexp 1)     ; at end of NoteOn form, bind pn
                 (let ((pn (point)))
                   (goto-char p1)
                   (let ((ch-res (re-search-forward ch-reg-exp pn t)))
                     (if ch-res 
                         (progn 
                           (setq found t)
                           (goto-char p0)
                           t)
                       (goto-char pn))))))
           (throw 'res nil)))))))

; ---------------------------------------------------------------------------------------------------------------

; Not used.
; (defun play-single-noteon-while-moving-point-abs-time (start end) 
;   (interactive "r")
;   (let* ((abs-time-list (collect-noteon-abs-times start end))
;          (delta-time-list (mapcar2 (function (lambda (t1 t2) (- t2 t1))) abs-time-list (cdr abs-time-list)))
;         )
;     (transient-mark-mode -1)
;     (goto-char start) (while (looking-at-white-space (point)) (forward-char 1))
;     (while (>= (length delta-time-list) 1)
;       (redisplay)
;       (execute-single-midl-event-synchroneous (car delta-time-list))  ; earlier version: play-single-noteon-maybe-cached
;       (redisplay)
;       (setq delta-time-list (cdr delta-time-list))
;       (forward-noteon 1)
;     )
;     (execute-single-midl-event-synchroneous 0)   ; earlier version: play-single-noteon-maybe-cached
;     (transient-mark-mode 1)))

; Do not use this function
(defun play-single-noteon-while-moving-point () 
  (interactive)
    (let* ((x-frame-position (cadr (mouse-pixel-position)))
           (speed (relative-playing-speed x-frame-position))
           (cache-success nil)
          )
      (while (>= speed 20)
        (message (concat (if (and use-single-note-midi-file-cache cache-success) "Via single note cache. " "Non-cached. ") 
                         "Speed: " (as-string speed) "%%. Move cursor to the right to slow down or stop."))
        (redisplay)
        (if use-single-note-midi-file-cache
            (let* ((abs-time (attribute-of-current-form 'absTime))
                   (channel  (attribute-of-current-form 'channel))
                   (note     (attribute-of-current-form 'note))
                   (vel      (attribute-of-current-form 'velocity))
                   (dur      (attribute-of-current-form 'duration))
                   (midi-file-path (concat (current-directory) "midi-temp/single-notes/" (single-midi-note-file-name abs-time channel note vel dur) "." "mid"))
                  )
              (if (file-exists-p midi-file-path)
                  (progn
                    (play-midi-file-sync midi-file-path)
                    (setq cache-success t)
                  )
                  (progn
                    (execute-single-midl-event-synchroneous speed)
                    (setq cache-success nil)
                  )
              )
           )
           (execute-single-midl-event-synchroneous speed)) 

        (setq x-frame-position (cadr (mouse-pixel-position)))
        (setq speed (relative-playing-speed x-frame-position))

        (condition-case nil
          (if (>= speed 10)
              (if (not (and (member (current-form-name) (list "NoteOn")) 
                            (appropriate-current-channel channels-to-play target-form-of-navigation)))
                  (if (eq channels-to-play t) 
                      (forward-noteon 0)
                    (forward-noteon 0 channels-to-play))
                (if (eq channels-to-play t) 
                    (forward-noteon 1)
                  (forward-noteon 1 channels-to-play))))
          (error (setq speed 0))  ; does not work!
        )
        (highlight-this-line)
       )
      (message "Playing has been stopped")))

(defun relative-playing-speed (x-frame-position)
  (let* ((fr-width (frame-pixel-width))
         (fr-width-half (/ fr-width 2)))
    (if (< x-frame-position fr-width-half)
        100
        (round (- 100
                  (* 100 
                     ( / (float (- x-frame-position fr-width-half 20))
                         (float fr-width-half))))))))

(defun relative-tempo (x-frame-position)
  (let ((fr-width (frame-pixel-width)))
    (round (* 100 (/ (float x-frame-position) fr-width)))))

(defun current-mouse-pixel-position ()
  (cadr (mouse-pixel-position)))

(defun centered-relative-tempo (x-pos)
  "A tempo percentage calcualted from the cursors x frame position x-pos in the selected window.
In a small middle interval, the tempo is 100. To the left it decreases linearly to nearly 0.
To the right it increases linearly to 500."
  (let* ((fr-width (frame-pixel-width))
         (fr-middle (/ fr-width 2))
         (fr-100-percent-middle-width (/ fr-width 20))
        )
    (cond ((and (>= x-pos (- fr-middle fr-100-percent-middle-width)) (<= x-pos (+ fr-middle fr-100-percent-middle-width)))  ; cursor in middle area
              100)
          ((< x-pos (- fr-middle fr-100-percent-middle-width))
             (let* ((x1 0)
                    (y1 10)
                    (x2 (- fr-middle fr-100-percent-middle-width))
                    (y2 100)
                    (slope (/ (float (- y2 y1)) (float (- x2 x1))))
                   )
               (round (+ (* slope x-pos) y1))))
          ((> x-pos (+ fr-middle fr-100-percent-middle-width))
             (let* ((x1 (+ fr-middle fr-100-percent-middle-width))
                    (y1 100)
                    (x2 fr-width)
                    (y2 500)
                    (slope (/ (float (- y2 y1)) (float (- x2 x1))))
                   )
               (round (+ (* slope (- x-pos x1)) y1))))
          (t (error "centered-relative-tempo: This case should not happen")))))


(defun collect-noteon-abs-times (start end)
 (interactive "r")
 (let ((res-lst nil))
   (save-excursion
     (goto-char start)
     (forward-noteon 0)
     (while (<= (point) end)
       (setq res-lst (cons (string-to-number (attribute-of-current-form 'absTime)) res-lst))
       (forward-noteon 1))
     (reverse res-lst))))

  
     

; ---------------------------------------------------------------------------------------------------------------

(defvar div-play-smf (concat div-midi-utilities-dir "playsmf.exe") "The full file path to the midi command line player.")
(defvar div-send-midi-exe (concat div-midi-utilities-dir "sendmidi.exe"))
(defvar div-lsmidiouts (concat div-midi-utilities-dir "lsmidiouts.exe"))
(defvar div-lsmidiins (concat div-midi-utilities-dir "lsmidiins.exe"))
(defvar div-brainstorm (concat div-midi-utilities-dir "brainstorm.exe"))

(defvar pending-off-args nil)

(defun execute-single-midl-event (&optional port0)
  "Assume that point is located at the initial parenthesis of a NoteOn message.  Play this NoteOn message. Do not move point. "
  (interactive)
  (let* ((port (if port0 port0 midi-out-port))
         (form-name (current-form-name))
         (SP        " ")
         (out-buf   (get-buffer-create "ooo"))
        )
    (cond ((equal form-name "NoteOn")
            (let* ((channel   (as-number (attribute-of-current-form 'channel)))  ; in the interval 1..16. When used below, the interval is 0..15. Thus 1 is subtracted.
                   (note      (attribute-of-current-form 'note))
                   (vel       (attribute-of-current-form 'velocity))
                   (on-args   (concat "--out" SP (as-string port) SP  "--note-on" SP
                                      (as-string (- channel 1)) SP (as-string note) SP  (as-string vel)))
                   (off-args  (concat "--out" SP (as-string port) SP  "--note-off" SP 
                                      (as-string (- channel 1)) SP (as-string note) SP  (as-string vel)))
                  )
               (if pending-off-args
                  (start-process "PlayOff" nil cygwin-shell "-c" (concat div-send-midi-exe SP pending-off-args )))
               (start-process "PlayOn" nil cygwin-shell "-c" (concat div-send-midi-exe SP on-args))
               (setq pending-off-args off-args)
               ))  ; (forward-midi-message)

          ((equal form-name "ProgramChange")
            (let* ((channel   (as-number (attribute-of-current-form 'channel)))
                   (number    (attribute-of-current-form 'number))
                   (pc-args   (concat "--out" SP (as-string port) SP  "--program-change" SP
                                      (as-string (- channel 1)) SP (as-string number))))
               (call-process cygwin-shell nil out-buf t "-c" (concat div-send-midi-exe SP pc-args))
               (message "ProgramChange DONE")))

          ((equal form-name "ControlChange")
            (let* ((channel    (as-number (attribute-of-current-form 'channel)))
                   (control    (attribute-of-current-form 'control))
                   (value      (attribute-of-current-form 'value))
                   (cc-args   (concat "--out" SP (as-string port) SP  "--control-change" SP 
                                      (as-string (- channel 1)) SP (as-string control) SP (as-string value) SP)))
               (call-process cygwin-shell nil out-buf t "-c" (concat div-send-midi-exe SP cc-args))
               (message "ControlChange DONE")))

          (t (error (concat "execute-single-midl-event: Unsupported event:" SP form-name))))))

; Do not use this function
(defun execute-single-midl-event-synchroneous (speed &optional port0)
  "Assume that point is located at the initial parenthesis of a NoteOn message.  Play that note without moving point.
In contrast to execute-single-midl-event, also stops the playing again.
"
  (interactive)
  (let* ((port (if port0 port0 midi-out-port))
         (form-name (current-form-name))
         (SP        " ")
         (out-buf   (get-buffer-create "ooo"))
        )
    (cond ((equal form-name "NoteOn")
            (let* ((channel   (as-number (attribute-of-current-form 'channel)))  ; in the interval 1..16. 
                                                                                 ; When used below, the interval is 0..15. 
                   (note      (attribute-of-current-form 'note))
                   (vel       (attribute-of-current-form 'velocity))
                   (on-args   (concat "--out" SP (as-string port) SP  "--note-on" SP
                                      (as-string (- channel 1)) SP (as-string note) SP  (as-string vel)))
                   (off-args  (concat "--out" SP (as-string port) SP  "--note-off" SP
                                      (as-string (- channel 1)) SP (as-string note) SP  (as-string vel)))
                  )
               (call-process cygwin-shell nil nil t "-c" (concat div-send-midi-exe SP on-args))

               (let*                                                                                      ; BINDINGS CURRENTLY UNUSED.
                     ((ppqn 480)  ; more generally, take from context.
                      (bpm 120) ; beats = quarter notes per minutes (= tempo). More generally, take from context. 
                      (tempo-mikro-sek-per-qn (/ 60000000.0 bpm))
                      (time-mikro-sek-per-tick (/ tempo-mikro-sek-per-qn ppqn))
                      (duration-seconds (/ (* (as-number dur) time-mikro-sek-per-tick) (float 1000000)))   ; dur not defined
                      (midi-player-latency (/ command-line-player-startup-latency 2))
                     )

                 (sit-for (max 0.05 (* (- 100 speed)  0.13)))  
                 ; (sit-for (+ midi-player-latency duration-seconds)) 
                 (start-process "PlayOff" nil cygwin-shell "-c" (concat div-send-midi-exe SP off-args)))))


          (t (error (concat "execute-single-midl-event-synchroneous: Unsupported event:" SP form-name))))))

(defun stop-pending-midi-event (&optional port0)
  "Assume that point is located at the initial parenthesis of a NoteOn message. Do not move point.
Play it either via play-note-on, or if possible, via chached midi files in midi-temp/single-notes"
  (interactive)
  (let* ((port (if port0 port0 midi-out-port))
         (SP        " ")
        )
    (if pending-off-args
        (progn 
          (start-process "PlayOff" nil cygwin-shell "-c" (concat div-send-midi-exe SP pending-off-args))
          (setq pending-off-args nil))
        (message "No pending note."))))


(defun show-midi-out-ports ()
  (interactive)
  (message "Querying for midi out ports...")
  (let ((out-buf (get-buffer-create "Midi output ports")))
    (set-buffer out-buf)
    (toggle-read-only -1)
    (erase-buffer)
    (call-process cygwin-shell nil out-buf t "-c" (concat div-lsmidiouts))
    (delete-other-windows)
    (split-window-vertically)
    (other-window 1)
    (set-window-buffer (selected-window)  out-buf)
    (goto-char (point-min))
    (toggle-read-only 1)
    (other-window 1)
    (message "Done. Current midi output port is %d." midi-out-port)))

(defun show-midi-in-ports ()
  (interactive)
  (message "Querying for midi in ports...")
  (let ((out-buf (get-buffer-create "Midi input ports")))
    (set-buffer out-buf)
    (toggle-read-only -1)
    (erase-buffer)
    (call-process cygwin-shell nil out-buf t "-c" (concat div-lsmidiins))
    (delete-other-windows)
    (split-window-vertically)
    (other-window 1)
    (set-window-buffer (selected-window)  out-buf)
    (goto-char (point-min))
    (toggle-read-only 1)
    (other-window 1)
    (message "Done. Current midi input port is %d." midi-in-port)))


(defvar seconds-until-stop 0)

(defun stop-midi-playing (&optional silent)
  "Interrupt MIDI playing."
  (interactive)
  (let ((process-name midi-play-process-name))

    (setq seconds-until-stop (max 0.0 (- (float-time) ( + start-play-time command-line-player-startup-latency))))  ; global variable, for the sake of
                                                                                                                   ; locate-point-at-quit-position-if-applicable
    (if now-live-moving-cursor 
        (progn
          (keyboard-quit)
        ))

    ; send note-off event of pending midi message, if there is pending note offs
    (if pending-off-args
        (let ((SP " "))
          (call-process cygwin-shell nil nil t "-c" (concat div-send-midi-exe SP pending-off-args))
          (setq pending-off-args nil)))

    ; kill the MIDI play process, if there:
    (if (get-process process-name)
        (delete-process process-name)
    )

    (delete-processes-with-prefix "PlayOn")    
    (delete-processes-with-prefix "PlayOff")    

    ; Fire the gm-reset.mid file, thus resetting the midi system:
    (play-midi-file (concat (concat laml-dir "styles/xml-in-laml/midi/" "midi/all-sound-off.mid")) midi-out-port t)
    ; earlier gm-reset. Possible alternative to all-sound-off.mid: all-notes-off.mid

    (if (not silent) (message "Stopped MIDI playing."))
    (run-at-time 0.3 nil (function locate-point-at-quit-position-if-applicable))

    ; compensates for irritating scroll problem when point is being put at last line of window:
    (if (and window-start-value-before-page-play (> (window-start) window-start-value-before-page-play))
        (progn   
           (scroll-down-1)
           (setq window-start-value-before-page-play nil)))
  )
)


(defun locate-point-at-quit-position-if-applicable ()
  (interactive)
  (if (and (> seconds-until-stop 0) (not (null live-cursor-parameters)))
      (progn
         (apply (function locate-point-at-quit-position) (cons seconds-until-stop live-cursor-parameters))
         (setq live-cursor-parameters nil)
         (setq seconds-until-stop 0))))


(defun locate-point-at-quit-position (seconds-until-stop pause-times in-buf p0 pn play-channels ppqn tempo-mikro-sek-per-qn)
  (let* ((time-mikro-sek-per-tick (/ (float tempo-mikro-sek-per-qn) ppqn))
         (forward-ticks (/ (* seconds-until-stop 1000000) time-mikro-sek-per-tick))
         (rest-ticks forward-ticks)
         (cur-pause-times pause-times)
         (done nil)
        )
    (switch-to-buffer in-buf)       ; necessary?
    (goto-char p0) 
    (forward-midi 0 play-channels)

    (setq rest-ticks (- rest-ticks (car cur-pause-times)))
    (setq cur-pause-times (cdr cur-pause-times))
    (while (and (< (point) pn) (> rest-ticks 0) (not done)) 
      (forward-midi 1 play-channels)
      (if (and (< (point) pn)) 
          (let ((pause-tm (if (null cur-pause-times) 1 (car cur-pause-times))))
            (setq rest-ticks (- rest-ticks (car cur-pause-times)))
            (setq cur-pause-times (cdr cur-pause-times))
          )))
    (highlight-this-line)))


(defun stop-midi-processes ()
  (interactive)
  (let ((process-name midi-play-process-name))

    ; kill the MIDI play process, if there:
    (if (get-process process-name)
        (delete-process process-name)
    )

    (delete-processes-with-prefix "PlayOn")    
    (delete-processes-with-prefix "PlayOff")

    (message "Done")))

(defun delete-processes-with-prefix (process-prefix-name)
  (let ((plist (process-list)))
    (mapcar 
     (function
      (lambda (p)
        (if (equal process-prefix-name
                   (substring (process-name p) 0 (min (length process-prefix-name) (length (process-name p)))))
            (delete-process p))))
     plist)))

(defun set-midi-ports (output-port-number input-port-number)
  "Convenient setting of both midi-out-port and midi-in-port"
  (interactive (list
                  (read-from-minibuffer (concat "New output port (was " (as-string midi-out-port) "): "))
                  (read-from-minibuffer (concat "New input port (was " (as-string midi-in-port) "): "))))
  (set-midi-output-port (as-number output-port-number))
  (set-midi-input-port  (as-number input-port-number)))

(defun set-midi-output-port (port-number)
  (interactive (list (read-from-minibuffer (concat "New output port (was " (as-string midi-out-port) "): "))))
  (setq midi-out-port (as-number port-number))
  (message (concat "New output port is " (as-string midi-out-port) ".")))

(defun set-midi-input-port (port-number)
  (interactive (list (read-from-minibuffer (concat "New input port (was " (as-string midi-in-port) "): "))))
  (setq midi-in-port (as-number port-number))
  (message (concat "New input port is " (as-string midi-in-port) ".")))


(defun guess-output-port (port-description-string)
  "Return the output port number of the port description string, such as \"Yamaha MOTIF XS8-1\". 
Returns a number or nil in case it is not possible to locate a suitable midi output port."
  (save-excursion
   (let ((out-buf (get-buffer-create "*MidiOutputPorts*")))
    (set-buffer out-buf)
    (call-process cygwin-shell nil out-buf t "-c" (concat div-lsmidiouts))
    (goto-char (point-min))
    (let ((search-result (search-forward port-description-string nil t))
          (result nil))
      (if search-result
          (progn
            (beginning-of-line 1)
            (while (looking-at-chars (point) white-space-char-list) (forward-char 1))
            (setq result (as-number (name-under-point))))
          (setq result nil))
      (kill-buffer out-buf)
      result))))

(defun guess-input-port (port-description-string)
  "Return the input port number of the port description string, such as \"Yamaha MOTIF XS8-1\". 
Returns a number or nil in case it is not possible to locate a suitable midi output port."
  (save-excursion
   (let ((out-buf (get-buffer-create "*MidiInputPorts*")))
    (set-buffer out-buf)
    (call-process cygwin-shell nil out-buf t "-c" (concat div-lsmidiins))
    (goto-char (point-min))
    (let ((search-result (search-forward port-description-string nil t))
          (result nil))
      (if search-result
          (progn
            (beginning-of-line 1)
            (while (looking-at-chars (point) white-space-char-list) (forward-char 1))
            (setq result (as-number (name-under-point))))
          (setq result nil))
      (kill-buffer out-buf)
      result))))

(defun description-of-input-port (port-number)
  "Return a textual description of input port-number, looked up at the time this function is called (via use of lsmidiins.exe)."
 (save-excursion
   (let ((out-buf (get-buffer-create "*MidiInputPortsInternal*")))
    (set-buffer out-buf)
    (call-process cygwin-shell nil out-buf t "-c" (concat div-lsmidiins))
    (goto-char (point-min))
    (let ((search-result (re-search-forward (regular-expression `(concat (at-beginning-of-line ,(concat "  " (as-string port-number) " "))))  nil t))
          (result nil))
      (if search-result
          (let ((p0 (point)))
            (end-of-line 1)
            (setq result (buffer-substring-no-properties p0 (point))))
          (setq result nil))
      (kill-buffer out-buf)
      result))))

(defun description-of-output-port (port-number)
  "Return a textual description of output port-number, looked up at the time this function is called (via use of lsmidiouts.exe)."
 (save-excursion
   (let ((out-buf (get-buffer-create "*MidiOutputPortsInternal*")))
    (set-buffer out-buf)
    (call-process cygwin-shell nil out-buf t "-c" (concat div-lsmidiouts))
    (goto-char (point-min))
    (let ((search-result (re-search-forward (regular-expression `(concat (at-beginning-of-line ,(concat "  " (as-string port-number) " "))))  nil t))
          (result nil))
      (if search-result
          (let ((p0 (point)))
            (end-of-line 1)
            (setq result (buffer-substring-no-properties p0 (point))))
          (setq result nil))
      (kill-buffer out-buf)
      result))))


; Set preferred instrument, and the midi-out-port and the midi-in-port accordingly.
(defvar preferred-instrument 'pc-sound-card "The preferred instrument in the MIDI LAML Environment. One of motif-xs, tyros, or pc-sound-card")

(defun set-instrument (i)
  "Set the instrument, and hereby - underlying - the current midi-input-port and the current midi-output-port."
  (interactive (list
                  (completing-read (concat "Instrument (now " (as-string preferred-instrument) "): ") '(("tyros" . nil) ("motif-xs" . nil) ("pc-sound-card" . nil)) nil nil)))

  ; Placed here because this function is always called. But not a good place
  (if (not is-command-line-player-startup-latency-estimated)
      (progn
        (message "Finding latency of the command line midi player on this machine...")
        (setq command-line-player-startup-latency (startup-latency-of-command-line-midi-player))
        (setq is-command-line-player-startup-latency-estimated t)
        (message "Done: %d." command-line-player-startup-latency)))

  (cond ((equal i "tyros") (set-instrument-tyros)) 
        ((equal i "motif-xs") (set-instrument-motif-xs))
        ((equal i "pc-sound-card") (set-instrument-pc-sound-card))
        (t (error "Unsupported instrument: %s" i))))

(defun set-midi-player (p)
  (interactive (list
                  (completing-read (concat "Midi player (now: " (as-string midi-player) "): ")
                                   '(("command-line-player" . nil) ("winamp" . nil)) nil t)))
  (setq midi-player (as-symbol p))
  (message "Done. Player = %s, instrument = %s, midi output port = %d, midi input port= %d."
           midi-player preferred-instrument midi-out-port midi-in-port)
)

(defun set-instrument-tyros ()
  (interactive)
  (message "Preferred instrument set to ...")
  (write-instrument 'tyros)
  (setq preferred-instrument 'tyros)
  (setq use-voice-context t)
  (let ((out-port (guess-output-port tyros-string-id))
        (in-port (guess-input-port tyros-string-id)))
    (if out-port (setq midi-out-port out-port))
    (if in-port  (setq midi-in-port  in-port))
    (show-midl-mode-status-string)
    (message 
     (concat "Preferred instrument set to Tyros. "
      (if (and out-port in-port)
          (format "midi-out-port = %d, midi-int-port = %d." out-port in-port)
          "The Tyros seems not to be connected - the midi-out-port and midi-int-port are NOT changed"))))
)

(defun set-instrument-motif-xs ()
  (interactive)
  (message "Preferred instrument set to ...")
  (write-instrument 'motif-xs)
  (setq preferred-instrument 'motif-xs)
  (setq use-voice-context nil)
  (let ((out-port (guess-output-port motif-xs-8-string-id))
        (in-port (guess-input-port motif-xs-8-string-id)))
    (if out-port (setq midi-out-port out-port))
    (if in-port  (setq midi-in-port  in-port))
    (show-midl-mode-status-string)
    (message 
      (concat "Preferred instrument set to Motif-XS. "
       (if (and out-port in-port)
           (format "midi-out-port = %d, midi-int-port = %d." out-port in-port)
           "The Motif-XS seems not to be connected - the midi-out-port and midi-int-port are NOT changed"))))
)

(defun set-instrument-pc-sound-card ()
  (interactive)
  (message "Preferred instrument set to ...")
  (write-instrument 'pc-sound-card)  ; general midi
  (setq preferred-instrument 'pc-sound-card)
  (setq use-voice-context nil)
  (let ((out-port (guess-output-port pc-sound-card-string-id))
        (in-port (guess-input-port   pc-sound-card-string-id)))
    (if out-port (setq midi-out-port out-port))
    (if in-port  (setq midi-in-port  in-port))
    (show-midl-mode-status-string)
    (message (format "midi-out-port = %d, midi-int-port = %d." midi-out-port midi-in-port))
  ))

(defun instrument-info ()
  (interactive)
  (message "The current preferred instrument is %s. Current midi output port = %d. Current midi input port = %d." preferred-instrument midi-out-port midi-in-port))

(defun write-instrument (i)
 (let ((instrument-file-path (concat midi-software-dir "preferred-instrument")))
  (if (file-exists-p instrument-file-path)
      (delete-file instrument-file-path))
  (file-write-no-message (as-symbol i) instrument-file-path)))


(defvar recording-pause-number-of-seconds 15 "The number of seconds after last midi events, which causes start of a new midi file.")
(defvar do-midi-output-test-before-recording t "Play a sample midi file before recording. Good for testing of connections and instrument USB status.")
(defvar play-midi-end-of-recording-notes t "Play a few notes to demark the end of recording.")

; Assigned by record-from-midi-port, and used by stop-recording:
(defvar pre-recording-background-color nil)
(defvar pre-recording-foreground-color nil)
(defvar pre-recording-number-of-midi-files 0) ; meaning uninitialized.
(defvar pre-recording-input-port-number 0) ; meaning uninitialized. NOT currently used.
(defvar pre-recording-input-port-description "") ; meaning uninitialized.

(defun record-from-midi-port ()
  (interactive)
 
  ; Testing instrument (midi-out-port):
  (if do-midi-output-test-before-recording
      (progn 
        (message "You should now hear a few notes (not recorded)")
        (play-midi-file (concat midi-software-dir "midi/" "pre-recording-notes-short.mid") midi-out-port t)))


  (let ((process-name "MIDI Recording"))
    (if (not (get-process process-name))
      (let ((port midi-in-port)  ;   (as-number (read-from-minibuffer "Record from midi port: "))
            (SP " ")
            (time-out recording-pause-number-of-seconds)
            (out-buf (get-buffer-create "ooo")) ; not used
           )
        (setq pre-recording-background-color (assoc-get-cdr 'background-color (frame-parameters)))
        (setq pre-recording-foreground-color (assoc-get-cdr 'foreground-color (frame-parameters)))
        (setq pre-recording-number-of-midi-files (length (recorded-midi-files)))
        (setq pre-recording-input-port-number port)
        (setq pre-recording-input-port-description (description-of-input-port port))
        (message (format "Recording from MIDI port %s (= %s) in %s. Stop: M-x stop-recording. Status: M-x recording-status" 
                         port pre-recording-input-port-description recording-dir))
        (start-process process-name nil cygwin-shell "-c" 
                       (concat "cd " recording-dir ";"
                               div-brainstorm SP "--in" SP (as-string port) SP "--timeout" SP (as-string time-out) SP "--prefix" SP "rec-"))
        (red-background)
      )
      (message "Recording already in progress. To stop it use M-x stop-recording"))))

; brainstorm [ --in <device id> ] [ --timeout <seconds> ] [ --extra-time <seconds> ] [ --prefix <filename prefix> ] [ --verbose ]

(defun stop-recording ()
  (interactive)
  (let ((process-name "MIDI Recording"))

    ; kill the MIDI recording process, if there and show recorded files.
    (if (get-process process-name)
        (progn (delete-process process-name)   ; the recorded messages are properly flushed and closed!
               (set-background-color pre-recording-background-color)
               (set-foreground-color pre-recording-foreground-color)
               (delete-other-windows)
               (split-window-vertically)
               (other-window 1)
               (dired recording-dir)
               (dired-sort-other (concat dired-listing-switches "t"))  ; always sort by modification time
               (goto-char (point-min))
               (search-forward "rec-20" nil t)
               (search-backward "rec-20" nil t)

               (if play-midi-end-of-recording-notes
                   (progn 
                     (play-midi-file (concat midi-software-dir "midi/" "post-recording-notes-short.mid") midi-out-port t)))

               (let* ((post-recording-number-of-midi-files (length (recorded-midi-files)))
                      (recording-count (- post-recording-number-of-midi-files pre-recording-number-of-midi-files))
                    )
                 (cond ((= recording-count 0) (message "Done. NOTHING recorded. Is the instrument at USB? Has the right instrument been selected?"))
                       ((= recording-count 1) (message (format "DONE. %s MIDI file has been recorded." recording-count)))
                       ((> recording-count 1) (message (format "DONE. %s MIDI files have been recorded." recording-count)))
                       (t "Done. Should not happen")))
               (setq pre-recording-background-color nil)
               (setq pre-recording-foreground-color nil)
               (setq pre-recording-number-of-midi-files 0)
               (setq pre-recording-input-port-number 0)
               (setq pre-recording-input-port-description "")
        )
        (message "Nothing to stop."))))

(defun recording-status ()
  "Display recording status in the mini buffer."
  (interactive)
  (let* ((recording-p (get-process "MIDI Recording"))
         (cur-recording-number-of-midi-files (length (recorded-midi-files)))
         (recording-count (- cur-recording-number-of-midi-files pre-recording-number-of-midi-files)))
    (if recording-p
        (message "Currently we are recording at in-port %s (= %s). Number of recorded files: %s. Creating new files after %s seconds of silence."
                 midi-in-port pre-recording-input-port-description recording-count recording-pause-number-of-seconds)
        (message "NO recording takes place."))))

        

(defun dired-recordings ()
  (interactive)
  (dired recording-dir)
  (dired-sort-other (concat dired-listing-switches "t")) ; always sort by modification time
  (goto-char (point-min))
  (search-forward "rec-20" nil t)
  (search-backward "rec-20" nil t)
  (message "DONE"))

(defun recorded-midi-files ()
 "Return the list of already recorded midi files in the designated recording-dir"
 (filter
   (function (lambda (x)
               (and (file-regular-p (concat recording-dir x)) (equal (file-name-extension x) "mid"))))
   (directory-files recording-dir)))



; -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
; GHOST FRAGMENTS COLLECTION.

(defvar midi-functions-files-list 
  (list
    (concat laml-dir "styles/xml-in-laml/midi/" "man/" "midi-laml-processing-lib.manlsp")
  )
  "The list of schemedoc manlsp files taken into consideration when collecting fragments in ghost files and when finding out about normalization of a MIDL buffer.")

(defvar midi-functions-files-contents nil "title and form fields - in a list of list structure - of forms from midi-functions-files-list. 
  The function ensure-and-get-function-list-from-schemedoc-documentation reads and caches the list.")

(defvar negative-abstractions-for-ghost-fragments (list "midi") "A list of function names to be excluded from the ghost fragment collection")


; Functions that calculates regular expressions via which we collect ghost fragments:

(defun midi-functions-reg-exp (function-name-list)
  `(or ,@(mapcar (function (lambda (n) `(concat "(" (zero-or-more (char-set tab newline space return)) ,n (one-or-more (char-set tab newline space return)) ))) function-name-list)))

(defun definition-reg-exp ()
  `(concat "(" (zero-or-more (char-set tab newline space return)) "define" (one-or-more (char-set tab newline space return))))

(defun abs-time-attr-reg-exp (&optional time)
  (if time
     `(concat (one-or-more (char-set tab newline space return)) "'absTime" (one-or-more (char-set tab newline space return)) "\"" ,(as-string time) "\"" (one-or-more (char-set tab newline space return)))
     `(concat (one-or-more (char-set tab newline space return)) "'absTime" (one-or-more (char-set tab newline space return)))))



(defvar count-of-ghost-fragments 0 "For internal use in add-all-ghost-fragments-of-midl-stack")
(defvar count-of-ghost-fragments-for-feedback-purposes 0 "A similar count, updated for feedback purposes only.")

(defun add-all-ghost-fragments-of-midl-stack ()
  "Activated from a midi laml file, find the relevant source fragment in ghost files, and present then in a ghost fragment browser."
  (interactive)
  (setq count-of-ghost-fragments 0)
  (setq count-of-ghost-fragments-for-feedback-purposes 0)
  (define-ghost-fragments-of-current-midl-stack 'ghosts-only)
  (message "Done. Inserted %d ghost fragments." count-of-ghost-fragments)
  (ghost-fragments-browser)

  ; Highlight first entry, if possible:
  (let ((res (search-forward "  1" nil t)))
    (if res
        (progn 
           (beginning-of-line 1)
           (highlight-this-line))
        (progn
           (message "There are no existing ghost fragments. Use M-x add-all-ghost-fragments-of-midl-stack or M-x add-project-wide-ghost-fragments-of-midl-stack to do so. ")
           (other-window 1))))
)

(defun add-project-wide-ghost-fragments-of-midl-stack ()
  "Activated from a midi laml file, find the relevant source fragment in the current MIDL file, the stack fiels and the ghost files,
and present then in a ghost fragment browser."
  (interactive)
  (setq count-of-ghost-fragments 0)
  (setq count-of-ghost-fragments-for-feedback-purposes 0)
  (define-ghost-fragments-of-current-midl-stack 'all)
  (message "Done. Inserted %d ghost fragments." count-of-ghost-fragments)
  (ghost-fragments-browser)

  ; Highlight first entry, if possible:
  (let ((res (search-forward "  1" nil t)))
    (if res
        (progn 
           (beginning-of-line 1)
           (highlight-this-line))
        (progn
           (message "There are no existing ghost fragments. Use M-x add-all-ghost-fragments-of-midl-stack or M-x add-project-wide-ghost-fragments-of-midl-stack to do so. ")
           (other-window 1))))
)

(defun reset-ghost-fragments ()
  (interactive)
  (let* ((buf-name ghost-fragment-browser-name)
         (current-project-name (file-name-proper (file-name-proper (current-file))))
         (project-dir (concat (current-directory) current-project-name "/"))
         (ghost-fragment-dir (concat project-dir "ghost-fragments/"))
         (ghost-fragment-files (directory-files ghost-fragment-dir)))
    (mapcar
      (function (lambda (f)
                   (if (not (or (equal f ".") (equal f "..")))
                       (delete-file (concat ghost-fragment-dir f)))))
      ghost-fragment-files)
   (message "All ghost fragments have been deleted")))


(defun browse-existing-ghost-fragments ()
  (interactive)
  (ghost-fragments-browser)
 
  ; Highlight first entry, if possible:
  (let ((res (search-forward "  1" nil t)))
    (if res
        (progn 
           (beginning-of-line 1)
           (highlight-this-line))
        (progn
           (message "There are no existing ghost fragments. Use M-x add-all-ghost-fragments-of-midl-stack or M-x add-project-wide-ghost-fragments-of-midl-stack to do so. ")
           (other-window 1)))))

(defun titles-and-forms-from-schemedoc-files (schemedoc-manlsp-file-list)
  "Return a list of entries of the form (name signature) of all documented entries from the list manlsp files in schemedoc-manlsp-file-list."
  (let* ((manlsp-list-list (mapcar (function file-read-schemedoc) schemedoc-manlsp-file-list))
         (documented-scheme-names-list
            (mapcar (function (lambda (manlsp-list)
                                 (mapcar (function (lambda (manlsp-entry) 
                                                      (if (assoc-get 'title manlsp-entry)
                                                          (list (assoc-get 'title manlsp-entry) (assoc-get 'form manlsp-entry))
                                                          nil)))
                                         manlsp-list)))
                     manlsp-list-list))
         (filtered-documented-scheme-names-list (mapcar (function (lambda (name-list) (filter (function (lambda (e) e)) name-list))) documented-scheme-names-list))
        )
      (apply (function append) filtered-documented-scheme-names-list)     
    ))

; Cache the function list taken from Schemedoc documentation in the variable midi-functions-files-contents.
; Return the function list.
(defun ensure-and-get-function-list-from-schemedoc-documentation (midi-functions-files-list)
  (if midi-functions-files-contents
      midi-functions-files-contents
      (progn
        (setq midi-functions-files-contents (titles-and-forms-from-schemedoc-files midi-functions-files-list))
        midi-functions-files-contents)))

(defun define-ghost-fragments-of-current-midl-stack (amount)
  "Define ghost fragments of all the current MIDL stack ghost files relative to the current state of the current -.midl file.
First, however, read and cache information from the manlsp files in midi-functions-files-list.
amount is either all or ghosts-only (a symbol)."

  ; Read and prepare information from manlsp files, pointed out by midi-functions-files-list. A kind of caching.

  (ensure-and-get-function-list-from-schemedoc-documentation midi-functions-files-list)

  (let* ((current-project-name (file-name-proper (file-name-proper (current-file))))
         (project-dir (concat (current-directory) current-project-name "/"))
         (ghost-file-path-list (cond ((eq amount 'ghosts-only) (find-all-ghost-files project-dir))
                                     ((eq amount 'all) (append
                                                         (find-all-stack-files project-dir)
                                                         (list (concat (current-directory) (current-file))) 
                                                         (find-all-ghost-files project-dir)
                                                         ))
                                     (t (error "define-ghost-fragments-of-current-midl-stack: Unknown amount parameter" amount))))
        )
    (mapcar (function (lambda (gfp)
                         (define-ghost-fragments-from-file gfp project-dir (length (find-all-stack-files project-dir)))))
            ghost-file-path-list)))


(defun define-ghost-fragments-from-file (ghost-file-path project-project-dir-path position-of-current-file)
  "Collect all ghost fragments in ghost-file-path and write them into the ghost-fragments directory of project-project-dir-path.
ghost-file-path is the file path to a ghost file on the MIDL stack.
project-project-dir-path is the full path to the MIDL project directory, in which we store the fragments.
position-of-current-file is the 0-based position of the current file when defining the fragment"
  (let ((fragments (collect-ghost-fragments-from-file ghost-file-path position-of-current-file)))
    (ensure-directory-existence project-project-dir-path "ghost-fragments")
    (setq count-of-ghost-fragments (+ count-of-ghost-fragments (length fragments)))
    (mapcar
      (function (lambda (frag)
                   (file-write frag (concat project-project-dir-path "ghost-fragments/" (next-ghost-name-fragment project-project-dir-path)))))
      fragments)))



(defun collect-ghost-fragments-from-file (file-path position-of-current-file)
  "Collect all ghost fragments in a single ghost file (on the MIDL stack).
The full path the ghost file (on the MIDL stack) is file-path."
  (save-excursion 
   (let ((temp-buf (generate-new-buffer "*ghost-file*"))
         (fns-reg-exp (midi-functions-reg-exp (documented-abstractions-in-schemedoc-contents midi-functions-files-contents)))
         (def-reg-exp (definition-reg-exp))
         (result nil)
        )
     (set-buffer temp-buf)
     (insert-file-contents file-path)
     (goto-char (point-min))

     (while (re-search-forward (regular-expression def-reg-exp) nil t)
       (let ((p (point)))
          (setq result (cons (capture-surrounding-definition file-path position-of-current-file) result))
          (setq count-of-ghost-fragments-for-feedback-purposes (+ count-of-ghost-fragments-for-feedback-purposes 1))
          (message (make-string count-of-ghost-fragments-for-feedback-purposes ?.))  ; feedback
       )
     )

     ; The cursor point is now after all definitions. The remaining capturings start at this position.
     ; This assumes that all definition come before the StandardMidiFile (or similar) form.

     (while (re-search-forward (regular-expression fns-reg-exp) nil t)
       (let ((p (point)))
          (setq result (cons (capture-surrounding-function-call file-path position-of-current-file) result))
          (setq count-of-ghost-fragments-for-feedback-purposes (+ count-of-ghost-fragments-for-feedback-purposes 1))
;         (message (make-string count-of-ghost-fragments-for-feedback-purposes ?.))   ; feedback
       )
     )

     (kill-buffer temp-buf)

     (reverse result))))

; Not used
(defun documented-abstractions-in-schemedoc-files (schemedoc-manlsp-file-list)
  "Return the list of documented names from the internal SchemeDoc manlsp files pointed out by the list schemedoc-manlsp-file-list"
  (let* ((manlsp-list-list (mapcar (function file-read-schemedoc) schemedoc-manlsp-file-list))
         (documented-scheme-names-list (mapcar (function (lambda (manlsp-list) (mapcar (function (lambda (manlsp-entry) (assoc-get 'title manlsp-entry))) manlsp-list))) manlsp-list-list))
         (filtered-documented-scheme-names-list (mapcar (function (lambda (name-list) (filter (function (lambda (e) e)) name-list))) documented-scheme-names-list))
        )
    (list-difference (apply (function append) filtered-documented-scheme-names-list) negative-abstractions-for-ghost-fragments)))

(defun documented-abstractions-in-schemedoc-contents (contents-list)    ; a flat list of entries of the form (name form)
  "Return the list of documented names from the contents-list. Subtract names in negative-abstractions-for-ghost-fragments."
 (list-difference (mapcar (function car) contents-list) negative-abstractions-for-ghost-fragments))


(defun find-all-ghost-files (project-dir)
  "Return a sorted list of full paths to the ghost files (on the MIDL stack) in project-dir"
  (if (file-exists-p project-dir)
      (let* ((ghost-files (filter 
                           (function (lambda (x) 
                                       (let ((y (file-name-proper (file-name-nondirectory x))))
                                         (ghost-name-p y))))
                           (directory-files project-dir)))
             (sorted-ghost-files 
               (sort ghost-files      ; each of the form "n-ghost.midl".   Sort in numeric order after n.
                     (function
                       (lambda (s1 s2)
                         (let ((s1-num (get-ghost-number s1))
                               (s2-num (get-ghost-number s2)))
                           (<= s1-num s2-num))))))
            )
        (mapcar (function (lambda (gf) (concat project-dir gf))) sorted-ghost-files))
   nil))

(defun find-all-stack-files (project-dir)
  "Return a sorted list of full paths to the stack files - non ghost files (on the MIDL stack) in project-dir"
  (if (file-exists-p project-dir)
      (let* ((stack-files (filter 
                           (function (lambda (x) 
                                       (let ((y (file-name-proper (file-name-nondirectory x))))
                                         (stack-name-p y))))
                           (directory-files project-dir)))
             (sorted-stack-files 
               (sort stack-files      ; each of the form "n.midl".   Sort in numeric order after n.
                     (function
                       (lambda (s1 s2)
                         (let ((s1-num (as-number (file-name-proper s1)))
                               (s2-num (as-number (file-name-proper s2))))
                           (<= s1-num s2-num))))))
            )
        (mapcar (function (lambda (gf) (concat project-dir gf))) sorted-stack-files))
   nil))

(defun get-ghost-number (gns)   ; gn of the form "n-ghost.midl"
  (let ((spl-lst (split-string gns "-")))
    (as-number (car spl-lst))))

  
(defun capture-surrounding-definition (ghost-file-path position-of-current-file)
  "Precondition: point just after '(define '. Return the surrounding definition (as a string).
Leave point just after the definition."
    (backward-up-list 1)
    (let ((p0 (point)))
       (forward-sexp 1)
       (let ((pn (point)))
         (prog1
           (make-ghost-fragment (buffer-substring-no-properties p0 pn) ghost-file-path nil nil position-of-current-file)
           (goto-char pn)))))

(defun capture-surrounding-function-call (ghost-file-path position-of-current-file)
  "Precondition: The cursor point is just after the funtion name, which is called. Return the surrounding function call with all actual parameters (as a string).
Leave the cursor after required parameters (if the signature prescribes rest parameters: (f p1 p2 . rest)) or after the calling form as such."
  (backward-sexp 1)
  (let* ((p0 (point))         ; just before the name of the calling form
         (fu-name (name-under-point))
         (fu-entry (find-in-list (function (lambda (e) (equal (car e) fu-name))) midi-functions-files-contents))
         (fu-signature (if fu-entry (cadr fu-entry) nil))
         (rest-par (if fu-signature (rest-parameters-in-signature fu-signature) nil))
         (mode (find-processing-mode-of-current-buffer))
        )
         
    (backward-up-list 1)
    (let ((p1 (point))        ; just before calling form
          (start-time nil)
          (end-time nil))
      (forward-sexp 1)
      (let ((pn (point)))

        ; Find last absTime attribute
        (goto-char pn) 
        (if (re-search-backward (regular-expression (abs-time-attr-reg-exp)) p1 t)
            (progn
              (backward-up-list 1)
              (setq end-time (attribute-of-current-form 'absTime))))

        ; Find first absTime attribute
        (goto-char p1) 
        (if (re-search-forward (regular-expression (abs-time-attr-reg-exp)) pn t)
            (progn
              (backward-up-list 1)
              (setq start-time (attribute-of-current-form 'absTime))))

        ; Adjust final position of cursor point:
        (if rest-par  
            (progn   ; rest-par is the number of rest parameters
               (goto-char p0)
               (forward-sexp (+ rest-par 1)))       ; Now just after the required parameters.
            (progn
               (goto-char p1)
               (forward-sexp 1))                    ; Now just past the function call
        )

        (cond ((and (equal mode "absTime") rest-par start-time end-time)
                (make-ghost-fragment (two-part-buffer-substring-no-properties p0 rest-par) ghost-file-path start-time end-time position-of-current-file))
              ((and (equal mode "deltaTime") rest-par)
                (make-ghost-fragment (two-part-buffer-substring-no-properties p0 rest-par) ghost-file-path nil nil position-of-current-file))
              (t 
                (make-ghost-fragment (buffer-substring-no-properties p1 pn) ghost-file-path start-time end-time position-of-current-file)))))))


(defun two-part-buffer-substring-no-properties (p0 rest-par)
  (save-excursion
    (goto-char p0)
    (forward-sexp (+ rest-par 1))
    (let ((p1 (point)))
      (concat "(" (buffer-substring-no-properties p0 p1) " ### " ")"))))


(defun rest-parameters-in-signature (sig0)
  "Return if there is rest parameters in signature sig0 (a list or string or symbol). 
If there is rest parameters, the returned value is the number of these."
  (if (symbolp sig0)
      nil
      (let ((sig (cond ((consp sig0) sig0)
                       ((stringp sig0) (car (read-from-string sig0)))
                       (t (error "rest-parameters-in-signature: list or string expected.")))))
        (if (improper-listp sig)
            (- (length (proper-list-part sig)) 1)
          nil))))



; Ghost fragments constructors and selectors.

(defun make-ghost-fragment (fragment ghost-file-path &optional start-time end-time position-of-current-file)
  "Ghost fragment constructor"
  (list 'ghost-fragment fragment ghost-file-path start-time end-time position-of-current-file))

; Ghost fragment selectors
(defun ghost-fragment-of (fragment)  (nth 1 fragment))
(defun ghost-fragment-ghost-file-path-of (fragment)  (nth 2 fragment))    ; The full path to the ghost file, from which this fragment has been extracted.
(defun ghost-fragment-start-time-of (fragment)  (nth 3 fragment))
(defun ghost-fragment-end-time-of (fragment) (nth 4 fragment))
(defun ghost-fragment-position-of-current-file (fragment) (nth 5 fragment))
(defun ghost-fragment-file-name (fragment) (nth 6 fragment))    ; only present when read from file. The name of the fragment file (proper file name, without path).


(defun next-ghost-name-fragment (project-directory-path)
  "Return the next ghost file name (a numeric string) in the project directory with path project-directory-path. A numeric string.
Always one more than the currently highest number."
  (let ((ghost-dir (concat project-directory-path "ghost-fragments/")))
    (cond ((not (file-exists-p ghost-dir)) "1")
          ((file-exists-p ghost-dir)
           (let* ((f-list (list-of-files ghost-dir)) ; all numeric
                  (f-num-lst (mapcar (function as-number) f-list)))
             (if (null f-num-lst)
                 "1"
                 (as-string (+ (apply (function max) f-num-lst) 1))))))))


; --------------------------------------------------------------------------------------------------------------------------------------
; A Ghost Fragment Browser.

(defvar ghost-fragment-browser-name "Ghost-fragment-browser")

(defun ghost-fragments-browser ()
  "Show a ghost fragment browser 'in the other window'."
  (interactive)
  (let* ((buf-name ghost-fragment-browser-name)
         (current-project-name (file-name-proper (file-name-proper (current-file))))
         (p-dir (concat (current-directory) current-project-name "/"))
        )
    (delete-other-windows)
    (split-window-vertically)
;    (other-window 1)
    (if (get-buffer buf-name) (kill-buffer buf-name))
    (let* ((buffer (get-buffer-create buf-name))
           (ghost-fragment-list (get-ghost-fragment-list))
           )
      (set-buffer buffer)
      (erase-buffer)
      (set-window-buffer (selected-window) buffer)
      (ghost-fragment-browse-mode)
      (setq project-dir ; buffer-local
            p-dir)
      
      (use-local-map ghost-fragment-browse-mode-map)
      (present-ghost-fragments ghost-fragment-list)

      ; adjust size of browser:
      (let* ((lines-max (/ (frame-height) 2))
             (actual-number-of-lines (length ghost-fragment-list))
             (extra-lines 4)
             (required-number-of-lines (max (+ extra-lines actual-number-of-lines) 6))
             )
        (if (< required-number-of-lines lines-max)
            (enlarge-window (- required-number-of-lines lines-max)) ; a negative numer, thus shrinks the window size
          ))

      (toggle-read-only 1)              ; always read only
      (goto-char (point-min))
      )))

(define-derived-mode ghost-fragment-browse-mode fundamental-mode "GhostFragmentBrowse"
  "A mode for browsing ghost fragments"
  (make-local-variable 'project-dir)
)

; Key and menu bindings:

(defvar ghost-fragment-browse-mode-map (make-sparse-keymap "GhostFragmentBrowse"))

(define-key ghost-fragment-browse-mode-map  "f" 'find-ghost-fragment)  
(define-key ghost-fragment-browse-mode-map  "o" 'find-ghost-fragment-other-window)  
(define-key ghost-fragment-browse-mode-map  "i" 'insert-ghost-fragment-in-other-window)
(define-key ghost-fragment-browse-mode-map  "I" 'insert-ghost-fragment-around-blank-separated-region)
(define-key ghost-fragment-browse-mode-map  "j" 'insert-ghost-fragment-in-around-by-abstime)
(define-key ghost-fragment-browse-mode-map  "d" 'delete-ghost-fragment)
(define-key ghost-fragment-browse-mode-map  "g" 'goto-ghost-fragment-level)
(define-key ghost-fragment-browse-mode-map  "\C-\M-d" 'delete-all-ghost-fragments)

(define-key ghost-fragment-browse-mode-map [down] 'next-line-and-highlight-line)   
(define-key ghost-fragment-browse-mode-map [up] 'previous-line-and-highlight-line)


(laml-define-key ghost-fragment-browse-mode-map [menu-bar ghost-fragments]
    (cons "Ghost Fragments" (make-sparse-keymap "ghost-fragments")))

(laml-define-key ghost-fragment-browse-mode-map [menu-bar ghost-fragments help-on-ghost-fragments]
      '("Help on ghost fragments" . (lambda () (interactive) (help-from-text-file "ghost-fragments.txt"))))

(laml-define-key ghost-fragment-browse-mode-map [menu-bar ghost-fragments menu-midi-ghost-browser-sep-3]
    '("----"))

(laml-define-key ghost-fragment-browse-mode-map [menu-bar ghost-fragments goto-ghost-fragment-level]
    (cons "Goto ghost fragment level" (function goto-ghost-fragment-level )))

(laml-define-key ghost-fragment-browse-mode-map [menu-bar ghost-fragments menu-midi-ghost-browser-sep-2]
    '("----"))

(laml-define-key ghost-fragment-browse-mode-map [menu-bar ghost-fragments delete-all-ghost-fragments]
    (cons "Delete all ghost fragments" (function delete-all-ghost-fragments )))

(laml-define-key ghost-fragment-browse-mode-map [menu-bar ghost-fragments delete-ghost-fragment]
    (cons "Delete ghost fragment" (function delete-ghost-fragment )))

(laml-define-key ghost-fragment-browse-mode-map [menu-bar ghost-fragments menu-midi-ghost-browser-sep-1]
    '("----"))

(laml-define-key ghost-fragment-browse-mode-map [menu-bar ghost-fragments insert-ghost-fragment-around-blank-separated-region]
    (cons "Insert ghost fragment around BLANK separated region" (function insert-ghost-fragment-around-blank-separated-region )))

(laml-define-key ghost-fragment-browse-mode-map [menu-bar ghost-fragments insert-ghost-fragment-in-around-by-abstime]
    (cons "Insert ghost fragment around absTime separated region" (function insert-ghost-fragment-in-around-by-abstime )))

(laml-define-key ghost-fragment-browse-mode-map [menu-bar ghost-fragments insert-ghost-fragment-in-other-window]
    (cons "Insert ghost fragment in other window" (function insert-ghost-fragment-in-other-window )))

(laml-define-key ghost-fragment-browse-mode-map [menu-bar ghost-fragments menu-midi-ghost-browser-sep-3]
    '("----"))

(laml-define-key ghost-fragment-browse-mode-map [menu-bar ghost-fragments find-ghost-fragment-other-window]
    (cons "Find ghost fragment in other window" (function find-ghost-fragment-other-window )))

(laml-define-key ghost-fragment-browse-mode-map [menu-bar ghost-fragments find-ghost-fragment]
    (cons "Find ghost fragment" (function find-ghost-fragment )))



(define-derived-mode 
        ghost-fragment-scheme-mode scheme-mode "GhostFragment" 
  "A specialize Scheme mode for ghost fragments"
)

(define-key ghost-fragment-scheme-mode-map [f4] 'kill-ghost-fragment-buffer-and-return-to-browser)   ; specialized kill buffer which ensures that an appropriate buffer appears after the killing 
(define-key ghost-fragment-scheme-mode-map "d" 'kill-ghost-fragment-buffer-and-return-to-browser)

(defun kill-ghost-fragment-buffer-and-return-to-browser ()
  (interactive)
  (let ((buf (get-buffer ghost-fragment-browser-name)))
    (kill-buffer nil)  ; The ghost fragment scheme buffer
    (if buf (set-window-buffer (selected-window) buf)))
)


(defun present-ghost-fragments (ghost-fragment-list)
  "Insert contents (header and entires) in the current buffer - a ghost fragment browser buffer."
  (insert (present-ghost-fragments-header-line)) (insert CR) (insert CR)   
  (mapcar 
   (function 
    (lambda (frag)
      (insert (present-single-fragment frag)) (insert CR)))
    ghost-fragment-list)
)

(defvar ghost-fragment-entry-length 100 "The presentation lenght of a ghost entry in the ghost fragment browser")

(defun present-ghost-fragments-header-line()
  "Returns a header line of the ghost fragment browser."
  (concat 
    (pad-string-to-length 4 "FRG#" 'right) " "
    (pad-string-to-length 10 "LEVEL" 'right)
    (pad-string-to-length 9 "START" 'right)
    (pad-string-to-length 9 "END" 'right) 
    "    "
    (pad-string-to-length (ghost-fragment-width)  "FRAGMENT" 'left)
  ))

(defun ghost-fragment-width ()
  "The width of the fragment part of a ghost fragment entry"
  (- (window-width (selected-window)) (+ 4 37)))

(defun present-single-fragment (frag)
  "Returns a single entry line of the ghost fragment browser."
  (let ((frag-text (single-line-string (ghost-fragment-of frag) (ghost-fragment-width)))
        (ghost-file-name (ghost-fragment-ghost-file-path-of frag))
        (start-abs-time (ghost-fragment-start-time-of frag))
        (end-abs-time (ghost-fragment-end-time-of frag))
        (pos-of-cur-file (ghost-fragment-position-of-current-file frag))
        (file-name (ghost-fragment-file-name frag)))
    (concat
     (pad-string-to-length 4 file-name 'right) " "
     (pad-string-to-length 10 (as-string (real-level-number-of (file-name-proper (file-name-nondirectory ghost-file-name)) pos-of-cur-file))  'right)
     (pad-string-to-length 9 (force-as-numeric-string start-abs-time) 'right)
     (pad-string-to-length 9 (force-as-numeric-string end-abs-time) 'right)
      "    "
      frag-text)))

(defun real-level-number-of (proper-file-name pos-of-cur-file)
  "The real number can be 1 (the bottom file name) or a greater integer. Returns an integer."
  (cond ((numeric-string proper-file-name) (+ (as-number proper-file-name) 1))
        ((equal "-ghost" (string-suffix proper-file-name 6)) (+ 2 (as-number (substring proper-file-name 0 (- (length proper-file-name) 6)))))
        (t (+ 1 pos-of-cur-file))))


(defun force-as-numeric-string (x)
 "Return x as string - under particular rules prescribed by the ghost fragment browser."
 (cond ((stringp x) x)
       ((numberp x) (as-string x))
       ((null x) "-")
       (t "???")))

(defun single-line-string (str final-length)
  "Return the prefix of str, of length final-length. Translate new line chars (CR) to space.
If str is longer than final-length, the suffix of the result becomes '...'"
  (let ((res (make-string final-length 32))  ; string with spaces
        (str-lgt (length str))
        (i 0)
        (new-line-chars (list 10 13)))
    (if (> str-lgt final-length)
        (progn
          (while (and (< i str-lgt) (< i (- final-length 3)))
            (if (memq (aref str i) new-line-chars)
                (aset res i 32)
                (aset res i (aref str i)))
            (setq i (+ i 1)))
          (aset res i ?.) (aset res (+ i 1) ?.) (aset res (+ i 2) ?.))
        (while (and (< i str-lgt) (< i final-length))
          (if (memq (aref str i) new-line-chars)
              (aset res i 32)
              (aset res i (aref str i)))
          (setq i (+ i 1))))
    res))


(defun get-ghost-fragment-list ()
  "Get and return the list of ghost fragments from the (implicitly selected) current project.
The (numeric) file name of each ghost fragment is appended to the entry."
  (let* ((current-project-name (file-name-proper (file-name-proper (current-file))))
         (project-dir (concat (current-directory) current-project-name "/"))
        )
    (get-ghost-fragment-list-of-project-dir project-dir)))

(defun get-ghost-fragment-list-of-project-dir (project-dir)
 "Get and return the list of ghost fragments from the given project-dir."
 (let ((ghost-fragment-dir (concat project-dir "ghost-fragments/")))
    (if (file-exists-p ghost-fragment-dir)
        (let ((files (sort (list-of-files ghost-fragment-dir) (lambda (f1 f2) (<= (as-number f1) (as-number f2))))))
          (mapcar (function (lambda (f)
                              (let ((read-entry (file-read (concat ghost-fragment-dir f))))
                                (if (not (= 6 (length read-entry)))
                                    (error "You are reading outdated ghost fragments!!!"))
                                (append read-entry (list f))))) files))
        nil)))
     

(defun find-ghost-fragment ()
  "From a ghost fragments browser, find the underlying fragment and show it in a buffer in the current window"
  (interactive)
  (let* ((file-name (get-current-ghost-file-entry-name))
         (buf (get-buffer-create (concat "ghost-fragment-" file-name)))
         (file-path (concat project-dir "ghost-fragments/" file-name))
         (ghost-fragment-entry (file-read file-path)))
    (set-buffer buf)
    (set-window-buffer (selected-window) buf)
    (erase-buffer)
    (insert (ghost-fragment-of ghost-fragment-entry))  ; project-dir is local to the browser buffer
    (ghost-fragment-scheme-mode)
    (set-buffer-modified-p nil)
    (toggle-read-only 1)
    (goto-char (point-min))))

(defun find-ghost-fragment-other-window ()
  "From a ghost fragments browser, find the underlying fragment and show it in a buffer in the other window"
  (interactive)
  (let* ((file-name (get-current-ghost-file-entry-name))
         (buf (get-buffer-create (concat "ghost-fragment-" file-name)))
         (file-path (concat project-dir "ghost-fragments/" file-name))
         (ghost-fragment-entry (file-read file-path)))
    (set-buffer buf)
    (set-window-buffer (other-window 1) buf)
    (erase-buffer)
    (insert (ghost-fragment-of ghost-fragment-entry))  ; project-dir is local to the browser buffer
    (scheme-mode)
    (set-buffer-modified-p nil)
    (goto-char (point-min))))

(defun insert-ghost-fragment-in-other-window ()
  "Insert the currently selected ghost fragment in the other window. Simple insert, no nesting involved.
As a precondtion, the cursor point must be within the relevant MidiTrack."
 (interactive)
  (let* ((file-name (get-current-ghost-file-entry-name))
         (ghost-fragment-entry (file-read (concat project-dir "ghost-fragments/" file-name)))
         (fragment (ghost-fragment-of ghost-fragment-entry))
         (split-fragment (split-string fragment "###"))
        )
    (other-window 1)
    (if (= (length split-fragment) 2)            ; nest the section from start-time to end-time into the fragment
        (message "This fragment must be inserted around some MIDI messages. Nothing done")
        (insert fragment)  ; project-dir is local to the browser buffer
    )
    (other-window 1)))

(defun insert-ghost-fragment-in-around-by-abstime ()
  "Insert the currently selected ghost fragment in the other window, around a section of MIDI message distinguished by absTime.
As a precondtion, the cursor point must be within the relevant MidiTrack."
 (interactive)
  (let* ((file-name (get-current-ghost-file-entry-name))
         (ghost-fragment-entry (file-read (concat project-dir "ghost-fragments/" file-name)))
         (fragment (ghost-fragment-of ghost-fragment-entry))
         (split-fragment (split-string fragment "###"))
         (start-time (as-number (ghost-fragment-start-time-of ghost-fragment-entry)))
         (end-time (as-number (ghost-fragment-end-time-of ghost-fragment-entry)))
        )
    (other-window 1)
    (if (and start-time end-time (= (length split-fragment) 2))            ; nest the section from start-time to end-time into the fragment
        (let ((sp (find-start-point-position-abs-time start-time))
              (ep (find-end-point-position-abs-time end-time))
              (pre-fragment (car split-fragment))
              (post-fragment (cadr split-fragment))
             )
          (if (and sp ep)
              (progn
                (goto-char ep) (insert post-fragment) ; first end point, in order not to disturb start point position
                (goto-char sp) (insert (concat "  " pre-fragment)) (insert CR))
              (message "Cannot"))
        )
        (cond ((and start-time end-time (not (= (length split-fragment) 2)))
                   (message "This fragment must be inserted as a single expression, and not nested around messages. Nothing done."))
              ((not (and start-time end-time))
                   (message "This fragment has no absolute time markers. Insert it around a blank separated region. Nothing done."))
              (t "Cannot insert this fragment around a section of MIDI messages. Nothing done.")))
    )
    (other-window 1))

(defun insert-ghost-fragment-around-blank-separated-region ()
  "Insert the currently selected ghost fragment (which must be a consist of two parts separated by ###) in the other window around the current blank separated region"
 (interactive)
  (let* ((file-name (get-current-ghost-file-entry-name))
         (ghost-fragment-entry (file-read (concat project-dir "ghost-fragments/" file-name)))
         (fragment (ghost-fragment-of ghost-fragment-entry))
         (split-fragment (split-string fragment "###"))
        )
    (other-window 1)
    (let ((sp (pos-of-previous-blank-line-before-point))
          (ep (pos-of-next-blank-line-after-point)))
     (if (and sp ep (= (length split-fragment) 2))
         (let ((pre-fragment (car split-fragment))
               (post-fragment (cadr split-fragment)))
           (goto-char ep) (insert post-fragment) ; first end point, in order not to disturb start point position
           (goto-char sp) (insert (concat "  " pre-fragment)) (insert CR))
        (message "Cannot. Nothing done.")))
    (other-window 1)))

(defun goto-ghost-fragment-level ()
  "From a ghost fragment browser, attempt to go the midl source file at level. This only make sense if the stack has not been changed (too much) since the defintion of the fragments"
  (interactive)
  (let* ((file-name (get-current-ghost-file-entry-name))
         (ghost-fragment-entry (file-read (concat project-dir "ghost-fragments/" file-name)))
         (file-path (ghost-fragment-ghost-file-path-of ghost-fragment-entry))
         (proper-file-name (file-name-proper (file-name-nondirectory file-path)))
         (pos-of-cur-file (ghost-fragment-position-of-current-file ghost-fragment-entry))           
         (level (real-level-number-of proper-file-name pos-of-cur-file))
         (count 1)
        )
    (other-window 1)

    (do-all-pops-of-midl-stack)
    (while (< count level)
      (undo-pop-midl-stack)
      (setq count (+ count 1)))

    (other-window 1)
  )
)

(defun find-start-point-position-abs-time (time)
  "Return the cursor position just before the event with absTime time. The point must be within the relevant track.
Return nil if impossible. Does first attempt an exact match, next a binary closest search."
  (save-excursion
    (beginning-of-line 1)
    (while (not (looking-at "(MidiTrack")) (backward-up-list 1))  ; now just before track
    (forward-sexp 1)
    (let ((pn (point)))   ; position at end of track
       (backward-sexp 1) (down-list 2)
       (let ((pi (point))   ; inside track
             (reg-exp (regular-expression (abs-time-attr-reg-exp time))))
         (if (re-search-forward reg-exp pn t)
             (progn
               (beginning-of-line 1)
               (+ (point) 1))
             (progn
               (goto-char pi) ; now inside track 
               (let ((bsr (binary-search-for-abs-time time)))
                 (if bsr 
                     (progn (goto-char bsr) (beginning-of-line 1) (point))
                   nil))))))))

(defun find-end-point-position-abs-time (time)
  "Return the cursor position just after the event with absTime time. The point must be within the 
currently selecgted track. Return nil if impossible. Does first attempt an exact match, next a binary closest search."
  (save-excursion
    (beginning-of-line 1)
    (while (not (looking-at "(MidiTrack")) (backward-up-list 1))
    (down-list 2)
    (let ((p0 (point)))  ; inside track
      (backward-up-list 2)
      (forward-sexp 1) ; now just after track

      (let ((reg-exp (regular-expression (abs-time-attr-reg-exp time))))
        (if (re-search-backward reg-exp p0 t)
            (progn
              (end-of-line 1)
              (point))
            (progn
               (goto-char p0)  ; now inside track
               (let ((bsr (binary-search-for-abs-time time)))
                 (if bsr 
                     (progn (goto-char bsr) (end-of-line 1) (point))
                   nil))))))))

(defun binary-search-for-abs-time (time)
  "Return the cursor position near a message with absTime time (the parameter of this function).
As a precondition, the point must be within the relevant track. And the mode of file must be absTime.
Return nil if impossible. This funcion assumes a normalized, line-oriented organization of the midl source file."
  (save-excursion
     (if (is-inside-track-of-standard-midi-file)
         (let ((start-line nil)
               (end-line nil))

             ; find line number of last line with absTime attribute:
             (while (not (looking-at "(MidiTrack")) (backward-up-list 1))
             (forward-sexp 1) 
             (re-search-backward (regular-expression (abs-time-attr-reg-exp)))
             (beginning-of-line 1)
             (setq end-line (line-number-at-pos (point)))

             ; find line number of first line with absTime attribute:
             (while (not (looking-at "(MidiTrack")) (backward-up-list 1))
             (re-search-forward (regular-expression (abs-time-attr-reg-exp)))
             (beginning-of-line 1)
             (setq start-line (line-number-at-pos (point)))

             (binary-search-for-abs-time-1 time start-line end-line))
         nil)
  ))

(defun binary-search-for-abs-time-1 (time start-line end-line)
  (if (= start-line end-line)
      (progn (goto-line start-line) (forward-through-white-space-and-comments) (point))
      (let ((mid-line (+ start-line (/ (- end-line start-line) 2))))
        (goto-line mid-line) (forward-through-white-space-and-comments)
        (let ((mid-time (attribute-of-current-form 'absTime 'number)))
          (cond ((= time mid-time) 
                  (forward-through-white-space-and-comments)
                  (point))
                ((= 1 (abs (- end-line start-line)))   ; end-line and start-line next to each other. Find the best:
                  (let ((diff-start-line nil)
                        (diff-end-line nil))
                    (goto-line start-line) (forward-through-white-space-and-comments) 
                    (setq diff-start-line (abs (- time (attribute-of-current-form 'absTime 'number))))
                    (goto-line end-line) (forward-through-white-space-and-comments)
                    (setq diff-end-line (abs (- time (attribute-of-current-form 'absTime 'number))))
                    (if (<= diff-start-line diff-end-line)
                        (progn (goto-line start-line) 
                               (forward-through-white-space-and-comments)
                               (point))
                        (progn (goto-line end-line) 
                               (forward-through-white-space-and-comments)
                               (point)))))
                ((< time mid-time) (binary-search-for-abs-time-1 time start-line mid-line))
                (t (binary-search-for-abs-time-1 time mid-line end-line)))))))

(defun delete-ghost-fragment ()
  "Delete the currently selected ghost fragment from within a ghost fragment browser."
  (interactive)
  (let* ((file-name (get-current-ghost-file-entry-name))
         (file-path (concat project-dir "ghost-fragments/" file-name))
         (p0 (point))
        )
    (if (file-exists-p file-path)
        (progn
          (delete-file file-path)
          (toggle-read-only)
          (erase-buffer)
          (let ((ghost-fragment-list (get-ghost-fragment-list-of-project-dir project-dir)))   ; project-dir is buffer-local in browser
            (present-ghost-fragments ghost-fragment-list))
          (goto-char p0)
          (toggle-read-only 1))
        (message "Ghost fragment does not exist.  Nothing done"))))

(defun delete-all-ghost-fragments ()
  "Delete all ghost fragments from within a ghost fragment browser."
  (interactive)
  (let* ((dir-path (concat project-dir "ghost-fragments/"))
         (ghost-fragment-files (list-of-files dir-path)))

    ; Delete files
    (mapcar 
     (function
       (lambda (f) (delete-file (concat dir-path f))))
     ghost-fragment-files)

    ; Update browser buffer:
    (toggle-read-only)
    (erase-buffer)
    (let ((ghost-fragment-list (get-ghost-fragment-list-of-project-dir project-dir))) ; project-dir is buffer-local in browser
      (present-ghost-fragments ghost-fragment-list))
    (goto-char (point-min))
    (toggle-read-only 1)))

(defun get-current-ghost-file-entry-name ()
 (save-excursion
   (beginning-of-line 1)
   (while (looking-at-chars (point) white-space-char-list) (forward-char 1))
   (name-under-point)))






; ---------------------------------------------------------------------------------------------------------------
; ABC support, including ps2pdf support

(defvar abc-title "Sample notes")
(defvar abc-time-signature "4/4")
(defvar abc-key-signature "C")
(defvar abc-tempo "100")
(defvar abc-default-note-length "1/4")
(defvar abc-midi-channel 3)

(defun make-abc-string (note-string)
  (format "X:1\nT:%s\nM:%s\nL:%s\nQ:%s\nK:%s\n%s\n%s"
    abc-title abc-time-signature abc-default-note-length abc-tempo abc-key-signature
    (concat "%%MIDI channel " (as-string abc-midi-channel))
    note-string))

; Always deltaTime - probably.
; Always PPQN 480? 
(defun abc (note-string)
  "Read a note string in ABC notation, parse it to midl, and insert the notes at the location of the cursor point
as deltaTimed notes."
  (interactive "sABC Notes: ")
  (let ((temp-abc-file-path (concat laml-temp-file-path "notes.abc"))
        (temp-midi-file-path (concat laml-temp-file-path "notes.midi"))
        (temp-midl-file-path (concat laml-temp-file-path "notes.midl"))
        (abc-string (make-abc-string note-string))
        (this-buffer (current-buffer))
        (out-buf (get-buffer-create "*abc-out*")) 
       )
    (if (file-exists-p temp-abc-file-path) (delete-file temp-abc-file-path))
    (if (file-exists-p temp-midl-file-path) (delete-file temp-midl-file-path))
    (if (file-exists-p temp-midl-file-path) (delete-file temp-midl-file-path))
    (file-write-string abc-string temp-abc-file-path)
    (call-process (abc2midi-exc) nil out-buf nil temp-abc-file-path  "-o" temp-midi-file-path)
    (sync-midi-file-to-laml-file "deltaTime" "0" nil temp-midi-file-path temp-midl-file-path)
    (let ((note-on-section-of-abc (get-note-on-section temp-midl-file-path)))
      (set-buffer this-buffer)
      (insert CR)
      (insert note-on-section-of-abc)
      (insert CR))))


(defun get-note-on-section (midl-path)
 (save-window-excursion
  (let ((buf (get-buffer-create "temp-midl"))
        (p0 nil)
        (pn nil)
        (res nil))
    (set-buffer buf)
    (insert-file-contents midl-path)

    (goto-char (point-min))
    (search-forward "(NoteOn " nil t)
    (beginning-of-line 1) 
    (setq p0 (point))

    (goto-char (point-max))
    (search-backward "(NoteOn " nil t)
    (end-of-line 1) 
    (setq pn (point))
 
    (setq res (buffer-substring-no-properties p0 pn))
    (kill-buffer buf)
    res)))


; ---------------------------------------------------------------------------------------------------------------

;;; Chord-wise navigation.

(defvar max-chord-length 40 "The maximum number of ticks in a 'chord window'. A reasonable value depends the PPQN")

(defvar preferred-chord-map-file-path (concat midi-software-dir "data/" "basic-chord-list.lsp") "The full path to the chord list on which the chord recognition is based.")

(defun process-current-chord-and-play ()
  "Assume that the cursor point is located at the beginning of chord sequence (at the start parenthesis of the first note). 
Process and play it. Works in both absTime mode and deltaTime mode."
  (interactive)
  (let* ((mode (find-processing-mode-of-current-buffer))
         (start-p (point))
         (end-p (find-end-of-chord-sequence mode max-chord-length))  ; after last message in chord sequence
        )
    (setq selection-begin start-p) (setq selection-end end-p) 
    (analyze-and-display-chord start-p end-p channels-to-play)
    (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p end-p 'play) 
    (highlight-this-region start-p end-p)
  )
)


(defun goto-next-chord-process-and-play ()
  "Assume that the cursor point is located at the beginning of chord sequence.
Skip these chords, goto the next chord sequence (or single note) that starts with a note in a relevant channel, process and play."
  (interactive)
  (set-mark nil)
  (let* ((mode (find-processing-mode-of-current-buffer))
         (end-p (find-end-of-chord-sequence mode max-chord-length))  ; after last message in chord sequence
        )

   ; Skip over current chord:
   (goto-char end-p) (forward-through-white-space-and-comments)  

   ; forwarding: Takes into consideration if point already is on a channel-relevant NoteOn message:
   (if (eq channels-to-play t) 
       (forward-midi 0)
       (forward-midi 0 channels-to-play))

   ; Process and play:
   (let* ((start-p (point))
          (end-p (find-end-of-chord-sequence mode max-chord-length)) ; after last message in chord sequence
         )
     (setq selection-begin start-p) (setq selection-end end-p) 
     (analyze-and-display-chord start-p end-p channels-to-play) 
     (if (member play-after-condition '(after-navigate after-navigate-and-edit))
         (process-laml-file-and-present-from-to channels-to-play (relative-percentage-playing-tempo) start-p end-p 'play))
     (highlight-this-region start-p end-p)
   )   
  )
)

(defun goto-previous-chord-process-and-play ()
  (interactive)
  (message "Cannot go back - chord by chord")
)



(defun find-end-of-chord-sequence (mode max-chord-length)
  "Relative to the current cursor point, and max-chord-length (in ticks), find the end cursor position of the last note in the current chord sequence.
Returns a position justs after the end of the last midi message of the chord sequence.
Works in both absTime mode and deltaTime mode."
  (save-excursion
    (cond ((equal "deltaTime" mode)
           (let ((delta-time-counter 0))
             (while (<= delta-time-counter max-chord-length)
               (forward-midi-message 1)
               (setq delta-time-counter (+ delta-time-counter (attribute-of-current-form 'deltaTime 'number))))
             (backward-midi-message 1)
             (forward-sexp 1)
             (point)))

          ((equal "absTime" mode)
           (let* ((start-abs-time (attribute-of-current-form 'absTime 'number))
                  (delta-time-counter 0))
             (while (<= delta-time-counter max-chord-length)
               (forward-midi-message 1)
               (setq delta-time-counter (- (attribute-of-current-form 'absTime 'number) start-abs-time)))
             (backward-midi-message 1)
             (forward-sexp 1)
             (point)))

          (nil  ; old absTime
           (let* ((cur-abs-time (attribute-of-current-form 'absTime 'number))
                  (end-pos (binary-search-for-abs-time (+ cur-abs-time max-chord-length))))
             (goto-char end-pos)
             (forward-sexp 1)
             (point)))
                  
          (t (error "find-end-of-chord-sequence: Invalid mode: %s" mode)))
  ))

(defvar chord-map nil "A comprehensive mapping of chord names to a list of notes in the chords.
  The notes in the chord are ordered, and all in the interval 0..11. The list is read when needed the first time,
and cached for subsequent use.")

; Chord map selectors
(defun chord-name-of (chord-entry) (car chord-entry))  
(defun chord-notes-of (chord-entry) (cdr chord-entry))  

(defun do-ensure-chord-map()
  "Do cache the chord info list"
  (if chord-map 
      'do-nothing
      (setq chord-map (file-read preferred-chord-map-file-path))))


(defun analyze-and-display-chord (start-p end-p channels-to-play)
  (let* ((chord-notes (find-note-values start-p end-p channels-to-play)) ; all note values in the chord interval
         (normalized-chord-notes (normalize-chord-list chord-notes))     ; normalized to the interval 0..11, duplicates removed, sorted
        )
    (do-ensure-chord-map)
    (let ((matching-chord (find-in-list
                             (function
                              (lambda (chord-entry) 
                                (equal normalized-chord-notes (chord-notes-of chord-entry))))
                           chord-map)))
      (message (chord-analysis-message chord-notes matching-chord))))) 

(defun chord-analysis-message (note-list matching-chord)
  (let* ((note-list-copy (copy-sequence note-list))  ; sort mutates it's first argument. Therefore first make a copy.
         (unique-note-list-copy (remove-duplicates note-list-copy))
         (note-name-list (mapcar (function (lambda (note-value) (note-number-to-note-name note-value 0)))
                                 (sort unique-note-list-copy   
                                       (function <=))))
         (note-name-list-string (list-to-string-with-sep note-name-list ", ")))
    (cond (matching-chord
            (format "The notes %s which form the chord  %s" note-name-list-string (chord-name-of matching-chord)))
          ((= 1 (length note-list))
            (format "A single note: %s" note-name-list-string))
          (t 
            (format "The notes %s" note-name-list-string)))))


(defun normalize-chord-list (chord-notes)
  (sort (remove-duplicates (mapcar (function (lambda (note-val) (mod note-val 12))) chord-notes))
        (function (lambda (n m) (<= n m)))))

(defun find-note-values (start-p end-p channels-to-play)
  "Return the list of note values (a list of integers) found in the interval start-p to end-p in channels-to-play.
end-p denotes a position just after a midi message. 
start-p is assumed to be a position just in front of a midi message. Works in both absTime and deltaTime mode.
This function takes channels-to-play (the last parameter) into consideration - it follow the convetions of the global variable of the same name."
 (save-excursion
   (let ((res nil)
        )
     (goto-char start-p)
     (while (< (point) end-p)
       (if (and (equal (current-form-name) "NoteOn")                                             ; A NoteOn midi message
                (play-channel-p (attribute-of-current-form 'channel 'number) channels-to-play)   ; ... belonging to the appropriate channels
           )
           (let ((note-value (attribute-of-current-form 'note 'number)))
             (setq res (cons note-value res)))) 
       (forward-midi-message 1) 
     )
     (reverse res))))

(defun play-channel-p (ch channels-to-play)
  "Is ch a playing channels according to the parameter channels-to-play.
channels-to-play follow the convetions of the global variable of the same name."
  (cond ((and (booleanp channels-to-play) (eq channels-to-play t))
            t)
        ((and (listp channels-to-play) (member ch channels-to-play))
            t)
        ((and (listp channels-to-play) (not (member ch channels-to-play)))
            nil)
        (t (error "play-channel-p: Illegal value of second parameter channels-to-play."))))


(defun note-stat (unique-p)
  "In the selected region, displays a list of all notes in the appropriate channels."
  (interactive (list
                  (completing-read "Statistics on unique notes only (yes/no)? "
                                   '(("yes" . t) ("no" . nil)) nil t)))
  (let* ((start-p (min (point) (mark)))
         (end-p (max (point) (mark)))
         (chord-notes (find-note-values start-p end-p channels-to-play)) ; all note values in the given interval
         (sorted-chord-notes (sort (copy-sequence chord-notes) (function <=)))
         (sorted-chord-notes-1 (if (equal unique-p "yes") (remove-duplicates sorted-chord-notes) sorted-chord-notes))
         (note-name-list (mapcar (function (lambda (note-value) (note-number-to-note-name note-value 0)))
                                 sorted-chord-notes-1))
         (note-name-list-string (list-to-string-with-sep note-name-list ", "))
        )
    (message note-name-list-string)))
  
; ---------------------------------------------------------------------------------------------------------------
; ABC postscript/pdf illustration:

(defun process-laml-file-and-illustrate-via-abc ()
  "Illustrate the current midl file in a postscript viewer via conversion to the abc format"
  (interactive)
  (let* ((laml-file-name (current-file))
         (proper-laml-file-name (file-name-proper laml-file-name))   ; with possible double name (icon name: x.icon)
         (midi-file-name (concat proper-laml-file-name ".mid"))
         (midi-file-path (concat (current-directory) "midi-temp/" midi-file-name))
        )
    (ensure-directory-existence (current-directory) "midi-temp/")
    (copy-file path-to-midi-error-file midi-file-path t) 
    (laml-sync-process-current-buffer)  
    (message "A MIDI file has been generated in midi-temp. Now illustrating...")
    (cond ((file-exists-p midi-file-path)   ; with possible double name
            (illustrate-midi-file-via-abc midi-file-path proper-laml-file-name))
          (t (error "Process-laml-file-and-play: Should not happen:")))))

(defun illustrate-midi-file-via-abc (midi-file-path title)
  "Illustrate midi file in midi-file-path in a postscript viewer via the abc format. Works only/best for small and simple midi files."
  (interactive "fIllustrate MIDI file: ")
  (let ((temp-abc-file-path (concat laml-temp-file-path "notes.abc"))
        (temp-ps-file-path (concat laml-temp-file-path "notes.ps"))
        (temp-pdf-file-path (concat laml-temp-file-path "notes.pdf"))
        (this-buffer (current-buffer))
        (out-buf (get-buffer-create "*abc-out*")) 
       )
    (set-buffer out-buf) (erase-buffer) (set-buffer this-buffer)
    (if (file-exists-p temp-abc-file-path) (delete-file temp-abc-file-path))
    (if (file-exists-p temp-ps-file-path) (delete-file temp-ps-file-path))
    (if (file-exists-p temp-pdf-file-path) (delete-file temp-pdf-file-path))

    (call-process (midi2abc-exc) nil out-buf nil midi-file-path  "-o" temp-abc-file-path "-title" title "-nogr")
    (call-process (abcm2ps-exc) nil out-buf nil temp-abc-file-path  "-c" "-O" temp-ps-file-path)
    (start-process (gsview-exc) out-buf (gsview-exc) temp-ps-file-path)

    (delete-other-windows)
    (split-window-vertically)
    (set-window-buffer (other-window 1) out-buf)
  )
)

; -------------------------------------------------------------------------------------------------------------------------------
; Handling of Motif XS jump places

; (defun xs-dir-jump-contribution (dir-name)
;  (let ((prefix "//Motif_xs-25cb60/media/"))
;   (let ((dir (cond ((file-directory-p (concat prefix "usb1-1" "/" dir-name "/")) (concat prefix "usb1-1" "/" dir-name "/"))
;                    ((file-directory-p (concat prefix "usb2-1" "/" dir-name "/")) (concat prefix "usb2-1" "/" dir-name "/"))
;                    (t (error "Cannot locate XS %s dir" dir-name)))))
;     dir)))

(defun xs-dir-jump-contribution (dir-name)
 (let ((prefix (concat "//" motif-host-name "/media/")))
  (let ((dir (cond ((file-directory-p (concat prefix "usb1-1" "/" dir-name "/")) (concat prefix "usb1-1" "/" dir-name "/"))
                   ((file-directory-p (concat prefix "usb2-1" "/" dir-name "/")) (concat prefix "usb2-1" "/" dir-name "/"))
                   (t (error "Cannot locate XS %s dir" dir-name)))))
    dir)))


(defun add-xs-jumping ()
  "Refresh the xsmidi, xscollections, and xsvoices jump places."
  (interactive)
  (if (boundp 'jump-places)
      (let ((contribution (list (cons "xsmidi" (xs-dir-jump-contribution "Midi"))
				(cons "xscollections" (xs-dir-jump-contribution "COLLECTIONS"))
				(cons "xsvoices" (parent-directory (xs-dir-jump-contribution "Primevoices")))
				(cons "xsrecordings" (xs-dir-jump-contribution "Recordings")))))

	; First get rid of possible old contributions:
	(setq jump-places
	      (filter (function (lambda (e) (not (member (car e) (list "xscollections" "xsvoices" "xsmidi" "xsrecordings"))))) jump-places))
        ; Next add new contributions:
	(setq jump-places
	      (append jump-places contribution))
	(message "DONE")
      )
      (error "add-xs-jumping: Not available in this version of MIDI LAML.")))

(defun xs ()
  "The the midi input and output port to Motix XS, and add Motif XS jump places"
  (interactive)
  (if (boundp 'jump-places)
      (progn
         (set-instrument "motif-xs")
         (add-xs-jumping)
         (setq midi-path-on-motif-xs (xs-dir-jump-contribution "Midi"))
      )
      (error "Instrument specific convenience function: Not available in this version of MIDI LAML.")))


; -------------------------------------------------------------------------------------------------------------------------------

(defvar use-live-cursor-movement t "A boolean that controls if live cursor movements are used. If false (nil) playing is not accompanied by a moving cursor")

(defvar now-live-moving-cursor nil "A boolean which is true if the cursor is moving live")

(defvar is-command-line-player-startup-latency-estimated nil
    "A boolean that tells if startup-latency-of-command-line-midi-player has been called, and if the variable 
command-line-player-startup-latency has been assigned to the result of the call to this function.")

(defvar command-line-player-startup-latency 0.0
   "The waiting time in seconds for startup of the command line midi player on a given machine. The function
startup-latency-of-command-line-midi-player determines this time interval empirically.")

(defun live-cursor-set-startup-latency (new-command-line-player-startup-latency)
  (interactive (list
                 (as-number
                   (read-from-minibuffer
                      (concat "Existing value: " (as-string command-line-player-startup-latency)
                              ". New value: ")))))
  (setq live-cursor-setup-stack (cons (list command-line-player-startup-latency command-line-player-speedup) live-cursor-setup-stack))
  (setq command-line-player-startup-latency new-command-line-player-startup-latency)
  (message "Done. New startup latency: %f. Old value is stacked." command-line-player-startup-latency))

(defun live-cursor-auto-set-startup-latency ()
  (interactive)
  (live-cursor-set-startup-latency (startup-latency-of-command-line-midi-player)))

(defvar live-cursor-setup-stack nil "A stack of pairs - each entry is values for command-line-player-startup-latency and command-line-player-speedup")

(defun live-cursor-manual-setup (new-command-line-player-startup-latency new-command-line-player-speedup)
  (interactive (list
                 (as-number
                   (read-from-minibuffer
                      (concat "Existing value of startup latency: " (as-string command-line-player-startup-latency)
                              ". New value: ")))
                 (as-number
                   (read-from-minibuffer
                      (concat "Existing value of speedup factor (smaller is faster) : " (as-string command-line-player-speedup)
                              ". New value: ")))))

  (setq live-cursor-setup-stack (cons (list command-line-player-startup-latency command-line-player-speedup) live-cursor-setup-stack))

  (setq command-line-player-startup-latency new-command-line-player-startup-latency)
  (setq command-line-player-speedup new-command-line-player-speedup)

  (message "DONE. Number of recovery stack entries %d. Latency is now %f. Speedup factor is now %f. Use M-x live-cursor-recover-settings to use old settings."
           (length live-cursor-setup-stack) new-command-line-player-startup-latency new-command-line-player-speedup))

(defun live-cursor-recover-settings ()
  (interactive)
  (if (> (length live-cursor-setup-stack) 0)
      (let* ((top-entry (car live-cursor-setup-stack))
             (latency (car top-entry))
             (speedup-factor  (cadr top-entry)))
        (setq command-line-player-startup-latency latency)
        (setq command-line-player-speedup speedup-factor)
        (setq live-cursor-setup-stack (cdr live-cursor-setup-stack))
        (message "Remaining stack entries %d. Latency is now %f. Speedup factor is now %f. Use M-x live-cursor-manual-setup to enter new settings."
                 (length live-cursor-setup-stack) command-line-player-startup-latency command-line-player-speedup))
      (message "Stack is empty. NOTHING DONE.")))

(defun live-cursor-info ()
  (interactive)
  (message "Stacked entries %d. Latency is now %f. Speedup factor is now %f. Use M-x live-cursor-manual-setup to enter new settings, and M-x live-cursor-recover-settings to recover."
           (length live-cursor-setup-stack) command-line-player-startup-latency command-line-player-speedup)

)



(defvar command-line-player-speedup 1.0
    "A factor muliplied on the waiting time before each played NotOn message. A number less than 1.0 makes the cursor movement faster.
A value greater than 1.0 makes the cursor movement slower.
On an old PC, it may be attractive to assign value smaller than 1.0, such as 0.9, to this variable in in your .emacs file.")

(defvar avoid-last-line-scrolling nil "A boolean variable which will avoid last line scrolling, but also prevent live cursor movement to last message.")

(defun faster-live-cursor ()
  (interactive)
  (setq command-line-player-speedup (- command-line-player-speedup 0.02))
  (message "Live cursor made faster. New value of command-line-player-speedup factor: %f" command-line-player-speedup))

(defun slower-live-cursor ()
  (interactive)
  (setq command-line-player-speedup (+ command-line-player-speedup 0.02))
  (message "Live cursor made slower. New value of command-line-player-speedup factor: %f" command-line-player-speedup))



(defun toggle-use-live-cursor-movement ()
  (interactive)
  (setq use-live-cursor-movement (not use-live-cursor-movement))
  (show-midl-mode-status-string)
  (if use-live-cursor-movement
      (message "Cursor is now moved while playing.")
      (message "NO cursor moving while playing.")))

(defvar live-cursor-parameters nil)

(defun live-cursor-noteon-move (pause-times in-buf p0 pn play-channels ppqn tempo-mikro-sek-per-qn)
  "Move the cursor over NoteOn messages in the region from p0 to pn, in the given tempo.
Used from sectional playing in parallel to asynchronous MIDI command line playing."
  (setq now-live-moving-cursor t)
  (setq live-cursor-parameters (list pause-times in-buf p0 pn play-channels ppqn tempo-mikro-sek-per-qn))
  (unwind-protect
   (let* ((p-start (point))
          (cur-abs-time nil)
          (next-abs-time nil)
          (time-mikro-sek-per-tick (/ (float tempo-mikro-sek-per-qn) ppqn))
          (done nil)
          (cur-pause-times pause-times)
          (first-pause-time (if (null cur-pause-times) 1 (car cur-pause-times)))
          (latest-highlight-point nil)
         )  
     (sit-for command-line-player-startup-latency)
     (switch-to-buffer in-buf)     
     (goto-char p0) 
     (forward-midi 0 play-channels)
     (highlight-this-line)
     (setq latest-highlight-point (point))
     (sit-for (/ (* first-pause-time time-mikro-sek-per-tick command-line-player-speedup) (float 1000000)))
     (if (null cur-pause-times) (setq done t) (setq cur-pause-times (cdr cur-pause-times)))
     (while (and (< (point) pn) (not done)) 
       (forward-midi 1 play-channels)
       (if (and (< (point) pn)) 
           (let ((pause-tm (if (null cur-pause-times) 1 (car cur-pause-times))))
             (highlight-this-line)
             (setq latest-highlight-point (point))
             (sit-for (/ (* pause-tm time-mikro-sek-per-tick command-line-player-speedup) (float 1000000)))
             (setq cur-pause-times (cdr cur-pause-times))
             (if avoid-last-line-scrolling (setq done (null (cdr cur-pause-times))))   ; only if playing entire window - to avoid last line disturbance.
           )
       )
     )
     (goto-char latest-highlight-point) (highlight-this-line)
   )
   (progn 
     (setq now-live-moving-cursor nil)
     (setq avoid-last-line-scrolling nil))))

(defun live-noteon-pause-list-abs-time (in-buf p0 pn play-channels)
 (save-excursion
  (let ((cur-abs-time nil)
        (next-abs-time nil)
        (done nil)
        (result nil)
       )
    (set-buffer in-buf)
    (goto-char p0)
    (forward-midi 0 play-channels)
    (setq cur-abs-time (attribute-of-current-form 'absTime 'number))
    (setq next-abs-time (save-excursion (forward-midi 1 play-channels) (attribute-of-current-form 'absTime 'number)))
    (setq result (cons (- next-abs-time cur-abs-time) result))
    (while (and (< (point) pn) (not done)) 
      (forward-midi 1 play-channels)
      (setq cur-abs-time (attribute-of-current-form 'absTime 'number))
      (setq next-abs-time 
            (condition-case nil 
                (save-excursion (forward-midi 1 play-channels) (attribute-of-current-form 'absTime 'number))
                (error (setq done t) cur-abs-time)))
      (if (and (< (point) pn)) 
          (setq result (cons (- next-abs-time cur-abs-time) result))))
    (goto-char p0)
    (reverse result))))

(defun live-noteon-pause-list-delta-time (in-buf p0 pn play-channels)
 (save-excursion
  (let ((cur-delta-time nil)
        (done nil)
        (result nil)
        (target-form-of-navigation t)  ; for DYNAMIC BINDING purposes!
        (accumulated-time 0)
       )
    (set-buffer in-buf)
    (goto-char p0)
    (forward-midi 0 t)  ; t because messages from all channels must be visited to accumulate their delta time contributions.
    (setq cur-delta-time (attribute-of-current-form 'deltaTime 'number))
    (setq accumulated-time cur-delta-time)
    (if (and (equal (kind-of-message-on-current-line) "NoteOn") (or (eq play-channels t) (member (attribute-of-current-form 'channel 'number) play-channels)))
        (progn 
          (setq result (cons accumulated-time result))
          (setq accumulated-time 0)))

    (while (and (< (point) pn) (not done)) 
      (forward-midi 1 t)
      (if (not (at-end-parenthesis-line (point)))
          (progn
            (setq cur-delta-time (attribute-of-current-form 'deltaTime 'number))
            (setq accumulated-time (+ accumulated-time cur-delta-time))
            (if (and (equal (kind-of-message-on-current-line) "NoteOn") (or (eq play-channels t) (member (attribute-of-current-form 'channel 'number) play-channels)))
                (progn
                  (setq result (cons accumulated-time result))
                  (setq accumulated-time 0))))
          (setq done t)))
    (goto-char p0)
    (cdr (reverse result)))))


(defun startup-latency-of-command-line-midi-player ()
  (/ (accumulate-right (function +) 0.0 (mapcar (function startup-latency-of-command-line-midi-player-single) (number-interval 1 10))) 10))
  
(defun startup-latency-of-command-line-midi-player-single (&optional dummy)
  (let ((t0 (get-internal-run-time))
        (path-to-empty-midi-file (concat midi-software-dir "midi/emtpy.mid"))
       )
     (play-midi-file-sync path-to-empty-midi-file)
     (let* ((tn (get-internal-run-time))
             (diff (time-subtract tn t0))
           )
        (+ (float (cadr diff)) (/ (caddr diff) (float 1000000))))))

(defun play-accuracy-of-command-line-midi-player (&optional dummy)
  "For information only, display the playing time of a 6.0 seconds silent MIDI file."
  (let ((t0 (get-internal-run-time))
        (path-to-empty-midi-file (concat midi-software-dir "midi/6-sek.mid"))
       )
     (play-midi-file-sync path-to-empty-midi-file)
     (let* ((tn (get-internal-run-time))
             (diff (time-subtract tn t0))
           )
        (message "Theoretical playing time: 6.0 seconds. Actual playing time: %f seconds." 
                 (+ (float (cadr diff)) (/ (caddr diff) (float 1000000)))))))


(defun rename-midl-project (new-proper-name)
  "Rename the current midl project to new-proper-name (which is a proper name).
new-proper-name is assumed to be non-existing."
  (interactive "sNew proper name: ")
  (let* ((orig-buf (current-buffer))
         (cur-dir (current-directory))
         (cur-name (file-name-sans-extension (buffer-name orig-buf)))
         (ext (file-name-extension (buffer-name orig-buf)))
        )
    (if (not (file-exists-p (concat cur-dir new-proper-name "." ext)))
        (progn
           (rename-file (concat cur-dir cur-name "." ext) (concat new-proper-name "." ext))     ; midl file
           (set-visited-file-name (concat new-proper-name "." ext))
           (rename-buffer (concat new-proper-name "." ext))                                     ; current buffer
           (if (file-exists-p (concat cur-dir cur-name))
               (rename-file (concat cur-dir cur-name) (concat new-proper-name)))                ; project dir (stack)

           ; In order redefine the stacks refered by the varibles midl-source-file-stack and midl-source-file-ghost-stack
           (setq midl-source-file-stack nil)
           (recover-midl-source-file-stack)
           (message "Done"))
        (message "%s.%s exists. Nothing done" new-proper-name ext))))

; ---------------------------------------------------------------------------------------------------------------

(defun check-midi-laml-setup ()
  "Check if MIDI LAML is configured appropriately in relation to use of external software."
  (interactive)
  (let* ((div-midi-dir-exists (file-exists-p div-midi-utilities-dir))
         (div-programs-exists (accumulate-right (function (lambda (x y) (and x y))) t (mapcar (function (lambda (fp) (file-exists-p fp))) (list div-play-smf div-send-midi-exe div-lsmidiouts div-lsmidiins div-brainstorm))))
         (cygwin-shell-exists (or (file-exists-p cygwin-shell) (file-exists-p (concat cygwin-shell ".exe"))))
  
         (recording-dir-exists (file-exists-p recording-dir))
         (midl-project-dir-exists (file-exists-p midl-project-dir))

         (winamp-exists (file-exists-p winamp-player-exc))

         (abc-dir-exists (file-exists-p abc-dir))
         (abc2midi-exists (file-exists-p (abc2midi-exc)))
         (midi2abc-exists (file-exists-p (midi2abc-exc)))
         (abc-ps-dir-exists (file-exists-p abc-ps-dir))
         (abcm2ps-exists (file-exists-p (abcm2ps-exc)))
         (ghost-view-dir-exists (file-exists-p gs-dir))
         (ghost-view-exists (file-exists-p (gsview-exc)))
 
         (midi-illustrate-exists (file-exists-p midi-illustrator-exc))
        )
   (let* ((buf-name "*MIDI LAML CHECK*")
          (info-buffer (if (get-buffer buf-name) (get-buffer buf-name) (generate-new-buffer buf-name))))
    (set-window-buffer (other-window 1) info-buffer)
    (set-buffer info-buffer)
    (toggle-read-only -1)
    (erase-buffer)

    (if div-midi-dir-exists
        (insert "DIV MIDI dir OK.")
        (insert "The DIV MIDI command line programmes cannot be located on this PC. This must be corrected.") )  (insert CR)

    (if div-programs-exists
        (insert "DIV MIDI programs OK.")
        (insert "Some important DIV MIDI programs cannot be located on this PC. This must be corrected.")) (insert CR)

    (if cygwin-shell-exists
        (insert "Cygwin shell OK.")
        (insert "The Cygwin shell cannot be located in this PC. This must be corrected.")) (insert CR)

    (if recording-dir-exists
        (insert "Recording dir OK.")
        (insert "The designated recording dir does not exists. This is only a problem if you plan to use the Tools > Recording functions")) (insert CR)

    (if midl-project-dir-exists
        (insert "MIDI LAML project dir OK.")
        (insert "The designated midi laml project directory does not exists. This is most likely NOT a problem for you.")) (insert CR)

    (if winamp-exists
        (insert "Winamp OK.")
        (insert "The file path to WinAmp does not exists. If you only use the command line player, this is NOT a problem for you.")) (insert CR)

    (if (and abc-dir-exists abc2midi-exists midi2abc-exists)
        (insert "The ABC dir and programs are OK.")
        (insert "The ABC dir, or one of the programs in this dir, does not exist. This is most likely not a problem for you.")) (insert CR)

    (if (and abc-ps-dir-exists abcm2ps-exists)
        (insert "The ABC postscript dir and program are OK.")
        (insert "The ABC postscript dir, or the program in the dir, does not exist. This is most likely not a problem for you.")) (insert CR)

    (if (and  ghost-view-dir-exists ghost-view-exists)
        (insert "The ghostview dir and ghostview are OK.")
        (insert "The ghostview dir, or the ghostview program, does not exist. Not a major problem.")) (insert CR)    

    (if midi-illustrator-support 
        (if midi-illustrate-exists
            (insert "Midi illustrator OK.")
            (insert "The Midi illustrator cannot be located on this PC. You cannot see scores of your MIDI files. But this is probably OK."))) (insert CR) (insert CR)

    (if (not (and div-midi-dir-exists div-programs-exists cygwin-shell-exists 
                  recording-dir-exists midl-project-dir-exists
                  winamp-exists 
                  abc-dir-exists abc2midi-exists midi2abc-exists abc-ps-dir-exists abcm2ps-exists
                  ghost-view-dir-exists ghost-view-exists))

        (progn
           (insert "YOU MAY WANT TO CHECK THE MIDI LAML SETUP IN YOUR .EMACS FILE - AND THE VERY TOP OF THE FILE") (insert CR)
           (insert laml-dir "styles/xml-in-laml/midi/midi.el")))

    (toggle-read-only 1)
    (set-buffer-modified-p nil))))


; Wheel commands:

(defvar window-start-value-before-page-play nil "The value of (window-start) before a page-oriented play operation")

(defun process-from-point-by-wheel ()
 "Supports both wheeling with and without moving cursor."
 (interactive)
 (setq window-start-value-before-page-play (window-start))
 (if (point-at-last-note-of-window)
     (if use-live-cursor-movement
         (process-next-page-and-play)
         (let ((end-pos (min (window-end)   (current-track-pos-after-last-midi-event))))
           (set-mark nil)
           (save-excursion (process-next-page-and-play))
;           (sit-for 2.5)   ; This pause gives the impression of waiting a little before turning the page.
           (wait-while-midi-playing)
           (goto-char (window-end))
           (recenter 1)))
     (if use-live-cursor-movement
         (progn
           (set-mark nil)
           (forward-midi 1 channels-to-play)
           (process-from-point-and-current-page-and-play))
         (let ((end-pos (min (window-end)   (current-track-pos-after-last-midi-event))))
           (set-mark nil)
           (save-excursion (process-from-point-and-current-page-and-play))
;           (sit-for 2.5)   ; This pause gives the impression of waiting a little before turning the page.
           (wait-while-midi-playing)
           (goto-char (window-end))
           (recenter 1))))

)

(defun wait-while-midi-playing()
  (if (get-process midi-play-process-name)
      (progn 
         (sit-for 0.5)
         (wait-while-midi-playing))))  

(defun point-at-last-note-of-window ()
  (let ((end-p   (min (window-end)   (current-track-pos-after-last-midi-event))))
    (save-excursion
      (forward-midi 2 channels-to-play)
      (let ((p (point)))
         (> p end-p)))))
        
   

(defun back-midi-playing-by-wheel ()
  (interactive)
  (reset-shadow-overlay)
  (if (get-process midi-play-process-name)
      (stop-midi-playing)
      (previous-midi-message-and-play)))


; Experimental
(defun midl-documentation-of-key ()
  "Show how a given key combination, function key or menu entry works"
  (interactive)
  (let* ((fn (local-key-binding (read-key-sequence "Enter key sequence: "))) 
         (doc (cond ((and fn (symbolp fn))                  (documentation fn))
                    ((and fn (consp fn) (eq (car fn) 'lambda)) (documentation-string-of-lambda-form fn)) 
                    (t "")))) 
    (show-tool-tip-text 
      (if (not (empty-string-p doc))
          (concat (if (symbolp fn) (as-string fn) "") (if (symbolp fn) CR "") doc )
          "NOT documented"))
   )
)

(defun documentation-string-of-lambda-form (lambda-form-list)
  (if (and (listp lambda-form-list) (>= (length lambda-form-list) 4))
      (cond ((stringp (caddr lambda-form-list)) (caddr lambda-form-list))
            ((stringp (caddr (cdr lambda-form-list))) (caddr (cdr lambda-form-list)))
            (t ""))
      ""))


; ---------------------------------------------------------------------------------------------------------------
; STEP RECORDING:

; INTERNAL EXPLANATION OF STEP RECORDING:
; 
; M-x step-record.
; A buffer is set up, in which the steps will appear after a little delay.
; Recordings are done by the MIDI brainstorm program that produces small MIDI files in a temp directory.
; After timeout the recorded MIDI file is processed:
;   1. By sync-midi-file-to-laml-step-recording-file in midi.el
;   2. This produces a laml file in a temp directory which applies midi-file-to-laml-step-recording (from midi.scm) on the midi file.
;      In order to make a suitable MIDL file.
;   3. The Scheme function midi-file-to-laml-ast produces that deltaTimed AST, which is passed through the Scheme function as-step-recording-on-motif-nested-variable-timing (from midi-laml-processing-lib.scm).
;   4. The function as-step-recording-on-motif-nested-variable-timing performs special purpose quantification on the pieces.
;      This not not yet tied together in a satisfactory way.  
; The steps above are done iteratively for each MIDI file created by brainstorm.
; 
; The most severe problems is instability, because some Scheme processings fail.

(defvar step-recording-p nil "Controls if step recording is in progress.")
(defvar pre-step-recording-background-color nil)
(defvar pre-step-recording-foreground-color nil)

(defun step-record (file-name)
  (interactive "sFile name of step recording: ")

  (setq step-recording-p t)
  (let* ((step-recording-midi-dir (concat laml-temp-file-path "step-recording/"))
         (file-name-with-extension (if (string-contains file-name ".midl") file-name (concat file-name ".midl")))
         (file-name-with-path-and-extension (concat step-recording-dir file-name-with-extension))
        )
    (if (file-exists-p file-name-with-path-and-extension)
        (message "The file %s exists. Nothing DONE. Try again." file-name-with-path-and-extension)
        (progn
          (ensure-directory-existence laml-temp-file-path "step-recording")
          (delete-midi-files-in-dir step-recording-midi-dir)

          (setq pre-step-recording-background-color (assoc-get-cdr 'background-color (frame-parameters)))
          (setq pre-step-recording-foreground-color (assoc-get-cdr 'foreground-color (frame-parameters)))
 
          (let* ((process-name "Step Recording")
                 (step-recording t)
                 (emergency-halt-step-recording nil)
                 (port midi-in-port) ;   (as-number (read-from-minibuffer "Record from midi port: "))
                 (SP " ")
                 (time-out 2)
                 (recording-buf (make-empty-step-recording-buffer file-name-with-path-and-extension))
                 (insertion-part 1)
                )

            (message "Step recording from MIDI port %s to the file %s.  Stop step recording by hitting the Pitch Bend Wheel." port file-name-with-extension)
            (green-background)

            (start-process process-name nil cygwin-shell "-c" 
                           (concat "cd " step-recording-midi-dir ";"
                                   div-brainstorm SP "--in" SP (as-string port) SP "--timeout" SP (as-string time-out) SP "--prefix" SP "step-rec-"))
            (catch 'exit
             (while step-recording
              (show-midl-mode-status-string)
              (let* ((step-recorded-files (filter (function (lambda (f) (equal (file-name-extension f) "mid")))  (eliminate-dot-and-dot-dot (directory-files step-recording-midi-dir))))
                     (sorted-step-recorded-files step-recorded-files))    ; sorting not necessary
                (if (not (null sorted-step-recorded-files))
                    (let* ((f1 (car sorted-step-recorded-files))
                           (midi-full-path (concat step-recording-midi-dir f1))
                           (midl-full-path (concat laml-temp-file-path "step.midl"))
                           )
                      (if (file-exists-p midl-full-path) (delete-file midl-full-path))

                      (setq quantification (get-quantification-from-mouse-x-position))

                      (sync-midi-file-to-laml-step-recording-file "deltaTime" "0" nil (as-string quantification) ; generates a midl file: midl-full-path
                                                                  midi-full-path midl-full-path)
                      (sit-for 0.3)

                      ; The following catches errors related to a situation when the file midl-full-path does not show up, because of errors in the Scheme processing of midl file.
                      (let ((n 1))
                        (while (and (not (file-exists-p midl-full-path)) (< n 10)) 
                          (message (as-string n) " " "Waiting for midl file to be generated...")
                          (setq n (+ n 1))
                          (sit-for 0.3))
                        (if (>= n 10) (setq emergency-halt-step-recording t))
                      )
                      (if emergency-halt-step-recording (progn (debug "Emergency") (throw 'exit t)))

                      (setq step-recording (not (is-there-an-end-step-recording-signal-in-file midl-full-path)))
                      (insert-notes-from-midl-file-selective midl-full-path recording-buf insertion-part)
                      (setq insertion-part (+ 1 insertion-part))
                      
                      (delete-file midi-full-path)
                      (delete-file midl-full-path)
                    )
                  )
                (if (<= (length sorted-step-recorded-files) 1) (sit-for 0.3))
                )
              )
            )
            (stop-step-recording emergency-halt-step-recording)) ))))


(defun get-quantification-from-mouse-x-position()
  (let ((x-pixel-pos (current-mouse-pixel-position)))
    (if x-pixel-pos
        (let* ((fr-width (frame-pixel-width))
               (fr-column-width (/ (float fr-width) 6.0))
               (determinator (/ (float x-pixel-pos) fr-column-width))
               )
          (cond ((<= determinator 1.0) 1)
                ((<= determinator 2.0) 2)
                ((<= determinator 3.0) 4)
                ((<= determinator 4.0) 8)
                ((<= determinator 5.0) 16)
                ((<= determinator 6.0) 32)
                (t 4)))
        4)))

(defun stop-step-recording (emergency)
  (interactive (list nil))
  (setq step-recording-p nil)
  (let ((process-name "Step Recording"))

    (save-buffer)
    (set-background-color pre-step-recording-background-color)
    (set-foreground-color pre-step-recording-foreground-color)

    ; kill the MIDI recording process, if there and show recorded files.
    (if (get-process process-name)
        (progn
          (delete-process process-name)
          (if emergency 
              (message "STEP RECORDING HAS ENDED - EMERGENCY")
              (message "STEP RECORDING HAS ENDED")))
        (message "Nothing to stop"))))  


(defun is-there-an-end-step-recording-signal-in-file (midl-file-path)
  "End of step recording is signalled by one or more pitch bend messages."
  (let ((buf (get-buffer-create "*step-recording-temp*"))
        (result nil)
       )
     (set-buffer buf)
     (erase-buffer)
     (insert-file midl-file-path)

     (goto-char (point-min))
     (let ((res (search-forward "(PitchBendChange" nil t)))
        (setq result res))

     (kill-buffer buf)
     result))

; NOT USED - insert-notes-from-midl-file-selective is used instead.
(defun insert-notes-from-midl-file (midl-file-path recording-buf insertion-part)
  (let ((buf (get-buffer-create "*step-recording-temp*"))
        (p0 nil) (p1 nil)
        (notes-string "")
       )
     (set-buffer buf)
     (erase-buffer)
     (insert-file midl-file-path)

     (goto-char (point-min))
     (let ((res (search-forward "(NoteOn" nil t)))
        (if res 
            (progn (beginning-of-line 1) (setq p0 (point)))))

     (goto-char (point-max))
     (let ((res (search-backward "(NoteOn" nil t)))
        (if res 
            (progn (end-of-line 1) (setq p1 (point)))))

     (if (and p0 p1)
         (setq notes-string (buffer-substring-no-properties p0 p1))
         (setq notes-string ""))

     (kill-buffer buf)

     (set-buffer recording-buf)
     (let ((p0 (point)))
       (insert notes-string) (insert CR)

       (if (= insertion-part 1)
           (ensure-start-at-0 p0)
           (ensure-pause-after-insertion p0)) 
     )
  )
)

(defun insert-notes-from-midl-file-selective (midl-file-path recording-buf insertion-part)
  (let ((buf (get-buffer-create "*step-recording-temp*"))
        (p0 nil) (p1 nil)
        (notes-string "")
       )
     (set-buffer buf)
     (erase-buffer)
     (insert-file midl-file-path)

     (goto-char (point-min))

     (while (search-forward "(NoteOn" nil t)
        (beginning-of-line 1)
        (setq p0 (point))
        (end-of-line 1)
        (setq p1 (point))

        (setq notes-string (concat notes-string (buffer-substring-no-properties p0 p1) CR))
     )
     (kill-buffer buf)

     (set-buffer recording-buf)
     (let ((p0 (point)))
       (insert notes-string) 
       (insert CR)    ; extra empty line - separates inserted parts

       (if (= insertion-part 1)
           (ensure-start-at-0 p0)
         (ensure-pause-after-insertion p0)) 
     )
     
  )
)

(defun ensure-start-at-0 (p-start)
  "Insert deltaTime 0 in the first NoteOn of the insertion. Insertion starts af p-start."
  (save-excursion
    (goto-char p-start)
    (set-attribute-of-current-form 'deltaTime 0)
  ))

(defun ensure-pause-after-insertion (p-start)
  "Insert a deltaTime such that there will be a pause of the same length as the duration of the previously inserted note"
  (save-excursion
    (goto-char p-start)
    (backward-sexp 1)
    (let ((duration-of-previous-noteon (attribute-of-current-form 'duration 'number)))
       (goto-char p-start) 
       (set-attribute-of-current-form 'deltaTime (* 2 duration-of-previous-noteon)))
  )
)


(defun sync-midi-file-to-laml-step-recording-file (processing-mode counter-transposition track-separation quantification full-path-midi-file full-path-laml-file) 
  (let ((laml-temp-processing-dir (concat laml-dir "temp/"))
        (laml-temp-file "temp-processing-program.laml"))

    (if (file-exists-p (concat laml-temp-processing-dir laml-temp-file)) (delete-file (concat laml-temp-processing-dir laml-temp-file)))

    (let* ((buf (make-a-file-from-laml-template 
                 "temp-processing-program.laml"
                 laml-temp-processing-dir
                 "midi-to-laml-processor-step-recording"
                 'laml-mode
                 (list
                  (list "FULL-PATH-TO-MIDI-FILE" full-path-midi-file)
                  (list "FULL-PATH-TO-LAML-FILE" full-path-laml-file)
                  (list "THE-PROCESSING-MODE" processing-mode)
                  (list "THE-COUNTER-TRANSPOSITION" counter-transposition)
                  (list "THE-QUANTIFICATION" quantification)
                  (list "THE-TRACK-SEPARATION" (if track-separation "true" "false"))
                  )
                 (concat laml-dir "styles/xml-in-laml/midi/" "templates/"))))
      (kill-buffer buf) ; Cleans up. Buffer not used - only the written file. 
      (laml-sync-process-file (concat laml-temp-processing-dir "temp-processing-program.laml") 'silent))
      (delete-file (concat laml-temp-processing-dir "temp-processing-program.laml"))
  )
)

            
(defun make-empty-step-recording-buffer(midl-file-path)
  (let ((buf (get-buffer-create "step-recording.midl")))
    (set-buffer buf)
    (set-window-buffer (selected-window) buf)
    (erase-buffer)
    (insert-file (concat laml-dir "styles/xml-in-laml/midi/" "templates/empty-step-recording-file.tpl"))
    (midi-laml-mode)
    (set-visited-file-name midl-file-path)
    (goto-char (point-min))
    (search-forward "MidiTrack" nil t)
    (forward-sexp 1)  ; stepping over the tempo Meta message
    (forward-char 2)
    buf
  )
)

; Moved to general el file June 1, 2011.
(defun eliminate-dot-and-dot-dot (file-name-list)
  (filter (function (lambda (fn) (and (not (equal fn ".")) (not (equal fn ".."))))) file-name-list))

(defun delete-midi-files-in-dir (dir)
  (let ((files (filter (function (lambda (f) (equal (file-name-extension f) "mid"))) (eliminate-dot-and-dot-dot (directory-files dir)))))
    (mapcar (function (lambda (file-name) (delete-file (concat dir file-name)))) files)))

; ---------------------------------------------------------------------------------------------------------------
; STACK BROWSER STUFF

(define-derived-mode 
        stack-browser-mode fundamental-mode "StackBrowser" 
  "A line-oriented browser of the MIDL stack."
)

(defvar stack-browser-mode-map (make-sparse-keymap "StackBrowser"))
(define-key stack-browser-mode-map [down] 'sb-next-line)   
(define-key stack-browser-mode-map [up] 'sb-previous-line)
(define-key stack-browser-mode-map [left] 'scroll-down-other-window)   
(define-key stack-browser-mode-map [right] 'scroll-other-window)   
(define-key stack-browser-mode-map [up] 'sb-previous-line)
(define-key stack-browser-mode-map "a" 'beginning-of-buffer-other-window)
(define-key stack-browser-mode-map "z" 'end-of-buffer-other-window)
(define-key stack-browser-mode-map "o" 'other-window)
(define-key stack-browser-mode-map "p" 'sb-process-laml-file-and-play)
(define-key stack-browser-mode-map "g" 'sb-goto-stack-level)
(define-key stack-browser-mode-map "\C-l" 'sb-reposition-highligting)

(laml-define-key stack-browser-mode-map [menu-bar stack-browser]
    (cons "Stack Browser" (make-sparse-keymap "Stack Browser")))

(laml-define-key stack-browser-mode-map [menu-bar stack-browser sb-process-laml-file-and-play]
      '("Play" . sb-process-laml-file-and-play))

(laml-define-key stack-browser-mode-map [menu-bar stack-browser sb-goto-stack-level]
      '("Goto level" . sb-goto-stack-level))



(defun stack-browser ()
  (interactive)

  (let* ((midl-buf-name (file-name-proper (buffer-name (current-buffer))))
         (browser-buffer-name (concat "stack-browser-" midl-buf-name))
         (browser-buffer (if (get-buffer browser-buffer-name) (get-buffer browser-buffer-name) (generate-new-buffer browser-buffer-name)))
         (current-project-name (file-name-proper (file-name-proper (current-file))))
         (current-dir (current-directory))
         (current-file (concat current-dir (buffer-name (current-buffer))))
         (stack-files-full-path (append (reverse midl-source-file-stack) (list current-file) midl-source-file-ghost-stack))  ; full path
         (midl-source-file-stack-lgt (length midl-source-file-stack))
        )

    (delete-other-windows)
    (save-buffer)
    (split-window-vertically)
    (set-window-buffer (selected-window) browser-buffer)
    (set-buffer browser-buffer)

    (insert-stack-browser-contents midl-source-file-stack-lgt stack-files-full-path midl-buf-name)
  )
)

(defun insert-stack-browser-contents (length-of-midl-source-file-stack stack-files-full-path proper-midl-file-name)
  (toggle-read-only -1)
  (erase-buffer)

  (mapcar2 
   (function
    (lambda (sf-full-path n)
      (let* ((sf (file-name-proper (file-name-nondirectory sf-full-path)))
             (kind-ch (cond ((numeric-string sf) "-")
                            ((equal sf proper-midl-file-name) "*")
                            (t ".")))
             (name (cond ((numeric-string sf) (concat sf "-stack"))      ; note used
                         ((equal sf proper-midl-file-name) "CURRENT")
                         (t sf)))
             (level (real-level-number-of sf length-of-midl-source-file-stack))
             (info (pp-stack-info-about sf-full-path))  ; time consuming - brings the MIDL file up in a temporary buffer for examination
            )
        (insert (format "%1s %2s: %s\n" kind-ch level info))
        (message (make-string n ?.))        
        )))
   stack-files-full-path 
   (number-interval 1 (length stack-files-full-path))   ; this parameter for feedback purposes only - not working well because of mark setting
  )

  (message "") ; resets feedback

  (goto-char (point-min))
  (while (not (looking-at "*")) (forward-line 1))
  (highlight-this-line)

                                        ; adjust size of browser:
  (let* ((lines-max (/ (frame-height) 2))
         (actual-number-of-lines (length stack-files-full-path))
         (extra-lines 3)
         (required-number-of-lines (+ extra-lines actual-number-of-lines))
         )
    (if (< required-number-of-lines lines-max)
        (enlarge-window (- required-number-of-lines lines-max)) ; a negative numer, thus shrinks the window size
      ))

  (toggle-read-only 1)
  (set-buffer-modified-p nil)
  (stack-browser-mode)
  )


(defun pp-stack-info-about (file-path)
  "Return a pretty printed stack info about the file with file-path (full path)"
  (save-window-excursion
    (let ((temp-buf (get-buffer-create "midl-buf-temp")))
       (set-buffer temp-buf)
       (erase-buffer)
       (insert-file file-path)
       (goto-char (point-min))

       (let* ((kind (kind-of-midl-file-in-current-buffer))
              (comment       (comment-of-midl-file))
              (midi-format   (if (eq kind 'midi) (format-of-midi-file-in-current-buffer) nil)) ; 0, 1
              (time-mode     (if (eq kind 'midi) (mode-of-standard-midi-file-current-buffer) nil)) ; absTime, deltaTime - A symbol
              (ppqn          (if (eq kind 'midi) (find-pulses-per-quarter-note-of-current-buffer) nil)) ; a number
              (counter-trans (if (eq kind 'midi) (find-counter-transposition-of-current-buffer) nil)) ; a string
              (normalized-p  (is-current-midl-buffer-normalized))
              (zoomed-p      (if (find-fixed-part-of-first-track-of-current-buffer) t nil)) 
             )
         (prog1 
             (cond  ((eq kind 'midi)
                     (format "SMF-%1d   PPQN: %4d   %9s   %1s%1d  %1s%1s  %s" 
                             midi-format ppqn (as-string time-mode) 
                             (if (>= (as-number counter-trans) 0) "+" "-") (abs (as-number counter-trans))
                             (if normalized-p "N" "P")
                             (if zoomed-p "Z" " ")
                             comment))
                    ((eq kind 'style)
                     "Style")
                    ((eq kind 'multi-pad)
                     "MultiPad")
                    (t "???"))       
           (kill-buffer temp-buf) )))))

(defun format-of-midi-file-in-current-buffer ()
  (save-excursion 
    (cond ((current-buffer-is-standard-midifile-of-format 0)
              0)
          ((current-buffer-is-standard-midifile-of-format 1)
              1)
          ((current-buffer-is-standard-midifile-of-format 2)
              2)
          (t (error "format-of-midi-file-in-current-buffer: unexpected format")))))

; Modelled after collect-ghost-fragments-from-file
(defun is-current-midl-buffer-normalized ()
 (save-excursion 
  (ensure-and-get-function-list-from-schemedoc-documentation midi-functions-files-list)  ; Cache the information in midi-functions-files-contents if necessary.
  (catch 'result
   (let ((fns-reg-exp (midi-functions-reg-exp (documented-abstractions-in-schemedoc-contents midi-functions-files-contents)))
         (def-reg-exp (definition-reg-exp))
         (result nil)
        )
     (goto-char (point-min))

     ; Skip initial MIDI LAML comment.
     (while (looking-at-chars (point) (list 9 10 13 32 59))  ; 59 = semicolon = comment char
       (cond ((looking-at-white-space (point)) (forward-char 1))
             (t (forward-line 1))))

     (if (re-search-forward (regular-expression def-reg-exp) nil t)
       (throw 'result nil) 
     )

     ; The cursor point is now after all definitions. The remaining capturings start at this position.
     ; This assumes that all definition come before the StandardMidiFile (or similar) form.

     (if (re-search-forward (regular-expression fns-reg-exp) nil t)
       (throw 'result nil)
     )

     t  ; If the control reaches this point, the buffer is considered normalized.
   )
  )
 )
)

(defun sb-next-line ()
  (interactive)

  (if (not (browser-stack-consistent (sb-count-stack-entries) (sb-count-ghost-entries)))
      (progn 
        (other-window 1)
        (stack-browser)))

  (let ((number-of-stack-entries (sb-count-stack-entries))
        (number-of-ghost-entries (sb-count-ghost-entries)))

    (if (>= number-of-ghost-entries 1)
        (progn
           (do-position-stack-browser-at-current-level)

           ; Operate on MIDL buffer:
           (other-window 1)
           (undo-pop-midl-stack)
           (other-window 1)

           (toggle-read-only -1)
           (set-stack-browser-kind-in-buffer "-")
           (next-line 1)
           (set-stack-browser-kind-in-buffer "*")
           (toggle-read-only 1)
           (highlight-this-line)
        )
        (message "There are no ghost entries. Nothing done."))))

(defun sb-previous-line ()
  (interactive)

  (if (not (browser-stack-consistent (sb-count-stack-entries) (sb-count-ghost-entries)))
      (progn 
        (other-window 1)
        (stack-browser)))

  (let ((number-of-stack-entries (sb-count-stack-entries))
        (number-of-ghost-entries (sb-count-ghost-entries)))
    (if (>= number-of-stack-entries 1)
        (progn
           (do-position-stack-browser-at-current-level)

           ; Operate on MIDL buffer:
           (other-window 1)
           (pop-and-reestablish-midl-file)
           (other-window 1)

           (toggle-read-only -1)
           (set-stack-browser-kind-in-buffer ".")
           (next-line -1)
           (set-stack-browser-kind-in-buffer "*")
           (toggle-read-only 1)
           (highlight-this-line)
        )
        (message "Already at top of stack. Nothing done.")
        )))

(defun browser-stack-consistent (number-of-stack-entries number-of-ghost-entries)
  "Return whether the browser stack is consistent relative to the stack and ghost lists in the other window. 
Must be called from a stack browser in a dual window setup (browser + MIDL buffer)."
 (save-window-excursion
    (other-window 1)
    (and (= number-of-stack-entries (length midl-source-file-stack))
         (= number-of-ghost-entries (length midl-source-file-ghost-stack)))))

(defun do-position-stack-browser-at-current-level ()
  (goto-char (point-min))
  (while (not (looking-at-char-number ?*))
         (progn 
            (forward-line 1) 
            (beginning-of-line 1))))

(defun sb-count-stack-entries ()
 (let ((count 0))
  (save-excursion
     (goto-char (point-min))
     (while (looking-at-char-number ?-)
         (progn 
            (setq count (+ count 1))
            (forward-line 1) 
            (beginning-of-line 1)))
     count)))


(defun sb-count-ghost-entries ()
 (let ((count 0))
  (save-excursion
     (goto-char (point-max))
     (previous-line 1)
     (beginning-of-line) 

     (while (looking-at-char-number ?.)
         (progn 
            (setq count (+ count 1))
            (forward-line -1) 
            (beginning-of-line 1)))
     count)))

(defun set-stack-browser-kind-in-buffer (kind-str)
  (beginning-of-line 1)
  (delete-char 1)
  (insert kind-str)
)

(defun reset-kind-chars-of-stack-brower (current-level)
 "Reset the initial 'kind char' of a stack browser. current-level is the new * entry."
 (toggle-read-only -1)

 (let ((number-of-entries (+ 1 (sb-count-stack-entries) (sb-count-ghost-entries)))
       (cur 1))
 
   (goto-char (point-min))  ; now at level cur
   (while (<= cur number-of-entries)
     (cond ((< cur current-level) (set-stack-browser-kind-in-buffer "-"))
           ((= cur current-level) (set-stack-browser-kind-in-buffer "*"))
           ((> cur current-level) (set-stack-browser-kind-in-buffer "."))
     )
     (setq cur (+ cur 1))
     (forward-line 1))
 )

 (toggle-read-only 1)
)

(defun sb-process-laml-file-and-play ()
  (interactive)
  (other-window 1)
  (process-laml-file-and-play)
  (other-window 1)
  (highlight-this-line))

(defun sb-goto-stack-level ()
  (interactive)

  (let ((level (level-of-stack-browser))
        (count 1))
    (other-window 1)

    ; Handle MIDL window:
    (do-all-pops-of-midl-stack)
    (while (< count level)
      (undo-pop-midl-stack)
      (setq count (+ count 1)))

    (other-window 1)

    ; Handle stack browser:
    (reset-kind-chars-of-stack-brower level)
    (do-position-stack-browser-at-current-level)  
    (highlight-this-line)))


(defun level-of-stack-browser ()
  (save-excursion
    (beginning-of-line 1)
    (while (not (looking-at-chars (point) (number-interval 48 57)))  ; not looking at a ciffer char
       (forward-char 1))
      (as-number (name-under-point))))

(defun sb-reposition-highligting ()
  (interactive)
  (do-position-stack-browser-at-current-level)
  (beginning-of-line 1)
  (highlight-this-line))


; ================================================================================================================================================
; VOICE COLLECTIONS.

(defvar voice-collection-vector (make-vector 17 nil) "A vector of voice entries. Entry number 0 is not used. Indexed by channel number (1..16)")
(defvar voice-collection-file-path nil "The full path to a voice collection file")
(defvar most-recent-voice-collection-dir nil) 

; (make-variable-buffer-local 'voice-collection-vector)
; (make-variable-buffer-local 'voice-collection-file-path)
; (make-variable-buffer-local 'most-recent-voice-collection-dir)

(define-derived-mode 
	voice-collection-mode fundamental-mode "VoiceCollection" 

  (setq mode-line-format 
     '("%e"
       #("-" 0 1
         (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
       mode-line-mule-info mode-line-modified mode-line-frame-identification mode-line-buffer-identification
       #("   " 0 3
         (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
       mode-line-position
       (vc-mode vc-mode)
       #("  " 0 2
         (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
       mode-line-modes
       (which-func-mode
        ("" which-func-format
         #("--" 0 2
           (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))))
       (:eval (voice-collection-mode-status-string))
       (global-mode-string
        (#("--" 0 2
           (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
         global-mode-string))
       #("-%-" 0 3
         (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))))

)

(defun voice-collection-mode-status-string ()
  (if voice-collection-file-path
      (concat "File: " voice-collection-file-path)
      "[Not connected to file]")

)

(defvar voice-collection-mode-map (make-sparse-keymap "Voice Collection"))

(laml-define-key voice-collection-mode-map [menu-bar voice-collections]
    (cons "Voice Collections" (make-sparse-keymap "voice-collections")))



(laml-define-key voice-collection-mode-map [menu-bar voice-collections set-volume-of-voice-in-voice-collection]
    (cons "Change volume..." (function set-volume-of-voice-in-voice-collection)))
(define-key voice-collection-mode-map "v" 'set-volume-of-voice-in-voice-collection)

(laml-define-key voice-collection-mode-map [menu-bar voice-collections set-pan-of-voice-in-voice-collection]
    (cons "Change pan..." (function set-pan-of-voice-in-voice-collection)))
(define-key voice-collection-mode-map "p" 'set-pan-of-voice-in-voice-collection)

(laml-define-key voice-collection-mode-map [menu-bar voice-collections set-reverb-of-voice-in-voice-collection]
    (cons "Change reverb..." (function set-reverb-of-voice-in-voice-collection)))
(define-key voice-collection-mode-map "r" 'set-reverb-of-voice-in-voice-collection)

(laml-define-key voice-collection-mode-map [menu-bar voice-collections set-chorus-of-voice-in-voice-collection]
    (cons "Change chorus..." (function set-chorus-of-voice-in-voice-collection)))
(define-key voice-collection-mode-map "c" 'set-chorus-of-voice-in-voice-collection)

(laml-define-key voice-collection-mode-map [menu-bar voice-collections menu-pieces-sep-3]
    '("----"))

(laml-define-key voice-collection-mode-map [menu-bar voice-collections voice-collection-file-info]
    (cons "File info" (function voice-collection-file-info)))
(define-key  voice-collection-mode-map "i" 'voice-collection-file-info)

(laml-define-key voice-collection-mode-map [menu-bar voice-collections menu-pieces-sep-2]
    '("----"))

(laml-define-key voice-collection-mode-map [menu-bar voice-collections delete-channel-from-voice-collection]
    (cons "Delete voice from channel" (function delete-channel-from-voice-collection)))
(define-key  voice-collection-mode-map "d" 'delete-channel-from-voice-collection)

(laml-define-key voice-collection-mode-map [menu-bar voice-collections vc-exchange-voices-with-higher]
    (cons "Move voice to higher channel" (function vc-exchange-voices-with-higher))
)
(define-key voice-collection-mode-map [C-down] 'vc-exchange-voices-with-higher)   

(laml-define-key voice-collection-mode-map [menu-bar voice-collections vc-exchange-voices-with-lower]
    (cons "Move voice to lower channel" (function vc-exchange-voices-with-lower))
)
(define-key voice-collection-mode-map [C-up] 'vc-exchange-voices-with-lower)

(define-key voice-collection-mode-map [down] 'vc-next-line-and-highlight-line)   
(define-key voice-collection-mode-map [up] 'vc-previous-line-and-highlight-line)
(define-key voice-collection-mode-map "\C-l" 'vc-reposition-highligting)


(laml-define-key voice-collection-mode-map [menu-bar voice-collections menu-pieces-sep-3]
    '("----"))

(laml-define-key voice-collection-mode-map [menu-bar voice-collections transfer-voices-to-motif-xs]
    (cons "Transfer to Instrument" (function transfer-voices-to-motif-xs)))
(define-key  voice-collection-mode-map "t" 'transfer-voices-to-motif-xs)

(laml-define-key voice-collection-mode-map [menu-bar voice-collections browse-voices]
    (cons "Browse Voices" (make-sparse-keymap "Browse Voices")))

(laml-define-key  voice-collection-mode-map [menu-bar voice-collections browse-voices browse-motif-xs-voices]
    (cons "Motif XS"
       (function
         (lambda () 
            (interactive)
            (delete-other-windows)
            (split-window-vertically)
            (other-window 1)
            (xs-voice-browser)))))

(laml-define-key  voice-collection-mode-map [menu-bar voice-collections browse-voices browse-tyros-voices]
    (cons "Tyros"
       (function
         (lambda () 
            (interactive)
            (delete-other-windows)
            (split-window-vertically)
            (other-window 1)
            (tyros-voice-browser)))))

(laml-define-key  voice-collection-mode-map [menu-bar voice-collections browse-voices browse-gm-voices]
    (cons "General MIDI"
       (function
         (lambda () 
            (interactive)
            (delete-other-windows)
            (split-window-vertically)
            (other-window 1)
            (gm-voice-browser)))))


(laml-define-key voice-collection-mode-map [menu-bar voice-collections menu-pieces-sep-1]
    '("----"))

(laml-define-key voice-collection-mode-map [menu-bar voice-collections save-voice-collection-as]
    (cons "Save as..." (function save-voice-collection-as)))
(define-key  voice-collection-mode-map "S" 'save-voice-collection-as)

(laml-define-key voice-collection-mode-map [menu-bar voice-collections save-voice-collection]
    (cons "Save" (function save-voice-collection)))
(define-key  voice-collection-mode-map "s" 'save-voice-collection)

(laml-define-key voice-collection-mode-map [menu-bar voice-collections open-voice-collection]
    (cons "Open voice collection file..." (function open-voice-collection)))
(define-key  voice-collection-mode-map "o" 'open-voice-collection)

(laml-define-key voice-collection-mode-map [menu-bar voice-collections new-voice-collection-from-voice-collection]
    (cons "New voice collection" (function new-voice-collection-from-voice-collection)))
(define-key  voice-collection-mode-map "n" 'new-voice-collection-from-voice-collection)


(defun present-voice-collection (&optional dir modified)
  "From a voice browser, present the voice collection in another window.
dir is a directory associated with the voice collection.
modified is a boolean value that indicates if the voice collection has been modified prior to saving it."
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  (let ((buf (get-buffer-create "Voice-collection")))
      (set-buffer buf)
      (toggle-read-only -1)
      (erase-buffer)
      (show-buffer (selected-window) buf)
      (mapcar2 (function present-voice-collection-entry) (cdr (vector-to-list voice-collection-vector)) (number-interval 1 16))
      (goto-char (point-min))
      (voice-collection-mode)
      (if dir (setq default-directory dir))  ; default-directory is buffer-local
      (set-buffer-modified-p modified)
      (toggle-read-only 1)
      (adjust-size-of-window 16 3)
      (other-window 1)
    ))

(defun present-voice-collection-this-window (&optional dir modified)
  "Voice collection in the current window.
dir is a directory associated with the voice collection.
modified is a boolean value that indicates if the voice collection has been modified prior to saving it."
  (interactive)
  (let ((buf (get-buffer-create "Voice-collection"))  
         )
      (set-buffer buf)
      (toggle-read-only -1)
      (erase-buffer)
      (show-buffer (selected-window) buf)
      (mapcar2 (function present-voice-collection-entry) (cdr (vector-to-list voice-collection-vector)) (number-interval 1 16))
      (goto-char (point-min))
      (voice-collection-mode)
      (if dir (setq default-directory dir))  ; default-directory is buffer-local
      (set-buffer-modified-p modified)
      (toggle-read-only 1)
    ))

(defun re-present-voice-collection (&optional dir modified)
  "From within the voice collection, re-present the fresh voice collection"
  (let ((line (line-number-at-pos)))
    (toggle-read-only -1)
    (erase-buffer)
    (mapcar2 (function present-voice-collection-entry) (cdr (vector-to-list voice-collection-vector)) (number-interval 1 16))
    (goto-char (point-min))
    (next-line (- line 1))
    (voice-collection-mode)
    (if dir (setq default-directory dir))  ; default-directory is buffer-local
    (set-buffer-modified-p modified)
    (toggle-read-only 1)))

(defun present-voice-collection-entry (entry channel)
  (if (not (null entry))
      (let* ((raw-pan (vcb-pan-of entry))
             (presented-pan (format "%1s%-2s" (cond ((< raw-pan 64) "L")
                                                   ((= raw-pan 64) "C")
                                                   (t "R"))
                                             (cond ((< raw-pan 64) (as-string raw-pan))
                                                   ((= raw-pan 64) "")
                                                   (t (as-string (- raw-pan 64)))) ))
            )
        (insert (format "Channel %2.d: %-40.40s   MSB:%3d  LSB:%3d  PC:%3d           REV:%3d  CHO:%3d  PAN:%4s  VOL:%3d\n" 
                        channel (vcb-voice-name-of entry) (vcb-msb-of entry) (vcb-lsb-of entry) (vcb-pc-of entry)
                        (vcb-reverb-of entry) (vcb-chorus-of entry) presented-pan  (vcb-volume-of entry)
                )
        )
      )
      (insert (format "Channel %2.d: -\n" channel))))

; --------------------------------------------------------------------------------
; Voice collection entry selectors:

; Default values applied in selectors and constructors
(defvar vcb-default-volume 100 "The default volume in a voice collection browser")
(defvar vcb-default-pan 64 "The default pan in a voice collection browser")
(defvar vcb-default-reverb 12 "The default volume in a voice collection browser")
(defvar vcb-default-chorus 0 "The default volume in a voice collection browser")

; Selectors:
(defun vcb-voice-name-of (entry) (nth 3 entry))
(defun vcb-msb-of (entry) (nth 0 entry))
(defun vcb-lsb-of (entry) (nth 1 entry))
(defun vcb-pc-of (entry) (nth 2 entry))

(defun vcb-volume-of (entry)
  (if (> (length entry) 4)
      (nth 4 entry)
      vcb-default-volume))

(defun vcb-pan-of (entry)
  (if (> (length entry) 4)
      (nth 5 entry)
      vcb-default-pan))

(defun vcb-reverb-of (entry)
  (if (> (length entry) 4)
      (nth 6 entry)
      vcb-default-reverb))

(defun vcb-chorus-of (entry)
  (if (> (length entry) 4)
      (nth 7 entry)
      vcb-default-chorus))

; --------------------------------------------------------------------------------

; Voice collection entry constructor function.
; The parameters (msb lsb pc voice-name volumne pan reverb chorus).
; The first four are mandatory (of historical reasons) and the other are optional.
(defun make-voice-collection-entry (msb lsb pc name &rest rest-pars)
  (let* ((rest-pars-lgt (length rest-pars))
         (vol (if (> rest-pars-lgt 0) (nth 0 rest-pars) vcb-default-volume))
         (pan (if (> rest-pars-lgt 1) (nth 1 rest-pars) vcb-default-pan))
         (reverb (if (> rest-pars-lgt 2) (nth 2 rest-pars) vcb-default-reverb))
         (chorus (if (> rest-pars-lgt 3) (nth 3 rest-pars) vcb-default-chorus))
        )
    (list msb lsb pc name vol pan reverb chorus)
  )
)


(defun new-voice-collection ()
  "Make a new empty voice collection in the other window"
  (interactive)
  (setq voice-collection-vector (make-vector 17 nil))
  (setq voice-collection-file-path nil)
  (setq most-recent-voice-collection-dir nil)
  (present-voice-collection)
  (message "Done"))

(defun add-voice-to-collection (channel)
  (interactive "nAdd voice to collection. Channel (between 1 and 16): ")
  (let* ((number (xs-voice-entry-number-of-line))
         (full-entry (nth (- number 1) current-xs-voice-list))
         (msb (xs-voice-msb full-entry))
         (lsb (xs-voice-lsb full-entry))
         (pc (xs-voice-pc full-entry))
         (name (xs-voice-name full-entry))
         (voice-entry (make-voice-collection-entry msb lsb pc name))
        )
    (aset voice-collection-vector channel voice-entry)
    (present-voice-collection nil t)
    (message "The voice %s has been added to channel %d" name channel)))


(defun delete-voice-from-channel (channel)
  (interactive "nDelete voice from collection: Channel (between 1 and 16): ")
  (aset voice-collection-vector channel nil)
  (present-voice-collection nil t)
  (message "Channel %d has been cleared" channel))

(defun delete-channel-from-voice-collection ()
  "From within a voice collection, delete the currently selected channel"
  (interactive)
  (let ((channel (line-number-at-pos)))
    (aset voice-collection-vector channel nil)
    (re-present-voice-collection nil t)
    (message "Channel %d has been cleared" channel)))

(defun vc-next-line-and-highlight-line ()
  (interactive)
  (let ((cur-channel (line-number-at-pos)))
    (if (< cur-channel 16)
      (progn
        (next-line 1)
        (highlight-this-line)))))


(defun vc-previous-line-and-highlight-line ()
  (interactive)
  (let ((cur-channel (line-number-at-pos)))
    (if (> cur-channel 1)
      (progn
        (previous-line 1)
        (highlight-this-line)))))



(defun vc-exchange-voices-with-lower (channel)
  (interactive (list (line-number-at-pos)))
  (if (> channel 1)
      (progn
        (vc-exchange-channels channel (- channel 1))  
        (re-present-voice-collection nil t)
        (previous-line 1)
        (highlight-this-line)
       )
      (message "Cannot. Nothing done")))

(defun vc-exchange-voices-with-higher (channel)
  (interactive (list (line-number-at-pos)))
  (if (< channel 16)
      (progn
        (vc-exchange-channels channel (+ channel 1))  
        (re-present-voice-collection nil t)
        (next-line 1)
        (highlight-this-line)
      )
      (message "Cannot. Nothing done")))

(defun vc-exchange-channels (ch1 ch2)
  (let ((temp-entry (aref voice-collection-vector ch1)))
    (aset voice-collection-vector ch1 (aref voice-collection-vector ch2))
    (aset voice-collection-vector ch2 temp-entry)
  )
)

(defun vc-reposition-highligting ()
  (interactive)
  (beginning-of-line 1)
  (highlight-this-line))


; (msb lsb pc voice-name volumne pan reverb chorus)
(defun set-volume-of-voice-in-voice-collection (new-volume)
  "From a voice collection browser, mutate the volume"
  (interactive "nNew volume of channel: ")
  (let* ((channel (line-number-at-pos))
         (vc-entry (aref voice-collection-vector channel))
        )
    (if (and (>= new-volume 0) (<= new-volume 127))
        (let ((new-entry (make-voice-collection-entry
                             (vcb-msb-of vc-entry) (vcb-lsb-of vc-entry) (vcb-pc-of vc-entry) ; msb lsb pc
                             (vcb-voice-name-of vc-entry) ; voice name
                             new-volume (vcb-pan-of vc-entry) (vcb-reverb-of vc-entry) (vcb-chorus-of vc-entry) ; vol, pan, rev, chor
                          )))
          (aset voice-collection-vector channel new-entry)
          (re-present-voice-collection nil t)
          (vc-reposition-highligting)
          (message "DONE")
        )
        (message "New volume must be in the range 0 .. 127. Nothing done") )))

(defun set-pan-of-voice-in-voice-collection (new-pan)
  "From a voice collection browser, mutate the pan. Pan input is a string, such as L5, C or R15."
  (interactive "sNew pan of channel, such as L10, C or R25: ")
  (let* ((raw-pan (LCR-pan-to-raw-pan new-pan))
         (channel (line-number-at-pos))
         (vc-entry (aref voice-collection-vector channel))
        )
    (if (and (>= raw-pan 0) (<= raw-pan 127))
        (let ((new-entry (make-voice-collection-entry
                             (vcb-msb-of vc-entry) (vcb-lsb-of vc-entry) (vcb-pc-of vc-entry) ; msb lsb pc
                             (vcb-voice-name-of vc-entry) ; voice name
                             (vcb-volume-of vc-entry) raw-pan (vcb-reverb-of vc-entry) (vcb-chorus-of vc-entry) ; vol, pan, rev, chor
                          )))
          (aset voice-collection-vector channel new-entry)
          (re-present-voice-collection nil t)
          (vc-reposition-highligting)
          (message "DONE")
        )
        (message "New pan must be in the range L0 .. C .. R63. Nothing done") )))

(defun LCR-pan-to-raw-pan (string-pan)
  (let* ((pan-letter (aref (upcase string-pan) 0))
         (pan-number (as-number (if (= pan-letter ?C) 0 (substring string-pan 1))))
        )
    (cond ((= pan-letter ?L) pan-number)
          ((= pan-letter ?C) 64)
          ((= pan-letter ?R) (+ 64 pan-number))
          (t (error "Illegal pan string: " string-pan)))))

(defun set-reverb-of-voice-in-voice-collection (new-reverb)
  "From a voice collection browser, mutate the reverb"
  (interactive "nNew reverb of channel: ")
  (let* ((channel (line-number-at-pos))
         (vc-entry (aref voice-collection-vector channel))
        )
    (if (and (>= new-reverb 0) (<= new-reverb 127))
        (let ((new-entry (make-voice-collection-entry
                             (vcb-msb-of vc-entry) (vcb-lsb-of vc-entry) (vcb-pc-of vc-entry) ; msb lsb pc
                             (vcb-voice-name-of vc-entry) ; voice name
                             (vcb-volume-of vc-entry) (vcb-pan-of vc-entry) new-reverb (vcb-chorus-of vc-entry) ; vol, pan, rev, chor
                          )))
          (aset voice-collection-vector channel new-entry)
          (re-present-voice-collection nil t)
          (vc-reposition-highligting)
          (message "DONE")
        )
        (message "New reverb must be in the range 0 .. 127. Nothing done") )))

(defun set-chorus-of-voice-in-voice-collection (new-chorus)
  "From a voice collection browser, mutate the chorus"
  (interactive "nNew chorus of channel: ")
  (let* ((channel (line-number-at-pos))
         (vc-entry (aref voice-collection-vector channel))
        )
    (if (and (>= new-chorus 0) (<= new-chorus 127))
        (let ((new-entry (make-voice-collection-entry
                             (vcb-msb-of vc-entry) (vcb-lsb-of vc-entry) (vcb-pc-of vc-entry) ; msb lsb pc
                             (vcb-voice-name-of vc-entry) ; voice name
                             (vcb-volume-of vc-entry) (vcb-pan-of vc-entry) (vcb-reverb-of vc-entry) new-chorus ; vol, pan, rev, chor
                          )))
          (aset voice-collection-vector channel new-entry)
          (re-present-voice-collection nil t)
          (vc-reposition-highligting)
          (message "DONE")
        )
        (message "New chorus must be in the range 0 .. 127. Nothing done") )))


(defun new-voice-collection-from-voice-collection ()
  "Make a new voice collection from within the voice collection."
  (interactive)
  (setq voice-collection-vector (make-vector 17 nil))
  (setq voice-collection-file-path nil)
  (setq most-recent-voice-collection-dir nil)
  (re-present-voice-collection)
  (message "Done"))

; Should work for any connected instrument, but currently bound to the Motif XS.
(defun transfer-voices-to-motif-xs ()
  (interactive)
  (message "Defining voices in the Motif XS...")
  (let ((laml-temp-processing-dir (concat laml-dir "temp/"))
        (voices-string
          (list-to-string
            (mapcar2 (function 
                       (lambda (vce ch) (if vce 
                                            (format "  (voice %d %d %d %d) (pan %d %d) (reverb %d %d) (chorus %d %d) (volume %d %d)"
                                                                                         ch (vcb-msb-of vce) (vcb-lsb-of vce) (vcb-pc-of vce)
                                                                                         ch (vcb-pan-of vce)
                                                                                         ch (vcb-reverb-of vce)
                                                                                         ch (vcb-chorus-of vce)
                                                                                         ch (vcb-volume-of vce)
                                            )
                                             "")))
                     (cdr (vector-to-list voice-collection-vector))
                     (number-interval 1 16))
            CR))
       ) 
    ; Make a midl file from a template - insert (voice ....) forms:
    (make-a-file-from-laml-template 
       "temp-processing-program.laml"
       laml-temp-processing-dir
       "voice-collection-set"
       'laml-mode
       (list
        (list "VOICES" voices-string)
       )
       (concat laml-dir "styles/xml-in-laml/midi/" "templates/"))

    ; Produce midi file:  
    (laml-sync-process-file (concat laml-temp-processing-dir "temp-processing-program.laml") 'silent)

    (play-midi-file-sync (concat laml-temp-processing-dir "midi-temp/" "temp-processing-program.mid") motif-xs-midi-output-port))
  (message "Done"))

; from within voice collection
(defun save-voice-collection-as (file-path)
  (interactive (list (read-file-name "Save in file (with initial path, with or without extension): " 
                                     (if most-recent-voice-collection-dir most-recent-voice-collection-dir midl-project-dir))))
  (let ((full-path (concat (file-name-sans-extension file-path) ".voices")))
    (if (file-exists-p full-path) (delete-file full-path))
    (file-write-no-message voice-collection-vector full-path)
    (re-present-voice-collection nil nil)
    (setq voice-collection-file-path full-path)
    (setq most-recent-voice-collection-dir (file-name-directory full-path))
    (force-mode-line-update)
    (message "Voice collection saved in %s" full-path)))

(defun voice-collection-file-info ()
  (interactive)
  (if voice-collection-file-path
      (message "Current voice collection file: %s" voice-collection-file-path)
      (message "No file is associated to the voice collection."))
)

; from within voice collection
(defun save-voice-collection ()
  (interactive)
  (if voice-collection-file-path
      (progn
        (file-write-no-message voice-collection-vector voice-collection-file-path)
        (re-present-voice-collection nil nil)
        (message "Voice collection saved in %s" voice-collection-file-path))
      (message "Use save-voice-collection-as instead of this operation."))
  (force-mode-line-update)
)

(defun open-voice-collection (file-path)
  (interactive (list (read-file-name "Open voice collection file from: " (if most-recent-voice-collection-dir most-recent-voice-collection-dir midl-project-dir) nil t)))
  (if (file-exists-p file-path)
      (progn
         (setq voice-collection-vector (file-read file-path))
         (if (listp voice-collection-vector) (setq voice-collection-vector (apply (function vector) voice-collection-vector)))  ; ensure that it is a vector
         (setq voice-collection-file-path file-path)
         (re-present-voice-collection (file-name-directory file-path))
         (message "Done")
       )
      (message "The voice collection file does not exist"))
  (force-mode-line-update)
)

(defun open-voice-collection-from-browser-other-window (file-path)
  (interactive (list (read-file-name "Open voice collection file from: " (if most-recent-voice-collection-dir most-recent-voice-collection-dir midl-project-dir) nil t)))
  (if (file-exists-p file-path)
      (progn
         (setq voice-collection-vector (file-read file-path))
         (if (listp voice-collection-vector) (setq voice-collection-vector (apply (function vector) voice-collection-vector)))  ; ensure that it is a vector
         (setq voice-collection-file-path file-path)
         (present-voice-collection (file-name-directory file-path))
         (setq most-recent-voice-collection-dir (file-name-directory file-path))

         (message "Done")
       )
      (message "The voice collection file does not exist")))

(defun open-voice-collection-from-browser-this-window (file-path)
  (interactive (list (read-file-name "Open voice collection file from: " (if most-recent-voice-collection-dir most-recent-voice-collection-dir midl-project-dir) nil t)))
  (if (file-exists-p file-path)
      (progn
         (setq voice-collection-vector (file-read file-path))
         (if (listp voice-collection-vector) (setq voice-collection-vector (apply (function vector) voice-collection-vector)))  ; ensure that it is a vector
         (setq voice-collection-file-path file-path)
         (present-voice-collection-this-window (file-name-directory file-path))
         (setq most-recent-voice-collection-dir (file-name-directory file-path))

         (message "Done")
       )
      (message "The voice collection file does not exist")))

(defun open-voice-collection-from-dired ()
  "Open a voice collection from dired."
  (interactive)
  (let ((selected-files (dired-get-marked-files nil)))
    (if (and (= 1 (length selected-files)) (file-regular-p (car selected-files)))
        (let* ((full-path-voices-file (car selected-files))
               (ext (file-name-extension full-path-voices-file)))
          (cond ((equal (downcase ext) "voices") 
                 (open-voice-collection-from-browser-this-window full-path-voices-file))
                (t "Only a -.voices file can be opened.")))
        (progn
          (beep)
          (message "Cannot open. You should only select a single file.")))))



; =======================================================================================================================================================================================
; SUPPORT OF MIDI MESSAGE SHADOWS.

(defvar shadow-overlay nil "A buffer local overlay which represents the shadow of the currently selected MIDI LAML message")

(defvar kind-of-shadow '(duration-of-current-event) 
 "The kind of shadow. A list whose first element is a symbol: none, duration-of-current-event, number-of-seconds, number-of-quarter-notes, until-end-of-current-bar.
Additional element represent relevant data of the kind of shadow. ")

(defvar shadow-kinds '((duration-of-current-event) (until-end-of-current-bar) (number-of-quarter-notes 2.0) (number-of-quarter-notes 4.0)
                       (number-of-quarter-notes 16.0) (number-of-seconds 2.0) (number-of-seconds 5.0) (number-of-seconds 10.0) (none))
  "A list of possible shadow overlap settings, used by toggle-shadow-setting")

(defvar number-of-current-shadow 0 "The first (number 0) of shadow-kinds is considered the current one (used by toggle-shadow-setting)")

(defun toggle-shadow-setting ()
  "Toggle the type and setting of shadow overlay."
  (interactive)
  (let ((filtered-shadow-kinds (if (and (eq (kind-of-midl-file-in-current-buffer) 'midi) (eq (mode-of-standard-midi-file-current-buffer) 'deltaTime))
                                   (filter (lambda (k) (not (equal '(until-end-of-current-bar) k))) shadow-kinds)
                                   shadow-kinds)))
    (setq number-of-current-shadow (mod (+ 1 number-of-current-shadow) (length filtered-shadow-kinds)))
    (setq kind-of-shadow (nth number-of-current-shadow filtered-shadow-kinds))
    (reset-shadow-overlay)
    (highlight-this-line)
    (mark-shadow-of-current-event)
    (message-about-current-shadow)))

(defun reset-shadow-overlay ()
  (if shadow-overlay (delete-overlay shadow-overlay))
  (setq shadow-overlay nil))

(defun mark-shadow-of-current-event ()
  "Markup the shadow (duration) of the current event"
  (if (not (eq (car kind-of-shadow) 'none))
      (let* ((p0 (left-margin-pos-of (point)))
             (duration-attr-val (attribute-of-current-form 'duration 'number))
             (shadow-duration (calculate-shadow-duration-time-ticks kind-of-shadow))
             (pn (if shadow-duration (end-char-position-of-event-starting-at p0 shadow-duration) nil))
            )
        (if pn
            (progn
              (if (null shadow-overlay)
                  (progn (setq shadow-overlay (make-overlay p0 pn (current-buffer)))
                         (overlay-put shadow-overlay 'face (cons 'background-color "grey88")))
                (move-overlay shadow-overlay p0 pn))
              (if (> pn (window-end)) (scroll-up (/ (frame-height) 2)))
              )
          (reset-shadow-overlay)))))

(defun calculate-shadow-duration-time-ticks (kind-of-shadow)
  "Return the number of shadow time ticks in kind-of-shadow. May return nil if it is impossible to calculate the result."
  (cond ((eq (car kind-of-shadow) 'duration-of-current-event)
           (let* ((duration-attr-val (attribute-of-current-form 'duration 'number)))
             (if duration-attr-val (as-number duration-attr-val) 0)))
        ((eq (car kind-of-shadow) 'number-of-seconds)
           (let* ((second-count (cadr kind-of-shadow))
                  (ppqn (find-pulses-per-quarter-note-of-current-buffer))                     ; maybe cache!
                  (tempo-us-per-quater-note (hex-string-to-int (find-global-tempo-value)))    ; maybe cache!
                 )
              (round(/ (float (* ppqn 1000000 second-count)) tempo-us-per-quater-note))))
        ((eq (car kind-of-shadow) 'number-of-quarter-notes)
           (let* ((quater-note-count (cadr kind-of-shadow))
                  (ppqn (find-pulses-per-quarter-note-of-current-buffer))
                 )
              (round (* ppqn (float quater-note-count)))))
        ((eq (car kind-of-shadow) 'until-end-of-current-bar)                                 ; ONLY absTime
          (if (not (eq (mode-of-standard-midi-file-current-buffer) 'deltaTime))
              (let* ((ppqn (find-pulses-per-quarter-note-of-current-buffer))
                     (time-signature '(4 4)) ; fixed for now
                     (num (car time-signature))
                     (denom (cadr time-signature))
                     (pulses-per-whole-note (* 4 ppqn))
                     (ticks-per-bar (round (* pulses-per-whole-note (/ (float num) (float denom)))))
                     (start-time-current-mes (attribute-of-current-form 'absTime 'number))
                     (bar-number (/ start-time-current-mes ticks-per-bar)) ; integer division, zero based
                     (bar-start-time (* bar-number ticks-per-bar))
                     (bar-end-time (+ bar-start-time ticks-per-bar))
                     )
                (- bar-end-time start-time-current-mes))
               nil))
        (t (error "calculate-shadow-duration-time-ticks: Unknown kind of shadow: %s" (car kind-of-shadow)))))


(defun end-char-position-of-event-starting-at (start-pos shadow-duration)
  "Return the ending char position of the shadow, of shadow-duration (a number of ticks), of the event starting at start-pos" 
  (save-excursion
    (goto-char start-pos)
    (while (looking-at-white-space (point)) (forward-char 1))
    (if (looking-at-chars (point) (list 40))  ; looking at start paren
        (let* ((attributes (attributes-of-current-form))
               (abs-time (plist-get attributes 'absTime))
               (delta-time (plist-get attributes 'deltaTime))
               (end-pos (cond (abs-time   (forward-search-for-abs-time   start-pos (+ (as-number abs-time) shadow-duration)))
                              (delta-time (forward-search-for-delta-time start-pos shadow-duration))
                              (t nil)))
              )
          (if end-pos
              (progn
                (goto-char end-pos)
                (end-of-line 1)
                (point))
            nil))
         nil)))

(defun forward-search-for-delta-time (start-pos-in-buffer duration)
  "Search forward from start-pos-in-buffer for an event ending relatively at duration. Only possible in pure deltaTime mode. Return a position (an integer) or nil."
  (save-excursion
     (let ((forward-proceed t)
           (accumulated-time 0)
          )
       (goto-char start-pos-in-buffer)
;       (setq forward-proceed (forward-midi-message 1))
       (if forward-proceed
           (progn
             (while (and forward-proceed (<= accumulated-time duration))
               (setq forward-proceed (forward-midi-message 1))
               (if forward-proceed (setq accumulated-time (+ accumulated-time (attribute-of-current-form 'deltaTime 'number)))))
             (backward-midi-message 1)
            (end-of-line 1)
            (point))
          nil))))

(defun forward-search-for-abs-time (start-pos-in-buffer end-time)
  "Search forward from start-pos-in-buffer for an event ending at end-time. Only possible in pure absTime mode. Return a position (an integer) or nil."
  (save-excursion
     (let ((forward-proceed t))
       (goto-char start-pos-in-buffer)
       (setq forward-proceed (forward-midi-message 1))
       (if forward-proceed
           (progn
             (while (and forward-proceed (<= (as-number (attribute-of-current-form 'absTime)) end-time))
               (setq forward-proceed (forward-midi-message 1)))
             (backward-midi-message 1)
            (end-of-line 1)
            (point))
          nil))))

(defun left-margin-pos-of (p)
 (save-excursion
   (goto-char p)
   (beginning-of-line 1)
   (point)))


(defun set-kind-of-shadow-duration-of-current-event ()
  (interactive)
  (setq kind-of-shadow '(duration-of-current-event))
  (reset-shadow-overlay)
  (highlight-this-line)
  (mark-shadow-of-current-event)
  (message "Done: Now the shadow shows the duration of the current MIDI LAML event.")
)

(defun set-kind-of-shadow-number-of-seconds (number-of-seconds)
  (interactive "nHow many seconds (a floating point numer): ")
  (setq kind-of-shadow (list 'number-of-seconds (float number-of-seconds)))
  (reset-shadow-overlay)
  (highlight-this-line)
  (mark-shadow-of-current-event)
  (message "Done: Now the shadow shows a period of %3.2f seconds." number-of-seconds))

(defun set-kind-of-shadow-number-of-quarter-notes (number-of-quarter-notes)
  (interactive "nHow many seconds (a floating point numer): ")
  (setq kind-of-shadow (list 'number-of-quarter-notes (float number-of-quarter-notes)))
  (reset-shadow-overlay)
  (highlight-this-line)
  (mark-shadow-of-current-event)
  (message "Done: Now the shadow shows a period of %3.2f quarter notes." number-of-quarter-notes))

(defun set-kind-of-shadow-until-end-of-current-bar ()
  (interactive)
  (if (and (eq (kind-of-midl-file-in-current-buffer) 'midi) (eq (mode-of-standard-midi-file-current-buffer) 'absTime))
      (progn
         (setq kind-of-shadow '(until-end-of-current-bar))
         (mark-shadow-of-current-event)
      )
      (message "The shadow until 'end of current bar' can only be used on Standard MIDI LAML Files in absTime mode. Nothing done.")))

(defun set-kind-of-shadow-none ()
  (interactive)
  (setq kind-of-shadow '(none))
  (highlight-this-line)
  (reset-shadow-overlay)
)

(defun message-about-current-shadow ()
  (interactive)
  (message
   (concat
    (cond ((eq (car kind-of-shadow) 'none)   "No shadow is used.")
          ((eq (car kind-of-shadow) 'until-end-of-current-bar) "The current shadow goes to the end of the current bar (only in absTime mode).")
          ((eq (car kind-of-shadow) 'number-of-quarter-notes)  (format "The current shadow is %2.2f quarter notes." (cadr kind-of-shadow)))
          ((eq (car kind-of-shadow) 'number-of-seconds)  (format "The current shadow is %3.2f seconds." (cadr kind-of-shadow)))
          ((eq (car kind-of-shadow) 'duration-of-current-event)   "The current shadow is the duration of the current MIDI LAML event.")
          (else   "Unknown kind of shadow. Should not happen."))
    " "
    "F10 toggles between kinds of shadow.")))

(defun brief-info-string-about-current-shadow ()
  (cond ((eq (car kind-of-shadow) 'none) "None")
        ((eq (car kind-of-shadow) 'until-end-of-current-bar) "End of bar")
        ((eq (car kind-of-shadow) 'number-of-quarter-notes) (format "%2.1f QN" (cadr kind-of-shadow)))
        ((eq (car kind-of-shadow) 'number-of-seconds) (format "%2.1f sec" (cadr kind-of-shadow)))
        ((eq (car kind-of-shadow) 'duration-of-current-event) "Duration")
        (else (error "Unknown kind of shadow. Should not happen."))))
