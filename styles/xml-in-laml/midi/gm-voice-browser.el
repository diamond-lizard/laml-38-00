; GENERAL MIDI VOICE BROWSER - modelled after the Tyros voice browser (by software copying...)
; Generation of cached voice samples: See templates/voice-phrases/gm-voice-generation.laml
; Main entry: gm-voice-browser

(defvar gm-midi-output-port nil "The output port of the Tyros - voice and arp browsing.")

(defvar gm-dir (concat midi-software-dir "data/gm/"))
(defun laml-temp-path () (concat laml-dir "temp/"))            ; NOT HERE!!!

(define-derived-mode gm-voice-browse-mode fundamental-mode "General MIDI browser"
  "A mode for browsing General MIDI voices"

  ; (kill-all-local-variables)

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
       (global-mode-string
        (#("--" 0 2
           (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
         global-mode-string))
       #("-%-" 0 3
         (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))))

)

; ---------------------------------------------------------------------------------------------------------------
; Key and menu bindings:

(defvar gm-voice-browse-mode-map (make-sparse-keymap "MidiPieces"))

(define-key gm-voice-browse-mode-map "p" (function play-gm-voice-octave))
(define-key gm-voice-browse-mode-map " " (function play-gm-voice-octave))
(define-key gm-voice-browse-mode-map "q" (function play-gm-voice-full-scale))
(define-key gm-voice-browse-mode-map "t" (function play-gm-voice-drums))
(define-key gm-voice-browse-mode-map "x" (function stop-midi-playing))  ; from midi.el

(define-key gm-voice-browse-mode-map "A" (function gm-add-voice-to-collection-ask-for-channel))
(define-key gm-voice-browse-mode-map "a" (function gm-add-voice-to-collection))

(define-key gm-voice-browse-mode-map [down] 'next-line-and-highlight-line)
(define-key gm-voice-browse-mode-map [up] 'previous-line-and-highlight-line)

; Contribution to Emacs tool menu: 
(laml-define-key global-map [menu-bar tools gm-voice-browser]
    (cons "GM Voice Browser" (function gm-voice-browser)))


(laml-define-key gm-voice-browse-mode-map [menu-bar gm-voice-browse menu-pieces-sep]
    '("----"))

(laml-define-key gm-voice-browse-mode-map [menu-bar gm-voice-browse]
    (cons "General MIDI Voices" (make-sparse-keymap "Gm-voices")))

(laml-define-key gm-voice-browse-mode-map [menu-bar gm-voice-browse gm-add-voice-to-collection-ask-for-channel]
    (cons "Add voice to collection..." (function gm-add-voice-to-collection-ask-for-channel)))

(laml-define-key gm-voice-browse-mode-map [menu-bar gm-voice-browse gm-add-voice-to-collection]
    (cons "Add voice to collection" (function gm-add-voice-to-collection)))

(laml-define-key gm-voice-browse-mode-map [menu-bar gm-voice-browse menu-pieces-sep-105]
    '("----"))

(laml-define-key gm-voice-browse-mode-map [menu-bar gm-voice-browse stop-midi-playing]
    (cons "Stop playing" (function stop-midi-playing)))

(laml-define-key gm-voice-browse-mode-map [menu-bar gm-voice-browse menu-pieces-sep-104]
    '("----"))

(laml-define-key gm-voice-browse-mode-map [menu-bar gm-voice-browse play-drums]
    (cons "Play Drum Sample" (function play-gm-voice-drums)))

(laml-define-key gm-voice-browse-mode-map [menu-bar gm-voice-browse play-full-scale]
    (cons "Play Full Scale" (function play-gm-voice-full-scale)))

(laml-define-key gm-voice-browse-mode-map [menu-bar gm-voice-browse play-octave]
    (cons "Play C4 to C5" (function play-gm-voice-octave)))

(laml-define-key gm-voice-browse-mode-map [menu-bar gm-voice-browse menu-pieces-sep-101]
  '("----"))

(laml-define-key  gm-voice-browse-mode-map [menu-bar gm-voice-browse new-voice-collection]   ; new-voice-collection from midi.el
      '("Empty voice collection browser" .  new-voice-collection))

(laml-define-key gm-voice-browse-mode-map [menu-bar gm-voice-browse gm-voice-revert-browser]
    (cons "Revert Browser" (function gm-voice-revert-browser)))


; ---------------------------------------------------------------------------------------------------------------

; Selectors:

(defun gm-voice-msb (entry)
  (nth 0 entry))

(defun gm-voice-lsb (entry)
  (nth 1 entry))

(defun gm-voice-pc (entry)
  (nth 2 entry))

(defun gm-voice-name (entry)
  (nth 3 entry))

(defun gm-voice-main-cat-1 (entry)
  (nth 4 entry))

; ---------------------------------------------------------------------------------------------------------------

; Presentation functions: Right justified, fixed width with space on left side.

(defun gm-voice-pres-msb (entry lgt)
 (let ((n (gm-voice-msb entry)))
  (pad-string-to-length lgt (as-string n) 'right)))

(defun gm-voice-pres-lsb (entry lgt)
 (let ((n (gm-voice-lsb entry)))
  (pad-string-to-length lgt (as-string n) 'right)))

(defun gm-voice-pres-pc (entry lgt)
 (let ((n (gm-voice-pc entry)))
  (pad-string-to-length lgt (as-string n) 'right)))

(defun gm-voice-pres-name (entry lgt)
  (pad-string-to-length lgt (gm-voice-name entry) 'right))

(defun gm-voice-pres-main-cat-1(entry lgt)
  (pad-string-to-length lgt (gm-voice-main-cat-1 entry) 'right))


(defun gm-voice-entry-number-of-line ()
 (save-excursion
   (beginning-of-line 1)
   (search-forward "#" nil t)
   (string-to-int (name-under-point))))


; ---------------------------------------------------------------------------------------------------------------

(defun present-gm-voice-entry (entry n)
  (let ((is-favorite nil))
   (concat 
     (if is-favorite "* " "  ")
     (pad-string-to-length 7 (concat "#" (as-string n)) 'right)

     (gm-voice-pres-msb entry 4)
     (gm-voice-pres-lsb entry 4)
     (gm-voice-pres-pc entry 4)

     (gm-voice-pres-name entry 35)

     (gm-voice-pres-main-cat-1 entry 25)
     )))

(defun present-gm-voice-header-line() 
   (concat 
    "  "
    (pad-string-to-length 7  "VOICE#" 'right)
    (pad-string-to-length 4 "MSB" 'right)
    (pad-string-to-length 4 "LSB" 'right)
    (pad-string-to-length 4 "PC" 'right)
    (pad-string-to-length 35 "VOICE NAME" 'right)
    (pad-string-to-length 25 "MAIN CATEGORY" 'right)
  ))

(defun present-gm-voice-list (gm-voice-list)
  (insert (present-gm-voice-header-line) CR)
  (mapcar2 (function (lambda (e n) (insert (concat (present-gm-voice-entry e n) CR))))
           gm-voice-list 
           (number-interval 1 (length gm-voice-list)) 
  )
)

(defun get-gm-voice-list ()
  (file-read (concat data-list-dir "general-midi-voices.dat")))

; Main entry to tyros voice browser.
(defun gm-voice-browser ()
  "Present a buffer of voice entries"
  (interactive)
  (if (get-buffer "Gm-voice-browser") (kill-buffer "Gm-voice-browser"))
  (let* ((buffer (get-buffer-create "Gm-voice-browser"))
        )
    (setq current-gm-voice-list (get-gm-voice-list))
    (set-buffer buffer) (erase-buffer)
    (set-window-buffer (selected-window) buffer)
    (gm-voice-browse-mode)
    (use-local-map gm-voice-browse-mode-map)
    (setq gm-midi-output-port (guess-output-port pc-sound-card-string-id))

    (present-gm-voice-list current-gm-voice-list)

    (toggle-read-only 1)  ; always read only
    (goto-char (point-min))
  )
)


; ---------------------------------------------------------------------------------------------------------------
; Voice list stack:

(defvar current-gm-voice-list nil "The current arpeggio list")

; ---------------------------------------------------------------------------------------------------------------

(defvar gm-voice-cache-directory (concat gm-dir "Midi-voice-phrases/") "The directory in which it is possible to chache voice midi files.")

(defun gm-play-midi-phrase-with-voice (msb lsb pc &optional variation)
 "Play some notes, determined by variation, with a voice characterized by msb, lsb, and pc.
Variation: octave, or full-scale. The default variation is octave."
 (let ((variation-1 (if variation variation "octave"))
      )
   
  ; Make midl file with set up of the given voice:
  (if (equal variation-1 "set")
      (let* ((channel (read-from-minibuffer "Set voice in channel: "))
             (buf
              (make-a-file-from-laml-template 
               "voice-sample.midl"
               (laml-temp-path)
               (concat "voice-sample-set") ; the template name
               'midi-laml-mode
               (list
                (list "*CH*" channel)
                (list "*MSB*" (as-string msb))
                (list "*LSB*" (as-string lsb))
                (list "*PC*" (as-string pc))
                )
               (concat laml-dir "styles/xml-in-laml/midi/" "templates/voice-phrases/")
               )))
        (kill-buffer buf))
      (let ((buf
             (make-a-file-from-laml-template 
              "voice-sample.midl"
              (laml-temp-path)
              (concat "voice-sample-" (as-string variation-1)) ; the template name
              'midi-laml-mode
              (list
               (list "*MSB*" (as-string msb))
               (list "*LSB*" (as-string lsb))
               (list "*PC*" (as-string pc))
               )
              (concat laml-dir "styles/xml-in-laml/midi/" "templates/voice-phrases/")
              )))
        (kill-buffer buf)))

  (laml-sync-process-file (concat (laml-temp-path) "voice-sample.midl") 'silent)  
  (play-midi-file (concat (laml-temp-path) "midi-temp/" "voice-sample.mid") gm-midi-output-port)
 )   
)


(defun gm-voice-midi-cache-path (msb lsb pc variation)
  (concat gm-voice-cache-directory variation "/" (as-string msb) "-" (as-string lsb) "-" (as-string pc) "." "mid"))

(defun gm-voice-play-selected-voice (variation)
  (let* ((number (gm-voice-entry-number-of-line))
         (full-entry (nth (- number 1) current-gm-voice-list))
         (msb (gm-voice-msb full-entry))
         (lsb (gm-voice-lsb full-entry))
         (pc (gm-voice-pc full-entry))
         (cached-voice-midi-path (gm-voice-midi-cache-path msb lsb pc variation)) 
        )
        
    (if (file-exists-p cached-voice-midi-path)
        (progn
          (play-midi-file cached-voice-midi-path gm-midi-output-port)
          (message "Cached")
        )
        (gm-play-midi-phrase-with-voice msb lsb pc variation)
    )
  )
)

(defun play-gm-voice-octave ()
  (interactive)
  (gm-voice-play-selected-voice "octave"))

(defun play-gm-voice-full-scale ()
  (interactive)
  (gm-voice-play-selected-voice "full-scale"))

(defun play-gm-voice-drums ()
  (interactive)
  (gm-voice-play-selected-voice "drums"))

(defun gm-voice-revert-browser ()
  (interactive)
  (gm-voice-browser))

(defvar gm-voice-browser-next-add-channel 1 "A global variable that points out the next channel where gm-add-voice-to-collection adds a voice in a voice collection")

(defun gm-add-voice-to-collection-ask-for-channel (channel)
  "Add the selected voice in the GM voice browser to the voice collection, as presented in the other window"
  (interactive "nAdd voice to collection. Channel (between 1 and 16): ")
  (let* ((number (gm-voice-entry-number-of-line))
         (full-entry (nth (- number 1) current-gm-voice-list))
         (msb (gm-voice-msb full-entry))
         (lsb (gm-voice-lsb full-entry))
         (pc (gm-voice-pc full-entry))
         (name (gm-voice-name full-entry))
         (voice-entry (make-voice-collection-entry msb lsb pc name))
        )
    (setq gm-voice-browser-next-add-channel (if (< channel 16) (+ channel 1) 1)) 
    (aset voice-collection-vector channel voice-entry)
    (other-window 1) ; the voice collection
    (re-present-voice-collection)
    (other-window 1)
    (message "The voice %s has been added to channel %d" name channel)))

(defun gm-add-voice-to-collection ()
  (interactive)
  (gm-add-voice-to-collection-ask-for-channel gm-voice-browser-next-add-channel)
)

(defun gm-voice-entry-number-of-line ()
 (save-excursion
   (beginning-of-line 1)
   (search-forward "#" nil t)
   (string-to-int (name-under-point))))