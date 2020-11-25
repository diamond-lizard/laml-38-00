; The MIDI LAML Chords Emacs mode
; (c) Kurt Nørmark, Department of Computer Science, Aalborg University.

(provide 'laml-chords-support)

(define-derived-mode 
	chords-mode laml-mode "Chords" 
  "A Chords LAML Emacs mode, which binds some keys (see below) to a useful LAML meaning. 
The Chords facility allows XML-in-LAML Scheme/Lisp markup of song text. In addition, the facilty
allows song transposition. 
The LAML Chords processor is a part of the LAML distribution.
LAML means 'Lisp Abstracted Markup Language'. LAML makes HTML and XML languages available as 
Scheme functions."

  (font-lock-mode t)

)

; Chords keymap:
(defvar chords-mode-map (make-sparse-keymap "Chords"))

;------------------------------------------------------------------------------------------------------------------
; Initial creation of chords file from raw song text. 

(defun make-chords ()
  "Given a buffer with a number of song lines, turn it into a LAML chords file."
  (interactive)

  (goto-char (point-min)) (insert CR)
  (surround-by-lines)

  (let ((the-song-text (buffer-substring (point-min) (point-max))))
    (erase-buffer)
    (laml-insert-template "chords")
     
    (search-forward "(song-verses" nil t)
    (forward-line 1)
    (insert the-song-text)
    (chords-mode)
  )

  (message "Now use the Emacs nest command to introduce verse forms around selected lines")

)


(defun surround-by-lines ()
 (goto-char (point-max)) (beginning-of-line)
 (while (not (= (point) (point-min)))
    (insert "    (line \"")
    (end-of-line 1)
    (insert "\")")
    (beginning-of-line)
    (if (not (= (point) (point-min)))
        (progn
          (previous-line 1) 
          (beginning-of-line 1)))))


; --------------------------------------------------------------------------------------------------------------------
; Specialized chord embedding Emacs commands. 

(defvar basic-chords '(c cs d eb e f fs g gs a hb h
                       cm csm dm ebm em fm fsm gm gsm am hbm hm
                       c7 cs7 d7 eb7 e7 f7 fs7 g7 gs7 a7 hb7 h7
                       c6 cs6 d6 eb6 e6 f6 fs6 g6 gs6 a6 hb6 h6
                       cm7 csm7 dm7 ebm7 em7 fm7 fsm7 gm7 gsm7 am7 hbm7 hm7))
; Additional notation
(defvar extra-chords '(db ds gb ab as
                       dbm dsm gbm abm asm
                       db7 ds7 gb7 ab7 as7
                       db6 ds6 gb6 ab7 as6
                       dbm7 dsm7 gbm7 abm7 asm7))

(defvar supported-chords (append basic-chords extra-chords (list 'unison)))

(defun embed-in-chord ()
  "A specialized embedding of a word phrase in a chord."
  (interactive)
  (let ((previous-embedding "")
        (embed-require-match t)
        (html-laml-embed-tag-list supported-chords)
        (nest-embed-attribute-insertion-map nil)
       )
    (call-interactively 'embed)))

(defun x-embed-in-chord ()
  "A specialized embedding of a word phrase in a chord. Adds an x attribute too."
  (interactive)
  (let* ((previous-embedding "")
         (embed-require-match t)
         (html-laml-embed-tag-list supported-chords)
         (nest-embed-attribute-insertion-map (mapcar (function (lambda (crd) (list crd 'x))) html-laml-embed-tag-list))
        )
    (call-interactively 'embed)))

(defun b-embed-in-chord ()
  "A specialized embedding of a word phrase in a chord. Adds an b attribute - bass note - too."
  (interactive)
  (let* ((previous-embedding "")
         (embed-require-match t)
         (html-laml-embed-tag-list supported-chords)
         (nest-embed-attribute-insertion-map (mapcar (function (lambda (crd) (list crd 'b))) html-laml-embed-tag-list))
        )
    (call-interactively 'embed)))

(defun x-b-embed-in-chord ()
  "A specialized embedding of a word phrase in a chord. Adds both an x - attribute abstraction - and a b attribute - bass note - too."
  (interactive)
  (let* ((previous-embedding "")
         (embed-require-match t)
         (html-laml-embed-tag-list supported-chords)
         (nest-embed-attribute-insertion-map (mapcar (function (lambda (crd) (list crd 'x 'b))) html-laml-embed-tag-list))
        )
    (call-interactively 'embed)))

(defun open-chords-html-documentation (doc-file)
  (start-process "Windows Application" nil (concat laml-dir "bin/call.bat") (concat laml-dir "styles/xml-in-laml/chords/man/" doc-file)))






(laml-define-key chords-mode-map [menu-bar chords]
    (cons "Chords" (make-sparse-keymap "Chords")))

(laml-define-key  chords-mode-map [menu-bar chords browse-manual]
      '("Browse the LAML Chords Reference Manual" . (lambda () (interactive) (open-chords-html-documentation "chords.html"))))

(laml-define-key chords-mode-map [menu-bar chords menu-chords-sep-1]
    '("----"))

(laml-define-key  chords-mode-map [menu-bar chords x-b-embed-in-chord]
      '("Embed in chord form with abstraction and bass note..." . x-b-embed-in-chord))

(laml-define-key  chords-mode-map [menu-bar chords b-embed-in-chord]
      '("Embed in chord form with bass note..." . b-embed-in-chord))

(laml-define-key  chords-mode-map [menu-bar chords x-embed-in-chord]
      '("Embed in chord form with abstraction..." . x-embed-in-chord))

(laml-define-key  chords-mode-map [menu-bar chords embed-in-chord]
      '("Embed in chord form..." . embed-in-chord))


(laml-define-key chords-mode-map [menu-bar chords menu-chords-sep-2]
    '("----"))

(laml-define-key  chords-mode-map [menu-bar chords make-chords]
      '("Initial chords markup" . make-chords))




(laml-define-key chords-mode-map (list "\C-x\C-e" "\C-c\C-x\C-e") 'embed-in-chord)
(laml-define-key chords-mode-map (list "\C-x\C-x" "\C-c\C-x\C-x") 'x-embed-in-chord)
(laml-define-key chords-mode-map (list "\C-x\C-b" "\C-c\C-x\C-b") 'b-embed-in-chord)
(laml-define-key chords-mode-map (list "\C-xB" "\C-c\C-xB") 'x-b-embed-in-chord)

