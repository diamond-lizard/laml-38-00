;;;; With the facilities programmed in this file it is possible to get access to LAML SchemeDoc information from Emacs.
;;;; This is extended with access to Scheme source file information and access to locally bound names in scheme definitions.
;;;; The interactive entry-level functions are display-schemedoc-information, reset-schemedoc-information and scheme-complete-symbol.
;;;; Relevant keybindings are found in the file emacs-support/laml-key-menu-bindings.el.

; SchemeDoc mode, backed by the file extension sdoc.

(define-derived-mode 
        schemedoc-mode laml-mode "SchemeDoc" 
  "A LAML Scheme Emacs mode mode, which binds some keys (see below) to a useful SchemeDoc functionality."

  (font-lock-mode t)

)

; ---------------------------------------------------------------------------------------------------------------
; SchemeDoc keybindings.

(laml-define-key schemedoc-mode-map [menu-bar schemedoc]
    (cons "SchemeDoc" (make-sparse-keymap "SchemeDoc")))

(laml-define-key  schemedoc-mode-map [menu-bar schemedoc insert-documentation-entry]
      '("Insert manual page" . (lambda () (interactive) (laml-insert-template "manual-page-xml-in-laml"))))

(laml-define-key schemedoc-mode-map [menu-bar schemedoc insert-documentation-section]
      '("Insert manual section" . (lambda () (interactive) (laml-insert-template "manual-section-xml-in-laml"))))


; ------------------------------------------------------------------------------------------------------------------
; SchemeDoc initialization - SchemeDoc support from interactive shell (comint).

(defun do-init-schemedoc ()
  (add-hook 'inferior-lisp-mode-hook 
   (function (lambda ()
      (define-schemedoc-commands-in-comint)
      )))
)

(defun define-schemedoc-commands-in-comint ()

  ; Define TAB, Shift TAB, and 1/2-key in inferior lisp mode:
  (define-key inferior-lisp-mode-map "\C-i" 'scheme-complete-symbol)
  (define-key inferior-lisp-mode-map (kbd "<backtab>") 'scheme-give-examples)  ; Shift TAB
  (define-key inferior-lisp-mode-map (kbd "½") 'scheme-complete-symbol-local-only)

  ; From within comint. Add entries to the SchemeDoc menu:
 
  (define-key inferior-lisp-mode-map [menu-bar schemedoc]
    (cons "SchemeDoc" (make-sparse-keymap "schemedoc")))

  (define-key inferior-lisp-mode-map [menu-bar schemedoc schemedoc-info-comint]
    '("Info about Schemedoc" . schemedoc-info-comint))

  (define-key inferior-lisp-mode-map [menu-bar schemedoc add-midi-laml-documentation]
    '("Add MIDI LAML Documentation" . add-midi-laml-documentation))

  (define-key inferior-lisp-mode-map [menu-bar schemedoc add-laml-documentation]
    '("Add LAML Documentation" . add-laml-documentation))

  (define-key inferior-lisp-mode-map [menu-bar schemedoc add-documentation-to-comint]
    '("Add Documentation..." . add-documentation-to-comint))

  (setq schemedoc-manlsp-file-list (init-schemedoc-file-list-from-current-scheme-source))

  (make-local-variable 'schemedoc-manlsp-file-list)
  (make-local-variable 'current-source-file-manlsp)
  (make-local-variable 'programmed-scheme-name-alist)
  (make-local-variable 'all-scheme-names-alist)
  (make-local-variable 'documented-scheme-names-alist)

)

(defun add-documentation-to-comint (man-file-path)
  "Add a documentation entry, in terms of a manual file path, to the documentation of comint"
  (interactive "fFull path to a manlsp file: ")

  (setq schemedoc-manlsp-file-list (cons man-file-path schemedoc-manlsp-file-list))
  (let ((documented-scheme-names (init-and-return-schemedoc-name-completion schemedoc-manlsp-file-list)))
    (setq documented-scheme-names-alist documented-scheme-names)
    (setq all-scheme-names-alist
           (sort (remove-duplicates-by-predicate
                    documented-scheme-names
                    (function (lambda (x y) (equal (car x) (car y)))))
                 (lambda (p1 p2) (or (equal (car p1) (car p2)) (string-lessp (car p1) (car p2))))))
    (message "Done")
  )

)


(defun add-documentation-list-to-comint (man-file-path-list)
  "Like add-documentation-to-comint, but adds a list of entriesi instead of just one"

  (setq schemedoc-manlsp-file-list (append man-file-path-list schemedoc-manlsp-file-list))
  (let ((documented-scheme-names (init-and-return-schemedoc-name-completion schemedoc-manlsp-file-list)))
    (setq documented-scheme-names-alist documented-scheme-names)
    (setq all-scheme-names-alist
           (sort (remove-duplicates-by-predicate
                    documented-scheme-names
                    (function (lambda (x y) (equal (car x) (car y)))))
                 (lambda (p1 p2) (or (equal (car p1) (car p2)) (string-lessp (car p1) (car p2))))))
    (message "Done")
  )
)
  


(defun add-laml-documentation ()
  (interactive)
  (add-documentation-list-to-comint (list (concat laml-dir "man/laml") 
					  (concat laml-dir "lib/man/general")
					  (concat laml-dir "lib/compatibility/man/compatibility")
					  (concat laml-dir "lib/xml-in-laml/man/xml-in-laml")
					  (concat laml-dir "lib/xml-in-laml/mirrors/man/xhtml10-transitional-mirror")
					  (concat laml-dir "lib/man/xhtml10-convenience")
					  (concat laml-dir "lib/man/color") (concat laml-dir "lib/man/time") 
					  (concat laml-dir "lib/man/file-read"))))

(defun add-midi-laml-documentation ()
  (interactive)
  (add-documentation-list-to-comint (list (concat laml-dir "r5rs/man/r5rs")
					  (concat laml-dir "man/laml")  (concat laml-dir "lib/man/general")
					  (concat laml-dir "lib/compatibility/man/compatibility")
					  (concat laml-dir "lib/xml-in-laml/man/xml-in-laml")
					  (concat laml-dir "styles/xml-in-laml/midi/man/midi-mirror")
					  (concat laml-dir "styles/xml-in-laml/midi/man/midi-laml-processing-lib")
					  )))

(defun schemedoc-info-comint ()
  (interactive)
    (schemedoc-manlsp-file-list-message))



; ------------------------------------------------------------------------------------------------------------------
; SchemeDoc functionality relevant on Scheme source files.


; Buffer local variables:
(defvar schemedoc-manlsp-file-list nil
  "A list of relative or absolute file names of manlsp files (without extension) to take into consideration when displaying
SchemeDoc help info. Relative files are relative to the directory of the current Scheme source file. Initialized
the first time you enquire about documentation.")

(defvar documented-scheme-names-alist nil 
  "An a-list of Scheme names for which there is access to SchemeDoc documentation. Maps to the number of the SchemeDoc file in schemedoc-manlsp-file-list")


(defvar schemedoc-preferred-linelength 80 "The preferred line length of text shown by LAML SchemeDoc")

; How to display SchemeDoc information.
(defvar schemedoc-display-mode 'help-buffer
  "How to display extracted SchemeDoc documentation in Emacs. Either help-buffer, tooltip, or nil") 

(defun display-schemedoc-information ()
  "Display SchemeDoc information of the name under point. This is the top level LAML SchemeDoc support function in Emacs.
Use of this function provides access to SchemeDoc manual information, as represented in -.manlsp files. The manlsp
files are generated by LAML SchemeDoc."
  (interactive)
   (let ((back-count 0))
    (if (null documented-scheme-names-alist) 
           (progn
              (message "First time use of SchemeDoc in this buffer. Initializing. Please wait...")
              (reset-schemedoc-information)
           ))
    (while (looking-at-white-space (point)) (backward-char 1) (setq back-count (+ back-count 1)))
    (let ((name-0 (prog-name-under-point-schemedoc)))
     (forward-char back-count)
     (let ((is-attribute (and (> (length name-0) 0) (= (aref name-0 0) ?'))))
        (if is-attribute
            (let* ((attribute-name-0 (substring name-0 1 (length name-0)))
                   (contextual-name (get-name-of-contextual-form))
                   (file-number (schemedoc-file-number-of-name contextual-name documented-scheme-names-alist))
                   (file-name (if file-number (file-path-of-file-number file-number) nil))
                   (doc-entry (find-schemedoc-documentation-of-name-given-number contextual-name file-number))
                  )
              (if doc-entry
                  (let* ((attributes-clause (assoc-get-cdr 'attributes doc-entry))
                         (all-attributes-alist (if attributes-clause (mapcar (function cdr) (cdr attributes-clause))))
                         (attribute-name-1 (completing-read (concat "Describe" " " contextual-name " " "attribute: ") all-attributes-alist nil nil attribute-name-0))
                        )
                    (cond ((eq schemedoc-display-mode 'help-buffer)
                           (schemedoc-help-buffer (format-schemedoc-entry-of-attribute doc-entry attribute-name-1)))
                          ((eq schemedoc-display-mode 'tooltip)
                           (tooltip-show (format-schemedoc-entry-of-attribute doc-entry attribute-name-1)))
                          ((eq schemedoc-display-mode 'minibuffer)
                           (message (format-schemedoc-entry-of-attribute doc-entry attribute-name-1)))
                          ((eq schemedoc-display-mode nil)
                           'nothing-showed)
                          (t (error "display-schemedoc-information. Unexpected value of schemedoc-display-mode"))))
                (beep))
            )
            (let* ((name-1 (if (assoc name-0 documented-scheme-names-alist) name-0 ""))
                   (name (completing-read "Describe defined Scheme name: " documented-scheme-names-alist nil nil name-1))
                   (file-number (schemedoc-file-number-of-name name documented-scheme-names-alist))
                   (file-name (if file-number (file-path-of-file-number file-number) nil))
                   (doc-entry (find-schemedoc-documentation-of-name-given-number name file-number))
                   )
              (if doc-entry
                  (cond ((eq schemedoc-display-mode 'help-buffer)
                         (schemedoc-help-buffer (format-schemedoc-entry doc-entry file-name)))
                        ((eq schemedoc-display-mode 'tooltip)
                         (tooltip-show (format-schemedoc-entry doc-entry file-name)))
                        ((eq schemedoc-display-mode 'minibuffer)
                         (message (format-schemedoc-entry doc-entry file-name)))
                        ((eq schemedoc-display-mode nil)
                         'nothing-showed)
                        (t (error "display-schemedoc-information. Unexpected value of schemedoc-display-mode")))
                (beep))))))))

(defun schemedoc-information (name name-alist kind)
  "Return the string with schemedoc information about name. Kind can be one of the symbols regular or examples-only."
  (let* ((file-number (schemedoc-file-number-of-name name name-alist))
         (file-name (if (and file-number (>= file-number 1)) (file-path-of-file-number file-number) nil))
         (doc-entry 
             (cond ((= file-number -1)  ; local name in current definition
                       (list (list 'source "Current definition") (list 'title name)))
                   ((= file-number 0)   ; name in source file
                      (find-schemedoc-documentation-of-name-in current-source-file-manlsp name))
                   (t  ; name from SchemeDoc documentation
                      (find-schemedoc-documentation-of-name-given-number name file-number)))
         )
        )
      (format-schemedoc-entry doc-entry file-name kind)))

(defun get-name-of-contextual-form ()
  (save-excursion
    (backward-up-list 1) (forward-char 1) 
    (while (looking-at-white-space (point)) (forward-char 1))
    (prog-name-under-point-schemedoc)))
    

(defun find-schemedoc-documentation-of-name-in (manlsp-lst name)
  (linear-search manlsp-lst
     (function 
      (lambda (el) 
        (equal (assoc-get 'title el) name)))
     (function (lambda (x) x))))


(defun prog-name-under-point-schemedoc ()
  "As prog-name-under-point, but can only return a proper name. 
In case of problems, return nil."
  (cond ((looking-at "'") "'")
        ((looking-at-white-space (point)) nil)
        ((looking-at ";") nil)
        ((or (looking-at "(") (looking-at ")")) nil)
        (t (name-under-point))))

(defun reset-schemedoc-information ()
  "Re-read the schemedoc files to take into account from the current Scheme source file.
Also re-read the LAML SchemeDoc information from the internal SchemeDoc files pointed out by schemedoc-manlsp-file-list.
Do NOT initialize names from the local source file."
  (interactive)
  (init-schemedoc-file-list-from-current-scheme-source)
  (init-schemedoc-name-completion)
  (schemedoc-manlsp-file-list-message)
)

(defun schemedoc-manlsp-file-list-message ()
  (message
    (concat 
      "The following SchemeDoc resources are effective: "
      (list-to-string-with-sep
        (mapcar (function (lambda (f) (file-name-proper (file-name-nondirectory f)))) schemedoc-manlsp-file-list) ", "))))

(defun info-emacs-schemedoc ()
  (interactive)
  (if (null schemedoc-manlsp-file-list)
      (progn
        (init-schemedoc-file-list-from-current-scheme-source)
        (init-schemedoc-name-completion)
        (schemedoc-manlsp-file-list-message))
      (schemedoc-manlsp-file-list-message)))
         
  

(defun init-schemedoc-file-list-from-current-scheme-source ()
  (make-variable-buffer-local 'schemedoc-manlsp-file-list)
  (let ((lst (schemedoc-file-list-from-current-scheme-source)))
    (setq schemedoc-manlsp-file-list 
          (filter (function (lambda (el) (not (null el)))) lst))))

(defun schemedoc-file-list-from-current-scheme-source ()
 "Return the current buffers SchemeDoc file list (the list of manlsp files from where to extract SchemeDoc information).
Depending on the major/minor modes of the buffer, a fixed set of manuals are taken into account.
In addition, the special tag  .schemedoc-dependencies  can be used to enumerate additional manuals.
Place, for instance, the  .schemedoc-dependencies  tag in the introductory comment of the Scheme source file." 
 (let ((default-schemedoc-list
          (cond ((and (eq major-mode 'laml-mode) (boundp 'elucidator-buffer-info) (eq elucidator-buffer-info 'setup))
                                                  (list (concat laml-dir "r5rs/man/r5rs") 
                                                        (concat laml-dir "man/laml")  (concat laml-dir "lib/man/general")
                                                        (concat laml-dir "lib/compatibility/man/compatibility")
                                                        (concat laml-dir "styles/xml-in-laml/elucidator-2/man/elucidator")
                                                 ))
                ((eq major-mode 'schemedoc-mode)  (list (concat laml-dir "r5rs/man/r5rs")
                                                        (concat laml-dir "man/laml")  (concat laml-dir "lib/man/general") 
                                                        (concat laml-dir "lib/compatibility/man/compatibility")
                                                        (concat laml-dir "styles/xml-in-laml/schemedoc-2/man/schemedoc")
                                                        (concat laml-dir "styles/xml-in-laml/schemedoc-index/man/schemedoc-index")
                                                        (concat laml-dir "lib/xml-in-laml/mirrors/man/xhtml10-transitional-mirror")
                                                  ))

                ((eq major-mode 'scheme-mode) (list (concat laml-dir "r5rs/man/r5rs") 
                                                    (concat laml-dir "man/laml") 
                                                    (concat laml-dir "lib/xml-in-laml/man/xml-in-laml")
                                                    (concat laml-dir "lib/xml-in-laml/mirrors/man/xhtml10-transitional-mirror")
                                                    (concat laml-dir "lib/man/xhtml10-convenience")
                                                    (concat laml-dir "lib/man/general")
                                                    (concat laml-dir "lib/compatibility/man/compatibility")
                                                    (concat laml-dir "lib/man/color") (concat laml-dir "lib/man/time") 
                                                    (concat laml-dir "lib/man/file-read")
                                              ))

                ((eq major-mode 'leno-mode)   (list (concat laml-dir "r5rs/man/r5rs")
                                                    (concat laml-dir "man/laml")  (concat laml-dir "lib/man/general")
                                                    (concat laml-dir "lib/compatibility/man/compatibility")
                                                    (concat laml-dir "styles/xml-in-laml/lecture-notes/man/lecture-notes")
                                                    (concat laml-dir "styles/xml-in-laml/lecture-notes-themes/man/lecture-notes-themes")
                                                    (concat laml-dir "lib/xml-in-laml/mirrors/man/xhtml10-transitional-mirror")
                                                    (concat laml-dir "lib/xml-in-laml/mirrors/man/svg11-mirror")
                                                    (concat laml-dir "lib/man/color") (concat laml-dir "lib/man/time")
                                                    (concat laml-dir "lib/man/svg-extensions") 
                                                    (concat laml-dir "lib/man/file-read")
                                              ))

                ((eq major-mode 'chords-mode) (list (concat laml-dir "r5rs/man/r5rs")
                                                    (concat laml-dir "man/laml")  (concat laml-dir "lib/man/general")
                                                    (concat laml-dir "lib/compatibility/man/compatibility")
                                                    (concat laml-dir "styles/xml-in-laml/chords/man/chords")
                                              ))

                ((eq major-mode 'midi-laml-mode) (list 
                                                    (concat laml-dir "r5rs/man/r5rs")
                                                    (concat laml-dir "man/laml")  (concat laml-dir "lib/man/general")
                                                    (concat laml-dir "lib/compatibility/man/compatibility")
                                                    (concat laml-dir "lib/xml-in-laml/man/xml-in-laml")
                                                    (concat laml-dir "styles/xml-in-laml/midi/man/midi-mirror")
                                                    (concat laml-dir "styles/xml-in-laml/midi/man/midi-laml-processing-lib")
                                              ))

                ((eq major-mode 'laml-mode)   (list (concat laml-dir "r5rs/man/r5rs")
                                                    (concat laml-dir "man/laml") 
                                                    (concat laml-dir "lib/man/general")
                                                    (concat laml-dir "lib/compatibility/man/compatibility")
                                                    (concat laml-dir "lib/xml-in-laml/man/xml-in-laml")
                                                    (concat laml-dir "lib/xml-in-laml/mirrors/man/xhtml10-transitional-mirror")
                                                    (concat laml-dir "lib/man/xhtml10-convenience")
                                                    (concat laml-dir "lib/man/color") (concat laml-dir "lib/man/time") 
                                                    (concat laml-dir "lib/man/file-read")))

                ((eq major-mode 'inferior-lisp-mode)                                       ; Scheme command prompt
                                              (list (concat laml-dir "r5rs/man/r5rs")
                                              ))
                (t nil))))
  (save-excursion
    (goto-char (point-min))
    (let ((search-res (search-forward ".schemedoc-dependencies" nil t)))
      (append
        (if search-res
            (mapcar (function normalize-schemedoc-dependency-file-path) (read-strings-from-scheme-source))
          nil)
        default-schemedoc-list)))))

(defun read-strings-from-scheme-source()
  "Return the list of file paths, as given in quoted strings on the current line. Stops when a newline is encountered."
  (while (not (or (= (char-under-point) 34) (= (char-under-point) 10))) (forward-char 1))
  (if (= (char-under-point) 10)
      nil
      (let ((p1 (point))
            (str nil))
        (forward-sexp 1)
        (cons (buffer-substring-no-properties (+ p1 1) (- (point) 1))
              (read-strings-from-scheme-source)))))

(defun normalize-schemedoc-dependency-file-path (file-path)
  "Normalize by eliminating tilde. Expand a possible -/ laml-dir prefix. Return relative or absolute path"
  (let* ((file-path-1 (eliminate-tilde-prefix file-path))
         (file-path-2 (expand-laml-prefix file-path-1))   ; the -/ prefix
         (path (file-name-directory file-path-2))
         (prop (file-name-proper (file-name-nondirectory file-path-2))))
   (concat path prop)))

(defun expand-laml-prefix (path)
  "If path is initiated with a '-/' prefix, and if the variable laml-dir is bound, substitute '-/' with the value of laml-dir 
and return the expanded path.  Else just return path."
  (if (and (>= (length path) 2) (= ?- (aref path 0)) (= ?/ (aref path 1)) (boundp 'laml-dir))   ; -/ path prefix
      (concat laml-dir (substring path 2 (length path)))
      path))

(defun normalize-source-file-dependency-file-path (file-path)
  "Normalize by eliminating tilde. Expand a possible -/ laml-dir prefix. Return relative or absolute path"
  (let* ((file-path-1 (eliminate-tilde-prefix file-path))
         (file-path-2 (expand-laml-prefix file-path-1))   ; the -/ prefix
        )
    file-path-2))

 
(defun init-schemedoc-name-completion ()
  "Initialize documented-scheme-names-alist from the internal SchemeDoc manlsp files pointed out by the list schemedoc-manlsp-file-list"
  (make-variable-buffer-local 'documented-scheme-names-alist)
  (message "Setting up SchemeDoc support...")
  (let* ((manlsp-list-list (mapcar (function file-read-schemedoc) schemedoc-manlsp-file-list))
         (documented-scheme-names-list (mapcar (function (lambda (manlsp-list) (mapcar (function (lambda (manlsp-entry) (assoc-get 'title manlsp-entry))) manlsp-list))) manlsp-list-list))
         (filtered-documented-scheme-names-list (mapcar (function (lambda (name-list) (filter (function (lambda (e) e)) name-list))) documented-scheme-names-list))
         (filtered-documented-scheme-names-alist  (mapcar2 (function (lambda (name-list n) (mapcar (function (lambda (name) (cons name n))) name-list))) filtered-documented-scheme-names-list (number-interval 1 (length schemedoc-manlsp-file-list)))))

    (setq documented-scheme-names-alist
          (sort (apply (function append) filtered-documented-scheme-names-alist)
                (lambda (p1 p2) (or (equal (car p1) (car p2)) (string-lessp (car p1) (car p2))))))))

(defun init-and-return-schemedoc-name-completion (schemedoc-manlsp-file-list)
  "Return the documented name alist from the internal SchemeDoc manlsp files pointed out by the list schemedoc-manlsp-file-list"
  (make-variable-buffer-local 'documented-scheme-names-alist)
  (message "Setting up SchemeDoc support...")
  (let* ((manlsp-list-list (mapcar (function file-read-schemedoc) schemedoc-manlsp-file-list))
         (documented-scheme-names-list (mapcar (function (lambda (manlsp-list) (mapcar (function (lambda (manlsp-entry) (assoc-get 'title manlsp-entry))) manlsp-list))) manlsp-list-list))
         (filtered-documented-scheme-names-list (mapcar (function (lambda (name-list) (filter (function (lambda (e) e)) name-list))) documented-scheme-names-list))
         (filtered-documented-scheme-names-alist  (mapcar2 (function (lambda (name-list n) (mapcar (function (lambda (name) (cons name n))) name-list))) filtered-documented-scheme-names-list (number-interval 1 (length schemedoc-manlsp-file-list)))))

    (sort (apply (function append) filtered-documented-scheme-names-alist)
                (lambda (p1 p2) (or (equal (car p1) (car p2)) (string-lessp (car p1) (car p2)))))))



(defun schemedoc-help-buffer (txt)
  (let ((help-buffer (if (get-buffer "*Help*") (get-buffer "*Help*") (generate-new-buffer "*Help*"))))
    (delete-other-windows)
    (split-window)
    (show-buffer (other-window 1) help-buffer)
    (set-buffer help-buffer)
    (toggle-read-only -1)
    (erase-buffer)
    (insert txt) (goto-char (point-min))
    (toggle-read-only 1)
    (set-buffer-modified-p nil)
    (other-window 1)))


; kind is either the symbol regular or examples-only.
; kind regular gives general information about everything apart from examples. 
; kind examples-only give only information about examples.
(defun format-schemedoc-entry (doc-entry informative-file-path &optional kind0)
 (let ((kind (if (null kind0) 'regular kind0)))
  (cond ((eq kind 'regular)
           (let* ((source (assoc-get 'source doc-entry))
                  (form (assoc-get 'form doc-entry))
                  (title (assoc-get 'title doc-entry))
                  (descr (assoc-get 'description doc-entry))
                  (returns (assoc-get 'returns doc-entry))
                  (parameters (assoc-get-cdr 'parameters doc-entry))
                  (attributes (assoc-get-cdr 'attributes doc-entry))
                  (xml-in-laml-attributes (assoc-get-cdr 'xml-in-laml-attributes doc-entry)) ; attributes of XML-in-LAML abstractions
                  (attribute-descriptions (assoc-get-cdr 'attribute-descriptions doc-entry))
                  (examples (assoc-get-cdr 'examples doc-entry))
                  (ex-count-descr (if examples 
                                      (if (> (length examples) 1)
                                          (concat "There are " (as-string (length examples)) " examples available. " schemedoc-how-to-get-examples )
                                          (concat "There is one example available. " schemedoc-how-to-get-examples)) 
                                    nil))
                  (content-model (assoc-get 'content-model doc-entry))
                  (laml-resource (assoc-get 'laml-resource doc-entry))
                  (scheme-source (assoc-get 'scheme-source-file doc-entry))
                  (misc (assoc-get 'misc doc-entry))
                  )
             (concat 
                     (cond (form 
                            (concat 
                             (cond ((stringp form) form)
                                   (t (prin1-to-string form)))
                             CR CR))
                           (title (concat title CR))
                           (t ""))
                     (if (and descr ex-count-descr) (concat (concat (break-lines descr 0) CR ex-count-descr) CR CR) "")
                     (if (and (not descr) ex-count-descr) (concat ex-count-descr CR CR) "")
                     (if (and descr (not ex-count-descr)) (concat (break-lines descr 0) CR CR) "")
                     (if content-model (concat "CONTENT MODEL FROM DTD: " content-model CR CR) "")
                     (if parameters (concat "PARAMETERS:" CR (format-parameters parameters) CR CR) "")
                     (if attribute-descriptions (concat "ATTRIBUTES:" CR (format-attribute-descriptions attribute-descriptions) CR CR) "")
                     (if xml-in-laml-attributes (concat "XML-IN-LAML ATTRIBUTES:" CR (format-xml-in-laml-attributes xml-in-laml-attributes) CR CR) "")
                     (if attributes (concat "RAW ATTRIBUTES FROM DTD:" CR (format-attributes attributes) CR CR) "")
                     (if returns (concat "RETURNS:" CR (break-lines returns 0) CR CR) "")

                     (if source (concat "ORIGIN: " source CR) "")
                     (cond 
                      ((and laml-resource (equal laml-resource "true") scheme-source) (concat "LAML Scheme source file: " scheme-source))
                      (scheme-source (concat "Scheme source file: " scheme-source))
                    ; (informative-file-path (concat "Location of documentation: " informative-file-path CR))
                      (t ""))

                   ; (if examples (concat "EXAMPLES:" CR  (format-examples examples) CR CR) "")
                     (if misc (concat "MISCELLANEOUS:" " " (break-lines misc 0) CR CR) "")

                     )))

        ((eq kind 'examples-only)
           (let* ((examples (assoc-get-cdr 'examples doc-entry))
                  )
             (concat (if examples (format-examples examples) "NO EXAMPLES to show")
                     )))

        (t "Should not happen!!!"))))

(defun format-schemedoc-entry-of-attribute (doc-entry attribute-name)
 (let* ((attributes (assoc-get-cdr 'attributes doc-entry))
        (attribute (linear-search attributes 
                      (function (lambda (e) (equal (cadr e) attribute-name))) (function (lambda (x) x)))) 
        (xml-in-laml-attributes (assoc-get-cdr 'xml-in-laml-attributes doc-entry))   ; attributes of XML-in-LAML abstractions
        (xml-in-laml-attribute (linear-search xml-in-laml-attributes 
                      (function (lambda (e) (equal (cadr e) attribute-name))) (function (lambda (x) x))))
        (attribute-descriptions (assoc-get-cdr 'attribute-descriptions doc-entry))
        (attribute-description (linear-search attribute-descriptions 
                      (function (lambda (e) (equal (cadr e) attribute-name))) (function (lambda (x) x))))
       )
    (concat 
            (if attribute (concat  (format-one-attribute-1 attribute) CR) "")
            (if xml-in-laml-attribute (concat  (format-one-attribute-1 xml-in-laml-attribute) CR) "")
            (if attribute-description (concat  (format-one-attribute-description-1 attribute-description)) "")
    )))

(defun make-lines-of-string (str)
  "Return a list of lines, each approximately  schemedoc-preferred-linelength  chars long"
  (make-lines-of-string-1 str (length str)))

(defun make-lines-of-string-1 (str str-lgt)
  (let ((i (min (max (- schemedoc-preferred-linelength 4) 0) str-lgt)))
    (while (and (< i str-lgt) (not (member (aref str i) white-space-char-list))) (setq i (+ i 1)))
    (cond ((= i str-lgt) (list str))
          ((and (< i str-lgt) (member (aref str i) white-space-char-list)) 
              (cons 
                (substring str 0 i)
                (let ((rest-string  (substring str (+ i 1))))
                  (make-lines-of-string-1 rest-string (length rest-string)))))
          (t (error "make-lines-of-string: Should not happen" str str-lgt i)))))


(defun format-parameters (parameter-list)
  (apply (function concat) (mapcar (function format-one-parameter) parameter-list)))

(defun format-examples (examples)
  (apply (function concat) (mapcar (function format-one-example) examples)))

(defun format-one-example (ex)
  (concat (cadr ex) CR CR))

(defun format-one-parameter (par)
  (concat " - " (cadr par) ":  " (break-lines (caddr par) 3) CR))

(defun format-attribute-descriptions (attribute-descriptions)
  (apply (function concat) (mapcar (function format-one-attribute-description) attribute-descriptions)))

(defun format-xml-in-laml-attributes (attribute-descriptions)
  (apply (function concat) (mapcar (function format-one-attribute) attribute-descriptions)))

(defun format-one-attribute-description (attribute-description)
  (concat " - " (cadr attribute-description) ":  " (break-lines (caddr attribute-description) 3) CR))

(defun format-one-attribute-description-1 (attribute-description)
  (concat 
;   (cadr attribute-description) ":  "
   (break-lines (caddr attribute-description) 0) CR))

(defun format-attributes (attributes)
  (apply (function concat) (mapcar (function format-one-attribute) attributes)))

(defun format-one-attribute (attribute)
  (concat " - " (cadr attribute) ":  " (prin1-to-string (caddr attribute)) "  " (car (cdddr attribute)) CR))

(defun format-one-attribute-1 (attribute)
  (concat (cadr attribute) ": " CR (break-lines (prin1-to-string (caddr attribute)) 0) CR))



(defun break-lines (str indent) 
 "Break the string str into a number of lines. Indent lines after CR with indent spaces. Return the line-broken string."
 (let* ((line-list (make-lines-of-string str))
        (line-list-lgt (length line-list))
        (indent-string (make-string indent 32)))
  (cond ((= 0 line-list-lgt) "")
        ((= 1 line-list-lgt) (car line-list))
        (t (let ((last-line (car (last line-list)))
                 (but-last-lines (butlast line-list)))
             (concat 
              (apply (function concat) (mapcar (function (lambda (ln) (concat ln CR indent-string))) but-last-lines))
              last-line))))))

            
(defun schemedoc-file-number-of-name (name name-alist)
  "Return the number of the file where name is documented. Return nil if name is not found."
  (cdr (assoc name name-alist)))

(defun file-path-of-file-number (n)
  "Return the file path of file number n i schemedoc-manlsp-file-list. The first file is given the number 1."
  (nth (- n 1) schemedoc-manlsp-file-list))  
  

(defun find-schemedoc-documentation-of-name-given-number (name file-number)
  "Return the appropriate association list from an internal manlsp SchemeDoc file describing name.
name is the name of a top-level entry documentation entry (typically a function). The information is found in file-number, relative to the list documented-scheme-names-alist."
  (if file-number 
      (let* ((manlsp-file-path (nth (- file-number 1) schemedoc-manlsp-file-list))
             (manlsp-structure (file-read-schemedoc manlsp-file-path))
             (manlsp-structure-header nil) ; (car manlsp-structure)  ; We cannot yet read the header part due to occurrences of #t
             (scheme-source nil)  ; (assoc-get 'scheme-source-file manlsp-structure-header)
             (laml-resource nil)  ; (assoc-get 'laml-resource manlsp-structure-header)
             (res (linear-search 
                   manlsp-structure  ; does not look in header part.
                   (function 
                    (lambda (el) 
                      (equal (assoc-get 'title el) name)))
                   (function (lambda (x) x)))))
        (if res
            (cons (list 'laml-resource laml-resource) 
                  (cons (list 'scheme-source-file (scheme-source-file-path scheme-source laml-resource))
                        (cons (list 'source (file-name-nondirectory manlsp-file-path))
                              res)))
          nil))
    nil))

(defun scheme-source-file-path (scheme-source laml-resource)
  (cond ((and scheme-source (equal laml-resource "true"))
            (substring scheme-source (length laml-dir)))
        (scheme-source scheme-source)
        (t nil))) 

(defun find-schemedoc-documentation-of-name (name)
  "Return the approriate association list from an internal manlsp SchemeDoc file describing name."
  (let ((file-number (schemedoc-file-number-of-name name)))  ; the number of the file in schemedoc-manlsp-file-list where to look for documentation
    (find-schemedoc-documentation-of-name-given-number name file-number)))


; ---------------------------------------------------------------------------------------------------------------
; COMPLETION AND TOOLTIP STUFF:

(defvar all-scheme-names-alist '() "All names - a trivial a-list - with Scheme names for completion.")

(defun reset-scheme-completion ()
  "Reset name completion information both from SchemeDoc and from the current source file." 
  (interactive)
  (reset-schemedoc-information)
  (let ((other-count (init-name-completion-current-source-file))) ; assigns current-source-file-manlsp and programmed-scheme-name-alist
    (setq all-scheme-names-alist
           (sort (remove-duplicates-by-predicate
                    (append documented-scheme-names-alist programmed-scheme-name-alist)
                    (function (lambda (x y) (equal (car x) (car y)))))
                 (lambda (p1 p2) (or (equal (car p1) (car p2)) (string-lessp (car p1) (car p2))))))
    (make-variable-buffer-local 'current-source-file-manlsp)
    (make-variable-buffer-local 'programmed-scheme-name-alist)
    (make-variable-buffer-local 'all-scheme-names-alist)

    (message "DONE.")

    (cond ((> other-count 1)
             (message "DONE. The names in the current source file and %s other source files can now be used for completion purposes." other-count))
          ((= other-count 1)
             (message "DONE. The names in the current source file and another source file can now be used for completion purposes." other-count))
          (t (message "DONE. The names in the current source file can now be used for completion purposes."))) ))

(defun reset-scheme-completion-from-source-file ()
  "Reset name completion information from the current source file, but not from SchemeDoc" 
  (interactive)
  (let ((other-count (init-name-completion-current-source-file))) ; assigns current-source-file-manlsp and programmed-scheme-name-alist
    (setq all-scheme-names-alist
          (sort (remove-duplicates-by-predicate
                 (append documented-scheme-names-alist programmed-scheme-name-alist)
                 (function (lambda (x y) (equal (car x) (car y)))))
                (lambda (p1 p2) (or (equal (car p1) (car p2)) (string-lessp (car p1) (car p2))))))
    (make-variable-buffer-local 'current-source-file-manlsp)
    (make-variable-buffer-local 'programmed-scheme-name-alist)
    (make-variable-buffer-local 'all-scheme-names-alist)

    (cond ((> other-count 1)
             (message "DONE. The names in the current source file and %s other source files can now be used for completion purposes." other-count))
          ((= other-count 1)
             (message "DONE. The names in the current source file and another source file can now be used for completion purposes." other-count))
          (t (message "DONE. The names in the current source file can now be used for completion purposes.")))))

; It is recommended to bind this command to the TAB key
; KN: Bound in emacs-support/laml-key-menu-bindings.el
(defun scheme-complete-symbol ()
  "Show tooltip completion information of the word under point, using both local and global names"
  (interactive)
  (if (null documented-scheme-names-alist)
      (progn
        (message "First time use of SchemeDoc in this buffer. Initializing. Please wait...")
        (reset-scheme-completion)  ; both SchemeDoc and name from source file(s) 
      ))

  (let ((schemedoc-preferred-linelength 55))               ; DYNAMIC BINDING of schemedoc-preferred-linelength
    (cond ((point-in-context-of-attribute) (scheme-complete-attribute-symbol))  ; show-scheme-attribute-information
          (t                               (scheme-complete-symbol-with-mode 'all)))))

; It is recommended to bind this command to the Shift-TAB key
; KN: Bound in emacs-support/laml-key-menu-bindings.el
(defun scheme-give-examples ()
  "Show tooltip examples - via SchemeDoc information - for word under point"
  (interactive)
  (if (null documented-scheme-names-alist)
      (progn
        (message "First time use of SchemeDoc in this buffer. Initializing. Please wait...")
        (reset-scheme-completion)  ; both SchemeDoc and name from source file(s) 
      ))

  (let ((schemedoc-preferred-linelength 85))               ; DYNAMIC BINDING of schemedoc-preferred-linelength
    (schemedoc-examples)))

; Not used. Now integrated in scheme-complete-attribute-symbol.
(defun show-scheme-attribute-information ()
  (let ((back-count 0))
    (while (looking-at-white-space (point)) (backward-char 1) (setq back-count (+ back-count 1)))
    (let* ((attribute-name-0 (prog-name-under-point-schemedoc))
           (attribute-name-1 (substring attribute-name-0 1 (length attribute-name-0)))
           (contextual-name (get-name-of-contextual-form))
           (file-number (schemedoc-file-number-of-name contextual-name documented-scheme-names-alist))
           (file-name (if file-number (file-path-of-file-number file-number) nil))
           (doc-entry (find-schemedoc-documentation-of-name-given-number contextual-name file-number))
          )
      (forward-char back-count)
      (show-tool-tip-text (format-schemedoc-entry-of-attribute doc-entry attribute-name-1)))))

(defun scheme-complete-attribute-symbol ()
 (let ((back-count 0))
   ; (while (looking-at-white-space (point)) (backward-char 1) (setq back-count (+ back-count 1)))
   (let* ((contextual-name (get-name-of-contextual-form))
          (file-number (schemedoc-file-number-of-name contextual-name documented-scheme-names-alist))
          (file-name (if file-number (file-path-of-file-number file-number) nil))
          (doc-entry (find-schemedoc-documentation-of-name-given-number contextual-name file-number))
          (attributes (assoc-get-cdr 'attributes doc-entry))
          (xml-in-laml-attributes (assoc-get-cdr 'xml-in-laml-attributes doc-entry))
          (end (point))
          (beg (save-excursion 
                 (while (not (looking-at "'")) (backward-char 1)) (+ (point) 1)))
          (pattern (buffer-substring-no-properties beg end))
          (all-attribute-names-alist (mapcar  (function cdr) (append attributes xml-in-laml-attributes)))
          (completions (sort (all-completions pattern all-attribute-names-alist) 
                             (function (lambda (s1 s2) (or (equal s1 s2) (string-lessp s1 s2))))))
         )

       (cond 
             ((and (= (length completions) 1) (equal (car completions) pattern))   ; first completion is a total match. 
                                                                                   ; Show detailed schemedoc information.
                 (show-tool-tip-text (format-schemedoc-entry-of-attribute doc-entry pattern))
             )
           ((and (> (length completions) 1) (equal (car completions) pattern))    ; first completion is a total match. 
                                                                                  ; Show detailed schemedoc information and hints about other mathces
                 (show-tool-tip-text
                    (concat (short-info-about-completions (cdr completions) pattern) CR CR
                            (format-schemedoc-entry-of-attribute doc-entry pattern)))
             )
           ((= 1 (length completions))                                            ; only one possible completion. Do the completion.
               (progn
                 (kill-region beg end)
                 (insert (car completions)))
             )
           (t (let ((longest-common-substring                                     ; Several completions. 
                        (try-completion pattern all-attribute-names-alist)))   ; Complete as much as possible and show list.
                   (if (> (length longest-common-substring) (length pattern))
                       (progn
                            (kill-region beg end)
                            (insert longest-common-substring)
                            (let* ((extended-pattern longest-common-substring)
                                   (new-completions (all-completions extended-pattern all-attribute-names-alist)))
                              (show-tool-tip-text (list-to-string-with-sep new-completions CR))))
                       (progn    
                         (if (= (length completions) 0) (beep))
                         (show-tool-tip-text (list-to-string-with-sep completions CR))))))))))


(defun point-in-context-of-attribute()
  (save-excursion
    (if (= ?' (char-under-point -1))
        t
        (progn
          (backward-sexp 1)
          (looking-at "'")))))

 
(defun scheme-complete-symbol-local-only ()
  "Show tooltip completion information of the word under point, using only local names"
  (interactive)
  (scheme-complete-symbol-with-mode 'local))

; State owned and managed by scheme-complete-symbol-with-mode:
(defvar completion-point nil "The value of point upon the last successful completion/info showing of scheme-complete-symbol-with-mode")

; The parameter mode can be all (meaning all names) or local (only local names).
(defun scheme-complete-symbol-with-mode (mode)
  "Complete the word under point, and show tooltip completion information of the word under point when it is complete"
   (if (and completion-point (= (point) completion-point))      ; Activated on same position twice - bring up help on the context.
       (condition-case nil
         (let* ((pattern (get-name-of-contextual-form))
                (all-scheme-names-alist-final all-scheme-names-alist))
           (show-tool-tip-text (schemedoc-information pattern all-scheme-names-alist-final 'regular))
           (setq completion-point nil)
           )
         (error (progn (message "Ups...") (setq completion-point nil)))) 

       (let* ((end (point))
              (beg (with-syntax-table (if (boundp 'scheme-mode-syntax-table) scheme-mode-syntax-table (syntax-table))
                     (save-excursion
                       (backward-sexp 1)
                       (while (= (char-syntax (following-char)) ?\')
                         (forward-char 1))
                       (point))))
              (pattern (buffer-substring-no-properties beg end))
              (all-scheme-names-alist-final 
               (cond ((eq mode 'all)
                      (append (trivial-alist (bound-names-until-point) -1) all-scheme-names-alist))
                                        ; append local names (marked with -1) with all other names (marked 0 or higher).
                     ((eq mode 'local) 
                      (trivial-alist (bound-names-until-point) -1))
                     (else (error "scheme-complete-symbol-with-mode: Unknown mode: %s" mode))))
              (completions (all-completions pattern all-scheme-names-alist-final))
              )
         (cond 
          ((and (= (length completions) 1) (equal (car completions) pattern)) ; First completion is a total match. 
                                                                              ; Show detailed schemedoc information.
           (setq completion-point (point))
           (show-tool-tip-text (schemedoc-information pattern all-scheme-names-alist-final 'regular))
           )
          ((and (> (length completions) 1) (equal (car completions) pattern)) ; First completion is a total match. 
                                                                              ; Show detailed schemedoc information and hints about other matches.
           (setq completion-point (point))
           (show-tool-tip-text
            (concat (short-info-about-completions (cdr completions) pattern) CR CR
                    (schemedoc-information pattern all-scheme-names-alist-final 'regular)))
           )
          ((= 1 (length completions)) ; only one possible completion. Do the completion.
           (progn
             (kill-region beg end)
             (insert (car completions))
             (setq completion-point nil)
             )
           )
          (t (let ((longest-common-substring ; Several completions. 
                    (try-completion pattern all-scheme-names-alist-final))) ; Complete as much as possible and show list.
               (if (> (length longest-common-substring) (length pattern))
                   (progn
                     (kill-region beg end)
                     (insert longest-common-substring)
                     (setq completion-point nil)
                     (let* ((extended-pattern longest-common-substring)
                            (new-completions (all-completions extended-pattern all-scheme-names-alist-final)))
                       (show-tool-tip-text (list-to-string-with-sep new-completions CR))))
                 (progn    
                   (if (= (length completions) 0) (beep))
                   (setq completion-point (point))
                   (show-tool-tip-text (list-to-string-with-sep completions CR))))))))))

; Modelled after scheme-complete-symbol-with-mode.  Somewhat simpler, however, due to fewer cases.
(defun schemedoc-examples ()
  "Show tooltip example information of the word under point when it is complete"
  (let* ((mode 'all)   ; parameter in scheme-complete-symbol-with-mode. Fixed value in this function.
         (end (point))
              (beg (with-syntax-table (if (boundp 'scheme-mode-syntax-table) scheme-mode-syntax-table (syntax-table))
                     (save-excursion
                       (backward-sexp 1)
                       (while (= (char-syntax (following-char)) ?\')
                         (forward-char 1))
                       (point))))
              (pattern (buffer-substring-no-properties beg end))
              (all-scheme-names-alist-final 
               (cond ((eq mode 'all)
                      (append (trivial-alist (bound-names-until-point) -1) all-scheme-names-alist))
                                        ; append local names (marked with -1) with all other names (marked 0 or higher).
                     ((eq mode 'local) 
                      (trivial-alist (bound-names-until-point) -1))
                     (else (error "scheme-complete-symbol-with-mode: Unknown mode: %s" mode))))
              (completions (all-completions pattern all-scheme-names-alist-final))
              )
         (cond 
          ((and (= (length completions) 1) (equal (car completions) pattern)) ; first completion is a total match. 
             (show-tool-tip-text (schemedoc-information pattern all-scheme-names-alist-final 'examples-only))
           )
          ((and (> (length completions) 1) (equal (car completions) pattern)) ; first completion is a total match. 
           (show-tool-tip-text
             (schemedoc-information pattern all-scheme-names-alist-final 'examples-only))
           )
          (t (beep)))))  


(defun short-info-about-completions (completions name)
  (let ((lgt (length completions)))
     (cond ((= lgt 1) (format "%s is complete. An additional completion:\n%s" name (as-string (car completions))))
           ((> lgt 20)
              (format "%s is complete. There are  %s  additional completions." name (length completions)))
           (t (format "%s is complete. Additional completions:\n%s."           name (list-to-string-with-sep completions " "))))))

(defun show-tool-tip-text (buffer-txt)
  (let* ((tooltip-frame-parameters '( ; (left . 10)     ; dynamic binding - shadowing more global variable
                                      ; (top . 10)
                                     (name . "tooltip")
                                     (internal-border-width . 5)
                                     (border-width . 1)))
         (tooltip-lines (cdr x-max-tooltip-size))
         (lines-in-buffer-txt (number-of-lines-in-string buffer-txt))
         (txt (if (<= lines-in-buffer-txt tooltip-lines) 
                  buffer-txt
                (concat (string-line-range buffer-txt 1 tooltip-lines) )))
        )
    (tooltip-show txt)))

; ---------------------------------------------------------------------------------------------------------------
; EXTRACTION OF DEFINED NAMES FROM A SCHEME SOURCE FILE:

(defvar current-source-file-manlsp nil "The list of pseudo Schemedoc manlsp entries of the current source file")

(defvar programmed-scheme-name-alist nil "The list of Scheme names in the current source file. A trivial a-list.")

(defun init-name-completion-current-source-file ()
  "Set up the two lists of informations about the Scheme names in the current source file.
Assigns the variables current-source-file-manlsp and programmed-scheme-name-alist.
This includes source files enumerated by .source-file-dependencies.
Called by the interactive command reset-scheme-completion. Returns the number existing files in .source-file-dependencies."
  ; Current source file:
  (let ((count-other 0))
    (setq current-source-file-manlsp (extract-name-and-form-documentation-of-current-source-file (buffer-name)))

    ; Files mentioned in .source-file-dependencies:
    (let ((other-source-files (find-source-file-dependencies)))
      (mapcar 
       (function 
        (lambda (other-file)
          (let ((full-other-file (if (absolute-file-path-p other-file) other-file (concat (current-directory) other-file)))
                )
            (if (file-exists-p full-other-file)
                (progn
                  (setq current-source-file-manlsp (append current-source-file-manlsp (extract-name-and-form-documentation-of-other-source-file full-other-file other-file)))
                  (setq count-other (+ 1 count-other)))))))
       other-source-files)
      )

    (setq programmed-scheme-name-alist
          (mapcar (function (lambda (e)
                              (cons (assoc-get 'title e) 0))) current-source-file-manlsp))

    count-other))

(defun find-source-file-dependencies ()
  (save-excursion
  (goto-char (point-min))
  (let ((search-res (search-forward ".source-file-dependencies" nil t)))
     (if search-res
         (mapcar (function normalize-source-file-dependency-file-path) (read-strings-from-scheme-source))
       nil))))


; (defun extract-name-and-form-documentation-of-current-source-file (source-file-info)
;   "Do the extraction of the current buffers Scheme source file. Returns an association list which is compatible with SchemeDoc's manlsp format."
;   (save-excursion
;     (let ((res-a-list '()))
;       (condition-case nil  ; extract names as long as possible. 
;         (progn
;           (goto-char (point-min))
;           (let ((next (goto-first-form)))
;             (while next
;               (progn
;                 (if (is-current-form-a-scheme-definition)
;                     (setq res-a-list 
;                           (cons (name-and-form-of-definition source-file-info) res-a-list)))
;                 (setq next (goto-next-form)))))
;           res-a-list)
;         (error res-a-list)  ; In case of problems, return the names extracted until the error occurs
;       ))))

(defun extract-name-and-form-documentation-of-current-source-file (source-file-info)
  "Do the extraction of the current buffers Scheme source file.
Robust even in case of parse problems. Returns an association list which is compatible with SchemeDoc's manlsp format."
  (save-excursion
   (catch 'result
    (let ((res-a-list '()))
       (goto-char (point-min))
       (let ((next (goto-first-form)))
         (while next
           (condition-case nil   ; extract names as long as possible. 
               (progn
                 (if (is-current-form-a-scheme-definition)
                     (setq res-a-list 
                           (cons (name-and-form-of-definition source-file-info) res-a-list)))
                 (setq next (goto-next-form)))
             (error (goto-next-definition res-a-list)))))
       res-a-list))))

(defun goto-next-definition (res-a-list)
  (end-of-line 1)
  (let ((found (re-search-forward (regular-expression '(at-beginning-of-line "(define")) nil t)))
    (if found
        (beginning-of-line 1)
        (throw 'result res-a-list))))


(defun extract-name-and-form-documentation-of-other-source-file (source-file-path source-file-info)
  "Do the extraction of a named Scheme source file. source-file-path is an absolute path to the source file.
Returns an association list which is compatible with SchemeDoc's manlsp format."
  (let ((cur-buffer (get-buffer (current-buffer)))
        (temp-scheme-buf (get-buffer-create "temp-scheme-source-file")))
    (set-buffer temp-scheme-buf)
    (insert-file source-file-path)
    (scheme-mode)
    (let ((result (extract-name-and-form-documentation-of-current-source-file source-file-info)))
      (kill-buffer temp-scheme-buf)
      (set-buffer cur-buffer)
      result))) 
    
(defun goto-first-form ()
  "Bring us to the beginning of the first form.
Return nil, if no such next form exists, else return non-nil (the value of point).
Precondition: At beginning of a buffer."
  (let ((p0 (point)))
    (forward-through-white-space-and-comments)
    (if (= (point) (point-max))
        nil
        (point))))    

(defun goto-next-form ()
  "Bring us to the beginning of the next form.
Return nil, if no such next form exists, else return non-nil (the value of point).
Precondition: At beginning - start parenthesis - of a form."
  (let ((p0 (point)))
    (forward-sexp 1)
    (forward-through-white-space-and-comments)
    (if (= (point) (point-max))
        nil
        (point))))

(defvar scheme-definition-regexp 
    (regular-expression (list 'concat "(" '(zero-or-more (char-set (char 32) (char 10) (char 13) (char 9))) 
                                          "define"
                                          '(zero-or-more (char-set (char 32) (char 10) (char 13) (char 9))) ))
    "A regular expression that captures a Scheme definition, in reality only the leading parenthesis and the define keyword."
)

(defun is-current-form-a-scheme-definition ()
 "Given the implicitly selected form - by point - return if is the form 
is considered to be a Scheme definition"
  (looking-at scheme-definition-regexp)
)

(defun name-and-form-of-definition (source-file-info)
  "Given that the implicitly selected form - by point - is a definition, return
its name and form as a list in a format, which conforms to the internal manlsp SchemeDoc format."
  (save-excursion
   (let ((p0 (point)))
    (let ((p1 (re-search-forward scheme-definition-regexp)))  ; will match - p1 points a defining form or name
      (list (list 'source source-file-info) 
            (list 'form (implicit-selected-current-form))
            (list 'title (implicit-selected-current-name)))))))

(defun implicit-selected-current-form ()
  (save-excursion
   (let ((p0 (point)))
     (forward-sexp 1)
     (buffer-substring-no-properties p0 (point)))))

(defun implicit-selected-current-name ()
  (save-excursion
    (cond ((looking-at "(")
             (forward-char 1)
             (forward-through-white-space)
             (let ((p0 (point)))
                (forward-sexp 1)
                (buffer-substring-no-properties p0 (point))))
          (t (let ((p0 (point)))
               (forward-sexp 1)
               (buffer-substring-no-properties p0 (point)))))))

; ---------------------------------------------------------------------------------------------------------------
; COMPLETION LISTS OF LOCALLY BOUND NAMES:

(defun bound-names-until-point ()
 "Return a list of bound names between the start of the current form and the current point.
Returns the empty list if the current form is not a definition."
 (save-excursion
   (let ((p1 (point)))
     (beginning-of-defun) 
     (let ((p0 (point)))
       (if (is-current-form-a-scheme-definition)
           (bound-names-from-until p0 p1)
           nil)))))

(defun bound-names-from-until (p0 p1)
  (let ((result '())
        (outer-scope-points (surrounding-scope-points p1))
       )
    (goto-char p0) 
    (goto-first-name)
    (while (< (point) p1)
      (let ((bound-result (is-name-bound)))
        (if (and bound-result (in-scope bound-result outer-scope-points))
            (setq result (cons (name-under-point) result))))
      (goto-next-name))
    (reverse result)))

(defun in-scope (bound-result outer-scope-points)
  (linear-search outer-scope-points
    (function (lambda (osp) (= osp (car bound-result))))
    (function (lambda (x) x))))

(defun surrounding-scope-points (p)
 (save-excursion
  (goto-char p)
  (condition-case nil
   (progn
      (backward-up-list 1)
      (goto-char (point))
      (cons (point) (surrounding-scope-points (point))))
   (error nil))))

(defun goto-next-name ()
  "Precondition: Point is selecting a name. Pass through it and navigate to next name"
  (forward-sexp 1) ; pass through the actual name
  (while (looking-at-chars (point) (list 9 10 13 32 40 41)) (forward-char 1))
  (cond ((looking-at ";") (end-of-line 1) (goto-first-name))
        ((looking-at "\"") (forward-sexp 1)  (goto-first-name))
        (t 'do-nothing)))

(defun goto-first-name ()
  "Navigate to next name"
  (while (looking-at-chars (point) (list 9 10 13 32 40 41)) (forward-char 1))
  (cond ((looking-at ";") (end-of-line 1) (goto-first-name))
        ((looking-at "\"") (forward-sexp 1)  (goto-first-name))
        (t 'do-nothing)))


(defun is-name-bound ()
  "Is the name under point bound. Returns a cons-pair, or nil."
  (let ((name (name-under-point)))
    (let ((a (in-lambda-parameter-list-context)))
      (if a (cons a 'lambda) 
          (let ((b (in-let-binding-name-context)))
              (if b (cons b 'let) 
                  (let ((c (in-define-signature-context-form1)))
                     (if c (cons c 'define)
                         (let ((d (in-define-signature-context-form2)))
                            (if d (cons d 'define) nil))))))))))

(defun in-lambda-parameter-list-context ()
  (save-excursion 
    (condition-case nil
      (progn 
        (backward-up-list 1)
        (backward-sexp 1)
        (if (looking-at "lambda")
            (- (point) 1)
            nil))
      (error nil))))

(defun in-let-binding-name-context ()
  (save-excursion 
    (condition-case nil
      (progn 
        (backward-char 1)
        (if (looking-at "(")
            (progn
              (backward-up-list 1)
              (backward-sexp 1)
              (if (or (looking-at "let") (looking-at "let*") (looking-at "letrec"))
                  (- (point) 1)
                  nil))
            nil))
      (error nil))))

(defun in-define-signature-context-form1 ()   ; (define (f x y ) ...) => f x y
  (save-excursion 
    (condition-case nil
      (progn 
        (backward-up-list 1)
        (backward-sexp 1)
        (if (looking-at "define")
            (- (point) 1)
            nil))
      (error nil))))

(defun in-define-signature-context-form2 ()   ; (define f (xx yy)) => f
  (save-excursion 
    (condition-case nil
      (progn 
        (backward-sexp 1)
        (if (looking-at "define")
            (- (point) 1)
            nil))
      (error nil))))
  
; -----------------------------------------------------------------------------------------------------------------------------
; Parameter template support.
; Functionality which extracts formal parameters from the head of a definition, or from a .form (...) part.
; Inserts SchemeDoc .parameter clauses at the position of point.
; Ugly programming - nice functionality.

; Regular expression which captures  (define some-name (lambda ...))
(setq def-reg-exp-1  (regular-expression (list 'concat "(" '(zero-or-more (char-set (char 32) (char 10) (char 13) (char 9))) 
                                            "define"
                                            '(zero-or-more (char-set (char 32) (char 10) (char 13) (char 9)))
                                            '(one-or-more (any-single-char))    
                                            '(zero-or-more (char-set (char 32) (char 10) (char 13) (char 9)))
                                            "("
                                            '(zero-or-more (char-set (char 32) (char 10) (char 13) (char 9)))
                                            "lambda"
                                            '(zero-or-more (char-set (char 32) (char 10) (char 13) (char 9))))))


; Regular expression which captures   (define some-form ...) 
(setq def-reg-exp-2  (regular-expression (list 'concat "(" '(zero-or-more (char-set (char 32) (char 10) (char 13) (char 9))) 
                                            "define"
                                            '(zero-or-more (char-set (char 32) (char 10) (char 13) (char 9)))
                                            )))

; Main function in this part:
(defun insert-parameter-templates ()
  "It is assumed that the cursor position is within a documentation comment. Insert appropriate .parameter templates of all formal parameters.
The parameters are taken from a .form clause if it exists, else from the head of the method. 
It is assumed, as precondition, that there exists a well-formed define form in front of point." 
  (interactive)

   (let ((p-cur (point)))
    (end-of-defun 1)
    (let ((p-end (point)))
      (goto-char p-cur)
      (let ((p1 (re-search-forward def-reg-exp-1 p-end t)))           ; position just after "(define N (lambda "
        (goto-char p-cur)
        (let* ((p2 (re-search-forward def-reg-exp-2 p-end t))         ; position just after "(define "
               (name (get-name-given-positions p1 p2))                ; The name of the function after point - used for identification of appropriate .form clause
               (parameters-from-form (get-parameters-from-form-given-name name))   ; Extract parameters from possible form part. Empty list if non-existent.
               (parameters-from-head (get-parameters-given-positions p1 p2))       ; Extract parameters from head of definition.
               (parameters (if (not (null parameters-from-form)) parameters-from-form parameters-from-head))
              )

          (cond ((null parameters) (beep))

                ((symbolp parameters)             ; corresponding to (define (n . p) ...) or (define n (lambda p) ...)
                   (goto-char p-cur)
                   (insert (format ";; .parameter %s " parameters))
                   (goto-char p-cur) (end-of-line 1)
                )

                ((improper-listp parameters)
                   (goto-char p-cur)
                   (if (not (null parameters))
                       (progn
                         ; Proper part of parameter list:
                         (mapcar (function 
                                  (lambda (par)
                                    (insert (format ";; .parameter %s " par)) (insert CR)))
                                 (proper-list-part parameters))

                         ; Improper part:
                         (insert (format ";; .parameter %s " (improper-list-part parameters)))
                         (goto-char p-cur) (end-of-line 1)
                       )
                     (beep)))

                (t (goto-char p-cur)
                   (if (not (null parameters))
                       (progn
                         ; All but the last parameters
                         (mapcar (function 
                                  (lambda (par)
                                    (insert (format ";; .parameter %s " par)) (insert CR)))
                                 (butlast parameters))

                         ; Insert the last one - without CR after it.
                         (insert (format ";; .parameter %s " (car (last parameters))))
                         (goto-char p-cur) (end-of-line 1)
                       )
                     (beep))
                   )))))))

; Extract .form (name ...) contributions prior to point.
(defun get-parameters-from-form-given-name (name)
  (let ((form-reg-exp (regular-expression (list 'concat ".form" '(zero-or-more (char-set (char 32) (char 10) (char 13) (char 9)))    ; .form (name ...
                                            "("
                                            '(zero-or-more (char-set (char 32) (char 10) (char 13) (char 9)))
                                            name
                                            ))))
     (if (re-search-backward form-reg-exp nil t)
         (progn
            (while (not (looking-at "(")) (forward-char 1))
            (let ((p0 (point)))
              (forward-sexp 1)
              (let ((form (car (read-from-string (buffer-substring-no-properties p0 (point))))))
                 (flatten-array-part-of-parameters (cdr form)))))
         nil)))

; Specialized function:
(defun flatten-array-part-of-parameters (x)
  (cond ((null x) nil)
        ((vectorp (car x))
            (append (as-list (car x)) (flatten-array-part-of-parameters (cdr x))))
        (t (cons (car x) (flatten-array-part-of-parameters (cdr x))))))


; A very specialized function:
(defun get-parameters-given-positions (p1 p2)
 (save-excursion
   (cond (p1 (goto-char p1)                                              ; position just after "(define N (lambda "
             (forward-sexp 1)
             (let ((res (car (read-from-string (buffer-substring-no-properties p1 (point))))))
                (if (consp res) res (list res))))
         (p2 (goto-char p2)                                              ; position just after "(define "
             (forward-sexp 1)
             (let ((res (car (read-from-string (buffer-substring-no-properties p2 (point))))))
                (if (consp res) (cdr res) nil)))
         (t nil))))

; A very specialized function:
(defun get-name-given-positions (p1 p2)
 (save-excursion
   (cond (p1 (goto-char p1)                                              ; position just after "(define N (lambda "
             (backward-up-list 1) (backward-sexp 1) (scheme-name-under-point))
         (p2 (goto-char p2)                                              ; position just after "(define "
             (down-list 1) (scheme-name-under-point))
         (t nil))))
       

; -------------------------------------------------------------------------------------------------------------------------------------------------------------             
   
(do-init-schemedoc) ; experimental as of april 2009