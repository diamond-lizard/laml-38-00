; ---------------------------------------------------------------------------------------------------------------
; FINDING LISP DEFINITIONS AND MULTI-FILE NAME SEARCHING.
; Interactively find the defintion/application of a selected name in multiple files (primarily scheme and elisp files).
; In other word, nagivate from an applied name to its definition.
; Programmed for both Scheme and Emacs Lisp.
; Keybidings are external to this file (in .emacs and/or laml-key-menu-bindings.el of the laml directory).
; Parts of the Scheme stuff is found in laml-general.el

(defvar elisp-file-position-stack nil "A list of stored positions (file-path . pos) in a this Elisp source file.")
(defvar elisp-file-ghost-position-stack nil "A auxiliary list of stored positions in a this Elisp source file.")
(defvar scheme-file-position-stack nil "A list of stored positions (file-path . pos) in a this Scheme source file.")
(defvar scheme-file-ghost-position-stack nil "A auxiliary list of stored positions in a this Scheme source file.")

(defvar scheme-source-files-for-finding nil "The list of all Scheme source files - full paths - in which to look for definitions.")
(defvar elisp-source-files-for-finding nil "The list of all Emacs Lisp source files - full paths - in which to look for definitions.")

(defun reset-scheme-source-file-finding ()
  (setq  scheme-file-position-stack nil)
  (setq  scheme-file-ghost-position-stack nil)
  (setq  scheme-source-files-for-finding nil)
)

(defun reset-elisp-source-file-finding ()
  (setq  elisp-file-position-stack nil)
  (setq  elisp-file-ghost-position-stack nil)  
  (setq  elisp-source-files-for-finding nil)
)

(defun reset-source-file-finding ()
  (interactive)
  (cond ((eq major-mode 'scheme-mode)
           (reset-scheme-source-file-finding) )
        ((eq major-mode 'emacs-lisp-mode)
           (reset-elisp-source-file-finding))  
        (t (error "")))
  (message "DONE"))


(defun push-lisp-file-position (full-file-path p)
  (cond ((eq major-mode 'scheme-mode)
           (setq scheme-file-position-stack (cons (cons full-file-path p) scheme-file-position-stack))
           (setq scheme-file-ghost-position-stack nil))
        ((eq major-mode 'emacs-lisp-mode)
           (setq elisp-file-position-stack (cons (cons full-file-path p) elisp-file-position-stack))
           (setq elisp-file-ghost-position-stack nil)) 
        (t (error ""))))

(defun pop-lisp-file-position ()
  (interactive)
  (cond ((eq major-mode 'scheme-mode)
           (if (not (null scheme-file-position-stack))
               (let ((top (car scheme-file-position-stack))
                     (cur-pos (point))
                     (cur-file (buffer-file-name (current-buffer)))
                    )
                 (find-file (car top))
                 (goto-char (cdr top)) (recenter)
                 (setq scheme-file-ghost-position-stack 
                        (cons (cons cur-file cur-pos) scheme-file-ghost-position-stack))
                 (setq scheme-file-position-stack (cdr scheme-file-position-stack))
                 (message "Back chain: %s. Forward chain: %s" (length scheme-file-position-stack) (length scheme-file-ghost-position-stack))
                 )
             (message "No more places to come back to...")))
        ((eq major-mode 'emacs-lisp-mode)
           (if (not (null elisp-file-position-stack))
               (let ((top (car elisp-file-position-stack))
                     (cur-pos (point))
                     (cur-file (buffer-file-name (current-buffer)))
                    )
                 (find-file (car top))
                 (goto-char (cdr top)) (recenter)
                 (setq elisp-file-ghost-position-stack 
                        (cons (cons cur-file cur-pos) elisp-file-ghost-position-stack))
                 (setq elisp-file-position-stack (cdr elisp-file-position-stack))
                 (message "Back chain: %s. Forward chain: %s" (length elisp-file-position-stack) (length elisp-file-ghost-position-stack))
                 )
             (message "No more places to come back to...")))
        (t (error ""))))

(defun pop-ghost-lisp-file-position ()
  (interactive)
  (cond ((eq major-mode 'scheme-mode)
           (if (not (null scheme-file-ghost-position-stack))
               (let ((top (car scheme-file-ghost-position-stack))
                     (cur-pos (point))
                     (cur-file (buffer-file-name (current-buffer)))
                    )
                 (find-file (car top))
                 (goto-char (cdr top)) (recenter)
                 (setq scheme-file-ghost-position-stack (cdr scheme-file-ghost-position-stack))
                 (setq scheme-file-position-stack (cons (cons cur-file cur-pos) scheme-file-position-stack))
                 (message "Back chain: %s. Forward chain: %s" (length scheme-file-position-stack) (length scheme-file-ghost-position-stack))
                 )
             (message "No more places to go forward...")))
         ((eq major-mode 'emacs-lisp-mode)
           (if (not (null elisp-file-ghost-position-stack))
               (let ((top (car elisp-file-ghost-position-stack))
                     (cur-pos (point))
                     (cur-file (buffer-file-name (current-buffer)))
                    )
                 (find-file (car top))
                 (goto-char (cdr top)) (recenter)
                 (setq elisp-file-ghost-position-stack (cdr elisp-file-ghost-position-stack))
                 (setq elisp-file-position-stack (cons (cons cur-file cur-pos) elisp-file-position-stack))
                 (message "Back chain: %s. Forward chain: %s" (length elisp-file-position-stack) (length elisp-file-ghost-position-stack))
                 )
             (message "No more places to go forward...")))
         (t (error ""))))

(defun scheme-definition-reg-exp (name)
  (regular-expression 
   `(or (concat "(" (zero-or-more (char-set tab newline space return)) 
                "define" (zero-or-more (char-set tab newline space return)) ,name
                (one-or-more (char-set tab newline space return)))
        (concat "(" (zero-or-more (char-set tab newline space return))
                  "define" (zero-or-more (char-set tab newline space return)) "("
                  (zero-or-more (char-set tab newline space return))  ,name (one-or-more (char-set tab newline space return))))))

(defun elisp-definition-reg-exp (name)
  (regular-expression 
   `(or (at-beginning-of-line 
          (concat "(" (zero-or-more (char-set tab newline space return)) "defun" 
                 (zero-or-more (char-set tab newline space return)) ,name (not-syntax-code word-constituent) ))
        (at-beginning-of-line 
          (concat "(" (zero-or-more (char-set tab newline space return)) "defvar"
                 (zero-or-more (char-set tab newline space return)) ,name (one-or-more (char-set tab newline space return)))))))

(defun find-lisp-definition-of (name)
  "Find the Scheme/Elisp defintion of name in the current file, and next in the files in scheme-source-files-for-finding/elisp-source-files-for-finding (depending on the major mode), and goto that place"
  (interactive (list (name-under-point-no-paren)))
  (let* ((define-reg-exp 
           (cond ((eq major-mode 'scheme-mode)
                    (scheme-definition-reg-exp name))
                 ((eq major-mode 'emacs-lisp-mode)
                    (elisp-definition-reg-exp name))
                 (t (error "Finding is only supported in scheme-mode and emacs-lisp-mode"))))
         (cur-pos (point))
         (cur-buf (current-buffer))
        )
    (goto-char (point-min))
    (let ((search-res (re-search-forward define-reg-exp nil t)))
      (if search-res
          (progn (push-lisp-file-position (buffer-file-name (current-buffer)) cur-pos)
                 (goto-char search-res)
                 (re-search-backward define-reg-exp nil t)
                 (recenter)
                 (cond ((eq major-mode 'scheme-mode)
                          (message "Find chain length is %s" (length scheme-file-position-stack))) 
                       ((eq major-mode 'emacs-lisp-mode)
                          (message "Find chain length is %s" (length elisp-file-position-stack)))
                       (t (error "")))
          )
          (let* ((remaining-source-files (filter     ; all other source files than the current one
                                          (function 
                                           (lambda (fp)
                                             (not (equal fp (buffer-file-name (current-buffer))))))
                                          (cond ((eq major-mode 'scheme-mode) scheme-source-files-for-finding)
                                                ((eq major-mode 'emacs-lisp-mode) elisp-source-files-for-finding)
                                                (t nil))))
                 (search-res (find-lisp-definition-in-files name remaining-source-files)))
             (if search-res
                 (progn
                    (push-lisp-file-position (buffer-file-name (current-buffer)) cur-pos)
                    (find-file (car search-res))
                    (goto-char (cdr search-res))
                    (re-search-backward define-reg-exp nil t)
                    (recenter)
                    (cond ((eq major-mode 'scheme-mode)
                             (message "Find chain length is %s" (length scheme-file-position-stack))) 
                          ((eq major-mode 'emacs-lisp-mode)
                             (message "Find chain length is %s" (length elisp-file-position-stack)))
                          (t (error "Major mode is supposed to be scheme-mode or emacs-lisp-mode"))))
                 (progn (set-buffer cur-buf)
                        (show-buffer (selected-window) cur-buf) 
                        (goto-char cur-pos)
                        (message "Cannot find a definition of %s in the designated files" name))))))))

(defun find-lisp-definition-in-files (name remaining-source-files)
  "Find the definition of name in one of the files remaining-source-files.
Return a cons pair of (full-file-path . pos), or nil if not found."
  (if (null remaining-source-files)
      nil
      (let* ((source-file (car remaining-source-files))
             (search-res (find-lisp-definition-in-single-file name source-file)))
        (if search-res
            search-res
            (find-lisp-definition-in-files name (cdr remaining-source-files))))))

(defun find-lisp-definition-in-single-file (name source-file-path)
  "Find the definition of name in source-file (full path).
If successful, return  a cons pair of (full-file-path . pos). If not, return nil."
  (save-excursion
    (find-file source-file-path)
    (let* ((define-reg-exp 
             (cond ((eq major-mode 'scheme-mode)
                      (scheme-definition-reg-exp name))
                   ((eq major-mode 'emacs-lisp-mode)
                      (elisp-definition-reg-exp name))
                   (t (error "Finding is only supported in scheme-mode and emacs-lisp-mode")))))
      (goto-char (point-min))
      (let ((search-res (re-search-forward define-reg-exp nil t)))
        (if search-res
            (cons source-file-path search-res)
            nil)))))

(defun name-under-point-no-paren ()
  (if (looking-at "(")
      (error "Please select a name")
      (let ((nm (name-under-point)))
        (if (and (> (length nm) 0) (= ?' (aref nm 0)))  ; nm starts with single quote
            (substring nm 1)
            nm)))) 
      

(defun find-lisp-definition-of-and-reset (name)
  "Find the Scheme defintion of name in the current file, at goto that place. Before finding, reset the stack of places"
  (interactive (list (name-under-point-no-paren)))

  (cond ((eq major-mode 'scheme-mode)
           (setq scheme-file-position-stack nil))
        ((eq major-mode 'emacs-lisp-mode)
           (setq elisp-file-position-stack nil))
        (t (error "")))

  (find-lisp-definition-of name))


(defun add-source-file-for-finding-definitions (full-path)
  (let ((extension (file-name-extension full-path)))
     (cond ((equal extension "scm")
              (if (not (member full-path scheme-source-files-for-finding))
                  (setq scheme-source-files-for-finding (cons full-path scheme-source-files-for-finding)))

           )
           ((equal extension "el")
              (if (not (member full-path elisp-source-files-for-finding))
                  (setq elisp-source-files-for-finding (cons full-path elisp-source-files-for-finding)))
           )
           (t (error "Only scheme and elisp source files can be handled")))
     (info-about-source-file-for-finding-definitions)))

(defun remove-source-file-for-finding-definitions (full-path)
  (let ((extension (file-name-extension full-path)))
     (cond ((equal extension "scm")
              (setq scheme-source-files-for-finding
                    (filter 
                     (function 
                       (lambda (fp)
                         (not (equal fp full-path))))
                     scheme-source-files-for-finding)))
           ((equal extension "el")
              (setq elisp-source-files-for-finding
                    (filter 
                     (function 
                       (lambda (fp)
                         (not (equal fp full-path))))
                     elisp-source-files-for-finding)))
           (t (error "Only scheme and elisp source files can be handled")))))

(defun info-about-source-file-for-finding-definitions ()
  (interactive)
  (let ((extension (file-name-extension (buffer-file-name (current-buffer)))))
     (cond ((equal extension "scm")
              (if (not (null scheme-source-files-for-finding))
                  (message "List of Scheme files: %s"
                           (list-to-string (mapcar (function file-name-nondirectory) scheme-source-files-for-finding) ", "))
                  (message "Scheme definitions are only searched for in the current file")))
           ((equal extension "el")
              (if (not (null elisp-source-files-for-finding))
                  (message "List of Elisp files: %s"
                           (list-to-string (mapcar (function file-name-nondirectory) elisp-source-files-for-finding) ", "))
                  (message "Elisp definitions are only searched for in the current file")))
           (t (message "Apply this function on a Scheme (-.scm) or Elisp (-.el) file")))))

(defun add-current-source-file-for-finding-definitions ()
  (interactive)
  (add-source-file-for-finding-definitions (buffer-file-name (current-buffer))))

(defun remove-current-source-file-for-finding-definitions ()
  (interactive)
  (remove-source-file-for-finding-definitions (buffer-file-name (current-buffer))))

(defun help-with-source-file-finding ()
  (interactive)
  (let* ((buf-name "*FindingDefinitionsHelp*")
         (help-buffer (if (get-buffer buf-name) (get-buffer buf-name) (generate-new-buffer buf-name))))
    (show-buffer (other-window 1) help-buffer)
    (set-buffer help-buffer)
    (toggle-read-only -1)
    (erase-buffer)
    (insert-file (concat laml-dir "emacs-support/" "source-finding-help.txt"))
    (toggle-read-only 1)
    (set-buffer-modified-p nil)
    (other-window 1)))


; --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
; Multi-file name searching.

; State variables for a single multi-file search:
(defvar multi-file-name-search-list nil)
(defvar multi-file-name-search-pos nil)
(defvar multi-file-name-search-name nil)
(defvar multi-file-name-search-starting-pos nil)
(defvar multi-file-name-search-starting-file nil)

(defun initiate-multi-file-name-search (name)
  (interactive (list (name-under-point-no-paren)))
  (let* ((cfp (buffer-file-name (current-buffer)))
         (extension (file-name-extension cfp))
       )

    (setq multi-file-name-search-starting-pos (point))
    (setq multi-file-name-search-starting-file cfp)

    ; Setting multi-file-name-search-list - the list of files to search, including the current one (in the current buffer).
    (setq multi-file-name-search-list
          (cons cfp
                (cond 
		 ((equal extension "scm")
		  (filter 
		   (function 
		    (lambda (full-path)
		      (not (equal cfp full-path))))
		   (reverse scheme-source-files-for-finding)))
		 ((equal extension "el")
		  (filter 
		   (function 
		    (lambda (full-path)
		      (not (equal cfp full-path))))
		   (reverse elisp-source-files-for-finding)))
                 (t nil)                                                                     ; ad hoc
;		 (t (error "Only scheme and elisp source files can be handled")) 
           )))
    (setq multi-file-name-search-name name)

    (beginning-of-buffer)

    (let ((search-res (search-forward name nil t)))
      (if search-res
          (progn
             (setq multi-file-name-search-pos (point)))
          (continue-multi-file-name-search)))

    ; Message:
    (if (not (null (cdr multi-file-name-search-list)))
        (message "Initiating search for \"%s\". Starting at beginning of current buffer, and will proceed through %s. To continue search use F8. To reset search: C-F8." name (list-to-string (mapcar (function file-name-nondirectory) (cdr multi-file-name-search-list)) ", "))
        (message "Initiating search for \"%s\". Starting at beginning of current buffer - no other files involved." name))
  ))


(defun continue-multi-file-name-search ()
  (interactive)
  (if (and (not (null multi-file-name-search-list)) (numberp multi-file-name-search-pos))
      (progn
         (find-file (car multi-file-name-search-list))
         (goto-char multi-file-name-search-pos)

         (let ((initial-point multi-file-name-search-pos)
               (search-res (search-forward multi-file-name-search-name nil t)))
           (if search-res
	       (progn
		 (setq multi-file-name-search-pos (point))
                 (if (= initial-point 0)  ; meaning: we have just started in a new file
                     (message "Continuing search for \"%s\", now in %s" multi-file-name-search-name (car multi-file-name-search-list))
                     (if (not (null (cdr multi-file-name-search-list)))
                         (message "Continuing search for \"%s\".  Pending files to search: %s. To reset search: C-F8." multi-file-name-search-name (list-to-string (mapcar (function file-name-nondirectory) (cdr multi-file-name-search-list)) ", "))
                         (message "Continuing search for \"%s\"." multi-file-name-search-name)))
               )
             (progn
               (setq multi-file-name-search-list (cdr multi-file-name-search-list))
               (setq multi-file-name-search-pos 0)
               (continue-multi-file-name-search)))))
      (progn
        (reset-multi-file-name-search)
        (message "Search is over - now back where the search started. Use S-F8 to start a new search.")
      )))

(defun reset-multi-file-name-search ()
   (interactive)
   (if (and (not (null multi-file-name-search-starting-file)) (not (null multi-file-name-search-starting-pos)))
       (progn
	 (find-file multi-file-name-search-starting-file)
	 (goto-char multi-file-name-search-starting-pos)

	 (setq multi-file-name-search-list nil)
	 (setq multi-file-name-search-pos nil)
	 (setq multi-file-name-search-name nil)
	 (setq multi-file-name-search-starting-pos nil)
	 (setq multi-file-name-search-starting-file nil)   

	 (message "Now back to initial position of multi file name search."))
       (message "Nothing to reset in multi-file name search.")))


             

; --------------------------------------------------------------------------------------------------------------------------------------------------------------------------

(defun next-top-form()
  (if (= (point) 1)
      (progn
        (end-of-defun 1)
        (backward-sexp 1))
      (progn
        (end-of-defun 2)
        (backward-sexp 1))))

(defun eval-defun-sequentially ()
  (interactive)
  (goto-char (point-min))
  (while t
     (next-top-form)
     (lisp-eval-defun)))