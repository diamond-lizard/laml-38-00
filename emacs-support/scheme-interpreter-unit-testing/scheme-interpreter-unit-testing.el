; This facility supports bookkeeping of test cases from a Scheme interpreter in Emacs.
; It relies on the Schematics Schemeunit software.

; ---------------------------------------------------------------------------------------------------------------
; CONFIGURATION PART
; As a matter of installation you must define the values of the following variables in your .emacs file
; BEFORE the loading of this file.


; (defvar test-suite-software-dir "some file path ended with forward slash" 
; "The directory in which we keep data about all known/registered test suites")


; ---------------------------------------------------------------------------------------------------------------

(if (or (not (boundp 'test-suite-software-dir)) (null test-suite-software-dir))
  (error "Please define the variable test-suite-software-dir in your .emacs file BEFORE you load the testing tool"))

(defvar comint-command-list-file-path
  (concat test-suite-software-dir "comint-commands.scm")
  "The full and absolute path the complete list of comint commands.")


; ---------------------------------------------------------------------------------------------------------------

; A list of test suit structures. Bookkeeping info.
(defvar test-suite-list nil "The list of known test suites - represented as a list of test suite structures")

; Selector of test suite structure: Access test suite name.
(defun test-suite-name-of (test-suite-struct)
  (car test-suite-struct))

; Selector of test suite structure: Access test suite directory.
(defun test-suite-directory-of (test-suite-struct)
  (cadr test-suite-struct))


; The current test suite name and directory:
(defvar current-test-suite-name nil)
(defvar current-test-suite-dir nil)

(defun current-testsuite-structure ()
  "Form the a testsuite structure (like an entry in test-suite-list) from current-test-suite-name 
and current-test-suite-dir."
  (list current-test-suite-name current-test-suite-dir))

; ---------------------------------------------------------------------------------------------------------------
; Initialization of Scheme interpreter unit testing 

(defun init-scheme-interpreter-unit-testing ()
 (let ((test-suite-list-path (concat test-suite-software-dir "test-suite-list.lsp")))
  (setq test-suite-list
    (if (file-exists-p test-suite-list-path)
        (file-read test-suite-list-path)
        nil))
  (add-hook 'inferior-lisp-mode-hook 
   (function (lambda ()
      (setq comint-input-sender (function extended-history-comint-send))
      (define-test-menu-in-comint)
      )))

  (add-hook 'kill-emacs-hook
   (function (lambda ()
     (save-comint-command-list previous-comint-command-list)
     (setq previous-comint-command-list nil)))) ))



; ---------------------------------------------------------------------------------------------------------------

(defun make-test-suite (dir0 name0)
  "Set up a new test suite and register it.
Beside bookkeeping, this involves creation of a name0-test dir and creation of files in this directory.
Connect to the new testsuite if you are not already connected to a testsuite. 
Else, ask if you wish to connect to the new testsuite."
  (interactive "DMake Scheme test suite in which directory: 
sName of test suite (name without spaces and without trailing '-test'): ")
 (let* ((name (concat name0 "-" "test"))
        (dir (ensure-trailing-slash (expand-file-name dir0)))
        (test-dir (concat dir name "/"))
       )
   (if (not (file-exists-p test-dir))
       (progn
         (make-directory test-dir)

         (make-file-in-directory test-dir "setup.scm" (concat "; Some actions that set up the test environment."))
         (make-file-in-directory test-dir "testcases" "")
         (make-file-in-directory test-dir "teardown.scm" (concat "; Some actions that tear down the test environment." ))
         (register-test-suite name0 (concat dir name "/"))

         (cond ((null current-test-suite-name)
                  (connect-to-testsuite name0))
               ((ask-user (concat "Do you want to connect to new testsuite - hereby disconnecting from " current-test-suite-name " - (yes or no) "))
                  (connect-to-testsuite name0))
               (t (message "DONE. You are not yet connected to the new test suite. Use M-x connect-to-testsuite")))
       )
       (progn
         (beep)
         (message (concat "The test suite directory " test-dir " already exists. Nothing done."))))))

(defun register-testsuite (testsuite-dir)
  (interactive "DRegister existing testsuite in directory (does NOT create new test files): ")
  (let ((testsuite-dir-1 (ensure-trailing-slash (eliminate-tilde-prefix testsuite-dir))))
    (if (is-testsuite-dir-p testsuite-dir-1)
        (let* ((dir-name (directory-name-proper testsuite-dir-1))
               (testsuite-name (name-of-testsuite-given-directory-name dir-name)))
          (if (not (member testsuite-name (mapcar (function car) test-suite-list)))
              (progn
                (register-test-suite testsuite-name testsuite-dir-1)
                (message (concat "The testsuite in " testsuite-dir-1 " has been registered under the name " testsuite-name)))
              (message (concat "A testsuite of the name " testsuite-name " is already registered. Consider a renaming of the new test directory."))))
      (progn
        (beep)
        (message "The directory is not a test directories. Test directories contain the files: setup.scm, teardown.scm, and testcases")))))

(defun name-of-testsuite-given-directory-name (dir-name)
  "Given dir-name, the proper name of test dir, return the name of the test" 
  (substring dir-name 0 (- (length dir-name) 5))) 

; Remove registration of the given testsuite.
(defun deregister-testsuite (testsuite-name)
  (interactive 
     (list (completing-read "Remove the registration of testsuite - no file deletion (space for possibilities): " 
                   (filter (function test-suite-exists-on-disk-p) test-suite-list) nil nil)))
  (deregister-test-suite testsuite-name)
  
  (if (equal current-test-suite-name testsuite-name)
      (progn
         (setq current-test-suite-name nil)
         (setq current-test-suite-dir nil)))

  (message (concat "Done. " testsuite-name " is no longer registered, but all test files remains."))
)

(defun deregister-all-testsuites ()
  (interactive)
  (setq test-suite-list nil)

  ; Make test-suite-list persistent, in a file that contains the list.
  (file-write test-suite-list (concat test-suite-software-dir "test-suite-list.lsp"))

  (setq current-test-suite-name nil)
  (setq current-test-suite-dir nil)

  (message "All testsuites have been deregistered, but no files have been deleted.")
)

; Deregister testsuites on name-list
(defun deregister-some-testsuites (name-list)
  (interactive)
  (let ((revised-test-suite-list     ; keep those test suites which are NOT on name-list
           (filter (function (lambda (ts) (not (member (test-suite-name-of ts) name-list)))) test-suite-list)))

    ; Make test-suite-list persistent, in a file that contains the list.
    (file-write revised-test-suite-list (concat test-suite-software-dir "test-suite-list.lsp"))
    (setq test-suite-list revised-test-suite-list)

    (setq current-test-suite-name nil)
    (setq current-test-suite-dir nil))
)

(defun deregister-unreachable-testsuites ()
  "Deregister all unreachable testsuites.
A testsuite is unreachable if its underlying directory has been moved, rename or deleted since the registration of the testsuite."
  (interactive)
  (let ((unreachable-test-suite-list
          (filter (function (lambda (ts) (not (test-suite-exists-on-disk-p ts)))) test-suite-list)))
    (if (= 0 (length unreachable-test-suite-list))
        (message "All testsuites are reachable. Nothing done.")
        (progn
          (deregister-some-testsuites (mapcar (function test-suite-name-of) unreachable-test-suite-list))
          (message (concat "DONE. " (int-to-string (length unreachable-test-suite-list)) " testsuites have been deregistered."))))))


; Register test suite
(defun register-test-suite (test-suite-name dir)
  (setq test-suite-list  (cons (list test-suite-name dir) test-suite-list))

  ; Make test-suite-list persistent, in a file that contains the list.
  (file-write test-suite-list (concat test-suite-software-dir "test-suite-list.lsp"))
)

; Eliminate test-suite-name from test-suite-list
(defun deregister-test-suite (test-suite-name)
  (setq test-suite-list 
    (filter (function (lambda (test-suite-list-record) (not (equal (car test-suite-list-record) test-suite-name)))) test-suite-list))

  ; Make test-suite-list persistent, in a file that contains the list.
  (file-write test-suite-list (concat test-suite-software-dir "test-suite-list.lsp"))

)

         

(defvar test-suites-added nil)

; Adds contributions to test-suite-list
(defun find-and-register-testsuites-in-dir (dir)
 (interactive "DFind and register all non-registered testsuites in directory: ")
 (setq test-suites-added nil)
 (find-and-register-test-suites-in-dir-1 (ensure-trailing-slash (eliminate-tilde-prefix dir)) nil)
 (if (null test-suites-added)
     (message "No unregistered testsuites have been found.")
     (message (concat "Registered " (int-to-string (length test-suites-added)) " new testsuite(s): " (list-to-string test-suites-added ", ")))))

; Adds contributions to test-suite-list, but ask first.
(defun q-find-and-register-testsuites-in-dir (dir)
 (interactive "DFind and register all non-registered testsuites in directory: ")
 (setq test-suites-added nil)
 (find-and-register-test-suites-in-dir-1 (ensure-trailing-slash (eliminate-tilde-prefix dir)) t)
 (if (null test-suites-added)
     (message "No testsuites have been added.")
     (message (concat "Registered " (int-to-string (length test-suites-added)) " new testsuite(s): " (list-to-string test-suites-added ", ")))))

(defun find-and-register-test-suites-in-dir-1 (dir query-user)
 (message dir)
 (cond ((is-testsuite-dir-p dir)
          (let ((testsuite-name (name-of-testsuite-given-directory-name (directory-name-proper dir)))) 
            (if (not (member testsuite-name (mapcar (function car) test-suite-list)))
                (if query-user
                  (if (ask-user (concat "Add testsuite in " dir " (yes or no): "))
                      (progn
                        (register-test-suite testsuite-name dir)
                        (setq test-suites-added (cons testsuite-name test-suites-added)))
                      (progn 'do-nothing))
                  (progn
                    (register-test-suite testsuite-name dir)
                    (setq test-suites-added (cons testsuite-name test-suites-added)))))))

       ((file-directory-p dir)
          (let ((subdir-list (filter (lambda (x) (not (or (equal x ".") (equal x ".."))))
                                     (filter
                                      (function (lambda (x)
                                                  (file-directory-p (concat dir x))))
                                      (directory-files dir)))))
            (mapcar
              (function (lambda (sd) (find-and-register-test-suites-in-dir-1 (concat dir sd "/") query-user)) subdir-list)
              subdir-list)))
       (t (progn
             'do-nothing))))


(defun connect-to-testsuite (test-suite-name)
 "Connect to an existing testsuite. The parameter test-suite-name is the name of testsuite to which we want to connect."
  (interactive 
     (list (completing-read "Connect to which testsuite (space for possibilities): " 
                   (filter (function test-suite-exists-on-disk-p) test-suite-list) nil nil)))
    
   (let* ((test-suite-struct (assoc test-suite-name test-suite-list))
          (test-suite-dir (if test-suite-struct (test-suite-directory-of test-suite-struct) nil)))
     (cond ((and test-suite-dir (file-exists-p test-suite-dir))
             (progn

               ; first break connection to existing testsuite - teardown:
               (if (and current-test-suite-name current-test-suite-dir)
                   (progn
                     (set-buffer (get-buffer "*inferior-lisp*"))
                     (insert (concat "(load " (string-it (concat current-test-suite-dir "teardown.scm")) ")")) (comint-send-input)))

               (setq current-test-suite-name test-suite-name)
               (setq current-test-suite-dir test-suite-dir)
               (if (and (is-laml-installed) (fboundp 'reset-schemedoc-information)) (reset-schemedoc-information)) ; in order to shift SchemeDoc manual 
               (set-buffer (get-buffer "*inferior-lisp*"))
               (insert (concat "(load " (string-it (concat current-test-suite-dir "setup.scm")) ")")) (comint-send-input)
               (message (concat "DONE. Subsequent test cases will be maintained in " current-test-suite-dir))

               ; Implicitly change mode line (status bar):
               (make-variable-buffer-local 'global-mode-string)
               (setq global-mode-string (concat "Unittest repository: " test-suite-name))
               ))
           (test-suite-dir
             (progn
               (beep)
               (message (concat "Cannot connect to " test-suite-name ". " "The test directory does not exist."))))
           (t 
             (progn
               (beep)
               (message (concat "Cannot connect to " test-suite-name)))))))

(defun temp-connect-to-testsuite (test-suite-name)
 "Temporarily connect to an existing testsuite. The parameter test-suite-name is the name of testsuite to which we want to connect."
  (interactive 
     (list (completing-read "Add testcase to which testsuite (space for possibilities): " 
                   (filter (function test-suite-exists-on-disk-p) test-suite-list) nil nil)))
    
   (let* ((test-suite-struct (assoc test-suite-name test-suite-list))
          (test-suite-dir (if test-suite-struct (test-suite-directory-of test-suite-struct) nil)))
     (cond ((and test-suite-dir (file-exists-p test-suite-dir))
             (progn
               (setq current-test-suite-name test-suite-name)
               (setq current-test-suite-dir test-suite-dir)
               ))
           (test-suite-dir
             (progn
               (beep)
               (message (concat "Cannot temporarily connect to " test-suite-name ". " "The test directory does not exist."))))
           (t 
             (progn
               (beep)
               (message (concat "Cannot temporarily connect to " test-suite-name)))))))

(defun connected-to-testsuite-p()
  (and (not (null current-test-suite-name)) (not (null current-test-suite-dir)))) 


(defun disconnect-from-current-testsuite ()
 "Disconnect from the current testsuite."
  (interactive)

  (if (and current-test-suite-name current-test-suite-dir)
      (progn
        (setq current-test-suite-name nil)
        (setq current-test-suite-dir nil)
        (setq global-mode-string "")
        (message "DONE."))
      (progn
         (message "You are not connected to any testsuite. Nothing done.")))
)

(defun temp-disconnect-from-current-testsuite ()
 "Disconnect from the current testsuie."
  (setq current-test-suite-name nil)
  (setq current-test-suite-dir nil)
)

; experimentally implemented.
(defun get-last-expression-from-command-interpreter ()
 "Get last expression from command interpreter - take from command input ring"
 (let* ((ring-first (car comint-input-ring)) 
        (ring-second (cadr comint-input-ring))
        (index (if (= 0 ring-first) ring-second ring-first))
        (vector (cddr comint-input-ring)))
  (depropertize-string (aref vector (- index 1)))))

(defun get-last-result-from-command-interpreter ()
  "Get last result from command interpreter - take from current buffer"
  (save-excursion
  (let ((p0 nil))
    (goto-char comint-last-input-end)
    (setq p0 (point))
    (forward-sexp 1)
    (buffer-substring p0 (point)))))

(defun get-result-from-mini-buffer ()
  "Prompt the user for a result - take from minibuffer"
  (read-from-minibuffer "Result: "))

; ---------------------------------------------------------------------------------------------------------------
; Source file support
; Administration and utilization of source file info associated with a test case.

(defvar source-info-comment "; A sequece of source file names (full file paths) associated with this testsuite" "The initial comment of a source-file-info file")

(defun source-files-under-test ()
  "Return the list of source files under test, if these files have been registered.
If not, just return nil"
 (if (not (or (null current-test-suite-name) (null current-test-suite-dir)))
     (let ((path-to-source-info-file (concat current-test-suite-dir "source-file-info")))
       (if (file-exists-p path-to-source-info-file)
           (file-read-all path-to-source-info-file)
           nil))
     nil))

(defun add-source-file-under-test (source-file-path)
  "Add source-file-path to the known source files of the current testsuite.
It is convenient, but not necessary, to keep track of the source files under test.
By having this information it becomes possible and flexible to navigate to a given function."
  (interactive "fAdd existing Scheme source file as a source file of current testsuite: ")
  (if (not (or (null current-test-suite-name) (null current-test-suite-dir)))
      (let ((path-to-source-info-file (concat current-test-suite-dir "source-file-info")))
        (if (file-exists-p path-to-source-info-file)
            (append-string-to-file path-to-source-info-file (concat CR (string-it-double (expand-file-name source-file-path))))
            (make-file-in-directory current-test-suite-dir "source-file-info" 
               (concat source-info-comment CR (string-it-double (expand-file-name source-file-path))))
        )
        (message "Done")
      )
      (message "Please first connect to a testsuite. Nothing done."))
)

(defun edit-source-file-info ()
  "Arrange that the source file info of the current testsuite (already existing) is brought up for editing"
  (interactive)
  (if (not (or (null current-test-suite-name) (null current-test-suite-dir)))
      (let ((path-to-source-info-file (concat current-test-suite-dir "source-file-info")))
        (if (file-exists-p path-to-source-info-file)
            (let ((w-list (window-list-of-frame)))
              (if (= 2 (length w-list))
                  (progn
                    (other-window 1)
                    (find-file path-to-source-info-file))
                  (progn
                    (delete-other-windows)
                    (split-window-vertically)
                    (other-window 1)
                    (find-file path-to-source-info-file)
                  )
               )
            )
            (message "Cannot edit. The source file info file does not exist: %s" path-to-source-info-file)
        )
        (message "Done")
      )
      (message "Please first connect to a testsuite. Nothing done.")) )

(defun find-function-in-source-file (fu-name)
  (interactive (list (name-under-point)))
  (let* ((defun-reg-exp (regular-expression 
                          `(or (concat "(" (zero-or-more " ") "define" (zero-or-more " ") ,fu-name)
                               (concat "(" (zero-or-more " ") "define" (zero-or-more " ") "(" (zero-or-more " ")  ,fu-name))))
         (source-files (source-files-under-test))
         (res (re-find-in-files defun-reg-exp source-files))
        )
    (cond (res
             (let ((file-path (car res))
                   (location (cdr res))
                   (w-list (window-list-of-frame))
                  )
               (do-in-other-window
                  (find-file file-path)
                  (goto-char location))
             )
           )
           ((null source-files) (message "Cannot find %s because no source file info is available" fu-name))
           (t (message "Cannot find %s" fu-name)))))

(defun edit-sourcefile-under-test ()
  (interactive)
  "Edit the source file under test. If more than one source file under test is registered, ask the user which of them to edit"
  (let ((source-files (source-files-under-test)))
    (cond ((= (length source-files) 1) 
            (do-in-other-window (find-file (car source-files))))
          ((> (length source-files) 1)
            (let* ((proper-source-file-names (mapcar (function (lambda (path) (file-name-proper (file-name-nondirectory path)))) source-files))
                   (selected-file-name (completing-read "Select source file: " proper-source-file-names nil t))
                   (selected-path (filter (function (lambda (file-path) (equal selected-file-name (file-name-proper (file-name-nondirectory file-path)))))
                                          source-files)) 
                  )
              (if (= (length selected-path) 1)
                  (do-in-other-window (find-file (car selected-path)))
                  (error "edit-sourcefile-under-test: Unexpected error"))))
          (t (error "Cannot edit source file. Are you connected to a testcase?")))))              


; ---------------------------------------------------------------------------------------------------------------

(defun construct-functional-test-case-pp (exp-string res-string assertion-string)
  "Return a string with a pretty printed functional testcase, ready to be added to a testcase file.
Do the necessary encoding of exp-string and res-string."
  (let ((encode-exp (must-encode-string-p exp-string))
        (encode-res (must-encode-string-p res-string))
        (test-case-id (make-time-string (current-time)))
       )
    (concat "(" "functional-testcase" CR
            "  " (string-it test-case-id) CR
            "  " "(" (if encode-exp "number-encoded" "raw-text") " "
                     (if encode-res "number-encoded" "raw-text") ")" CR
            "  " "(use-as-example)" CR
            "  " (string-it assertion-string)  CR
            "  " (string-it (if encode-exp (comint-string-encode exp-string) exp-string)) CR
            (if encode-exp 
                (concat "  " "; " (comment-of-encoded-string (comint-string-encode exp-string)) CR)
                "")
            "  " (string-it (if encode-res (comint-string-encode res-string) res-string)) CR
            (if encode-res 
                (concat "  " "; " (comment-of-encoded-string (comint-string-encode res-string)) CR)
                "")
            ")")))


(defun construct-functional-error-test-case-pp (exp-string)
  "Return a string with a pretty printed functional error testcase, ready to be added to a testcase file.
Do the necessary encoding of exp-string"
  (let ((encode-exp (must-encode-string-p exp-string))
        (test-case-id (make-time-string (current-time)))
       )
    (concat "(" "error-functional-testcase" CR
            "  " (string-it test-case-id) CR
            "  " "(" (if encode-exp "number-encoded" "raw-text") ")" CR
            "  " "(use-as-example)" CR
            "  " (string-it (if encode-exp (comint-string-encode exp-string) exp-string)) CR
            (if encode-exp 
                (concat "  " "; " (comment-of-encoded-string (comint-string-encode exp-string)) CR)
                "")
            "  " "error"
            ")")))

(defun construct-documentation-case-pp (exp-string documentation)
  "Return a string with a pretty printed documentation case, ready to be added to a testcase file.
Do the necessary encoding of exp-string"
  (let ((encode-exp (must-encode-string-p exp-string))
        (test-case-id (make-time-string (current-time)))
       )
    (concat "(" "documentation-case" CR
            "  " (string-it test-case-id) CR
            "  " "(" (if encode-exp "number-encoded" "raw-text") ")" CR
            "  " (string-it (if encode-exp (comint-string-encode exp-string) exp-string)) CR
            "  " (string-it documentation)
            ")")))


; ---------------------------------------------------------------------------------------------------------------

; Add a functional test case to current test case file.
; The functional testcase is aggregated from an expression, value and assertion, 
; as represented by the three parameters.
(defun add-functional-test-case-to-current-test-suite (exp-string res-string assertion-string)
 (let* ((buf (generate-new-buffer "testcase-temp"))
        (test-case-file-path (concat current-test-suite-dir "testcases"))
       )
    (save-excursion
      (set-buffer buf)
      (insert (construct-functional-test-case-pp exp-string res-string assertion-string))
      (insert CR CR)
      (append-to-file (point-min) (point-max) test-case-file-path)
      (kill-buffer buf))))

(defun add-functional-error-test-case-to-current-test-suite (exp-string)
 (let* ((buf (generate-new-buffer "testcase-temp"))
        (test-case-file-path (concat current-test-suite-dir "testcases"))
       )
    (save-excursion
      (set-buffer buf)
      (insert (construct-functional-error-test-case-pp exp-string))
      (insert CR CR)
      (append-to-file (point-min) (point-max) test-case-file-path)
      (kill-buffer buf))))

(defun add-documentation-case-to-current-test-suite (exp-string documentation)
 (let* ((buf (generate-new-buffer "testcase-temp"))
        (test-case-file-path (concat current-test-suite-dir "testcases"))
       )
    (save-excursion
      (set-buffer buf)
      (insert (construct-documentation-case-pp exp-string documentation))
      (insert CR CR)
      (append-to-file (point-min) (point-max) test-case-file-path)
      (kill-buffer buf))))

; ---------------------------------------------------------------------------------------------------------------


(defun make-time-string (time-list)
  (concat (int-to-string (car time-list)) "-" (int-to-string (cadr time-list))))


; ---------------------------------------------------------------------------------------------------------------
; Generation of SchemeUnit Scheme file from testcases files.

(defun derive-schemeunit-test-suite (test-suite-dir)
  "Derive and make a schemeunit test file from the current testsuite"
  (if (file-exists-p test-suite-dir)
      (let* ((test-case-file-name (concat test-suite-dir "testcases"))
             (test-setup-file-name (concat test-suite-dir "setup.scm"))
             (test-teardown-file-name (concat test-suite-dir "teardown.scm"))
             (imperative-testcases-file-name (concat test-suite-dir "imperative-testcases.scm"))
             (test-case-list (file-read-all test-case-file-name))
             (test-case-list-lgt (length test-case-list))
             (schemeunit-test-case-list (mapcar2 (function testcase-to-schemeunit-testcase) test-case-list (number-interval 1 test-case-list-lgt)))
             (schemeunit-functional-test-case-aggregated-string (accumulate-right (function concat) "" schemeunit-test-case-list))
             (schemeunit-imperative-testcases-string (if (file-exists-p imperative-testcases-file-name) (file-read-string imperative-testcases-file-name) ""))
             (schemeunit-test-setup (file-read-string test-setup-file-name))
             (schemeunit-test-tear-down (file-read-string test-teardown-file-name)) 
             (buf nil)
             )
                                        ; Somehow include setup.scm and teardown.scm in the generated testsuite.scm scheme file.
        (setq buf
              (make-a-file-from-laml-template 
               "testsuite.scm"
               test-suite-dir
               "schemeunit-skeleton-1"
               'scheme-mode
               (list
                (list "TEST-SETUP-HERE" schemeunit-test-setup)
                (list "FUNCTIONAL-TEST-CASES-HERE" schemeunit-functional-test-case-aggregated-string)
                (list "IMPERATIVE-TEST-CASES-HERE" schemeunit-imperative-testcases-string)
                (list "TEST-TEAR-DOWN-HERE" schemeunit-test-tear-down)
                )
                (concat test-suite-software-dir "templates/")
         ))
        (kill-buffer buf))))

(defun derive-schemeunit-test-suite-with-single-testcase (the-testcase test-suite-dir)
  "Derive and make a schemeunit test file with a single (imperative) testcase, namely the parameter the-testcase"
  (if (file-exists-p test-suite-dir)
      (let* ((test-setup-file-name (concat test-suite-dir "setup.scm"))
             (test-teardown-file-name (concat test-suite-dir "teardown.scm"))
             (schemeunit-test-setup (file-read-string test-setup-file-name))
             (schemeunit-test-tear-down (file-read-string test-teardown-file-name)) 
             (buf nil)
             )
                                        ; Somehow include setup.scm and teardown.scm in the generated testsuite.scm scheme file.
        (setq buf
              (make-a-file-from-laml-template 
               "testsuite.scm"
               test-suite-dir
               "schemeunit-skeleton-1"
               'scheme-mode
               (list
                (list "TEST-SETUP-HERE" schemeunit-test-setup)
                (list "FUNCTIONAL-TEST-CASES-HERE" "")
                (list "IMPERATIVE-TEST-CASES-HERE" the-testcase)
                (list "TEST-TEAR-DOWN-HERE" schemeunit-test-tear-down)
                )
                (concat test-suite-software-dir "templates/")
         ))
        (kill-buffer buf))))


; ---------------------------------------------------------------------------------------------------------------
; New format for testcase entries (list structure). As of March 2, 2007.
; 
; The general format:
; 
;   (<type-of-testcase>
;    <testcase-id>
;    <testcase-encoding>
;    <example-info>
;    <type-dependent-rest-stuff>
;   )
; 
; <example-info> is the list (use-as-example n1 n2 ...)
; where ni are names of functions in which testcase is used as an example.
; 
; Functional testcase format:
; 
;   (functional-testcase
;    <testcase-id>
;    (<expr-encoding> <value-encoding>)
;    <example-info>
;    <assertion>
;    <expression>
;    <value>
;   )
;
; Here follows the error functional test case
; 
;   (error-functional-testcase
;    <testcase-id>
;    (<expr-encoding>)
;    <example-info>
;    <expression>
;    <kind-of-exception>
;   )
; 
; <expr-encoding> and <value-encoding> are either raw-text or number-encoded.
; Expressions and values are number-encoded if they contain characters which will
; cause problems for the Emacs Lisp reader.

; ---------------------------------------------------------------------------------------------------------------
; No constructors of testcases, because they are constructed as pretty printed lists by the function 
; add-functional-test-case-to-current-test-suite

; Selectors of testcases:

(defun test-case-type-of (testcase-struct)
  "Return the type of a testcase. Currently either functional-testcase or error-functional-testcase"
  (car testcase-struct))

(defun test-case-name-of (testcase-struct)
  "Return the id of a testcase"
  (cadr testcase-struct))

(defun test-case-encoding-of (testcase-struct)
  "Return the encoding of a testcase. A list of one or two symbols depending on the type of the testcase.
   The first gives the encoding of the expression. The last (if present) give the encoding of the value.
   Symbols: raw-text or number-encoded."
  (car (cdr (cdr testcase-struct))))

(defun test-case-example-info-of (testcase-struct)
  "Return example info (a list) of the testcase"
  (nth 3 testcase-struct))

(defun test-case-expression-of (testcase-struct)
  "Return the expression of a testcase"
  (cond ((eq (test-case-type-of testcase-struct) 'functional-testcase)
           (nth 5 testcase-struct))
        ((eq (test-case-type-of testcase-struct) 'error-functional-testcase)
           (nth 4 testcase-struct))
        ((eq (test-case-type-of testcase-struct) 'documentation-case)
           (nth 3 testcase-struct))
        (t (error "test-case-expression-of: Invalid access to expression of testcase."))))

(defun test-case-result-of (testcase-struct)
  "Return the value of a functional testcase"
  (cond ((eq (test-case-type-of testcase-struct) 'functional-testcase)
           (nth 6 testcase-struct))
        (t (error "test-case-result-of: Invalid access to result of testcase."))))

(defun test-case-assertion-of (testcase-struct)
  "Return the assertion of a functional testcase"
  (cond ((eq (test-case-type-of testcase-struct) 'functional-testcase)
           (nth 4 testcase-struct))
        (t (error "test-case-assertion-of: Invalid access to assertion of testcase."))))

(defun test-case-kind-of-exception-of (testcase-struct)
  "Return the kind-of-exception of an error functional testcase"
  (cond ((eq (test-case-type-of testcase-struct) 'error-functional-testcase)
           (nth 5 testcase-struct))
        (t (error "test-case-kind-of-exception-of: Invalid access to kind-of-exception."))))

; ---------------------------------------------------------------------------------------------------
; Extended selectors - doing decoding if necessary. 
; Naming of the functions:  real-... versions.

(defun real-test-case-expression-of (testcase-struct)
  "Return the expression of a testcase. Decoded if necessary."
  (let ((enc (test-case-encoding-of testcase-struct)))
    (cond ((eq (car enc) 'raw-text)
             (test-case-expression-of testcase-struct))
          ((eq (car enc) 'number-encoded)
             (comint-string-decode (test-case-expression-of testcase-struct)))
          (t (error "real-test-case-expression-of: Unknown encoding.")))))


(defun real-test-case-result-of (testcase-struct)
  "Return the value of a functional testcase. Decoded if necessary"
  (if (eq 'functional-testcase (test-case-type-of testcase-struct))
      (let ((enc (test-case-encoding-of testcase-struct)))
        (cond ((eq (cadr enc) 'raw-text)
               (test-case-result-of testcase-struct))
              ((eq (cadr enc) 'number-encoded)
               (comint-string-decode (test-case-result-of testcase-struct)))
              (t (error "real-test-case-result-of: Unknown encoding."))))
      (error "real-test-case-result-of applied on wrong type of test case")))



; ---------------------------------------------------------------------------------------------------------------   

(defun must-encode-string-p (str)
  "Return whether str - a comint expression or value - should be number-encoded."
  (let ((char-list (string-to-list str))) 
    (catch 'res
      (mapcar
        (function 
          (lambda (ch) 
             (if (or (member ch '(34 92 35))    ; double quote, backslash, or hash 
                     (< ch 32) (> ch 126))
                 (throw 'res t))))
         char-list)
      nil)))


(defun testcase-to-schemeunit-testcase (testcase n)
  "Convert testcase, as represented in a testcase file, to a string that represents a schemeunit testcase"
  (cond ((eq 'functional-testcase (test-case-type-of testcase))
            (let ((name (test-case-name-of testcase))
                  (expr (real-test-case-expression-of testcase))
                  (res  (real-test-case-result-of testcase))
                  (assertion-fn (test-case-assertion-of testcase))
                  (test-case-id-string (concat "test case" " " (int-to-string n)))
                 )
              (concat 
               "    " "("  "make-test-case" " " (if name (string-it name) (string-it test-case-id-string))  CR
               "       " "(assert" " " assertion-fn " " expr " " "'" res ")" ")" CR)))

        ((eq 'error-functional-testcase (test-case-type-of testcase))
            (let ((name (test-case-name-of testcase))
                  (expr (real-test-case-expression-of testcase))
                  (test-case-id-string (concat "test case" " " (int-to-string n)))
                 )
              (concat 
               "    " "("  "make-test-case" " " (if name (string-it name) (string-it test-case-id-string))  CR
               "       " "(assert-exn" " " "void" " " "(lambda () " expr ")" ")" ")" CR)))

        ((eq 'documentation-case (test-case-type-of testcase))
            "")  ; does not affect the schemeunit test cases.

        (t (error (concat "testcase-to-schemeunit-testcase: Unknown type of testcase " (symbol-to-string (test-case-type-of testcase)))))))

(defun run-testsuite ()
  "Make a Schemeunit testsuite from the collected test cases in the current testsuite, and run it.
   Splits the screen to let you see the result of the test execution."
  (interactive)
  (if (and current-test-suite-name current-test-suite-dir)
      (progn
        (derive-schemeunit-test-suite current-test-suite-dir)
        (let* ((buf-name "test-run-report")
               (buf (get-buffer buf-name)))
          (if buf (save-excursion (set-buffer buf) (toggle-read-only -1) (erase-buffer)))
          (testing-sync-process-file (concat current-test-suite-dir "testsuite.scm"))
          (set-mode-of-buffer buf-name 'test-run-report-mode t)
        )

        (other-window 1)

        (message "DONE."))
      (message "The current testsuite is not defined.  Please connect to a testsuite."))
)

; To general:
(defun set-mode-of-buffer (buffer-name mode &optional read-only-p)
  "Assign mode to buf"
  (save-excursion
     (set-buffer (get-buffer buffer-name))
     (apply mode nil)
     (if read-only-p (toggle-read-only))))


(defun testing-sync-process-file (file &optional processing-mode)
  "Process a Scheme file synchronously.  The parameter is assumed to be a full path to a Scheme file.
As feedback, show the Scheme output while processing. The presentation of the processing
can be suppressed by passing the symbol silent as processing mode. "
  (let* ((path (file-name-directory file))
         (filename-without-extension (file-name-proper (file-name-nondirectory file)))
         (file-extension (file-name-extension (file-name-nondirectory file)))
         (out-buf (get-buffer-create "test-run-report")))

    (if (not (and processing-mode (eq processing-mode 'silent)))
        (do-in-other-window 
         (show-buffer (selected-window) out-buf)
         (set-buffer out-buf)
         (toggle-read-only -1)  ; now writable
         (erase-buffer)
         (insert (concat "Processing file " file " with " (symbol-to-string scheme-system) CR)))
    )

    (call-testing-process path filename-without-extension file-extension out-buf)))

; Adapted from call-laml-process. Works only for MzScheme.
(defun call-testing-process (dir file-without-extension extension output-buffer)
 "Start an synchronous LAML process on the file characterized by the first three parameters. Show output in output-buffer.
This stalls the e-lisp execution until the LAML process terminates."
 (let* ((file-path (concat dir file-without-extension "." extension))
       )
  (call-process scheme-exec nil output-buffer nil  "-v" "-d" file-path file-without-extension dir)
  
 )
)



(defun document-testsuite ()
  "Make HTML documentation of the current testsuite. This feature depends on LAML, and can only be used if you have a running LAML system."
  (interactive)

  (if (and current-test-suite-name current-test-suite-dir)
      (let ((doc-prog-name "generate-documentation.scm"))
  
        (generate-scheme-testsuite-generation-program current-test-suite-name current-test-suite-dir doc-prog-name)

        ; run it:
        (let ((buf (get-buffer "laml-output")))
          (if buf (erase-named-buffer buf))
          (laml-sync-process-file (concat current-test-suite-dir doc-prog-name))
          (other-window 1)
        )

        ; delete it afterwards:
        (delete-file (concat current-test-suite-dir doc-prog-name))
 
        (let ((abs-file-path-to-url (absolute-file-path-to-url (concat current-test-suite-dir "documentation.html"))))
          (if abs-file-path-to-url
              (progn
                (kill-new (concat abs-file-path-to-url))
                (message "The URL of the documentation file has been copied to the clipboard"))))
      )
      (progn
         (beep)
         (message "The current testsuite is not defined. Please connect to a testsuite using  M-x connect-to-testsuite"))))


(defun generate-scheme-testsuite-generation-program (test-suite-name test-suite-dir doc-prog-name)
  "Make a Scheme file in file doc-prog-name that generates the documentation from the testcases in test-suite-dir."
  (make-a-file-from-laml-template 
       doc-prog-name
       test-suite-dir
       "schemeunit-documentation-generation"
       'scheme-mode
       (list
        (list "FULL-PATH-TO-TESTCASES" (concat test-suite-dir "testcases"))
        )
       (concat test-suite-software-dir "templates/")))



(defun run-all-testsuites ()
  "Make Schemeunit test suites from the collected test cases in all registered and available testsuites.
Run these testsuites.
Splits the screen to let you see the result of the test executions."
  (interactive)

  (delete-other-windows)
  (let ((buf (get-buffer "test-run-report")))
   (if buf (save-excursion (set-buffer buf) (toggle-read-only -1) (erase-buffer))))

   (mapcar
     (function
      (lambda (test-suite)
       (let ((test-suite-dir (test-suite-directory-of test-suite)))
        (derive-schemeunit-test-suite test-suite-dir)
        (testing-sync-process-file (concat test-suite-dir "testsuite.scm") 'silent))))
      test-suite-list)


  (split-window-vertically)
  (other-window 1) 
  (show-buffer (selected-window) (get-buffer "test-run-report"))
  (save-excursion (set-buffer (get-buffer "test-run-report")) (toggle-read-only 1))
  (other-window 1) 

  (message "DONE.")
)

(defun edit-testsuite()
  "Arrange that the testsuite is edited in a split screen setup."
  (interactive)
  (if current-test-suite-dir
      (let ((full-file-path (concat current-test-suite-dir "testcases")))
        (if (get-buffer "testcases") (kill-buffer (get-buffer "testcases")))
        (do-in-other-window
          (find-file full-file-path)
          (goto-char (point-max))
          (testcases-mode)
          (other-window 1)
        )
      )
      (progn
         (beep)
         (message "Current test suite is not defined. Use  M-x connect-to-testsuite"))))

(defun show-schemeunit-testsuite ()
   "Arrange that the actual, generated SchemeUnit testsuite is showed in a split screen setup."
  (interactive)
  (if current-test-suite-dir
      (let ((full-file-path (concat current-test-suite-dir "testsuite.scm")))
        (if (get-buffer "testsuite.scm") (kill-buffer (get-buffer "testsuite.scm")))
        (do-in-other-window
          (find-file full-file-path)
          (toggle-read-only 1)
          (goto-char (point-max))
          (other-window 1)
        )
      )
      (progn
         (beep)
         (message "Current testsuite is not defined. Use  M-x connect-to-testsuite"))))


(defun show-schemeunit-test-report ()
   "Arrange that the most recent test run report showed in a split screen setup."
  (interactive)
  (if (get-buffer "test-run-report")
      (do-in-other-window
          (show-buffer (selected-window) (get-buffer "test-run-report"))
          (other-window 1)
        )
      (progn
         (beep)
         (message "Cannot find a 'text-run-report' buffer"))))

(defun show-imperative-testcases ()
  "Show the explicitly defined imperative testcases"
  (interactive)
  (if current-test-suite-dir
      (let ((full-file-path (concat current-test-suite-dir "imperative-testcases.scm")))
        (if (get-buffer "imperative-testcases.scm") (kill-buffer (get-buffer "imperative-testcases.scm")))
        (do-in-other-window
          (find-file full-file-path)
          (imperative-testcase-mode)
          (goto-char (point-max))
        )
      )
      (progn
         (beep)
         (message "Current testsuite is not defined. Use  M-x connect-to-testsuite"))))


(defun edit-test-suite-setup ()
  "Arrange that the setup file of the current test suite is edited in split screen setup."
  (interactive)
  (if current-test-suite-dir
      (let ((full-file-path (concat current-test-suite-dir "setup.scm")))
        (do-in-other-window
          (find-file full-file-path)
          (other-window 1)
        )
      )
      (progn
         (beep)
         (message "Current testsuite is not defined. Use  M-x connect-to-testsuite"))))

(defun edit-test-suite-teardown()
  "Arrange that the teardown file of the current test suite is edited in split screen setup."
  (interactive)
  (if current-test-suite-dir
      (let ((full-file-path (concat current-test-suite-dir "teardown.scm")))
        (do-in-other-window         
          (find-file full-file-path)
          (other-window 1)
        )
      )
      (progn
         (beep)
         (message "Current testsuite is not defined. Use  M-x connect-to-testsuite"))))


(defun info-current-testsuite ()
  "Provide some info about the current test suite in the other window"
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  (other-window 1)
  (let ((buf (get-buffer-create "*testsuite-info*")))
    (set-buffer buf)
    (erase-buffer)
    (if (and current-test-suite-name current-test-suite-dir)
        (progn
          (insert-info-testsuite (current-testsuite-structure))
          (insert "Added testcases will be part of the testsuite listed above."))
        (progn
          (insert "The current testsuite is not defined.") (insert CR)
          (insert "Use  M-x connect-to-testsuite  or the Unit Testing menu \"Connect to testsuite...\" to connect a testsuite.") (insert CR)
          (insert CR) ))
    (show-buffer (selected-window) buf))
  (other-window 1))

(defun info-all-testsuites ()
  "Provide info on all testsuites in test-suite-list"
  (interactive) 
 (let* ((reachable-test-suite-list
          (filter (function test-suite-exists-on-disk-p) test-suite-list))
        (unreachable-test-suite-list
          (filter (function (lambda (ts) (not (test-suite-exists-on-disk-p ts)))) test-suite-list))
        (lgt-reachable-test-suite-list (length reachable-test-suite-list))
      )
  (delete-other-windows)
  (split-window-vertically)
  (other-window 1)
  (let ((buf (get-buffer-create "*testsuite-info*")))
    (set-buffer buf)
    (erase-buffer)
    (if (= 0 lgt-reachable-test-suite-list)
        (progn
          (insert "There are no registered test suites.") (insert CR)
          (insert "Use  M-x make-test-suite  for creation of a single testsuite.") 
          (insert "Use  M-x find-and-register-testsuites-in-dir  for bulk registration of existing testcases") 
          (insert CR) (insert CR))
        (progn
         (if (<= lgt-reachable-test-suite-list 1)
             (insert (concat "There is only one testsuite available:"))
             (insert (concat "There are " (int-to-string lgt-reachable-test-suite-list) " " "registered testsuites available" ":")))
         (insert CR) (insert CR)
         (mapcar
          (function insert-info-testsuite)
          reachable-test-suite-list) 
         (insert CR)
         (if (< lgt-reachable-test-suite-list (length test-suite-list))
             (progn
               (insert (concat (int-to-string (- (length test-suite-list) lgt-reachable-test-suite-list))
                               " registered testsuite(s) cannot be reached:" CR))
               (mapcar (function (lambda (ts) (insert (concat "   " (test-suite-name-of ts) CR)))) unreachable-test-suite-list)
             ))
         (goto-char (point-min))))
    (show-buffer (selected-window) buf))
  (other-window 1)))

(defun test-suite-exists-on-disk-p (testsuite)
  "Does testsuite (represented by a testsuite structure (a list)) actually exist on disk."
  (file-exists-p (concat (test-suite-directory-of testsuite) "testcases")))

(defun is-testsuite-dir-p (dir)
  "Is dir actually a testsuite directory?. dir must be forward slash terminated."
  (let ((dir-name (directory-name-proper dir)))
  (and (> (length dir-name) 5) 
       (equal "test" (substring dir-name (- (length dir-name) 4)))
       (file-exists-p (concat dir "testcases")))))
       


(defun insert-info-testsuite (testsuite-structure)
 (let* ((test-suite-dir (test-suite-directory-of testsuite-structure))
        (test-suite-name (test-suite-name-of testsuite-structure))
        (test-case-file-name (concat test-suite-dir "testcases"))
        (test-case-list (file-read-all test-case-file-name))
        )
   (insert "INFO ABOUT TESTSUITE: ")  (insert CR)
   (insert "  Name: " test-suite-name) (insert CR) 
   (insert "  Location: " test-suite-dir) (insert CR)
   (insert "  Number of testcases: " (number-to-string (length test-case-list))) (insert CR) (insert CR)
   (insert CR)))

(defun help-testing ()
  (interactive)
  (let ((help-buffer (if (get-buffer "*TestingHelp*") (get-buffer "*TestingHelp*") (generate-new-buffer "*TestingHelp*"))))
    (show-buffer (other-window 1) help-buffer)
    (set-buffer help-buffer)
    (toggle-read-only -1)
    (erase-buffer)
    (insert-file (concat test-suite-software-dir "testing-help.txt"))
    (toggle-read-only 1)
    (set-buffer-modified-p nil)
    (other-window 1)))

(defun testcases-help ()
  (interactive)
  (let ((help-buffer (if (get-buffer "*TestingHelp*") (get-buffer "*TestingHelp*") (generate-new-buffer "*TestingHelp*"))))
    (show-buffer (other-window 1) help-buffer)
    (set-buffer help-buffer)
    (toggle-read-only -1)
    (erase-buffer)
    (insert-file (concat test-suite-software-dir "testcases-help.txt"))
    (toggle-read-only 1)
    (set-buffer-modified-p nil)
    (other-window 1)))



; ---------------------------------------------------------------------------------------------------------------
; Find test case - navigation from for instance test run to the test case.

(defun find-testcase (testcase-id)
  "Find test case, given a selection (under point) of the unique test case id,
and show the testcase in the other window"
  (interactive (list (name-under-point)))

  ; First try the current testsuite - the most likely
  (let ((p (testsuite-contains-id (current-testsuite-structure) testcase-id))) 
    (if p
        (do-in-other-window
          (find-file (concat current-test-suite-dir "testcases"))
          (testcases-mode)
          (goto-char p)
        )
        (let ((tsl test-suite-list)             ; Try all of them - including current again...
                (done nil))
            (while (and (not (null tsl)) (not done))
              (let ((p (testsuite-contains-id (car tsl) testcase-id))) 
                (if p
                    (progn
                      (setq done t)
                      (do-in-other-window
                        (find-file (concat (test-suite-directory-of (car tsl)) "testcases"))
                        (testcases-mode)
                        (goto-char p)
                      )
                    )
                  (setq tsl (cdr tsl)))))))))

(defun get-testcase (testcase-id)
  "Locate the test case with testcase-id, and return the list structure that represents it."

  ; First try the current testsuite - the most likely
  (save-window-excursion
  (let ((p (testsuite-contains-id (current-testsuite-structure) testcase-id))) 
    (if p
        (save-excursion
          (find-file (concat current-test-suite-dir "testcases"))
          (goto-char p)
          (read (current-buffer))
        )
        (let ((tsl test-suite-list)             ; Next try all of them - including current again...
              (done nil)
              (result nil))
            (while (and (not (null tsl)) (not done))
              (let ((p (testsuite-contains-id (car tsl) testcase-id))) 
                (if p
                    (progn
                      (setq done t)
                      (save-excursion
                        (find-file (concat (test-suite-directory-of (car tsl)) "testcases"))
                        (goto-char p)
                        (setq result (read (current-buffer)))
                      )
                    )
                  (setq tsl (cdr tsl)))))
            result)))))

(defun delete-testcase (testcase-id)
  "Find and delete the test case with testcase-id."

  ; First try the current testsuite - the most likely
  (let ((p (testsuite-contains-id (current-testsuite-structure) testcase-id))) 
    (if p
        (do-in-other-window
          (find-file (concat current-test-suite-dir "testcases"))
          (goto-char p)
          (kill-sexp 1)
          (save-buffer)
          (other-window 1)
          (message "The testcase has been deleted")
        )
        (let ((tsl test-suite-list)             ; Try all of them - including current again...
                (done nil))
            (while (and (not (null tsl)) (not done))
              (let ((p (testsuite-contains-id (car tsl) testcase-id))) 
                (if p
                    (progn
                      (setq done t)
                      (do-in-other-window
                        (find-file (concat (test-suite-directory-of (car tsl)) "testcases"))
                        (goto-char p)
                        (kill-sexp 1)
                        (save-buffer)
                        (other-window 1)
                        (message "The testcase has been deleted")
                      )
                    )
                  (setq tsl (cdr tsl)))))
           (if (not done) (message "Cannot delete testcase."))))))


(defun expression-of-testcase-under-point ()
  "The point is located at the initial parenthesis of a test case. Return the tested expression (a text string)"
     (real-test-case-expression-of (testcase-under-point)))

(defun testcase-under-point ()
  "The point is located at the initial parenthesis of a test case. Return the testcase (as a parsed/read list)"
  (save-excursion
    (let ((p0 (point)))
      (forward-sexp 1)
      (let ((p1 (point)))
        (car (read-from-string (buffer-substring-no-properties p0 p1)))))))
           
               

(defun testsuite-contains-id (testsuite testcase-id)
  "Does the test case (like an entry in test-suite-list) contain testcase-id (of the form 17375-45207 (a string)).
If so, return the start position of the test case (the position of the start parenthesis). Else return nil"
  (if (get-buffer "some-testsuite") (kill-buffer (get-buffer "some-testsuite")))
  (save-excursion
   (let ((testcase-file (concat (test-suite-directory-of testsuite) "testcases")))
     (if (file-exists-p testcase-file) 
         (let ((buf (get-buffer-create "some-testsuite"))
	       (found nil)
	       (res nil))
	   (set-buffer buf)
	   (insert-file testcase-file) (goto-char (point-min))

	   (while (and (more-testcases-ahead) (not found))
	     (goto-next-testcase)
	     (goto-testcase-id)
	     (setq found (equal (name-under-point) testcase-id))
	     (if (not found) (progn (backward-char 1) (backward-up-list 1) (forward-sexp 1))))
	   (setq res (if found
			 (progn (backward-char 1) (backward-up-list 1) (point))
		       nil))
	   (kill-buffer buf)
	   res)
         nil))))
    

; Helping function to testsuite-contains-id
(defun more-testcases-ahead ()
  "Given a point in a testcases buffer. Are there more test cases ahead."
  (save-excursion
   (search-forward "(" nil t)))

; Helping function to testsuite-contains-id
(defun goto-next-testcase ()
  "We have just passed a test case. Navigate point the the beginning of the next testcase"
  (while (not (looking-at "(")) (forward-char 1)) )

; Helping function to testsuite-contains-id
(defun goto-testcase-id ()
  "Given point at the inital parenthesis of a test case, move point to the first char of the test case id"
  (forward-char 1)
  (forward-sexp 2) (backward-sexp 1)
  (forward-char 1)) 
    


(defun update-testcases-in-other-window-if-there()
  (other-window 1)
  (if (equal "testcases" (buffer-name (current-buffer)))
      (progn
        (revert-buffer nil t)
        (goto-char (point-max))))
  (other-window 1))
    


; ---------------------------------------------------------------------------------------------------------------
; Acceptance and rejection commands for test results:

(defun ok ()
  "Accepts the result as being equal?. Register test case"
  (interactive)
  (let ((test-expression (get-last-expression-from-command-interpreter))
        (test-result (get-last-result-from-command-interpreter))
        (used-assertion "equal?"))
    (if (connected-to-testsuite-p)
        (progn
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (update-testcases-in-other-window-if-there)
          (message (concat "Test case added to suite " current-test-suite-name "." " Comparison is done by the Scheme predicate equal?")))
        (progn
          (call-interactively (function temp-connect-to-testsuite))
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (temp-disconnect-from-current-testsuite)))))

(defun ok-numerical-equal ()
  "Accepts the result as being =. Register test case"
  (interactive)
  (let ((test-expression (get-last-expression-from-command-interpreter))
        (test-result (get-last-result-from-command-interpreter))
        (used-assertion "="))
    (if (connected-to-testsuite-p)
        (progn
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (update-testcases-in-other-window-if-there)
          (message (concat "Test case added to suite " current-test-suite-name "." " Comparison is done by the Scheme predicate =")))
        (progn
          (call-interactively (function temp-connect-to-testsuite))
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (temp-disconnect-from-current-testsuite)))))
        

(defun ok-eq ()
  "Accepts the result as being eq?. Register test case"
  (interactive)
  (let ((test-expression (get-last-expression-from-command-interpreter))
        (test-result (get-last-result-from-command-interpreter))
        (used-assertion "eq?"))
    (if (connected-to-testsuite-p)
        (progn
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (update-testcases-in-other-window-if-there)
          (message (concat "Test case added to suite " current-test-suite-name "." " Comparison is done by the Scheme predicate eq?")))
        (progn
          (call-interactively (function temp-connect-to-testsuite))
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (temp-disconnect-from-current-testsuite)))))

(defun ok-eqv ()
  "Accepts the result as being eqv?. Register test case"
  (interactive)
  (let ((test-expression (get-last-expression-from-command-interpreter))
        (test-result (get-last-result-from-command-interpreter))
        (used-assertion "eqv?"))
    (if (connected-to-testsuite-p)
        (progn
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (update-testcases-in-other-window-if-there)
          (message (concat "Test case added to suite " current-test-suite-name "." " Comparison is done by the Scheme predicate eqv?")))
        (progn
          (call-interactively (function temp-connect-to-testsuite))
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (temp-disconnect-from-current-testsuite)))))

(defun ok-other (predicate-name)
  "Accepts the result as being equivalent, using another predicate.
When used interactively, this function prompts you for the name of the predicate.
Register test case"
  (interactive "sThe name of a known, binary Scheme predicate: ")
  (let ((test-expression (get-last-expression-from-command-interpreter))
        (test-result (get-last-result-from-command-interpreter))
        (used-assertion predicate-name))
    (if (connected-to-testsuite-p)
        (progn
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (update-testcases-in-other-window-if-there)
          (message (concat "Test case added to suite " current-test-suite-name "." " Comparison is done by the Scheme predicate " predicate-name " which is supposed to be loaded.")))
        (progn
          (call-interactively (function temp-connect-to-testsuite))
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (temp-disconnect-from-current-testsuite)))))

(defun is-error ()
  "The entered expression gives an error. Register test case"
  (interactive)
  (let ((test-expression (get-last-expression-from-command-interpreter))
       )
    (if (connected-to-testsuite-p)
        (progn
          (add-functional-error-test-case-to-current-test-suite test-expression)
          (update-testcases-in-other-window-if-there)
          (message (concat "Test case added to suite " current-test-suite-name "." " The test cases succeeds if the expression causes an error.")))
        (progn
          (call-interactively (function temp-connect-to-testsuite))
          (add-functional-error-test-case-to-current-test-suite test-expression)
          (temp-disconnect-from-current-testsuite)))))

(defun for-documentation-purposes()
  "Provide documentation for the entered expression. Register a documentation case." 
  (interactive)
  (let ((test-expression (get-last-expression-from-command-interpreter))
        (documentation (read-from-minibuffer "Documentation: "))
       )
    (if (connected-to-testsuite-p)
        (progn
          (add-documentation-case-to-current-test-suite test-expression documentation)
          (update-testcases-in-other-window-if-there)
          (message (concat "Documentation case added to suite " current-test-suite-name "." " The documentation case is neutral relative to testing.")))
        (progn
          (call-interactively (function temp-connect-to-testsuite))
          (add-documentation-case-to-current-test-suite test-expression documentation)
          (temp-disconnect-from-current-testsuite)))))


; ---------------------------------------------------------------------------------------------------------------
; Acceptance and rejection commands for test results, which also prompts for a result.
; Intended for test-driven development where testcases are written before the functions.

(defun ok-prompt-for-result ()
  "Register test case, but first prompt for result."
  (interactive)
  (let ((test-expression (get-last-expression-from-command-interpreter))
        (test-result (get-result-from-mini-buffer))
        (used-assertion "equal?"))
    (if (connected-to-testsuite-p)
        (progn
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (update-testcases-in-other-window-if-there)
          (message (concat "Test case added to suite " current-test-suite-name "." " Comparison is done by the Scheme predicate equal?")))
        (progn
          (call-interactively (function temp-connect-to-testsuite))
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (temp-disconnect-from-current-testsuite)))))

(defun ok-numerical-equal-prompt-for-result ()
  "Accepts the result as being =. Register test case"
  (interactive)
  (let ((test-expression (get-last-expression-from-command-interpreter))
        (test-result (get-result-from-mini-buffer))
        (used-assertion "="))
    (if (connected-to-testsuite-p)
        (progn
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (update-testcases-in-other-window-if-there)
          (message (concat "Test case added to suite " current-test-suite-name "." " Comparison is done by the Scheme predicate =")))
        (progn
          (call-interactively (function temp-connect-to-testsuite))
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (temp-disconnect-from-current-testsuite)))))
        

(defun ok-eq-prompt-for-result ()
  "Accepts the result as being eq?. Register test case"
  (interactive)
  (let ((test-expression (get-last-expression-from-command-interpreter))
        (test-result (get-result-from-mini-buffer))
        (used-assertion "eq?"))
    (if (connected-to-testsuite-p)
        (progn
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (update-testcases-in-other-window-if-there)
          (message (concat "Test case added to suite " current-test-suite-name "." " Comparison is done by the Scheme predicate eq?")))
        (progn
          (call-interactively (function temp-connect-to-testsuite))
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (temp-disconnect-from-current-testsuite)))))

(defun ok-eqv-prompt-for-result ()
  "Accepts the result as being eqv?. Register test case"
  (interactive)
  (let ((test-expression (get-last-expression-from-command-interpreter))
        (test-result (get-last-result-from-command-interpreter))
        (used-assertion "eqv?"))
    (if (connected-to-testsuite-p)
        (progn
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (update-testcases-in-other-window-if-there)
          (message (concat "Test case added to suite " current-test-suite-name "." " Comparison is done by the Scheme predicate eqv?")))
        (progn
          (call-interactively (function temp-connect-to-testsuite))
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (temp-disconnect-from-current-testsuite)))))

(defun ok-other-prompt-for-result (predicate-name)
  "Accepts the result as being equivalent, using another predicate.
When used interactively, this function prompts you for the name of the predicate.
Register test case"
  (interactive "sThe name of an existing binary Scheme predicate: ")
  (let ((test-expression (get-last-expression-from-command-interpreter))
        (test-result (get-result-from-mini-buffer))
        (used-assertion predicate-name))
    (if (connected-to-testsuite-p)
        (progn
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (update-testcases-in-other-window-if-there)
          (message (concat "Test case added to suite " current-test-suite-name "." " Comparison is done by the Scheme predicate " predicate-name " which is supposed to be loaded.")))
        (progn
          (call-interactively (function temp-connect-to-testsuite))
          (add-functional-test-case-to-current-test-suite test-expression test-result used-assertion)
          (temp-disconnect-from-current-testsuite)))))



; ---------------------------------------------------------------------------------------------------------------
; Handling of testing of imperative programs - procedures.
; Incomplete stuff...

(defun add-imperative-testcase ()
  "Open a buffer which allows for definition of an imperative testcase.
This covers in reality a lower-level SchemeUnit testcase"
  (interactive)

  (let ((buf (get-buffer-create "imperative-test-case")))
    (set-buffer buf)
    (erase-buffer)
    (insert-file (concat test-suite-software-dir "templates/" "imperative-testcase.tpl"))
    (goto-char (point-min))
    (replace-string "id" (make-time-string (current-time)))
    (goto-char (point-min))
    (imperative-testcase-mode)
    (do-in-other-window (show-buffer (selected-window) buf)))
 
)


; ---------------------------------------------------------------------------------------------------------------
; Unit testing in relation to LAML SchemeDoc.

; LAML Version 32: Not activated any more.
(defun most-likely-documentation-of-testsuite (testsuite-struct)
  "Return a full path to SchemeDoc resource (manlsp file) relevant for the current test suite.
Works if the name of the testsuite is the same as the name of the SchemeDoc manual file, and if
the manual is located at ../man/name-of-test-suite.
Return nil if the documentation resource cannot be located."
  (let* ((name (test-suite-name-of testsuite-struct))
         (dir  (test-suite-directory-of testsuite-struct))
         (parent-dir (parent-directory dir))
         (candidate-schemedoc-path (concat parent-dir "man/" name))
         (candidate-schemedoc-path-with-ext (concat candidate-schemedoc-path "." "manlsp")))
    (if (file-exists-p candidate-schemedoc-path-with-ext)
        candidate-schemedoc-path
        nil)))


; ---------------------------------------------------------------------------------------------------------------
; Dired utility on testcases

(defun testcases-dired()
  "Set up a dired directory editor of all (actually existing) testcases in all registered testsuites.
The starting point is the list test-suite-list."
  (interactive)
  (dired 
    (cons "Testsuites"
        (mapcar 
          (function (lambda (testsuite) (concat (test-suite-directory-of testsuite) "testcases")))
          (filter (function (lambda (ts) (file-exists-p (concat (test-suite-directory-of ts) "testcases")))) test-suite-list))))) 


; ---------------------------------------------------------------------------------------------------------------
; Setup of the Testing menu in inferior lisp mode. Also some 
; If possible, move to Scheme context...

(defun define-test-menu-in-comint()

  ; Tools menu contributions:  

  (define-key global-map [menu-bar tools scheme-unit-testing]
    (cons "Scheme Unit Testing" (make-sparse-keymap "Scheme Unit Testing")))

  (define-key  global-map [menu-bar tools scheme-unit-testing find-testcase]
      '("Find test case from ID" . find-testcase))


  ; The Unit Testing menu:
 
  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test]
    (cons "Unit Testing" (make-sparse-keymap "schemeunit-test")))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test testcases-help]
    '("Help about test cases" . testcases-help))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test help-testing]
    '("Help about interactive unit testing" . help-testing))
  (laml-define-key inferior-lisp-mode-map (list "\C-t?" "\C-c\C-t?") 'help-testing)

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test info-all-testsuites]
    '("Info about all registered test suites" . info-all-testsuites))
  (laml-define-key inferior-lisp-mode-map (list "\C-t\C-j" "\C-c\C-t\C-j") 'info-all-testsuites)

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test info-current-testsuite]
    '("Info about current test suite" . info-current-testsuite))
  (laml-define-key inferior-lisp-mode-map (list "\C-t\C-i" "\C-c\C-t\C-i") 'info-current-testsuite)

  (define-key  inferior-lisp-mode-map [menu-bar schemeunit-test separator-6]
    '("----"))

  (if (is-laml-installed)
      (progn
        (define-key inferior-lisp-mode-map [menu-bar schemeunit-test document-testsuite]
          '("Document current test suite" . document-testsuite))
        ))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test default-test-windows-setup]
    '("Default test windows setup" . (lambda () (interactive (switch-activity "scheme-testing")))))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test forced-saving-of-comint-command-list]
    '("Save comint history" . forced-saving-of-comint-command-list))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test open-previous-commands]
    '("Open comint history" . open-previous-commands))


  (define-key  inferior-lisp-mode-map [menu-bar schemeunit-test separator-5]
    '("----"))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test edit-source-file-under-test]
    '("Edit source file info" . edit-source-file-info))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test add-source-file-under-test]
    '("Add source file info..." . add-source-file-under-test))


  (define-key  inferior-lisp-mode-map [menu-bar schemeunit-test separator-8]
    '("----"))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test ok-other-prompt-for-result]
    '("Add a result and a functional test case: custom predicate..." . ok-other-prompt-for-result))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test ok-eqv-prompt-for-result]
    '("Add a result and a functional test case: eqv?" . ok-eqv-prompt-for-result))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test ok-eq-prompt-for-result]
    '("Add a result and a functional test case: eq?" . ok-eq-prompt-for-result))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test ok-numerical-equal-prompt-for-result]
    '("Add a result and a functional test case: =" . ok-numerical-equal-prompt-for-result))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test ok-prompt-for-result]
    '("Add a result and a functional test case: equal?" . ok-prompt-for-result))

  (laml-define-key inferior-lisp-mode-map (list "\C-t\C-p" "\C-c\C-t\C-p") 'ok-prompt-for-result)



  (define-key  inferior-lisp-mode-map [menu-bar schemeunit-test separator-7]
    '("----"))


;  (laml-define-key inferior-lisp-mode-map (list "\C-t\C-d" "\C-c\C-t\C-d") 'for-documentation-purposes)
;  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test for-documentation-purposes]
;    '("Add a documentation case" . for-documentation-purposes))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test add-imperative-testcase]
    '("Add an imperative test case..." . add-imperative-testcase))

  (laml-define-key inferior-lisp-mode-map (list "\C-t\C-e" "\C-c\C-t\C-e") 'is-error)
  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test is-error]
    '("Add an error test case" . is-error))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test ok-other]
    '("Add a functional test case: custom predicate..." . ok-other))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test ok-eqv]
    '("Add a functional test case: eqv?" . ok-eqv))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test ok-eq]
    '("Add a functional test case: eq?" . ok-eq))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test ok-numerical-equal]
    '("Add a functional test case: =" . ok-numerical-equal))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test ok]
    '("Add a functional test case: equal?" . ok))

  (laml-define-key inferior-lisp-mode-map (list "\C-t\C-t" "\C-c\C-t\C-t") 'ok)

  (define-key  inferior-lisp-mode-map [menu-bar schemeunit-test separator-4]
    '("----"))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test  find-function-in-source-file]
    '("Find selected function" . find-function-in-source-file))
  (laml-define-key inferior-lisp-mode-map (list "\C-t\C-f" "\C-c\C-t\C-f") 'find-function-in-source-file)

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test show-schemeunit-test-report]
    '("Open most recent test run report" . show-schemeunit-test-report))
  (laml-define-key inferior-lisp-mode-map (list "\C-tr" "\C-c\C-tr") 'show-schemeunit-test-report)

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test show-schemeunit-testsuite]
    '("Open most recent SchemeUnit test suite" . show-schemeunit-testsuite))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test show-imperative-testcases]
    '("Open imperative test cases" . show-imperative-testcases))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test edit-test-suite-teardown]
    '("Open current tear down file" . edit-test-suite-teardown))
  (laml-define-key inferior-lisp-mode-map (list "\C-t\C-z" "\C-c\C-t\C-z") 'edit-test-suite-teardown)

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test edit-test-suite-setup]
    '("Open current setup file" . edit-test-suite-setup))
  (laml-define-key inferior-lisp-mode-map (list "\C-t\C-x" "\C-c\C-t\C-x") 'edit-test-suite-setup)


  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test edit-sourcefile-under-test]
    '("Open source file under test" . edit-sourcefile-under-test))
  (laml-define-key inferior-lisp-mode-map (list "\C-te" "\C-c\C-te") 'edit-sourcefile-under-test)


  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test edit-test-suite]
    '("Open current test suite" . edit-testsuite))
  (laml-define-key inferior-lisp-mode-map (list "\C-t\C-y" "\C-c\C-t\C-y") 'edit-testsuite)


  (define-key  inferior-lisp-mode-map [menu-bar schemeunit-test separator-3a]
    '("----"))

;   (define-key inferior-lisp-mode-map [menu-bar schemeunit-test document-all-testsuites]
;     '("Document all testsuites" . document-all-testsuites))
;   (laml-define-key inferior-lisp-mode-map (list "\C-td" "\C-c\C-t\d") 'document-all-testsuites)

;  (if (is-laml-installed)
;      (define-key  inferior-lisp-mode-map [menu-bar schemeunit-test separator-3]
;        '("----")))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test run-all-testsuites]
    '("Run all test suites" . run-all-testsuites))
  (laml-define-key inferior-lisp-mode-map (list "\C-t\C-s" "\C-c\C-t\C-s") 'run-all-testsuites)

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test run-testsuite]
    '("Run current test suite" . run-testsuite))
  (laml-define-key inferior-lisp-mode-map (list "\C-t\C-r" "\C-c\C-t\C-r") 'run-testsuite)

  (define-key  inferior-lisp-mode-map [menu-bar schemeunit-test separator-2]
    '("----"))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test  deregister-unreachable-testsuites]
    '("Deregister all unreachable test suites" . deregister-unreachable-testsuites))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test deregister-all-testsuites]
    '("Deregister all test suites" . deregister-all-testsuites))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test deregister-testsuite]
    '("Deregister test suite..." . deregister-testsuite))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test register-testsuite]
    '("Register test suite..." . register-testsuite))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test q-find-and-register-testsuites-in-dir]
    '("Find and register test suites by query..." . q-find-and-register-testsuites-in-dir))
  (laml-define-key inferior-lisp-mode-map (list "\C-tf" "\C-c\C-tf") 'q-find-and-register-testsuites-in-dir)

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test find-and-register-testsuites-in-dir]
    '("Find and register test suites..." . find-and-register-testsuites-in-dir))
;  (laml-define-key inferior-lisp-mode-map (list "\C-t\C-f" "\C-c\C-t\C-f") 'find-and-register-testsuites-in-dir)

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test make-test-suite]
    '("Make new test suite..." . make-test-suite))
  (laml-define-key inferior-lisp-mode-map (list "\C-t\C-n" "\C-c\C-t\C-n") 'make-test-suite)

  (define-key  inferior-lisp-mode-map [menu-bar schemeunit-test separator-1]
    '("----"))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test disconnect-from-current-testsuite]
    '("Disconnect from current test suite" . disconnect-from-current-testsuite))

  (define-key inferior-lisp-mode-map [menu-bar schemeunit-test connect-to-testsuite]
    '("Connect to test suite..." . connect-to-testsuite))
  (laml-define-key inferior-lisp-mode-map (list "\C-t\C-c" "\C-c\C-t\C-c") 'connect-to-testsuite)


  ; To provide for access to SchemeDoc information from the Scheme REPL:
  (laml-define-key inferior-lisp-mode-map (list "\C-hf" "\C-c\C-hf") 'display-schemedoc-information)


)

; ---------------------------------------------------------------------------------------------------------------
;;; Test run report mode

(define-derived-mode 
        test-run-report-mode fundamental-mode "Test Run Report Mode" 
  "A mode used for presentation of test run results."

  (define-key test-run-report-mode-map [menu-bar schemeunit-test]
    (cons "Unit Testing" (make-sparse-keymap "schemeunit-test")))

  (define-key test-run-report-mode-map [menu-bar schemeunit-test testcases-help]
    '("Help on test cases" . testcases-help))

  (define-key test-run-report-mode-map [menu-bar schemeunit-test time-of-testcase-from-test-report]
    '("Date and time of test case" . time-of-testcase-from-test-report))
  (define-key test-run-report-mode-map "t" 'time-of-testcase-from-test-report)

  (define-key test-run-report-mode-map  [menu-bar schemeunit-test separator-3]
    '("----"))

  (define-key test-run-report-mode-map [menu-bar schemeunit-test evaluate-current-testcase-from-test-report]
    '("Evaluate current test case" . evaluate-current-testcase-from-test-report))
  (define-key test-run-report-mode-map "\C-\M-x" 'evaluate-current-testcase-from-test-report)

  (define-key test-run-report-mode-map [menu-bar schemeunit-test read-current-testcase-from-test-report]
    '("Read current test case" . read-current-testcase-from-test-report))
  (define-key test-run-report-mode-map "\C-\M-y" 'read-current-testcase-from-test-report)

 
  (define-key test-run-report-mode-map  [menu-bar schemeunit-test separator-2]
    '("----"))


  (define-key test-run-report-mode-map [menu-bar schemeunit-test delete-current-testcase-from-test-report]
    '("Delete test case" . delete-current-testcase-from-test-report))
  (define-key test-run-report-mode-map "D" 'delete-current-testcase-from-test-report)

  (define-key test-run-report-mode-map [menu-bar schemeunit-test show-and-delete-current-testcase-from-test-report]
    '("Show test case and delete it next" . show-and-delete-current-testcase-from-test-report))
  (define-key test-run-report-mode-map "d" 'show-and-delete-current-testcase-from-test-report)

  (define-key test-run-report-mode-map  [menu-bar schemeunit-test separator-1]
    '("----"))  

  (define-key test-run-report-mode-map [menu-bar schemeunit-test show-value-of-current-testcase-from-test-report]
    '("Show test value" . show-value-of-current-testcase-from-test-report))
  (define-key test-run-report-mode-map "v" 'show-value-of-current-testcase-from-test-report)

  (define-key test-run-report-mode-map [menu-bar schemeunit-test show-expression-of-current-testcase-from-test-report]
    '("Show test expression" . show-expression-of-current-testcase-from-test-report))
  (define-key test-run-report-mode-map "e" 'show-expression-of-current-testcase-from-test-report)

  (define-key test-run-report-mode-map [menu-bar schemeunit-test find-current-testcase-from-test-report]
    '("Find testcase" . find-current-testcase-from-test-report))
  (define-key test-run-report-mode-map "f" 'find-current-testcase-from-test-report)

  (define-key test-run-report-mode-map  [menu-bar schemeunit-test separator-0]
    '("----"))

  (define-key test-run-report-mode-map [menu-bar schemeunit-test goto-previous-problem-in-test-report]
    '("Goto previous problem" . goto-previous-problem-in-test-report))
  (define-key test-run-report-mode-map "p" 'goto-previous-problem-in-test-report)

  (define-key test-run-report-mode-map [menu-bar schemeunit-test goto-next-problem-in-test-report]
    '("Goto next problem" . goto-next-problem-in-test-report))
  (define-key test-run-report-mode-map "n" 'goto-next-problem-in-test-report)


)

(defun goto-next-problem-in-test-report ()
  "From the line with a testcase id, find the next error or failure line."
  (interactive)
  (let* ((reg-exp (regular-expression '(at-beginning-of-line (or "Error:" "Failure:"))))
         (search-res (re-search-forward reg-exp nil t)))
    (if search-res
        (progn (beginning-of-line 1) (next-line 2))
        (message "Cannot locate next error or failure"))))

(defun goto-previous-problem-in-test-report ()
  "From the line with a testcase id, find the previous error or failure line."
  (interactive)
  (previous-line 3)
  (let* ((reg-exp (regular-expression '(at-beginning-of-line (or "Error:" "Failure:"))))
         (search-res (re-search-backward reg-exp nil t)))
    (if search-res
        (progn (beginning-of-line 1) (next-line 2))
        (progn (next-line 3) ; compensation...
               (message "Cannot locate previous error or failure")))))


(defun find-current-testcase-from-test-report ()
  (interactive)
  (let ((testcase-id (name-under-point)))
    (find-testcase testcase-id)
    (other-window 1)))


(defun time-of-testcase-from-test-report ()
  "Show - in the minibuffer - the date and time of the currently selected testcase-id"
  (interactive)
  (save-excursion
    (let* ((id (name-under-point))
           (two-ints (mapcar (function as-number) (split-string id "-")))
           (time-list (decode-time (append two-ints '(0))))
          )
       (message "Date and time of test case (day-month-year hours:min): %d-%d-%d at %d:%d" (nth 3 time-list) (nth 4 time-list)  (nth 5 time-list)  (nth 2 time-list) (nth 1 time-list))
    )))

(defun evaluate-current-testcase-from-test-report ()
  "It is assumed that the point is within a testcase id. Transfer the testcase the other buffer (the Scheme interpreter) and evaluate it.
If necessary, this command brings the interactive Scheme shell up in the other window."
  (interactive)
   (let ((testcase (get-testcase (name-under-point))))
    (if testcase
        (let ((expr (real-test-case-expression-of testcase)))
	  (do-in-other-window
	   (show-buffer (selected-window) (get-buffer "*inferior-lisp*"))) ; point is now in interactive shell
	  (insert expr)
	  (comint-send-input)
	  (other-window 1))
        (message "Cannot find test case"))))

(defun read-current-testcase-from-test-report ()
  "Transfer the testcase under point to the other buffer (the Scheme interpreter). Do NOT evaluate it.
The point is assumed to INSIDE the testcase, or at the start parenthesis of the testcase."
  (interactive)
  (let ((testcase (get-testcase (name-under-point))))
    (if testcase
        (let ((expr (real-test-case-expression-of testcase)))
	  (do-in-other-window
	   (show-buffer (selected-window) (get-buffer "*inferior-lisp*"))) ; point is now in interactive shell
	  (insert expr)
	  )
        (message "Cannot find test case"))))

(defun show-expression-of-current-testcase-from-test-report ()
  (interactive)
  (let ((testcase (get-testcase (name-under-point))))
    (if testcase
        (let ((expr (real-test-case-expression-of testcase)))
          (message expr))
        (message "Cannot find test case"))))

(defun show-value-of-current-testcase-from-test-report ()
  (interactive)
  (let ((testcase (get-testcase (name-under-point))))
    (if testcase
        (let ((value (real-test-case-result-of testcase)))
          (message value))
        (message "Cannot find test case"))))

(defun delete-current-testcase-from-test-report ()
  (interactive)
  (save-window-excursion
    (delete-testcase (name-under-point))))

(defun show-and-delete-current-testcase-from-test-report ()
  (interactive)
  (let* ((testcase-id (name-under-point))
         (testcase (get-testcase testcase-id)))
    (if testcase
        (let ((expr (real-test-case-expression-of testcase)))
	  (do-in-other-window
	   (show-buffer (selected-window) (get-buffer "*inferior-lisp*"))) ; point is now in interactive shell
	  (insert expr)
          (delete-testcase testcase-id))
        (message "Cannot find test case"))))


; ---------------------------------------------------------------------------------------------------------------

;;; Testcases mode - the mode used for buffers with our representation of testcases.

(define-derived-mode 
        testcases-mode fundamental-mode "Testcases Mode" 
  "A mode used for presentation and direct editing of testcases in a given testsuite."


  (define-key testcases-mode-map [menu-bar schemeunit-test]
    (cons "Unit Testing" (make-sparse-keymap "schemeunit-test")))

  (define-key testcases-mode-map [menu-bar schemeunit-test testcases-help]
    '("Help on test cases" . testcases-help))

  (define-key testcases-mode-map [menu-bar schemeunit-test replace-string-in-all-testcases]
    '("Replace string in testcases..." . replace-string-in-all-testcases))

  (define-key testcases-mode-map [menu-bar schemeunit-test evaluate-current-testcase]
    '("Evaluate current testcase" . evaluate-current-testcase))
  (define-key testcases-mode-map "\C-\M-x" 'evaluate-current-testcase)

  (define-key testcases-mode-map [menu-bar schemeunit-test read-current-testcase]
    '("Read current testcase" . read-current-testcase))
  (define-key testcases-mode-map "\C-\M-y" 'read-current-testcase)

  (define-key testcases-mode-map [menu-bar schemeunit-test find-function-under-test]
    '("Find function under test" . find-function-under-test))
  (define-key testcases-mode-map "\C-c\C-f" 'find-function-under-test)

  (define-key testcases-mode-map [menu-bar schemeunit-test time-of-testcase]
    '("Date and time of testcase" . time-of-testcase))
  (define-key testcases-mode-map "\C-c\C-t" 'time-of-testcase)

  (define-key testcases-mode-map [menu-bar schemeunit-test run-testsuite]
    '("Run current testsuite" . run-testsuite))

  (define-key testcases-mode-map [menu-bar schemeunit-test default-test-windows-setup-1]
    '("Default test windows setup" . (lambda () (interactive (switch-activity "scheme-testing")))))

)


(defun find-function-under-test ()
  "In a Scheme source file, find the function/procedure under test.
The point is assumed to INSIDE the testcase, or at the start parenthesis of the testcase."
  (interactive)
  (save-excursion
    (if (not (and (looking-at "(") (= 0 (current-column))))
        (beginning-of-defun 1)) 
    (let* ((expr (expression-of-testcase-under-point))
           (function-name (guess-function-name-of-calling-form-string expr))
         )
      (if function-name
          (find-function-in-source-file function-name)
          (message "Cannot find function under test"))
    )))

(defun time-of-testcase ()
  "Show - in the minibuffer - the date and time of a testcase"
  (interactive)
  (save-excursion
    (if (not (and (looking-at "(") (= 0 (current-column))))
        (beginning-of-defun 1)) 
    (let* ((id (test-case-name-of (testcase-under-point)))  ; the id of form "n-m" where n and m are integers
           (two-ints (mapcar (function as-number) (split-string id "-")))
           (time-list (decode-time (append two-ints '(0))))
          )
       (message "Date and time of test case (day-month-year hours:min): %d-%d-%d at %d:%d" (nth 3 time-list) (nth 4 time-list)  (nth 5 time-list)  (nth 2 time-list) (nth 1 time-list))
    )))

(defun guess-function-name-of-calling-form-string (form-str)
  "Extract form of \"(form ... ...)\".
Return a the name of the calling form (a string) or nil if problems occur."
  (condition-case nil
   (let ((beg nil) (end nil) (i 0))
     (while (eq 40 (aref form-str i)) (setq i (+ i 1)))
     (setq beg i)
     (while (not (memq (aref form-str i) (list 32 10 12 9))) (setq i (+ i 1)))
     (setq end i)
     (substring form-str beg end))
   (error nil)))

(defun evaluate-current-testcase ()
  "Transfer the testcase under point to the other buffer (the Scheme interpreter) and evaluate it.
The point is assumed to INSIDE the testcase, or at the start parenthesis of the testcase.
If necessary, this command brings the interactive Scheme shell up in the other window.
"
  (interactive)
  (save-excursion
    (if (not (and (looking-at "(") (= 0 (current-column))))
        (beginning-of-defun 1)) 
    (let ((expr (expression-of-testcase-under-point)))
      (do-in-other-window
        (show-buffer (selected-window) (get-buffer "*inferior-lisp*")))  ; point is now in interactive shell
      (insert expr)
      (comint-send-input)
      (other-window 1))))

(defun read-current-testcase ()
  "Transfer the testcase under point to the other buffer (the Scheme interpreter). Do NOT evaluate it.
The point is assumed to INSIDE the testcase, or at the start parenthesis of the testcase."
  (interactive)
  (save-excursion
    (if (not (and (looking-at "(") (= 0 (current-column))))
        (beginning-of-defun 1)) 
    (let ((expr (expression-of-testcase-under-point)))
      (do-in-other-window
        (show-buffer (selected-window) (get-buffer "*inferior-lisp*")))  ; point is now in interactive shell
      (insert expr)
    )))


; ---------------------------------------------------------------------------------------------------------------

(define-derived-mode 
       imperative-testcase-mode fundamental-mode "Imperative Testcase Mode" 
  "A mode used for editing of a single imperative (low-level SchemeUnit) testcase"

  (define-key imperative-testcase-mode-map [menu-bar schemeunit-test]
    (cons "Unit Testing" (make-sparse-keymap "schemeunit-test")))

  (define-key imperative-testcase-mode-map [menu-bar schemeunit-test imperative-testcases-help]
    '("Help on imperative testcase" . imperative-testcases-help))

  (define-key imperative-testcase-mode-map [menu-bar schemeunit-test accept-imperative-testcase]
    '("Accept testcase" . accept-imperative-testcase))
  (define-key imperative-testcase-mode-map "\C-t\C-t" 'accept-imperative-testcase)

  (define-key imperative-testcase-mode-map [menu-bar schemeunit-test run-selected-schemeunit-testcase]
    '("Run this testcase" . run-selected-schemeunit-testcase))

  (define-key imperative-testcase-mode-map [menu-bar schemeunit-test run-testsuite]
    '("Run current testsuite" . run-testsuite))

)

(defun accept-imperative-testcase ()
  (interactive)
  (if (not (null current-test-suite-dir))
      (append-string-to-file 
        (concat current-test-suite-dir "imperative-testcases.scm")
        (concat (buffer-substring-no-properties (point-min) (point-max)) CR CR)
      )
      (error "Cannot add imperative testcase, because you are not connected to a testsuite")))

(defun run-selected-schemeunit-testcase ()
  (interactive)
  (let ((start-test-case-reg-exp (regular-expression '(concat "(" (zero-or-more " ") "make-test-case"))))
    (if (not (looking-at start-test-case-reg-exp))
	(beginning-of-defun 1))
 
    (let ((testcase-str (balanced-selection)))
       (derive-schemeunit-test-suite-with-single-testcase testcase-str current-test-suite-dir))

    (let ((buf (get-buffer "laml-output")))
      (if buf (erase-named-buffer buf))
      (testing-sync-process-file (concat current-test-suite-dir "testsuite.scm")))
    (other-window 1)
    (message "DONE.") 
  )
)

; general
(defun balanced-selection ()
 "Return a substring of the current buffer from the current point to the point of (forward-sexp 1)." 
 (save-excursion
  (let ((p0 (point)))
    (forward-sexp 1)
    (buffer-substring-no-properties p0 (point)))))

; ---------------------------------------------------------------------------------------------------------------



; An similar function exists in laml.el.
(defun run-scheme-interactively ()
  "Start an interactive Scheme process."
  (interactive)
  (let ((inferior-lisp-program scheme-exec))
    (run-lisp inferior-lisp-program))
)

(defun is-laml-installed ()
  (and (boundp 'laml-dir) (boundp 'laml-platform))) 
  
; ---------------------------------------------------------------------------------------------------------------
; Extended command history management.
; On a regular basis, save all entered commands in a file for subsequent reuse.

(defvar previous-comint-command-list nil "A list of recently entered comint strings")

(defvar previous-comint-command-list-length-threshold 5 "Deliver previous-comint-command-list to a file when its length exceeds the value of this variable")

(defun extended-history-comint-send (proc string)
   (setq previous-comint-command-list (cons string previous-comint-command-list))
   (if (> (length previous-comint-command-list) previous-comint-command-list-length-threshold)
       (save-excursion
          (save-comint-command-list previous-comint-command-list)
          (setq previous-comint-command-list nil)))
   (comint-simple-send proc string))

(defun save-comint-command-list (lst)
  (let ((buffer (generate-new-buffer "*recent-comint-commands*")))
    (set-buffer buffer)
    (mapcar (function (lambda (c) (insert c) (insert CR))) (reverse lst))
    (append-to-file (point-min) (point-max) comint-command-list-file-path)
    (kill-buffer buffer)))

(defun open-previous-commands ()
  (interactive)
 
  ; Save existing command list first, in order to present an updated list:
  (save-comint-command-list previous-comint-command-list)
  (setq previous-comint-command-list nil)

  (if (file-exists-p comint-command-list-file-path)
    (progn
      (delete-other-windows)
      (split-window-vertically)
      (other-window 1)
      (if (get-buffer (file-name-nondirectory comint-command-list-file-path)) 
          (kill-buffer (get-buffer (file-name-nondirectory comint-command-list-file-path))))
      (find-file comint-command-list-file-path)
      (scheme-mode)
      (goto-char (point-max))
      (message "You can edit the list, and write it back if you wish to clean it up.")
    )
    (progn
      (beep)
      (message "No previous commands exist at this point in time. Try again when you have worked a while..."))))

(defun forced-saving-of-comint-command-list ()
  (interactive)
  (save-comint-command-list comint-command-list-file-path)
  (setq previous-comint-command-list nil)
  (message (concat "DONE. The updated command list is located in " comint-command-list-file-path)))

; ---------------------------------------------------------------------------------------------------------------
; Comint string encoding and decoding.
; Problem to be solved: Strings entered in comint may need escaping if the string is to part of an Emacs Lisp expression in the testcase file.
; We read such Emacs Lisp expression by use of the normal reader. In order to circumvent all these problems
; we encode and decode the string. Notice that the encoding of a string is itself a string.

(defun comint-string-encode (str)
  "Encode the string str. The encoded representation is itself a string."
  (prin1-to-string (string-to-list str)))

(defun comint-string-decode (encoded-str)
  "Decode the (number) encoded string encoded-str to a string and return it."
  (list-to-string (car (read-from-string encoded-str)) ""))


; ---------------------------------------------------------------------------------------------------------------
; Transitional stuff from older testcase format to the new format:

(defun construct-functional-test-case-pp-1 (exp-string res-string assertion-string testcase-id)
  "Return a string with a pretty printed functional testcase, ready to be added to a testcase file.
Do the necessary encoding of exp-string and res-string."
  (let ((encode-exp (must-encode-string-p exp-string))
        (encode-res (must-encode-string-p res-string))
       )
    (concat "(" "functional-testcase" CR
            "  " (string-it testcase-id) CR
            "  " "(" (if encode-exp "number-encoded" "raw-text") " "
                     (if encode-res "number-encoded" "raw-text") ")" CR
            "  " "(use-as-example)" CR
            "  " (string-it assertion-string)  CR
            "  " (string-it (if encode-exp (comint-string-encode exp-string) exp-string)) CR
            (if encode-exp 
                (concat "  " "; " (comment-of-encoded-string (comint-string-encode exp-string)) CR)
                "")
            "  " (string-it (if encode-res (comint-string-encode res-string) res-string)) CR
            (if encode-res 
                (concat "  " "; " (comment-of-encoded-string (comint-string-encode res-string)) CR)
                "")
            ")")))

(defun construct-functional-error-test-case-pp-1 (exp-string testcase-id)
  "Return a string with a pretty printed functional error testcase, ready to be added to a testcase file.
Do the necessary encoding of exp-string"
  (let ((encode-exp (must-encode-string-p exp-string))
       )
    (concat "(" "error-functional-testcase" CR
            "  " (string-it testcase-id) CR
            "  " "(" (if encode-exp "number-encoded" "raw-text") ")" CR
            "  " "(use-as-example)" CR
            "  " (string-it (if encode-exp (comint-string-encode exp-string) exp-string)) CR
            (if encode-exp 
                (concat "  " "; " (comment-of-encoded-string (comint-string-encode exp-string)) CR)
                "")
            "  " "error"
            ")")))



(defun comment-of-encoded-string (encoded-string)
  (let ((char-number-list (car (read-from-string encoded-string))))
    (list-to-string 
      (mapcar
       (function
         (lambda (cn) 
           (if (not (or (= cn 10) (= cn 13)))
               (char-to-string cn)
               "")))
       char-number-list))))

(defun old-testcase-to-new-testcase (old)
  (let ((old-expr (car old))
        (old-res (cadr old))
        (old-ass (caddr old))
        (old-id (cadddr old))
        (old-ex (car (cdr (cdddr old)))))
    (if (equal old-ass "errs")
        (insert (concat (construct-functional-error-test-case-pp-1 old-expr old-id) CR CR))
        (insert (concat (construct-functional-test-case-pp-1 old-expr old-res old-ass old-id) CR CR)))))


(defun old-testcase-file-to-new-testcase-buffer (old-test-file-path)
  (let* ((atl (file-read-all old-test-file-path))
         (buf (get-buffer-create "new-testcases")))
    (set-buffer buf)
    (erase-buffer)
    (mapcar (function (lambda (tc) (old-testcase-to-new-testcase tc)))
            atl)))

; ---------------------------------------------------------------------------------------------------------------

(defun replace-string-in-all-testcases (from-string to-string)
 "Replace occurrences of from-string with to-string in the expression part of all test cases in the current buffer."
 (interactive "sReplace string in expression: 
sWith: ")
 (let ((count 0))
  (goto-char (point-min))
  (while (looking-at-chars (point) white-space-char-list) (forward-char 1))
  (while (< (point) (point-max))
     (let ((res (replace-string-in-test-case from-string to-string)))
        (if res (setq count (+ count 1)))
        (message "Replacements done: %s" count)
        (while (looking-at-chars (point) white-space-char-list) (forward-char 1))))
  (message "DONE. Replacements done: %s" count)))


(defun replace-string-in-test-case (from-string to-string)
  "Replace occurrences of from-string with to-string in the expression part of testcase under point. 
After the operation, the cursor point is located just after the test case. Return t if a replacement is done, else nil."
  (let* ((testcase-rec (read-from-current-buffer))
         (type (test-case-type-of testcase-rec))
         (name (test-case-name-of testcase-rec))
         (encoding (test-case-encoding-of testcase-rec))
         (example-info (test-case-example-info-of testcase-rec)))
    (cond
          ((eq type 'functional-testcase)
            (let* ((expr (real-test-case-expression-of testcase-rec))
                   (res (real-test-case-result-of testcase-rec))
                   (ass (test-case-assertion-of testcase-rec))
                   (expr-replacement (string-replace-match from-string expr to-string)))
                (if expr-replacement
                    (progn
                      (kill-sexp 1)
                      (insert (construct-functional-test-case-pp expr-replacement res ass))
                      t)
                    (progn (forward-sexp 1) nil))))

          ((eq type 'error-functional-testcase)
            (let* ((expr (real-test-case-expression-of testcase-rec))
                   (exception (test-case-kind-of-exception-of testcase-rec))
                   (expr-replacement (string-replace-match from-string expr to-string)))
                (if expr-replacement
                    (progn
                      (kill-sexp 1)
                      (insert (construct-functional-error-test-case-pp exp-replacement))
                      t)
                    (progn (forward-sexp 1) nil))))

          (t (error "replace-string-in-test-case: Called on non-supported type of test case."))
    )
  ) 
)

; To general
(defun read-from-current-buffer ()
  "Read the next expression of the current buffer. Do not move point"
  (save-excursion
    (let ((p0 (point)))
      (forward-sexp 1)
      (let ((str (buffer-substring-no-properties p0 (point))))
        (car (read-from-string str))))))


; ---------------------------------------------------------------------------------------------------------------
; To general

;; This function is a near-equivalent of the elisp function replace-match
;; which work on strings instead of a buffer.  The FIXEDCASE parameter
;; of replace-match is not implemented.
;; Authors: Sebastian Kremer <sk@thp.Uni-Koeln.DE>, Per Cederqvist <ceder@lysator.liu.se>, Inge Wallin <inge@lysator.liu.se>

(defun string-replace-match (regexp string newtext &optional literal global)
  "Replace first match of REGEXP in STRING with NEWTEXT.
If no match is found, nil is returned instead of the new string.

Optional arg LITERAL non-nil means to take NEWTEXT literally. If LITERAL is 
nil, character `\\' is the start of one of the following sequences:
  \\\\   will be replaced by a single \\.
  \\&   will be replaced by the text which matched the regexp.
  \\N   where N is a number and 1 <= N <= 9, will be replaced
       by the Nth subexpression in REGEXP. Subexpressions are grouped
       inside \\( \\).

Optional arg GLOBAL means to replace all matches instead of only the first."

  (let ((data (match-data)))
    (unwind-protect

	(if global
	    (let ((result "") 
		  (start 0)
		  matchbeginning
		  matchend)
	      (while (string-match regexp string start)
		(setq matchbeginning (match-beginning 0)
		      matchend (match-end 0)
		      result (concat result
				     (substring string start matchbeginning)
				     (if literal
					 newtext
				       (elib-string-expand-newtext)))
		      start matchend))

	      (if matchbeginning	; matched at least once
		  (concat result (substring string start))
		nil))

	  ;; not GLOBAL
	  (if (not (string-match regexp string 0))
	      nil
	    (concat (substring string 0 (match-beginning 0))
		    (if literal newtext (elib-string-expand-newtext))
		    (substring string (match-end 0)))))
      (store-match-data data))))


(defun elib-string-expand-newtext ()
  ;; Expand \& and \1..\9 (referring to STRING) in NEWTEXT.
  ;; Uses match data and fluid vars `newtext', `string'.
  ;; Note that in Emacs 18 match data are clipped to current buffer
  ;; size...so the buffer should better not be smaller than STRING.
  (let ((pos 0)
	(len (length newtext))
	(expanded-newtext ""))
    (while (< pos len)
      (setq expanded-newtext
	    (concat expanded-newtext
		    (let ((c (aref newtext pos)))
		      (if (= ?\\ c)
			  (cond ((= ?\& (setq c (aref newtext
						      (setq pos (1+ pos)))))
				 (substring string
					    (match-beginning 0)
					    (match-end 0)))
				((and (>= c ?1) 
				      (<= c ?9))
				 ;; return empty string if N'th
				 ;; sub-regexp did not match:
				 (let ((n (- c ?0)))
				   (if (match-beginning n)
				       (substring string
						  (match-beginning n)
						  (match-end n))
				     "")))
				(t (char-to-string c)))
			(char-to-string c)))))
      (setq pos (1+ pos)))
    expanded-newtext))

; ---------------------------------------------------------------------------------------------------------------

(init-scheme-interpreter-unit-testing)

