; Dired extensions.
; File manipulations based on a split window layout, where files are copied from the dired in 
; Emacs window to another. Used just a few places in LAML software (for instance in midi.el).

; Extensions to dired that allow us to copy files from on dired directory to another
; to copy directories recursively, and to delete directories recursively.
; You should be very careful when using these powerful procedures. Few questions are asked -
; the work is just done.

; The zip and unzip support in this file depends on Unix/Linux. 
; If you want to zip and unzip files from dired, you must provide Windows-specific implementations of
; the functions zip-directory-from-dired and unzip-file-from-dired. Load these functions after this file.


(defun copy-files-to-other-window-directory ()
  "From dired, copy the selected files and directories to other windows current buffer.
Selected directories are copied recursively, but the target directory is not deleted first.
Deleting directories can be done with M-x delete-files-from-dired.
This procedure assumes you are have splitted you current frame (window) in two sub-windows,
one of which is running dired. Copy the selected files from the dired window to the other buffer's
directory, even the target files exist."
  (interactive)
  (save-excursion
    (let ((selected-files (dired-get-marked-files nil)))
      (dired-unmark-all-marks)
      (other-window 1)
      (let ((other-windows-dir (current-directory)))
        (if (and other-windows-dir (> (length selected-files) 0))
            (progn
             (copy-files selected-files other-windows-dir)
             (if (eq major-mode 'dired-mode) (revert-buffer))
             (other-window 1)
             (message "DONE"))

            (message "You must be in a dired context, and the other window must have a current-directory. Nothing done."))))))


(defun delete-files-from-dired (non-confirm-p)
 "From dired, delete selected files and directories recursively. Notice that 
you should not use the delete mark, but the normal mark (*) to delete with this command.
The directories . and .. cannot be deleted by this procedure.
In case nothing is marked, the file or directory under point is deleted!
The parameter, non-confirm-p, controls if the deletions are to confirmed before executed.
Without prefix argument, using C-u, non-confirm-p nil.
With prefix argument, using C-u, non-confirm-p becomes non-nil.
"
  (interactive "P")
  (save-excursion
    (let* ((selected-files (dired-get-marked-files nil))
           (selected-files-1 
              (filter 
                (function (lambda (x) 
                 (let ((y (file-name-nondirectory x)))
                   (not (or (equal y ".") (equal y ".."))))))
                selected-files))
          )
      (if (not non-confirm-p) ; interactive default
        (if (yes-or-no-p 
	     (if (= 1 (length selected-files))
		 (let* ((fpath (car selected-files))
			(fname (file-name-nondirectory fpath))
			(fprop (file-name-proper fname))
			(fext (file-name-extension fpath))
			(faggr (concat fprop (if fext (concat "." fext) ""))))
		   (concat "Delete file or directory " faggr "? "))
                 (let* ((mes (measure-dired-selection))
                        (top-dir-count (car mes))
                        (file-total-count (cdr mes))
                       )
                    (cond ((= 0 top-dir-count) 
                            (concat "Delete a total of " (number-to-string file-total-count) " files "))
                          ((= 1 top-dir-count) 
                            (concat "Delete a total of " (number-to-string file-total-count) " files, including the files in one directory? "))
                          (t 
                            (concat "Delete a total of " (number-to-string file-total-count) " files, including the files in " (number-to-string top-dir-count) " directories? ")
                   )))))
            (progn
	     (mapcar
	      (lambda(f)
		(cond ((file-directory-p f) 
		       (delete-directory-recursively f))
		      ((file-regular-p f)
		       (delete-file f)
		       (message "Deleted file: " f))
		      (t nil)))
	      selected-files-1)
             (revert-buffer))
	  (message "Nothing deleted"))
        (progn
         (mapcar
	  (lambda(f)
	    (cond ((file-directory-p f) 
		   (delete-directory-recursively f))
		  ((file-regular-p f)
		   (delete-file f)
		   (message "Deleted file: " f))
		  (t nil)))
	  selected-files-1)
         (revert-buffer))
      ))
    (message "Done")))

(defun move-files-to-other-window-directory ()
  "From dired, move the selected files and directories to other windows current buffer.
Moving a directory is probably problematic. 
This procedure assumes you are have splitted you current frame (window) in two sub-windows,
one of which is running dired. Move the selected files from the dired window to the other buffer's
directory, even the target files exist."
  (interactive)
  (save-excursion
    (let ((selected-files (dired-get-marked-files nil)))
      (dired-unmark-all-marks)
      (other-window 1)
      (let ((other-windows-dir (current-directory)))
        (if (and other-windows-dir (> (length selected-files) 0))
            (progn
             (move-files selected-files other-windows-dir)
             (if (eq major-mode 'dired-mode) (revert-buffer))
             (other-window 1)
             (revert-buffer)
             (message "DONE"))

            (message "You must be in a dired context, and the other window must have a current-directory. Nothing done."))))))

; Return a cons pair of the number of (top level) directories and the total number of files
; in the current dired selection
(defun measure-dired-selection ()
  (save-excursion
    (let* ((selected-files (dired-get-marked-files nil))
           (selected-files-1 
              (filter 
                (function (lambda (x) 
                 (let ((y (file-name-nondirectory x)))
                   (not (or (equal y ".") (equal y ".."))))))
                selected-files))
           (dir-list (filter (function file-directory-p) selected-files-1))
           (dir-list-1 (mapcar (lambda (x) (concat x "/")) dir-list))
           (file-list (filter (function file-regular-p) selected-files-1))
          )
      (cons 
       (length dir-list)
       (+ (length file-list)
          (accumulate-right (function +) 0 (mapcar (function count-files) dir-list-1)))))))


(defun count-files (dir)
  (let* ((files (list-of-files dir))
         (dirs (list-of-directories dir))
         (full-path-dirs (mapcar (lambda (d) (concat dir d "/")) dirs))
        )
   (+ (length files) 
      (accumulate-right (function +) 0 (mapcar (function count-files) full-path-dirs)))))

  

(defun move-files (file-path-list target-dir)
  (mapcar
   (function 
    (lambda (f)
	(cond ((file-directory-p f)   ; works? 
               (let* (
                      (name (file-name-nondirectory f))
                      (initial-path (file-name-directory f))
                     )
	       (rename-file f target-dir t)))

	      ((file-regular-p f)
               (let* (
                      (name (file-name-proper (file-name-nondirectory f)))
          	      (ext (file-name-extension f))
	              (name-ext (concat name (if ext (concat "." ext) "")))
                      (initial-path (file-name-directory f))
                     )
	       (rename-file f (concat target-dir name-ext) t)
	       (message (concat f " => " (concat target-dir name-ext)))))

	      (t nil))
	))
   file-path-list))


(defun copy-files (file-path-list target-dir)
  "Copy all files in file-path-list (list of full paths) to target dir.
Files modified before Januar 1, 1980 8:24:00 get the current time as their time stamp.
Such files are assumed to come from devices that do not keep track of time."
  (mapcar
   (function 
    (lambda (f)
	(cond ((file-directory-p f) 
               (let* (
                      (name (file-name-nondirectory f))
                      (initial-path (file-name-directory f))
                     )
	       (copy-directory-recursively initial-path name target-dir)))

	      ((file-regular-p f)
               (let* (
                      (name (file-name-proper (file-name-nondirectory f)))
          	      (ext (file-name-extension f))
	              (name-ext (concat name (if ext (concat "." ext) "")))
                      (initial-path (file-name-directory f))
                      (file-attr-lst (file-attributes f))
                      (modification-time (nth 5 file-attr-lst))
                      (keep-time (>= (car modification-time) 4815))  ; keep time, if file is dated after 1980 8:24:00
                     )
	       (message (concat f " => " (concat target-dir name-ext)))
	       (copy-file f (concat target-dir name-ext) t keep-time)))

	      (t nil))
	))
   file-path-list))



; copy directory d in in-dir to to-dir, and recursively, all files too.
; indir/d => to-dir/d
(defun copy-directory-recursively (in-dir d to-dir)
  (if (not (overlapping-paths-p (concat in-dir d "/") to-dir))
      (progn
        (if (not (file-exists-p (concat to-dir d))) ; directory in-dir/d does not exist
	    (make-directory (concat to-dir d))) ; makes d in to-dir
	(let* ((files-in-d (append (list-of-files (concat in-dir d "/")) (list-of-directories (concat in-dir d "/"))))
	       (file-paths-in-d 
		(mapcar (lambda (f) (concat in-dir d "/" f)) files-in-d))
	       )
	  (copy-files file-paths-in-d (concat to-dir d "/"))
	  ))
       (error "You cannot copy a directory into itself or one of its subdirectories")))

; Is dir2 located in dir1?
; Should perhaps be conservative with respect to upper/lower case of directories (on windows, at least)
(defun overlapping-paths-p (dir1 dir2)
  (if (>= (length dir2) (length dir1))
      (equal (substring dir2 0 (length dir1)) dir1)
      nil))

(defun delete-directory-recursively (dir)
 (let* ((dir-lgt (length dir))
        (dir1 (if (equal (substring dir (- dir-lgt 1) dir-lgt) "/") dir (concat dir "/")))
        (dir-files (list-of-files dir1))
        (dir-dirs (list-of-directories dir1))
       )
   (mapcar (lambda (f) 
             (message "Deleting file: " f)
             (delete-file (concat dir1 f)))
           dir-files)
   (mapcar (lambda (d) (delete-directory-recursively (concat dir1 d "/"))) dir-dirs)
   (message (concat "Deleting " dir1))
   (delete-directory dir1)))
  
   

(defun unzip-file-from-dired ()
 "Unzip the selected file from a dired window.
Only a single zip file can be unzipped at a time."
 (interactive)
 (let ((selected-files (dired-get-marked-files nil)))
   (if (= 1 (length selected-files))
       (let* ((fpath (car selected-files))
	      (fname (file-name-nondirectory fpath))
	      (in-dir (file-name-directory fpath))
	      (fprop (file-name-proper fname))
	      (fext (file-name-extension fpath)))
	 (if (equal fext "zip")
	     (progn
               (message "Unzipping ...")
               (call-process "/bin/sh" nil nil t "-c" 
			   (concat "cd " in-dir ";" "unzip " fname))
               (revert-buffer)
               (message "DONE")
              )
	   (progn
	     (beep)
	     (message "You can only unzip a file with extension zip"))))
     (progn
       (beep)
       (message "You can only unzip a single file at a time")))))


(defun zip-directory-from-dired ()
 "Zip the selected directory from a dired window. Put the zip file in the current directory.
Only a single directory can be zipped at a time."
 (interactive)
 (let ((selected-files (dired-get-marked-files nil)))
   (if (and (= 1 (length selected-files)) (file-directory-p (car selected-files)))
       (let* ((fpath (car selected-files))
	      (dir (file-name-nondirectory fpath))
	      (in-dir (file-name-directory fpath))
             )
          (message "Zipping ...")
          (zip in-dir dir)
          (revert-buffer)
          (message "DONE")
       )
     (progn
       (beep)
       (message "You can only zip a single directory at a time")))))


(defun zip (in-dir dir)
 "Zip the directory DIR in IN-DIR.  Deletes an existing zip file before zipping."
 (let ((zip-file-ext (concat in-dir dir ".zip"))
      )
  (if (file-exists-p zip-file-ext) (delete-file zip-file-ext))

  (call-process "/bin/sh" nil nil t "-c" 
      (concat "cd " in-dir ";" "zip -ry "
              dir " " dir))))

(defun files-satisfying (dir pred)
  "Return a list of those file names (full path) in dir that satisfy pred.
pred is applied on the full path name.
dir is a slash terminated full path to a directory."
  (let ((file-list (list-of-files dir))
        (dir-list (list-of-directories dir)))
    (append
      (filter pred (mapcar (lambda (fn) (concat dir fn) ) file-list))
      (accumulate-right (function append) nil 
       (mapcar 
	(function
	 (lambda (subdir) (files-satisfying (concat dir subdir "/") pred)))
	dir-list)))))


(defun print-files-from-dired ()
 "From dired, print selected files. Notice that 
you should the normal mark (*) to print with this command.
Directories, including . and .., cannot be printed by this procedure.
In case nothing is marked, the file or directory under point is printed.
This function is specific to cs.aau.dk, and it depends on Unix and a2ps.
It also depends on Emacs Lisp stuff outside the LAML distribution.
"
  (interactive)
  (save-excursion
    (let* ((printer
   	     (read-from-minibuffer "Which printer (c12a e12a e11a e21b e21c e22a c12a) " "e42a"))
           (selected-files (dired-get-marked-files nil))
           (selected-files-non-dir (filter (lambda (f) (not (file-directory-p f))) selected-files))
           (selected-files-1 
              (filter 
                (function (lambda (x) 
                 (let ((y (file-name-nondirectory x)))
                   (not (or (equal y ".") (equal y ".."))))))
                selected-files-non-dir))
          )
      (mapcar
       (function 
        (lambda (f) 
           (prc-enscript-file f 'a2ps printer)))
        selected-files-1))))

(defun open-in-windows ()
  "From dired, open the implicitly selected file (the one marked by the cursor) in Windows."
  (interactive)
   (let* ((cur-dir (current-directory))
          (cur-dir-1 (if (= (aref cur-dir (- (length cur-dir) 1)) ?/)  ; cur-dir without trailing slach 
                        (substring cur-dir 0 (- (length cur-dir) 1))
                        cur-dir))
        )
     (cond ((looking-at "\\.\\.")
            (start-process "Windows Explorer" nil (concat laml-dir "bin/explore.bat")
                           (forward-slash-to-back-slash (parent-directory cur-dir-1))))
           ((looking-at "\\.")
            (start-process "Windows Explorer" nil (concat laml-dir "bin/explore.bat") (forward-slash-to-back-slash cur-dir-1)))
           (t (let ((selected-files (dired-get-marked-files nil 1))) ; means get one implicitly selected - the one where point is
                (cond ((and (= 1 (length selected-files)) (file-regular-p (car selected-files)))
                       (let* ((fpath (car selected-files)))
                         (start-windows-application fpath)
                         ))
                      ((and (= 1 (length selected-files)) (file-directory-p (car selected-files)))
                       (let* ((fpath (car selected-files)))
                         (start-windows-explorer fpath)
                         ))
                      (t (progn
                           (beep)
                           (message "You must select a single regular file - not a directory")))))))))

(defun start-windows-explorer (path)
  (start-process "Windows Explorer" nil (concat laml-dir "bin/explore.bat") (to-windows-directory-path path)))

(defun start-windows-application (path)
  (start-process "Windows Application" nil (concat laml-dir "bin/call.bat") path))  

(defun to-windows-directory-path (path)
  "Return a windows path with backward slash, and no trailing slash"
  (let ((path-lgt (length path)))
    (if (= (aref path (- path-lgt 1)) 47)  ; path ends in forward slash
        (forward-slash-to-back-slash (substring-no-properties path 0 (- path-lgt 1)))
        (forward-slash-to-back-slash path))))

(defun forward-slash-to-back-slash (str)
  "Replace forward slashes with backward slashes in the string str."
  (replace-chars-in-string str (list 47) 92)    ; 47 is forward slash.  92 is backward slash.
)


; ---------------------------------------------------------------------------------------------------------------
; USB dired support

(defvar usb-drive-interval (number-interval ?d ?z) "The interval of drive letters, chars = integers, which are considered 
  USB drive letters on this machine. Reassign this variable in your .emacs startup file.")

(defun usb-memory-stick-drive-letter (&optional dir-of-interest)
  "Return a list of USB memory drive letters which are active (and contains at least one file or directory in its root) on this machine. 
May engage in a dialogue with the user. Depends on the global variable usb-drive-interval. If the optional parameter dir-of-interest is provided,
it is a relative directory path. Only USB drives in which the given directory path exists are returned."
  (let* ((dir-of-interest-1 (if dir-of-interest dir-of-interest ""))
         (drive-status (mapcar (function 
                                (lambda (drive-letter)
                                  (let ((root-dir (concat (string drive-letter) ":" "/" dir-of-interest-1)))
                                   (if (and (file-directory-p root-dir)
                                            (not (null (directory-files root-dir)))   ; new as of march 12, 2011
                                       )
                                       (list (string drive-letter) t)
                                       (list (string drive-letter) nil)))))
                                usb-drive-interval))
         (drive-status-ok (filter (function (lambda (ds) (cadr ds))) drive-status))
         (drive-letters (mapcar (function car) drive-status-ok))
        )
    (cond ((= (length drive-letters) 1) (car drive-letters))
          ((= (length drive-letters) 0)
            (read-from-minibuffer 
              (format "No USB stick %s is connected. Name of drive: "
                      (if dir-of-interest (concat "with " dir-of-interest-1 " ") ""))))
          ((> (length drive-letters) 1)
            (completing-read 
              (format "Several USB sticks %sconnected. Name of drive: (%s): " 
                      (if dir-of-interest (concat "with " dir-of-interest-1 " ") "")
                      (list-to-string (mapcar (function (lambda (dr-letter)
                                                          (let ((dr-label (get-autorun-field dr-letter "label")))
                                                            (if dr-label (concat dr-letter " = " dr-label) dr-letter)))) 
                                              drive-letters)
                                    ", "))
              drive-status-ok)))))

(defun usb ()
  "Activate dired on the root dir of a (automatically) selected USB drive."
  (interactive)
  (let* ((drive-letter (usb-memory-stick-drive-letter)) 
         (drive-label (get-autorun-field drive-letter "label"))
        )
    (dired (concat drive-letter ":" "/"))
    (dired-sort-other (concat dired-listing-switches "t"))
    (revert-buffer)
    (goto-char (point-min))
    (dired-next-line 2)
    (if drive-label
        (message "Label of this USB memory stick: %s" drive-label)
        (message "No label in autorun.inf file on this USB memory stick"))))

(defun usb-photo-D400 ()
  "Activate dired on the DCMI dir of a (automatically) selected USB drive which contains a DCIM directory.
Suitable for certain Canon Cameras. Rather specialized!"
  (interactive)
  (let* ((drive-letter (usb-memory-stick-drive-letter "DCIM")) 
        )
    (dired (concat drive-letter ":" "/" "DCIM"))
    (dired-sort-other (concat dired-listing-switches "t"))
    (revert-buffer)
    (goto-char (point-min))
    (dired-next-line 2)
    (message "DONE")))



(defun get-autorun-field (drive-letter field)
  "Return the value of field in the autorun.inf file on the drive with drive-letter.
drive-letter is a single letter string. Returns a string.
Returns nil if field is non-existing, if no autorun.inf file exitst, or if the drive is not mounted."
  (if (file-directory-p (concat drive-letter ":" "/"))
      (if (file-exists-p (concat drive-letter ":" "/" "autorun.inf"))
          (let ((buf (get-buffer-create "autorun-buf"))
                (res nil))
             (set-buffer buf)
             (insert-file (concat drive-letter ":" "/" "autorun.inf"))
             (let ((field-pos (re-search-forward (regular-expression `(at-beginning-of-line (concat ,field (zero-or-more " ") "="  (zero-or-more " ")))) nil t)))
                (if field-pos
                    (let ((p0 (point)))
                      (end-of-line 1)
                      (setq res (buffer-substring-no-properties p0 (point))))
                    (setq res nil)))
             (kill-buffer buf)
             res
          )
          nil)
      nil))


; ---------------------------------------------------------------------------------------------------------------

(defun kill-buffer-1 ()
  "Kill the current buffer and report in message field"
  (interactive)
  (let ((buffer-name (buffer-name (current-buffer))))
    (kill-buffer nil)
    (message "Killed the buffer %s" buffer-name)))

(defun navigate-up-directory ()
  "Navigate up the directory structure, and kill posible soure dired buffer."
  (interactive)
  (let* ((buffer-name (buffer-name (current-buffer)))
         (cur-dir (current-directory))
         (parent-dir (parent-directory cur-dir))
         (mm major-mode)
        )
    (cond ((eq mm 'dired-mode) 
            (kill-buffer nil)
            (dired parent-dir))
          (t (dired cur-dir)))

    (if (eq mm 'dired-mode)
        (message "Killed dired buffer %s and navigated to its parent." buffer-name)
        (message "Started dired on the current directory of buffer %s (which still exits)" buffer-name))
  )
)

(defun kill-frame ()
  "Kill the current buffer and the frame in which it is shown. Actually, just kill the frame..."
  (interactive)
  (let ((buffer-name (buffer-name (current-buffer))))
;    (kill-buffer nil)
    (delete-frame (selected-frame))))

    
; ---------------------------------------------------------------------------------------------------

(defun danish-file-names-from-dired ()
  "Rename all the the selected files to have old style danish letters"
  (interactive)
  (let ((selected-files (dired-get-marked-files nil)))
     (mapcar (function danish-file-name-from-dired-single-file) selected-files))
  (dired-unmark-all-marks))

(defun danish-file-name-from-dired-single-file (selected-file)
  "Rename the selected file to have old style danish letters"
  (let* ((f-name (file-name-proper (file-name-nondirectory selected-file)))
         (path (file-name-directory selected-file))
         (ext (file-name-extension selected-file))
         (dk-f-name (old-danish-letters f-name))
         (new-path-name (concat path dk-f-name "." ext)))
    (rename-file selected-file new-path-name)
    (revert-buffer)))


(defun old-danish-letters (str)
  (cond ((empty-string-p str) "")
        (t (let ((ch (aref str 0))
                 (rest-str (substring str 1)))
             (concat
              (cond ((= ch ?æ) "ae")
                    ((= ch ?ø) "oe")
                    ((= ch ?å) "aa")
                    ((= ch ?Æ) "Ae")
                    ((= ch ?Ø) "Oe")
                    ((= ch ?Å) "Aa")
                    (t (make-string 1 ch)))
              (old-danish-letters rest-str))))))

  

(defun capitalize-file-names-from-dired ()
  "Rename all the selected files to have capitalized initial letter"
  (interactive)
  (let ((selected-files (dired-get-marked-files nil)))
     (mapcar (function capitalize-file-name-from-dired-single-file) selected-files))
  (dired-unmark-all-marks))

(defun capitalize-file-name-from-dired-single-file (selected-file)
  "Rename the selected file to have capitalized initial letter"
  (let* ((f-name (file-name-proper (file-name-nondirectory selected-file)))
         (path (file-name-directory selected-file))
         (ext (file-name-extension selected-file))
         (cap-name (capital-initial-letter f-name))
         (new-path-name (concat path cap-name "." ext)))
    (rename-file selected-file new-path-name)
    (revert-buffer)))    

(defun capital-initial-letter (str)
  "Capitalize the initial character. Only the ASCII characters below 128 are affected."
  (let* ((initial-char (aref str 0))
         (rest (substring str 1))
         (new-initial-char (cond ((and (>= initial-char ?a) (<= initial-char ?z)) (- initial-char (- ?a ?A)))
                                 (t initial-char)))
       )
    (concat (char-to-string new-initial-char) rest)))

; ---------------------------------------------------------------------------------------------------------------

(defvar count-marked-directories 0)

(defun do-not-backup-symbolic-links-in-current-dir ()
  (interactive)
  (setq count-marked-directories 0)
  (do-not-backup-symbolic-links-in-dir (current-directory))
  (message "Marked in total %d directories." count-marked-directories)
)  

(defun do-not-backup-symbolic-links-in-dir (dir)
  (let* ((dir-lst (list-of-directories dir))
         (symbolic-link-dirs (filter (lambda (x) (file-symlink-p (concat dir x))) dir-lst))
         (rec-dir-lst (list-difference dir-lst symbolic-link-dirs))
        )
    (if (not (null symbolic-link-dirs))
        (progn
          (file-write symbolic-link-dirs (concat dir "negative-backup.bui"))
          (message "Found and registered %d symbolic links in %s. A negative-backup.bui file has been generated." (length symbolic-link-dirs) dir)
          (setq count-marked-directories (+ 1 count-marked-directories)) 
        )
    )

    (mapcar (function (lambda (d) (do-not-backup-symbolic-links-in-dir (concat dir d "/")))) rec-dir-lst)))
