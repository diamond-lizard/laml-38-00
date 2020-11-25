(defun make-laml-photo-show (dir0 title prev-film-name next-film-name)
  "Make a photo show in dir0 which shows the photos located in this directory."
  (interactive "DMake photo show in which directory: 
sTitle of show: 
sName of previous film directory: 
sName of next film directory: 
")
  (let ((dir (ensure-trailing-slash (expand-file-name dir0)))
       )
   (if dir
       (if (file-exists-p (concat dir "make-show.laml"))
          (progn 
            (beep) 
            (message "An index.laml file already exists in this directory. Nothing done."))
          (progn

            (let* ((photo-buf nil))

              ; Make the setup file
              (setq photo-buf
                    (make-a-file-from-laml-template 
                     "make-show.laml"
                     dir
                     "photo-show"
                     'laml-mode
                       (list
                         (list "*PHOTO-TITLE*" (string-it title))
                         (list "*PREVIOUS-FILM*" prev-film-name)
                         (list "*NEXT-FILM*" next-film-name)
                       )
                     ))


              (show-buffer (selected-window) photo-buf)
              (set-buffer-file-coding-system (quote iso-latin-1))

              (message "Done.  Now process the buffer with  C-o .")

              )))

    (progn
      (beep)
      (message "Cannot determine the current directory. Sorry...")))))

(laml-define-key global-map [menu-bar tools laml make-photo-show]
      '("Make new photo show..." . make-laml-photo-show))

