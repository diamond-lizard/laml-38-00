; Collection of CSS clauses from leno source program.
; Use M-x collect-css-clauses

(defvar results nil)

(defun collect-all (match)
 (let ((res (search-forward match nil t)))
   (if res
       (progn
         (search-backward match nil t) ; to position at start
         (setq results (cons (form-under-point) results))
         (forward-sexp 1)
         (collect-all match)
       ))))

(defun collect-css-clauses ()
  (interactive)
  (goto-char (point-min))
  (setq results nil)
  (collect-all "(leno-css-styled")
  (setq leno-css-styled-clauses (reverse results))

  (goto-char (point-min))
  (setq results nil)
  (collect-all "(leno-css-styled-extra")
  (setq leno-css-styled-extra-clauses (reverse results))

  (goto-char (point-min))
  (setq results nil)
  (collect-all "(leno-css-class")
  (setq leno-css-class-clauses (reverse results))
)
  
  
  



   