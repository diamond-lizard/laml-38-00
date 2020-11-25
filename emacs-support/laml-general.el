;; Functions that are generally useful.
;; Please notice that this collection of functions serve as my 
;; general elisp functions. In the ideal situation, the LAML distribution
;; should only include those functions which are actually used by
;; the LAML stuff.

(provide 'general-stuff)

; colides with CL function. Therefore a renamed version occurs afterwards.
(defun reduce (f startval l)
  "reduce the list l by the binary function f.
   Use start value startval (cf MacLennnan p. 396)"
  (cond ((null l) startval)
        (t (apply f (list (car l) (reduce f startval (cdr l)))))))

;(defun accumulate-right (f startval l)
;  "Reduce the list l by the binary function f.
;   Use start value startval (cf MacLennnan p. 396)"
;  (cond ((null l) startval)
;        (t (apply f (list (car l) (accumulate-right f startval (cdr l)))))))

(defun accumulate-right (f startval lst)
  "Reduce the list lst by the binary function f.
   Use start value startval. Programmed iteratively"
  (let ((lst-rev (reverse lst))
        (res startval))
    (while lst-rev
      (setq res (apply f (list (car lst-rev) res)))
      (setq lst-rev (cdr lst-rev)))
    res))

(defun flatten (list-of-lists)
  (accumulate-right (function append) nil list-of-lists))

(defun remove-duplicates (lst)
  "Remove the duplicate from lst and return the result. Non-destructive.
Comparison done by the function equal. In case of duplicates, keep the first one in the result."
  (let ((the-lst (reverse lst)) ; reverses, to keep first duplicate in the final list.
        (res nil)
       )
    (while (not (null the-lst))
           (progn
              (if (not (member (car the-lst) (cdr the-lst)))
                  (setq res (cons (car the-lst) res)))
              (setq the-lst (cdr the-lst))))
    (reverse res)))

(defun remove-duplicates-by-predicate (lst pred)
  "Remove duplicates from lst. A pair of duplicates satisfy the predicate pred: (element element) -> boolean.
In case of duplicates, keep the first one in the result. An iterative function."
  (let ((the-lst (reverse lst)) ; reverses, to keep first duplicate in the final list.
        (res nil)
       )
    (while (not (null the-lst))
           (progn
              (if (not (member-by-predicate (car the-lst) (cdr the-lst) pred))
                  (setq res (cons (car the-lst) res))
                  ; include (car ths-lst) when we meet it last time
              )
              (setq the-lst (cdr the-lst))))
    (reverse res)))

; Recursive - iterative version next.
; (defun member-by-predicate (el lst pred)
;   "Is el member of lst. Elements are compared with the predicate pred: (element element) -> boolean.
; The first parameter, el, is supplied as the first parameter to the predicate.
; Uses the same return value convention as member and memq. Recursive function."
;   (if (null lst)
;       nil
;       (let ((first-el (car lst)))
;         (if (apply pred (list el first-el))
;             lst
;             (member-by-predicate el (cdr lst) pred)))))

(defun member-by-predicate (el lst pred)
  "Is el member of lst. Elements are compared with the predicate pred: (element element) -> boolean.
The first parameter, el, is supplied as the first parameter to the predicate.
Uses the same return value convention as member and memq. An iterative function."
  (let ((lst-1 lst))
   (catch 'result 
    (while (not (null lst-1))
       (let ((first-el (car lst-1)))
        (if (apply pred (list el first-el))
            (throw 'result lst-1)    
            (setq lst-1 (cdr lst-1)))))
   nil)))
  

(defun duplicates (lst)
  "Return the elements in lst which occur two or more times. Non-destructive.
Comparison done by equal."
  (let ((the-lst lst)
        (res nil)
       )
    (while (not (null the-lst))
           (progn
              (if (member (car the-lst) (cdr the-lst))
                  (setq res (cons (car the-lst) res)))
              (setq the-lst (cdr the-lst))))
    (remove-duplicates (reverse res)) ; if remove-duplicates is not called, some duplicates may be reported more than once
  ))


; (defun filter (p x)
;   "Filter takes two arguments: a predicate P and a list X.
; Return the elements of the list X that satiesfy the predicate P"
;   (cond ((null x) nil)
;         ((apply p (list (car x))) (cons (car x) (filter p (cdr x))))
;         (t (filter p (cdr x)))))

(defun filter (p x)
"Filter takes two arguments: a predicate P and a list X.
Return the elements of the list X that satiesfy the predicate P"
;;Non-recursive version of filter. Overwrites the previous recursive one.
  (let ((res-list nil))
    (while x
      (if (apply p (list (car x)))
          (setq res-list (cons (car x) res-list)))
      (setq x (cdr x)))
    (reverse res-list)))

;(defun mapcar2 (f x y)
; "Generalization of mapcar. The lists x and y are assumed to have the same length."
;  (cond ((null x) nil)
;        (t (cons 
;             (apply f (list (car x) (car y) )) (mapcar2 f (cdr x) (cdr y))))))


(defun mapcar2 (f x y)
 "Generalization of mapcar.
Do the mapping as long as there are elements in both lists. Programmed iteratively."
  (let ((xlst x) (ylst y) 
        (res nil))
    (while (not (or (null xlst) (null ylst)))
      (setq res (cons (apply f (list (car xlst) (car ylst))) res))
      (setq xlst (cdr xlst)) (setq ylst (cdr ylst)))
    (reverse res)))


(defun multi-apply (f v n)
 "Assume that f is a function that takes a single argument, and
  that the result-type of f is equivalent to the type of its
  argument. Apply f n times, first on v, next on (f v), etc."
 (cond ((eq n 0) v)
       (t (multi-apply f
                       (apply f (list v))
                       (1- n)))))


(defun find-element (l test result)
 "Find the first element e in the list l that satiesfies (test e).
  Return (result e). If no such element can be found, return nil"
 (cond ((null l) nil)
       ((apply test (list (car l)))
            (apply result (list (car l))))
       (t (find-element (cdr l) test result))))

(defun find-all-elements-by-selector (e lst selector)
  "Find all elements x in list for which (equal (selector x) e)." 
  (let ((the-lst lst)
        (res nil))
    (while (not (null the-lst))
        (progn
          (if (equal (apply selector (list (car the-lst) )) e)
              (setq res (cons (car the-lst) res)))
          (setq the-lst (cdr the-lst))))

   (reverse res)))


;; iterative version of find-element:

(defun linear-search (l test result)
 "Find the first element e in the list l that satiesfies (test e).
  Return (result e). If no such element can be found, return nil"
 (let ((res nil))
  (while (and l (not res))
   (cond ((apply test (list (car l)))
             (setq res (apply result (list (car l)))))
        (t (setq l (cdr l)))))
  res))


(defun find-in-list (pred lst)
  "Search linearly through list for the first element on which the predicated pred holds.
If no such element exists, return nil. An iteratively implemented function."
  (catch 'result
   (let ((tail lst))
    (while (not (null tail))
       (if (apply pred (list (car tail)))
           (throw 'result (car tail))
           (setq tail (cdr tail))))
    nil)))


(defun id-function1(a)
  "The identification function of one argument"
  a)

(defun improper-listp (x)
  "Return whether X is a cons-structure that ends with a non-nil value."
  (if (consp x)
      (let ((last-cell (last-conscell x)))
         (if (cdr last-cell) t nil))
      nil))

(defun proper-list-part (x)
  "If x is an improper list, then return a shallow copy of the proper part of the list.  If x is a proper list, return a shallow copy of the list.
On other parameters, a fatal error occurs. A recursive function."
  (cond ((and (consp x) (consp (cdr x)))
           (cons (car x) (proper-list-part (cdr x))))
        ((consp x)
           (cons (car x) nil))
        (t (error "proper-list-part called on non S-expression"))))


(defun improper-list-part (x)
  "If x is an improper list, then return the improper part. If x is a proper list, return nil. 
On other parameters, a fatal error occurs. An iterative function."
  (let ((last-cell (last-conscell x)))
    (if (not (null last-cell))
        (cdr last-cell)
        (error "improper-list-part called on non S-expression"))))
        

(defun last-conscell (x)
  "Return a reference to the last cons-cell in x. 
If x isn't a cons-cell, just return nil. An iterative function."
  (if (consp x)
      (let ((y x))
        (while (consp (cdr y))
          (setq y (cdr y)))
        y)
      nil))
    

(defvar CR "
" "CR character constant")

(defvar QUOTE "'")

(defvar EMPTY-STRING "" "The empty string constant")

(defvar SP " ")

(defun abs (n)
 "Return the absolute numeric value of n"
 (if (< n 0) (- 0 n) n))


(defun make-unique (directory filename ext)
  "Return a file name (corresponding to FILENAME)
   which is gaurantied NOT to exist. If necessary, append
   a number to the file name."
 (let ((file-path (concat directory filename "." ext)))
   (if (file-exists-p file-path)
       (let* ((i 1)
              (mod-filename (concat filename (as-string i)))
              (mod-file-path (concat directory mod-filename "." ext)))
        (while (file-exists-p mod-file-path)
         (setq i (1+ i))
         (setq mod-filename (concat filename (as-string i)))
         (setq mod-file-path (concat directory mod-filename "." ext)))
        mod-filename)
       filename)))

(defun delete-file-if-exists (fn)
  "Delete the file name FN if it exists."
  (if (file-exists-p fn) (delete-file fn)))

(defun delete-files (fn ext-list)
  "Delete all files named FN with an extension in EXT-LIST.
FN is without extension. If a file doesn't exist, do nothing."
  (mapcar 
   (function 
   (lambda (ext)
     (delete-file-if-exists (concat fn "." ext))))
     ext-list))

(defun sub-directories (dir)
 "Return a list of sub-directories of DIR. Each entry in the
list is a string. The list does not include the current directory
and the parent directory."
 (cddr  ;; to filter away . and ..
  (filter
   (function (lambda (x)
               (file-directory-p (concat dir x))))
   (directory-files dir))))

(defun determine-directory ()
  "What is the current directory. Return the current directory. Also works on a dired directory.
Returns nil of the current directory cannot be located."
  (cond ((buffer-file-name) (current-directory))
        ((eq major-mode 'dired-mode) (dired-current-directory))
        (t nil)))

  
(defun lonely-file (extension dir)
  "If there is only one file with EXTENSION in DIR, return its name (without initial path).
Else return nil"
 (let* ((files (directory-files dir))
        (files-1 (filter (function (lambda (f) (equal extension (file-name-extension f)))) files)))
   (if (= 1 (length files-1))
       (car files-1)
       nil)))

(defun get-from-buffer (start-delimiter end-delimiter)
 "Return a substring around point of the current buffer.
The substring must be delimited by START-DELIMITER and END-DELIMITER resp.
If anything goes wrong, return nil."
 ;; Should maybe allow LISTS of start- and end-delimiters.

 (save-excursion
   (let ((beg (point))
         (start nil)
         (end nil))
   (and 
    (search-forward end-delimiter nil t)
    (setq end (- (point) 1))
    (goto-char beg)
    (search-backward start-delimiter nil t)
    (setq start (+ 1 (point)))
    (buffer-substring-no-properties start end)))))


(defun ask-user (str &optional default-answer)
  "Ask user by prompting via STR. If the answer is yes return t, else nil. An optional DEFAULT-ANSWER can be provided."
  (if (equal "yes"
             (completing-read (concat str " ") '(("yes" . nil) ("no". nil)) nil t default-answer))
      t
      nil))

(defun prompt-user (prompt a-list &optional must-match)
  "Ask the user a question PROMPT. A-LIST is the possible answer and return values.
If must match is true you are required to answer one of the A-LISTs answers."
  (let ((res  (completing-read prompt a-list nil must-match)))
    (cdr (assoc res a-list))))

;CAR and CDR combinations:

;; 2

(defun cddr (l)
  (cdr (cdr l)))

(defun cadr (l)
  (car (cdr l)))

(defun caar (l)
  (car (car l)))

(defun cdar (l)
  (cdr (car l)))

;; 3


(defun caaar(l)
  (car (car (car l))))

(defun caadr(l)
  (car (car (cdr l))))

(defun cadar(l)
  (car (cdr (car l))))

(defun caddr(l)
  (car (cdr (cdr l))))

(defun cdaar(l)
  (cdr (car (car ))))

(defun cdadr(l)
  (cdr (car (cdr l))))

(defun cddar(l)
  (cdr (cdr (car l))))

(defun cdddr(l)
  (cdr (cdr (cdr l))))


;; association lists

(defun acons (key value alist)
  (cons (cons key value) alist))

(defun trivial-alist (l &optional val)
  "Convert l to an alist, where each element of l becomes the
car of a pair. The cdr of the pair becomes val, which defaults to nil."
  (if l
      (cons (cons (car l) val) (trivial-alist (cdr l) val))
      nil))


;; showing info on various topics:

(defun show-info (info-file &rest in-line)
  "Display the text on the file INFO in a help buffer.
If the second parameter is non-nil, display INFO directly (as a string)
in the help buffer."
   (let ((current-buffer (current-buffer))
         (help-buffer (if (get-buffer "*Help*")
                           (get-buffer "*Help*")
                           (generate-new-buffer "*Help*"))))       
  (set-buffer help-buffer)
  (erase-buffer)
  
  (if in-line
      (insert info-file)
      (insert-file-contents info-file))

  (goto-char (point-min))
  (select-window (display-buffer help-buffer))
  (set-buffer current-buffer)))

;; HOOKS:

(defun attach-hook (hook-symbol function-name)
  "A convenient function for handling of Emacs hooks.
Calling form (attach-hook hook-symbol function-name).
Attaches an additional function-name to the symbol 
HOOK-SYMBOL. Attach-hook always assigns a list to HOOK-SYMBOL."
 (let ((hook-symbol-value 
         (if (boundp hook-symbol)
             (eval hook-symbol)
             nil)))
  (set hook-symbol
    (cond ((listp hook-symbol-value)
             (append hook-symbol-value (list function-name)))
          ((symbolp hook-symbol-value)
             (list hook-symbol-value function-name))
          (t (error "Error in attach-hook"))))))


;; some and every

(defun some (p l)
  "Apply the predicate P on each element of L, until it gives non-nil.
Return the tail of L, on which P applied on (car tail) gives non-nil."
  (cond ((null l) nil)
        ((apply p (list (car l))) l)
        (t (some p (cdr l)))))

(defun every (p l)
  (cond ((null l) t)
        ((not (apply p (list (car l)))) nil)
        (t (every p (cdr l)))))

(defun scroll-down-other-window ()
  "scroll-other-window, but in opposite direction"
  (interactive)
  (let ((w (selected-window)))
    (other-window 1)
    (condition-case nil
       (scroll-down nil)
     (error (select-window w)))
    (select-window w)))

(defun scroll-up-1 ()
  (interactive)
  (scroll-up 1))

(defun scroll-down-1 ()
  (interactive)
  (scroll-down 1))

(defun beginning-of-buffer-other-window ()
  "Based on the assumptionon of a dual window setup, go to the begninng of the other window"
  (interactive)
  (other-window 1)
  (goto-char (point-min))
  (other-window 1))

(defun end-of-buffer-other-window ()
  "Based on the assumptionon of a dual window setup, go to the end of the other window"
  (interactive)
  (other-window 1)
  (goto-char (point-max))
  (other-window 1))

;; STRING <-> SYMBOL CONVERSION:

(defun string-to-symbol (str)
  "Convert STR to a symbol."
 (car (read-from-string str)))

(defun symbol-to-string (symbol)
  "Convert SYMBOL to a string."
  (symbol-name symbol))

;;; STRING-TO-INT PREDICATE AND CONVERSION PRIMITIVE
;;; string-to-int is sick! it returns 0 in, for instance, the following
;;; case (string-to-int "pip"). It has something to do with "yes" and "no"!!!
;;; Please notice the limitation!!!

(defun string-numberp (x)
  "If the string X is a number, return the corresponding number.
Else return nil. Limitation: X must be readable via read."
  (let ((x-read (read x)))
     (if (numberp x-read) x-read nil)))


;; PUT-IN AND PUT-AROUND PROCEDURES

(defun put-in (p s)
 "Put in prefix P and suffix S at the current possition.
  Place the cursort right after P."
 (let ((bp (point))
       (pt nil))
   (insert p)
   (setq pt (point))
   (insert s)
   (goto-char pt)))


(defun put-around-region (p s &rest point-in-front)
 "Put prefix string p and suffix string s around the current region.
  POS-IN-FRONT is optional. If it is non-nil, place the cursor in
  front of P. Else place cursor after S. "
 (let ((bp (point))
       (rb (region-beginning))
       (re (region-end)))

   ;; handle suffix:
   (goto-char re) (insert s)
   (if (not point-in-front) (progn (set-mark (point)) (push-mark)))

   ;; handle prefix:
   (goto-char rb) 
   (if point-in-front (progn (set-mark (point)) (push-mark)))
   (insert p) 

   ;; set point:
   (pop-mark) (exchange-point-and-mark)))


(defun put-around-word1 (p i s il)
 "Put prefix string p and suffix string s around the currently selected
  word (in which the cursor is located).
  Also, il possitions from the end of the word, insert the infix string i."
 (let ((bp (point))
      (rb (progn (if (not (looking-at "\\<"))
                 (forward-word -1))
                 (point)))
      (re (progn (forward-word 1)
                 (point)))
      (pt nil))
   (goto-char re) (insert s) 
   (setq pt (+ (point) (length s) 2))
   (goto-char (- (point) (+ (length s) il))) (insert i)
   (goto-char rb) (insert p)
   (goto-char pt)))

(defun put-around-word (p s &rest point-in-front)
 "Put prefix string p and suffix string s around the currently selected
word (in which the cursor is located). POS-IN-FRONT is optional. If it
is non-nil, place the cursor in front of P. Else place cursor after S." 
 (let ((bp (point))
       (rb (progn (if (not (looking-at "\\<"))
                      (forward-word -1))
                  (point)))
       (re (progn (forward-word 1)
                  (point)))
       (pt nil)
       (pif (cond ((null point-in-front) nil)
                  ((consp point-in-front) (car point-in-front))
                  (t nil)))) 
   (goto-char re) (insert s) 
   (setq pt (+ (point) (length s) 2))
   (goto-char rb) (insert p)  ; now point is after prefix.
   (if pif
       (forward-char (- (length p)))
       (forward-char (+ (length s) (- re rb))))
))


(defun mapcan (f x)
  "The traditional Lisp mapcan function."
  (cond ((null x) nil)
        (t (nconc
             (apply f (list (car x) )) (mapcan f (cdr x))))))

(defun mem-equal (e l)
 "As memq, but using equal for comparion"
  (some (function (lambda (f) (equal e f))) l))

(defun insert-line-prefix (start end str)
  "Insert a string STR (third arg) at the beginning of each line in the 
char interval from START to END (first and second arg). Leave the point
unchanged."
  (interactive "r
sPrefix string:")
  (save-excursion
   (let ((start1 0))
    (goto-char start) (beginning-of-line) (setq start1 (point))
    (goto-char end)
    (beginning-of-line) 
    (let ((progress t)
          (p))
     (while (and (>= (point) start1) progress)
      (progn 
        (insert str)
        (setq p (point))
        (previous-line 1)
        (setq progress (not (= p (point))))
        (beginning-of-line)))))))



(defun insert-line-postfix (start end str)
  "Insert a string STR (third arg) at the end of each line in the 
char interval from START to END (first and second arg). Leave the point
unchanged. PROBABLY WRONG. SHOULD MOVE UPWARD INSTEAD OF DOWNWARD. SEE
INSERT-LINE-PREFIX."
  (interactive "r
sPostfix string:")
  (save-excursion
    (goto-char start)
    (end-of-line)
    (while (< (point) end)
      (progn 
        (insert str)
        (next-line 1)
        (end-of-line)))))

(defun delete-line-prefix (start end number-of-chars)
  "In the range of lines from START (a point) to END (another point, larger
than START), delete the leading NUMBER-OF-CHARS on each line. The lines
includes the line, in which the points START and END are located."
    (interactive "r
nNumber of chars: ")
  (if (> number-of-chars 0)
  (save-excursion
   (let ((start1 0))
    (goto-char start) (beginning-of-line) (setq start1 (point))
    (goto-char end)
    (beginning-of-line)
    (let ((progress t)
          (p)) 
     (while (and (>= (point) start1) progress)
      (progn 
        (delete-char number-of-chars)
        (setq p (point))
        (previous-line 1)
        (setq progress (not (= p (point))))
        (beginning-of-line))))))))

(defun insert-region-in-other-window (ff tt)
  (interactive "r")
  (let ((str (buffer-substring ff tt)))
     (other-window 1)
     (insert str)
     (next-position)  ; ???
     (other-window 1)))

(defun sexp-starting-at-point ()
  "Return the sexpression that starts at the current position."
  (let ((opoint (point)))
   (save-excursion
    (forward-sexp 1)
    (car (read-from-string (buffer-substring opoint (point)))))))

(defun shrink-other-window (n i)
  "Make other window N smaller by I lines.
When called interactively, N defaults to 1 and I to an appropriate number."
  (interactive (list 1 4))
  (other-window n)
  (shrink-window i)
  (other-window (- n)))

(defun enlarge-other-window (n i)
  "Make other window N Smaller By I lines.
When called interactively, N defaults to 1 and I to an appropriate number."
  (interactive (list 1 4))
  (other-window n)
  (enlarge-window i)
  (other-window (- n)))


(defun erase-named-buffer (buffer)
  "Erase the buffer with erase-buffer. buffer is a buffer, not a buffer name."
   (save-window-excursion
     (set-buffer buffer)
     (erase-buffer)))

(defun buffer-string-of-buffer (buffer)
  "Return the textual content of buffer (which is a buffer, not just a name of a buffer)."
  (save-window-excursion
    (set-buffer buffer)
    (buffer-string)))

(defun map-lines (f)
  "Apply the function f on each line after point in the current
buffer. (The line, in which point is located is also processed.)  As a
precondition, the point is located at the beginning of line each time
f is located.  The function f is supposed to have one parameter,
namely the line as a string. Return a list of f-applications on the
lines."
 (let ((res-list nil))
  (save-excursion
    (beginning-of-line)
    (while (not (eobp))
      (let* ((p1 (point))
             (p2 (save-excursion (end-of-line) (point)))
             (l (buffer-substring p1 p2)))
        (setq res-list 
              (cons (apply f (list l)) res-list)))
        (goto-next-line) ))
  (reverse res-list)))


(defun map-lines-in-region (f from to)
  "Apply the function F on each line in the region between FROM and TO. (The
line, in which FROM is located is also processed.)  As a
precondition, FROM must be less than or equal to TO.  The point is
located at the beginning of line each time f is located.  The function
f is supposed to have one parameter, namely the line as a string.
Return a list of f-applications on the lines."
  (let ((beg (point-min-marker))
        (end (point-max-marker))
        (res nil))
   (let ((zmacs-regions nil))
     (unwind-protect
         (narrow-to-region from to)
         (beginning-of-buffer)
         (setq res (map-lines f))
         (narrow-to-region beg end)))
         (message "")
   res))

;; A helping function to map-lines:
(defun goto-next-line()
  (let ((p (point)))
    (forward-line 1)
    (if (= p (point)) ; no progress via next-line
        (goto-char (point-max)))))


(defun map-paragraphs (f)
  "Apply the function f on each paragraph after point in the current
buffer. (The paragraph, in which point is located is also processed.)
As a precondition, the point is located at the beginning of of the
first line of each paragraph.  The function f is supposed to have one
parameter, namely the paragraph as a string. Return a list of
f-applications on the paragraphs."
 (let ((res-list nil))
  (save-excursion
    (if (not (beginning-of-paragraph-p)) (start-of-paragraph-text)) 
    (while (not (eobp))
      (let* ((p1 (point))
             (p2 (save-excursion (end-of-paragraph-text) (point)))
             (l (buffer-substring p1 p2)))
        (setq res-list 
              (cons (apply f (list l)) res-list)))
        (goto-next-paragraph) ))
  (reverse res-list)))

(defun map-paragraphs-in-region (f from to)
  "Apply the function F on each paragraph in the region between FROM
and TO. (The paragraph, in which FROM is located is also processed.)
As a precondition, FROM must be less than or equal to TO.  In order to
work as expeced, FROM should be the beginning of the first paragraph
and TO should be at the end of the last paragraph.  The point is
located at the beginning of the first line in a paragraph each time f
is located.  The function f is supposed to have one parameter, namely
the line as a string. Return a list of f-applications on the
paragraphs."
  (let ((beg (point-min-marker))
        (end (point-max-marker))
        (res nil))
   (let ((zmacs-regions nil))
     (unwind-protect
         (narrow-to-region from to)
         (beginning-of-buffer)
         (setq res (map-paragraphs f))
         (narrow-to-region beg end)))
         (message "")
   res))

;; A helping function to map-paragraphs:
(defun goto-next-paragraph()
  (let ((p (point)))
    (end-of-paragraph-text)
    (skip-through (list 32 10))  ; skip space and CRs
    (if (= p (point)) ; no progress via skipping.
        (goto-char (point-max)))))

(defvar CR-int (string-to-char CR) "The integer value of CR")

(defun beginning-of-paragraph-p ()
  "Return whether point is locaated at the begining af a paragraph.
This is the strict interpretation. If there is a space and a CR just
before point, this function will return false"
  (cond ((bobp) t)
        ((and (= (char-after (- (point) 1)) CR-int) (= 2 (point))) t)
        ((and (= (char-after (- (point) 1)) CR-int) 
              (= (char-after (- (point) 2)) CR-int)) t)
        (t nil)))

(defun skip-through (char-val-list)
  "forward point until point is different from the character values
   in CHAR-VAL-LIST.
   Returns nil if end-of-buffer is reached, else return t."
  (let ((ch (following-char)))
    (while (and (not (eobp)) (memq ch char-val-list))
       (forward-char) (setq ch (following-char)))
    (if (eobp) nil t)))

(defun skip-through-reverse (char-val-list)
  "backward point until point is different from the character values
   in CHAR-VAL-LIST.
   Returns nil if beginning-of-buffer is reached, else return t."
  (let ((ch (preceding-char)))
    (while (and (not (bobp)) (memq ch char-val-list))
       (backward-char) (setq ch (preceding-char)))
    (if (bobp) nil t)))

;; Special forms:

(defmacro defvar1 (symbol value doc)
  "Equivalent to defvar, but the defvar1 special form allows a
defvar1 form to be evaluated several times, hereby causing the
symbol's value to be redefined."
  (` (progn (setq (, symbol) (, value))
            (put '(, symbol) 'variable-decumentation (, doc))
            '(, symbol))))

; for Scheme compatibility:
(defmacro define (symbol value)
  (` (setq (, symbol) (, value))))

(defmacro do-in-buffer (string &rest contents-expr-list)
 "Execute CONTENTS-EXPR-LIST (the second rest par) in a temporary
buffer with contents STRING (first par). The point is set to (point-min)
before the execution of the lisp forms. The result
of the last of the forms in CONTENTS-SEXP-LIST is returned.
Typical call:
  (do-in-buffer string-valued-var (search-forward \"pip\"))."


 (` (let ((current-buffer (car (buffer-list)))
          (displ-buffer
            (if (get-buffer "*temporary-buffer-009*")
                (get-buffer "*temporary-buffer-009*")
                (generate-new-buffer "*temporary-buffer-009*"))))

  (set-buffer displ-buffer)
  (erase-buffer)
  (insert (, string))
  (goto-char (point-min))

  (let ((res (progn (,@ contents-expr-list))))
      (set-buffer current-buffer)
      res))))

(defmacro do-in-other-window (&rest forms)
  "Carry out forms in the other window of the current frame. Create the other window if necessary
This macro aims at a two-window lay of the current frame. Point will, per default, be located in the other window"
  `(let ((w-list (window-list-of-frame)))
     (if (= 2 (length w-list))
	 (progn
	   (other-window 1)
           ,@forms
	   (other-window 1)  ; new as of May 24, 2010
         )
         (progn
	   (delete-other-windows)
	   (split-window-vertically)
	   (other-window 1)
           ,@forms
	   (other-window 1)   ; new as of May 24, 2010
	   ))))

(defun mail-current-buffer ()
  "Insert the contents of the current buffer into a mail sending buffer."
  (interactive)
  (let ((text (buffer-string)))
    (mail)
    (end-of-buffer)
    (insert text)
    (beginning-of-buffer) (end-of-line) ))

(defun news-current-buffer ()
  "Insert the contents of the current buffer into a news sending buffer."
  (interactive)
  (let ((text (buffer-string)))
    (postnews)
    (end-of-buffer)
    (insert text)
    (beginning-of-buffer) (end-of-line) ))

(defun mail-region ()
  "Insert the currently selected region in the current buffer into a
mail sending buffer."
  (interactive)
  (let ((text (buffer-substring (point) (mark))))
    (mail)
    (end-of-buffer)
    (insert text)
    (beginning-of-buffer) (end-of-line) ))

(defun news-region ()
  "Insert the currently selected region in the current buffer into a
news sending buffer."
  (interactive)
  (let ((text (buffer-substring (point) (mark))))
    (postnews)
    (end-of-buffer)
    (insert text)
    (beginning-of-buffer) (end-of-line) ))

(defun count-words-buffer ()
   "Count words, chars, and lines in the current buffer.
Relies on the (unix) operating system command wc."
  (interactive)
  (let ((temp-file "~/temp/wc-file")
        (temp-buf (generate-new-buffer "wc-buffer"))
        (cur-buf (current-buffer))
        (info-buffer(if (get-buffer "info-buffer")
                                (get-buffer "info-buffer")
                                (generate-new-buffer "info-buffer"))))

      (set-buffer info-buffer)  (erase-buffer)
      (set-buffer temp-buf)
      (insert-buffer cur-buf)
      (write-file temp-file)
      (call-process "/bin/csh" nil info-buffer nil "-c" (concat "wc -w " temp-file))
      (set-buffer info-buffer)
      (beginning-of-buffer) (search-forward "/")
      (message (concat "Number of words in buffer: " (buffer-substring 1 (- (point) 1))))
      (kill-buffer temp-buf) (kill-buffer info-buffer)
      (delete-file-if-exists temp-file)))

(defun count-chars-region ()
  "Count the number of characters in region and report in minibuffer."
  (interactive)
  (let ((bp (point))
        (rb (region-beginning))
        (re (region-end)))
    (message (concat (int-to-string (- re rb)) " " "characters in region"))
    (- re rb)))

(defun count-words-region ()
   "Count words, chars, and lines in the current region in the current buffer. 
Relies on the (unix) operating system command wc."
  (interactive)
  (let ((temp-file "~/temp/wc-file")
        (temp-buf (generate-new-buffer "wc-buffer"))
        (text (buffer-substring (min (point) (mark)) (max (point) (mark))))
        (cur-buf (current-buffer))
        (info-buffer (if (get-buffer "info-buffer")
                                (get-buffer "info-buffer")
                                (generate-new-buffer "info-buffer"))))

      (set-buffer info-buffer)  (erase-buffer)
      (set-buffer temp-buf)
      (insert-string text)
      (write-file temp-file)
      (call-process "/bin/csh" nil info-buffer nil "-c" (concat "wc -w " temp-file))
      (set-buffer info-buffer)
      (beginning-of-buffer) (search-forward "/")
      (message (concat "Number of words in region: " (buffer-substring 1 (- (point) 1))))
      (kill-buffer temp-buf) (kill-buffer info-buffer)
      (delete-file-if-exists temp-file)))

(defun count-lines-file (file-path)
  "Count the number of lines in the file, given by file-path. Returns the count (an integer)." 
  (let ((temp-buf (generate-new-buffer "line-count-buffer"))
        (count 0)
       )
    (set-buffer temp-buf)
    (erase-buffer)
    (insert-file-contents file-path)
    (goto-char (point-min))
    (while (= 0 (forward-line 1))
      (setq count (+ count 1)))
    (prog1
      count
      (kill-buffer temp-buf))))

(defun collect-buffer ()
  "Collect this buffer in the collect-buffer. This function supports
the collection of a number of buffers to one big buffer."
  (interactive)
 (save-excursion
  (let ((text (buffer-substring (point-min) (point-max)))
        (colbuf (if (get-buffer "*collect*")
                      (get-buffer "*collect*")
                      (generate-new-buffer "*collect*")))
        )
    (set-buffer colbuf)
    (goto-char (point-max))
    (insert "") (insert CR)
    (insert text))))
  

(defun call-unix-process (cmd &optional buf)
  "Syncroneous call the unix commond CMD. If BUF is given it should be
a buffer, in which to insert the output of the command."
  (if buf
    (call-process "/bin/csh" nil buf nil "-c" cmd)
    (call-process "/bin/csh" nil nil nil "-c" cmd)))

(defun list-prefix (lst length)
  "Return a prefix of the list LST of length LENGTH. If the 
length of LST is less than LENGTH, LST is returned (uncopied)."
  (if (> (length lst) length)
      (list-prefix-help lst length)
      lst))

(defun list-prefix-help (lst length)
  ; assume that LST is longer than length.
  (cond ((> length 0) (cons (car lst) (list-prefix-help (cdr lst) (- length 1))))
        (t '())))

(defun all-matches (reg-exp &optional level-count)
  "In the current buffer, and from the current buffer
position, return a list of all consequtive matches
of the regular expression REG-EXP. The elements in the list
can be restrited to sub-matches by giving an integer LEVEL-COUNT"
  (let ((level-c (if level-count level-count 0))
        (single-res t)
        (list-res nil))
    (while single-res
       (setq single-res (re-search-forward reg-exp nil t))
       (if single-res 
           (setq list-res 
                 (cons (buffer-substring (match-beginning level-c)
                                         (match-end level-c))
                       list-res))))
    list-res))


(defun buffer-directory-name (buffer)
  "Return the name of the directory of the file, associated with the
current buffer. If none, return nil"
  (let ((file-name (buffer-file-name buffer)))
    (if file-name
        (file-name-directory file-name)
        nil)))

(defun strip-version-of-string (str)
  "If the suffix of string is of the form <i> for some string i (typically an integer), return a prefix of str stripped from this suffix.
If not, just return str. Useful to get rid of <number> suffix from unique buffer names."
  (let ((i (find-in-string-from-end str ?<))
        (j (find-in-string-from-end str ?>)))
    (if (and i j (< i j))
        (substring str 0 i)
        str)))

(defun current-directory ()
  "Return the directory of the current buffer, if such a directory makes sense
for the buffer"
  (if (eq major-mode 'dired-mode)
      (dired-current-directory)
      (buffer-directory-name (current-buffer))))


(defun current-file ()
  "Return the filename (without directory) of the current-buffer.
If no such file exists, return nil"
  (file-name-nondirectory (buffer-file-name (current-buffer))))

(defun find-last-point (str)
  "Return the position of the last point in the string str. 
Do not allow to pass '/' while looking for the point. If no point is found under these conditions, return nil."
  (let ((idx (- (length str) 1)))
    (while (and (>= idx 0) (not (eq (aref str idx) ?.)) (not (eq (aref str idx) ?/))) (setq idx (- idx 1)))
    (if (and (>= idx 0) (eq (aref str idx) ?.)) idx nil)))

(defun file-name-proper (filename)
  "Takes as input a filename, without directory path.
Return the file name proper. That is, the file name without the file extension.
If no dot is found, return filename.
Use the native Emacs Lisp function file-name-nondirectory to access the the proper file name and the extension of a file path.
Use the native Emacs Lisp function file-name-directory to access the directory path of a file path."
  (let ((point-idx (find-last-point filename)))
    (if point-idx 
        (substring filename 0 point-idx)
        filename)))

(defun directory-name-proper (file-path)
  "Takes as input an absolute or relative, forward slash terminated path to a directory.
Return the proper name of the directory, without initial path. 
The remaining part of file-path can be accessed by the function parent-directory."
  (let ((file-path-1 (substring file-path 0 (- (length file-path) 1))))
    (file-name-nondirectory file-path-1)))


(defun file-name-extension (filename)
  "Takes a filename, without directory path.
Returns the extension. If no extension is found, return nil."
  (let ((point-idx (find-last-point filename)))
    (if point-idx 
        (substring filename (+ 1 point-idx) (length filename))
        nil)))

(defun parent-directory (dir-path)
 "Return the parent directory of the directory path (absolute or relative) dir-path (a string).
dir-path must be forward slash terminated."
  (let* ((path-list (file-path-to-path-list dir-path))
         (parent-path-list (butlast path-list))
         (root-el (if (stringp (car path-list)) nil (car path-list)))
         (res (concat
               (cond ((eq (car root-el) 'winroot)
                      (concat (cadr root-el) ":" "/"))
                     ((eq (car root-el) 'unixroot)
                      "/")
                     ((null root-el) (concat (car path-list) "/"))   ; relative path. Compensate: Put in first el! Hack!
                     (t (error "parent-directory: root problem" (car root-el))))
               (list-to-string (cdr parent-path-list) "/"))))
    (if (eq (aref res (- (length res) 1)) ?/)
        res
        (concat res "/"))))


(defun ensure-directory-existence (prefix-dir dir)
 "Ensure that the directory with path (concat prefix-dir dir) exists.
If necessary, create dir in prefix-dir. It is assumed as a precondition that
prefix-dir exists before the activation of this procedure."
  (if (not (file-exists-p (concat prefix-dir dir))) 
      (make-directory (concat prefix-dir dir))))



(defun ensure-trailing-slash (dir-string)
  "Ensure that dir-string ends in a slash: /"
  (let* ((dir-lgt (length dir-string))
         (last-chr (aref dir-string (- dir-lgt 1))))
   (if (eq last-chr ?/)
       dir-string
       (concat dir-string "/"))))


(defun see-file ()
  "Open file which is selected by a string under point, surrounded by quotes"
 (interactive)
 (let ((file-name (get-from-buffer "\"" "\"")))
   (find-file file-name)))


(defun sh ()
  "Make a shell in the current directory. Ad hoc command."
  (interactive)
  (let ((existing-shell (get-buffer "*shell*")))
   (if existing-shell
       (kill-buffer existing-shell))
   (shell)
   (comint-send-input)
))

; (defun last (l)
;   (cond ((null (cdr l)) l)
;         (t (last (cdr l)))))
; 
; (defun butlast (l)
;   (cond ((null (cdr l)) nil) ; <- last element
;         (t (cons (car l) (butlast (cdr l))))))



; The functions last and butlast are taken from subr.el.
; butlast is defined in Emacs 21, but not in earlier versions.
; In order to support older versions of Emacs we include both here.

(defun last (x &optional n)
  "Return the last link of the list X.  Its car is the last element.
If X is nil, return nil.
If N is non-nil, return the Nth-to-last link of X.
If N is bigger than the length of X, return X."
  (if n
      (let ((m 0) (p x))
        (while (consp p)
          (setq m (1+ m) p (cdr p)))
        (if (<= n 0) p
          (if (< n m) (nthcdr (- m n) x) x)))
    (while (consp (cdr x))
      (setq x (cdr x)))
    x))

(defun butlast (x &optional n)
  "Returns a copy of LIST with the last N elements removed."
  (if (and n (<= n 0)) x
    (nbutlast (copy-sequence x) n)))

(defun nbutlast (x &optional n)
  "Modifies LIST to remove the last N elements."
  (let ((m (length x)))
    (or n (setq n 1))
    (and (< n m)
         (progn
           (if (> n 0) (setcdr (nthcdr (- (1- m) n) x) nil))
           x))))



(defun string-it (x)
  "Enclose the string in explic string quotes"
  (concat "\"" x "\""))

(defun list-it (&rest x)
  (prin1-to-string x))


(defvar scheme-extended-name-characters (list ?+ ?- ?. ?* ?/ ?< ?= ?> ?! ?? ?: ?$ ?% ?_ ?& ?~ ?^) "The list of non-alphabetical and non-ciffer scheme name letters")

; Aliases of each other:
(defvar blank-characters (list 32 13 10 9) "The list of blank characters")
(defvar white-space-char-list (list 9 10 13 32) "The list of white space characters") 


(defun blank-char-p (ch)
  (member ch blank-characters))

(defun blank-string-p (str)
  (or (empty-string-p str)
      (string-of-char-list-p str blank-characters)))

(defun string-of-char-list-p (str char-list)
  "Is each character in str member of char-list. char-list is a list of characters (integers). If str is empty the result is always true."
  (catch 'res
   (let ((str-chars (string-to-list str)))
    (while (not (null str-chars))
      (if (not (member (car str-chars) char-list))
          (throw 'res nil))
      (setq str-chars (cdr str-chars)))
    t ; result if all characters in str are member of char-list
   )))


(defun scheme-name-char-p (ch)
  "Is ch a possible Scheme name constituent char"
  (or
     (and (>= ch ?a) (<= ch ?z))
     (and (>= ch ?A) (<= ch ?Z))
     (and (>= ch ?0) (<= ch ?9))
     (member ch scheme-extended-name-characters)))

(defun goto-start-of-scheme-word ()
  "Go to start of scheme word"
  (while (scheme-name-char-p (char-under-point))
    (forward-char -1))
  (forward-char 1))

(defun goto-end-of-scheme-word ()
  "Go to end of scheme word"
  (while (scheme-name-char-p (char-under-point))
    (forward-char 1))
  (forward-char -1))

  
; There must be a more direct way, via a stream???  
(defun file-read (file)
  "Read the first Lisp expression from file (full path)"
  (save-excursion
    (let ((temp-buf (generate-new-buffer "file-reading.tmp")))
      (set-buffer temp-buf)
      (insert-file-contents file)
      (goto-char (point-min))
      (prog1
        (read temp-buf)
        (kill-buffer temp-buf)))))

(defun file-read-schemedoc (file-path-0)
  "A specialized file-read function for reading SchemeDoc manlsp files.
Read the first Lisp expression from file (absolute or relative path). Works both with and without the manlsp extension in file-path-0."
  (let* ((file-path (file-name-sans-extension file-path-0))
         (abs-file-path (if (file-name-absolute-p file-path) (concat file-path ".manlsp") (concat (current-directory) file-path ".manlsp"))))
    (if (file-exists-p abs-file-path)  ; it may be a slim distribution
        (save-excursion
          (let ((temp-buf (generate-new-buffer "file-reading.tmp")))
            (set-buffer temp-buf)
            (insert-file-contents abs-file-path)
            (goto-char (point-min))
            (forward-char 1) (kill-sexp 1) ; deletes the header part of the read manlsp file 
            (goto-char (point-min))
            (prog1
                (read temp-buf)
              (kill-buffer temp-buf))))
        nil
    )))

(defun forward-through-white-space-and-comments ()
 "Move point over white space and comments in an elisp file"
 (while (or (looking-at-white-space (point)) 
            (looking-at-chars (point) (list 59)))   ; looking at semicolon
     (cond ((looking-at-white-space (point)) (forward-char 1))
           ((looking-at-chars (point) (list 59)) (forward-line 1)))))

(defun file-read-all (file)
  "Read the list of all Lisp expressions from file (full path)"
  (let ((res nil))
   (save-excursion
     (let ((temp-buf (generate-new-buffer "file-reading.tmp")))
       (set-buffer temp-buf)
       (insert-file-contents file)
       (goto-char (point-min))
       (forward-through-white-space-and-comments)

       (while (not (eobp))
         (progn 
          (setq res (cons (read temp-buf) res))
          (forward-through-white-space-and-comments)))
       (kill-buffer temp-buf)
       (reverse res)))))

(defun file-read-string (file)
  "Read the textual content of file (full path) and return it as a string"
  (save-excursion
     (let ((temp-buf (generate-new-buffer "file-reading"))
           (res nil))
       (set-buffer temp-buf)
       (insert-file-contents file)
       (setq res (buffer-substring-no-properties (point-min) (point-max)))
       (kill-buffer temp-buf)
       res)))

(defun file-write (lst file)
  "Write the list lst to file (full path)."
  (save-excursion
    (let ((temp-buf (generate-new-buffer "file-writing")))
      (set-buffer temp-buf)
      (prin1 lst temp-buf)
      ; (write-file file nil):
      (write-region (point-min) (point-max) file nil 0)  ; 0: to suppres mini buffer message
      (kill-buffer temp-buf)
)))

(defun file-write-string (str file)
  "Write the string str to file (full path)"
  (let ((temp-buf (generate-new-buffer "file-writing")))
      (set-buffer temp-buf)
      (insert str)
      (write-file file)
      (kill-buffer temp-buf)))

  
  

(defun open-file-at-point (file-path-expr)
  "Read a relative or absolute file path under point (first char of, initial string quote) and
open the file relative to the current directory. Absolute path's will also work. Good for LENO source-program clauses.
If a list form is under point, evaluate it in Emacs, in order to attempt to get the correct
file path."
  (interactive
    (list (sexp-starting-at-point)))
  (let ((cur-dir (current-directory)))
    (cond ((and (stringp file-path-expr) (file-name-absolute-p file-path-expr)) (find-file-other-window file-path-expr))
          ((and (stringp file-path-expr) (not (file-name-absolute-p file-path-expr))) (find-file-other-window (concat cur-dir file-path-expr)))
          ((listp file-path-expr)
             (let ((file-path-eval (eval file-path-expr)))
               (cond ((and (stringp file-path-eval) (file-name-absolute-p file-path-eval)) (find-file-other-window file-path-eval))
                     ((and (stringp file-path-eval) (not (file-name-absolute-p file-path-eval))) (find-file-other-window (concat cur-dir file-path-eval)))
                     (t (error "open-file-path failed (1)")))))
          (t (error "open-file-path failed (2)")))))



; ---------------------------------------------------------------------------------------------------
; Indirect LAML processing support together with general processing from Emacs.

; Return an LAML file name taking from the first line of the current buffer in case it is of the form ;=> filename.
; This return value may server as a boolean true value as well.
; If the buffer does not start with the special characters, return nil (false).
(defun locate-indirect-laml-processing-current-buffer ()
  (save-excursion
    (goto-char (point-min))
    (if (looking-at ";=>")
        (let ((p1 4))
          (end-of-line)
          (let ((p2 (point)))
            (buffer-substring p1 p2)))
        nil)))

(defun do-process-buffer ()
  "Do process contents of buffer in a mode dependent way. Depends on the procedure laml-process-file, which is not
yet defined for all platforms, scheme systems, and operating systems."
  (interactive)
  (cond ((and (boundp 'elucidator-buffer-info) (or (eq elucidator-buffer-info 'program) (eq elucidator-buffer-info 'documentation))) (elucidate))
        ((eq major-mode 'laml-mode) (laml-process-current-buffer))
        ((and (or (eq major-mode 'lisp-mode) (eq major-mode 'scheme-mode)) (locate-indirect-laml-processing-current-buffer))
          (let ((processing-file (locate-indirect-laml-processing-current-buffer)))
             (save-buffer)
             (laml-process-file processing-file)
          )
        )

        ; Here you can add other kind of mode-dependent processings

        (t (message "Can't process buffer. No processing procedure defined on current major mode."))))


; ---------------------------------------------------------------------------------------------------

(defun break-sentences-paragraph ()
 "Break the sentences in the current paragraph such that each sentence occupies a single line.
Sentences are assumed to be separated by either linefeed or double space."
 (interactive)
 (save-excursion
  (let ((fill-column 100000))
   (end-of-paragraph-text)
   (let ((end-point (point)))
     (start-of-paragraph-text)
     (let ((start-point (point)))
      (fill-paragraph nil)
        (forward-sentence 1)
        (if (looking-at "  ") (progn (delete-char 2) (insert-char 10 1)))
      (while (< (point) end-point)
        (forward-sentence 1)
        (if (looking-at "  ") (progn (delete-char 2) (insert-char 10 1)))))))))
  
(defun lisp-eval-buffer () 
  (interactive)
  (lisp-eval-region (point-min) (point-max)))

; ---------------------------------------------------------------------------------------------------

(defun forward-sexp-robust (n)
  "Do n forward-sexp and return the position of point after the moves.
In case there is not n S-expressions ahead, the point is not moved, and nil is returned."
  (condition-case nil
    (let ((p0 (point)))
            (forward-sexp n)
            (if (= p0 (point)) nil (point)))
    (scan-error nil)))

     
; ---------------------------------------------------------------------------------------------------

(defun is-inside-string ()
  "Is point inside a string"
  (save-excursion
    (let ((pt (point)))
      (beginning-of-defun 1)
      (let* ((parse-result (parse-partial-sexp (point) pt))
             (inside-string-info (nth 3 parse-result)))
        (if inside-string-info t nil)))))

(defun is-inside-comment ()
  "Is point inside a string"
  (save-excursion
    (let ((pt (point)))
      (beginning-of-defun 1)
      (let* ((parse-result (parse-partial-sexp (point) pt))
             (inside-comment-info (nth 4 parse-result)))
        (if inside-comment-info t nil)))))

(defun char-under (p)
  "Return the character at position p. p may, for instance, be the value of (point)."
  (buffer-substring-no-properties p (+ p 1)))



(defun lines-of-string (str)
  "Return the list of lines in str"
  (let ((p (find-in-string str 10)))
    (if p 
        (cons (substring str 0 p) (lines-of-string (substring str (+ p 1) (length str))))
        (list str))))


(defun find-in-string (str ch &optional start-idx)
  "Return the first postion of ch in str, starting from index start-idx. If not found, return nil. start-idx defaults to 0."
  (let ((start-idx-1 (if start-idx start-idx 0)))
   (find-in-string-1 str ch start-idx-1 (length str))))

(defun find-in-string-from-end (str ch &optional start-idx)
  "Return the last postion of ch in str, starting from index start-idx. If not found, return nil. start-idx defaults to one less than the length of str."
  (let* ((lgt (length str))
          (start-idx-1 (if start-idx start-idx (- lgt 1))))
   (find-in-string-from-end-1 str ch start-idx-1 lgt)))

(defun string-contains (str test-str &optional case-fold)
  "Does str contain test-str as a substring. If provided, and if non-nil, case-fold ask for non case sensitive string comparison.
Implemented by a straight-forward O(n*n) double loop."
  (let* ((str-0 (if case-fold (downcase str) str))
         (test-str-0 (if case-fold (downcase test-str) test-str))
         (str-lgt (length str-0))
         (test-str-lgt (length test-str-0))
        )
     (cond ((empty-string-p str-0) (empty-string-p test-str-0))
           ((empty-string-p test-str-0) t)
           ((> test-str-lgt str-lgt) nil)           
           (t (string-contains-non-empty str-0 test-str-0 str-lgt test-str-lgt)))))

; Help function to string-contains. 
; str and test-str are non-empty, and test-str is not longer than str.
(defun string-contains-non-empty (str test-str str-lgt test-str-lgt)
  (let* ((i 0)    ; index in str
         (j 0)    ; index in test-str
        )
    (catch 'result
      (while (<= i (- str-lgt test-str-lgt))
        (progn
          (while (< j test-str-lgt)
            (if (= (aref str (+ i j)) (aref test-str j))
                (progn
                  (setq j (+ j 1))
                  (if (= j test-str-lgt) (throw 'result t)))
              (progn
                (setq j test-str-lgt))
              )
            )
          (setq i (+ i 1))
          (setq j 0)))
       nil)))

(defun find-in-string-1 (str ch i lgt)
 (if (= 0 lgt)
     nil
     (progn 
       (while (and (not (eq ch (aref str i))) (< i (- lgt 1)))
         (setq i (+ i 1)))
       (if (eq ch (aref str i)) i nil))))

(defun find-in-string-from-end-1 (str ch i lgt)
 (if (= 0 lgt)
     nil
     (progn 
       (while (and (not (eq ch (aref str i))) (> i 0))
         (setq i (- i 1)))
       (if (eq ch (aref str i)) i nil))))


(defun string-line-range (str n m)
  "Return the substring of str corresponding to line number n to m (both including). The first line counts as line 1."
  (string-line-range-1 str (- n 1) (- m 1) (lines-of-string str)))

(defun string-line-range-1 (str n m str-lines)
  (cond ((> n m) "")
        ((= n m) (elt str-lines n))
        (t (concat (elt str-lines n) (char-to-string 10) (string-line-range-1 str (+ n 1) m str-lines)))))

(defun number-of-lines-in-string (str)
  "Return the number of lines in the string str"
  (let ((n 0) (i 0) (lgt (length str)))
    (while (< i lgt)
           (progn
             (if (= 10 (aref str i))
                 (setq n (+ n 1)))
             (setq i (+ i 1))))
    ( + n 1)))
    
; (defun number-interval (n m)
;   (if (<= n m) (cons n (number-interval (+ n 1) m)) nil))

(defun number-interval (n m)
  "Return a list of all integer numbers starting from n and ending with m.
If n is larger than m, return the empty list. Programmed iteratively. "
  (if (<= n m)
      (let ((res nil)
            (nr n))
        (while (<= nr m)
          (setq res (cons nr res))
          (setq nr (+ nr 1)))
        (reverse res))
      nil))


(defun depropertize-string (str)
  "Return a copy of STR with text properties removed."
  (let ((str (copy-sequence str)))
    (set-text-properties 0 (length str) nil str)
    str))


(defvar home-dir-url-prefix-map
  nil
  "A mapping from an absolute file prefixes to corresponding URLs, most specific first.
Used for transferring a URL to the clipboard when you process a LAML document with Emacs.
A list of cons pairs.
The absolute file prefix part of a file path can be substituted by the corresponding URL part.
This mapping can (and should probably) be maintained separately for each machine you use. Re-assign this variable in your .emacs file."
)


(defun absolute-file-path-to-url (absolute-file-path-0)
  "Transform the absolute-file-path to some equivalent URL, either a file://... or http:/... url.
If the first part of the file path corresponds to an entry in home-dir-url-prefix-map replace the prefix part of the file
path with the corresponding URL prefix. Relies on the value of the variable home-dir-url-prefix-map.
Returns nil if no match is found."
 (let* ((absolute-file-path (eliminate-tilde-prefix absolute-file-path-0))
        (matching-home-dir-url-prefix (match-home-dir-url-prefix absolute-file-path))
       )
  (if matching-home-dir-url-prefix
      (substitute-home-dir-url-prefix absolute-file-path matching-home-dir-url-prefix)
      nil)))


(defun match-home-dir-url-prefix (file-path)
  "Find an entry in home-dir-url-prefix-map for a prefix of file-path. Return the corresponding cons pair of home-dir-url-prefix-map"
  (linear-search 
     home-dir-url-prefix-map
     (function 
        (lambda (e) 
           (let ((lgt-car-e (length (car e)))
                 (lgt-file-path (length file-path)))
             (equal (car e) (substring file-path 0 (min lgt-car-e lgt-file-path))))
        ))
     (function (lambda (x) x))))

(defun substitute-home-dir-url-prefix (file-path matching-home-dir-url-prefix)
  "Replace a prefix of file-path accorcing to matching-home-dir-url-prefix.
Assume, as a precondition, that the car of matching-home-dir-url-prefix is a prefix of file-path."
  (let ((match-file-path (car matching-home-dir-url-prefix))
        (replacement-url (cdr matching-home-dir-url-prefix)))
    (concat
       replacement-url
       (substring file-path (length match-file-path)))))
    

(defun sexp-delimitor ()
  "Return a list of two numbers: the start and the end of the word under point. If not possible, return nil."
 (save-excursion
  (let ((re (progn (forward-sexp 1)
                 (point)))
        (rb (progn (forward-sexp -1)
                 (point))))
    (if (and rb re) (list rb (- re 1)) nil))))

(defun word-delimitor ()
  "Return a list of two numbers: the start and the end of the word under point"
 (save-excursion
  (let ((rb (progn (forward-word -1)
                 (point)))
        (re (progn (forward-word 1)
                 (point))))
    (if (and rb re) (list rb (- re 1))))))


(defun list-difference (l1 l2)
  "Return the list difference between l1 and l2. Thus, return all those elements in
l1 which are not member of l2. For an element i l1, if it belongs to l2, remove it from l1.
Else include it. A non-destructive function. Comparison done by equal via member."
  (filter 
    (function (lambda (e1) (not (member e1 l2))))
    l1))
      

(defun every-second-element (lst)
  "Return the first, third, etc element of the list lst until it is exhausted."
  (let ((lst1 lst) (res nil) (i 0))
    (while lst1
       (if (evenp i) (setq res (cons (car lst1) res)))
       (setq i (+ i 1))
       (setq lst1 (cdr lst1)))
    (reverse res)))
    

(defun multi-search-forward (search-list)
  "Search for each element in search-list, starting in the current possition of point.
An element in search-list can be a string or a structural regular expression (a list).
The effect and result of the closest match takes effect.
If no match at all occurs, nil is retured, and point is not affected. 
Else, point is moved to the end of the match, and the value of point after the match is returned."
 (let ((case-fold-search nil))   ; case sensitive search
  (let* ((start (point))
	 (search-result 
	  (mapcar 
	   (function
	    (lambda (search-element)
	      (goto-char start)
	      (cond ((stringp search-element)
		     (search-forward search-element nil t))
		    ((consp search-element)
		     (re-search-forward (regular-expression search-element) nil t))
		    (t (error "multi-search: Unknown kind of element in search list"))))) 
	   search-list))
	 (number-search-result (filter (function numberp) search-result))
	 (min-search-result (if (null number-search-result) nil (apply (function min) number-search-result))))
    (if min-search-result 
        (progn
          (goto-char min-search-result)
          min-search-result)
      nil))))

(defun vector-to-list (vector)
  "Return the list of elements of vector"
  (let ((lgt (length vector))
        (i 0)
        (res nil))
    (while (< i lgt)
      (setq res (cons (elt vector i) res))
      (setq i (+ i 1)))
    (reverse res)))



(defun string-search-forward (in-string search-string &optional from to)
  "Search for first occurrence of search-string in in-string within
the index interval from to. from is assumed to be smaller than to.
Return the index of the beginning of the first match, or nil
if no match is found. All indexes are zero bound.
The parameter from defaults to 0 and to defaults to (length in-string).
A relative primitive and simple minded implementation via a temp buffer.
Alternative: string-contains, which is an O(n*n) implementation."
  (let* ((from-1 (if from from 0))
         (to-1 (if to to (length in-string)))
         (temp-buf-name "*search-buffer*")
         (temp-buf (if (get-buffer temp-buf-name)
                       (get-buffer temp-buf-name)
                       (generate-new-buffer temp-buf-name)))
         (cur-buffer (current-buffer)))
    (set-buffer temp-buf)
    (erase-buffer)
    (insert in-string)
    (goto-char (+ from-1 1))
    (let ((res (search-forward search-string (+ to-1 1) t)))
      (kill-buffer temp-buf)
      (set-buffer cur-buffer)
      (if res (- res (+ (length search-string) 1)) nil)
      )))

(defun string-search-backward (in-string search-string &optional from to)
  "Search for last occurrence of search-string in in-string within
the index interval from to. from is assumed to larger than to.
Return the index of the beginning of the last match, or nil
if no match is found. All indexes are zero bound.
The parameter from defaults to 0 and to defaults to (length in-string).
A relative primitive and simple minded implementation via a temp buffer."
  (let* ((from-1 (if from from (length in-string)))
         (to-1 (if to to 0))
         (temp-buf-name "*search-buffer*")
         (temp-buf (if (get-buffer temp-buf-name)
                       (get-buffer temp-buf-name)
                       (generate-new-buffer temp-buf-name)))
         (cur-buffer (current-buffer)))
    (set-buffer temp-buf)
    (erase-buffer)
    (insert in-string)
    (goto-char (+ from-1 1))
    (let ((res (search-backward search-string (+ to-1 1) t)))
      (kill-buffer temp-buf)
      (set-buffer cur-buffer)
      (if res (- res 1) nil)
      )))


(defun replace-chars-in-string (str char-lst replacement)
  "In a copy of the string str, replace all occurrences of a char from char-lst with with the replacement.
replacement may be a character (a number in elisp) or a string.
This function does not alter str. It returns a modified copy."
  (let ((i 0)
        (str-copy (string-copy str)))
    (while (< i (length str-copy))
      (cond ((and (numberp replacement)  ; in the meaning of a char
                  (member (aref str-copy i) char-lst))
               (aset str-copy i replacement)
               (setq i (+ i 1)))
            ((and (stringp replacement)
                  (member (aref str-copy i) char-lst))
               (let ((new-str-copy nil))
                 (setq new-str-copy (concat (substring str-copy 0 i) replacement (substring str-copy (+ i 1) (length str-copy))))
                 (setq str-copy new-str-copy)
                 (setq i (+ i (length replacement)))))
            (t (setq i (+ i 1)))))
      str-copy))

; Old version, as of January 13, 2009:
; (defun replace-chars-in-string (str char-lst ch-to)
;   "In a copy of the string str, replace all occurrences of a char from char-lst with with the char ch-to.
; This function does not alter str. It returns a modified copy."
;   (let ((i 0)
;         (str-lgt (length str))
;         (str-copy (string-copy str)))
;     (while (< i str-lgt)
;       (if (member (aref str-copy i) char-lst)
;           (aset str-copy i ch-to))
;       (setq i (+ i 1)))
;     str-copy))

(defun string-copy (str)
  "Make and return a copy of the string str"
  (let* ((i 0)
         (str-lgt (length str))
         (res (make-string str-lgt 0)))
   (while (< i str-lgt)
     (aset res i (aref str i))
     (setq i (+ i 1)))
   res))


(defun window-list-of-frame (&optional frame)
  "Return the list of windows that appear in frame. The parameter frame0 defaults to the selected frame"  
  (let* ((frame1 (if frame frame (selected-frame)))
         (wt (window-tree frame1)))
    (if (and wt (listp wt) (= 2 (length wt)))
        (cond ((windowp (car wt))
                 (list (car wt)))  ; only a single window in frame
              ((and (listp (car wt)) (> (length (car wt)) 2))
                 (cddr (car wt)))
              (t (error "window-list-of-frame: Unexpected structure of window tree, as returned from the function window-tree (1)")))
       (error "window-list-of-frame: Unexpected structure of window tree, as returned from the function window-tree (2)"))))


(defun find-in-files (str file-list)
  "Search for the string str in the list of files file-list (list of full and absolute file paths).
We only care about the first match. If successful, return a cons pair of (file-path . location), where
location is the point of the first match. else return nil."
  (catch 'result
    (let ((list-of-files file-list)
          (temp-buf (get-buffer-create "temp-search-buffer"))
         )
      (while (not (null list-of-files))
        (let ()
          (set-buffer temp-buf) 
          (erase-buffer)
          (insert-file-contents (car list-of-files))
          (goto-char (point-min))
          (let ((res (search-forward str nil t)))
            (if res 
                (throw 'result (cons (car list-of-files) res))
                (setq list-of-files (cdr list-of-files))))))
        nil ; result if reg-exp is not found in any file
     )))

(defun re-find-in-files (reg-exp file-list)
  "Search for the regular expression reg-exp in the list of files file-list (list of full and absolute file paths).
We only care about the first match. If successful, return a cons pair of (file-path . location), where
location is the point of the first match. else return nil."
  (catch 'result
    (let ((list-of-files file-list)
          (temp-buf (get-buffer-create "temp-search-buffer"))
         )
      (while (not (null list-of-files))
        (let ()
          (set-buffer temp-buf) 
          (erase-buffer)
          (insert-file-contents (car list-of-files))
          (goto-char (point-min))
          (let ((res (re-search-forward reg-exp nil t)))
            (if res 
                (throw 'result (cons (car list-of-files) res))
                (setq list-of-files (cdr list-of-files))))))
        nil ; result if reg-exp is not found in any file
     )))

(defun make-file-in-directory (dir file-name file-contents)
  "Make a file named file-name in directory dir with initial contents file-contents (a string)."
  (let ((new-buffer (create-file-buffer (concat dir file-name))))
    (set-buffer new-buffer)
    (insert file-contents)
    (write-file (concat dir file-name))
    (kill-buffer new-buffer)))

(defun append-string-to-file (file-path additional-contents)
  "Append the string additional-contents to the existing or non-existing file file-path (absolute file path).
In case file-path is non-existing, insert additional-contents into an empty file."
  (if t
      (let ((buffer (find-file file-path)))
        (set-buffer buffer)
        (goto-char (point-max))
        (insert additional-contents)
        (write-file file-path)
        (kill-buffer buffer))
      (error "Cannot append to non-existing file %s" file-path)))


(defun most-recent-file-visiting (prefix)
  "Return the most recent visited file with the given prefix following the value of home-user-dir"
  (let* ((buf-list (buffer-list))
         (buffer-file-path-list (mapcar (function buffer-file-name) buf-list))
         (matcing-buffer-file-path-list (filter (function (lambda (file-path) (string-starts-with file-path (concat home-user-dir prefix)))) buffer-file-path-list)))
    (if (null matcing-buffer-file-path-list)
        nil
        (car matcing-buffer-file-path-list))))

(defun most-recent-file-with-extension (ext)
  "Return the most recent visited file with the given file extension."
  (let* ((buf-list (buffer-list))
         (buffer-file-path-list (mapcar (function buffer-file-name) buf-list))
         (matcing-buffer-file-path-list (filter (function (lambda (file-path) (equal (file-name-extension file-path) ext))) buffer-file-path-list)))
    (if (null matcing-buffer-file-path-list)
        nil
        (car matcing-buffer-file-path-list))))

(defun string-starts-with (in-string prefix)
  "Does in-string start with prefix string. Returns nil on non-string parameters. Not efficient due to use of string-search-forward."
  (if (and (stringp in-string) (stringp prefix))
      (let ((search-res (string-search-forward in-string prefix)))
        (and search-res (= 0 (string-search-forward in-string prefix))))
      nil))
      


(defun column-number-of (p)
  "Return the column number of position p (such as (point))"
  (save-excursion
    (beginning-of-line 1)
    (- p (point))))


; ---------------------------------------------------------------------------------------------------

(defun as-string(x)
  "Convert x to a string. Returns \"??\" if it is not possible to convert."
  (cond ((numberp x) (number-to-string x))
        ((booleanp x) 
            (if x "true" "false"))
        ((symbolp x) (symbol-to-string x))
        ((stringp x) x)
        ((listp x)
            (concat "(" 
               (string-merge (mapcar (function as-string) x) (make-list (- (length x) 1) " "))
               ")"))
        ((vectorp x)
          (let ((lst (vector-to-list x)))
            (concat "[" 
               (string-merge (mapcar (function as-string) lst) (make-list (- (length lst) 1) " "))
               "]")))
        ; add array
        (t "??")))

(defun as-symbol (x)
  "Convert x to a symbol. A fatal error occurs if it is not possible convert."
  (cond ((booleanp x) 
            (if x (as-symbol "true") (as-symbol "false")))
        ((symbolp x) x)
        ((stringp x) (string-to-symbol x))
        (t (error "as-symbol. Cannot convert to string"))))

(defun as-number (x)
  "Convert x to a number. Causes a fatal error if the conversion is not possible"
  (cond ((booleanp x) (if x 1 0))
        ((and (stringp x) (numeric-string-maybe-float x)) (string-to-number x))
        ((symbolp x) (as-number (symbol-to-string x)))
        ((numberp x) x)  ; false -> 0, true -> 1
        (t (error "as-number. Cannot convert to number."))))

(defvar number-constituent-list (list 48 49 50 51 52 53 54 55 56 57  46 45) "The list of chars that are can occur in a number: 0-9 and . and -")

(defun numeric-string-maybe-float (str)
  "Is str is a numeric decimal string with a possible point \".\"?"
  (let ((str-lgt (length str)))
  (if (> str-lgt 0)
      (catch 'result
        (let ((i 0))
           (while (< i str-lgt)
              (if (not
                    (or
                     (and (>= (aref str i) 48) (<= (aref str i) 57)) ; ciffer
                     (= (aref str i) 46) ; decimal point
                     (= (aref str i) 45) ; -
		    ))
                  (throw 'result nil))
              (setq i (+ i 1))
           )
           t))
      nil)))

; (defun numeric-string-maybe-float (str)
;   "Is str is a numeric decimal string with a possible point \".\"?"
;   (let* ((str-lst (string-to-list str))
;          (non-numeric-elements (filter (function (lambda (ch) (not (member ch number-constituent-list)))) str-lst)))
;     (and (> (length str) 0) (null non-numeric-elements))))

(defun string-merge (str-list-1 str-list-2)
 (cond ((null str-list-1) (apply (function concat) str-list-2))
       ((null str-list-2) (apply (function concat) str-list-1))
       (t (concat
              (car str-list-1) (car str-list-2)
              (string-merge (cdr str-list-1) (cdr str-list-2))))))

(defun as-list (x)
  "Convert x to a list."
  (cond ((stringp x) (string-to-list x))
        ((listp x) x)
        ((consp x) x)
        ((vectorp x) (vector-to-list x)) 
        (t (list x))))

; ---------------------------------------------------------------------------------------------------------------

(defun name-under-point ()
  "Return the word under point, delimited by sexp"
  (let* ((wd (sexp-delimitor))  
         (first (car wd)) 
         (last (+ 1 (cadr wd)))
         (word (buffer-substring-no-properties first last)))
    word))

(defun list-of-directories (dir)
 "Return a list of directories in DIR. Each entry in the
list is a string. The list does not include the current directory
and the parent directory."
 (filter (function (lambda (x) (not (or (equal x ".") (equal x "..")))))
  (filter
   (function (lambda (x)
	       (file-directory-p (concat dir x))))
   (directory-files dir))))

(defun list-of-files (dir)
  "Return a list of regular files in DIR. Each entry in the
list is a string."
  (filter
   (function (lambda (x)
	       (file-regular-p (concat dir x))))
   (directory-files dir)))

; ---------------------------------------------------------------------------------------------------------------

; Finding Scheme definitions (taken out of laml-misc-tools.el) - I forgot why.

(defvar scheme-file-position-stack nil "A list of stored positions in a this Scheme source file.")
(defvar scheme-file-ghost-position-stack nil "A auxiliary list of stored positions in a this Scheme source file.")
; (make-variable-buffer-local 'scheme-file-position-stack)
; (make-variable-buffer-local 'scheme-file-ghost-position-stack)

(defun push-scheme-file-position (p)
  (setq scheme-file-position-stack (cons p scheme-file-position-stack))
  (setq scheme-file-ghost-position-stack nil))

(defun pop-scheme-file-position ()
  (interactive)
  (if (not (null scheme-file-position-stack))
      (let ((top (car scheme-file-position-stack))
            (cur-pos (point)))
         (goto-char top) (recenter)
         (setq scheme-file-ghost-position-stack (cons cur-pos scheme-file-ghost-position-stack))
         (setq scheme-file-position-stack (cdr scheme-file-position-stack))
      )
      (message "No more places to come back to...")))

(defun pop-ghost-scheme-file-position ()
  (interactive)
  (if (not (null scheme-file-ghost-position-stack))
      (let ((top (car scheme-file-ghost-position-stack)))
         (goto-char top) (recenter)
         (setq scheme-file-ghost-position-stack (cdr scheme-file-ghost-position-stack))
         (setq scheme-file-position-stack (cons top scheme-file-position-stack))
      )
      (message "No more places to go forward...")))
  
      

(defun find-scheme-definition-of (name)
  "Find the Scheme defintion of name in the current file, and goto that place"
  (interactive (list (name-under-point)))
  (let* ((define-reg-exp (regular-expression 
                          `(or (concat "(" (zero-or-more " ") "define" (zero-or-more " ") ,name)
                               (concat "(" (zero-or-more " ") "define" (zero-or-more " ") "(" (zero-or-more " ")  ,name))))
         (cur-pos (point)))
    (goto-char (point-min))
    (let ((search-res (re-search-forward define-reg-exp nil t)))
      (if search-res
          (progn (push-scheme-file-position cur-pos) (goto-char search-res) (re-search-backward define-reg-exp nil t) (recenter))
          (progn (goto-char cur-pos) (message "Cannot find a definition of %s in this file" name))))))


; ---------------------------------------------------------------------------------------------------------------


; Added May 25, 2010.

(defun black-background ()
  (interactive)
  (set-background-color "black")
  (set-foreground-color "white"))

(defun white-background ()
  (interactive)
  (set-background-color "white")
  (set-foreground-color "black"))

(defun red-background ()
  (interactive)
  (set-background-color "red")
  (set-foreground-color "white"))

(defun green-background ()  
  (interactive)
  (set-background-color "green")
  (set-foreground-color "black"))


(defun assoc-get (key a-list)
  (let ((r (assq key a-list)))
    (if r (cadr r) nil)))

(defun assoc-get-cdr (key a-list)
  (let ((r (assq key a-list)))
    (if r (cdr r) nil)))


(defun list-to-string-with-sep (string-list sep)
  (cond ((null string-list) "")
        ((null (cdr string-list)) (car string-list))
        (t (concat (car string-list) sep (list-to-string-with-sep (cdr string-list) sep)))))


(defun is-current-buffer-blank()
  (save-excursion
     (goto-char (point-min))
     (while (looking-at-chars (point) white-space-char-list) (forward-char 1))
     (= (point) (point-max))
  )
)


(defun hex-string-to-int (hex-str)
  "Convert the string hex-str, which may contain spaces, to the corresponding integer.
Spaces are simply ignored. The inverse function is int-to-hex-string."
  (let ((res 0)
        (i (- (length hex-str) 1))
        (pow 0))
    (while (>= i 0)
      (let ((ch (aref hex-str i)))
        (if (= ch 32)
            (setq i (- i 1))
            (progn
              (setq res (+ res (* (hex-ciffer-value (aref hex-str i)) (power 16 pow))))
              (setq i (- i 1))
              (setq pow (+ pow 1))))))
    res))

(defun int-to-hex-string (n &optional space-sep)
  "Convert a non-negative integer n to a hex string. The inverse function of hex-string-to-int.
The boolean space-sep, if provided and non-nil, provides for space separation of pairs
of hex digits"
  (let ((nb n)
        (rest 0)
        (i 0)
        (res ""))
    (if (= n 0) 
        (if space-sep "00" "0")
        (progn
          (while (> nb 0)
            (setq rest (% nb 16))
            (setq nb (/ nb 16))
            (setq i (+ i 1))
            (if (and space-sep (not (= i 1)) (= (% i 2) 1)) (setq res (concat " " res)))
            (setq res (concat (int-to-hex-ciffer rest) res)))
          (if (and space-sep (= (% i 2) 1))
              (concat "0" res)
            res)))))

(defun int-to-hex-ciffer (i)
   "Return the hex ciffer of i, as a string. i must be between 0 and 15.
Ex: 0 to \"0\", 15 to \"f\"."
   (cond ((and (>= i 0) (<= i 9))   (char-to-string (+ i 48)))
         ((and (>= i 10) (<= i 15)) (char-to-string (+ i 55)))
         (t (error "int-to-hex-ciffer: i must be between 0 and 15 (incl) %d" i))))

(defun hex-ciffer-value (ch)
  "Given an legal ASCII ciffer character (an int in E-lisp), return its ciffer value"
  (cond ((and (>= ch 48) (<= ch 57))  ; 0-9
           (- ch 48))
        ((and (>= ch 97) (<= ch 102)) ; a-f
           (- ch 87))
        ((and (>= ch 65) (<= ch 70))  ; A-F
           (- ch 55))
        (t (error "hex-ciffer-value: Illegal ciffer: %c" ch))))

(defun power(a b)
  "Raise a to the power of b. The parameter b must be an integer, and a must be numeric."
  (let ((res 1)
        (count 1))
    (while (<= count b)
       (setq res (* a res))
       (setq count (+ count 1)))
    res))

(defun read-text-file (path)
 (save-excursion
  (let ((buf (get-buffer-create "read-buffer")))
    (set-buffer buf)
    (insert-file-contents path)
    (let ((res (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer buf)
      res))))


(defun delete-sexp (n)
  (let ((p0 (point)))
     (forward-sexp n)
     (delete-region p0 (point))))

(defun delete-line (n)
  (let ((p0 (point)))
    (end-of-line n) (while (looking-at-chars (point) (list 10 13)) (forward-char 1))
    (delete-region p0 (point))))


(defun file-write-no-message (lst file)
  "Write the list lst to file (full path) without any feedback in the minibuffer."
  (save-excursion
    (let ((temp-buf (generate-new-buffer "file-writing")))
      (set-buffer temp-buf)
      (prin1 lst temp-buf)
      (write-region (point-min) (point-max) file nil 1)  ; trailing 1 suppresses feedback in minibuffer.
      (kill-buffer temp-buf)
)))

(defun next-line-and-highlight-line ()
  (interactive)
  (next-line 1)
  (highlight-this-line)
)

(defun previous-line-and-highlight-line ()
  (interactive)
  (previous-line 1)
  (highlight-this-line))

(defun highlight-this-line ()
 (let ((pn nil))
  (transient-mark-mode 1)
  (end-of-line 1)
  (setq pn (point))
  (beginning-of-line 1)
  (while (looking-at-chars (point) '(9 10 13 32))   ; white space
         (forward-char 1))
  (set-mark pn)
  (setq mark-active 1)
  (setq deactivate-mark nil)
 ))

(defun highlight-this-region (reg-start reg-end)
  (transient-mark-mode 1)
  (set-mark reg-end)
  (goto-char reg-start)
  (setq mark-active 1)
  (setq deactivate-mark nil)
)

; ----------------------------------------------------------------------------------------------------------------------
; GRADUAL INSERTION OF TEXT STRINGS.
; Simulated human typing.

(defvar typing-speed-factor 0.35 "A positive number multiplied to the waiting time between characters 'typed' by insert-gradually")

(defvar fast-typing-thresshold 170 "The thresshold where a factor of long-string-speedup-factor speed up is effectuated.")

(defvar long-string-speedup-factor 2 "The speed up factor after a number of fast-typing-thresshold have been entered pr. simulation")

(defvar extra-spaces-after-CR 0 "Extra spaces to be typed after CR. Can be used to handle indentation appropriately in multi-line text.") 

(defvar typing-timing-0 (list 0.1 0.07 0.05 0.01 0.03 0.012 0.08 0.35 0.73 0.1 0.05 0.15 0.01 0.15 0.09 0.2 0.099 0.3 0.1 0.06) "A list of pauses (in seconds) between individual characters being typed.")

(defvar typing-timing-1 (list 0.1 0.07 0.05 0.01 0.03 0.012 0.08 0.35 0.73 0.1 0.05 0.15 -1.0 0.01 0.15 0.09 0.2 0.099 0.3 0.1 0.06) "A list of pauses (in seconds) between individual characters being typed.")

(defvar typing-timing-2 (list  0.15 0.01 0.15 0.09 0.2 0.099 0.3 0.1 0.06) "A list of pauses (in seconds) between individual characters being typed.")

(defvar typing-timing-3 (list  0.15 0.01 0.15 0.09 0.2 0.099 0.3 0.15 -1 0.06) "A list of pauses (in seconds) between individual characters being typed.")

(defvar current-typing-rythm 'random "A symbol used for selection of the simulated typing rythm")

(defun insert-gradually (str)
  (let* ((str-lst (string-to-list str))
         (sit-times (actual-typing-rythm current-typing-rythm))
         (factor typing-speed-factor)
         (i 0)
        )
    (while str-lst
      (insert (char-to-string (car str-lst)))        ; insert a character
      (if (= (car str-lst) 10) ; CR
          (insert (make-string extra-spaces-after-CR 32)))
      (sit-for (* factor (abs (car sit-times))))     ; pause for some time
      (if (< (car sit-times) 0)        
          (progn
             (backward-delete-char-untabify 1)
             (sit-for (* factor 2.0))
          )  
          (setq str-lst (cdr str-lst))
      )
      (setq sit-times (cdr sit-times))
      (if (null sit-times) (setq sit-times (actual-typing-rythm current-typing-rythm)))
      (setq i (+ i 1))
      (if (= i fast-typing-thresshold) (setq factor (/ factor long-string-speedup-factor))) ; speedup after character number 70
    )
  )
)

(defun faster-typing ()
  "Provide for doubling the typing speed"
  (interactive)
  (setq typing-speed-factor (/ typing-speed-factor 2))
  (message "Done. Typing speed doubled. Now: %f." typing-speed-factor)
)

(defun slower-typing ()
  "Provide for halfing the typing speed"
  (interactive)
  (setq typing-speed-factor (* typing-speed-factor 2))
  (message "Done. Half speed typing. Now: %f." typing-speed-factor)
)

(defun set-typing-rythm (kind)
  (interactive 
    (list (as-symbol (completing-read "Kind: " (list "manual-long" "manual-short" "manual-long-with-regress" "manual-short-with-regress" "random" "random-with-regress")))))
  (setq current-typing-rythm kind))

(defun actual-typing-rythm (kind)
  (cond ((eq kind 'manual-long) typing-timing-0)
        ((eq kind 'manual-long-with-regress) typing-timing-1)
        ((eq kind 'manual-short) typing-timing-2)
        ((eq kind 'manual-short-with-regress) typing-timing-3)
        ((eq kind 'random) (random-typing-times-of-length 100))
        ((eq kind 'random-with-regress) (random-typing-times-with-regress-of-length 100))
        (t (random-typing-times-of-length 100))))

(defun random-typing-times-with-regress-of-length (n)
  (let ((result nil)
        (i 0))
   (while (< i n)
      (let ((r (abs (/ (float (random)) (* most-positive-fixnum 3)))))
         (if (>= r 0.04)
             (setq result (cons r result))
             (setq result (cons -1 result))))
      (setq i(+ i 1)))
   result))

(defun random-typing-times-of-length (n)
  (let ((result nil)
        (i 0))
   (while (< i n)
      (setq result (cons  (abs (/ (float (random)) (* most-positive-fixnum 3))) result))
      (setq i(+ i 1)))
   result))

; ---------------------------------------------------------------------------------------------------------------
; Gradual insertion of S-expressions from a given, existing buffer. Quite specific to a Scheme REPL in an Emacs buffer - Inferior Lisp Mode.

(defvar current-sexpr-number 0 "The number of the next dialogue item, when initially incremented.")
(defvar sexpr-buffer-name nil "The name of an existing buffer from which to take dialogue items. The named buffer must contain S-expressions") 
(defvar auto-comint-send nil "Send inserted text to comint automatically if true")

(defun insert-next-sexpr-item ()
  "Insert the next dialogue item, an S-expression, from the designated buffer named sexpr-buffer-name"
  (interactive)
  (setq current-sexpr-number (+ current-sexpr-number 1))
  (let ((dialogue-line (get-sexpr-from-buffer sexpr-buffer-name current-sexpr-number)))
    (goto-char (point-max))
    (clear-current-expression-if-any)
    (insert-gradually dialogue-line)
    (if auto-comint-send (comint-send-input))))

(defun insert-previous-sexpr-item ()
  "Insert the previous dialogue item, an S-expression, from the designated buffer named sexpr-buffer-name"
  (interactive)
  (if (> current-sexpr-number 1) (setq current-sexpr-number (- current-sexpr-number 1)) (error "Cannot"))
  (let ((dialogue-line (get-sexpr-from-buffer sexpr-buffer-name current-sexpr-number)))
    (goto-char (point-max))
    (clear-current-expression-if-any)
    (insert-gradually dialogue-line)
    (if auto-comint-send (comint-send-input))))

(defun clear-current-expression-if-any ()
  (let ((need-to-clear nil))
    (save-excursion
      (backward-char 2)
      (setq need-to-clear (not (looking-at "> "))))
    (if need-to-clear 
        (progn
          (backward-sexp 1)
          (kill-sexp 1)))))


(defun get-sexpr-from-buffer (buf-name dialogue-number)
  "Get dialogue item number dialog-number from the existing buffer named buf-name." 
  (save-window-excursion
    (set-buffer buf-name)
    (goto-char (point-min))
    (forward-sexp dialogue-number)
    (backward-sexp 1)
    (let ((p0 (point)))
      (forward-sexp 1)
      (buffer-substring-no-properties p0 (point)))))

; (define-key inferior-lisp-mode-map [right] 'insert-next-sexpr-item)
; (define-key inferior-lisp-mode-map [left] 'insert-previous-sexpr-item)
; (define-key inferior-lisp-mode-map [home] 'asl-setup)
; (define-key inferior-lisp-mode-map [prior] 'faster-typing)
; (define-key inferior-lisp-mode-map [next] 'slower-typing)

; ---------------------------------------------------------------------------------------------------------------
; Gradual insertion of text pieces from files in a particular directory pointed out by the variable text-piece-directory.
; Insert files with non-negative integer names, in numeric order. File names do not need to be consecutive.
; You must assign the variable text-piece-directory to hold the full path name of an existing directory.

(defvar current-text-piece-number 0 "The current text piece number")
(defvar text-piece-directory nil "The full path to an existing directory with text pieces in files with integer names")

(defun insert-next-text-piece ()
  (interactive)
  (setq current-text-piece-number (+ current-text-piece-number 1))
  (let* ((text-piece-file-names (sort (mapcar (function as-number) (filter (function numeric-string) (eliminate-dot-and-dot-dot (directory-files text-piece-directory)))) (function <=))))
    (if (> current-text-piece-number (length text-piece-file-names)) (setq current-text-piece-number 1)) ; wrap
    (if (and (>= current-text-piece-number 1) (<= current-text-piece-number (length text-piece-file-names)))
        (let ((text-piece (read-text-file (concat text-piece-directory (as-string (nth (- current-text-piece-number 1) text-piece-file-names))))))
          (insert-gradually text-piece))
        (message "Cannot"))))

(defun goto-next-text-piece ()
  (interactive)
  (setq current-text-piece-number (+ current-text-piece-number 1))
  (message "Next: %d" (+ current-text-piece-number 1)))

(defun goto-previous-text-piece ()
  (interactive)
  (setq current-text-piece-number (- current-text-piece-number 1))
  (message "Next: %d" (+ current-text-piece-number 1)))

(defun insert-previous-text-piece ()
  (interactive)
  (setq current-text-piece-number (- current-text-piece-number 1))
  (let* ((text-piece-file-names (sort (mapcar (function as-number) (filter (function numeric-string) (eliminate-dot-and-dot-dot (directory-files text-piece-directory)))) (function <=))))
    (if (<= current-text-piece-number 0) (setq current-text-piece-number (length text-piece-file-names)))  ; wrap
    (if (and (>= current-text-piece-number 1) (<= current-text-piece-number (length text-piece-file-names)))
        (let ((text-piece (read-text-file (concat text-piece-directory (as-string (nth (- current-text-piece-number 1) text-piece-file-names))))))
          (insert-gradually text-piece))
        (message "Cannot"))))

; (define-key c-mode-map [S-up]   (function insert-previous-text-piece))
; (define-key c-mode-map [S-down] (function insert-next-text-piece))
; (define-key c-mode-map [C-S-up]   (function goto-previous-text-piece))
; (define-key c-mode-map [C-S-down] (function goto-next-text-piece))
; (define-key c-mode-map [S-home] (function (lambda () (interactive) (setq current-text-piece-number 0) (message "Done. Next = 1."))))


; ---------------------------------------------------------------------------------------------------------------

; Generally useful
(defun swap-sexp-with-previous ()
  "Swap the currently selected S-expression with the previous (sibling) S-expression.
Fails if the selected S-expression is the first in its enclosing list.
As both a precondition and postcondition (invariant) the start parenthesis of the S-expression is selected."
  (interactive)
  (transpose-sexps 1)
  (backward-sexp 2))

; Generally useful
(defun swap-sexp-with-next ()
  "Swap the currently selected S-expression with the next (sibling) S-expression.
Fails if the selected S-expression is the last in its enclosing list.
As both a precondition and postcondition (invariant) the start parenthesis of the S-expression is selected."
  (interactive)
  (forward-sexp 1) (while (looking-at-white-space (point)) (forward-char 1))
  (transpose-sexps 1)  
  (backward-sexp 1))
            

(defun string-suffix (str n)
  "Return the n last characters of str, if str is of length less than n. Else return str"
  (if (< (length str) n)
      str
      (substring str (- (length str) n))))
  
(defun delete-string-prefix (char-list str)
  "Return a shorter string, in which the longest prefix that consists of characters from char-list, is deleted."
  (let ((str-lgt (length str))
        (i 0))
    (while (and (< i str-lgt) (member (aref str i) char-list)) (setq i (+ i 1)))
    (substring str i str-lgt)))
        
; ---------------------------------------------------------------------------------------------------------------
; FRAME FONT SIZE CONTROL - as of this version: works only in Windows version of GNU Emacs (because of font names).

(defvar current-frame-font-size 12 "The current font size, as set by the command set-size-of-frame-font-courier-new")
(defvar current-frame-font-weight "normal" "The current font weight, as set by the command set-size-of-frame-font-courier-new")
(defvar frame-font-sizes (list 16 12 9 7) "The frame font sizes toggled between by the following function")
(defvar current-frame-font-size-number 0)

; Used to enlarge frame, for instance in an auditorium during teaching:
(defun set-size-of-frame-font-courier-new (size weight)
 (interactive 
    (list
      (read-from-minibuffer "Font size of courier new font: ")
      (completing-read "Weight (normal or bold): " '("normal" "bold"))))
 (setq current-frame-font-size (as-number size))
 (setq current-frame-font-weight weight)
 (set-frame-font  (concat "-*-Courier New-" weight "-r-*-*-" (as-string size) "-*-*-*-c-*-iso8859-1") t))

; Commands for increasing and decreasing frame font size:

(defun increase-frame-font-size-normal ()
  (interactive)
  (setq current-frame-font-size (+ current-frame-font-size 1))
  (setq current-frame-font-weight "normal")
  (set-size-of-frame-font-courier-new current-frame-font-size current-frame-font-weight)
  (message "Current font size %d, normal" current-frame-font-size))

(defun decrease-frame-font-size-normal ()
  (interactive) 
  (setq current-frame-font-size (- current-frame-font-size 1))
  (setq current-frame-font-weight "normal")
  (set-size-of-frame-font-courier-new current-frame-font-size current-frame-font-weight)
  (message "Current font size %d, normal" current-frame-font-size))

(defun increase-frame-font-size-bold ()
  (interactive)
  (setq current-frame-font-size (+ current-frame-font-size 1))
  (setq current-frame-font-weight "bold")
  (set-size-of-frame-font-courier-new current-frame-font-size current-frame-font-weight)
  (message "Current font size %d, bold" current-frame-font-size))

(defun decrease-frame-font-size-bold ()
  (interactive)
  (setq current-frame-font-size (- current-frame-font-size 1))
  (setq current-frame-font-weight "bold")
  (set-size-of-frame-font-courier-new current-frame-font-size current-frame-font-weight)
  (message "Current font size %d, bold" current-frame-font-size))

(defun set-frame-font-sizes (numbers)
  (interactive "XList of frame font size among which to toggle (a Lisp list valued expression): ")
  (setq frame-font-sizes numbers))

(defun toggle-frame-font-size-up ()
  (interactive) 
  (setq current-frame-font-size-number (mod (+ current-frame-font-size-number 1) (length frame-font-sizes)))
  (setq current-frame-font-size (nth current-frame-font-size-number frame-font-sizes))
  (set-size-of-frame-font-courier-new current-frame-font-size current-frame-font-weight)
  (message "Current font size %d, bold" current-frame-font-size))

(defun toggle-frame-font-size-down ()
  (interactive) 
  (setq current-frame-font-size-number (mod (- current-frame-font-size-number 1) (length frame-font-sizes)))
  (setq current-frame-font-size (nth current-frame-font-size-number frame-font-sizes))
  (set-size-of-frame-font-courier-new current-frame-font-size current-frame-font-weight)
  (message "Current font size %d, bold" current-frame-font-size))

; Keybindings: typically in .emacs:
; (define-key global-map [S-prior] 'increase-frame-font-size-normal)
; (define-key global-map [S-next] 'decrease-frame-font-size-normal)
; (define-key global-map [C-prior] 'increase-frame-font-size-bold)
; (define-key global-map [C-next] 'decrease-frame-font-size-bold)
;  
; (define-key global-map [S-C-next]  'toggle-frame-font-size-up)
; (define-key global-map [S-C-prior] 'toggle-frame-font-size-down)

; ---------------------------------------------------------------------------------------------------------------

(defun eliminate-dot-and-dot-dot (file-name-list)
  (filter (function (lambda (fn) (and (not (equal fn ".")) (not (equal fn ".."))))) file-name-list))

(defvar ciffer-list (list 48 49 50 51 52 53 54 55 56 57) "The list of chars that are ciffers: 0-9")

(defun numeric-string (str)
  "Is the string str numeric"
  (let* ((str-lst (string-to-list str))
         (non-numeric-elements (filter (function (lambda (ch) (not (member ch ciffer-list)))) str-lst)))
    (and (> (length str) 0) (null non-numeric-elements))))