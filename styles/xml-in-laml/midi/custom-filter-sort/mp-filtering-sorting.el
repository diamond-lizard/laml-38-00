; ONE OF THE MAIN PARTS:
(defun mp-filter-function (entry)
  (member (m-section-name entry) (list "Main-A" "Main-B" "Main-C" "Main-D")))

; ONE OF THE MAIN ENDINGS:
(defun mp-filter-function (entry)
  (member (m-section-name entry) (list "Ending-A" "Ending-B" "Ending-C" "Ending-D")))

; ONE OF THE MAIN INTROS:
(defun mp-filter-function (entry)
  (member (m-section-name entry) (list "Intro-A" "Intro-B" "Intro-C" "Intro-D")))

; ONE OF THE MAIN FILLS:
(defun mp-filter-function (entry)
  (and
     (member (m-section-name entry) (list "Fill-In-AA" "Fill-In-BB" "Fill-In-CC" "Fill-In-DD" "Fill-In-BA"))))

; KEY SIGNATURE 3/4
(defun mp-filter-function (entry)
 (let ((ts (m-time-signature entry)))
   (and (= (car ts) 3) (= (cadr ts) 4))))

; KEY SIGNATURE 2/4
(defun mp-filter-function (entry)
 (let ((ts (m-time-signature entry)))
   (and (= (car ts) 2) (= (cadr ts) 4))))

(defun mp-filter-function (entry)
 (let ((ts (m-time-signature entry)))
   (not
     (or (and (= (car ts) 3) (= (cadr ts) 4))
         (and (= (car ts) 4) (= (cadr ts) 4))))))

(defun mp-filter-function (entry)
 (let ((nn (m-number-of-notes entry)))
   (> nn 7)))

(defun mp-filter-function (entry)
 (let ((nn (m-number-of-different-notes entry)))
   (> nn 20)))


; ---------------------------------------------------------------------------------------------------
; Comparison functions - for sorting:

(defun mp-leq (e1 e2)
  (<= (m-number-of-notes e1) (m-number-of-notes e2)))

(defun mp-leq (e1 e2)
  (<= (m-number-of-quarter-notes e1) (m-number-of-quarter-notes e2)))

(defun mp-leq (e1 e2)
  (<= (m-channel e1) (m-channel e2)))

(defun mp-leq (e1 e2)
  (or (equal (m-instrument-name e1) (m-instrument-name e2))
      (string-lessp (m-instrument-name e1) (m-instrument-name e2))))

(defun mp-leq (e1 e2)
  (<= (length (m-instrument-name e1)) (length (m-instrument-name e2))))


(defun mp-leq (e1 e2)
  (or (equal (m-section-name e1) (m-section-name e2))
      (string-lessp (m-section-name e1) (m-section-name e2))))


(defun mp-filter-function (entry)
  (and
     (equal (m-style-name entry) "BritPop")
     (>= (m-channel entry) 9)
     (member (m-section-name entry) (list "Main-A" "Main-B" "Main-C" "Main-D"))))

