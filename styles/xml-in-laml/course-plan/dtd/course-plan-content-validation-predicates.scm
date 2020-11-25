; (course-info, course-intro?, (lecture-plan)* 
; (define (course-plan-course-plan-1-checker? contents)
;  (let ((contents-1 (if (list? contents) (filter (negate white-space-related?) contents) contents)))
;    (cond ((not (list? contents-1))	; #f - no contents probably
; 	  (xml-add-problem! 
;             (xml-enrich-error-message "The  course-plan element cannot be empty: it must at least have a course-info clause" contents-1)))
; 
;          ((and (list? contents-1) (null? contents-1))
;           (xml-add-problem! 
;             (xml-enrich-error-message "The  course-plan element cannot be empty: it must at least have a course-info clause" contents-1))) 
; 
; 	 ((and (list? contents-1) (>= (length contents-1) 1) (not (or (equal? (ast-element-name (first contents-1)) "course-info") (equal? (ast-element-name (first contents-1)) "course-intro"))))
;            (xml-add-problem! 
;             (xml-enrich-error-message "The first element of a  course-plan  element must be a course-intro or a course-info element" contents-1)))
; 
; 	 ((and (list? contents-1) (> (length contents-1) 1) (equal? (ast-element-name (first contents-1)) "course-intro")
;                (equal? (ast-element-name (second contents-1)) "course-info"))
;             (if (not (all-ast-member-of? (list "lecture-plan") (cddr contents-1)))
;                    (xml-add-problem! 
; 		    (xml-enrich-error-message "The  course-plan  element must be ended with zero, one, or more lecture-plan elements. Only a single course-intro is allowed" contents-1))))
; 
; 	 ((and (list? contents-1) (> (length contents-1) 1) (equal? (ast-element-name (first contents-1)) "course-info"))
;             (if (not (all-ast-member-of? (list "lecture-plan") (cdr contents-1)))
;                    (xml-add-problem! 
; 		    (xml-enrich-error-message "The  course-plan  element must be ended with zero, one, or more lecture-plan elements" contents-1))))
; 
; 
; 	 (else (xml-add-problem! 
; 		    (xml-enrich-error-message "The  course-plan  element be of the form: course-info course-intro? lecture-plan" contents-1))))))