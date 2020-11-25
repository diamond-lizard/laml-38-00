
(define (bike-bike-management-checker? contents)
 (let ((contents-1 (if (list? contents) (filter (negate white-space-related?) contents) contents)))
   (cond ((not (list? contents-1))	; #f - no contents probably
	  (xml-add-problem! 
            (xml-enrich-error-message 
              "The  bike  element cannot be empty: it must at least have a frame and a wheel element" contents-1)))

         ((and (list? contents-1) (null? contents-1))
          (xml-add-problem! 
            (xml-enrich-error-message 
              "The  bike  element cannot be empty: it must at least have a frame and a wheel element" contents-1)))

	 ((and (list? contents-1) (= (length contents-1) 1))
           (xml-add-problem! 
            (xml-enrich-error-message "A  bike  element must be a frame element and at least one wheel" contents-1))) 

	 ((and (list? contents-1) (>= (length contents-1) 1) (not (equal? (ast-element-name (first contents-1)) "frame")))
           (xml-add-problem! 
            (xml-enrich-error-message "The first element of a  bike  element must be a frame element" contents-1)))

	 ((and (list? contents-1) (>= (length contents-1) 2) (not (equal? (ast-element-name (second contents-1)) "wheel")))
           (xml-add-problem! 
            (xml-enrich-error-message "The second element of a  bike  element must be a wheel element" contents-1)))

	 ((and (list? contents-1) (>= (length contents-1) 2) (equal? (ast-element-name (second contents-1)) "wheel"))
           (check-star-sequence! (list "wheel" "brake" "lock") (cddr contents-1) "bike"))

	 (else #f))))


; Assume that seq-list is a list of strings, of the form ("x" "y" "z"), zero or more elements.
; Check that contents satisfy the content model   x*, y*, z* .
(define (check-star-sequence! seq-list contents el-name)
  (check-star-sequence-1! seq-list contents el-name seq-list))

(define (check-star-sequence-1! seq-list contents el-name orig-seq-list)
  (let ((pp-with-stars (lambda (lst) (list-to-string (map (lambda (e) (string-append e "*")) lst) " "))))
   (cond ((null? contents) #t)          ; Accepted
         ((and (null? seq-list) (not (null? contents)))
          (xml-add-problem! 
           (xml-enrich-error-message 
            (string-append "The " el-name " element instance does not have  "
                           (pp-with-stars orig-seq-list) "  as a suffix")  contents)))
         ((not (null? seq-list))
          (if (equal? (ast-element-name (first contents)) (car seq-list))
              (check-star-sequence-1! seq-list (cdr contents) el-name orig-seq-list)
              (check-star-sequence-1! (cdr seq-list) contents el-name orig-seq-list))))))
               
        
        
        