; This is a Scheme program that transform a soccer XML-in-LAML document to
; a soccer tournament table. This Scheme program is similar to an XSLT program.

(lib-load "xml-in-laml/mirrors/xhtml10-transitional-mirror.scm")

(define text-contents aggregated-ast-cdata-contents) ; alias
(define (team-of-match match n) (text-contents (sub-ast match 'team n))) 
(define (score-of-match match n) (as-number (get-prop 'score (ast-attributes (sub-ast match 'team n)))))
(define map (curry-generalized map))

(define (results! soccer-ast)
 (let* ((ttl (list "Result of group" (get-prop 'group (ast-attributes soccer-ast))))
        (team-list (remove-duplicates-by-predicate
                      (traverse-and-collect-all-from-ast soccer-ast (ast-of-type? 'element-name 'team) text-contents)
                      equal?))
        (match-list (traverse-and-collect-all-from-ast soccer-ast (ast-of-type? 'element-name 'match) id-1))
        
        (html-ast 
         (html 
          (head (title ttl))
          (body
            (h1 ttl)
            (table 'cellpadding "5"
             (tr (map td (list "Team" "Played" "Won" "Drawn" "Lost" "For" "Against")))
             (map (compose tr (map (compose td as-string))) (data-of-teams team-list match-list)))))

        )
       )
   (write-html '(pp) html-ast)))

(define (data-of-teams team-name-list match-ast-list)
  (map (lambda (team) (data-of-team team match-ast-list)) team-name-list))

(define (data-of-team team-name match-ast-list)
 (let ((played (length (filter (lambda (match) (or (equal? team-name (team-of-match match 1)) (equal? team-name (team-of-match match 2)))) match-ast-list)))
       (won (length (filter (lambda (match) (or (and (equal? team-name (team-of-match match 1)) (> (score-of-match match 1) (score-of-match match 2)))
                                                (and (equal? team-name (team-of-match match 2)) (> (score-of-match match 2) (score-of-match match 1))))) match-ast-list)))

       (drawn (length (filter (lambda (match) (or (and (equal? team-name (team-of-match match 1)) (= (score-of-match match 1) (score-of-match match 2)))
                                                  (and (equal? team-name (team-of-match match 2)) (= (score-of-match match 2) (score-of-match match 1))))) match-ast-list)))
       (lost  (length (filter (lambda (match) (or (and (equal? team-name (team-of-match match 1)) (< (score-of-match match 1) (score-of-match match 2)))
                                                  (and (equal? team-name (team-of-match match 2)) (< (score-of-match match 2) (score-of-match match 1))))) match-ast-list)))
       (for   (sum-list
               (map (lambda (match) (cond ((equal? team-name (team-of-match match 1)) (score-of-match match 1))
                                          ((equal? team-name (team-of-match match 2)) (score-of-match match 2))
                                          (else 0))) match-ast-list)))
       (against  (sum-list
                  (map (lambda (match) (cond ((equal? team-name (team-of-match match 1)) (score-of-match match 2))
                                             ((equal? team-name (team-of-match match 2)) (score-of-match match 1))
                                             (else 0))) match-ast-list)))
      )
   (list team-name played won drawn lost for against)))
  

  


