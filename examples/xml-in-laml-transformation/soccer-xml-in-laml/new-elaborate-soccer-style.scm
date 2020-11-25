; This is a Scheme program that transform a soccer XML-in-LAML document to
; a soccer tournament table. This Scheme program is similar to an XSLT program.

(lib-load "xml-in-laml/mirrors/xhtml10-transitional-mirror.scm")

(define text-contents aggregated-ast-cdata-contents) ; alias
(define (team-of-match match n) (text-contents (ast-subtree match 'team n))) 
(define (score-of-match match n) (as-number (get-prop 'score (ast-attributes (ast-subtree match 'team n)))))
(define map (curry-generalized map))

(define (won? team-name) (lambda (match) (match-compare team-name match >)))
(define (drawn? team-name) (lambda (match) (match-compare team-name match =)))
(define (lost? team-name) (lambda (match) (match-compare team-name match <)))

(define (match-compare team-name match rel?)
  (or (and (equal? team-name (team-of-match match 1)) (rel? (score-of-match match 1) (score-of-match match 2)))
      (and (equal? team-name (team-of-match match 2)) (rel? (score-of-match match 2) (score-of-match match 1)))))

(define count-this id-1)
(define (count-other n) (if (= n 1) 2 1))

(define (goals-for team-name) (lambda (match) (goals-of team-name match count-this)))
(define (goals-against team-name) (lambda (match) (goals-of team-name match count-other)))

(define (goals-of team-name match count-which)
   (cond ((equal? team-name (team-of-match match 1)) (score-of-match match (count-which 1)))
	 ((equal? team-name (team-of-match match 2)) (score-of-match match (count-which 2)))
	 (else 0)))

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
 (let* ((won (length (filter (won? team-name) match-ast-list)))
        (drawn (length (filter (drawn? team-name) match-ast-list)))
        (lost  (length (filter (lost? team-name)  match-ast-list)))
 
        (played (+ won drawn lost))

        (for   (sum-list (map (goals-for team-name) match-ast-list)))
        (against   (sum-list (map (goals-against team-name) match-ast-list)))
       )
    (list team-name played won drawn lost for against)))
  

  


