; This is a Scheme program that transform a soccer XML-in-LAML document to
; a simple overview of some soccer games. This Scheme program is similar to an XSLT program.

(lib-load 
 "xml-in-laml/mirrors/xhtml10-transitional-mirror.scm")

(define (results! soccer-ast)
 (let* ((ttl (list "Result of group"
                   (ast-attribute soccer-ast 'group))))
  (write-html '(pp) 
   (html (head (title ttl))
         (body (h1 ttl) (present-matches soccer-ast))))))

(define (present-matches soccer-ast)
 (find-asts soccer-ast "match"
  (lambda (match-ast)
   (let* ((team1 (ast-subtree match-ast 'team 1))
          (team1-score (ast-attribute team1 'score))
          (team2 (ast-subtree match-ast 'team 2))
          (team2-score (ast-attribute team2 'score))
          (dt (ast-subtree match-ast 'date)))
     (list 
      (h2 (ast-text team1) "versus"
          (ast-text team2))
      (p "Played on" (ast-text dt))
      (p "Result:"
         (ast-text team1) team1-score _ ","
         (ast-text team2) team2-score))))))