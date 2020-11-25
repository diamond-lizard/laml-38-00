;;;; This is a library of common and generally useful Scheme functions, which are used in the LAML libraries,
;;;; styles or tools.

; Reorganized June 11, 2001.  Old version in old/general.scm

; The LAML library and programs written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999  Kurt Normark, normark@cs.auc.dk.
; 
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


;;; List selection functions and their generators. 
;;; As an alternative to using car, cadr etc. we provide for generation of more general list selector functions.

;; Returns a function, which selects element number n in a list.
;; The second parameter, which is optional, is used for error message purposes.
;; In general, this parameter should be a string corresponding to the name of the selector function.
;; If the second parameter is given, we check whether the list is long enough for selection.
;; If not, we give a decent error message. We recommend use of the second parameter in order to
;; avoid meaningless error messages.
;; The first element is number 1.
;; (make-selector-function 1) corresponds to car, (make-selector-function 2) corresponds to cadr, etc.
(define (make-selector-function n . selector-name)
 (let ((selector-name-1 (if (null? selector-name) #f (car selector-name))))
   (if selector-name-1
       (lambda (lst) 
         (let ((lgt (length lst)))
            (if (> n lgt)
                (display-error (string-append "The selector function " (as-string selector-name-1) ": " 
                               "The list "  (as-string lst) " is is too short for selection. "
                               "It must have at least " (as-string n) " elements."
                               ))
                (list-ref lst (- n 1)))))
       (lambda (lst) (list-ref lst (- n 1))))))

;; Make and return a mutator function which mutates element number n in a list.
;; The returned function takes a list and a new value as arguments.
;; This function takes one optional parameter, which is the name of the mutator
;; This is used for error message purposes.
(define (make-mutator-function n . optional-parameter-list)
 (let ((mutator-name (optional-parameter 1 optional-parameter-list)))
   (if mutator-name
       (lambda (lst new-value) 
         (let ((lgt (length lst)))
            (if (> n lgt)
                (display-error (string-append "The mutator function " (as-string mutator-name) ": " 
                                 "The list "  (as-string lst) " is is too short for mutator. "
                                 "It must have at least " (as-string n) " elements."
                               ))
                (let ((cons-pair (list-tail lst (- n 1))))
                  (set-car! cons-pair new-value)))))
      (lambda (lst new-value) 
         (let ((lgt (length lst)))
            (if (> n lgt)
                (display-error (string-append "Error in mutator:"
                                 "The list "  (as-string lst) " is is too short for mutator. "
                                 "It must have at least " (as-string n) " elements."))
                (let ((cons-pair (list-tail lst (- n 1))))
                  (set-car! cons-pair new-value))))))))


;; Return the first element of a list
;; .form (first lst)
(define first car)

;; Return the second element of a list
;; .form (second lst)
(define second cadr)

;; Return the third element of a list
;; .form (third lst)
(define third caddr)

;; Return the fourth element of a list
;; .form (fourth lst)
(define fourth cadddr)

;; Return the fifth element of a list
;; .form (fifth lst)
;; .returns The fifth element of the list
(define fifth (make-selector-function 5))

;; Return the sixth element of a list
;; .form (sixth lst)
(define sixth (make-selector-function 6))

;; Return the seventh element of a list
;; .form (seventh lst)
(define seventh (make-selector-function 7))

;; Return the eighth element of a list
;; .form (eighth lst)
(define eighth (make-selector-function 8))

;; Return the nineth element of a list
;; .form (nineth lst)
(define nineth (make-selector-function 9))


;;; Association and property list functions. 
;;; Here follows a number of functions which work on alists, or make alists. Also a number of property list functions are provided.

;; A functions which converts the key position in an a-lists to a symbol.
;; .parameter key-value-pair a pair, such as ("key" . "val")
;; .returns a pair (key . "val")
(define (symbolize-key key-value-pair)
 (cons (as-symbol (car key-value-pair)) (cdr key-value-pair)))

;; Add a key-value pair to a-list. Like acons in some systems.
(define (extend-a-list key value a-list)
 (cons (cons (as-symbol key) value) a-list))

;; Return a value from an alist, or gives an error if key does not exist.
;; .parameter key is a symbol.
;; .parameter a-list an association list with symbols as kesy.
;; .returns the first value of key in a-list.
;; .misc Uses the function assq (based on eq? for key comparions) internally.
;; .internal-references "similar function" "defaulted-get"
(define (get key a-list)
  (let ((res (assq key a-list)))
    (if (pair? res) 
        (cdr res)
        (error (string-append "Cannot find " (as-string key) " in " (as-string a-list))))))

;; Return the value of key in alist (by means of cdr of assq). If no association is found return default.
;; .internal-references "similar function" "get"
(define (defaulted-get key alist default)
  (let ((res (assq key alist)))
    (if res
       (cdr res)
       default)))

;; Make an alist from a key-list and a val-list. 
;; .pre-condition the lengths of the two input lists are equal.
(define (alist-from-keys-and-values key-list val-list)
  (if (= (length key-list) (length val-list))
      (alist-from-keys-and-values-1 key-list val-list)
      (error "alist-from-keys-and-values: key and val list do not have same lengths")))

(define (alist-from-keys-and-values-1 key-list val-list)
  (if (null? key-list)
      '()
      (cons (cons (car key-list) (car val-list))
            (alist-from-keys-and-values-1 (cdr key-list) (cdr val-list)))))

;; Make and return an association list from a property list plist.
(define (propertylist-to-alist plist)
  (let ((lgt (length plist)))
   (cond ((null? plist) '())
         ((= 1 lgt) (error "property-list-to-a-list called with list of odd length. A property list is always of even length"))
         ((>= lgt 2) (cons (cons (car plist) (cadr plist)) (propertylist-to-alist (cddr plist)))))))

;; Make and return a property list from an association list.
(define (alist-to-propertylist alist)
  (cond ((null? alist) '())
        (else (cons (car (car alist)) (cons (cdr (car alist)) (alist-to-propertylist (cdr alist)))))))

;; Return every second element of list, starting with the first element.
;; This function is useful to extract the keys or values of a property list.
(define (every-second-element lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) (list (car lst)))
        (else (cons (car lst) (every-second-element (cddr lst))))))

;;; Filter and accumulation functions. 
;;; This sections provides variants of the very useful higher order filtering function.

;; Filter a list lst by means of the predicate pred. Preserves the ordering of elements in lst.
;; .returns the elements in lst that fulfills the predicate pred.
;; .misc Based on a tail recursive traversal of lst.
;; .internal-references "similar function" "filter-no-ordering"
(define (filter pred lst)
  (reverse (filter-help pred lst '())))

;; Like filter, but the ordering among elements in the resulting list is unknown and arbitrary.
;; Actually returns filtered list in reverse order. OK in situations,
;; where a boolean result is needed: Are there anything passing the filter?
;; .internal-references "similar function" "filter"
(define (filter-no-ordering pred lst)
  (filter-help pred lst '()))

(define (filter-help pred lst res)
  (cond ((null? lst) res)
        ((pred (car lst)) (filter-help pred (cdr lst) (cons (car lst) res)))
        (else (filter-help pred (cdr lst) res))))

;; Map and filter a list lst by means of the predicate pred.
;; If the predicate pred returns a true value v on the element e in list, return
;; v instead of e (this is the mapping effect). 
;; Only return those mapped elements that fullfil pred.
;; .misc Remember that any non-#f element counts as the true (#t) value.
(define (mapping-filter pred lst)
  (reverse (mapping-filter-help pred lst '())))

(define (mapping-filter-help pred lst res)
  (if (null? lst)
      res
      (let ((pred-appl (pred (car lst))))
        (if pred-appl 
            (mapping-filter-help pred (cdr lst) (cons pred-appl res))
            (mapping-filter-help pred (cdr lst) res)))))

;; A higher order functions which right accumulates a list.
(define (accumulate-right f init lst)
  (if (null? lst)
        init
        (f (car lst) (accumulate-right f init (cdr lst)))))

;;; Mapping functions.
;;; Here is a set of generalized mapping functions. These functions are all similar to map (which may take an arbitrary number of lists).
;;; Notice however, that map2, map3, etc does not require all lists to be of equal lengths.

;; Like map, but maps f on two lists. 
;; .returns Returns a list of length equal to the length of the shortest of the input lists.
(define (map2 f lst1 lst2)
  (if (or (null? lst1) (null? lst2)) '()
      (cons (f (car lst1) (car lst2)) 
            (map2 f (cdr lst1) (cdr lst2)))))

;; Like map, but maps f on three lists
;; .returns Returns a list of length equal to the length of the shortest of the input lists.
(define (map3 f lst1 lst2 lst3)
  (if (or (null? lst1) (null? lst2) (null? lst3)) '()
      (cons (f (car lst1) (car lst2) (car lst3)) 
            (map3 f (cdr lst1) (cdr lst2) (cdr lst3)))))

;; Like map, but maps f on four lists
;; .returns Returns a list of length equal to the length of the shortest of the input lists.
(define (map4 f lst1 lst2 lst3 lst4)
  (if (or (null? lst1) (null? lst2) (null? lst3)  (null? lst4)) '()
      (cons (f (car lst1) (car lst2) (car lst3) (car lst4)) 
            (map4 f (cdr lst1) (cdr lst2) (cdr lst3) (cdr lst4)))))

;; Like map, but maps f on five lists
;; .returns Returns a list of length equal to the length of the shortest of the input lists.
(define (map5 f lst1 lst2 lst3 lst4 lst5)
  (if (or (null? lst1) (null? lst2) (null? lst3)  (null? lst4) (null? lst5)) '()
      (cons (f (car lst1) (car lst2) (car lst3) (car lst4) (car lst5) ) 
            (map5 f (cdr lst1) (cdr lst2) (cdr lst3) (cdr lst4) (cdr lst5)))))



;;; Other higher-order functions.

;; A higher order functions which negates the predicate p. Negate accepts a predicate and returns the negated predicate.
(define (negate p)
  (lambda (x) 
    (if (p x) #f #t)))

;; Return a composed function which applies f on g
;; Both f and g are supposed to take a single argument.
(define (compose f g)
  (lambda (x)
    (f (g x))))

;; Generate a less than or equal predicate from the enumeration-order.
;; If p is the generated predicate, (p x y) is true if and only if
;; (selector x) comes before (or at the same position) as (selector y)
;; in the enumeration-order. Thus, (selector x) is assumed to give a
;; value in enumeration-order. Comparison with elements in the enumeration-list
;; is done with eq?
(define (generate-leq enumeration-order selector)
  (lambda (x y)
     ; x and y supposed to be elements in enumeration order
     (let ((x-index (list-index (selector x) enumeration-order))
           (y-index (list-index (selector y) enumeration-order)))
       (<= x-index y-index))))

; A helping function of generate-leq.
; Return the position of e in lst. First is 1
; compare with eq?
; if e is not member of lst return (+ 1 (length lst))
(define (list-index e lst)
 (cond ((null? lst) 1)
       ((eq? (car lst) e) 1)
       (else (+ 1 (list-index e (cdr lst))))))

;; Generalize f with ad hoc currying. 
;; f is a function which, in its native form, takes two or more parameters.
;; The generalization allows f to act as a curried function. In case (curry-generalized f)
;; only receives a single parameter, it returns a lambda function which waits for the 
;; remaining parameters.
;; .example (define gmap (curry-generalized map))
;; .example (define gfilter (curry-generalized filter))
(define (curry-generalized f)
  (lambda rest
    (cond ((= (length rest) 1) (lambda lst (apply f (cons (car rest) lst))))
          ((>= (length rest) 2) (apply f (cons (car rest) (cdr rest)))))))

;;; List and Sexpr functions.

;; Return a list of all numbers from f to t. Return the empty list if f is greater than t.
(define (number-interval f t)
 (if (<= f t) (cons f (number-interval (+ f 1) t)) '()))


;; Return the proper part of an S-expression
(define (proper-part lst)
  (cond ((and (pair? lst) (pair? (cdr lst)))  (cons (car lst) (proper-part (cdr lst))))
        ((pair? lst) (cons (car lst) '()))
        (else '())))

;; Return the first improper part of an S-expression
(define (first-improper-part lst)
  (cond ((and (pair? lst) (pair? (cdr lst)))  (first-improper-part (cdr lst)))
        ((pair? lst) (cdr lst))
        (else (error (string-append "Troubles in first-improper-part:" (as-string lst))))))



;; Return a list of n elements, each being el
(define (make-list n el)
  (if (<= n 0) '() (cons el (make-list (- n 1) el))))

;; Replicate lst cyclically to a list of length lgt
(define (replicate-to-length lst lgt)
  (reverse (replicate-to-length-1 lst lst '() 0 lgt)))

; helping function to replicate-to-length
; original-lst is constant through this function.
; elements are taken out of lst
; the result is accumulated up in res
; count goes from 0 to lgt
(define (replicate-to-length-1 original-lst lst res count lgt)
 (cond ((null? lst) (replicate-to-length-1 original-lst original-lst res count lgt))
       ((< count lgt) (replicate-to-length-1 original-lst (cdr lst) (cons (car lst) res) (+ 1 count) lgt))
       (else res)))


;; Flatten a list of lists to one list.
(define (flatten lst-of-lst)
  (accumulate-right append '() lst-of-lst))

;; Add all elments in a list of numbers
(define (sum-list lst)
  (accumulate-right + 0 lst))



;; Return the first n elements of lst. 
;; Just returns lst if n is equal or greater than the length of lst.
;; In other cases, a new 'copy' is returned.
(define (front-sublist lst n)
 (if (>= n (length lst)) lst (front-sublist-1 lst n)))

; A helping operation to front-sublist
(define (front-sublist-1 lst n)
 (cond ((= n 0) '())
       ((and (> n 0) (not (null? lst))) (cons (car lst) (front-sublist-1 (cdr lst) (- n 1))))
       ((and (> n 0) (null? lst)) '())
       (else (error "front-sublist-1: Should not happen"))))

;; Merge list1 and list2. Let e1 be the head of list1 and e2 the head of list2.
;; take e2 if (pred e1 e2) holds. Else e1
(define (merge-lists list1 list2 pred)
 (cond ((null? list1) list2)
       ((null? list2) list1)
       ((pred (car list1) (car list2)) (cons (car list2) (merge-lists list1 (cdr list2) pred)))
       (else (cons (car list1) (merge-lists (cdr list1) list2 pred)))))

;; A very simple and basic list search function.
;; Return first element which matches lst.
;; If no element is found, return #f.
;; Tail recursive and iterative
(define (find-in-list pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) (car lst))
        (else (find-in-list pred (cdr lst)))))

;; Return all but the last element of a list. Quick and dirty version.
(define (butlast lst)
  (reverse (cdr (reverse lst))))

;; Return the last element of a list. Quick and dirty version.
(define (last lst)
  (car (reverse lst)))

;; Duplicate removal - non-destructive.
;; This function uses equal for comparison of elements.
(define (remove-duplicates lst)
 (remove-duplicates-help lst '()))

(define (remove-duplicates-help lst res)
  (cond ((null? lst) (reverse res))
        ((member (car lst) res) (remove-duplicates-help (cdr lst) res))
        (else (remove-duplicates-help (cdr lst) (cons (car lst) res)))))

;; A variant of remove-duplicates with a selector function.
;; This function applies a selector function before comparisons and member is called.
;; This function uses equal for comparison of elements.
(define (remove-duplicates-with-selection lst selector)
 (remove-duplicates-with-selection-help lst '() '() selector))

(define (remove-duplicates-with-selection-help lst res selected-res selector)
  (cond ((null? lst) (reverse res))
        ((member (selector (car lst)) selected-res)
            (remove-duplicates-with-selection-help (cdr lst) res selected-res selector))
        (else (remove-duplicates-with-selection-help (cdr lst) (cons (car lst) res)  (cons (selector (car lst)) selected-res) selector ))))

;; Return the element of lst just before el, or #f if no such element exists.
;; Comparsion is done via application of selector on the elements of lst, and via eq?.
;; More precise return the element e of lst just before f, where (eq? (selector f) el).
(define (element-before el lst selector)
  (element-before-1 el lst selector (length lst)))

(define (element-before-1 el lst selector lgt)
  (cond ((<= lgt 1) #f)
        ((eq? el (selector (car lst))) #f)
        ((eq? el (selector (cadr lst))) (car lst))
        (else (element-before-1 el (cdr lst) selector (- lgt 1)))))

;; Return the element of lst just after el, or #f if no such element exists.
;; Comparsion is done via application of selector on the elements of lst, and via eq?.
;; More precise return the element e of lst just after f, where (eq? (selector f) el).
(define (element-after el lst selector)
  (element-after-1 el lst selector (length lst)))

(define (element-after-1 el lst selector lgt)
  (cond ((<= lgt 1) #f)
        ((eq? el (selector (car lst))) (cadr lst))
        (else (element-after-1 el (cdr lst) selector (- lgt 1)))))


;; Remove the elements of lst2 from lst1. 
;; A non-destructive function.
;; Uses eq? for comparison
(define (list-difference lst1 lst2)
  (list-difference-1 lst1 lst2 '()))

(define (list-difference-1 lst1 lst2 res)
  (cond ((null? lst1) (reverse res))
        ((memq (car lst1) lst2) (list-difference-1 (cdr lst1) lst2 res))
        (else (list-difference-1 (cdr lst1) lst2 (cons (car lst1) res)))))



;; Return a list of pairs of elements from lst1 and lst2.
;; In other words, return an association list with keys from lst1 and values from lst2.
;; The list is as long as the shortest of lst1 and lst2.
(define (pair-up lst1 lst2)
  (pair-up-1 lst1 lst2 '()))

(define (pair-up-1 lst1 lst2 res)
  (cond ((or (null? lst1) (null? lst2)) (reverse res))
        (else (pair-up-1 (cdr lst1) (cdr lst2) (cons (cons (car lst1) (car lst2)) res)))))

;; Return a list of lists of elements from lst.
;; Each sub list is of length n.
;; Take elements consequtive (by rows) and put them into sublists.
;; .internal-references "More general function" "sublist-by-predicate"
(define (sublist-by-rows n lst)
 (let ((lgt (length lst)))
  (cond ((<= n 0) (error  (string-append "sublist-by-rows: Cannot deal with row numbers less than or equal to zero: " (as-string n))))
        ((< lgt n) (error (string-append "sublist-by-rows: Cannot deal with list of fewer elements than the sublist length:" (as-string lgt))))
        (else (sublist-by-rows-1 n lst 0 '() '())))))

(define (sublist-by-rows-1 n lst m res RESULT)
  (cond ((and (null? lst) (null? res)) (reverse RESULT))  ;@a
        ((and (null? lst) (not (null? res))) (reverse (cons (reverse res) RESULT)))    ;@b
        ((= m n ) (sublist-by-rows-1 n lst 0 '() (cons (reverse res) RESULT)))         ;@c
        ((<= m n) (sublist-by-rows-1 n (cdr lst) (+ m 1) (cons (car lst) res) RESULT)) ;@d
        (else (error "sublist-by-rows-1: Should not happen"))))                       

;; Return sublists of lst in two column format. Thus each produced sublist is of length 2.
;; Good for presentation of the list in two columns, column by column.
;; In cases there is an uneven number of elements in lst, we add extra (the second parameter).
(define (sublist-by-2columns lst extra)
 (if (null? lst)
     '()
  (let* ((lgt (length lst))
         (lst1 (if (even? lgt) lst (append lst (list extra))))
         (row-sublst (sublist-by-rows (quotient (if (even? lgt) lgt (+ 1 lgt)) 2) lst1)) ; @i
        )
    (map ; @j
      (lambda (e1 e2) (list e1 e2))
      (car row-sublst) (cadr row-sublst)))))

;; Return sublists of lst in an n column format. Thus each produced sublist is of length n
;; (the first parameter).
;; In cases there is not enough elements, we add extra (the last parameter).
(define (sublist-by-columns n lst extra)
 (if (null? lst)
     '()
  (let* ((lgt (length lst))
         (q (quotient lgt n))
         (lst1 (if (multiplum-of lgt n) lst (append lst (make-list (- (* (+ q 1) n) lgt) extra)))) ; @a
         (rows (if (multiplum-of lgt n) q (+ q 1)))
         (row-sublst (sublist-by-rows rows lst1)))
    (multi-pair row-sublst))))

;; Pair up first elements, second elements of a list of lists.
;; All first elements of the sublists are handled first, whereafter
;; we take all second elements, etc. 
;; .parameter lst-of-lst A list of lists.
;; .pre-condition All lists in lst-of-list are of equal lengths.
(define (multi-pair lst-of-lst)
  (cond ((null? (car lst-of-lst)) '())
        (else (let ((cars (map car lst-of-lst))
                    (cdrs (map cdr lst-of-lst)))
                (cons cars (multi-pair cdrs))))))

;; Return a list of sublists of elements from lst controlled by an element predicate p.
;; The sublists are formed consequtively by taking elements from lst.  The predicate p decides
;; when to start a new sublist. Thus, when p evaluates to true, we start
;; a new sublist. The predicate p takes as parameters the current
;; elements, the previous element, and the number of elements before the
;; current one, p is not activated on (car lst). 
;; This function generalizes sublist-by-rows.
;; p: (cur prev n) -> boolean
(define (sublist-by-predicate lst p)
  (cond ((null? lst) '()) ;@a
        ((= 1 (length lst)) (list lst))  ; @b special case: sublist the only element.
        (else (sublist-by-predicate-1 (cdr lst) (car lst) p 1 (list (car lst)) '()))))


(define (sublist-by-predicate-1 lst previous-el p n res RESULT)
  (cond ((and (null? lst) (null? res)) (reverse RESULT)) ;@d
        ((and (null? lst) (not (null? res))) (reverse (cons (reverse res) RESULT))) ;@e
        ((p (car lst) previous-el n)   (sublist-by-predicate-1 (cdr lst) (car lst) p (+ n 1) (list (car lst)) (cons (reverse res) RESULT))) ;@f
        (else                          (sublist-by-predicate-1 (cdr lst) (car lst) p (+ n 1) (cons (car lst) res) RESULT)))) ;@g

;; Remove duplicates from lst.
;; A duplicates satisfy the comparing predicate p.
;; (p element element) -> boolean.
(define (remove-duplicates-by-predicate lst p)
 (remove-duplicates-by-predicate-1 lst p '()))

(define (remove-duplicates-by-predicate-1 lst p res)
  (cond ((null? lst) (reverse res))
        ((member-by-predicate (car lst) res p) (remove-duplicates-by-predicate-1 (cdr lst) p res))
        (else (remove-duplicates-by-predicate-1 (cdr lst) p (cons (car lst) res)))))

;; Return the duplicates in lst. 
;; The duplicates are returned in the order of their fist occurence in lst.
;; Comparison of elements is done by the predicate (p element element) -> boolean.
(define (duplicates-by-predicate lst p)
  (duplicates-by-predicate-1 lst p '()))

(define (duplicates-by-predicate-1 lst p res)
  (cond ((null? lst) (reverse res))
        ((member-by-predicate (car lst) (cdr lst) p) (duplicates-by-predicate-1 (cdr lst) p (cons (car lst) res)))
        (else (duplicates-by-predicate-1 (cdr lst) p res))))  

;; Is el member of lst by means of the predicate p.
;; el is always passed as the first parameter to p.
;; If el is member, return the suffix of the list in which the first element (and el) satisfy the predicate.
;; Else return #f.
;; The element el and elements of lst are compared by p, el as the first one.
;; p: (el1, el2) -> boolean
(define (member-by-predicate el lst p)
  (cond ((null? lst) #f)
        ((p el (car lst)) lst)
        (else (member-by-predicate el (cdr lst) p))))


;; Cut the tail of lst; The tail to be cutted starts with an element which fulfils pred.
;; Notice that the first element which fulfils the predicate is not included in the resulting list.
(define (cut-list-by-predicate lst pred)
  (cond ((null? lst) '())
        ((pred (car lst)) '())
        (else (cons (car lst) (cut-list-by-predicate (cdr lst) pred)))))


;; Return whether every element in set-list-1 (a list) is a member of set-list-2, compared by the comparator comp.
;; This corresponds to a subset operations on sets, represented by a list.
;; comp: el x el -> boolean.
(define (subset-of-by-predicate set-list-1 set-list-2 comp)
  (cond ((null? set-list-1) #t)
        ((member-by-predicate (car set-list-1) set-list-2 comp) (subset-of-by-predicate (cdr set-list-1) set-list-2 comp))
        (else #f)))

;; Return the index of the first occurrence of el in lst. 
;; Return #f is el is not found in lst.
;; Comparison is done by comparator.
;; The index of the first element is 0.
(define (index-in-list-by-predicate lst el comparator)
  (index-in-list-by-predicate-1 lst el comparator 0))

(define (index-in-list-by-predicate-1 lst el comparator i)
  (cond ((null? lst) #f)
        ((comparator el (car lst)) i)
        (else (index-in-list-by-predicate-1 (cdr lst) el comparator (+ i 1)))))




;; Divide the elements of lst into sublists of sublist-length.
;; In case that sublist-length does not divide (length lst) the last
;; sublist will be shorter than the others.
;; .example (sublistify '(1 2 3 4 5 6 7 8 9) 4) = ((1 2 3 4) (5 6 7 8) (9))
(define (sublistify lst sublist-length)
 (if (<= (length lst) sublist-length)
     (list lst)
     (let ((first-sublist (list-prefix lst sublist-length))
           (rest-lst (list-tail lst sublist-length)))
       (cons first-sublist
             (sublistify rest-lst sublist-length)))))

;; Return the list of the first n elements of lst.
;; If n < (length lst) just return lst.
(define (list-prefix lst n)
  (if (< (length lst) n)
      lst
      (list-prefix-1 lst n)))

(define (list-prefix-1 lst n)
  (if (= n 0)
      '()
      (cons (car lst) (list-prefix-1 (cdr lst) (- n 1)))))


;; Return the sublist consisting of element a to element b of the list lst.
;; Both element number a and b are included in the resulting list. The first element counts as element number 1.
;; .example (list-part 3 5 '(a b c d e f g h)) = (c d e)
;; .pre-condition a >= 1, a <= b, b <= (length lst), and a and b are postive integers.
(define (list-part a b lst)
  (list-part-help a b lst 1 (length lst) '()))

(define (list-part-help a b lst  i lgt res)
 (cond ((> i b) (reverse res))
       ((and (>= i a) (<= i b) (not (null? lst))) (list-part-help a b (cdr lst) (+ i 1) lgt (cons (car lst) res)))
       ((and (<= i a) (not (null? lst)))  (list-part-help a b (cdr lst) (+ i 1) lgt res))
       ((null? lst) (error (string-append "list-part error: " (as-string i))))))




;;; Conversion functions. 
;;; In this category we provide a number of useful conversion functions.

;; Convert a character to a string
(define (char->string ch)
  (make-string 1 ch))


;; Convert x to a string. Conversion of numbers, symbols, strings, booleans, characters, proper list and improper lists are supported.
(define (as-string x)
  (cond ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        ((string? x) x)
        ((boolean? x) 
            (if x "true" "false"))   ; consider "#t" and "#f" as alternatives
        ((char? x) (char->string x))
        ((list? x)
            (string-append "(" 
               (string-merge (map as-string x) (make-list (- (length x) 1) " "))
               ")"))
        ((pair? x)
            (string-append "(" 
               (apply string-append
                  (map (lambda (y) (string-append (as-string y) " ")) (proper-part x))
               )
               " . " (as-string (first-improper-part x))
               ")"))
        (else "??")))

;; Convert x to a string, in which string constituents themselves are quoted.
;; Good for output and messages, in which strings should appear in string quotes.
(define (as-quoted-string x)
  (cond ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        ((string? x) (string-it x))
        ((boolean? x) 
            (if x "true" "false"))   ; consider "#t" and "#f" as alternatives
        ((char? x) (char->string x))
        ((list? x)
            (string-append "(" 
               (string-merge (map as-quoted-string x) (make-list (- (length x) 1) " "))
               ")"))
        ((pair? x)
            (string-append "(" 
               (apply string-append
                  (map (lambda (y) (string-append (as-quoted-string y) " ")) (proper-part x))
               )
               " . " (as-quoted-string (first-improper-part x))
               ")"))
        (else "??")))

;; Convert x to a symbol. String, symbols, booleans, and characters are supported
(define (as-symbol x)
  (cond ((symbol? x) x)
        ((string? x) (string->symbol x))
        ((boolean? x) 
            (if x (as-symbol "true") (as-symbol "false")))
        ((char? x) (as-symbol (char->string x)))
        (else #f)))

;; Convert x to a number. Strings, numbers, chars and booleans are supported.
;; Strings with digits are converted using string->number, chars are converted with char->integer, true is converted to 1, and false to 0.
(define (as-number x)
  (cond ((string? x) (string->number x))
        ((number? x) x)
        ((char? x) (char->integer x))
        ((boolean? x) (if x 1 0))  ; false -> 0, true -> 1
        (else (error (string-append "Cannot convert to number " (as-string x))))
  ))

;; Convert x to a character. Integers, strings, booleans and symbols are supported.
;; If x is an integer between 0 and 255 return ASCII char number x. If x is a string return the first character in the string (which is supposed to be non-empty).
;; If x is a boolean return the character #\t for true and #\f for false. If x is a symbol return the first character of the print name of the string. Else return #\?.
(define (as-char x)
  (cond ((char? x) x)
        ((integer? x) 
           (if (and (>= x 0) (<= x 255)) 
               (integer->char x)
               #\?))
        ((string? x) (string-ref x 0))
        ((boolean? x) (if x #\t #\f))
        ((symbol? x)  (as-char (as-string x)))
        (else #\?)))

;; Convert x to a list. 
;; This function converts strings to a list of substring, which in the original string are separated by spaces, newlines, or tabs.
;; .internal-references "more general function" "string-to-list"
;; .example (as-list "xy z abc ") => ("xy" "z" "abc")
(define (as-list x)
  (cond ((string? x) (string-to-list x (list #\space (as-char 13) (as-char 10) #\tab)))
        ((list? x) x)
        ((pair? x) x)
        (else (list x))))

;; Convert a string to a list.
;; The second parameter is a list of separator characters.
(define (string-to-list str element-separator-chars)
 (filter (negate empty-string?)
  (string-to-list-help str "" '() element-separator-chars (string-length str))))

(define (string-to-list-help str next-el res-list element-separator-chars str-lgt)
 (if (= 0 str-lgt)
     (reverse (cons next-el res-list))     ; add last 'rest element: next-el
     (let ((next-char (string-ref str 0))
	   (rest-string (substring str 1 str-lgt)))
       (cond 
         ((memq next-char element-separator-chars) (string-to-list-help rest-string "" (cons next-el res-list) element-separator-chars (- str-lgt 1)))
         (else (string-to-list-help rest-string (string-append next-el (as-string next-char)) res-list element-separator-chars (- str-lgt 1)))))))

;; Convert x to a boolean. The strings "false", "no", and "NO" are converted to #f. Other strings are converted to #t.
(define (as-boolean x)
  (cond ((string? x) (if (or (equal? x "false") (equal? x "no") (equal? x "NO")) #f #t))
        ((boolean? x) x)
        (else (error "Cannot convert to boolean"))))


;; Return a string with the elements of str-lst separated by separator.
;; .parameter str-list A list of strings
;; .parameter separator A string which is used to separate the list elements in the resulting string.
(define (list-to-string str-lst separator)
  (string-merge 
     str-lst
     (make-list (- (length str-lst) 1) separator)))

;; If x is considered true return #t else #f. 
;; See also as-boolean which is more versatile.
;; Recall that all values except #f, conveniently, act as a true value.
(define (turn-into-boolean x)
  (if x #t #f))


;;; String predicates.

;; Is the string str empty
(define (empty-string? str)
  (= (string-length str) 0))

;; A list of characters considered as blank space characters
(define white-space-char-list (list #\space (as-char 13) (as-char 10) #\tab))

;; Is the string str empty or blank (consists of white space)
(define (blank-string? str)
  (or (empty-string? str) 
      (string-of-char-list? str white-space-char-list)))

;; Is the string str purely numeric?
;; More specifically, does it consist exclusively of the ciffers 0 through 9.
(define (numeric-string? str)
 (string-of-char-list? str (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 )))

;; Are all characters in str member of char-list (a list of characters).
(define (string-of-char-list? str char-list)
  (string-of-char-list-1? str char-list 0 (string-length str)))

(define (string-of-char-list-1? str char-list i lgt)
  (if (= i lgt)
      #t
      (and (memq (string-ref str i) char-list)
           (string-of-char-list-1? str char-list (+ i 1) lgt))))


;; Are all characters in str different from the characters in char list (a list of characters).
(define (string-of-negative-char-list? str char-list)
  (string-of-negative-char-list-1? str char-list 0 (string-length str)))

(define (string-of-negative-char-list-1? str char-list i lgt)
  (if (= i lgt)
      #t
      (and (not (memq (string-ref str i) char-list))
           (string-of-negative-char-list-1? str char-list (+ i 1) lgt))))



;;; Other string functions.
;;; Among the functions in this section you will find string search and replacement functions. 


;; Return a list of two strings taken from str. ch is a character.
;; The first is the prefix of str up to the first occurence of ch
;; The second is the suffix from ch to the end of str
(define (split-on ch str)
 (let ((sp (split-point ch str)))
   (list (substring str 0 sp)

         (substring str (+ sp 1) (string-length str)))))

;; Return the character position where ch occurs the first time in str.
;; If it does not appear, the procedure returns #f.
;; This function allocates some temporary strings, and as such it is not efficient. 
;; Use find-in-string instead.
;; .internal-references "similar string find function" "substring-index"


(define (split-point ch str)
 (call-with-current-continuation
  (lambda (exit)
   (cond ((equal? str "") #f)
         ((eq? ch (string-ref str 0)) 0)
         (else (let ((res (split-point ch (substring str 1 (string-length str)))))
                  (if (not res)
                      (exit #f)
                      (+ 1 res))))))))


;; Search linearly for the character ch in the string str. 
;; An optional start postion start-post tells at which position to start the search (default is position 0).
;; Return the index of the first occurence of ch, or #f if it does not exist in str.
;; The index of the first character in a string is 0.
(define (find-in-string str ch . start-pos)
 (let ((start-pos-1 (if (null? start-pos) 0 (car start-pos))))
  (find-in-string-1 str ch start-pos-1 (string-length str))))

(define (find-in-string-1 str ch i lgt)
  (cond ((>= i lgt) #f)
        ((eq? ch (string-ref str i)) i)
        (else (find-in-string-1 str ch (+ i 1) lgt))))



;; Search linearly for the character ch in the string str, beginning from the rear end of str.
;; Return the index of the last occurence of ch, or #f if it does not exist in str.
;; The index of the first character in a string is 0.
(define (find-in-string-from-end str ch)
 (let ((lgt (string-length str)))
  (find-in-string-from-end-1 str ch (- lgt 1) lgt)))

(define (find-in-string-from-end-1 str ch i lgt)
  (cond ((< i 0) #f)
        ((eq? ch (string-ref str i)) i)
        (else (find-in-string-from-end-1 str ch (- i 1) lgt))))


;; Does str contain sub-str as substring, starting at position pos?
;; An efficient implementation without any string copying, only character comparsion.
(define (looking-at-substring? str pos sub-str)
  (looking-at-substring-1? str pos sub-str 0 (string-length str) (string-length sub-str)))

(define (looking-at-substring-1? str pos sub-str i lgt1 lgt2)
 (let ((a (+ i pos)))
  (cond ((= i lgt2) #t)
        ((and (< a lgt1) (< i lgt2) (eq? (string-ref str a) (string-ref sub-str i)))
          (looking-at-substring-1? str pos sub-str (+ i 1) lgt1 lgt2))
        (else #f))))

;; Starting from char-pos, skip characters in string from char-list.
;; Return the first index higher or equal to start-pos, which contains
;; a character which is NOT in char-list. If start-pos is higher than
;; the maximum legal string index, return start-post.
(define (skip-chars-in-string str char-list start-pos)
  (skip-chars-in-string-1 str char-list start-pos (string-length str)))

(define (skip-chars-in-string-1 str char-list i lgt)
  (cond ((and (< i lgt) (memq (string-ref str i) char-list))
          (skip-chars-in-string-1 str char-list (+ i 1) lgt))
        ((and (< i lgt) (not (memq (string-ref str i) char-list)))
          i)
        (else i)))

;; Merge str-list-1 with str-list-2, returning one string.
;; Strings from the first list are merged with the strings from the second list.
;; In case one list is shorter than the other, the strings from the longests lists
;; are concatenated and appended
;; .example (string-merge (list "aa" "bb" "cc") (list "XX" "YY")) => "aaXXbbYYcc"
(define (string-merge str-list-1 str-list-2)
 (cond ((null? str-list-1) (apply string-append str-list-2))
       ((null? str-list-2) (apply string-append str-list-1))
       (else (string-append
                 (car str-list-1) (car str-list-2)
                 (string-merge (cdr str-list-1) (cdr str-list-2))))))

;; In in-string, substitute each occurence of character ch with the string str
(define (transliterate in-string ch str)
 (transliterate-1 in-string 0 (string-length in-string) 
                  (make-string (* (string-length in-string) (string-length str)) #\space) 0
                  ch str))

(define (transliterate-1 in-string n in-length
         out-string m 
         ch str)
  ; n is the position in the input
  ; m is the positin in the output
  (cond ((= n in-length) (substring out-string 0 m))
        ((< n in-length)
           (let ((in-char (string-ref in-string n)))
              (if (eq? in-char ch)
                  (begin (copy-string-into! out-string m str)
                         (transliterate-1 in-string (+ n 1) in-length out-string (+ m (string-length str)) ch str))
                  (begin (copy-string-into! out-string m (as-string in-char)) 
                         (transliterate-1 in-string (+ n 1) in-length out-string (+ m 1) ch str)))))
        (else (error "transliterate error")) ))


;; Delete the substring of length lgt from index i in the string str.
;; A non-destructive function which returns the result (a shorter string than the input).
;; i is supposed to be a valid index in str. If lgt is too long for str, we just delete to the end of str.
;; The first character is number 0.
(define (delete-string-portion str i lgt)
  (let* ((str-lgt (string-length str))
         (prefix (substring str 0 (max i 0)))
         (suffix (substring str (min (+ i lgt) str-lgt) str-lgt)))
    (string-append prefix suffix)))

; -----------------------------------------------------------------------------



;; In str1 replace all occurences of str2 with str3 and return the resulting string.
;; str2 is not allowed to be empty.
;; A non-destructive function which leaves all input strings unaffected.
(define (replace-string str1 str2 str3)
  (if (not (empty-string? str2))
      (replace-string-1 0 str1 str2 str3)
      (error (string-append "replace-string: Cannot replace empty string in " str1))))

; A helping function of replace-string which replaces from a given index i.
(define (replace-string-1 i str1 str2 str3)
  (let ((match-index (substring-index str1 i str2)))
    (if match-index 
        (replace-string-1
          (+ match-index (string-length str3))
          (put-into-string (delete-string-portion str1 match-index (string-length str2))  match-index str3)
          str2
          str3)
        str1)))


  

;; Put pre-putin at pre-index, and post-putit at post-index in the string str.
;; Return the result. Str is not affected.
;; .pre-condition pre-index is less than post-index.
(define (put-around-substring str pre-index pre-putin post-index post-putin)
 (put-into-string
    (put-into-string str post-index post-putin)
                         pre-index pre-putin))

;; Before the character with index put in putin-str into str, and return the resulting,
;; extended string. I.e, make room in the resulting string for putin-str, and slide a suffix of str 
;; to the right. Str is left unchanged. The first character is number 0.
(define (put-into-string str index putin-str)
 (let ((res (make-string (+ (string-length str) (string-length putin-str)))))
   (copy-string-into! res 0 (substring str 0 index))
   (copy-string-into! res index putin-str)
   (copy-string-into! res (+ index (string-length putin-str)) (substring str index (string-length str)))
   res))

;; Embed substring, as found in string, into embed-function.
;; A non-destructive function.
;; .parameter embed-function a function of one parameter, such as em, b.
;; .returns str with the first occurence of substring embedded into an activation of embed-function.
;; .example (embed-substring "LAML" "LAML is programmed in Scheme" em)
(define (embed-substring substring str embed-function)
  (let* ((i (substring-index str 0 substring)))
    (if i
        (let* ((pruned-str (delete-string-portion str i (string-length substring)))
	       (new-str (put-into-string pruned-str i (embed-function substring))))
          new-str)
        str)))

;; Copy source into target and overwrite a portion of target. 
;; Both target and source are strings, and i is an integer index.
;; The first char of source becomes chararter number i in the target string.
;; The first character in a string is number 0.
;; Target is mutated by this procedure.
;; If there is not enough room for source in target, only part of the source is copied into a suffix of target.
(define (copy-string-into! target i source)
  (copy-string-into-help! target i (string-length target) source 0 (string-length source)))

(define (copy-string-into-help! target i target-length source j source-length)
  ; A helping operation, doing the real work, of copy-string-into!
  (cond ((= i target-length) target)
        ((= j source-length) target)
        ((< j source-length) 
              (begin (string-set! target i (string-ref source j))
                     (copy-string-into-help! target (+ 1 i) target-length source (+ 1 j) source-length)))))

;; Return the index of the first occurence of find-str in str.
;; The search starts at str-index.
;; The first character in str has index 0.
;; If find-str is not a substring of str, starting the search at str-index, #f is returned.
(define (substring-index str str-index find-str)
 (let ((str-length (string-length str))
       (find-str-length (string-length find-str)))
  (cond ((= 0 (string-length find-str)) str-index)  ; new 10.9.98
        ((> str-index (- str-length find-str-length)) #f)
        ((substring-index-help str str-index str-length find-str 0 find-str-length) str-index)
        (else (substring-index str (+ 1 str-index) find-str)))))

(define (substring-index-help str str-index str-length find-str find-str-index find-str-length)
  ; return whether find-str matches at postion str-index at str.
  ; I.e., at boolean results from this function
  ; str-length is the length of str.
  ; find-str-length is the length of the remaining part of find-str to match.
  ; find-str-index is the actual index ind find-str.
  ; str-index is the actua index of str.
  (cond((= 0 find-str-length) #t)
       ((= str-index str-length) #f)
       ((eq? (string-ref str str-index) (string-ref find-str find-str-index))
          (substring-index-help str (+ str-index 1) str-length find-str (+ 1 find-str-index) (- find-str-length 1)))
       (else #f)))

;; Return the first sentence in str (including a point).
;; The first sentence is running up to the first point followed by space or line termination.
(define (first-sentence-in-string str)
  (let* ((point-index (first-sentence-split-point str)))
    (if (number? point-index) (substring str 0 (+ 1 point-index)) str)))

;; Return all but the first sentence in str.
(define (but-first-sentence-of-string str)
  (let ((point-index (first-sentence-split-point str)))
    (if point-index (substring str (+ point-index 2) (string-length str)) "")))

; Return the split point of the first sentence in str.
; If no split point can be located, return #f.
(define (first-sentence-split-point str)
  (let* ((point-index-0 (substring-index str 0 ". "))
         (point-index-1 (substring-index str 0 (string-append "." (as-string (as-char 10)))))
         (point-index-2 (substring-index str 0 (string-append "." (as-string (as-char 13)))))
         (point-index-min (min-special point-index-0 point-index-1 point-index-2)))
    point-index-min))

(define (min-special . numbers-or-nulls)
  (min-special-1 numbers-or-nulls #f))

(define (min-special-1 numbers-or-nulls res)
  (cond ((null? numbers-or-nulls) res)
        ((boolean? res) (min-special-1 (cdr numbers-or-nulls) (car numbers-or-nulls)))
        ((and (number? res) (number? (car numbers-or-nulls)) (<  (car numbers-or-nulls) res))
            (min-special-1 (cdr numbers-or-nulls) (car numbers-or-nulls)))
        ((and (number? res) (number? (car numbers-or-nulls)) (>= (car numbers-or-nulls) res))
            (min-special-1 (cdr numbers-or-nulls) res))
        (else (min-special-1 (cdr numbers-or-nulls) res))))

;; Strip initial occurences of chars from char-list from string. Returns the empty string if given the empty string.
;; This function makes intermediate substrings, and as such it is not efficient.
(define (strip-initial-characters char-list string)
  (if (= (string-length string) 0)
      ""
      (if (memq (string-ref string 0) char-list)
          (strip-initial-characters char-list (substring string 1 (string-length string)))
          string)))


;; Strip trailing occurences of chars from char-list from string. Returns the empty string if given the empty string.
;; This function makes intermediate substrings, and as such it is not efficient.
(define (strip-trailing-characters char-list string)
  (if (= (string-length string) 0)
      ""
      (let ((lgt (string-length string)))
       (if (memq (string-ref string (- lgt 1)) char-list)
           (strip-trailing-characters char-list (substring string 0 (max 0 (- lgt 2))))
           string))))



;; Strip all initial space characters and lineshifting characters from string.
(define (strip-initial-spaces string)
  (strip-initial-characters 
     (list #\space (integer->char 10) (integer->char 13) (integer->char 9) (integer->char 12))
     string))
      
;; Concatenete a number of strings. Con is an alias of string-append.
;; It is the intension to use it when concatenating HTML constituents.
;; .form (con . string-list)
;; .reference "similar function" "concatenate" "../html4.0-loose/man/surface.html#concatenate"
(define con string-append)

;; Like con, but ensure that there are white space in between the concatenated strings.
(define (con-space . string-list)
  (let ((space-list (make-list (- (length string-list) 1) " ")))
    (string-merge string-list space-list)))

; con-par is in the html library file



(define quote-string (as-string #\"))

;; embed the string x in double string quotes
(define (string-it x)
  (string-append quote-string  x quote-string))

(define single-quote-string (as-string #\'))

;; embed the string x in single string quotes
(define (string-it-single x)
  (string-append single-quote-string x single-quote-string))

;; Exchange destructively char n and m in str. First character is number 0.
;; Not a function, thus no return value.
(define (exchange-chars-in-str! str n m)
  (let ((remember-char (string-ref str m)))
    (string-set! str m (string-ref str n))
    (string-set! str n remember-char)))

;; Ensure that the last character in str (a string) is ch (a char)
(define (ensure-final-character str ch)
  (let ((lgt (string-length str)))
    (if (and (> lgt 0)
             (eq? ch (string-ref str (- lgt 1))))
         str
         (string-append str (as-string ch)))))

;; Repeat the string str n times.
;; If n equals 0, return the empty string. 
;; Causes a fatal error if n is negative.
(define (repeat-string str n)
  (cond ((< n 0) (error (string-append "repeat-string with negative repeat count is not supported: " (as-string n))))
        ((= n 0) "")
        (else (string-append str (repeat-string str (- n 1))))))




;;; Functions that change letter case in string.
;;; Here comes a number of functions which changes the letter case of a string.
;;; In general we recommend use of the non-destructive versions of the functions, thus
;;; encouraging a clean, functional programming style. Due a difference between mutable and
;;; immutable strings, we have experienced problems with the destructive procedures in MzScheme.


;  Capitalizing characters and strings.

;; Mutate str to have an initial capital character. 
;; A destructive procedure. See capitalize-string-nd for a non-destructive variant.
;; .internal-references "non-destructive variant" "capitalize-string-nd"
(define (capitalize-string str)
  (string-set! str 0 (capitalize-char (string-ref str 0)))
  str)

;; Return str with capital, initial character. 
;; A non-destructive variant of capitalize-string.
;; .internal-references "destructive variant" "capitalize-string"
(define (capitalize-string-nd str)
  (let ((res (string-copy str)))
    (string-set! res 0 (capitalize-char (string-ref str 0)))
    res))

; if it makes sense, return the capital character corresponding to ch.
; else, just return ch
(define (capitalize-char ch)
  (let ((char-code (char->integer ch)))
    (if (lower-case-letter-code? char-code)
        (let ((offset (small-capital-offset char-code)))
          (integer->char (+ char-code offset)))
        ch)))

(define (lower-case-letter-code? n)
  (or (and (>= n 97) (<= n 122)) (= n 230) (= n 248) (= n 229)))

; in all cases, the distance between lower and upper case letters are -32 in the ASCII table
(define (small-capital-offset n)
 (cond ((and (>= n 97) (<= n 122)) -32)
       ((= n 230) -32)
       ((= n 248) -32)
       ((= n 229) -32)
       (else 0)))

; -----------------------------------------------------------------------------
; Upcasing all characters in a string:

;; Upcase all characters in str. This function is non-destructive, i.e., it does not change the parameter str.
(define (upcase-string str)
  (let ((res (make-string (string-length str) #\space)))
    (upcase-string-help! str res 0 (string-length str))))

(define (upcase-string-help! input output i lgt)
  (cond ((>= i lgt) output)
        (else (string-set! output i (capitalize-char (string-ref input i)))
              (upcase-string-help! input output (+ i 1) lgt))))

; -----------------------------------------------------------------------------

; Downcasing all characters in a string:

;; Downcase all characters in str. This function is non-destructive, i.e., it does not change the parameter str.
(define (downcase-string str)
  (let ((res (make-string (string-length str) #\space)))
    (downcase-string-help! str res 0 (string-length str))))

(define (downcase-string-help! input output i lgt)
  (cond ((>= i lgt) output)
        (else (string-set! output i (decapitalize-char (string-ref input i)))
              (downcase-string-help! input output (+ i 1) lgt))))

; -----------------------------------------------------------------------------

; decapitalizing characters and strings.

;; Mutate str to have lower case, initial character. 
;; A destructive procedure. See decapitalize-string-nd for a non-destructive variant.
;; .internal-references "non-destructive variant" "decapitalize-string-nd"
(define (decapitalize-string str)
  (string-set! str 0 (decapitalize-char (string-ref str 0)))
  str)

;; Return str with lower case, initial character. 
;; A non-destructive variant of decapitalize-string.
;; .internal-references "destructive variant" "decapitalize-string"
(define (decapitalize-string-nd str)
  (let ((res (string-copy str)))
    (string-set! res 0 (decapitalize-char (string-ref str 0)))
    res))

; If it makes sense, return the lower case character corresponding to ch.
; else, just return ch.
(define (decapitalize-char ch)
  (let ((char-code (char->integer ch)))
    (if (upper-case-letter-code? char-code)
        (let ((offset (large-capital-offset char-code)))
          (integer->char (+ char-code offset)))
        ch)))

(define (upper-case-letter-code? n)
  (or (and (>= n 65) (<= n 90)) (= n 198) (= n 216) (= n 197)))

(define (large-capital-offset n)
 ; in all cases, the distance between lower and upper case letters are -32 in the ASCII table
 (cond ((and (>= n 65) (<= n 90)) 32)
       ((= n 198) 32)
       ((= n 216) 32)
       ((= n 197) 32)
       (else 0)))

; ---------------------------------------------------------------------------------------------------



;;; Message displaying and error handling procedures.

;; Display a warning message line on standard output via the Scheme display function.
;; This is not a fatal error
(define (display-warning message)
  (display (string-append "Warning: " message)) (newline))

;; Display an error message and stop the program. 
;; This is a fatal event.
(define (display-error message)
  (error message))

;; Display a message on standard output.
;; Not a warning, and not fatal by any means.
(define (display-message message)
  (begin (display (string-append message)) (newline)))


;; Stop the program with messages. 
;; This procedures takes an arbitrary number of parameters, which are string converted and string-appended
;; to the final error message.
(define (laml-error . messages)
  (error
    (string-merge 
      (map as-string messages)
      (make-list (- (length messages) 1) " "))))

;; Return a list of error message strings for those conditions that are true.
;; The function returns #f in case no errors are found.
;; There are no errors if all conditions evaluate to #f, and in this case returns the #f.
;; err-condition-message-list is a property list (of even length) of error-condition error messages pairs.
;; This function checks the conditions and returns a concatenated list of error messages.
;; If no error conditions are found, return #f.
(define (errors-among-conditions . err-condition-message-list)
  (errors-among-conditions-1 err-condition-message-list #f '()))

(define (errors-among-conditions-1 err-condition-message-list errors-found accumulated-error-messages)
  (cond ((null? err-condition-message-list) (if errors-found (reverse accumulated-error-messages) #f))
        (else (let ((error-condition (car err-condition-message-list))
                    (error-message (cadr err-condition-message-list)))
                (if error-condition 
                    (errors-among-conditions-1 (cddr err-condition-message-list) #t (cons error-message accumulated-error-messages))
                    (errors-among-conditions-1 (cddr err-condition-message-list) errors-found accumulated-error-messages))))
  ))




;;; File name components.

;; Return the filename component sans the final extension.
;; The extension, in a file name, is the part that follows the last `.'.
;; If no dot character is found the function returns file-name
(define (file-name-sans-extension file-name)
  (let ((extension-pos (find-in-string-from-end file-name #\.)))
    (if extension-pos
        (substring file-name 0 extension-pos)
        file-name)))

;; Return the part of file-name without extension and without an initial path.
;; Works as expected even there are dots in the initial path.
;; .example (file-name-proper "/xxx/yyy/zzz.eee") = "zzz".
(define (file-name-proper file-name)
 (let* ((extension-pos (find-in-string-from-end file-name #\.))
        (forward-slash-pos (find-in-string-from-end file-name #\/))
        (backward-slash-pos (find-in-string-from-end file-name #\\))
        (max-slash-pos (cond ((and forward-slash-pos backward-slash-pos) (max forward-slash-pos backward-slash-pos))
                             (forward-slash-pos forward-slash-pos)
                             (backward-slash-pos backward-slash-pos)
                             (else -1)))
        (extension-pos-1 (if (and extension-pos (> extension-pos max-slash-pos)) extension-pos #f))
      )
  (substring
      file-name
      (+ max-slash-pos 1)
      (if extension-pos-1 extension-pos-1 (string-length file-name)))))


;; Return the extension of file-name. If there is no extension, return the empty string.
;; The extension, in a file name, is the part that follows the last `.'.
;; This function handles dots in the initial path properly.
(define (file-name-extension file-name)
 (let ((extension-pos (find-in-string-from-end file-name #\.))
       (forward-slash-pos (find-in-string-from-end file-name #\/))
       (backward-slash-pos (find-in-string-from-end file-name #\\)))
  (cond ((and extension-pos forward-slash-pos (> extension-pos forward-slash-pos))
            (substring file-name (+ extension-pos 1) (string-length file-name)))
        ((and extension-pos forward-slash-pos (<= extension-pos forward-slash-pos))
            "")
        ((and extension-pos backward-slash-pos (> extension-pos backward-slash-pos))
             (substring file-name (+ extension-pos 1) (string-length file-name)))
        ((and extension-pos backward-slash-pos (<= extension-pos backward-slash-pos))
             "")
        (extension-pos (substring file-name (+ extension-pos 1) (string-length file-name)))
        (else ""))))


;; Return the initial path of the file-name.
;; The initial path of a file name is the prefix of the file name without the proper file name
;; and without the extension. The initial path ends in a forward of backward slash, or it is empty.
(define (file-name-initial-path file-name)
 (let ((extension-pos (find-in-string-from-end file-name #\.))
       (forward-slash-pos (find-in-string-from-end file-name #\/))
       (backward-slash-pos (find-in-string-from-end file-name #\\)))
  (substring 
    file-name
    0
    (cond ((and forward-slash-pos backward-slash-pos) (+ 1 (max forward-slash-pos backward-slash-pos)))
	  (forward-slash-pos (+ 1 forward-slash-pos))
	  (backward-slash-pos (+ 1 backward-slash-pos))
	  (else 0))
  )))

;; Return whether x - a string - represents an absolute path to a file.
(define (absolute-file-path? x)
  (let ((forward-slash-pos (find-in-string x #\/))
        (backward-slash-pos (find-in-string x #\\))
        (colon-pos (find-in-string x #\:)))
     (or (and (number? forward-slash-pos) (= 0 forward-slash-pos))
         (and (number? colon-pos) (= 1 colon-pos)))))

;; Return the name of the parent directory of dir (a string), or #f if dir is the root directory.
(define (parent-directory dir)
  (let* ((dir1 (ensure-final-character dir #\/))
         (lgt (string-length dir1))
         (dir2 (substring dir1 0 (max (- lgt 1) 0)))  ; dir without ending slash
         (forward-slash-pos (find-in-string-from-end dir2 #\/))
         (backward-slash-pos (find-in-string-from-end dir2 #\\)))
   (cond ((and forward-slash-pos backward-slash-pos (>= forward-slash-pos backward-slash-pos))
             (substring dir2 0 (+ 1 forward-slash-pos)))
         ((and forward-slash-pos backward-slash-pos (>= backward-slash-pos forward-slash-pos))
             (substring dir2 0 (+ 1 backward-slash-pos)))
         (forward-slash-pos
             (substring dir2 0 (+ 1 forward-slash-pos)))
         (backward-slash-pos
             (substring dir2 0 (+ 1 backward-slash-pos)))
         (else #f))))         

;; Return the number of directory levels in between dir1 and dir2.
;; If dir1 is not a subdirectory of dir2, or dir2 is not a subdirectory of dir1 return #f.
;; .example (directory-level-difference "/x/x/z/v/" "/x/x/") = 2
;; .example (directory-level-difference "/x/x/" "/x/x/z/v/") = -2
(define (directory-level-difference dir1 dir2)
 (let ((dir1-lc (downcase-string dir1))
       (dir2-lc (downcase-string dir2)))
  (let ((res1 (directory-level-difference-1 dir1-lc dir2-lc 0))
        (res2 (directory-level-difference-1 dir2-lc dir1-lc 0)))
    (cond ((and res1 (number? res1)) res1)
          ((and res2 (number? res2)) (- res2))
          (else #f)))))

(define (directory-level-difference-1 dir1 dir2 n)
 (let ((parent-dir-1 (parent-directory dir1)))
   (cond ((and dir1 dir2 (equal? dir1 dir2)) n)
         ((and parent-dir-1 (string? parent-dir-1)) (directory-level-difference-1 parent-dir-1 dir2 (+ n 1)))
         ((not parent-dir-1) #f))))

;; Given a relative file path, return a list of path constituents.
;; A relative file path is not allowed to start with a slash.
;; This function does only support forward slashes.
;; .example (relative-path-to-path-list "xxx/yyy/zzz/") = ("xxx" "yyy" "zzz")
;; .example (relative-path-to-path-list "xxx/yyy/zzz") = ("xxx" "yyy" "zzz")
;; .example (relative-path-to-path-list "xxx") = ("xxx")
(define (relative-path-to-path-list dir)
  (let* ((dir1 (if (eq? (string-ref dir (- (string-length dir) 1)) #\/)
                   (substring dir 0 (- (string-length dir) 1))
                   dir))  ; no trailing slash
         (lgt (string-length dir1))
         (forward-slash-pos (find-in-string dir1 #\/)))
    (if forward-slash-pos
        (cons (substring dir1 0 forward-slash-pos)
              (relative-path-to-path-list (substring dir1 (+ 1 forward-slash-pos) lgt)))
        (list dir1))))

;; Ensure that the directory with path (string-append prefix-dir file-and-ext) exists.
;; If necessary, create dir in prefix-dir.
(define (ensure-directory-existence! prefix-dir dir)
  (if (not (directory-exists? (string-append prefix-dir dir)))
      (make-directory-in-directory prefix-dir dir)))

;; Ensure that the relative path, as represented by dir, exists in prefix-dir.
;; Creates the necessary directories in prefix-dir.
(define (ensure-directory-path-existence! prefix-dir dir)
 (let ((path-list (relative-path-to-path-list dir)))
   (ensure-directory-path-existence-1! prefix-dir path-list)))

(define (ensure-directory-path-existence-1! prefix-dir path-list)
  (if (not (null? path-list))
      (let ((first-path (car path-list)))
         (ensure-directory-existence! prefix-dir first-path)
         (ensure-directory-path-existence-1! (string-append prefix-dir first-path "/") (cdr path-list)))))


;;; Other functions. 
;;; Here follows a set of miscellaneous functions.


;; A quite special HTML line breaking function.
;; Html format str, either with br og p tags between lines.
;; depends on break-at-all from decoding stuff. 
;; Should perhaps be in the convenience library???
(define (re-break str)
 (letrec ((line-breaker (break-at-all #\newline)))  ; from decoding stuff
  (let* ((lines (line-breaker str))
         (line-lengths (map string-length lines))
         (max-line-length (max-int-list line-lengths)))
   (if (> max-line-length 120)
       (apply string-append 
          (map (lambda (ln) (string-append ln "<p>")) lines))
       (apply string-append 
          (map (lambda (ln) (string-append ln "<br>")) lines))))))

(define (max-int-list lst)
 (max-int-list-help lst 0))
 
(define (max-int-list-help lst res)
  (if (null? lst) 
      res
      (max-int-list-help (cdr lst) (max res (car lst)))))


;; Return a CR string
(define CR "
")

;; Return a CR string.
;; Please notice that there is a conflict between this function and the MzScheme url.ss net stuff.
;; (We should get rid of this function in LAML).
(define (newline-string)
  CR)


; Functions earlier in the cgi library:

;; Save the alist on a file named filename. Filename is assumed to be a full path to the file.
(define (save-a-list alist filename)
  (if (file-exists? filename)   ; new 31.3.2000
       (delete-file filename))
  (with-output-to-file filename
     (lambda () (write alist))))

;; Return a unique file name with prefix. The suffic becomes the current-time i seconds representation
(define (unique-timed-file-name prefix)
  (string-append prefix (number->string (current-time))))

;; Append x to file-name. The file is assumed to contain a Lisp list. x is added (actually pre-pended) to the list on the file,
;; and the file is written back. The ordering of the elements in the file list is not assumed to be important.
;; As a precondition, the file named file-name is assumed to exists.
(define (file-append file-name x)
 (let* ((port (open-input-file file-name))
        (contents (read port))
        (new-contents (append (list x) contents)))
  (close-input-port port)
  (delete-file file-name)  ; new!
  (let ((output-port (open-output-file file-name)))
    (write new-contents output-port)
    (close-output-port output-port))))

;; Read the (next) Lisp expression from file-name
(define (file-read file-name)
 (let* ((port (open-input-file file-name))
        (contents (read port)))
   (close-input-port port)
   contents))

;; Read all Lisp expression from file-name.
;; This function returns these forms as a list of top level forms from the file.
(define (file-read-all file-name)
 (let* ((port (open-input-file file-name))
        (contents (file-read-all-1 port '())))
   (close-input-port port)
   (reverse contents)))

(define (file-read-all-1 port res)
 (let ((form (read port)))
   (if (eof-object? form)
       res
       (file-read-all-1 port (cons form res)))))

;; Write the list expression x at the file named file-name
(define (file-write x file-name)
  (if (file-exists? file-name) (delete-file file-name))
  (let ((output-port (open-output-file file-name)))
    (write x output-port)
    (close-output-port output-port)))

;; Displays the first parameter x on a file named filename
(define (save-on-file x filename)
  (if (file-exists? filename)   ; new 31.3.2000
       (delete-file filename))
  (with-output-to-file filename
     (lambda () (display x))))


;; The identify function of one parameter
(define (id-1 x) x)




;; Is a (the first par) a multiplum of b (the last par)?
(define (multiplum-of a b)
  (= 0 (remainder a b)))



;; Copy the text file in from-path to the file in to-path. 
;; A quick and dirty solution by reading and writing strings to and from files.
;; If the destination file exists you must pass a third parameter, overwrite, with the value #t
(define (copy-text-file from-path to-path overwrite?)
  (if (and (file-exists? to-path) overwrite?) (delete-file to-path))
  (let ((contents (read-text-file from-path)))
    (if (not (file-exists? to-path))
        (write-text-file contents to-path)
        (error (string-append "copy-a-file: Overwriting an existing file requires a third overwrite #t parameter: " to-path)))))


;; Copy each of the files in the list files from source-dir to target-dir.
;; Both source-dir and target-dir ends in a slash.
(define (copy-files files source-dir target-dir)
  (letrec ((copy-a-file 
            (lambda (f)
             (let ((target-file (string-append target-dir f)))
               (if (file-exists? target-file) (delete-file target-file))
               (copy-file (string-append source-dir f) target-file)))))
   (for-each copy-a-file files)))


