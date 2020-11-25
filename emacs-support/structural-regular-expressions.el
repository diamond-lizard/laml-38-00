(provide 'structural-regular-expressions)

; Translation of structural regular expressions to textual regular
; expressions. The meta character such as + and * are automatically
; escaped when they occur in strings.
; 
; Rationale: It is awkward and error prone to type and mentally parse a
; textual regular expressions. This package provides a clean syntax of
; regular expressions.
; 
; It takes time to convert a structural regular expression to a textual
; one.  One possible usage will be to convert a structural regular
; expressions to its textual counterpart only once. This is done with
; the function regular-expression.
 
 
; SYNTAX AND TRANSLATION.
; X-es are regular expressions, which in the simple case just are strings.
; and C-es are characters encoded as strings of length one.

; (group X1)                                              \( X1 \)
; (concat X1 ... Xn)   
; (or X1 ... Xn)                                          \|
; (any-single-char)                                       .
; (zero-or-more X)                                        *
; (one-or-more X)                                         +
; (zero-or-one X)                                         ?
; (char-set C1 ... CN)                                    [ ...]
; (char-range C1 CN)                                      ..-..
; Within char-set and char-range
;   tab       backspace
;   newline   vertical-tab
;   formfeed  return
;   escape    space
;   quote
;   (char n), n a decimal number
;
; (char-set C1 ... (char-range C2 C3))
; (complement-char-set C1 ... CN)                         [^ ... ]
; (complement-char-range C1 CN)                           [^..-..]
; (at-beginning-of-line X)                                ^
; (at-end-of-line X)                                      $
; (at-beginning-of-buffer X)                              \`
; (at-end-of-buffer X)                                    \'
; (at-beginning-of-word X)                                \<
; (at-end-of-word X)                                      \>
; (any-single-word-constituent-char)                      \w
; (any-single-non-word-constituent-char)                  \W
; (syntax-code i),  i is a character syntax code          \si
; (not-syntax-code i),                                    \Si
;     i is a character syntax code,
;     which either is a one letter string
;     or one of the symbols
;       whitespace            word-constituent
;       symbol-constituent)   punctuation-character
;       open-parenthesis      close-parenthesis
;       close-parenthesis     string-quote
;       escape                character-quote
;       paired-delimiter      expression-prefix
;       comment-starter       comment-ender
; 
; Note:
; Only one level of char-range is allowed inside a char-set.
; 
; 
; EXAMPLES:
; (concat (syntax-code "(") "abekat" (syntax-code ")"))  
; (at-beginning-of-line (concat (any-single-char) (one-or-more "x")))  
; (at-end-of-line (concat (any-single-char) (one-or-more "x")))  
; (concat "a" (one-or-more (complement-char-range "b" "f")) "x")   
; (concat "abe" (complement-char-set (char-range "d" "j")) "kat")  
; (one-or-more (or (char-range "a" "f") (char-set "x" "y" "z")))
; (one-or-more (or (char-range (char 97) (char 102)) (char-set "x" "y" "z")))
; (one-or-more (char-set "a" "f" (char-range "h" "k"))) 
; (concat (char-set "a" "b" "c") "kat") 
; (one-or-more (or (one-or-more "xxx") (concat "abe" "kat")))   
; (or (one-or-more "xxx") (concat "abe" "kat")) 
; (concat "abe" (zero-or-more "kat"))  
; (or "foo" "bar")    
; (one-or-more (char-set tab newline space escape))  
; (concat (syntax-code open-parenthesis) 
;         (one-or-more (or (any-single-word-constituent-char)
;                          (char-set space)))
;         (syntax-code close-parenthesis))
; (char-set "^" "[" "-" "]")  NOT OK - strange inteference
; (char-set "a" "[" "]") 
; (char-set "]" "a" "[" )
; (char-set "-" "a" ) OK - some strange behaviour on '^'...
; (char-set "^")   error!
; (char-set "^" "a")  



; top-level function:

(defun regular-expression (reg-exp)
  "Return a textual regular expression from the structural
regular-expression REG-EXP. This function is the external interface
function of the structural regular-expression package. The following
summarizes the syntax. X-es are structural regular expressions,
including strings. C-es are characters which we represent as string of
length one. Within string X-es the characters that have
special meanings within textual regular expressions can be used
without problems (they are explicitly translated). Similarly
with C-es in character sets the character strings `]', `-' and `^',
which are special in textual regular expressions, can be used
without problems, because proper translations are performed.

Syntax summary:

  (group X1)  
  (concat X1 ... Xn)
  (or X1 ... Xn)                                     
  (any-single-char)                                  
  (zero-or-more X)                                   
  (one-or-more X)                                    
  (zero-or-one X)                                    

  (char-set C1 ... CN)                               
  (complement-char-set C1 ... CN)                    
  (char-range C1 CN)                                 
  (complement-char-range C1 CN)                      
  Ci may also be:
    tab       backspace
    newline   vertical-tab
    formfeed  return
    escape    space
    (char n), n a decimal number

  (at-beginning-of-line X)                           
  (at-end-of-line X)                                 
  (at-beginning-of-buffer X)                         
  (at-end-of-buffer X)                               
  (at-beginning-of-word X)                           
  (at-end-of-word X)                                 
  (any-single-word-constituent-char)                 
  (any-single-non-word-constituent-char)             

  (syntax-code i),  
  (not-syntax-code i),
    i is a character syntax code,
    which either is a one letter string
    or one of the symbols
      whitespace            word-constituent
      symbol-constituent    punctuation-character
      open-parenthesis      close-parenthesis
      close-parenthesis     string-quote
      escape                character-quote
      paired-delimiter      expression-prefix
      comment-starter       comment-ender
"
  
  (reg-exp-traverse reg-exp))

; a primitive function, used under testing:
(defun rsf (reg-exp)
  (re-search-forward (reg-exp-traverse reg-exp)))

(defun reg-exp-traverse (reg-exp)
  "Return a textual regular expression from the structural
regular expression REG-EXP-LIST.  This is the toplevel function
in the structural regular expression package."
  (cond ((listp reg-exp) (reg-exp-traverse-list reg-exp))
        ((stringp reg-exp) (reg-exp-traverse-string reg-exp))
        ((symbolp reg-exp) 
         (reg-exp-char-set-list-element reg-exp))
        (t (error "Error in structural regular expression"))))


(defun reg-exp-traverse-string (reg-exp-string)
  (reg-exp-escape-special-chars reg-exp-string))

;(defun reg-exp-traverse-symbol (reg-exp-symbol)
;  (reg-exp-escape-special-chars (symbol-to-string reg-exp-symbol)))

(defun reg-exp-escape-special-chars (str)
  "In STR, replace the meta characters in regular expressions
with their escaped couterparts. "
  (reg-exp-string-replace-list str '("\\" "$" "^" "." "*" "+" "?" "[" "]" )
                  '("\\\\" "\\$" "\\^" "\\." "\\*" "\\+" "\\?" "\\[" "\\]")))
    
(defun reg-exp-traverse-list (reg-exp-list)
  "Handle a structural regular expression list"
  (cond  ((eq 'group  (car reg-exp-list))
              (concat "\\("
                      (reg-exp-traverse (cadr reg-exp-list))
                      "\\)"))
         ((eq 'concat (car reg-exp-list)) 
            (mapconcat
              (function reg-exp-traverse)
              (cdr reg-exp-list)
              ""))
        ((eq 'zero-or-more  (car reg-exp-list))
              (concat "\\("
                      (reg-exp-traverse (cadr reg-exp-list))
                      "\\)"
                      "*"))
        ((eq 'one-or-more  (car reg-exp-list))
              (concat "\\("
                      (reg-exp-traverse (cadr reg-exp-list))
                      "\\)"
                      "+"))
        ((eq 'zero-or-one  (car reg-exp-list))
              (concat "\\("
                      (reg-exp-traverse (cadr reg-exp-list))
                      "\\)"
                      "?"))
        ((eq 'or (car reg-exp-list))   ; implicitly grouped, May 18, 2011
              (concat
                "\\("
                  (mapconcat 
                   (function 
                    (lambda (re)
                      (reg-exp-parenthesize (reg-exp-traverse re))))
                   (cdr reg-exp-list)
                   "\\|")
                "\\)"))
       ((eq 'any-single-char (car reg-exp-list)) ".")
       ((eq 'at-beginning-of-line (car reg-exp-list))
              (concat "^\\("
                      (reg-exp-traverse (cadr reg-exp-list))
                      "\\)"))
       ((eq 'at-end-of-line (car reg-exp-list))
              (concat "\\("
                      (reg-exp-traverse (cadr reg-exp-list))
                      "\\)$"))
        ((eq 'char-set  (car reg-exp-list))
              (concat "["
                 (mapconcat
                   (function reg-exp-char-set-element)
                   (reg-exp-global-char-set-translate (cdr reg-exp-list))
                   "")
                 "]"))
        ((eq 'complement-char-set  (car reg-exp-list))
              (concat "[^"
                 (mapconcat
                   (function reg-exp-char-set-element)
                   (reg-exp-global-char-set-translate (cdr reg-exp-list))
                   "")
                 "]"))
        ((eq 'char-range  (car reg-exp-list))
              (concat "["
                       (concat (reg-exp-char-set-element (cadr reg-exp-list))
                                  "-"
                               (reg-exp-char-set-element (caddr reg-exp-list)))
                      "]"))
        ((eq 'complement-char-range  (car reg-exp-list))
              (concat "[^"
                       (concat (reg-exp-char-set-element (cadr reg-exp-list))
                                  "-"
                               (reg-exp-char-set-element (caddr reg-exp-list)))
                      "]"))
        ((eq 'at-beginning-of-buffer (car reg-exp-list))
               (concat "\\`\\("
                      (reg-exp-traverse (cadr reg-exp-list))
                      "\\)"))
        ((eq 'at-end-of-buffer (car reg-exp-list))
              (concat "\\("
                      (reg-exp-traverse (cadr reg-exp-list))
                      "\\)\\'"))
        ((eq 'at-beginning-of-word (car reg-exp-list))
               (concat "\\<\\("
                      (reg-exp-traverse (cadr reg-exp-list))
                      "\\)"))
        ((eq 'at-end-of-word (car reg-exp-list))
              (concat "\\("
                      (reg-exp-traverse (cadr reg-exp-list))
                      "\\)\\>"))
       ((eq 'any-single-word-constituent-char (car reg-exp-list)) "\\w") 
       ((eq 'any-single-non-word-constituent-char (car reg-exp-list)) "\\W") 
       ((eq 'syntax-code (car reg-exp-list)) 
              (concat "\\s" (reg-exp-syntax-code (cadr reg-exp-list))))
       ((eq 'not-syntax-code (car reg-exp-list)) 
              (concat "\\S" (reg-exp-syntax-code (cadr reg-exp-list))))

       (t (error (concat "Unknown keyword in regular expression: " 
                         (symbol-to-string (car reg-exp-list)))))
  ))

(defun reg-exp-char-set-element(x)
  "Handle a char-set element in a structural regular expression"
  (cond ((symbolp x) (reg-exp-char-set-list-element x))
        ((and (stringp x) (= 1 (length x))) x)
        ((reg-exp-char-range-p x) (concat (reg-exp-char-set-element (cadr x))
                                  "-"
                                  (reg-exp-char-set-element (caddr x))))
        ((reg-exp-char-p x) (char-to-string (cadr x)))
        ((and (stringp x) (equal x "--")) x)
        (t (error "Error in char set"))))

(defun reg-exp-char-p (x)
  (and (consp x) (= 2 (length x)) (eq (car x) 'char)))

(defun reg-exp-char-range-p (x)
  "Handle a char-range in a structural regular expression"
  (and (listp x) (eq 'char-range (car x))))

(defun reg-exp-char-set-list-element (x)
  "Handle a symbol within a char set"
  (cond ((eq 'tab x) "\t")
        ((eq 'backspace x ) "\b")
        ((eq 'newline x) "\n")
        ((eq 'vertical-tab x) "\v")
        ((eq 'formfeed x) "\f")
        ((eq 'return x) "\r")
        ((eq 'escape x) "\e")
        ((eq 'space x) " ")
        ((eq 'quote x) "\"")
        (t (error ("Unknown symbol in regular expression")))))
        

(defun reg-exp-parenthesize (re)
  "Surround the expression RE with parentheses"
  (concat "\\(" re "\\)"))


(defun reg-exp-string-replace (str single-letter-search-string replace-string)
 "In STR search for occurences of SINGLE-LETTER-SEARCH-STRING and replace
it with occurences of REPLACE-STRING. As the name indicates,
SINGLE-LETTER-SEARCH-STRING is a string of only one letter."
  (let* ((str-list (string-to-list str))
         (str-out-list
           (mapcar
             (function 
               (lambda (ch)
                  (if (eq ch (string-to-char single-letter-search-string))
                      replace-string
                      (char-to-string ch))))
             str-list)))
    (apply (function concat) str-out-list)))

               
(defun reg-exp-string-replace-list (str from-list to-list)
  "Return a copy of STR in which occurences of the single letters strings in FROM-LIST,
which is a list of strings, are substituted by the (geneal) strings in TO-LIST.
From-list and to-list are required to lists of equal lengths. Ealiers substitutions
may be affected by the late substitutions. Thus, the replacements are sequential."
  (let ((strg str)
        (fl from-list)
        (tl to-list))
    (while fl
      (progn
         (setq strg (reg-exp-string-replace strg (car fl) (car tl)))
         (setq fl (cdr fl))
         (setq tl (cdr tl))))
    strg))

(defun reg-exp-syntax-code (x)
  "Handle symbolic as well as real emacs syntax codes"
  (cond ((symbolp x)
         (cond ((eq x 'whitespace) " ")
               ((eq x 'word-constituent) "w")
               ((eq x 'symbol-constituent) "_")
               ((eq x 'punctuation-character) ".")
               ((eq x 'open-parenthesis) "(")
               ((eq x 'close-parenthesis) ")")
               ((eq x 'close-parenthesis) ")")
               ((eq x 'string-quote) "\"")
               ((eq x 'escape) "\\")
               ((eq x 'character-quote) "/")
               ((eq x 'paired-delimiter) "$")
               ((eq x 'expression-prefix) "'")
               ((eq x 'comment-starter) "<")
               ((eq x 'comment-ender) ">")
               (t (error "Unknown symbolic char-set element"))))
         ((stringp x) x)
         (t (error "Error in syntax code"))))

(defun reg-exp-global-char-set-translate (lst)
  "Make some global changes in LST, which is a list
of char set elements. The changes implement the special
char convention for `]', `-' and `^' in textual regular
expressions. Return the modified list as result."
  (let ((res lst))
    (if (and (= (length lst) 1) (eq (car lst) "^")) 
	(error "Error in char-set: not possible to have single element: '^'"))

    ; force "^" not to be first. 
    (if (equal (car res) "^")
        (setq res (reverse res)))
    (if (equal (car res) "^")
        (error "Weird character set list in char-set"))
 
    ; force "]" to be first:
    (if (member "]" lst)
	(setq res (cons "]" 
			(filter
			 (function
			  (lambda (x)
			    (not (equal x "]"))))
			 lst))))

    ; translate "-" to "--"
    (setq res 
      (mapcar
       (function
         (lambda(x)
           (if (equal x "-") "--" x)))
       res))

    res))

