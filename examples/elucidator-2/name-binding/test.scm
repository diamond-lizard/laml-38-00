;; The function point. This function acts as a class.
(define (point 
           x y) ; yy
 (let 
  ( ; yy
    ( ; a
      x ; b
       5
     ;c
    ) 
       (y y)
      )

   ;;; Accessors

   ; The method that accesses x
   ; More
   (define (getx) x)

   ;; The method that accesses y
   (define (gety) y)

   ;; Add p to this point
   (define (add p) 
    (point 
     (+ x (send 'getx p))
     (+ y (send 'gety p))))

   ;; Return the type of this point
   (define (type-of) 'point 
   )
     
   (define self (lambda (length)
     (cond ((eqv? length 'getx) getx)
           ((eqv? length 'gety) gety)
           ((eqv? length 'add)  add)
           ((eqv? length 'type-of) type-of)
	   (else (error "Undefined length" length)))))
     
   self))

(define (p12)
  (let loop ((a 1)
             (b 10))
    (if (= a 10)
        '()
        (cons (+ a b) (loop (+ a 1) (+ b 2))))))

(define (tt)

  (let ((car 5))
    (let ((cadr car))
      (+ car cadr)
      (- 7 (+ car cadr))))

  (let ((car length)
        (length car))
    (null? length))

  (let ((car length)
        (length car))
    (list car length))
  (let* ((car length)
         (length car))
    (list car length))
  (letrec ((car length)
           (length car))
    (list car length)))


(define strange  ; xxx
  (lambda (if case strange?)  ; yyy
    (strange? if case loop)
    (letrec  ((length (+ 5 lambda))  
              (lambda length))
      (+ if case loop length lambda))))

(define (strange? if case loop)
 (strange if case loop)
 (let (( ; And here
         length  ; HERE
          5 ; and everywhere
         ) ; xxx
       (lambda 7)) ; yyy
   (+ if case loop length lambda))
 (let ((length 5)
       (lambda 7)) ; yyy
   (+ if case loop length lambda)))



;; The factorial functions. Also known as n!
;; .parameter n An integer
;; .pre-condition n >= 0
;; .returns n * (n-1) * ... * 1
(define (fac n)
 (if (= n 0) 1 (* n (fac (- n 1)))))

(define (length x) (* 2 x))

(define (lambda x) (length (* x x))) 

(define y (lambda (x) (* 2 x)))