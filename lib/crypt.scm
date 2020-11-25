; The LAML library and programs written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999  Kurt Normark.
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


;;;; A password crypting library.
;;;; This is a primitive encrypting based on a fairly elaborate transformation of
;;;; string to another string.
;;;; The sheer complexity of the program below adds to the value of the encryption.
;;;; The main function is: crypt-string.
;;;; This library requires the general library.
;;;; .title Reference Manual of the Crypt Library

;;; The main crypting function. 
;;; The main crypting function is crypt-string below.

;; A poor mans password crypting function. The one and only external function of this library.
;; This cryptation will immediately be broken by experts!
;; However, it is better than nothing in practical situations.
(define (crypt-string input-password)
  (let* ((opw-0 (extend-string input-password "kurt"))
         (opw-1 (permute-string opw-0 permutation-vector))
         (opw-2 (shift-up-and-down opw-1 shift-vector)))
    opw-2))

; -----------------------------------------------------------------------------
;;; Helping functions. 
;;; This section contains a few helping functions which may be of some use
;;; in other situations.

;; Extend string st by extension by means of string merging.
(define (extend-string str extension)
 (string-merge (map char->string (string->list str)) (map char->string (string->list extension))))

; -----------------------------------------------------------------------------
; STRING PERMUATION

;; Return a permuation of the string carried out determinitically by
;; means of permutation-vector
;; The second parameter, permutation-vector, is a vector of positive integers
(define (permute-string str permutation-vector)
  (let ((out-str (string-copy str) ))
    (permute-string-1 out-str 0 (string-length str) permutation-vector)))

(define (permute-string-1 o-str n lgt permutation-vector)
  (if (= n lgt)
      o-str
      (begin
        (exchange-chars-in-str! o-str n (calculate-exchange-position n lgt permutation-vector))
        (permute-string-1 o-str (+ n 1) lgt permutation-vector))))


;; Return a number between n and m (actually m - 1), calculated by means of permutation-vector
;; n < m
(define (calculate-exchange-position n m permutation-vector)
  (let* ((d (- m n))
        (x (vector-ref permutation-vector (remainder d permutation-vector-length))))
    (+ n (remainder x d))))

(define permutation-vector '#(4 6 8 13 9 7 2 1 3 23 17))
(define permutation-vector-length (vector-length permutation-vector))
    
        
; -----------------------------------------------------------------------------
; STRING SHIFTING

;; Shift the characters in the string str up and down by means of a shift-vector.
;; A shift-vector is a vector of integers.
(define (shift-up-and-down str shift-vector)
 (let ((out-str (string-copy str)))
  (shift-up-and-down-1 out-str 0 (string-length out-str) shift-vector)))

(define shift-vector '#(4 -5 3 -7 11 -13 2 -1))
(define shift-vector-length (vector-length shift-vector))

(define (shift-up-and-down-1 str n lgt shift-vector)
 (let ((shift-amount (vector-ref shift-vector (remainder n shift-vector-length))))
  (if (= lgt n)
      str
      (begin
         (shift-char-in-string! str n shift-amount)
         (shift-up-and-down-1 str (+ n 1) lgt shift-vector)))))

; Mutate string by cyceling the char number n with displacement
; displacement is an integer. Negative displacements are allowed.
(define (shift-char-in-string! str n displacement)
  (let* ((str1 (downcase-string str)) ; donwcased string
         (chr (string-ref str1 n))    ; extract char n
         (a (char->integer chr)))         ; and calculate integer value
    (if (and (>= a 97) (<= a 122))
        (let* ((m (- a 97))           ; normalize to number between 0 and 25
               (x (+ m displacement)) ; add displacement
               (y (remainder x 26))   ; normalize to integer between 0 and 25
               (chr-new (integer->char (abs (+ 97 (abs y)))))  ; a shifted char
              )
           (string-set! str n chr-new)))))
 

; -----------------------------------------------------------------------------




