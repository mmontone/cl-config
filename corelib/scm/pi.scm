;;;; "pi.scm" Programs for computing digits of PI and e.
;; Copyright (C) 1991, 1993, 1994, 1995 Free Software Foundation, Inc.
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Authors: Aubrey Jaffer & Jerry D. Hedden

;;; (pi <n> <d>) prints out <n> digits of pi in groups of <d> digits.

;;; 'Spigot' algorithm origionally due to Stanly Rabinowitz.
;;; This algorithm takes time proportional to the square of <n>/<d>.
;;; This fact can make comparisons of computational speed between systems
;;; of vastly differring performances quicker and more accurate.

;;; Try (pi 100 5)
;;; The digit size <d> will have to be reduced for larger <n> or an
;;; overflow error will occur (on systems lacking bignums).

;;; It your Scheme has bignums try (pi 1000).

(define (pi n . args)
  (if (null? args) (bigpi n)
      (let* ((d (car args))
	     (r (do ((s 1 (* 10 s))
		     (i d (- i 1)))
		    ((zero? i) s)))
	     (n (+ (quotient n d) 1))
	     (m (quotient (* n d 3322) 1000))
	     (a (make-vector (+ 1 m) 2)))
	(vector-set! a m 4)
	(do ((j 1 (+ 1 j))
	     (q 0 0)
	     (b 2 (remainder q r)))
	    ((> j n))
	  (do ((k m (- k 1)))
	      ((zero? k))
	    (set! q (+ q (* (vector-ref a k) r)))
	    (let ((t (+ 1 (* 2 k))))
	      (vector-set! a k (remainder q t))
	      (set! q (* k (quotient q t)))))
	  (let ((s (number->string (+ b (quotient q r)))))
	    (do ((l (string-length s) (+ 1 l)))
		((>= l d) (display s))
	      (display #\0)))
	  (if (zero? (modulo j 10)) (newline) (display #\ )))
	(newline))))

;;; (pi <n>) prints out <n> digits of pi.

;;; 'Spigot' algorithm originally due to Stanly Rabinowitz:
;;;
;;; PI = 2+(1/3)*(2+(2/5)*(2+(3/7)*(2+ ... *(2+(k/(2k+1))*(4)) ... )))
;;;
;;; where 'k' is approximately equal to the desired precision of 'n'
;;; places times 'log2(10)'.
;;;
;;; This version takes advantage of "bignums" in SCM to compute all
;;; of the requested digits in one pass!  Basically, it calculates
;;; the truncated portion of (PI * 10^n), and then displays it in a
;;; nice format.

(define (bigpi digits)
  (let* ((n (* 10 (quotient (+ digits 9) 10)))	; digits in multiples of 10
	 (z (inexact->exact (truncate		; z = number of terms
			     (/ (* n (log 10)) (log 2)))))
	 (q (do ((x 2 (* 10000000000 x))	; q = 2 * 10^n
		 (i (/ n 10) (- i 1)))
		((zero? i)  x)))
	 (_pi (number->string			; _pi = PI * 10^n
	       ;; do the calculations in one pass!!!
	       (let pi_calc ((j z) (k (+ z z 1)) (p (+ q q)))
		 (if (zero? j)
		     p
		     (pi_calc (- j 1) (- k 2) (+ q (quotient (* p j) k))))))))
    ;; print out the result ("3." followed by 5 groups of 10 digits per line)
    (display (substring _pi 0 1)) (display #\.) (newline)
    (do ((i 0 (+ i 10)))
	((>= i n))
      (display (substring _pi (+ i 1) (+ i 11)))
      (display (if (zero? (modulo (+ i 10) 50)) #\newline #\ )))
    (if (not (zero? (modulo n 50))) (newline))))

;;; (e <n>) prints out <n> digits of 'e'.

;;; Uses the formula:
;;;
;;;           1    1    1    1          1
;;;   e = 1 + -- + -- + -- + -- + ... + --
;;;           1!   2!   3!   4!         k!
;;;
;;; where 'k' is determined using the desired precision 'n' in:
;;;
;;;    n  <  ((k * (ln(k) - 1)) / ln(10))
;;;
;;; which uses Stirling's formula for approximating ln(k!)
;;;
;;; This program takes advantage of "bignums" in SCM to compute all
;;; the requested digits at once!  Basically, it calculates the
;;; fractional part of 'e' (i.e., e-2) as a fraction of two bignums
;;; 'e_n' and 'e_d', determines the integer part of (e_n * 10^n)/e_d,
;;; and then displays it in a nice format.

(define (e digits)
  (let* ((n (* 10 (quotient (+ digits 9) 10)))	; digits in multiples of 10
	 (k (do ((i 15 (+ i 1)))		; k = number of terms
		((< n (/ (* i (- (log i) 1)) (log 10)))  i)))
	 (q (do ((x 1 (* 10000000000 x))	; q = 10^n
		 (i (/ n 10) (- i 1)))
		((zero? i)  x)))
	 (_e (let ((ee
		    ; do calculations
		    (let e_calc ((i k) (e_d 1) (e_n 0))
		      (if (= i 1)
			  (cons (* q e_n) e_d)
			  (e_calc (- i 1) (* e_d i) (+ e_n e_d))))))
	       (number->string (+ (quotient (car ee) (cdr ee))
				  ; rounding
				  (if (< (remainder (car ee) (cdr ee))
					 (quotient (cdr ee) 2))
				      0 1))))))
    ;; print out the result ("2." followed by 5 groups of 10 digits per line)
    (display "2.") (newline)
    (do ((i 0 (+ i 10)))
	((>= i n))
      (display (substring _e i (+ i 10)))
      (display (if (zero? (modulo (+ i 10) 50)) #\newline #\ )))
    (if (not (zero? (modulo n 50))) (newline))))
