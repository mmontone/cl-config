;;;; "bench.scm", Scheme benchmarks: digits of pi and random statistics.
;; Copyright (C) 1996, 1997, 2001, 2002 Free Software Foundation, Inc.
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Author: Aubrey Jaffer.

(require 'transcript)
(require-if 'inexact 'root)
(require-if 'inexact 'printf)
(require 'random)
(require 'array)
;;(load (in-vicinity (implementation-vicinity) "prng-v.scm"))

(load (in-vicinity (program-vicinity) "pi.scm"))
(define isqrt
  (cond ((provided? 'inexact) sqrt)
	(else (require 'root) integer-sqrt)))
(define i/
  (cond ((provided? 'inexact) /)
	(else quotient)))
(define around
  (cond ((provided? 'inexact)
	 (let ()
	   (require 'printf)
	   (lambda (x prec) (sprintf #f "%.*g" prec x))))
	(else (lambda (x prec) x))))

(define (time-call proc . args)
  (let ((start-time (get-internal-run-time)))
    (apply proc args)
    (i/ (* 1000 (- (get-internal-run-time) start-time))
	internal-time-units-per-second)))

(define (benchmark-pi . arg)
  (define file (if (null? arg) "pi.log" (car arg)))
  (do ((digits 50 (+ digits digits))
       (t 0 (time-call pi (+ digits digits) 4)))
      ((> t 3600)
       (do ((tl '() (cons (time-call pi digits 4) tl))
	    (j 12 (+ -1 j)))
	   ((zero? j)
	    (let* ((avg (i/ (apply + tl) (length tl)))
		   (dev (isqrt (i/ (apply
				    + (map (lambda (x) (* (- x avg) (- x avg)))
					   tl))
				   (length tl)))))
	      (and file (transcript-on file))
	      (for-each display
			(list digits " digits of pi took " (around avg 4) ".ms"
			      " +/- " (around dev 2) ".ms"))
	      (newline)
	      (let ((scaled-avg (i/ (* (i/ (* avg 1000) digits) 1000) digits))
		    (scaled-dev (i/ (* (i/ (* dev 1000) digits) 1000) digits)))
		(for-each display
			  (list " That is about "
				(around scaled-avg 4) ".ms/(kB)^2"
				" +/- "
				(around scaled-dev 2) ".ms/(kB)^2"))
		(newline)
		(and file (transcript-off)))
	      ))))))

(define (prng samples modu sta)
  (define sra (make-array (A:fixN32b) samples))
  (do ((cnt (+ -1 samples) (+ -1  cnt))
       (num (random modu sta) (random modu sta))
       (sum 0 (+ sum num)))
      ((negative? cnt)
       (set! sum (+ sum num))
       (let ((mean (i/ sum samples)))
	 (define (square-diff x) (define z (- x mean)) (* z z))
	 (do ((cnt (+ -1 samples) (+ -1 cnt))
	      (var2 0 (+ (square-diff (array-ref sra cnt)) var2)))
	     ((negative? cnt)
	      (for-each display
			(list sum " / " samples " = "
			      mean " +/- " (isqrt (i/ var2 samples))))
	      (newline)))))
    (array-set! sra num cnt)))

(define (benchmark-prng . arg)
  (define file (if (null? arg) "prng.log" (car arg)))
  (define sta
    (seed->random-state "http://swissnet.ai.mit.edu/~jaffer/SLIB.html"))
  (do ((samples 125 (* 4 samples))
       (t 0 (time-call prng (* 2 samples) 999 sta)))
      ((or (> t 1000) (and (not (provided? 'bignum)) (> samples 1000)))
       (do ((tl '() (cons (time-call prng samples 999 sta) tl))
	    (j 12 (+ -1 j)))
	   ((zero? j)
	    (let* ((avg (i/ (apply + tl) (length tl)))
		   (dev (isqrt (i/ (apply
				    + (map (lambda (x) (* (- x avg) (- x avg)))
					   tl))
				   (length tl)))))
	      (and file (transcript-on file))
	      (for-each display
			(list samples " random samples took " (around avg 4) ".ms"
			      " +/- " (around dev 2) ".ms"))
	      (newline)
	      (let ((scaled-avg (i/ (* avg 1000) samples))
		    (scaled-dev (i/ (* dev 1000) samples)))
		(for-each display
			  (list " That is about "
				(around scaled-avg 4) ".ms/kB"
				" +/- "
				(around scaled-dev 2) ".ms/kB"))
		(newline)
		(and file (transcript-off)))))))))

(benchmark-prng)
(newline)
(benchmark-pi)
