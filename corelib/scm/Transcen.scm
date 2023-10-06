;;;; "Transcen.scm", Complex transcendental functions for SCM.
;; Copyright (C) 1992, 1993, 1995, 1997, 2005, 2006, 2018 Free Software Foundation, Inc.
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

;;; Author: Jerry D. Hedden.

;;;; 2005-05 SRFI-70 extensions.
;;; Author: Aubrey Jaffer

(define compile-allnumbers #t)		;for HOBBIT compiler

;;;; Legacy real function names
(cond
 ((defined? $exp)
  (define real-sqrt $sqrt)
  (define real-exp $exp)
  (define real-expt $expt)
  (define real-ln $log)
  (define real-log10 $log10)

  (define real-sin $sin)
  (define real-cos $cos)
  (define real-tan $tan)
  (define real-asin $asin)
  (define real-acos $acos)
  (define real-atan $atan)

  (define real-sinh $sinh)
  (define real-cosh $cosh)
  (define real-tanh $tanh)
  (define real-asinh $asinh)
  (define real-acosh $acosh)
  (define real-atanh $atanh))

 (else
  (define $sqrt real-sqrt)
  (define $exp real-exp)
  (define $expt real-expt)
  (define $log real-ln)
  (define $log10 real-log10)

  (define $sin real-sin)
  (define $cos real-cos)
  (define $tan real-tan)
  (define $asin real-asin)
  (define $acos real-acos)
  (define $atan real-atan)

  (define $sinh real-sinh)
  (define $cosh real-cosh)
  (define $tanh real-tanh)
  (define $asinh real-asinh)
  (define $acosh real-acosh)
  (define $atanh real-atanh)))

(define (real-log base x)
  (if (and (real? x) (not (negative? x)) (real? base) (positive? base))
      (/ (real-ln x) (real-ln base))
      (slib:error 'real-log base x)))

(define $pi (* 4 (real-atan 1)))
(define pi $pi)
(define (pi* z) (* $pi z))
(define (pi/ z) (/ $pi z))

;;;; Complex functions

(define (exp z)
  (if (real? z) (real-exp z)
      (make-polar (real-exp (real-part z)) (imag-part z))))

(define (ln z)
  (if (and (real? z) (>= z 0))
      (real-ln z)
      (make-rectangular (real-ln (magnitude z)) (angle z))))
(define log ln)

(define (sqrt z)
  (if (real? z)
      (if (negative? z) (make-rectangular 0 (real-sqrt (- z)))
	  (real-sqrt z))
      (make-polar (real-sqrt (magnitude z)) (/ (angle z) 2))))

(define (sinh z)
  (if (real? z) (real-sinh z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* (real-sinh x) (real-cos y))
			  (* (real-cosh x) (real-sin y))))))
(define (cosh z)
  (if (real? z) (real-cosh z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* (real-cosh x) (real-cos y))
			  (* (real-sinh x) (real-sin y))))))
(define (tanh z)
  (if (real? z) (real-tanh z)
      (let* ((x (* 2 (real-part z)))
	     (y (* 2 (imag-part z)))
	     (w (+ (real-cosh x) (real-cos y))))
	(make-rectangular (/ (real-sinh x) w) (/ (real-sin y) w)))))

(define (asinh z)
  (if (real? z) (real-asinh z)
      (log (+ z (sqrt (+ (* z z) 1))))))

(define (acosh z)
  (if (and (real? z) (>= z 1))
      (real-acosh z)
      (log (+ z (sqrt (- (* z z) 1))))))

(define (atanh z)
  (if (and (real? z) (> z -1) (< z 1))
      (real-atanh z)
      (/ (log (/ (+ 1 z) (- 1 z))) 2)))

(define (sin z)
  (if (real? z) (real-sin z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* (real-sin x) (real-cosh y))
			  (* (real-cos x) (real-sinh y))))))
(define (cos z)
  (if (real? z) (real-cos z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* (real-cos x) (real-cosh y))
			  (- (* (real-sin x) (real-sinh y)))))))
(define (tan z)
  (if (real? z) (real-tan z)
      (let* ((x (* 2 (real-part z)))
	     (y (* 2 (imag-part z)))
	     (w (+ (real-cos x) (real-cosh y))))
	(make-rectangular (/ (real-sin x) w) (/ (real-sinh y) w)))))

(define (asin z)
  (if (and (real? z) (>= z -1) (<= z 1))
      (real-asin z)
      (* -i (asinh (* +i z)))))

(define (acos z)
  (if (and (real? z) (>= z -1) (<= z 1))
      (real-acos z)
      (+ (/ (angle -1) 2) (* +i (asinh (* +i z))))))

(define (atan z . y)
  (if (null? y)
      (if (real? z)
	  (real-atan z)
	  (/ (log (/ (- +i z) (+ +i z))) +2i))
      ($atan2 z (car y))))

;;;; SRFI-70
(define (expt z1 z2)
  (cond ((and (exact? z2) (not (and (zero? z1) (negative? z2))))
	 (integer-expt z1 z2))
	((zero? z2) (+ 1 (* z1 z2)))
	((and (real? z2) (real? z1) (positive? z1))
	 (real-expt z1 z2))
	(else
	 (exp (* (if (zero? z1) (real-part z2) z2) (log z1))))))

(define (quo x1 x2)
  (if (and (exact? x1) (exact? x2))
      (quotient x1 x2)
      (truncate (/ x1 x2))))

(define (rem x1 x2)
  (if (and (exact? x1) (exact? x2))
      (remainder x1 x2)
      (- x1 (* x2 (quo x1 x2)))))

(define (mod x1 x2)
  (if (and (exact? x1) (exact? x2))
      (modulo x1 x2)
      (- x1 (* x2 (floor (/ x1 x2))))))

(define (exact-round x) (inexact->exact (round x)))
(define (exact-floor x) (inexact->exact (floor x)))
(define (exact-ceiling x) (inexact->exact (ceiling x)))
(define (exact-truncate x) (inexact->exact (truncate x)))

(define (infinite? z) (and (= z (* 2 z)) (not (zero? z))))
(define (finite? z) (not (infinite? z)))

(provide 'math-real)
(provide 'srfi-94)
