;;;; Linear-space O(PN) sequence comparison.
;;; "Idiffer.scm" Top-level sequence-comparison functions.
;; Copyright (C) 2003, 2004 Free Software Foundation, Inc.
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

;;; Author: Aubrey Jaffer

(define (diff:invert-edits! edits)
  (define cost (car (array-dimensions edits)))
  (do ((idx (+ -1 cost) (+ -1 idx)))
      ((negative? idx))
    (array-set! edits (- (array-ref edits idx)) idx)))

(define (edits2lcs! lcs edits A)
  (define cost (car (array-dimensions edits)))
  (define len-a (car (array-dimensions A)))
  (let loop ((edx 0)
	     (sdx 0)
	     (adx 0))
    (let ((edit (if (< edx cost) (array-ref edits edx) 0)))
      (cond ((>= adx len-a))
	    ((positive? edit)
	     (loop (+ 1 edx) sdx adx))
	    ((zero? edit)
	     (array-set! lcs (array-ref A adx) sdx)
	     (loop edx (+ 1 sdx) (+ 1 adx)))
	    ((>= adx (- -1 edit))
	     (loop (+ 1 edx) sdx (+ 1 adx)))
	    (else
	     (array-set! lcs (array-ref A adx) sdx)
	     (loop edx (+ 1 sdx) (+ 1 adx)))))))

(define (diff:longest-common-subsequence A B . p-lim)
  (define M (car (array-dimensions A)))
  (define N (car (array-dimensions B)))
  (set! p-lim (if (null? p-lim) -1 (car p-lim)))
  (let ((edits (if (< N M)
		   (diff:edits B A p-lim)
		   (diff:edits A B p-lim))))
    (and edits
	 (let* ((cost (car (array-dimensions edits)))
		(lcs (make-array A (/ (- (+ N M) cost) 2))))
	   (edits2lcs! lcs edits (if (< N M) B A))
	   lcs))))

(define (diff:edits A B . p-lim)
  (define M (car (array-dimensions A)))
  (define N (car (array-dimensions B)))
  (define est (diff:edit-length A B (if (null? p-lim) -1 (car p-lim))))
  (and est
       (let ((CCRR (make-array (A:fixZ32b) (* 2 (+ (max M N) 1))))
	     (edits (make-array (A:fixZ32b) est)))
	 (define fp (make-array (A:fixZ32b)
				(+ (max (- N (quotient M 2))
					(- M (quotient N 2)))
				   (- est (abs (- N M))) ; 2 * p-lim
				   3)))
	 (cond ((< N M)
		(diff2edits! edits fp CCRR B A)
		(diff:invert-edits! edits))
	       (else
		(diff2edits! edits fp CCRR A B)))
	 edits)))

(define (diff:edit-length A B . p-lim)
  (define M (car (array-dimensions A)))
  (define N (car (array-dimensions B)))
  (set! p-lim (if (null? p-lim) -1 (car p-lim)))
  (let ((fp (make-array (A:fixZ32b) (if (negative? p-lim)
					(+ 3 M N)
					(+ 3 (abs (- N M)) p-lim p-lim)))))
    (if (< N M)
	(diff2editlen fp B A p-lim)
	(diff2editlen fp A B p-lim))))
