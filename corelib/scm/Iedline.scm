;;;; "Iedline.scm" SCM interface to readline library
;; Copyright (C) 1994, 1995 Free Software Foundation, Inc.
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

;; Author: Radey Shouman

;; Change both current-input-port and current-output-port to
;; allow line editing of input.
;; All output goes through a soft port in order to detect prompt
;; lines, i.e. lines unterminated by a newline.

(define (make-edited-line-port)
  (let ((prompt "")
	(outp (default-output-port))
	(inp (default-input-port))
	(strp (call-with-input-string "" identity)))
    (make-soft-port
     (vector (lambda (c)
	       (write-char c outp))
	     (lambda (s)
	       (display s outp)
	       (or (zero? (string-length s))
		   (eq? #\newline (string-ref s (- (string-length s) 1)))
		   (begin
		     (set! prompt (string-append "\r" s))
		     (force-output outp))))
	     (lambda ()
	       (force-output outp))
	     (lambda ()
	       (let tail ((c (read-char strp)))
		 (if (char? c) c
		     (let ((str (read-edited-line prompt)))
		       (if (string? str)
			   (let ((n (string-length str)))
			     (add-history str)
			     (vector-set-length! str (+ 1 n))
			     (string-set! str n #\newline)
			     (set! strp (call-with-input-string
					 str identity))
			     (tail (read-char strp)))
			   str)))))
	     #f)
     open_both)))

(define line-editing
  (let ((edit-port #f)
	(oiport #f)
	(ooport #f))
    (lambda arg
      (define past edit-port)
      (cond ((null? arg))
	    ((and (car arg) (not edit-port))
	     (set! edit-port (make-edited-line-port))
	     (set! oiport (set-current-input-port edit-port))
	     (set! ooport (set-current-output-port edit-port)))
	    (edit-port
	     (set-current-input-port oiport)
	     (set-current-output-port ooport)
	     (set! edit-port #f)))
      past)))

(and
 (if (provided? 'unix) (isatty? (current-input-port)) #t)
 (eq? (current-input-port) (default-input-port))
 (not (getenv "EMACS"))
 (line-editing #t))
