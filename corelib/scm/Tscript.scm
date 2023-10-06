;;;; "Tscript.scm" transcript-on and transcript-off.
;; Copyright (C) 1999 Free Software Foundation, Inc.
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

;;; Author: Radey Shouman

(define transcript-on #f)
(define transcript-off #f)

(let ((*transcript-stack* '()))
  (define (trans-on filename)
    (let ((trans (open-output-file filename))
	  (inp (current-input-port))
	  (outp (current-output-port))
	  (errp (current-error-port)))
      (define (clone-port port)
	(make-soft-port
	 (vector (and (output-port? port)
		      (lambda (c)
			(write-char c port)
			(write-char c trans)))
		 (and (output-port? port)
		      (lambda (s)
			(display s port)
			(display s trans)))
		 (and (output-port? port)
		      (lambda ()
			(force-output port)
			(force-output trans)))
		 (and (input-port? port)
		      (lambda ()
			(let ((c (read-char port)))
			  (if (eof-object? c)
			      (close-output-port trans)
			      (write-char c trans))
			  c)))
		 (lambda ()
		   (close-port port)))
	 (if (input-port? port)
	     (if (output-port? port) "r+" "r")
	     "w")))

      (set! *transcript-stack*
	    (cons (list trans
			(current-input-port)
			(current-output-port)
			(current-error-port))
		  *transcript-stack*))
      (set-current-input-port (clone-port inp))
      (set-current-output-port (clone-port outp))
      (set-current-error-port (clone-port errp))))

  (define (trans-off)
    (cond ((pair? *transcript-stack*)
	   (apply (lambda (trans inp outp errp)
		    (close-port trans)
		    (set-current-input-port inp)
		    (set-current-output-port outp)
		    (set-current-error-port errp))
		  (car *transcript-stack*))
	   (set! *transcript-stack* (cdr *transcript-stack*)))
	  (else
	   (error "No transcript active"))))

  (set! transcript-on trans-on)
  (set! transcript-off trans-off))

(provide 'transcript)
