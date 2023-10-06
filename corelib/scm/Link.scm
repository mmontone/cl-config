;;;; "Link.scm", Dynamic linking/loading code for SCM.
;; Copyright (C) 1993, 1994, 1995, 1997, 1998, 2002 Free Software Foundation, Inc.
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

;;; Author: Aubrey Jaffer.

(cond
 ((defined? dyn:link)
(define link:able-suffix
  (cond ((provided? 'shl) ".sl")
	((provided? 'sun-dl) ".so")
	((provided? 'mac-dl) ".shlb")
        ((provided? 'win32-dl) ".dll")
	(else ".o")))
(define (file->init_name name)
  (string-append
   "init_"
   (list->string
    (map (lambda (chr) (if (eqv? #\- chr) #\_ chr))
	 (map char-downcase (string->list name))))))
(define link:link
  (lambda (file . libs)
    (let* ((sl (string-length file))
	   (lasl (string-length link:able-suffix))
	   (fname (let loop ((i (- sl 1)))
		    (cond ((negative? i) file)
			  ((vicinity:suffix? (string-ref file i))
			   (substring file (+ i 1) sl))
			  (else (loop (- i 1))))))
	   (nsl (string-length fname))
	   (name (cond ((< nsl lasl) fname)
		       ((string-ci=? (substring fname (- nsl lasl) nsl)
				     link:able-suffix)
			(substring fname 0 (- nsl lasl)))
		       (else fname)))
	   (linkobj #f))
      (if (and (provided? 'sun-dl)
	       (< 3 sl)
	       (not (eqv? (string-ref file 0) '#\/)))
	  (set! file (string-append "./" file)))
      (with-load-pathname file
        (lambda ()
	  (load:pre 'link file)
	  (set! linkobj (or (provided? 'sun-dl) (dyn:link file)))
	  (and linkobj
	       (for-each (lambda (lib)
			   (or (dyn:link lib)
			       (slib:error "couldn't link: " lib)))
			 libs))
	  (if (provided? 'sun-dl) (set! linkobj (dyn:link file)))
	  (cond ((not linkobj) #f)
		((dyn:call (file->init_name name) linkobj)
		 (load:post 'link file)
		 #t)
		(else (dyn:unlink linkobj) #f))))))))

 ((defined? vms:dynamic-link-call)
(define link:able-suffix #f)
(define (link:link file)
  (define dir "")
  (define fil "")
  (let loop ((i (- (string-length file) 1)))
    (cond ((negative? i) (set! dir file))
	  ((vicinity:suffix? (string-ref file i))
	   (set! dir (substring file 0 (+ i 1)))
	   (set! fil (substring file (+ i 1) (string-length file))))
	  (else (loop (- i 1)))))
  (with-load-pathname file
    (lambda ()
      (load:pre 'link file)
      (vms:dynamic-link-call dir fil (file->init_name fil))
      (load:post 'link file))))))

(cond
 ((provided? 'sun-dl)
  ;; These libraries are (deferred) linked in conversion to ".so"
(define (usr:lib lib) #f)
(define (x:lib lib) #f))
 ((provided? 'shl)
(define (usr:lib lib)
  (if (member lib '("c" "m"))
      (string-append "/lib/lib" lib link:able-suffix)
      (string-append "/usr/lib/lib" lib link:able-suffix)))
(define (x:lib lib) (string-append "/usr/X11R5/lib/lib"
				   lib link:able-suffix)))
 ((provided? 'dld:dyncm)
(define (usr:lib lib)
  (or (and (member lib '("c" "m"))
	   (let ((sa (string-append "/usr/lib/lib" lib ".sa")))
	     (and (file-exists? sa) sa)))
      (string-append "/usr/lib/lib" lib ".a")))
(define (x:lib lib) (string-append "/usr/X11/lib/lib" lib ".sa")))
 ((provided? 'dld)
(define (usr:lib lib) (string-append "/usr/lib/lib" lib ".a"))
(define (x:lib lib) (string-append "/usr/X11/lib/lib" lib ".sa"))))
