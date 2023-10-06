;;;; "mkimpcat.scm" Build SCM-specific catalog for SLIB.
;; Copyright (C) 1993, 1994, 1995, 1997, 1998, 1999, 2001, 2003, 2004, 2006 Free Software Foundation, Inc.
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

(let ((catname "implcat")
      (iv (implementation-vicinity)))
  (define (in-implementation-vicinity . paths) (apply in-vicinity iv paths))
  (call-with-output-file (in-implementation-vicinity catname)
    (lambda (op)
      (define (display* . args)
	(for-each (lambda (arg) (display arg op)) args)
	(newline op))
      (define (in-wb-vicinity . paths) (apply in-vicinity iv "../wb/c/" paths))
      (define (in-xscm-vicinity . paths) (apply in-vicinity iv "../xscm-2.01/" paths))
      (define (add-link feature . libs)
	(define syms '())
	;; remove #f from libs list
	(set! libs (let rem ((l libs))
		     (cond ((null? l) l)
			   ((symbol? (car l))
			    (set! syms (cons (car l) syms))
			    (rem (cdr l)))
			   ((car l) (cons (car l) (rem (cdr l))))
			   (else (rem (cdr l))))))
	(cond ((file-exists? (car libs))
	       (display " " op)
	       (write
		(cons feature (cons 'compiled (append syms libs)))
		op)
	       (newline op)
	       #t)
	      (else #f)))
      (define (add-alias from to)
	(display " " op)
	(write (cons from to) op)
	(newline op))
      (define (add-source feature filename)
	(cond ((file-exists? (string-append filename (scheme-file-suffix)))
	       (display " " op)
	       (write (list feature 'source filename) op)
	       (newline op)
	       #t)
	      (else #f)))
      (define (add-links feature usr:lib x:lib link:able-suffix)
	(display* "#+" feature)
	(display* "(")
	(begin
	  (cond ((add-link 'hobbit (in-implementation-vicinity "hobbit" link:able-suffix))))
	  (cond ((add-link 'i/o-extensions
			   (in-implementation-vicinity "ioext" link:able-suffix)
			   (usr:lib "c"))
		 (add-alias 'directory-for-each 'i/o-extensions)
		 (add-alias 'directory 'i/o-extensions)
		 (add-alias 'line-i/o 'i/o-extensions)
		 (add-alias 'pipe 'i/o-extensions)))
	  (cond ((add-link 'rev2-procedures
			   (in-implementation-vicinity "sc2"
						       link:able-suffix))))
	  (cond ((add-link 'byte
			   (in-implementation-vicinity "byte"
						       link:able-suffix))))
	  (cond ((or
		  (add-link 'db
			    (in-implementation-vicinity "wbscm.so"))
		  (add-link 'db
			    (in-implementation-vicinity "db" link:able-suffix)
			    (in-implementation-vicinity "handle" link:able-suffix)
			    (in-implementation-vicinity "blink" link:able-suffix)
			    (in-implementation-vicinity "prev" link:able-suffix)
			    (in-implementation-vicinity "ent" link:able-suffix)
			    (in-implementation-vicinity "sys" link:able-suffix)
			    (in-implementation-vicinity "del" link:able-suffix)
			    (in-implementation-vicinity "stats" link:able-suffix)
			    (in-implementation-vicinity "blkio" link:able-suffix)
			    (in-implementation-vicinity "scan" link:able-suffix)
			    (usr:lib "c"))
		  (add-link 'db
			    (in-wb-vicinity "wbscm.so"))
		  (add-link 'db
			    (in-wb-vicinity "db" link:able-suffix)
			    (in-wb-vicinity "handle" link:able-suffix)
			    (in-wb-vicinity "blink" link:able-suffix)
			    (in-wb-vicinity "prev" link:able-suffix)
			    (in-wb-vicinity "ent" link:able-suffix)
			    (in-wb-vicinity "sys" link:able-suffix)
			    (in-wb-vicinity "del" link:able-suffix)
			    (in-wb-vicinity "stats" link:able-suffix)
			    (in-wb-vicinity "blkio" link:able-suffix)
			    (in-wb-vicinity "scan" link:able-suffix)
			    (usr:lib "c")))
		 ;; wbtab and rwb-isam moved to "Simple associations"
		 (add-alias 'wb 'db)))
	  (cond ((add-link 'mysql
			   (in-implementation-vicinity "database"
						       link:able-suffix)
			   ;;(usr:lib "mysqlclient") ;?
			   )))
	  (cond ((add-link 'stringvector
			   (in-xscm-vicinity "strvec" link:able-suffix))
		 (add-source 'x11   (in-xscm-vicinity "x11"))
		 (add-source 'xevent(in-xscm-vicinity "xevent"))
		 (add-source 'xt    (in-xscm-vicinity "xt"))
		 (add-source 'xm    (in-xscm-vicinity "xm"))
		 (add-source 'xmsubs(in-xscm-vicinity "xmsubs"))
		 (add-source 'xaw   (in-xscm-vicinity "xaw"))
		 (add-source 'xpm   (in-xscm-vicinity "xpm"))))

	  (add-link 'turtle-graphics
		    (in-implementation-vicinity "turtlegr" link:able-suffix)
		    (x:lib "X11")
		    (usr:lib "m")
		    (usr:lib "c"))
	  (add-link 'Xlib
		    (in-implementation-vicinity "x" link:able-suffix)
		    (x:lib "X11")
		    (usr:lib "c"))
	  (add-link 'curses
		    (in-implementation-vicinity "crs" link:able-suffix)
		    (usr:lib "ncurses")
		    ;;(usr:lib "curses")
		    ;;(usr:lib "termcap")
		    (usr:lib "c"))
	  (add-link 'edit-line
		    (in-implementation-vicinity "edline" link:able-suffix)
		    (usr:lib "readline")
		    (usr:lib "termcap")
		    (usr:lib "c"))
	  (add-link 'regex
		    (in-implementation-vicinity "rgx" link:able-suffix)
		    (usr:lib "c"))
	  (add-link 'unix
		    'i/o-extensions
		    (in-implementation-vicinity "unix" link:able-suffix)
		    (usr:lib "c"))
	  (add-link 'posix
		    (in-implementation-vicinity "posix" link:able-suffix)
		    (usr:lib "c"))
	  (add-link 'socket
		    (in-implementation-vicinity "socket" link:able-suffix)
		    (usr:lib "c"))
	  (add-link 'diff
		    (in-implementation-vicinity "differ" link:able-suffix))
	  (add-link 'record
		    (in-implementation-vicinity "record" link:able-suffix))
	  (add-link 'generalized-c-arguments
		    (in-implementation-vicinity "gsubr" link:able-suffix))
	  (add-link 'array-for-each
		    (in-implementation-vicinity "ramap" link:able-suffix))
	  (add-link 'byte-number
		    (in-implementation-vicinity "bytenumb" link:able-suffix))
	  )
	(display* ")")
	)

      (begin
	(display* ";\"" catname "\" Implementation-specific SLIB catalog for "
		  (scheme-implementation-type) (scheme-implementation-version)
		  ".  -*-scheme-*-")
	(display* ";")
	(display* ";			DO NOT EDIT THIS FILE")
	(display* "; it is automagically generated by \"" *load-pathname* "\"")
	(newline op)
	)

      ;; Output association lists to file "implcat"

      (begin
	;; Simple associations -- OK for all modes of dynamic-linking
	(display* "(")
	(add-alias '2rs 'r2rs)
	(add-alias '3rs 'r3rs)
	(add-alias '4rs 'r4rs)
	(add-alias '5rs 'r5rs)
	(add-source 'hobbit (in-implementation-vicinity "hobbit"))
	(add-source 'scmhob (in-implementation-vicinity "scmhob"))
	(add-source 'regex-case (in-implementation-vicinity "rgxcase"))
	(add-source 'url-filename (in-implementation-vicinity "urlfile"))
	(add-source 'disarm (in-implementation-vicinity "disarm"))
	(add-source 'build (in-implementation-vicinity "build"))
	(add-source 'compile (in-implementation-vicinity "compile"))
	(or
	 (add-source 'wb-table (in-implementation-vicinity "wbtab"))
	 (add-source 'wb-table (in-wb-vicinity "wbtab")))
	(or
	 (add-source 'rwb-isam (in-implementation-vicinity "rwb-isam"))
	 (add-source 'rwb-isam (in-wb-vicinity "rwb-isam")))
	(display* ")")
	)

      (display* "#+" 'primitive-hygiene)
      (display* "(")
      (add-source 'macro (in-implementation-vicinity "Macro"))
      (display* ")")

      (add-links 'dld
		 (lambda (lib) (string-append "/usr/lib/lib" lib ".a"))
		 (lambda (lib) (string-append "/usr/X11/lib/lib" lib ".sa"))
		 ".o")
      (add-links 'dld:dyncm
		 (lambda (lib)
		   (or (and (member lib '("c" "m"))
			    (let ((sa (string-append "/usr/lib/lib" lib ".sa")))
			      (and (file-exists? sa) sa)))
		       (string-append "/usr/lib/lib" lib ".a")))
		 (lambda (lib) (string-append "/usr/X11/lib/lib" lib ".sa"))
		 ".o")
      (add-links 'shl
		 (lambda (lib)
		   (if (member lib '("c" "m"))
		       (string-append "/lib/lib" lib ".sl")
		       (string-append "/usr/lib/lib" lib ".sl")))
		 (lambda (lib)
		   (string-append "/usr/X11R5/lib/lib" lib ".sl"))
		 ".sl")
      (add-links 'sun-dl
		 ;; These libraries are (deferred) linked in conversion to ".so"
		 (lambda (lib) #f)
		 (lambda (lib) #f)
		 ".so")
      (add-links 'win32-dl
                 (lambda (lib) #f)
                 (lambda (lib) #f)
                 ".dll")
      )))
