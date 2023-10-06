#! ./scmlit \
- !#
;;;; "xgen.scm", Convert C Event structs to xevent.h and xevent.scm.
;; Copyright (C) 1991-2000 Free Software Foundation, Inc.
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

(define (xgen.scm args)
  (cond ((= 1 (length args))
	 (xatoms)
	 (apply xgen args)
	 #t)
	(else (xgen.usage))))

(define (xgen.usage)
  (display "\
\
Usage: xgen.scm /usr/include/X11/Xlib.h
\
  Creates xevent.h and xevent.scm, from the `typedef struct's
  in /usr/include/X11/xlib.h.

http://people.csail.mit.edu/jaffer/SCM
"
	   (current-error-port))
  #f)

(require 'common-list-functions)
(require 'string-search)
(require 'string-case)
(require 'line-i/o)
(require 'printf)
(require 'scanf)

(define progname (if (defined? *optind*)
		     (list-ref *argv* (+ -1 *optind*))
		     (car (program-arguments))))

;; SCHEMEIFY-NAME:
;; * Changes _ to -
;; * Changes the first - to : if it is within the first 3 characters.
;; * inserts dashes between `StudlyCaps'

(define (schemeify-name pre name)
  (define nstr (string-subst name "_" "-"))
  (let ((sid (string-index nstr #\-)))
    (cond ((and pre sid (< sid 3)) (string-set! nstr sid #\:)
	   nstr)
	  (pre (string-append pre (StudlyCapsExpand nstr)))
	  (else (StudlyCapsExpand nstr)))))

(define (extract-structs port)
  (define typedef-struct (string-append (string #\newline) "typedef struct {"))
  (define structs '())
  (do ((find? (find-string-from-port? typedef-struct port)
	      (find-string-from-port? typedef-struct port)))
      ((not find?) (reverse structs))
    (set! structs (cons (extract-struct port) structs))))

(define (extract-struct port)
  (define elts '())
  (do ((typ (read-token port) (read-token port)))
      ((or (eof-object? typ) (eq? #\} typ))
       (let ((name (read-token port)))
	 (let ((chr (read-token port)))
	   (cond ((eqv? #\; chr))
		 (else (slib:error 'expected #\; 'but-read chr)))
	   (cons name (reverse elts)))))
    (letrec ((loop
	      (lambda (name)
		;;(print 'typ= typ 'name= name)
		(case name
		  ((#\*)
		   (case (string->symbol typ)
		     ((char) (set! typ "string"))
		     (else (set! typ (string-append typ "*"))))
		   (loop (read-token port)))
		  (else
		   (let loop2 ((chr (read-token port)))
		     (case chr
		       ((#\;)
			(set! elts (cons (list typ name) elts)))
		       ((#\,)
			(set! elts (cons (list typ name) elts))
			(loop (read-token port)))
		       ((#\[)
			(find-string-from-port? "]" port)
			(case (string->symbol typ)
			  ((char) (set! typ "string"))
			  (else (set! typ (string-append typ "*"))))
			(loop2 (read-token port)))
		       (else (slib:error 'expected #\; 'read chr))))))
		)))
      (case (string->symbol typ)
	((unsigned)
	 (set! typ (read-token port))
	 (case (string->symbol typ)
	   ((long short char int) (set! typ "int")
	    (loop (read-token port)))
	   (else (loop typ))))
	((struct)
	 (set! typ (read-token port))
	 (loop (read-token port)))
	((union)
	 (find-string-from-port? close-brace-string port)
	 ;;(set! typ "union")
	 (loop (read-token port)))
	(else (loop (read-token port)))))))

(define close-brace-string (string #\}))

(define (read-token port)
  (let ((chr (peek-char port)))
    (cond ((eqv? chr #\newline)
	   (read-char port)
	   (do ((fchr (peek-char port) (peek-char port)))
	       ((not (eqv? #\# fchr)))
	     (read-char port)
	     (if (eq? 'if (read port))
		 (do ((fchr (peek-char port) (peek-char port)))
		     ((eqv? #\# fchr))
		   (read-line port)))
	     (read-line port))
	   (read-token port))
	  ((char-whitespace? chr)
	   (read-char port)
	   (read-token port))
	  ((eqv? #\/ chr)
	   (cond ((and (find-string-from-port? "/*" port)
		       (find-string-from-port? "*/" port
					       ;;(lambda (chr) (display chr) #f)
					       ))
		  ;;(newline)
		  (read-token port))
		 (else
		  (slib:error 'botched-comment (read-line port)))))
	  ((or (char-alphabetic? chr) (eqv? #\_ chr))
	   (car (scanf-read-list "%[a-zA-Z_0-9]" port)))
	  ;;((string-index "[]*" chr) (string->symbol (string chr)))
	  (else (read-char port)))))

(defconst Bool (string->symbol "Bool"))
(defconst Time (string->symbol "Time"))

(define event-map
  '(
    ("XMotionEvent"		"MotionNotify")
    ("XKeyEvent"		"KeyPress"	"KeyRelease")
    ("XButtonEvent"		"ButtonPress"	"ButtonRelease")
    ("XPointerMovedEvent"	"MotionNotify")
    ("XCrossingEvent"		"EnterNotify"	"LeaveNotify")
    ("XFocusChangeEvent"	"FocusIn"	"FocusOut")
    ("XKeymapEvent"		"KeymapNotify")
    ("XExposeEvent"		"Expose")
    ("XGraphicsExposeEvent"	"GraphicsExpose")
    ("XNoExposeEvent"		"NoExpose")
    ("XVisibilityEvent"		"VisibilityNotify")
    ("XCreateWindowEvent"	"CreateNotify")
    ("XDestroyWindowEvent"	"DestroyNotify")
    ("XUnmapEvent"		"UnmapNotify")
    ("XMapEvent"		"MapNotify")
    ("XMapRequestEvent"		"MapRequest")
    ("XReparentEvent"		"ReparentNotify")
    ("XConfigureEvent"		"ConfigureNotify")
    ("XConfigureRequestEvent"	"ConfigureRequest")
    ("XGravityEvent"		"GravityNotify")
    ("XResizeRequestEvent"	"ResizeRequest")
    ("XCirculateEvent"		"CirculateNotify")
    ("XCirculateRequestEvent"	"CirculateRequest")
    ("XPropertyEvent"		"PropertyNotify")
    ("XSelectionClearEvent"	"SelectionClear")
    ("XSelectionRequestEvent"	"SelectionRequest")
    ("XSelectionEvent"		"SelectionNotify")
    ("XColormapEvent"		"ColormapNotify")
    ("XClientMessageEvent"	"ClientMessage")
    ("XMappingEvent"		"MappingNotify")
    ))

(define event-fields '())
(define event-field-idx #x10)
(define (do-field xevent.scm fname)
  (define apr (assoc fname event-fields))
  (cond (apr (cdr apr))
	(else
	 (set! event-fields (acons fname event-field-idx event-fields))
	 (fprintf xevent.scm "(define X-event:%s #x%02x)\n"
		  (schemeify-name #f fname)
		  event-field-idx)
	 (set! event-field-idx (+ 1 event-field-idx))
	 (+ -1 event-field-idx))))

(define (xgen filename)
  (let ((structs (remove-if-not
		  (lambda (struct) (substring? "Event" (car struct)))
		  (call-with-input-file filename extract-structs))))
    (call-with-open-ports
     (open-file "xevent.h" "w")
     (open-file "xevent.scm" "w")
     (lambda (xevent.h xevent.scm)
       (define evs #f)
       (fprintf xevent.h "/* %s extracted typedef structs from %s */\n"
		progname filename)
       (fprintf xevent.h
		"#ifdef SCM_EVENT_FIELDS\n")
       (fprintf xevent.scm ";; %s extracted typedef structs from %s\n"
		progname filename)
       (for-each
	(lambda (struct)
	  (define name (car struct))
	  (set! evs (assoc name event-map))
	  (and
	   evs
	   (for-each
	    (lambda (decl)
	      (define typ (string->symbol (car decl)))
	      (qase typ
		((,Bool ,Time int char)
		 (fprintf xevent.h "  ")
		 (for-each (lambda (event-name)
			     (fprintf xevent.h "case (%s<<8)+0x%02x: "
				      event-name
				      (do-field xevent.scm (cadr decl))))
			   (cdr evs))
		 (fprintf xevent.h "return %s(((%s *) x)->%s);\n"
			  (qase typ
			    ((,Bool) "x_make_bool")
			    ((,Time) "ulong2num")
			    ((int char) "MAKINUM"))
			  name
			  (cadr decl)))
		;;(else (print 'typ typ))
		))
	    (cdr struct))))
	structs)
       (fprintf xevent.h "#else\n")
       (for-each (lambda (apr)
		   (for-each (lambda (evnt)
			       (fprintf xevent.h
					"  {%-20s \"%s\"},\n"
					(string-append evnt ",") evnt))
			     (cdr apr)))
		 event-map)
       (fprintf xevent.h "#endif\n")))))

(define (xatoms)
  (define /usr/include/X11/Xatom.h "/usr/include/X11/Xatom.h")
  (define /usr/include/X11/Xcms.h "/usr/include/X11/Xcms.h")
  (call-with-open-ports
   (open-file /usr/include/X11/Xatom.h "r")
   (open-file "/usr/include/X11/Xcms.h" "r")
   (open-file "xatoms.scm" "w")
   (lambda (xatom.h xcms.h xatoms.scm)
     (fprintf xatoms.scm ";; %s extracted definitions from %s\n"
	      progname /usr/include/X11/Xatom.h)
     (do ((line (read-line xatom.h) (read-line xatom.h)))
	 ((eof-object? line))
       (let ((lst (scanf-read-list "#define XA_%s ((Atom) %d)" line)))
	 (and (list? lst)
	      (case (length lst)
		((2) (fprintf xatoms.scm "(define %s %d)\n"
			      (string-subst (car lst) "_" "-")
			      (cadr lst)))
		((0) #f)		;(write-line line)
		(else (slib:error 'xatom.h 'line line))))))
     (fprintf xatoms.scm ";; %s extracted definitions from %s\n"
	      progname /usr/include/X11/Xcms.h)
     (do ((line (read-line xcms.h) (read-line xcms.h)))
	 ((eof-object? line))
       (let ((lst (scanf-read-list "#define Xcms%s (XcmsColorFormat)0x%4x%4x"
				   line)))
	 (and (list? lst)
	      (case (length lst)
		((3) (apply fprintf xatoms.scm "(define\tX:%s\t#x%04x%04x)\n"
			    (string-subst (car lst) "Format" "")
			    (cdr lst)))
		((2) (fprintf xatoms.scm "(define\tX:%s\t#x%08x)\n"
			      (string-subst (car lst) "Format" "")
			      (cadr lst)))
		((0 1) #f)
		(else (slib:error 'xcms.h 'line line)))))))))

;;; Local Variables:
;;; mode:scheme
;;; End:
(exit (xgen.scm (list-tail *argv* *optind*))) ;(and *script* )
