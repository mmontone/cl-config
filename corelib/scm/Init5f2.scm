;;;; "Init.scm", Scheme initialization code for SCM.
;; Copyright (C) 1991-2008 Free Software Foundation, Inc.
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

(define (scheme-implementation-type) 'scm)
(define (scheme-implementation-version) "5f2")
(define (scheme-implementation-home-page)
  "http://people.csail.mit.edu/jaffer/SCM")

;@
(define in-vicinity string-append)
;@
(define (user-vicinity)
  (case (software-type)
    ((vms)	"[.]")
    (else	"")))
;@
(define vicinity:suffix?
  (let ((suffi
	 (case (software-type)
	   ((amiga)				'(#\: #\/))
	   ((macos thinkc)			'(#\:))
	   ((ms-dos windows atarist os/2)	'(#\\ #\/))
	   ((nosve)				'(#\: #\.))
	   ((unix coherent plan9)		'(#\/))
	   ((vms)				'(#\: #\]))
	   (else
	    (slib:warn "require.scm" 'unknown 'software-type (software-type))
	    "/"))))
    (lambda (chr) (and (memv chr suffi) #t))))
;@
(define (pathname->vicinity pathname)
  (let loop ((i (- (string-length pathname) 1)))
    (cond ((negative? i) "")
	  ((vicinity:suffix? (string-ref pathname i))
	   (substring pathname 0 (+ i 1)))
	  (else (loop (- i 1))))))
(define (program-vicinity)
  (if *load-pathname*
      (pathname->vicinity *load-pathname*)
      (slib:error 'program-vicinity " called; use slib:load to load")))
;@
(define sub-vicinity
  (case (software-type)
    ((vms) (lambda
	       (vic name)
	     (let ((l (string-length vic)))
	       (if (or (zero? (string-length vic))
		       (not (char=? #\] (string-ref vic (- l 1)))))
		   (string-append vic "[" name "]")
		   (string-append (substring vic 0 (- l 1))
				  "." name "]")))))
    (else (let ((*vicinity-suffix*
		 (case (software-type)
		   ((nosve) ".")
		   ((macos thinkc) ":")
		   ((ms-dos windows atarist os/2) "\\")
		   ((unix coherent plan9 amiga) "/"))))
	    (lambda (vic name)
	      (string-append vic name *vicinity-suffix*))))))
;@
(define (make-vicinity <pathname>) <pathname>)
;@
(define with-load-pathname
  (let ((exchange
	 (lambda (new)
	   (let ((old *load-pathname*))
	     (set! *load-pathname* new)
	     old))))
    (lambda (path thunk)
      (let ((old #f))
	(dynamic-wind
	    (lambda () (set! old (exchange path)))
	    thunk
	    (lambda () (exchange old)))))))

(define slib:features
  (append '(ed getenv tmpnam abort transcript with-file
	       ieee-p1178 rev4-report rev4-optional-procedures
	       hash object-hash delay dynamic-wind fluid-let
	       multiarg-apply multiarg/and- logical defmacro
	       string-port source current-time sharp:semi
	       math-integer ;math-real and srfi-94 provided in "Transcen.scm"
	       vicinity srfi-59 srfi-96 srfi-23
	       srfi-60)			;logical
	  (if (defined? *features*) *features* slib:features)))
(if (defined? *features*) (set! *features* slib:features))

(define eval
  (let ((@eval @eval)
	(@copy-tree @copy-tree))
    (lambda (x) (@eval (@copy-tree x)))))

(define (exec-self)
  (require 'i/o-extensions)
  (execv (execpath) (if *script*
			(cons (car (program-arguments))
			      (cons "\\"
				    (member *script* (program-arguments))))
			(program-arguments))))

(define (display-file file . port)
  (call-with-input-file file
    (lambda (inport)
      (do ((c (read-char inport) (read-char inport)))
	  ((eof-object? c))
	(apply write-char c port)))))
(define (terms)
  (display-file (in-vicinity (implementation-vicinity) "COPYING")))

;;; Read integer up to first non-digit
(define (read:try-number port . ic)
  (define chr0 (char->integer #\0))
  (let loop ((arg (and (not (null? ic)) (- (char->integer (car ic)) chr0))))
    (let ((c (peek-char port)))
      (cond ((eof-object? c) #f)
	    ((char-numeric? c)
	     (loop (+ (* 10 (or arg 0))
		      (- (char->integer (read-char port)) chr0))))
	    (else arg)))))

(define (read-array-type port)
  (define (bomb pc wid)
    (error 'array 'syntax? (symbol-append "#" rank "A" pc wid)))
  (case (char-downcase (peek-char port))
    ((#\:) (read-char port)
     (let ((typ (let loop ((arg '()))
		  (if (= 4 (length arg))
		      (string->symbol (list->string (reverse arg)))
		      (let ((c (read-char port)))
			(and (not (eof-object? c))
			     (loop (cons (char-downcase c) arg))))))))
       (define wid (and typ (not (eq? 'bool typ)) (read:try-number port)))
       (define (check-suffix chrs)
	 (define chr (read-char port))
	 (if (and (char? chr) (not (memv (char-downcase chr) chrs)))
	     (error 'array-type? (symbol-append ":" typ wid chr))))
       (define prot (assq typ '((floc (128 . +64.0i)
				      (64  . +64.0i)
				      (32  . +32.0i)
				      (16  . +32.0i))
				(flor (128 . 64.0)
				      (64  . 64.0)
				      (32  . 32.0)
				      (16  . 32.0))
				(fixz (64 . -64)
				      (32 . -32)
				      (16 . -16)
				      (8  . -8))
				(fixn (64 . 64)
				      (32 . 32)
				      (16 . 16)
				      (8  . 8))
				(char . #\a)
				(bool . #t))))
       (if prot (set! prot (cdr prot)))
       (cond ((pair? prot)
	      (set! prot (assv wid (cdr prot)))
	      (if (pair? prot) (set! prot (cdr prot)))
	      (if wid (check-suffix (if (and (inexact? prot) (real? prot))
					'(#\b #\d)
					'(#\b)))))
	     (prot)
	     (else (check-suffix '())))
       prot))
    ((#\\) (read-char port) #\a)
    ((#\t) (read-char port) #t)
    ((#\c #\r) (let* ((pc (read-char port)) (wid (read:try-number port)))
		 (case wid
		   ((64 32) (case pc
			      ((#\c) (* +i wid))
			      (else (exact->inexact wid))))
		   (else (bomb pc wid)))))
    ((#\s #\u) (let* ((pc (read-char port)) (wid (read:try-number port)))
		 (case (or wid (peek-char port))
		   ((32 16 8) (case pc
				((#\s) (- wid))
				(else wid)))
		   (else (bomb pc wid)))))
    (else #f)))

;;; We come into read:array with number or #f for RANK.
(define (read:array rank dims port)
  (define (make-it rank dims typ)
    (list->uniform-array (cond (rank)
                               ((null? dims) 1)
                               (else (length dims)))
                         typ
                         (read port)))
  (let loop ((dims dims))
    (define dim (read:try-number port))
    (if dim
	(loop (cons dim dims))
	(case (peek-char port)
	  ((#\*) (read-char port) (loop dims))
	  ((#\: #\\ #\t #\c #\r #\s #\u #\T #\C #\R #\S #\U)
           (make-it rank dims (read-array-type port)))
	  (else
           (make-it rank dims #f))))))

;;; read-macros valid for LOAD and READ.
(define (read:sharp c port reader) ; ignore reader
  (case c
    ;; Used in "implcat" and "slibcat"
    ((#\+) (if (slib:provided? (read port))
	       (read port)
	       (begin (read port) (if #f #f))))
    ;; Used in "implcat" and "slibcat"
    ((#\-) (if (slib:provided? (read port))
	       (begin (read port) (if #f #f))
	       (read port)))
    ((#\a #\A) (read:array #f '() port))
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
     (let* ((num (read:try-number port c))
	    (chr (peek-char port)))
       (case chr
	 ((#\a #\A) (read-char port)
	  (read:array num '() port))
	 ((#\*) (read-char port)
	  (read:array #f (list num) port))
	 (else
	  (read:array 1 (list num) port))
	 ;;(else (error 'sharp 'syntax? (symbol-append "#" num chr)))
	 )))
    (else (error "unknown # object" c))))

;;; read-macros valid only in LOAD.
(define (load:sharp c port reader) ;reader used only for #.
  (case c
    ((#\') (read port))
    ((#\.) (eval (reader port)))
    ((#\!) (let skip ((metarg? #f))
	     (let ((c (read-char port)))
	       (case c
		 ((#\newline) (if metarg? (skip #t)))
		 ((#\\) (skip #t))
		 ((#\!) (cond ((eqv? #\# (peek-char port))
			       (read-char port)
			       (if #f #f))
			      (else (skip metarg?))))
		 (else (if (char? c) (skip metarg?) c))))))
    ;; Make #; convert the rest of the line to a (comment ...) form.
    ;; "build.scm" uses this.
    ((#\;) (let skip-semi ()
	     (cond ((eqv? #\; (peek-char port))
		    (read-char port)
		    (skip-semi))
		   (else (require 'line-i/o)
			 `(comment ,(read-line port))))))
    ((#\?) (case (read port)
	     ((line) (port-line port))
	     ((column) (port-column port))
	     ((file) (port-filename port))
	     (else #f)))
    (else (read:sharp c port read))))

;;; We can assume TOK has at least 2 characters.
(define char:sharp
  (letrec ((numeric-1
            (lambda (tok radix)
              (numeric (substring tok 1 (string-length tok)) radix)))
           (numeric
            (lambda (tok radix)
              (cond ((string->number tok radix) => integer->char))))
           (compose
	    (lambda (modifier tok)
	      (and (char=? #\- (string-ref tok 1))
		   (if (= 3 (string-length tok))
		       (modifier (string-ref tok 2))
		       (let ((c (char:sharp
				 (substring tok 2 (string-length tok)))))
			 (and c (modifier c)))))))
	   (control
	    (lambda (c)
	      (and (char? c)
		   (if (eqv? c #\?)
		       (integer->char 127)
		       (integer->char (logand #o237 (char->integer c)))))))
	   (meta
	    (lambda (c)
	      (and (char? c)
		   (integer->char (logior 128 (char->integer c)))))))
    (lambda (tok)
      (case (string-ref tok 0)
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7) (numeric tok 8))
        ((#\O #\o) (numeric-1 tok 8))
        ((#\D #\d) (numeric-1 tok 10))
        ((#\X #\x) (numeric-1 tok 16))
	((#\C #\c) (compose control tok))
	((#\^) (and (= 2 (string-length tok)) (control (string-ref tok 1))))
	((#\M #\m) (compose meta tok))))))

;;;; Function used to accumulate comments before a definition.
(define comment
  (let ((*accumulated-comments* '()))
    (lambda args
      (cond ((null? args)
	     (let ((ans
		    (apply string-append
			   (map (lambda (comment)
				  (string-append (or comment "") "\n"))
				(reverse *accumulated-comments*)))))
	       (set! *accumulated-comments* '())
	       (if (equal? "" ans)
		   "no-comment"		;#f
		   (substring ans 0 (+ -1 (string-length ans))))))
	    (else (set! *accumulated-comments*
			(append (reverse args) *accumulated-comments*)))))))

(define : ':)				;for /bin/sh hack.
(define !#(if #f #f))			;for scsh hack.

;;;; Here are some Revised^2 Scheme functions:
(define 1+ (let ((+ +)) (lambda (n) (+ n 1))))
(define -1+ (let ((+ +)) (lambda (n) (+ n -1))))
(define 1- -1+)
(define <? <)
(define <=? <=)
(define =? =)
(define >? >)
(define >=? >=)
(define t #t)
(define nil #f)
(define identity cr)

(cond ((defined? defsyntax)
(defsyntax define-syntax (the-macro defsyntax)))
      (else
(define defsyntax define)
(define the-macro identity)))
(defsyntax sequence (the-macro begin))
(define copy-tree @copy-tree)

;;; VMS does something strange when output is sent to both
;;; CURRENT-OUTPUT-PORT and CURRENT-ERROR-PORT.
(case (software-type) ((vms) (set-current-error-port (current-output-port))))

;;; OPEN_READ, OPEN_WRITE, and OPEN_BOTH are used to request the proper
;;; mode to open files in.  MS-DOS does carriage return - newline
;;; translation if not opened in `b' mode.

(define open_read (case (software-type)
		    ((ms-dos windows atarist) 'rb)
		    (else 'r)))
(define open_write (case (software-type)
		     ((ms-dos windows) 'wbc)
		     ((atarist) 'wb)
		     (else 'w)))
(define open_both (case (software-type)
		    ((ms-dos windows) 'r+bc)
		    ((atarist) 'r+b)
		    (else 'r+)))
(define ((make-moder str) mode)
  (if (symbol? mode)
      (string->symbol (string-append (symbol->string mode) str))
      (string-append mode str)))
(define _ionbf (make-moder "0"))
(define _tracked (make-moder "?"))
(define _exclusive (make-moder "x"))

(define could-not-open #f)

(define (open-output-file str)
  (or (open-file str open_write)
      (and (procedure? could-not-open) (could-not-open) #f)
      (error "OPEN-OUTPUT-FILE couldn't open file " str)))
(define (open-input-file str)
  (or (open-file str open_read)
      (and (procedure? could-not-open) (could-not-open) #f)
      (error "OPEN-INPUT-FILE couldn't open file " str)))

(define (string-index str chr)
  (define len (string-length str))
  (do ((pos 0 (+ 1 pos)))
      ((or (>= pos len) (char=? chr (string-ref str pos)))
       (and (< pos len) pos))))

(if (not (defined? try-create-file))
(define (try-create-file str modes . perms)
  (if (symbol? modes) (set! modes (symbol->string modes)))
  (let ((idx (string-index modes #\x)))
    (cond ((slib:in-catalog? 'i/o-extensions)
	   (require 'i/o-extensions)
	   (apply try-create-file str modes perms))
	  ((not idx)
	   (warn "not exclusive modes?" modes str)
	   (try-open-file str modes))
	  (else (set! modes (string-append (substring modes 0 idx)
					   (substring modes (+ 1 idx)
						      (string-length modes))))
		(cond ((not (string-index modes #\w))
		       (warn 'try-create-file "not writing?" modes str)
		       (try-open-file str modes))
		      (else
		       (cond ((and (not (null? perms))
				   (not (eqv? #o666 (car perms))))
			      (warn "perms?" (car perms) str)))
		       (cond ((file-exists? str) #f)
			     (else (try-open-file str modes))))))))))

(if (not (defined? file-position))
(define (file-position . args) #f))
(if (not (defined? file-set-position))
(define file-set-position file-position))

(define close-input-port close-port)
(define close-output-port close-port)

(define (call-with-open-ports . ports)
  (define proc (car ports))
  (cond ((procedure? proc) (set! ports (cdr ports)))
	(else (set! ports (reverse ports))
	      (set! proc (car ports))
	      (set! ports (reverse (cdr ports)))))
  (let ((ans (apply proc ports)))
    (for-each close-port ports)
    ans))

(define (call-with-input-file str proc)
  (call-with-open-ports (open-input-file str) proc))

(define (call-with-output-file str proc)
  (call-with-open-ports (open-output-file str) proc))

(define (with-input-from-port port thunk)
  (dynamic-wind (lambda () (set! port (set-current-input-port port)))
		thunk
		(lambda () (set! port (set-current-input-port port)))))

(define (with-output-to-port port thunk)
  (dynamic-wind (lambda () (set! port (set-current-output-port port)))
		thunk
		(lambda () (set! port (set-current-output-port port)))))

(define (with-error-to-port port thunk)
  (dynamic-wind (lambda () (set! port (set-current-error-port port)))
		thunk
		(lambda () (set! port (set-current-error-port port)))))

(define (with-input-from-file file thunk)
  (let* ((nport (open-input-file file))
	 (ans (with-input-from-port nport thunk)))
    (close-port nport)
    ans))

(define (with-output-to-file file thunk)
  (let* ((nport (open-output-file file))
	 (ans (with-output-to-port nport thunk)))
    (close-port nport)
    ans))

(define (with-error-to-file file thunk)
  (let* ((nport (open-output-file file))
	 (ans (with-error-to-port nport thunk)))
    (close-port nport)
    ans))

(define (call-with-outputs thunk proc)
  (define stdout #f)
  (define stderr #f)
  (define status #f)
  (set! stdout
	(call-with-output-string
	 (lambda (stdout)
	   (set! stderr
		 (call-with-output-string
		  (lambda (stderr)
		    (call-with-current-continuation
		     (lambda (escape)
		       (dynamic-wind
			   (lambda ()
			     (set! status #f)
			     (set! stdout (set-current-output-port stdout))
			     (set! stderr (set-current-error-port stderr)))
			   (lambda () (set! status (list (thunk))))
			   (lambda ()
			     (set! stdout (set-current-output-port stdout))
			     (set! stderr (set-current-error-port stderr))
			     (if (not status) (escape #f))))))))))))
  (apply proc stdout stderr (or status '())))

(define browse-url
  (case (software-type)
    ((unix coherent plan9)
     (lambda (url)
       (define (try cmd end) (zero? (system (string-append cmd url end))))
       (or (try "netscape-remote -remote 'openURL(" ")'")
	   (try "netscape -remote 'openURL(" ")'")
	   (try "netscape '" "'&")
	   (try "netscape '" "'"))))
    (else
     (lambda (url)
       (slib:warn 'define (software-type) 'case 'of 'browse-url 'in
		  *load-pathname*)))))

(define (warn . args)
  (define cep (current-error-port))
  (if (defined? print-call-stack) (print-call-stack cep))
  (perror "WARN")
  (errno 0)
  (display "WARN:" cep)
  (for-each (lambda (x) (display #\space cep) (write x cep)) args)
  (newline cep)
  (force-output cep))

(define (error . args)
  (define cep (current-error-port))
  (if (defined? print-call-stack) (print-call-stack cep))
  (perror "ERROR")
  (errno 0)
  (display "ERROR:" cep)
  (for-each (lambda (x) (display #\space cep) (write x cep)) args)
  (newline cep)
  (force-output cep)
  (abort))

(define set-errno errno)
(define slib:exit quit)
(define exit quit)

(define (print . args)
  (define result #f)
  (for-each (lambda (x) (set! result x) (write x) (display #\space)) args)
  (newline)
  result)
(define (pprint . args)
  (define result #f)
  (for-each (lambda (x) (set! result x) (pretty-print x)) args)
  result)
(define (pp . args)
  (for-each pretty-print args)
  (if #f #f))

(if (not (defined? file-exists?))
(define (file-exists? str)
  (let ((port (open-file str open_read)))
    (errno 0)
    (and port (close-port port) #t))))
(define (file-readable? str)
  (let ((port (open-file str open_read)))
    (errno 0)
    (and port
	 (char-ready? port)
	 (do ((c (read-char port)
		 (and (char-ready? port) (read-char port)))
	      (i 0 (+ 1 i))
	      (l '() (cons c l)))
	     ((or (not c) (eof-object? c) (<= 2 i))
	      (if (null? l) #f (list->string (reverse l))))))))

(define difftime -)
(define offset-time +)

(if (not (defined? ed))
(define (ed . args)
  (system (apply string-append
		 (or (getenv "EDITOR") "ed")
		 (map (lambda (s) (string-append " " s)) args)))))

(if (not (defined? output-port-width))
(define (output-port-width . arg) 80))

(if (not (defined? output-port-height))
(define (output-port-height . arg) 24))

(if (not (defined? last-pair))
(define (last-pair l) (if (pair? (cdr l)) (last-pair (cdr l)) l)))

(define slib:error error)
(define slib:warn warn)
(define slib:tab #\tab)
(define slib:form-feed #\page)
(define slib:eval eval)

(define (make-exchanger . pair) (lambda (rep) (swap-car! pair rep)))

;;;; Load.
(define load:indent 0)
(define (load:pre op file)
  (define cep (current-error-port))
  (cond ((> (verbose) 1)
	 (display
	  (string-append ";" (make-string load:indent #\space)
			 (symbol->string op) "ing " file)
	  cep)
	 (set! load:indent (modulo (+ 2 load:indent) 16))
	 (newline cep)))
  (force-output cep))

(define (load:post op filesuf)
  (define cep (current-error-port))
  (errno 0)
  (cond ((> (verbose) 1)
	 (set! load:indent (modulo (+ -2 load:indent) 16))
	 (display (string-append ";" (make-string load:indent #\space)
				 "done " (symbol->string op) "ing " filesuf)
		  cep)
	 (newline cep)
	 (force-output cep))))

;;; Here for backward compatibility
(define scheme-file-suffix
  (case (software-type)
    ((NOSVE) (lambda () "_scm"))
    (else (lambda () ".scm"))))

(define (has-suffix? str suffix)
  (let ((sufl (string-length suffix))
	(sl (string-length str)))
    (and (> sl sufl)
	 (string=? (substring str (- sl sufl) sl) suffix))))

(define *load-reader* #f)
(define (scm:load file . libs)
  (define filesuf file)
  (define hss (has-suffix? file (scheme-file-suffix)))
  (load:pre 'load file)
  (or (and (defined? link:link) (not hss)
	   (or (let ((s2 (file-readable? file)))
		 (and s2 (not (equal? "#!" s2)) (apply link:link file libs)))
	       (and link:able-suffix
		    (let* ((fs (string-append file link:able-suffix))
			   (fs2 (file-readable? fs)))
		      (and fs2 (apply link:link fs libs) (set! filesuf fs) #t)
		      ))))
      (and (null? libs) (try-load file *load-reader*))
      ;;HERE is where the suffix gets specified
      (and (not hss) (errno 0)		; clean up error from TRY-LOAD above
	   (set! filesuf (string-append file (scheme-file-suffix)))
	   (try-load filesuf *load-reader*))
      (and (procedure? could-not-open) (could-not-open) #f)
      (begin (set! load:indent 0)
	     (error "LOAD couldn't find file " file)))
  (load:post 'load filesuf))
(define load scm:load)
(define slib:load load)

(define (scm:load-source file)
  (define sfs (scheme-file-suffix))
  (define filesuf file)
  (load:pre 'load file)
  (or (and (or (try-load file *load-reader*)
	       ;;HERE is where the suffix gets specified
	       (and (not (has-suffix? file sfs))
		    (begin (set! filesuf (string-append file sfs))
			   (try-load filesuf *load-reader*)))))
      (and (procedure? could-not-open) (could-not-open) #f)
      (error "LOAD couldn't find file " file))
  (load:post 'load filesuf))
(define slib:load-source scm:load-source)

;;; This is the vicinity where this file resides.
(define implementation-vicinity #f)

;;; (library-vicinity) should be defined to be the pathname of the
;;; directory where files of Scheme library functions reside.
(define library-vicinity #f)

;;; (home-vicinity) should return the vicinity of the user's HOME
;;; directory, the directory which typically contains files which
;;; customize a computer environment for a user.
(define home-vicinity #f)

(if (not (defined? getpw))
    (define read-line
      (if (defined? read-line)
	  read-line
	  (lambda port
	    (let* ((chr (apply read-char port)))
	      (if (eof-object? chr)
		  chr
		  (do ((chr chr (apply read-char port))
		       (clist '() (cons chr clist)))
		      ((or (eof-object? chr) (char=? #\newline chr))
		       (list->string (reverse clist))))))))))
(if (not (defined? getpw))
    (define string-index
      (if (defined? string-index)
	  string-index
	  (lambda (str chr)
	    (define len (string-length str))
	    (do ((pos 0 (+ 1 pos)))
		((or (>= pos len) (char=? chr (string-ref str pos)))
		 (and (< pos len) pos)))))))

(define (login->home-directory login)
  (cond ((defined? getpw)
	 (let ((pwvect (getpw login)))
	   (and pwvect (vector-ref pwvect 5))))
	((not (file-exists? "/etc/passwd")) #f)
	(else
	 (call-with-input-file "/etc/passwd"
	   (lambda (iprt)
	     (let tryline ()
	       (define line (read-line iprt))
	       (define (get-field)
		 (define idx (string-index line #\:))
		 (and idx
		      (let ((fld (substring line 0 idx)))
			(set! line (substring line (+ 1 idx)
					      (string-length line)))
			fld)))
	       (cond ((eof-object? line) #f)
		     ((string-index line #\:)
		      => (lambda (idx)
			   (define name (substring line 0 idx))
			   (cond ((equal? login name)
				  (do ((ans (get-field) (get-field))
				       (cnt 4 (+ -1 cnt)))
				      ((or (negative? cnt) (not ans)) ans)))
				 (else (tryline))))))))))))

(define (getlogin) (or (getenv "USER") (getenv "LOGNAME")))

;;; If the environment variable SCHEME_LIBRARY_PATH is undefined, use
;;; (implementation-vicinity) as (library-vicinity).  "require.scm",
;;; the first file loaded from (library-vicinity), can redirect it.
(define (set-vicinities! init-file)
  (set! implementation-vicinity
	(let ((vic (substring
		    init-file
		    0
		    (- (string-length init-file)
		       (string-length "Init.scm")
		       (string-length (scheme-implementation-version))))))
	  (lambda () vic)))
  (let ((library-path (getenv "SCHEME_LIBRARY_PATH")))
    (if library-path
	(set! library-vicinity (lambda () library-path))
	(let ((filename (in-vicinity (implementation-vicinity) "require.scm")))
	  (or (try-load filename)
	      (try-load (in-vicinity (implementation-vicinity) "requires.scm"))
	      (error "Can't load" filename))
	  (if (not library-vicinity) (error "Can't find library-vicinity")))))
  (set! home-vicinity
	(let ((home (getenv "HOME")))
	  (and (not home) login->home-directory
	       (let ((login (getlogin)))
		 (and login (set! home (login->home-directory login)))))
	  (and home
	       (case (software-type)
		 ((unix coherent plan9 ms-dos) ;V7 unix has a / on HOME
		  (if (not
		       (eqv? #\/ (string-ref home (+ -1 (string-length home)))))
		      (set! home (string-append home "/"))))))
	  (lambda () home))))
;;; SET-VICINITIES! is also called from BOOT-TAIL
(set-vicinities! *load-pathname*)

;;;; Initialize SLIB
(load (in-vicinity (library-vicinity) "require"))

;;; This enables line-numbering for SLIB loads.
(define *slib-load-reader* (and (defined? read-numbered) read-numbered))

;;; DO NOT MOVE!  SLIB:LOAD-SOURCE and SLIB:LOAD must be defined after
;;; "require.scm" is loaded.
(define (slib:load-source file)
  (fluid-let ((*load-reader* *slib-load-reader*))
    (scm:load-source file)))
(define (slib:load file . libs)
  (fluid-let ((*load-reader* *slib-load-reader*))
    (apply scm:load file libs)))

;;; Legacy grease
(if (not (defined? slib:in-catalog?))
    (define slib:in-catalog? require:feature->path))

;;; Dynamic link-loading
(cond ((or (defined? dyn:link)
	   (defined? vms:dynamic-link-call))
       (load (in-vicinity (implementation-vicinity) "Link"))))

;;; Redefine to ease transition from *features* to slib:features.
(define (provide feature)
  (cond ((not (memq feature slib:features))
	 (set! slib:features (cons feature slib:features))
	 (if (defined? *features*) (set! *features* slib:features)))))

(cond ((defined? link:link)
(define (slib:load-compiled . args)
  (cond ((symbol? (car args))
	 (require (car args))
	 (apply slib:load-compiled (cdr args)))
	((apply link:link args)
	 (if (defined? *features*) (set! slib:features *features*)))
	(else (error "Couldn't link files " args))))
(provide 'compiled)))

;;; Complete the function set for feature STRING-CASE.
(cond
 ((defined? string-upcase!)
(define (string-upcase str) (string-upcase! (string-copy str)))
(define (string-downcase str) (string-downcase! (string-copy str)))
(define (string-capitalize str) (string-capitalize! (string-copy str)))
(define string-ci->symbol
  (let ((s2cis (if (equal? "x" (symbol->string 'x))
		   string-downcase string-upcase)))
    (lambda (str) (string->symbol (s2cis str)))))
(define symbol-append
  (let ((s2cis (if (equal? "x" (symbol->string 'x))
		   string-downcase string-upcase)))
    (lambda args
      (string->symbol
       (apply string-append
	      (map
	       (lambda (obj)
		 (cond ((char? obj) (string obj))
		       ((string? obj) (s2cis obj))
		       ((number? obj) (s2cis (number->string obj)))
		       ((symbol? obj) (symbol->string obj))
		       ((not obj) "")
		       (else (error 'wrong-type-to 'symbol-append obj))))
	       args))))))
(define (StudlyCapsExpand nstr . delimitr)
  (set! delimitr
	(cond ((null? delimitr) "-")
	      ((char? (car delimitr)) (string (car delimitr)))
	      (else (car delimitr))))
  (do ((idx (+ -1 (string-length nstr)) (+ -1 idx)))
      ((> 1 idx) nstr)
    (cond ((and (> idx 1)
		(char-upper-case? (string-ref nstr (+ -1 idx)))
		(char-lower-case? (string-ref nstr idx)))
	   (set! nstr
		 (string-append (substring nstr 0 (+ -1 idx))
				delimitr
				(substring nstr (+ -1 idx)
					   (string-length nstr)))))
	  ((and (char-lower-case? (string-ref nstr (+ -1 idx)))
		(char-upper-case? (string-ref nstr idx)))
	   (set! nstr
		 (string-append (substring nstr 0 idx)
				delimitr
				(substring nstr idx
					   (string-length nstr))))))))
(provide 'string-case)))

;;;; Bit order and lamination

;;(define (logical:ones deg) (lognot (ash -1 deg)))

;;; New with SRFI-60
(define (rotate-bit-field n count start end)
  (define width (- end start))
  (set! count (modulo count width))
  (let ((mask (lognot (ash -1 width))))
    (define azn (logand mask (arithmetic-shift n (- start))))
    (logior (arithmetic-shift
	     (logior (logand mask (arithmetic-shift azn count))
		     (arithmetic-shift azn (- count width)))
	     start)
	    (logand (lognot (ash mask start)) n))))
;;; Legacy
;;(define (logical:rotate k count len) (rotate-bit-field k count 0 len))

(define (log2-binary-factors n)
  (+ -1 (integer-length (logand n (- n)))))

(define (bit-reverse k n)
  (do ((m (if (negative? n) (lognot n) n) (arithmetic-shift m -1))
       (k (+ -1 k) (+ -1 k))
       (rvs 0 (logior (arithmetic-shift rvs 1) (logand 1 m))))
      ((negative? k) (if (negative? n) (lognot rvs) rvs))))
(define (reverse-bit-field n start end)
  (define width (- end start))
  (let ((mask (lognot (ash -1 width))))
    (define zn (logand mask (arithmetic-shift n (- start))))
    (logior (arithmetic-shift (bit-reverse width zn) start)
	    (logand (lognot (ash mask start)) n))))

(define (integer->list k . len)
  (if (negative? k) (slib:error 'integer->list 'negative? k))
  (if (null? len)
      (do ((k k (arithmetic-shift k -1))
	   (lst '() (cons (odd? k) lst)))
	  ((<= k 0) lst))
      (do ((idx (+ -1 (car len)) (+ -1 idx))
	   (k k (arithmetic-shift k -1))
	   (lst '() (cons (odd? k) lst)))
	  ((negative? idx) lst))))

(define (list->integer bools)
  (do ((bs bools (cdr bs))
       (acc 0 (+ acc acc (if (car bs) 1 0))))
      ((null? bs) acc)))
(define (booleans->integer . bools)
  (list->integer bools))

;;;; SRFI-60 aliases
(define arithmetic-shift ash)
(define bitwise-ior logior)
(define bitwise-xor logxor)
(define bitwise-and logand)
(define bitwise-not lognot)
;;(define bit-count logcount)		;Aliases bit-vector function
;;BITWISE-BIT-COUNT returns negative count for negative inputs.
(define bit-set?   logbit?)
(define any-bits-set? logtest)
(define first-set-bit log2-binary-factors)
(define bitwise-merge bitwise-if)

(define @case-aux
  (let ((integer-jump-table 1)
	(char-jump-table 2))
    (lambda (keys actions else-action)
      (let ((n (length keys)))
	(define (every-key pred)
	  (let test ((keys keys))
	    (or (null? keys)
		(and (pred (car keys)) (test (cdr keys))))))
	(define (jump-table keys)
	  (let ((minkey (apply min keys))
		(maxkey (apply max keys)))
	    (and (< (- maxkey minkey) (* 4 n))
		 (let ((actv (make-vector
			      (+ 2 (- maxkey minkey)) else-action)))
		   (for-each
		    (lambda (key action)
		      (vector-set! actv (+ 1 (- key minkey)) action))
		    keys actions)
		   (list integer-jump-table minkey actv)))))
	(cond ((< n 5) #f)
	      ((every-key integer?)
	       (jump-table keys))
	      ((every-key char?)
	       (let* ((int-keys (map char->integer keys)))
		 (cond ((jump-table int-keys) =>
			(lambda (x)
			  (cons char-jump-table
				(cons (integer->char (cadr x))
				      (cddr x)))))
		       (else #f)))))))))

;;;defmacro from dorai@cs.rice.edu (heavily hacked by jaffer):
(define *defmacros* '())
(define (defmacro? m) (and (assq m *defmacros*) #t))

(define defmacro:transformer
  (lambda (f)
    (procedure->memoizing-macro
     (lambda (exp env)
       (@copy-tree (apply f (remove-line-numbers! (cdr exp))))))))

(define defmacro:get-destructuring-bind-pairs
  (lambda (s e)
    (let loop ((s s) (e e) (r '()))
      (cond ((pair? s)
	     (loop (car s) `(car ,e)
		   (loop (cdr s) `(cdr ,e) r)))
	    ((null? s) r)
	    ((symbol? s) (cons `(,s ,e) r))
	    (else (error 'destructuring-bind "illegal syntax"))))))

(defsyntax destructuring-bind
  (let ((destructuring-bind-transformer
	 (lambda (s x . ff)
	   (let ((tmp (gentemp)))
	     `(let ((,tmp ,x))
		(let ,(defmacro:get-destructuring-bind-pairs s tmp)
		  ,@ff))))))
    (set! *defmacros*
	  (acons 'destructuring-bind
		 destructuring-bind-transformer *defmacros*))
    (defmacro:transformer destructuring-bind-transformer)))

(defsyntax defmacro:simple-defmacro
  (let ((defmacro-transformer
	  (lambda (name parms . body)
	    `(defsyntax ,name
	       (let ((transformer (lambda ,parms ,@body)))
		 (set! *defmacros* (acons ',name transformer *defmacros*))
		 (defmacro:transformer transformer))))))
    (set! *defmacros* (acons 'defmacro defmacro-transformer *defmacros*))
    (defmacro:transformer defmacro-transformer)))

(defmacro:simple-defmacro defmacro (name . body)
  (define (expn name pattern body)
    (let ((args (gentemp)))
      `(defmacro:simple-defmacro ,name ,args
	 (destructuring-bind ,pattern ,args ,@body))))
  (if (pair? name)
      (expn (car name) (cdr name) body)
      (expn name (car body) (cdr body))))

(define (macroexpand-1 e)
  (if (pair? e) (let ((a (car e)))
		  (cond ((symbol? a) (set! a (assq a *defmacros*))
			 (if a (apply (cdr a) (cdr e)) e))
			(else e)))
      e))

(define (macroexpand e)
  (if (pair? e) (let ((a (car e)))
		  (cond ((symbol? a)
			 (set! a (assq a *defmacros*))
			 (if a (macroexpand (apply (cdr a) (cdr e))) e))
			(else e)))
      e))

(define gentemp
  (let ((*gensym-counter* -1))
    (lambda ()
      (set! *gensym-counter* (+ *gensym-counter* 1))
      (string->symbol
       (string-append "scm:G" (number->string *gensym-counter*))))))

(define defmacro:eval slib:eval)
(define defmacro:load load)
;; slib:eval-load definition moved to "slib/require.scm"

;;;; Autoloads for SLIB procedures.

(define (trace-all . args) (require 'debug) (apply trace-all args))
(define (track-all . args) (require 'debug) (apply track-all args))
(define (stack-all . args) (require 'debug) (apply stack-all args))
(define (break-all . args) (require 'debug) (apply break-all args))
(define (pretty-print . args) (require 'pretty-print) (apply pretty-print args))

;;; (require 'transcript) would get us SLIB transcript -- not what we want.
(define (transcript-on arg)
  (load (in-vicinity (implementation-vicinity)
		     (string-append "Tscript" (scheme-file-suffix))))
  (transcript-on arg))
(define (transcript-off)
  (error "No transcript active"))

;;;; Macros.

;;; Trace gets re-defmacroed when tracef autoloads.
(defmacro trace x (cond ((null? x) '()) (else (require 'trace) `(trace ,@x))))
(defmacro track x (cond ((null? x) '()) (else (require 'track) `(track ,@x))))
(defmacro stack x (cond ((null? x) '()) (else (require 'stack) `(stack ,@x))))
(defmacro break x (cond ((null? x) '()) (else (require 'break) `(break ,@x))))

(defmacro defvar (var val)
  `(if (not (defined? ,var)) (define ,var ,val)))
(defmacro defconst (name value)
  (cond ((list? name) `(defconst ,(car name) (lambda ,(cdr name) ,value)))
	(else (cond ((not (slib:eval `(defined? ,name))))
		    ((and (symbol? name) (equal? (slib:eval value)
						 (slib:eval name))))
		    (else (error 'trying-to-defconst name
				 'to-different-value value)))
	      `(define ,name ,value))))
(defmacro qase (key . clauses)
  `(case ,key
     ,@(map (lambda (clause)
	      (if (list? (car clause))
		  (cons (apply
			 append
			 (map (lambda (elt)
				(case elt
				  ((unquote) '(unquote))
				  ((unquote-splicing) '(unquote-splicing))
				  (else
				   (eval (list 'quasiquote (list elt))))))
			      (car clause)))
			(cdr clause))
		  clause))
	    clauses)))
(defmacro (casev . args) `(qase ,@args))

(defmacro fluid-let (clauses . body)
  (let ((ids (map car clauses))
	(temp (gentemp))
	(swap (gentemp)))
    `(let* ((,temp (list ,@(map cadr clauses)))
	    (,swap (lambda () (set! ,temp (set! ,ids ,temp)))))
       (dynamic-wind
	   ,swap
	   (lambda () ,@body)
	   ,swap))))

(define (scm:print-binding sexp frame)
  (cond ((not (null? (cdr sexp)))
	 (display "In")
	 (for-each (lambda (exp) (display #\space) (display exp)) (cdr sexp))
	 (display ": ")))
  (do ((vars (car frame) (cdr vars))
       (vals (cdr frame) (cdr vals)))
      ((not (pair? vars))
       (cond ((not (null? vars)) (write vars)
	      (display " := ") (write (car vals))))
       (newline))
    (write (car vars)) (display " = ") (write (car vals)) (display "; ")))

(define print-args
  (procedure->memoizing-macro
   (lambda (sexp env)
     (define (fix-list frm)
       (cond ((pair? frm) (cons (car frm) (fix-list (cdr frm))))
	     ((null? frm) '())
	     ((symbol? frm) (list frm))
	     (else '())))
     (define frm (car env))
     `(scm:print-binding
       ',sexp
       ,(cond ((symbol? frm) `(list ',frm ,frm))
	      ((list? frm) `(list ',frm ,@frm))
	      ((pair? frm)
	       (let ((jlp (fix-list frm)))
		 `(list ',(if (symbol? (cdr (last-pair frm))) frm jlp)
			,@jlp))))))))

(cond
 ((defined? stack-trace)

;;#+breakpoint-error;; remove line to enable breakpointing on calls to ERROR
(define error
  (letrec ((oerror error)
           (nerror
            (lambda args
              (dynamic-wind
                  (lambda () (set! error oerror))
                  (lambda ()
                    (define cep (current-error-port))
                    (if (defined? print-call-stack)
                        (print-call-stack cep))
                    (perror "ERROR")
                    (errno 0)
                    (display "ERROR: " cep)
                    (if (not (null? args))
                        (begin (display (car args) cep)
                               (for-each (lambda (x) (display #\space cep) (write x cep))
                                         (cdr args))))
                    (newline cep)
                    (cond ((stack-trace) (newline cep)))
                    (display " * Breakpoint established: (continue <val>) to return." cep)
                    (newline cep) (force-output cep)
                    (require 'debug) (apply breakpoint args))
                  (lambda () (set! error nerror))))))
    nerror))

(define (user-interrupt . args)
  (define cep (current-error-port))
  (newline cep)
  (if (defined? print-call-stack)
      (print-call-stack cep))
  (display "ERROR: user interrupt" cep)
  (newline cep)
  (cond ((stack-trace) (newline cep)))
  (display " * Breakpoint established: (continue <val>) to return." cep)
  (newline cep) (force-output cep)
  (require 'debug) (apply breakpoint args))
  ))

(cond ((and (inexact? (string->number "0.0")) (not (defined? exp)))
       (or (and (defined? usr:lib)
		(usr:lib "m")
		(load (in-vicinity (implementation-vicinity) "Transcen")
		      (usr:lib "m")))
	   (load (in-vicinity (implementation-vicinity) "Transcen"))))
      (else
       (define (infinite? z) #f)
       (define finite? number?)
       (define inexact->exact identity)
       (define exact->inexact identity)
       (define round->exact identity)
       (define floor->exact identity)
       (define ceiling->exact identity)
       (define truncate->exact identity)
       (define expt integer-expt)))

(define (numerator q)
  (if (not (rational? q)) (error 'numerator q))
  (do ((num q (* 2 num)))
      ((integer? num) num)))

(define (denominator q)
  (if (not (rational? q)) (error 'denominator q))
  (do ((num q (* 2 num))
       (den (- q q -1) (* 2 den)))
      ((integer? num) den)))

;;;; http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/lisp/code/math/isqrt/isqrt.txt
;;; Akira Kurihara
;;; School of Mathematics
;;; Japan Women's University
;@
(define integer-sqrt
  (let ((table '#(0
		  1 1 1
		  2 2 2 2 2
		  3 3 3 3 3 3 3
		  4 4 4 4 4 4 4 4 4))
	(square (lambda (x) (* x x))))
    (lambda (n)
      (define (isqrt n)
	(if (> n 24)
	    (let* ((len/4 (quotient (- (integer-length n) 1) 4))
		   (top (isqrt (ash n (* -2 len/4))))
		   (init (ash top len/4))
		   (q (quotient n init))
		   (iter (quotient (+ init q) 2)))
	      (cond ((odd? q) iter)
		    ((< (remainder n init) (square (- iter init))) (- iter 1))
		    (else iter)))
	    (vector-ref table n)))
      (if (and (exact? n) (integer? n) (not (negative? n)))
	  (isqrt n)
	  (slib:error 'integer-sqrt n)))))

(if (defined? array?)
(begin

(define (array-null? array)
  (zero? (apply * (map (lambda (bnd) (- 1 (apply - bnd)))
		       (array-shape array)))))
(define (create-array prot . args)
  (if (array-null? prot)
      (dimensions->uniform-array args (array-prototype prot))
      (dimensions->uniform-array args (array-prototype prot)
				 (apply array-ref prot
					(map car (array-shape prot))))))
(define make-array create-array)
(define (list->array rank proto lst)
  (list->uniform-array rank (array-prototype proto) lst))
(define (vector->array vect prototype . dimensions)
  (define vdx (vector-length vect))
  (if (not (eqv? vdx (apply * dimensions)))
      (slib:error 'vector->array vdx '<> (cons '* dimensions)))
  (let ((ra (apply make-array prototype dimensions)))
    (define (v2ra dims idxs)
      (cond ((null? dims)
	     (set! vdx (+ -1 vdx))
	     (apply array-set! ra (vector-ref vect vdx) (reverse idxs)))
	    (else
	     (do ((idx (+ -1 (car dims)) (+ -1 idx)))
		 ((negative? idx) vect)
	       (v2ra (cdr dims) (cons idx idxs))))))
    (v2ra dimensions '())
    ra))
(define (array->vector ra)
  (define dims (array-dimensions ra))
  (let* ((vdx (apply * dims))
	 (vect (make-vector vdx)))
    (define (ra2v dims idxs)
      (if (null? dims)
	  (let ((val (apply array-ref ra (reverse idxs))))
	    (set! vdx (+ -1 vdx))
	    (vector-set! vect vdx val)
	    vect)
	  (do ((idx (+ -1 (car dims)) (+ -1 idx)))
	      ((negative? idx) vect)
	    (ra2v (cdr dims) (cons idx idxs)))))
    (ra2v dims '())))
(define (make-uniform-wrapper prot)
  (if (string? prot) (set! prot (string->number prot)))
  (if prot
      (lambda opt (if (null? opt)
		      (list->uniform-array 1 prot '())
		      (list->uniform-array 0 prot (car opt))))
      vector))
(define Ac64 (make-uniform-wrapper "+64i"))
(define Ac32 (make-uniform-wrapper "+32i"))
(define Ar64 (make-uniform-wrapper "64."))
(define Ar32 (make-uniform-wrapper "32."))
(define As64 (make-uniform-wrapper -64))
(define As32 (make-uniform-wrapper -32))
(define As16 (make-uniform-wrapper -16))
(define As8  (make-uniform-wrapper -8))
(define Au64 (make-uniform-wrapper  64))
(define Au32 (make-uniform-wrapper  32))
(define Au16 (make-uniform-wrapper  16))
(define Au8  (make-uniform-wrapper  8))
(define At1  (make-uniform-wrapper  #t))

;;; New SRFI-58 names
;; flonums
(define A:floC128b Ac64)
(define A:floC64b Ac64)
(define A:floC32b Ac32)
(define A:floC16b Ac32)
(define A:floR128b Ar64)
(define A:floR64b Ar64)
(define A:floR32b Ar32)
(define A:floR16b Ar32)
;; decimal flonums
(define A:floQ128d Ar64)
(define A:floQ64d Ar64)
(define A:floQ32d Ar32)
;; fixnums
(define A:fixZ64b As64)
(define A:fixZ32b As32)
(define A:fixZ16b As16)
(define A:fixZ8b  As8)
(define A:fixN64b Au64)
(define A:fixN32b Au32)
(define A:fixN16b Au16)
(define A:fixN8b  Au8)
(define A:bool    At1)

(define (array-shape a)
  (let ((dims (array-dimensions a)))
    (if (pair? dims)
	(map (lambda (ind) (if (number? ind) (list 0 (+ -1 ind)) ind))
	     dims)
	dims)))
(define array=? equal?)
(provide 'srfi-47)
(provide 'srfi-58)
(provide 'srfi-63)
))

(if (defined? bigdbl:powers-of-5)
    (do ((i 0 (+ i 1))
	 (acc 1 (* acc 5)))
	((>= i (vector-length bigdbl:powers-of-5)))
      (vector-set! bigdbl:powers-of-5 i acc)))

(define (alarm-interrupt) (alarm 0))
(if (defined? setitimer)
    (begin
      (define profile-alarm #f)
      (define (profile-alarm-interrupt) (profile-alarm 0))
      (define virtual-alarm #f)
      (define (virtual-alarm-interrupt) (virtual-alarm 0))
      (define milli-alarm #f)
      (let ((make-alarm
	     (lambda (sym)
	       (and (setitimer sym 0 0)	;DJGPP supports only REAL and PROFILE
		    (lambda (value . interval)
		      (cadr
		       (setitimer sym value
				  (if (pair? interval) (car interval) 0))))))))
	(set! profile-alarm (make-alarm 'profile))
	(set! virtual-alarm (make-alarm 'virtual))
	(set! milli-alarm (make-alarm 'real)))))

;;;; Initialize statically linked add-ons
(cond ((defined? scm_init_extensions)
       (scm_init_extensions)
       (if (defined? *features*) (set! slib:features *features*))
       (set! scm_init_extensions #f)))

;;; Use *argv* instead of (program-arguments), to allow option
;;; processing to be done on it.  "ScmInit.scm" must
;;;	(set! *argv* (program-arguments))
;;; if it wants to alter the arguments which BOOT-TAIL processes.
(define *argv* #f)

(if (not (defined? *syntax-rules*))
    (define *syntax-rules* #f))
(if (not (defined? *interactive*))
    (define *interactive* #f))

(define (boot-tail dumped?)
  (cond ((not *argv*)
	 (set! *argv* (program-arguments))
	 (cond (dumped?
		(set-vicinities! dumped?)
		(verbose (if (and (isatty? (current-input-port))
				  (isatty? (current-output-port)))
			     (if (<= (length *argv*) 1) 2 1)
			     0))))
	 (cond ((provided? 'getopt)
		(set! *optind* 1)
		(set! *optarg* #f)))))

;;; This loads the user's initialization file, or files named in
;;; program arguments.
  (or *script*
      (eq? (software-type) 'THINKC)
      (member "-no-init-file" (program-arguments))
      (member "--no-init-file" (program-arguments))
      (try-load (in-vicinity (or (home-vicinity) (user-vicinity))
			     (string-append "ScmInit") (scheme-file-suffix))
		*load-reader*)
      (errno 0))

  ;; Include line numbers in loaded code.
  (if (defined? read-numbered)
      (set! *load-reader* read-numbered))

  (cond
   ((and (> (length *argv*) 1) (char=? #\- (string-ref (cadr *argv*) 0)))
    (require 'getopt)
;;; (else
;;;  (define *optind* 1)
;;;  (define getopt:opt #f)
;;;  (define (getopt optstring) #f))

    (let* ((simple-opts "muvqibs")
	   (arg-opts '("a kbytes" "-version" "-help"
		       "-no-symbol-case-fold"
		       "no-init-file" "-no-init-file" "p number"
		       "h feature" "r feature" "d filename"
		       "f filename" "l filename"
		       "c string" "e string" "o filename"))
	   (opts (apply string-append ":" simple-opts
			(map (lambda (o)
			       (string-append (string (string-ref o 0)) ":"))
			     arg-opts)))
	   (didsomething #f)
	   (moreopts #t)
	   (exe-name (symbol->string (scheme-implementation-type)))
	   (up-name (apply string (map char-upcase (string->list exe-name)))))

      (define (do-thunk thunk)
	(if *interactive*
	    (thunk)
	    (let ((complete #f))
	      (dynamic-wind
		  (lambda () #f)
		  (lambda ()
		    (thunk)
		    (set! complete #t))
		  (lambda ()
		    (if (not complete) (close-port (current-input-port))))))))

      (define (do-string-arg)
	(require 'string-port)
	(do-thunk
	 (lambda ()
	   ((if *syntax-rules* macro:eval eval)
	    (call-with-input-string
		(string-append "(begin " *optarg* ")")
	      read))))
	(set! didsomething #t))

      (define (do-load file)
	(do-thunk
	 (lambda ()
	   (cond (*syntax-rules* (require 'macro) (macro:load file))
		 (else (load file)))))
	(set! didsomething #t))

      (define (usage preopt opt postopt success?)
	(define cep (if success? (current-output-port) (current-error-port)))
	(define indent (make-string 6 #\space))
	(define i 3)
	(cond ((char? opt) (set! opt (string opt)))
	      ;;((symbol? opt) (set! opt (symbol->string opt)))
	      )
	(display (string-append preopt opt postopt) cep)
	(newline cep)
	(display (string-append "Usage: "
				exe-name
				" [-a kbytes] [-" simple-opts "]") cep)
	(for-each
	 (lambda (o)
	   (display (string-append " [-" o "]") cep)
	   (set! i (+ 1 i))
	   (cond ((zero? (modulo i 5)) (newline cep) (display indent cep))))
	 (cdr arg-opts))
	(display " [-- | -s | -] [file] [args...]" cep) (newline cep)
	(if success? (display success? cep) (quit #f)))

      ;; -a int => ignore (handled by scm_init_from_argv)
      ;; -c str => (eval str)
      ;; -e str => (eval str)
      ;; -d str => (require 'databases) (open-database str)
      ;; -f str => (load str)
      ;; -l str => (load str)
      ;; -r sym => (require sym)
      ;; -h sym => (provide sym)
      ;; -o str => (dump str)
      ;; -p int => (verbose int)
      ;; -m     => (set! *syntax-rules* #t)
      ;; -u     => (set! *syntax-rules* #f)
      ;; -v     => (verbose 3)
      ;; -q     => (verbose 0)
      ;; -i     => (set! *interactive* #t)
      ;; -b     => (set! *interactive* #f)
      ;; -s     => set argv, don't execute first one
      ;; --no-symbol-case-fold => symbols preserve character case
      ;; -no-init-file => don't load init file
      ;; --no-init-file => don't load init file
      ;; --help => print and exit
      ;; --version => print and exit
      ;; --     => last option

      (let loop ((option (getopt-- opts)))
	(case option
	  ((#\a)
	   (cond ((> *optind* 3)
		  (usage "scm: option `-" getopt:opt "' must be first" #f))
		 ((or (not (exact? (string->number *optarg*)))
		      (not (<= 1 (string->number *optarg*) 10000)))
		  ;;	This size limit should match scm.c ^^
		  (usage "scm: option `-" getopt:opt
			 (string-append *optarg* "' unreasonable") #f))))
	  ((#\e #\c) (do-string-arg))	;sh-like
	  ((#\f #\l) (do-load *optarg*)) ;(set-car! *argv* *optarg*)
	  ((#\d) (require 'databases)
	   (open-database *optarg*))
	  ((#\o) (require 'dump)
	   (if (< *optind* (length *argv*))
	       (dump *optarg* #t)
	       (dump *optarg*)))
	  ((#\r) (do-thunk (lambda ()
			     (if (and (= 1 (string-length *optarg*))
				      (char-numeric? (string-ref *optarg* 0)))
				 (case (string-ref *optarg* 0)
				   ((#\2) (require 'r2rs))
				   ((#\3) (require 'r3rs))
				   ((#\4) (require 'r4rs))
				   ((#\5) (require 'r5rs)
				    (set! *syntax-rules* #t))
				   (else (require (string->symbol *optarg*))))
				 (require (string->symbol *optarg*))))))
	  ((#\h) (do-thunk (lambda () (provide (string->symbol *optarg*)))))
	  ((#\p) (verbose (string->number *optarg*)))
	  ((#\q) (verbose 0))
	  ((#\v) (verbose 3))
	  ((#\i) (set! *interactive* #t) ;sh-like
	   (verbose (max 2 (verbose))))
	  ((#\b) (set! didsomething #t)
	   (set! *interactive* #f))
	  ((#\s) (set! moreopts #f)	;sh-like
	   (set! didsomething #t)
	   (set! *interactive* #t))
	  ((#\m) (set! *syntax-rules* #t))
	  ((#\u) (set! *syntax-rules* #f))
	  ((#\n) (if (not (string=? "o-init-file" *optarg*))
		     (usage "scm: unrecognized option `-n" *optarg* "'" #f)))
	  ((#\:) (usage "scm: option `-" getopt:opt "' requires an argument" #f))
	  ((#\?) (usage "scm: unrecognized option `-" getopt:opt "'" #f))
	  ((#f) (set! moreopts #f)	;sh-like
	   (cond ((and (< *optind* (length *argv*))
		       (string=? "-" (list-ref *argv* *optind*)))
		  (set! *optind* (+ 1 *optind*)))))
	  (else
	   (or (cond ((not (string? option)) #f)
		     ((string-ci=? "no-init-file" option))
		     ((string-ci=? "no-symbol-case-fold" option))
		     ((string-ci=? "version" option)
		      (display
		       (string-append exe-name " "
				      (scheme-implementation-version)
				      "
Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996 Free Software Foundation, Inc.
"
				      up-name
				      " may be distributed under the terms of"
				      " the GNU General Public Licence;
certain other uses are permitted as well."
				      " For details, see the file `COPYING',
which is included in the "
				      up-name " distribution.
There is no warranty, to the extent permitted by law.
"
				      ))
		      (cond ((execpath) =>
			     (lambda (path)
			       (display " This executable was loaded from ")
			       (write path)
			       (newline))))
		      (quit #t))
		     ((string-ci=? "help" option)
		      (usage "This is "
			     up-name
			     ", a Scheme interpreter."
			     (let ((sihp (scheme-implementation-home-page)))
			       (if sihp
				   (string-append "Latest info: " sihp "
")
				   "")))
		      (quit #t))
		     (else #f))
	       (usage "scm: unknown option `--" option "'" #f))))

	(cond ((and moreopts (< *optind* (length *argv*)))
	       (loop (getopt-- opts)))
	      ((< *optind* (length *argv*)) ;No more opts
	       (set! *argv* (list-tail *argv* *optind*))
	       (set! *optind* 1)
	       (cond ((and (not didsomething) *script*)
		      (do-load *script*)
		      (set! *optind* (+ 1 *optind*))))
	       (cond ((and (> (verbose) 2)
			   (not (= (+ -1 *optind*) (length *argv*))))
		      (display "scm: extra command arguments unused:"
			       (current-error-port))
		      (for-each (lambda (x) (display (string-append " " x)
						     (current-error-port)))
				(list-tail *argv* (+ -1 *optind*)))
		      (newline (current-error-port)))))
	      ((and (not didsomething) (= *optind* (length *argv*)))
	       (set! *interactive* #t)))))

    (cond ((not *interactive*) (quit))
	  ((and *syntax-rules* (not (provided? 'macro)))
	   (require 'repl)
	   (require 'macro)
	   (let* ((oquit quit))
	     (set! quit (lambda () (repl:quit)))
	     (set! exit quit)
	     (repl:top-level macro:eval)
	     (oquit))))
    ;;otherwise, fall into natural SCM repl.
    )
   (else (errno 0)
	 (set! *interactive* #t)
	 (for-each load (cdr (program-arguments))))))
