#! /bin/sh
:;exec scm -e"(set! *script* \"$0\")" -f$0 "$@"
;;;; "compile.scm", Compile C ==> Scheme ==> object-file.
;; Copyright (C) 1992-2002 Free Software Foundation, Inc.
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

(require-if 'compiling 'hobbit)
(require-if 'compiling 'filename)
(require-if 'compiling 'build)

(define (compile.scm args)
  (cond ((and (<= 1 (length args))
	      (not (eqv? #\- (string-ref (car args) 0))))
	 (apply compile-file args))
	(else (compile.usage))))

(define (compile.usage)
  (display "\
\
Usage: compile.scm FILE1.scm FILE2.scm ...
\
  Compiles Scheme FILE1.scm FILE2.scm ... to an object file named
  FILE1<object-suffix>, where <object-suffix> is the object file suffix
  for your computer (for instance, `.o').  FILE1.scm must be in the
  current directory; FILE2.scm ... can be in other directories.

http://people.csail.mit.edu/jaffer/SCM
"
	   (current-error-port))
  #f)

;;; This unusual autoload loads either the
;;; source or compiled version if present.
(if (not (defined? hobbit))		;Autoload for hobbit
(define (hobbit . args)
  (require 'hobbit)
  (apply hobbit args)))

(define (find-option-file file)
  (let ((opt file))
    (if (file-exists? opt)
	(list "-f" opt)
	'())))
;@
(define (compile-file file . args)
  (define sfs (scheme-file-suffix))
  (require 'filename)
  (apply hobbit file args)
  (let ((command
	 (apply list
		"build"
		"-hsystem"
		"-tdll"
		(string-append "--compiler-options=-I" (implementation-vicinity))
		"-c" (replace-suffix file sfs ".c")
		(find-option-file (replace-suffix file sfs ".opt")))))
    (require 'build)
    (cond ((>= (verbose) 3) (write command) (newline)))
    (build-from-whole-argv command)))
;@
(define (compile->executable exename . files)
  (define sfs (scheme-file-suffix))
  (require 'filename)
  (for-each hobbit files)
  (let ((inits (map (lambda (file)
		      (string-append "-iinit_" (replace-suffix file sfs "")))
		    files))
	(files (map (lambda (file)
		      (string-append "-c" (replace-suffix file sfs ".c")))
		    files)))
    (define command (append (list "build"
				  "-hsystem"
				  "--type=exe"
				  "-o" exename
				  "-F" "compiled-closure" "inexact"
				  (string-append "--linker-options=-L"
						 (implementation-vicinity)))
			    (find-option-file (string-append exename ".opt"))
			    files
			    inits))
    (require 'build)
    (cond ((>= (verbose) 3) (write command) (newline)))
    (build-from-whole-argv command)))

;;; Local Variables:
;;; mode:scheme
;;; End:
(and *script* (exit (compile.scm (list-tail *argv* *optind*))))
