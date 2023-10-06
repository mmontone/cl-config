;;;; "syntest2.scm" Test macros.
;; Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
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

(require 'macro)

;; Redefine some derived special forms.

(define-syntax let
  (syntax-rules ()
    ((let ((?name ?val) ...) . ?body)
     ((lambda (?name ...) . ?body) ?val ...))
    ((let ?proc ((?name ?val) ...) . ?body)
     (let ((?proc #f)
	   (?name ?val) ...)
       (set! ?proc (lambda (?name ...) . ?body))
       (?proc ?name ...)))))

(define-syntax let*
  (syntax-rules ()
    ((let* () . ?body)
     ((lambda () . ?body)))
    ((let* ((?name ?val)) . ?body)
     ((lambda (?name) . ?body) ?val))
    ((let* ((?name ?val) ?binding ...) . ?body)
     (let* ((?name ?val))
       (let* (?binding ...) . ?body)))))

(define-syntax letrec
  (syntax-rules ()
    ((letrec ((?name ?val) ...) . ?body)
     (let ((?name #f) ...)
       (set! ?name ?val) ...
       (let () . ?body)))))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and ?exp)
     (let ((x ?exp))
       (if x x #f)))
    ((and ?exp . ?rest)
     (let ((x ?exp))
       (if x (and . ?rest) #f)))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or ?exp)
     (let ((x ?exp))
       (if x x #f)))
    ((or ?exp . ?rest)
     (let ((x ?exp))
       (if x x (or . ?rest))))))

(define (force promise)
  (promise))

(define (make-promise proc)
  (let ((result #f))
    (lambda ()
      (if result (car result)
	  (let ((x (proc)))
	    (if result (car result)
		(begin (set! result (list x))
		       x)))))))

(define-syntax delay
  (syntax-rules ()
    ((delay ?expr)
     (make-promise (lambda () ?expr)))))

(define-syntax do
  (syntax-rules ()
    ((do ((?name ?init . ?step) ...)
	 (?test . ?result)
       ?body ...)
     (let-syntax ((do-step (syntax-rules ()
			     ((do-step ?n) ?n)
			     ((do-step ?n ?s) ?s)))
		  (do-result (syntax-rules ()
			       ((do-result) (if #f #f))
			       ((do-result . ?r) (begin . ?r)))))
       (let loop ((?name ?init) ...)
	 (if ?test
	     (do-result . ?result)
	     (begin ?body ...
		    (loop (do-step ?name . ?step) ...))))))))

(define-syntax case
  (syntax-rules (else)
    ((case ?x (else . ?conseq))
     (begin . ?conseq))
    ((case ?x (?lst . ?conseq))
     (if (memv ?x '?lst) (begin . ?conseq)))
    ((case ?x (?lst . ?conseq) . ?rest)
     (if (memv ?x '?lst)
	 (begin . ?conseq)
	 (case ?x . ?rest)))))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond ?clause0 . ?clauses)
     (letrec-syntax 
	 ((cond-aux
	   (syntax-rules (else =>)
	     ((cond-aux) (if #f #f))
	     ((cond-aux (else . ?conseq))
	      (begin . ?conseq))
	     ((cond-aux (?test => ?proc) . ?rest)
	      (let ((val ?test))
		(if val (?proc val) (cond-aux . ?rest))))
	     ((cond-aux (?test) . ?rest)
	      (or ?test (cond-aux . ?rest)))
	     ((cond-aux (?test . ?conseq) . ?rest)
	      (if ?test (begin . ?conseq) (cond-aux . ?rest))))))
       (cond-aux ?clause0 . ?clauses)))))

;; This may fail if you redefine CONS, LIST, APPEND, or LIST->VECTOR
;; It uses the (... ...) escape.
;; All forms are evaluated inside a LETREC-SYNTAX body (is this a problem?).

(define-syntax quasiquote
  (syntax-rules ()
    ((_ ?template)
     (letrec-syntax
	 ((qq
	   (syntax-rules (unquote unquote-splicing quasiquote)
	     ((_ (unquote ?form) ())
	      ?form)
	     ((_ (unquote ?form) (?depth))
	      (list 'unquote (qq ?form ?depth)))
	     ((_ (quasiquote ?form) ?depth)
	      (list 'quasiquote (qq ?form (?depth))))
	     ((_ ((unquote-splicing ?form) . ?rest) ())
	      (append ?form (qq ?rest ())))
	     ((_ ((unquote-splicing ?form) . ?rest) (?depth))
	      (append (list 'unquote-splicing (qq ?form ?depth))
		      (qq ?rest (?depth))))
	     ((_ (?car . ?cdr) ?depth)
	      (cons (qq ?car ?depth) (qq ?cdr ?depth)))
	     ((_ #(?elt (... ...)) ?depth)
	      (list->vector (qq (?elt (... ...)) ?depth)))
	     ((_ ?atom ?depth)
	      '?atom))))
       (qq ?template ())))))

;;(load "r4rstest.scm")


