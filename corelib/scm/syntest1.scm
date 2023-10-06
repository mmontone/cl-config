;Most of the tests themselves are taken from William Clinger's reference 
;implementation of syntax-rules, `macros.will' in the Scheme repository
;at ftp.cs.indiana.edu

;Copyright 1992 William Clinger

;Permission to copy this software, in whole or in part, to use this
;software for any lawful purpose, and to redistribute this software
;is granted subject to the restriction that all copies made of this
;software must include this copyright notice in full.

;I also request that you send me a copy of any improvements that you
;make to this software so that they may be incorporated within it to
;the benefit of the Scheme community.


(require 'macro)

(define synerrs '())

(define-syntax test
  (syntax-rules ()
		((test ?exp ?ans)
		 (begin
		   (display '?exp)
		   (display " ==> ")
		   (let* ((exp (copy-tree '?exp))
			  (x ?exp)
			  #+f(x (eval (macro:expand '?exp)))
			  )
		     (display x)
		     (newline)
		     (or (equal? x ?ans)
			 (begin
			   (set! synerrs
				 (cons (list x ?ans '?exp) synerrs))
			   (display "ERROR: expected ")
			   (display ?ans)
			   (newline))))))
		((test ?exp0 ?exp1 ?exp2 ...)
		 (begin (display '?exp0)
			(newline)
			?exp0 (test ?exp1 ?exp2 ...)))))

(test (let ((x 'outer))
	(let-syntax ((m (syntax-rules () ((m) x))))
	  (let ((x 'inner))
	    (m))))
      'outer)

(test (let-syntax ((when (syntax-rules
			  ()
			  ((when ?test ?stmt1 ?stmt2 ...)
			   (if ?test (begin ?stmt1 ?stmt2 ...))))))
	(let ((if #t))
	  (when if (set! if 'now))
	  if))
      'now)

(test (letrec-syntax
	  ((or (syntax-rules
		()
		((or) #f)
		((or ?e) ?e)
		((or ?e1 ?e2 ...)
		 (let ((temp ?e1))
		   (if temp temp (or ?e2 ...)))))))
	(let ((x #f)
	      (y 7)
	      (temp 8)
	      (let odd?)
	      (if even?))
	  (or x
	      (let temp)
	      (if y)
	      y)))
      7)

(test (let ((=> #f))
	(cond (#t => 'ok)))
      'ok)

; This syntax of set*! matches that of an example in the R4RS.
; That example was put forth as an example of a hygienic macro
; that supposedly couldn't be written using syntax-rules.  Hah!

(test (define-syntax set*!
	(syntax-rules
	 ()
	 ((set*! (?var ?val) ...)
	  (set*!-help (?val ...) () (?var ?val) ...))))
      (define-syntax set*!-help
	(syntax-rules
	 ()
	 ((set*!-help () (?temp ...) (?var ?val) ...)
	  (let ((?temp ?val) ...)
	    (set! ?var ?temp) ...))
	 ((set*!-help (?var1 ?var2 ...) ?temps ?assignments ...)
	  (set*!-help (?var2 ...) (temp . ?temps) ?assignments ...))))
      (let ((x 3)
	    (y 4)
	    (z 5))
	(set*! (x (+ x y z))
	       (y (- x y z))
	       (z (* x y z)))
	(list x y z))

      '(12 -6 60))

(test (let ((else #f))
	(cond (#f 3)
	      (else 4)
	      (#t 5)))
      5)

(test (define-syntax push
	(syntax-rules ()
		      ((push item place)
		       (set! place (cons item place)))))
      (let* ((cons (lambda (name)
		     (case name
		       ((phil)  '("three-card monte"))
		       ((dick)  '("secret plan to end the war"
				  "agnew"
				  "not a crook"))
		       ((jimmy) '("why not the best"))
		       ((ron)   '("abolish the draft"
				  "balance the budget"))
		       (else    '()))))
	     (scams (cons 'phil)))
	(push (car (cons 'jimmy)) scams)
	(push (cadr (cons 'ron)) scams)
	scams)
      '("balance the budget" "why not the best" "three-card monte"))

(test (define-syntax replic
	(syntax-rules ()
	  ((_ (?x ...) (?y ...))
	   (let ((?x (list ?y ...)) ...)
	     (list ?x ...)))))
      (replic (x y z) (1 2))
      '((1 2) (1 2) (1 2)))

;; The behavior of this one is one is not specified by R5RS, below
;; is what SCM does.
;(test (define-syntax spread
;	(syntax-rules ()
;	  ((_ ?x (?y ...))
;	   '(((?x ?y) ...)))))
;      (spread x (1 2 3))
;      '(((x 1) (x 2) (x 3))))

(cond
 ((null? synerrs)
  (newline)
  (display "Passed all tests\n")
  (display "Load \"syntest2\" to rewrite derived expressions and test\n"))
 (else
  (newline)
  (display "FAILED, errors were:")
  (newline)
  (display "(got expected call)")
  (newline)
  (for-each (lambda (l) (write l) (newline)) synerrs)
  (newline)))

