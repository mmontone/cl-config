;;;; "Macro.scm", Support for syntax-rules macros.
;; Copyright (C) 1997, 1998, 1999 Free Software Foundation, Inc.
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
;;
;; As in SYNTAX-CASE, the identifier ... may be quoted in a
;; SYNTAX-RULES pattern or template as (... ...).
;;
;; THE-MACRO may be used to define macros, eg
;; (define-syntax foo (the-macro and))
;; defines the syntactic keyword FOO to have the same transformer
;; as the macro AND.

(require 'rev2-procedures) ;append!
(require 'record)

(define macro:compile-syntax-rules
  ;; We keep local copies of these standard special forms, otherwise,
  ;; redefining them before they are memoized below can lead to
  ;; infinite recursion.
  (let-syntax ((lambda (the-macro lambda))
		(begin (the-macro begin))
		(quote (the-macro quote))
		(let (the-macro let))
		(let* (the-macro let*))
		(letrec (the-macro letrec))
		(and (the-macro and))
		(or (the-macro or))
		(delay (the-macro delay))
		(do (the-macro do))
		(case (the-macro case))
		(cond (the-macro cond))
		(quasiquote (the-macro quasiquote)))
    (let ((var-rtd (make-record-type '? '(name rank)))
	  (e-pat-rtd (make-record-type '... '(pattern vars)))
	  (rule-rtd (make-record-type 'rule '(pattern inserted template))))

      (define pattern-variable (record-constructor var-rtd '(name rank)))
      (define pattern-variable? (record-predicate var-rtd))
      (define pattern-variable->name
	(let ((acc (record-accessor var-rtd 'name)))
	  (lambda (x) (identifier->symbol (acc x)))))
      (define pattern-variable->rank (record-accessor var-rtd 'rank))

      ;; An ellipsis-pattern is used both for ellipses in patterns and
      ;; ellipses in templates.  In a pattern, VARS is the list of variables
      ;; bound by the ellipsis pattern.  In a template, VARS is the list of
      ;; variables opened by the ellipsis template.

      (define ellipsis-pattern (record-constructor e-pat-rtd '(pattern vars)))
      (define ellipsis-pattern? (record-predicate e-pat-rtd))
      (define ellipsis-pattern->pattern (record-accessor e-pat-rtd 'pattern))
      (define ellipsis-pattern->vars (record-accessor e-pat-rtd 'vars))

      (define make-rule
	(record-constructor rule-rtd '(pattern inserted template)))
      (define rule->pattern (record-accessor rule-rtd 'pattern))
      (define rule->inserted (record-accessor rule-rtd 'inserted))
      (define rule->template (record-accessor rule-rtd 'template))

      (define (append2 x y)
	(if (null? y) x
	    (append x y)))

      (define (append-if pred x y)
	(let recur ((x x))
	  (cond ((null? x) y)
		((pred (car x)) (cons (car x) (recur (cdr x))))
		(else (recur (cdr x))))))

      (define ellipsis?
	(let (($... (renamed-identifier '... #f)))
	  (lambda (x env)
	    (and
	     (identifier? x)
	     (identifier-equal? x $... env)))))

      ;; Yeah, it's quadratically slow.
      (define (duplicates? vars)
	(if (null? vars)
	    #f
	    (if (memq (car vars) (cdr vars))
		(car vars)
		(duplicates? (cdr vars)))))

      (define (compile-pattern literals rule-exp env-def)
	(define (compile1 pat vars rank ell? k)
	  (cond ((null? pat)
		 (k pat vars))
		((identifier? pat)
		 (if (or (memq pat literals)
			 (and (not ell?) (ellipsis? pat env-def)))
		     (k (renamed-identifier pat env-def) vars)
		     (let ((var (pattern-variable pat rank)))
		       (k var (cons (cons pat var) vars)))))
		((vector? pat)
		 (compile1 (vector->list pat) vars rank ell?
			   (lambda (comp vars)
			     (k (list->vector comp) vars))))
		((not (pair? pat))
		 (k pat vars))
		((and ell? (ellipsis? (car pat) env-def))
		 (or (and (pair? (cdr pat))
			  (null? (cddr pat)))
		     (error "bad ellipsis quote:" pat))
		 (compile1 (cadr pat) vars rank #f k))
		((and ell?
		      (pair? (cdr pat))
		      (ellipsis? (cadr pat) env-def))
		 (or (null? (cddr pat))
		     (error "bad ellipsis:" pat))
		 (compile1
		  (car pat) '() (+ rank 1) ell?
		  (lambda (comp1 vars1)
		    (k (list
			(ellipsis-pattern comp1 (map cdr vars1)))
		       (append2 vars1 vars)))))
		(else ; pat is a pair
		 (compile1
		  (car pat) '() rank ell?
		  (lambda (comp1 vars1)
		    (compile1
		     (cdr pat) vars rank ell?
		     (lambda (comp2 vars2)
		       (k (cons comp1 comp2)
			  (append2 vars1 vars2)))))))))
	(let ((pat (car rule-exp))
	      (tmpl (cadr rule-exp)))
	  (if (identifier? pat)
	      (apply make-rule #f (rewrite-template tmpl '() env-def))
	      (compile1
	       (cdr pat) '() 0 #t
	       (lambda (compiled vars)
		 (let ((dup (duplicates? (map car vars))))
		   (if dup
		       (error
			"syntax-rules: duplicate pattern variable:"
			dup " in rule " rule-exp)
		       (apply make-rule
			      (cons #f compiled)
			      (rewrite-template tmpl vars env-def)))))))))

      (define (rewrite-template template vars env-def)
	(let rewrite1 ((tmpl template)
		       (rank 0)
		       (inserted '())
		       (ell? #t)
		       (k (lambda (compiled inserted opened)
			    (list inserted compiled))))
	  (cond ((null? tmpl)
		 (k tmpl '() '()))
		((identifier? tmpl)
		 (let ((v (assq tmpl vars)))
		   (cond ((not v)
			  (k tmpl (list tmpl) '()))
			 ((zero? (pattern-variable->rank (cdr v)))
			  (k (cdr v) '() '()))
			 ((>= rank (pattern-variable->rank (cdr v)))
			  (k (cdr v) '() (list (cdr v))))
			 (else
			  (error "pattern variable rank mismatch:" tmpl
				 " in " template)))))
		((vector? tmpl)
		 (rewrite1
		  (vector->list tmpl) rank inserted ell?
		  (lambda (compiled inserted opened)
		    (k (list->vector compiled) inserted opened))))
		((not (pair? tmpl))
		 (k tmpl '() '()))
		((and ell? (ellipsis? (car tmpl) env-def))
		 ;; (... ...) escape
		 (or (and (pair? (cdr tmpl))
			  (null? (cddr tmpl)))
		     (error "Bad ellpsis quote:" tmpl
			    " in template " template))
		 (rewrite1 (cadr tmpl) rank inserted #f k))
		((and ell?
		      (pair? (cdr tmpl))
		      (ellipsis? (cadr tmpl) env-def))
		 (rewrite1
		  (car tmpl) (+ rank 1) '() ell?
		  (lambda (comp1 ins1 op1)
		    (if (null? op1)
			(error "Bad ellipsis:" tmpl
			       " in template " template))
		    (rewrite1
		     (cddr tmpl) rank inserted ell?
		     (lambda (comp2 ins2 op2)
		       (k (cons (ellipsis-pattern comp1 op1)
				comp2)
			  (append2 ins1 ins2)
			  (append-if (lambda (op)
				       (> (pattern-variable->rank op)
					  rank))
				     op1 op2)))))))
		(else ; tmpl is a pair
		 (rewrite1
		  (car tmpl) rank '() ell?
		  (lambda (comp1 ins1 op1)
		    (rewrite1
		     (cdr tmpl) rank inserted ell?
		     (lambda (comp2 ins2 op2)
		       (k (cons comp1 comp2)
			  (append2 ins1 ins2)
			  (append2 op1 op2))))))))))

;;; Match EXP to RULE, returning alist of variable bindings or #f.

      (define (match rule exp env-use)
	(define (match1 r x)
	  (cond ((null? r)
		 (and (null? x) '()))
		((pair? r)
		 (if (ellipsis-pattern? (car r))
		     (and
		      (list? x)
		      (let ((pat (ellipsis-pattern->pattern (car r))))
			(let match-list ((x x)
					 (vals '()))
			  (if (null? x)
			      (if (null? vals)
				  (map list (ellipsis-pattern->vars (car r)))
				  (let ((vars (map car (car vals))))
				    (apply map list vars
					   (map (lambda (al)
						  (map cdr al))
						(reverse vals)))))
			      (let ((val (match1 pat (car x))))
				(and val
				     (match-list (cdr x) (cons val vals))))))))
		     (and
		      (pair? x)
		      (let ((v1 (match1 (car r) (car x))))
			(and v1
			     (let ((v2 (match1 (cdr r) (cdr x))))
			       (and v2 (append2 v1 v2))))))))
		((identifier? r)		;literal
		 (and (identifier? x) (identifier-equal? r x env-use) '()))
		((pattern-variable? r)
		 (list (cons r x)))
		((vector? r)
		 (and (vector? x)
		      (match1 (vector->list r) (vector->list x))))
		(else
		 (and (equal? r x) '()))))
	(let ((pat (rule->pattern rule)))
	  (if (pair? pat)
	      (and (pair? exp)
		   (match1 (cdr pat) (cdr exp)))
	      (if (pair? exp) #f '()))))

      (define (substitute-in-template x-use rule vars env-def)
	(define (length-error pats vals)
	  (apply error
		 "syntax-rules: pattern variable length mismatch:\n"
		 x-use
		 (map (lambda (name val)
			`(,(pattern-variable->name
			    name) -> ,val))
		      pats vals)))

	(let ((ins (map (lambda (id)
			  (cons id (renamed-identifier id env-def)))
			(rule->inserted rule))))
	  (let subst1 ((tmpl (rule->template rule))
		       (vars vars))
	    (cond ((null? tmpl)
		   tmpl)
		  ((pair? tmpl)
		   (if (ellipsis-pattern? (car tmpl))
		       (let* ((enames (ellipsis-pattern->vars (car tmpl)))
			      (etmpl (ellipsis-pattern->pattern (car tmpl)))
			      (evals (map (lambda (nam)
					    (cdr (assq nam vars)))
					  enames))
			      (n (length (car evals))))
			 (let check ((es (cdr evals)))
			   (if (pair? es)
			       (if (= n (length (car es)))
				   (check (cdr es))
				   (length-error enames evals))))
			 (append!
			  (map (lambda (eval)
				 (subst1 etmpl
					 (append!
					  (map cons enames eval)
					  vars)))
			       (apply map list evals))
			  (subst1 (cdr tmpl) vars)))
		       (cons (subst1 (car tmpl) vars)
			     (subst1 (cdr tmpl) vars))))
		  ((identifier? tmpl)
		   (let ((a (assq tmpl ins)))
		     (if a (cdr a) tmpl)))
		  ((pattern-variable? tmpl)
		   (@copy-tree (cdr (assq tmpl vars))))
		  ((vector? tmpl)
		   (list->vector (subst1 (vector->list tmpl) vars)))
		  (else
		   tmpl)))))

      ;; MACRO:COMPILE-SYNTAX-RULES
      (lambda (x-def env-def)
	(let ((x-def (remove-line-numbers! x-def)))
	  (or (and (list? x-def)
		   (< 2 (length x-def))
		   (list? (cadr x-def)))
	      (error "Malformed syntax-rules:" x-def))
	  (let ((literals (cadr x-def)))
	    (for-each (lambda (x)
			(or (identifier? x)
			    (error "Bad literals list:" x-def)))
		      literals)
	    (let ((rules (map (lambda (rule-expr)
				(or (and (list? rule-expr)
					 (= 2 (length rule-expr))
					 (let ((pat (car rule-expr)))
					   (or (pair? pat)
					       (identifier? pat))))
				    (error "Bad rule:" rule-expr))
				(compile-pattern literals rule-expr env-def))
			      (cddr x-def))))

	      (lambda (x-use env-use)
		;;FIXME We should use the line numbers.
		(let ((x-use (remove-line-numbers! x-use)))
		  (let loop ((rules rules))
		    (cond ((null? rules)
			   (error "macro use does not match definition:"
				  x-use))
			  ((match (car rules) x-use env-use)
			   => (lambda (vars)
				(substitute-in-template
				 x-use (car rules) vars env-def)))
			  (else
			   (loop (cdr rules))))))))))))))

(define-syntax syntax-rules
  (procedure->syntax
   (lambda (expr env-def)
     (let ((transformer (macro:compile-syntax-rules expr env-def)))
       (let loop ((rules (cddr expr)))
	 (cond ((null? rules)
		(procedure->memoizing-macro transformer))
	       ((identifier? (caar rules))
		(procedure->identifier-macro transformer))
	       (else
		(loop (cdr rules)))))))))


;; Explicit renaming macro facility, as in
;; W. Clinger, "Hygienic Macros Through Explicit Renaming"
(define (macro:renaming-transformer-procedure proc env-def)
  (procedure->memoizing-macro
   (lambda (expr env-use)
     (proc (@copy-tree expr)
	   (let ((al '()))
	     (lambda (id)
	       (cond ((not (identifier? id))
		      (error id "non-identifier passed to rename procedure"
			     expr))
		     ((assq id al) => cdr)
		     (else
		      (let ((r-id (renamed-identifier id env-def)))
			(set! al (cons id r-id))
			r-id)))))
	   (lambda (id1 id2)
	     (or (and (identifier? id1)
		      (identifier? id2)
		 (error (if (identifier? id1) id2 id1)
			"non-identifier passed to compare procedure"
			expr)))
	     (identifier-equal? id1 id2 env-use))))))

(define renaming-transformer
  (let ((?transformer
	 (renamed-identifier 'macro:renaming-transformer-procedure #f))
	(?syntax-quote (renamed-identifier 'syntax-quote #f)))
    (procedure->memoizing-macro
     (lambda (exp env-def)
       `(,?transformer ,(cadr exp) (,?syntax-quote ,env-def))))))

(define macro:load load)
(define macro:eval eval)
(define (macro:expand . args)
  (load (in-vicinity (implementation-vicinity) "Macexp"))
  (apply macro:expand args))
(provide 'macro)

;; These are not part of the SYNTAX-RULES implementation, but I see
;; no better place to put them:

;; A debugging utility macro that is easy to grep for.
(define-syntax @print
  (syntax-rules (quote)
    ((_ '?arg)
     (begin (write '?arg)
	    (newline)))
    ((_ ?arg)
     (begin (write '?arg)
	    (display " => ")
	    (let ((x ?arg))
	      (write x)
	      (newline)
	      x)))
    ((_ ?arg1 ?arg ...)
     (begin
       (@print ?arg1)
       (begin
	 (display " ")
	 (@print ?arg))
       ...))))

(define-syntax @pprint
  (syntax-rules (quote)
    ((_ '?arg)
     (begin (write '?arg)
	    (newline)))
    ((_ ?arg)
     (begin (write '?arg)
	    (display " => ")
	    (let ((x ?arg))
	      (pprint x)
	      (newline)
	      x)))
    ((_ ?arg1 ?arg ...)
     (begin
       (@pprint ?arg1)
       (begin
	 (display " ")
	 (@pprint ?arg))
       ...))))

;; Better run time error reporting than the version in Init*.scm,
;; also only takes a given car or cdr once.
(define-syntax destructuring-bind
  (syntax-rules ()
    ((_ "PARSE-LLIST" () ?val ?body ?err)
     (if (null? ?val) ?body (?err '() ?val)))
    ((_ "PARSE-LLIST" (?name1 . ?rest) ?val ?body ?err)
     (if (pair? ?val)
	 (let ((carv (car ?val))
	       (cdrv (cdr ?val)))
	   (destructuring-bind "PARSE-LLIST" ?name1 carv
              (destructuring-bind "PARSE-LLIST" ?rest cdrv ?body ?err)
	      ?err))
	 (?err '(?name1 . ?rest) ?val)))
    ((_ "PARSE-LLIST" ?name ?val ?body ?err)
     (let ((?name ?val)) ?body))
    ((_ ?llist ?val ?body1 ?body ...)
     (let ((err (lambda (pat val)
		  (slib:error 'destructuring-bind '?llist
			      val "does not match" pat)))
	   (val ?val))
       (destructuring-bind "PARSE-LLIST" ?llist val
			   ;;Use LET to allow internal DEFINE in body.
			   (let () ?body1 ?body ...)
			   err)))))
