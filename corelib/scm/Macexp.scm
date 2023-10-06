;;;; "Macexp.scm", macro expansion, respecting hygiene.
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

;; LLIST is a lambda list, BINDINGS an alist using the same identifiers.
(define extended-environment
  (let ((env:annotation-marker 4))
    (lambda (llist bindings env)
      (cons llist (cons env:annotation-marker (cons bindings env))))))

(define syntax-extended-environment
  (let ((env:syntax-marker 6))
    ;;BINDINGS is an alist
    (lambda (bindings env)
      (cons (cons env:syntax-marker bindings) env))))

(define (environment-ref env id)
  (environment-annotation id env))

;;(debind:if_ ?llist ?val ?body ?alt)
;;?alt should evaluate to a procedure taking two arguments, a pattern
;;and an object.  This macro requires the (... ...) ellipsis quote
;;extension.
(define-syntax debind:if
  (syntax-rules ()
    ((_ "PARSE LLIST" () ?val ?body ?alt)
     (if (null? ?val) ?body (?alt '() ?val)))

    ((_ "PARSE LLIST" (?pattern (... ...)) ?val ?body ?alt)
     (let loop ((v ?val))
       (cond ((null? v)
	      (debind:if "PARSE ..." ?pattern ?val ?body))
	     ((pair? v)
	      (let ((carv (car v)))
		(debind:if "PARSE LLIST" ?pattern carv
			   (loop (cdr v)) ?alt)))
	     (else
	      (?alt '(?pattern (... ...)) ?val)))))

    ((_ "PARSE LLIST" (?first . ?rest) ?val ?body ?alt)
     (if (pair? ?val)
	 (let ((carv (car ?val))
	       (cdrv (cdr ?val)))
	   (debind:if "PARSE LLIST" ?first carv
		      (debind:if "PARSE LLIST" ?rest cdrv ?body ?alt)
		      ?alt))
	 (?alt '(?first . ?rest) ?val)))

    ((_ "PARSE LLIST" ?name ?val ?body ?alt)
     (let ((?name ?val)) ?body))

    ((_ "PARSE ..." () ?val ?body)
     ?body)

    ((_ "PARSE ..." (?pattern (... ...)) ?val ?body)
     (debind:if "PARSE ..." ?pattern ?val ?body))

    ((_ "PARSE ..." (?first . ?rest) ?val ?body)
     (debind:if "PARSE ..." ?first (map car ?val)
	(debind:if "PARSE ..." ?rest (map cdr ?val)
			    ?body)))

    ((_ "PARSE ..." ?name ?val ?body)
     (let ((?name ?val)) ?body))

    ((_ ?llist ?val ?body)
     (debind:if ?llist ?val ?body
		(lambda (pat val "debind:if" '?llist val
			     "does not match" pat))))

    ((_ ?llist ?val ?body ?alt)
     (let ((val ?val)
	   (alt ?alt))
       (debind:if "PARSE LLIST" ?llist val ?body alt)))))

;; Uncomment for DESTRUCTURING-BIND enhanced with ellipsis (...) patterns.
;(define-syntax destructuring-bind
;  (syntax-rules ()
;    ((_ ?llist ?val ?body1 ?body ...)
;     (debind:if ?llist ?val
;		(let () ?body1 ?body ...)
;		(lambda (pat val)
;		  (slib:error 'destructuring-bind '?llist
;			      val "does not match" pat))))))

;; This should really dispatch on the keyword only, then
;; use destructuring-case for each keyword, that way errors
;; may be more accurately reported for primitives.
;;
;;(keyword-case expr env (pattern body ...) ...)
(define-syntax keyword-case
  (syntax-rules (else)
    ((_ "RECURSE" ?expr ?env)
     (error 'keyword-case ?expr "not matched"))
    ((_ "RECURSE" ?expr ?env
	(else ?body1 ?body ...))
     (let () ?body1 ?body ...))
    ((_ "RECURSE" ?expr ?env
	((?keyword . ?pattern) ?body1 ?body ...)
	?clause ...)
     (let ((alt (lambda (ignore1 ignore2)
		  (keyword-case "RECURSE" ?expr ?env ?clause ...))))
       ;;Keywords are renamed in the top-level environment for each
       ;;comparison, this is wasteful and somewhat ugly.
       (if (identifier-equal? (renamed-identifier '?keyword '())
			      (car ?expr) ?env)
	   (debind:if ?pattern (cdr ?expr)
		      (let () ?body1 ?body ...)
		      alt)
	   (alt #f #f))))
    ((_ ?expr ?env ?clause1 ?clause ...)
     (let ((expr ?expr))
       (if (or (not (pair? expr))
	       (not (identifier? (car expr))))
	   (error 'keyword-case expr "bad form")
	   (keyword-case "RECURSE" expr ?env ?clause1 ?clause ...))))))

;; This is still not safe when ENV has non-macro bindings in it.
;; It could be made safe by rebuilding an equivalent environment,
;; retaining values only for syntactic bindings.
(define (macro:expand-syntax form env pretty? verbose?)
  (define globals '())
  (define shadowed-globals '())
  (define top-lambda (renamed-identifier 'LAMBDA #f))
  (define top-let (renamed-identifier 'LET #f))
  (define top-let* (renamed-identifier 'LET* #f))
  (define top-letrec (renamed-identifier 'LETREC #f))
  (define top-arrow (renamed-identifier '=> #f))
  (define top-else (renamed-identifier 'ELSE #f))
  (define top-define (renamed-identifier 'DEFINE #f))
  (define top-begin (renamed-identifier 'BEGIN #f))
  (define (arrow? id env)
    (and (identifier? id)
	 (identifier-equal? id top-arrow env)))
  (define (else? id env)
    (and (identifier? id)
	 (identifier-equal? id top-else env)))
  (define (define? form env)
    (and (list? form)			;FORM will have been expanded.
	 (identifier? (car form))
	 (identifier-equal? top-define (car form) env)))
  (define (begin? form env)
    (and (list? form)
	 (identifier? (car form))
	 (identifier-equal? (car form) top-begin env)))

  (define locally-bound? environment-annotation)

  (define pretty-name
    (if pretty?
	(letrec ((counter 0)
		 (genname
		  (lambda (sym)
		    (set! counter (+ counter 1))
		    (string->symbol
		     (string-append (symbol->string sym)
				    "|" (number->string counter))))))
	  (lambda (name env)
	    (if (symbol? name)
		(if (or (memq name
			      '(LAMBDA LET LET* LETREC DO DEFINE SET!
				       BEGIN IF COND CASE AND OR QUOTE
				       QUASIQUOTE UNQUOTE UNQUOTE-SPLICING
				       DEFINE-SYNTAX LET-SYNTAX LETREC-SYNTAX
				       SYNTAX-QUOTE ELSE =>))
			(locally-bound? name env))
		    (genname name)
		    name)
		(genname (identifier->symbol name)))))
        (lambda (name env)
          name)))

  ;; Local bindings -> (identifier pretty-name (usage-context ...))
  ;; This will change.
  (define (initial-binding name env)
    (or (identifier? name)
	(slib:error 'macro:expand name "not identifier"))
    (list name (pretty-name name env) '()))
  (define binding->name cadr)
  (define binding->contexts caddr)
  (define (binding-add-context! b context)
    (let ((ctx (caddr b)))
      (if (not (list? ctx))
	  (error 'not-a-list ctx))
      (or (memq context ctx)
	  (set-car! (cddr b) (cons context ctx)))))

  ;; Produces an alist
  (define (llist->bindings llist env)
    (let recurse ((ll llist))
      (cond ((pair? ll)
	     (cons (initial-binding (car ll) env)
		   (recurse (cdr ll))))
	    ((identifier? ll)
	     (list (initial-binding ll env)))
	    ((null? ll) ll)
	    (else (error 'strange-lambda-list llist)))))

  (define (expand-begin forms env context)
    (if (null? forms)
	'()
	(let recurse ((forms forms))
	  (if (null? (cdr forms))
	      (list (expand (car forms) env context))
	      (cons (expand (car forms) env 'SIDE-EFFECT)
		    (recurse (cdr forms)))))))

  (define (expand-body forms env context)
    (define (rewrite forms defs)
      (if (null? defs)
	  (expand-begin forms env context)
	  (list
	   (expand-primitive
	    `(,top-letrec ,(reverse defs) ;reverse just to make it pretty
			  ,@forms) env context))))
    (let loop ((forms forms)
	       (defs '()))
      (if (null? (cdr forms))
	  (rewrite forms defs)
	  (let ((form1 (expand (car forms) env 'SIDE-EFFECT)))
	    (cond ((define? form1 env)
		   (loop (cdr forms)
			 (cons (cdr form1) defs)))
		  ((begin? form1 env)
		   (loop (append (cdr form1) (cdr forms))
			 defs))
		  (else (rewrite forms defs)))))))

  (define (lookup id env)
    (or (environment-ref env id)
	(let* ((sym (identifier->symbol id))
	       (binding (cond ((assq sym globals))
			      (else
			       (let ((b (initial-binding sym env)))
				 (set! globals (cons b globals))
				 b)))))
	  (cond ((not pretty?) id)
		((not (locally-bound? sym env)))
		((assq sym shadowed-globals))
		(else
		 (set! shadowed-globals
		       (cons (cons sym (binding->name binding))
			     shadowed-globals))))
	    binding)))

  (define pretty-varref
    (if pretty?
	(lambda (id env)
	  (if (symbol? id)
	      id
	      (let ((sym (identifier->symbol id)))
		(if (identifier-equal? id sym env) sym id))))
	(lambda (id env) id)))

  (define unpaint
    (if pretty?
	(lambda (x)
	  (cond ((symbol? x) x)
		((identifier? x) (identifier->symbol x))
		((pair? x) (cons (unpaint (car x)) (unpaint (cdr x))))
		((vector? x) (let* ((n (vector-length x))
				    (v (make-vector n)))
			       (do ((i 0 (+ i 1)))
				   ((>= i n) v)
				 (vector-set! v i (unpaint (vector-ref x i))))))
		(else x)))
	identity))

  (define (expand* forms env context)
    (map (lambda (form) (expand form env context)) forms))

  (define (expand-primitive form env context)
    (define keyword (and (pair? form)
			 (if pretty?
			     (identifier->symbol (car form))
			     (car form))))
    (keyword-case
     form env
     ;;Binding forms
     ((LAMBDA llist body1 body ...)
      (let* ((bindings (llist->bindings llist env))
	     (env (extended-environment llist bindings env))
	     (body (expand-body (cons body1 body) env context))
	     (llist (let recurse ((ll llist)
				  (bl bindings))
		      (cond ((null? ll) '())
			    ((pair? ll) (cons (binding->name (car bl))
					      (recurse (cdr ll) (cdr bl))))
			    (else (binding->name  bl))))))
	`(,keyword ,llist ,@body)))
     ((LET ((names values) ...) body1 body ...)
      (let* ((values (expand* values env 'VALUE))
	     (bindings (llist->bindings names env))
	     (env (extended-environment names bindings env))
	     (body (expand-body (cons body1 body) env context)))
	`(,keyword ,(map (lambda (b val) (list (binding->name b) val))
			 bindings values)
		   ,@body)))
     ((LET name1 ((names values) ...) body1 body ...)
      (expand `((,top-letrec
		 ((,name1 (,(pretty-varref top-lambda env) ,names
			   ,@(cons body1 body))))
		 ,name1) ,@values)
	      env context))
     ((LETREC ((names values) ...) body1 body ...)
      (let* ((bindings (llist->bindings names env))
	     (env (extended-environment names bindings env))
	     (values (expand* values env 'VALUE))
	     (body (expand-body (cons body1 body) env context)))
	`(,keyword ,(map (lambda (b val) (list (binding->name b) val))
			 bindings values)
		   ,@body)))
     ((LET* ((names values) ...) body1 body ...)
      (let recurse ((ns names)
		    (vs values)
		    (env env)
		    (bs '()))
	(if (null? ns)
	    (let ((body (expand-body (cons body1 body) env context)))
	      `(,keyword ,(reverse bs) ,@body))
	    (let ((binding (initial-binding (car ns) env)))
	      (recurse (cdr ns) (cdr vs)
		       (extended-environment (car ns)
					     (list binding)
					     env)
		       (cons (list (binding->name binding)
				   (expand (car vs) env 'VALUE))
			     bs))))))
     ((DO ((names inits . steps) ...)
	  (test exit ...)
	  body ...)
      (let* ((steps (map (lambda (name step)
			   (if (null? step) name (car step)))
			 names steps))
	     (inits (expand* inits env 'VALUE))
	     (bindings (llist->bindings names env))
	     (env (extended-environment names bindings env))
	     (steps (expand* steps env 'VALUE))
	     (test (expand test env 'BOOLEAN))
	     (exit (expand-begin exit env context))
	     (body (expand-begin body env 'SIDE-EFFECT)))
	`(,keyword
	  ,(map (lambda (binding init step)
		  (list (binding->name binding) init step))
		bindings inits steps)
	  ,(cons test exit)
	  ,@body)))
     ((DEFINE (name . llist) body ...)
      (expand-primitive
       `(,keyword ,name (,top-lambda ,llist ,@body)) env context))
     ((DEFINE name value)
      (cond ((null? env)		;Top level
	     (binding-add-context! (lookup name env) 'DEFINE)
	     `(,keyword ,(pretty-varref name env)
			,(expand value env 'VALUE)))
	    (else
	     `(,keyword ,name ,value)))) ;Expansion will be done by expand-body.

     ((SET! var value)
      (let ((b (lookup var env)))
	(binding-add-context! b 'SET!)
	`(,keyword ,(binding->name b) ,(expand value env 'VALUE))))

     ;;Non-binding forms
     ((BEGIN body ...)
      (let ((body (expand-begin body env context)))
	(if (null? (cdr body))
	    (car body)
	    `(,keyword ,@body))))
     ((IF test conseq . alt)
      `(,keyword ,(expand test env 'BOOLEAN)
		 ,(expand conseq env context)
		 ,@(if (pair? alt)
		       (list (expand (car alt) env context))
		       '())))
     ((COND (test exprs ...) ...)
      `(,keyword
	,@(map (lambda (test exprs)
		 (cond ((null? exprs) (list (expand test env context)))
		       ((arrow? (car exprs) env)
			(list (expand test env 'VALUE)
			      (pretty-varref top-arrow env)
			      (expand (cadr exprs) env 'PROCEDURE)))
		       ((else? test env)
			(cons (pretty-varref top-else env)
			      (expand-begin exprs env context)))
		       (else
			(cons (expand test env 'BOOLEAN)
			      (expand-begin exprs env context)))))
	       test exprs)))
     ((CASE obj (datums exprs ...) ...)
      `(,keyword ,(expand obj env 'VALUE)
	,@(map (lambda (datums exprs)
		 (cons (if (else? datums env)
			   (pretty-varref datums env)
			   (unpaint datums))
		       (expand-begin exprs env context)))
	       datums exprs)))
     ((AND forms ...)
      `(,keyword ,@(expand* forms env context)))
     ((OR forms ...)
      `(,keyword ,@(expand* forms env context)))

     ;; Should unpaint synthetic identifiers
     ((QUOTE obj)
      `(,keyword ,(unpaint obj)))
     ((QUASIQUOTE obj)
      `(,keyword
	,(let qexp ((obj obj)
		    (depth 0))
	   (cond ((not (pair? obj))
		  (unpaint obj))
		 ((identifier? (car obj))
		  (let ((keyword (car obj)))
		    (keyword-case
		     obj env
		     ((QUASIQUOTE arg)
		      (list keyword (qexp arg (+ depth 1))))
		     ((UNQUOTE arg)
		      (list keyword
			    (if (zero? depth)
				(expand arg env context)
				(qexp arg (- depth 1)))))
		     ((UNQUOTE-SPLICING arg)
		      (list keyword
			    (if (zero? depth)
				(expand arg env context)
				(qexp arg (- depth 1)))))
		     (else
		      (cons (unpaint keyword) (qexp (cdr obj) depth))))))
		 (else
		  (cons (qexp (car obj) depth)
			(qexp (cdr obj) depth)))))))
     ((DEFINE-SYNTAX name def)
      form)
     ((LET-SYNTAX ((names defs) ...) body1 body ...)
      (let* ((env (syntax-extended-environment '() env))
	     (defs (map (lambda (name def)
			  (cons name (eval-syntax def env)))
			names defs))
	     (env (syntax-extended-environment defs env))
	     (body (expand-body (cons body1 body) env context)))
	(if pretty?
	    `(,(pretty-varref top-let env) () ,@body)
	    `(,top-let* (,(list marker #f)) ,@body))))
     ((LETREC-SYNTAX ((names defs) ...) body1 body ...)
      (let* ((eframe (map (lambda (name) (cons name #f)) names))
	     (env (syntax-extended-environment eframe env)))
	(do ((ds defs (cdr ds))
	     (ef eframe (cdr ef)))
	    ((null? ds))
	  (set-cdr! (car ef) (eval-syntax (car ds) env)))
	(let ((body (expand-body (cons body1 body) env context)))
	  (if pretty?
	      `(,(pretty-varref top-let env) () ,@body)
	      `(,top-let* (,(list marker #f)) ,@body)))))
     ;;SCM extension
     ((SYNTAX-QUOTE obj)
      `(,keyword ,obj))
     (else
      (warn 'expand-syntax "Unexpected primitive syntax" form)
      form)))

  (define (handle-shadowed form env)
    (if (define? form env)
	(list (car form) (cadr form)
	      (handle-shadowed (caddr form)))
	`(,(pretty-varref top-let env)
	  ,(map (lambda (s)
		  (list (cdr s)
			(if (environment-ref env (car s))
			    (renamed-identifier (car s) #f)
			    (car s))))
		shadowed-globals)
	  ,form)))

  (define (expand form env context)
    (cond
     ((identifier? form)
      (let ((expanded (@macroexpand1 form env)))
	(cond ((eq? expanded form)
               form)
	      ((not expanded)
	       (let* ((b (lookup form env))
		      (name (binding->name b)))
		 (binding-add-context! b context)
		 name))
	      (else
               (expand expanded env context)))))
     ((number? form) form)
     ((char? form) form)
     ((boolean? form) form)
     ((null? form) form)
     ((string? form) form)
     ((list? form)
      (if (identifier? (car form))
	  (let ((expanded (@macroexpand1 form env)))
	    (cond ((eq? expanded form)
		   (expand-primitive form env context))
		  ((not expanded)
		   (cons (expand (car form) env 'PROCEDURE)
			 (map (lambda (arg)
				(expand arg env 'VALUE))
			      (cdr form))))
		  (else
		   (expand expanded env context))))
	  (cons (expand (car form) env 'PROCEDURE)
		(expand* (cdr form) env 'VALUE))))
     (else
      (warn 'expand-syntax "Unexpected type of form" form)
      form)))

  (let ((res (expand form env 'TOP)))
    (cond (verbose?
	   (display "Globals: ")
	   (pretty-print globals)
	   (display "Shadowed Globals: ")
	   (pretty-print shadowed-globals)))
    (cond ((null? shadowed-globals) res)
	  ((not (begin? res env)) (handle-shadowed res env))
	  (else (cons (car res)
		      (map (lambda (form)
			     (handle-shadowed form env))
			   (cdr res)))))))

(define (macro:expand form . opt)
  (macro:expand-syntax form '()
		       (not (memq 'not-pretty opt))
		       (memq 'verbose opt)))

;; Debugging fodder.
#+(or)
(begin
  (define (read* filename)
    (call-with-input-file filename
      (lambda (p)
	(let loop ((forms '()))
	  (let ((form (read p)))
	    (if (eof-object? form)
		(cons 'BEGIN (reverse forms))
		(loop (cons form forms))))))))
  (define (expand-file filename . opt)
    (apply macro:expand (read* filename) '() opt))
  (define s (read*  (or *load-pathname* "Macexp.scm"))))


;;; Local Variables:
;;; mode:scheme
;;; eval:(put 'destructuring-bind 'scheme-indent-function 1)
;;; eval:(put 'destructuring-case 'scheme-indent-function 1)
;;; End:
