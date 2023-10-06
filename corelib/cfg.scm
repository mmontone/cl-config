(define *schemas* '())
(define *configs* '())

(define (make-config name)
  (list (cons 'name name)
	(cons 'parent #f)
	(cons 'settings '())
	(cons 'schema #f)
	(cons 'options '())))

(define (config-name config)
  (cdr (assoc 'name config)))

(define (settings config)
  (cdr (assoc 'settings config)))

(define (assoc-set! lst key value)
  (let ((ass (assoc key lst)))
    (if ass
	(begin (set-cdr! ass value)
	       lst)
	(append lst (list (cons key value))))))

(define (assoc-get lst key)
  (cdr (assoc key lst)))

(define (config-set! config attribute-name value)
  (assoc-set! config 'settings
	      (assoc-set! (settings config) attribute-name value)))

(define (config-get config name)
  (assoc-get (settings config) name))

(define (print-config config)
  (display (config-name config))
  (newline)
  (newline)
  (for-each (lambda (attr)
	      (display (car attr))
	      (display ": ")
	      (display (cdr attr))
	      (newline))
	    (settings config)))

(define (cfg:validate config schema)
  (let ((schema (or schema (config-schema config)
		    (error "No config schema"))))
    (error "TODO")))

(define (config-schema config)
  (assoc-get config 'schema))

(define (config-parent config)
  (assoc-get config 'parent))

(define (set-parent! config parent)
  (assoc-set! config 'parent parent))

(define (register-config config)
  (set! *configs*
	(assoc-set! *configs* (config-name config) config)))

(define (find-config name)
  (assoc-get *configs* name))

(define (cfg:save config)
  (error "TODO"))

;; load config from source
(define (cfg:load source)
  (error "TODO"))

(define (hello-world)
  (display "hello world"))
