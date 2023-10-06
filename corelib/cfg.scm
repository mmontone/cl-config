(require 'record)

(define *schemas* '())
(define *configs* '())

(define (call-with-optional-args args defaults proc)
  (if (> (length args) (length defaults))
      (error "Bad arguments"))
  (let ((i 0))
    (apply proc
           (map (lambda (default)
                  (let ((val
                         (if (<= (1+ i) (length args))
                             (list-ref args i)
                             default)))
                    (set! i (1+ i))
                    val))
                defaults))))

;;(call-with-optional-args '() '(#t #f)
;;                         (lambda (x y) (list x y)))

;;(call-with-optional-args '(foo) '(#t #f)
;;                         (lambda (x y) (list x y)))

;;(call-with-optional-args '(foo bar) '(#t #f)
;;                         (lambda (x y) (list x y)))

;;(call-with-optional-args '(foo bar baz) '(#t #f)
;;                         (lambda (x y) (list x y)))

(define (assoc-set! lst key value)
  (let ((ass (assoc key lst)))
    (if ass
        (begin (set-cdr! ass value)
               lst)
        (append lst (list (cons key value))))))

(define (assoc-get lst key . default)
  (let ((pair (assoc key lst)))
    (if (pair? pair)
        (values (cdr pair) #t)
        (values (if (and (not (null? default))
                         (procedure? (car default)))
                    ((car default))
                    #f)
                #f))))

(define-record-type <config>
  (%make-config name)
  config?
  (name config-name set-config-name!)
  (doc config-doc set-config-doc!)
  (schema config-schema set-config-schema!)
  (settings settings set-config-settings!)
  (parent config-parent set-config-parent!)
  (options config-options set-config-options!))

(define (make-config name)
  (let ((cfg (%make-config name)))
    (set-config-doc! cfg #f)
    (set-config-schema! cfg #f)
    (set-config-settings! cfg '())
    (set-config-parent! cfg #f)
    (set-config-options! cfg '())
    cfg))

(define (make-schema name)
  (list (cons 'type 'cfg:schema)
        (cons 'name name)
        (cons 'settings '())))

(define (schema? x)
  (and (list? x)
       (eq? (assoc-get x 'type) 'cfg:schema)))

(define (make-setting name type . options)
  (list name type options))

(define (config-set! config attribute-name value)
  (set-config-settings! config
                        (assoc-set! (settings config) attribute-name value)))

(define (config-get config name)
  (call-with-values
      (lambda () (assoc-get (settings config) name (lambda () (error "Invalid setting"))))
    (lambda (x y) x)))

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

(define (register-config config)
  (set! *configs*
    (assoc-set! *configs* (config-name config) config)))

(define (find-config name)
  (assoc-get *configs* name))

(define (cfg:save config destination)
  (error "TODO"))

;; load config from source
(define (cfg:load source)
  (error "TODO"))

(define (hello-world)
  (display "hello world"))
