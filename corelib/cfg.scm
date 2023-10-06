(require 'define-record-type)

;;---- Utilities ------------------------------

(define (get-prop key property-list . default)
  (cond ((null? property-list) (if (not (null? default))
                                   (car default)
                                   #f))
        ((null? (cdr property-list))
           (error "Malformed property list"))
        ((equal? key (car property-list))
           (cadr property-list))
        (else (apply get-prop key (cons (cdr (cdr property-list)) default)))))

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

(define (assoc-get* lst key . default)
  (call-with-values
      (lambda () (apply assoc-get lst (cons key default)))
    (lambda (val found?)
      val)))

;;(assoc-get* '() 'x)
;;(assoc-get* '((x . 22)) 'x)

;; ---------- Config library ---------------

(define *schemas* '())
(define *configs* '())

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

(define-record-type <schema>
  (%make-schema name)
  schema?
  (name schema-name set-schema-name!)
  (doc schema-doc set-schema-doc!)
  (settings schema-settings set-schema-settings!)
  (parent schema-parent set-schema-parent!)
  (options schema-options set-schema-options!))

(define (make-schema name)
  (let ((schema (%make-schema name)))
    (set-schema-doc! schema #f)
    (set-schema-settings! schema '())
    (set-schema-parent! schema #f)
    (set-schema-options! schema '())
    schema))

(define-record-type <setting>
  (%make-setting name type)
  setting?
  (name setting-name set-setting-name!)
  (type setting-type set-setting-type!)
  (required? setting-required? set-setting-required!)
  (default setting-default set-setting-default!)
  (doc setting-doc set-setting-doc!))

(define (make-setting name type . options)
  (let ((setting (%make-setting name type)))
    (set-setting-required! setting #t)
    (set-setting-default! setting #f)
    (set-setting-doc! setting #f)
    setting))

(define (add-setting schema setting)
  (set-schema-settings!
   schema
   (assoc-set! (schema-settings schema)
               (setting-name setting)
               setting)))

(define-record-type <settings-group>
  (%make-settings-group name)
  settings-group?
  (name settings-group-name)
  (doc settings-group-doc set-settings-group-doc!)
  (settings settings-group-setting set-settings-group-settings!))

(define (make-settings-group name)
  (let ((settings-group (%make-settings-group name)))
    (set-settings-group-doc! settings-group #f)
    (set-settings-group-settings! settings-group '())
    settings-group))

;;------ api ----------------------------------

(define (config-set! config attribute-name value)
  (set-config-settings! config
                        (assoc-set! (settings config) attribute-name value)))

(define (config-get config name)
  (assoc-get* (settings config) name (lambda () (error "Invalid setting"))))

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
  (assoc-get* *configs* name (lambda () (error "Config not found"))))

(define (cfg:save config destination)
  (error "TODO"))

;; load config from source
(define (cfg:load source)
  (error "TODO"))

(define (hello-world)
  (display "hello world"))
