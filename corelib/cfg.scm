(require 'define-record-type)

;;---- Utilities ------------------------------

(require 'format)

(define (empty? list)
  (zero? (length list)))

(define (assert condition . message-and-args)
  (let ((message (if (empty? message-and-args)
                     "Assertion failed"
                     (car message-and-args)))
        (args (if (empty? message-and-args)
                  '()
                  (cdr message-and-args))))
    (if (not condition)
        (error (apply format message args)))))

;; (assert #t)
;; (assert #f)
;; (assert #f "No")
;; (assert #f "No: ~a" 'foo)

(define (car* lst)
  (and (not (empty? lst))
       (car lst)))

(car* '())
(car* '(a))

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

(define (make-config-from-spec name spec)
  (let ((config (make-config name)))
    (set-config-parent! config (car spec))
    (set-config-settings! config (cadr spec))
    config))

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

(define (make-setting-from-spec spec)
  (let ((name (car spec))
        (type (cadr spec))
        (required? (get-prop ':required? (cddr spec) #t))
        (default (get-prop ':default (cddr spec) #f))
        (doc (get-prop ':doc (cddr spec) #f))
        (summary (get-prop ':summary (cddr spec) #f)))
    (make-setting name type
                  ':required? required?
                  ':default default
                  ':doc doc
                  ':summary summary)))

(define (make-schema-from-spec name spec)
  (let ((parent (car spec))
        (settings (map make-setting-from-spec (cadr spec)))
        (options (cddr spec)))
    (let ((schema (make-schema name)))
      (set-schema-parent! schema parent)
      (set-schema-options! schema options)
      (set-schema-doc! schema (get-prop ':doc options))
      (set-schema-settings! schema settings)
      schema)))

(define-record-type <setting>
  (%make-setting name type)
  setting?
  (name setting-name set-setting-name!)
  (type setting-type set-setting-type!)
  (required? setting-required? set-setting-required!)
  (default setting-default set-setting-default!)
  (doc setting-doc set-setting-doc!)
  (summary setting-summary set-setting-summary!)
  (cli-name setting-cli-name set-setting-cli-name!)
  (cli-switch setting-cli-switch set-setting-cli-switch!))

(define (make-setting name type . options)
  (let ((setting (%make-setting name type)))
    (set-setting-required! setting (get-prop ':required? options #t))
    (set-setting-default! setting (get-prop ':default options #f))
    (set-setting-summary! setting (get-prop ':summary options #f))
    (set-setting-doc! setting (get-prop ':doc options #f))
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

(define (config-get config name . default)
  (assoc-get* (settings config) name
              (if (not (empty? default))
                  (car default)
                  (lambda () (error "Invalid setting")))))

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

(define (validate-setting setting config)
  (let ((setting-val (config-get config (setting-name setting) 'cfg:unset)))
    (cond
     ((and (eq? setting-val 'cfg:unset)
           (setting-required? setting))
      (error (format "~a is required" (setting-name setting))))
     ((not (eq? setting-val 'cfg:unset))
      (case (setting-type setting)
        ((string)
         (or (string? setting-val)
             "Should be a string"))
        ((integer)
         (or (integer? setting-val)
             "Should be an integer"))
        ((boolean)
         (or (boolean? setting-val)
             "Should be a boolean"))
        (else (format "Invalid type: ~a" (setting-type setting))))))))

(define (validate-with-schema config schema)
  (let ((errors '()))
    (for-each (lambda (setting)
                (let ((err (validate-setting setting config)))
                  (if (string? err)
                      (set! errors (append! errors (list (cons (setting-name setting) err)))))))
              (schema-settings schema))
    errors))

(define (cfg:validate config . optional-schema)
  (let ((schema (or (car* optional-schema)
                    (config-schema config)
                    (error "No config schema"))))
    (validate-with-schema config schema)))

(define (register-config config)
  (set! *configs*
    (assoc-set! *configs* (config-name config) config)))

(define (find-config name)
  (assoc-get* *configs* name (lambda () (error "Config not found"))))

(define (cfg:save config destination)
  (cond
   ((string? destination)
    ;; Assume a pathname
    (let ((ext (file-extension destination)))
      (if ext
          (cond
           ((string=? ext "json")
            (save-config->json config destination))
           ((string=? ext "xml")
            (save-config->xml config destination))
           (#t
            (save-config->sexp config destination)))
          (cfg:save-sexp config destination))))
   (#t (error (format "Invalid store destination: ~a" destination)))))

(define (config-spec config)
  (list* (config-parent config)
         (settings config)
         (config-options config)))

(define (save-config->sexp config destination)
  (call-with-output-file destination
    (lambda (out)
      (write (config-spec config) out))))

;; load config from source
(define (cfg:load source)
  (cond
   ((string? source)
    ;; Assume a pathname
    (let ((ext (file-extension source)))
      (if ext
          (cond
           ((string=? ext "json")
            (load-config-from-json source))
           ((string=? ext "xml")
            (load-config-from-xml source))
           (#t
            (load-config-from-sexp source)))
          (load-config-from-sexp source))))
   (#t (error (format "Invalid source: ~a" source)))))

(define (load-config-from-sexp pathname)
  (call-with-input-file pathname
    (lambda (in)
      (let ((spec (read in)))
        (make-config-from-spec "config" spec)))))
