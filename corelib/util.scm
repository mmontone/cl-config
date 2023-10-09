(require 'regex)
(require 'eval)
(require 'srfi-1)
(require 'macro)
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

(define proc-regex "#<primitive-procedure (.*)>")

(define (proc-name proc)
  (let* ((printed-proc (call-with-output-string
                        (lambda (out)
                          (write proc out))))
         (matchv (regmatchv proc-regex printed-proc)))
    (if matchv
        (substring printed-proc
                   (vector-ref matchv 2)
                   (vector-ref matchv 3))
        #f)))

(define apropos-defs '())

;; (define (init-apropos-defs)
;;   (set! apropos-defs '())
;;   (for-each (lambda (proc)
;;               (let ((pname (proc-name proc)))
;;                 (if pname
;;                     (set! apropos-defs (cons pname apropos-defs)))))
;;             (cadr (scheme-report-environment 5)))
;;   apropos-defs)

(define (init-apropos-defs)
  (set! apropos-defs (cadr (scheme-report-environment 5))))

(init-apropos-defs)

(define (apropos what)
  (cond
   ((symbol? what)
    (apropos (symbol->string what)))
   ((string? what)
    (filter (lambda (sym) (regsearch what (symbol->string sym)))
            apropos-defs))))

;; https://pschombe.wordpress.com/2006/03/10/destructuring-bind/
;; (define-syntax destructuring-bind
;;   (lambda (x)
;;     (letrec ((gen-let-bindings
;;               (lambda (expr form)
;;                 (cond ((null? form) '())
;;                       ((pair? form)
;;                        (if (eq? (car form) '&rest)
;;                            (list (list (car (cdr form)) expr))
;;                            (append
;;                             (gen-let-bindings (list 'car expr) (car form))
;;                             (gen-let-bindings (list 'cdr expr) (cdr form)))))
;;                       (else (list (list form  expr))))))
;;              (finaltransformer
;;               (lambda (form expr body)
;;                 (let ((tsym (gensym)))
;;                   `(let ((,tsym ,expr)) (let ,(gen-let-bindings tsym form) ,@body))))))
;;       (let ((syn (cdr (syntax-object->datum x))))
;;         (datum->syntax-object x (finaltransformer
;;                                  (car syn)
;;                                  (car (cdr syn))
;;                                  (cdr (cdr syn))))))))

(define (compose f g)
  (lambda args
    (f (apply g args))))

(define (curry f . c)
  (lambda x (apply f (append c x))))

(define-syntax when
  (syntax-rules ()
    ((_ condition body ...)
     (if condition (begin body ...)))))

(define-syntax unless
  (syntax-rules ()
    ((_ condition body ...)
     (if (not condition) (begin body ...)))))
