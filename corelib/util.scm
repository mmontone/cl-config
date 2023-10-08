(require 'regex)
(require 'eval)
(require 'srfi-1)

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
