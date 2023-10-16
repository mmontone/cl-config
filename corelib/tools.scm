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
