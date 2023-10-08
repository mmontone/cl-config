;; parsing of command line arguments using a configuration schema

(require 'regex)

(define (cfgcli:parse arguments schema config)
  (let ((tokens (vector->list (string-split "\\s+" arguments)))
        (status 'setting)
        (setting #f))
    (for-each (lambda (token)
                (case status
                  ((setting)
                   (let ((cliarg (car tokens)))
                     (set! tokens (cdr tokens))
                     (cond
                      ;; Long option
                      ((regmatch "^--(.*)" cliarg)
                       (let* ((matchv (regmatchv "^--(.*)" cliarg))
                              (cliarg-name (substring cliarg
                                                      (vector-ref matchv 2)
                                                      (vector-ref matchv 3)))
                              (schema-setting (find (lambda (setting)
                                                      (string=? (symbol->string (setting-name setting))
                                                                cliarg-name))
                                                    (schema-settings schema))))
                         (if (not schema-setting)
                             (error (format "Invalid setting: ~a" cliarg-name))
                             (begin
                               (set! setting schema-setting)
                               (set! status 'value))))))))
                  ;; Parse setting value
                  ((value)
                   (let ((val token))
                     (config-set! config (setting-name setting) val)
                     (set! status 'setting)))))
              tokens))
  config)

;; print command line help using schema
(define (cfgcli:print-help schema)
  (error "TODO"))

;; print help about setting
(define (cfgcli:print-help-setting schema setting)
  (error "TODO"))

;; search setting
(define (cfgcli:apropos schema setting)
  (error "TODO"))
