;; parsing of command line arguments using a configuration schema

;; TODO:
;; - Consider a generator of a Bash script that provides the custom completion for the command line.

(require 'regex)
(require 'srfi-1)

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
(define (cfgcli:print-help schema cli-name)
  (display (format "Usage: ~a [OPTIONS]" cli-name))
  (newline)
  (when (schema-doc schema)
    (display (schema-doc schema))
    (newline))
  (newline)
  (display "Options:")
  (newline)
  (for-each (lambda (setting)
              (display "--")
              (display (symbol->string (setting-name setting)))
              (display "    ")
              (when (setting-summary setting)
                (display (setting-summary setting)))
              (newline))
            (schema-settings schema))) 

;; print help about setting
(define (cfgcli:print-help-setting schema setting)
  (error "TODO"))

;; search setting
(define (cfgcli:apropos schema setting)
  (error "TODO"))
