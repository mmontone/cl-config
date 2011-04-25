(defpackage :cl-config
  (:nicknames :cfg)
  (:use :cl)
  (:export #:with-configuration
	   #:cfg
	   #:cfg*
	   #:configuration
	   #:configuration-schema
	   #:*configuration*
	   #:*configurations*
	   #:*configuration-schemas*
	   #:define-configuration-validator
	   #:with-configuration-section
	   #:xml-writer
	   #:xml-reader
	   #:sexp-writer
	   #:sexp-reader
	   #:serialize
	   #:find-configuration
	   #:find-configuration-schema
	   #:serialize
	   #:unserialize
	   #:serialize-configuration-schemas
	   #:serialize-configurations
	   #:define-configuration-schema
	   #:define-configuration
	   #:make-configuration
	   #:name
	   #:title
	   #:get-option-value
	   #:read-configuration-option
	   #:define-configurable-function
	   #:with-configuration-values
	   #:with-current-configuration-values	   
	   #:with-schema-validation
	   #:define-configuration-schema-option-type
	   #:define-option-validator
	   #:define-option-processor

	   ;; Installers
	   #:installer
	   #:wizard-installer
	   #:configuration-installer
	   #:standard-installer
	   #:go-back
	   #:define-installer
	   #:define-wizard-installer
	   #:define-standard-installer
	   #:idefun
	   #:with-input
	   #:start-section
	   #:input
	   #:choose
	   #:question
	   #:alert
	   #:prompt
	   #:install-warning
	   #:install-error
	   #:install-errors
	   #:repl-installer))