(defpackage :cl-config
  (:nicknames :cfg)
  (:use :cl)
  (:export #:with-configuration
	   #:cfg
	   #:cfg*
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
	   ))