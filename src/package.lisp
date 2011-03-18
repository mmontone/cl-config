(defpackage :cl-config
  (:nicknames :cfg)
  (:use :cl)
  (:export #:cfg
	   #:cfg*
	   #:*configurations*
	   #:*configuration-schemas*
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
	   ))