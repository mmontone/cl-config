(defpackage :cl-config
  (:nicknames :cfg)
  (:use :cl)
  (:export #:cfg
	   #:*configurations*
	   #:*configuration-schemas*
	   #:xml-writer
	   #:xml-reader
	   #:sexp-writer
	   #:sexp-reader
	   #:serialize
	   #:find-configuration
	   #:find-configuration-schema))