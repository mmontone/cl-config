(in-package :cfg.web)

(define-configuration-schema cl-config-web-configuration ()
  (:title "CL-CONFIG Web application configuration")
  (:documentation "CL-CONFIG Web application configuration schema")
  (:section :import/export "Import export settings"
	    (:import/export-filepath "Import/export filepath" :pathname :optional t)
	    (:import-on-load "Import on load" :boolean :default nil)
	    (:export-on-save "Export on save" :boolean :default nil)
	    (:backend "Backend" (:one-of (:sexp "SEXP backend")
					 (:xml "XML backend")
					 (:binary "Binary backend"))
		      :default :xml))
  (:section :webapp-configuration "Web application settings"
	    (:documentation "Web application configuration")
	    (:host "Host" :text :default "localhost")
	    (:port "Port" :integer :default 4242)
	    (:logo "Logo" :pathname :optional t)
	    (:stylesheet "Stylesheet" :url :optional t))
  (:section :general-settings "General settings"
	    (:title "Title" :text :default "CL-CONFIG configurator")
	    (:about "About" :text)))

(define-configuration standard-cl-config-web-configuration ()
  (:configuration-schema cl-config-web-configuration)
  (:title "Standard CL-CONFIG Web application configuration")
  (:documentation "Standard CL-CONFIG Web application configuration")
  (:section :general-settings
   (:about #.(with-open-file (s (asdf::system-relative-pathname :cl-config
							      "README.md"))
	       (let* ((len (file-length s))
		      (data (make-string len)))
		 (values data (read-sequence data s)))))))