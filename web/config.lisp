(in-package :cfg.web)

(define-configuration-schema cl-config-web-configuration ()
  (:title "CL-CONFIG Web application configuration")
  (:documentation "CL-CONFIG Web application configuration schema")
  (:section :import/export "Import export settings"
	    (:import/export-filepath "Import/export filepath" :pathname :optional t)
	    (:import-on-load "Import on load" :boolean :default t)
	    (:export-on-save "Export on save" :boolean :default t)
	    (:backend "Backend" (:one-of (:sexp "SEXP backend")
					 (:xml "XML backend")
					 (:serialize "Serialize backend"))
		      :default :xml))
  (:section :webapp-configuration "Web application settings"
	    (:documentation "Web application configuration")
	    (:host "Host" :text :default "localhost")
	    (:port "Port" :integer :default 4242)
	    (:logo "Logo" :pathname :optional t)
	    (:stylesheet "Stylesheet" :url :optional t)))

(define-configuration standard-cl-config-web-configuration ()
  (:configuration-schema cl-config-web-configuration)
  (:title "Standard CL-CONFIG Web application configuration")
  (:documentation "Standard CL-CONFIG Web application configuration"))