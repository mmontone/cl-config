(in-package :cfg.web)

(define-configuration-schema cl-config-web-configuration ()
  (:title "CL-CONFIG Web application configuration")
  (:documentation "CL-CONFIG Web application configuration")
  (:section :webapp-configuration "Web application configuration"
	    (:documentation "Web application configuration")
	    (:host "Host" :text :default "localhost")
	    (:port "Port" :integer :default 8080)
	    (:logo "Logo" :pathname
		   :default (asdf:system-relative-pathname :cl-config-web
							   "web/logo.jpg"))))