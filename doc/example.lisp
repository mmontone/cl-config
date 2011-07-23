(in-package :cfg)

(define-configuration-schema database-configuration ()
   (:title "Database configuration")
   (:documentation "Database configuration")
   (:section :database-configuration "Database configuration"
      (:documentation "Section for configuring the database")
      (:connection-type "Connection type"
          (:one-of (:socket "Socket"
			    :configuration 'db-socket-configuration)
		   (:tcp "TCP"
			 :configuration 'db-tcp-configuration)))
      (:username "Username" :text :documentation "The database engine username")
      (:password "Password" :text :documentation "The database engine password")
      (:name "Database name" :text)
      (:host "Database host" :text)
      (:database-parameters "Database parameters" :text :default "" :advanced t)))

(define-configuration-schema cl-config-application-configuration ()
  (:title "CL-CONFIG Application Configuration")
  (:documentation "CL-CONFIG Application Configuration")
  (:section :configuration-settings "Configuration settings"
	    (:load-configs-from-file "Load configurations from file"
				     :boolean :default t)
	    (:load-configs-file "Configurations file" :pathname :optional t)
	    (:select-config-from-file "Select configuration from file"
				      :boolean :default t)
	    (:select-config-file "Select configuration file" :pathname :optional t)))

(define-configuration-schema db-socket-configuration ()
    (:title "Socket configuration")
    (:section :db-socket-configuration "Socket configuration"
        (:path "Socket" :pathname
               :default "/tmp/socket.soc")))

(define-configuration-schema db-tcp-configuration ()
    (:title "TCP configuration")
    (:section "TCP configuration"
        (:url "URL" :url
              :default "localhost")))

(define-configuration-schema logging-configuration ()
    (:title "Logging configuration")
    (:documentation "Logging configuration")
    (:section :logging-configuration "Logging configuration"
        (:documentation "Logging configuration")
        (:backend "Backend"
            (:one-of (:log5 "Log5")))
	(:debugging-levels "Debugging levels" (:list (:info "Info")
						     (:warning "Warning")
						     (:profile "Profile")))
	(:output-location "Output location"
                    (:one-of (:standard-output "Standard output"
					       :default *standard-output*)
			     (:file "File" :default "/tmp/log.log"))
		    :default '*standard-output)
        (:active-layers "Active layers"
			(:list
			 (:debugging "Debugging"
				     :configuration 'debugging-layer)
			 (:database "Database"
				    :configuration database-layer)
			 (:control-flow "Control flow")
			 (:system "System")))))

(define-configuration-schema webapp-configuration (logging-configuration)
    (:title "Web application configuration")
    (:documentation "Web application configuration")
    (:section :webapp-configuration "Web application configuration"
        (:documentation "Web application configuration")
        (:http-server "HTTP server"
            (:one-of (:apache "Apache"
			      :configuration 'apache-configuration)
                    (:hunchentoot "Hunchentoot"
				  :configuration 'hunchentoot-configuration)))
        (:host "Host" :text :default "localhost")
	(:port "Port" :integer :default 8080)
	(:catch-errors "Catch errors" :boolean :default t)))

(define-configuration-schema view-configuration ()
   (:title "View configuration")
   (:documentation "View configuration")
   (:section :view-configuration "View configuration"
      (:documentation "Section for configuring the application view")
      (:renderer "Renderer" (:one-of (:http "Standard HTTP rendering")
				     (:ajax "Ajax rendering"))
		 :default :http)
      (:view-type "View type" (:one-of (:standard "Standard view type")
				       (:ctx "Context view type"))
		  :default :standard))
   (:section :cache-configuration "Cache configuration"
	     (:documentation "View cache configuration section. http://www.mnot.net/cache_docs")
	     (:active-cache "Active cache" :boolean :default nil
			    :documentation "Activates view cache")
	     (:expires "Expires" :text :default "")
	     (:max-age "Max age" :integer :default 0
		       :documentation "Specifies the maximum amount of time that an representation will be considered fresh")
	     (:s-max-age "Shared max age" :integer :default 0
			 :documentation "Similar to max-age, except that it only applies to shared (e.g., proxy) caches)")
	     (:public "Public" :boolean :default nil
		      :documentation "Marks authenticated responses as cacheable; normally, if HTTP authentication is required, responses are automatically private.")
	     (:private "Private" :boolean :default nil
		       :documentation "Allows caches that are specific to one user (e.g., in a browser) to store the response; shared caches (e.g., in a proxy) may not.")
	     (:no-cache "No cache" :boolean :default nil
			:documentation "Forces caches to submit the request to the origin server for validation before releasing a cached copy, every time. This is useful to assure that authentication is respected (in combination with public), or to maintain rigid freshness, without sacrificing all of the benefits of caching.")
	     (:no-store "No store" :boolean :default nil
			:documentation "Instructs caches not to keep a copy of the representation under any conditions.")
	     (:must-revalidate "Must revalidate" :boolean :default nil
			       :documentation "Tells caches that they must obey any freshness information you give them about a representation. HTTP allows caches to serve stale representations under special conditions; by specifying this header, you are telling the cache that you want it to strictly follow your rules.")
	     (:proxy-revalidate "Proxy revalidate" :boolean :default nil
				:documentation "Similar to must-revalidate, except that it only applies to proxy caches.")))
      
(define-configuration-schema standard-configuration
    (cl-config-application-configuration
     webapp-configuration
     database-configuration
     view-configuration)
      (:title "Standard configuration")
      (:documentation "Standard configuration for a Gestalt application")
      (:page-title "Page title" :text :default "Gestalt application"))

(define-configuration standard-configuration ()
  (:title "Standard configuration")
  (:configuration-schema standard-configuration)
  (:section :database-configuration
	    (:connection-type :socket
			      :value2
			      '(:db-socket-configuration
				(:path "/tmp/my-socket.soc")))
	    (:username "root")
	    (:password "root")
	    (:name "standard-database")
	    (:host "localhost"))
  (:section :webapp-configuration
	    (:http-server :hunchentoot))
  (:section :logging-configuration
	    (:active-layers (:debugging))
	    (:output-location :standard-output)
	    (:debugging-levels (:info))
	    (:backend :log5)))

(define-configuration debug-configuration (standard-configuration)
    (:configuration-schema standard-configuration)
    (:title "Debug configuration")
    (:section :database-configuration
        (:name "debug-database"))
    (:section :logging-configuration
       (:output-location :standard-output)
       (:active-layers (:debugging :database))
       (:debugging-levels (:info :warning :error)))
    (:section :webapp-configuration
	      (:catch-errors nil))
    (:documentation "Debugging configuration scheme"))

(define-configuration test-configuration (standard-configuration)
    (:configuration-schema standard-configuration)
    (:title "Test configuration")
    (:section :database-configuration
        (:name "test-database"))
    (:section :logging-configuration
       (:output-location :file :value2 "/tmp/test.log")
       (:active-layers (:debugging :database) :inherit t)
       (:debugging-levels (:warning :error)))
    (:documentation "Testing configuration scheme"))

;; (defapplication my-application (standard-application)
;;    ...
;;    (:configuration 'debug-configuration-scheme))


;; Installer example

(defmacro configure-section (section configuration options)
  (let ((fname (gensym "CONFIGURE-SECTION-")))
  `(labels ((,fname ()
	    (with-input ,(mapcar (lambda (option)
			  (intern (symbol-name option)))
			options)
	      (collecting-validation-errors (errors found-p)
		  (progn
		    ,@(loop for option in options
			 collect
			   `(setf (get-option-value (list ,section ,option)
						    ,configuration)
				  ,(intern (symbol-name option)))))
		(when found-p
		  (install-errors errors)
		  (,fname))))))
     (,fname))))

(define-wizard-installer my-installer
    (:title "My installer"
	    :documentation "This is my installer")
  (let ((configuration (cfg:with-schema-validation (nil)
			 (make-configuration my-config ()
					     (:title "My config")
					     (:configuration-schema standard-configuration)))))

    ;; Configure web section
    (start-section :webapp-configuration "Web configuration")
    (configure-section :webapp-configuration configuration
		       (:http-server :host :port))
    
    ;; Configure the database section
    (start-section :database-configuration "Database configuration")
    (configure-section :database-configuration configuration
		       (:name :host :username :password :connection-type))

    ;; Configure logging section
    (start-section :logging-configuration "Logging configuration")
    (configure-section :logging-configuration configuration
		       (:backend :debugging-levels :output-location :active-layers))
    (validate-configuration configuration)
    configuration))