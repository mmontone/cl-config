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
      (:database-name "Database name" :text)
      (:database-parameters "Database parameters" :text :default "" :advanced t)))

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
	(:debugging-level "Debugging level" (:list (:info "Info")
						   (:warning "Warning")
						   (:profile "Profile")))
	(:output-location "Output location"
                    (:one-of (:standard-output "Standard output"
					       :default *standard-output*)
			     (:file "File" :default "/tmp/log.log")))
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
            (one-of (:apache "Apache" (:configuration 'apache-configuration))
                    (:hunchentoot "Hunchentoot" (:configuration 'hunchentoot-configuration))))
        (:host "Host" :text :default "localhost")))                    


(define-configuration-schema standard-configuration
                        (webapp-configuration database-configuration)
      (:documentation "Standard configuration for a Gestalt application")
      (:page-title "Page title" :type :text :default "Gestalt application"))

(define-configuration standard-configuration ()
  (:title "Standard configuration")
  (:configuration standard-configuration)
   (:database-configuration
       (:connection-type :socket
           (:db-socket-configuration
              (:path "/tmp/my-socket.soc")))
       (:username "root")
       (:password "root")
       (:database-name "standard-database"))
   (:webapp-configuration
       (:host "localhost")
       (:http-server :hunchentoot)))

(define-configuration-scheme debug-configuration-scheme (standard-configuration-scheme)
    (:configuration standard-configuration)
    (:database-configuration
        (:database-name "debug-database"))
    (:logging-configuration
       (:output-location :file "/tmp/debug.log")
       (:active-layers :debugging :database
           (:debugging-levels :info :warning :error)))
    (:documentation "Debugging configuration scheme"))

(define-configuration-scheme test-configuration-scheme (standard-configuration-scheme)
    (:configuration standard-configuration)
    (:database-configuration
        (:database-name "test-database"))
    (:logging-configuration
       (:output-location :file "/tmp/test.log")
       (:active-layers :debugging :database
           (:debugging-levels :warning :error)))
    (:documentation "Testing configuration scheme"))

(defapplication my-application (standard-application)
   ...
   (:configuration 'debug-configuration-scheme))

