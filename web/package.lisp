(defpackage cl-config.web
  (:nicknames :cfg.web)
  (:use :cl :cl-config :hunchentoot :cl-who :parenscript)
  (:export #:start-cl-config-web
	   #:stop-cl-config-web))