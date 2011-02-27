(in-package :cfg.tests)

(def-suite cl-config)
(in-suite cl-config)

(defun reset ()
  (setf cl-config::*configurations* (make-hash-table :test #'equalp))
  (setf cl-config::*configuration-schemas* (make-hash-table :test #'equalp)))

(test test-xml-serialization
  (let ((writer (make-instance 'xml-writer))
	(reader (make-instance 'xml-reader)))

    (define-configuration-schema database-configuration-schema ()
      (:title "Database configuration")
      (:documentation "Database configuration")
      (:section :database-configuration "Database configuration"
		(:documentation "Section for configuring the database")
		(:connection-type "Connection type"
				  (:one-of (:socket "Socket")
					   (:tcp "TCP"))
				  :default :socket
				  :documentation "The connection type!!")
		(:email "Email"	:email :documentation "Email")
		(:username "Username" :text :documentation "The database engine username")
		(:password "Password" :text :documentation "The database engine password")
		(:database-name "Database name" :text)
		(:path "Pathname" :pathname)
		(:database-parameters "Database parameters" :text  :default "" :advanced t :optional t)))

    (let ((original-conf (find-configuration-schema 'database-configuration-schema)))
      (with-output-to-string (s)
	(serialize original-conf s writer)
	(let ((conf (unserialize s reader)))
	  (is (configuration-schema-equalp original-conf conf)))))))

(test test-inheritance)
(test test-optional-options)
(test test-required-options)
(test test-options-validation)
(test test-options-processing)