(in-package :cfg)

(defmacro define-installer (name (&key title
				       configuration-schema
				       backend
				       output-file
				       editor-mode) &body body)
  
  `(defun ,name (&key (configuration-schema
		       ,(or configuration-schema
			    `(error "Provide the configuration schema")))
		      (backend ,(or backend :sexp))
		      (output-file ,(or output-file
					`(error "Provide the output file")))
		 (editor-mode ,(or editor-mode :repl)))
     (declare (ignorable configuration-schema backend output-file editor-mode))
     ,@body))

(defclass installer ()
  ((title :initform (error "Provide the title")
	  :accessor title)
   (configuration-schema :initform (error "Provide the configuration schema")
			 :accessor configuration-schema)
   (backend :initform (error "Provide the backend")
	    :accessor backend)
   (output-file :initform (error "Provide the output file")
		:accessor output-file)
   (pre-install-code :initform #'identity
		     :accessor pre-install-code)
   (post-install-code :initform #'identity
		      :accessor post-install-code)))

(defun repl-installer (installer)
  (handler-bind
      ((validation-error (lambda (e)
			   (continue))))
    (make-configuration :installer '()
			(list :title (title installer)
			      :configuration-schema (configuration-schema installer)))))