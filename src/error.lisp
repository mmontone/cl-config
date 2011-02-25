(in-package :cl-config)

(define-condition validation-error ()
  ((target :initarg :target
	   :initform (error "Set up the target")
	   :accessor target)
   (error-msg :initarg :error-msg
	      :initform (error "Provide the error message")
	      :accessor error-msg))
  (:report (lambda (c s)
	     (format s "~A on ~A" (error-msg c) (target c)))))

(define-condition option-value-not-found-error ()
  ((option :initarg :option
	   :initform (error "Provide the option")
	   :accessor option)
   (configuration :initarg :configuration
		  :initform (error "Provide the configuration")
		  :accessor configuration))
  (:report (lambda (c s)
	     (format s "Option value for ~A was not found in ~A"
		     (option c) (configuration c)))))
   
(defun validation-error (target error-msg &rest args)
  (error 'validation-error
	 :target target
	 :error-msg (apply #'format nil (cons error-msg args))))

(defun option-value-not-found-error (option configuration)
  (error 'option-value-not-found-error
	 :option option
	 :configuration configuration))
