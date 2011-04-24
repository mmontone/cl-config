(in-package :cl-config)

(defmacro collecting-validation-errors ((errors found-p) expr &body body)
  `(multiple-value-bind (,errors ,found-p)
       (%collecting-validation-errors
	(lambda () ,expr))
     ,@body))

(define-condition validation-error ()
  ((target :initarg :target
	   :initform (error "Set up the target")
	   :accessor target)
   (error-msg :initarg :error-msg
	      :initform (error "Provide the error message")
	      :accessor error-msg))
  (:report (lambda (c s)
	     (format s "~A on ~A" (error-msg c) (target c)))))

(defmethod print-object ((error validation-error) stream)
  (print-unreadable-object (error stream :type t :identity t)
    (format stream "~A on ~A" (error-msg error) (target error))))

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
  (cerror "Skip validation"
	  'validation-error
	 :target target
	 :error-msg (apply #'format nil (cons error-msg args))))

(defun option-value-not-found-error (option configuration)
  (error 'option-value-not-found-error
	 :option option
	 :configuration configuration))

(defun %collecting-validation-errors (func)
  (let ((errors nil))
    (handler-bind
	((cfg::validation-error
	  (lambda (c)
	    (push c errors)
	    (continue c))))
      (funcall func))
    (values errors (plusp (length errors)))))