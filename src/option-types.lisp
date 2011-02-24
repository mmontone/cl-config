(in-package :cl-config)

(defvar *option-types* (make-hash-table :test #'equal))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-configuration-schema-option-type (type args &body body)
  `(setf (gethash ',type *option-types*)
	 (lambda ,args ,@body)))
  
  (defmacro define-option-validator (type (value &optional (option (gensym "OPTION-")))
				   condition error-msg &rest args)
  (let ((result (gensym "RESULT-")))
    `(defmethod %validate-configuration-option ((type ,type) ,option)
       (let ((,value (value ,option)))
	 (let ((,result ,condition))
	   (if (not ,result)
	       (validation-error ,option
				 (format nil ,error-msg ,@args))
	       t)))))))

(defclass configuration-schema-option-type ()
  ())

(defclass one-of-configuration-schema-option-type (configuration-schema-option-type)
  ((options :initarg :options
	    :accessor options))
  (:documentation "Choose one of the options"))

(defclass text-configuration-schema-option-type (configuration-schema-option-type)
  ()
  (:documentation "Fill in some text"))

(defclass list-configuration-schema-option-type (configuration-schema-option-type)
  ((options :accessor options))
  (:documentation "Choose a list of options"))

(defclass email-configuration-schema-option-type (configuration-schema-option-type)
  ()
  (:documentation "Fill an email address"))

(defclass url-configuration-schema-option-type (configuration-schema-option-type)
  ()
  (:documentation "Fill in an URL"))

(defclass pathname-configuration-schema-option-type (configuration-schema-option-type)
  ()
  (:documentation "Fill in a pathaname"))

(defclass configuration-schema-option-type-item ()
  ((name :initarg :name
	 :accessor name
	 :initform (error "Provide the type-item name"))
   (title :initarg :title
	  :accessor title
	  :initform (error "Provide the type-item title"))
   (configuration :initarg :configuration
		  :accessor configuration
		  :initform nil
		  :documentation "How to configure this item-type")))

(defmethod print-object ((item configuration-schema-option-type-item) stream)
  (print-unreadable-object (item stream :type t :identity t)
    (format stream "~A ~S" (name item) (title item))))

(define-configuration-schema-option-type :one-of (&rest options)
  (make-instance
   'one-of-configuration-schema-option-type
   :options (mapcar
	     (lambda (option)
	       (destructuring-bind (name title &rest args)
		   option
		 (apply #'make-configuration-schema-type-item
			name (cons title args))))
	     options)))

(define-configuration-schema-option-type :some-of (options &rest args)
  (apply #'make-instance 'list-configuration-schema-option-type
	 (append
	  (list
	   :options (list (mapcar #'make-configuration-schema-type-item options)))
	 args)))

(define-configuration-schema-option-type :text (&rest args)
  (apply #'make-instance 'text-configuration-schema-option-type
	 args))

(define-configuration-schema-option-type :url (&rest args)
  (apply #'make-instance 'url-configuration-schema-option-type
	 args))

(define-configuration-schema-option-type :email (&rest args)
  (apply #'make-instance 'email-configuration-schema-option-type
	 args))

(define-configuration-schema-option-type :pathname (&rest args)
  (apply #'make-instance 'pathname-configuration-schema-option-type
	 args))

(defun make-configuration-schema-type-item (name title &rest args)
  (apply #'make-instance 'configuration-schema-option-type-item
		 (append (list :name name
			       :title title)
			 args)))

(defmethod %validate-configuration-option ((type configuration-schema-option-type) option)
  t)

(define-option-validator text-configuration-schema-option-type
    (value option)
  (stringp value)
  "~A should be a string for ~A" value option)

(define-option-validator one-of-configuration-schema-option-type
    (value option)
  (some (lambda (v)
	  (equalp v value))
	(loop
	   for opt in (options (option-type (schema-option option)))
	   collect (name opt)))
  "~A value should be one of ~A"
  value
  (loop
     for opt in (options (option-type (schema-option option)))
     collect (name opt))
  option)