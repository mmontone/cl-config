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
				 ,error-msg ,@args)
	       t))))))

  (defmacro define-option-processor (type (value &optional (option (gensym "OPTION-")))
				     &body body)
    `(defmethod %process-configuration-option ((type ,type) ,option)
       (let ((,value (value ,option)))
	 (setf (value ,option)
	       (progn ,@body))))))

(defclass configuration-schema-option-type ()
  ())

(defclass boolean-configuration-schema-option-type (configuration-schema-option-type)
  ()
  (:documentation "The possible values are nil or t"))

(defmethod title ((type boolean-configuration-schema-option-type))
  "Boolean")

(defclass sexp-configuration-schema-option-type (configuration-schema-option-type)
  ()
  (:documentation "An sexp as configuration"))

(defmethod title ((type sexp-configuration-schema-option-type))
  "SEXP")

(defclass maybe-configuration-schema-option-type (configuration-schema-option-type)
  ((type :initarg :type
	 :accessor type*
	 :initform (error "Provide the type")))
  (:documentation "Maybe the type"))

(defmethod title ((type maybe-configuration-schema-option-type))
  "Maybe")
	  
(defclass one-of-configuration-schema-option-type (configuration-schema-option-type)
  ((options :initarg :options
	    :accessor options))
  (:documentation "Choose one of the options"))

(defmethod title ((type one-of-configuration-schema-option-type))
  "One of")

(defclass text-configuration-schema-option-type (configuration-schema-option-type)
  ()
  (:documentation "Fill in some text"))

(defmethod title ((type text-configuration-schema-option-type))
  "Text")

(defclass list-configuration-schema-option-type (configuration-schema-option-type)
  ((options :initarg :options
	    :accessor options))
  (:documentation "Choose a list of options"))

(defmethod title ((type list-configuration-schema-option-type))
  "List")

(defclass email-configuration-schema-option-type (configuration-schema-option-type)
  ()
  (:documentation "Fill an email address"))

(defmethod title ((type email-configuration-schema-option-type))
  "Email")

(defclass url-configuration-schema-option-type (configuration-schema-option-type)
  ()
  (:documentation "Fill in an URL"))

(defmethod title ((type url-configuration-schema-option-type))
  "URL")

(defclass pathname-configuration-schema-option-type (configuration-schema-option-type)
  ()
  (:documentation "Fill in a pathaname"))

(defmethod title ((type pathname-configuration-schema-option-type))
  "Pathname")

(defclass configuration-schema-option-type-item ()
  ((name :initarg :name
	 :accessor name
	 :initform (error "Provide the type-item name"))
   (title :initarg :title
	  :accessor title
	  :initform (error "Provide the type-item title"))
   (default :initarg :default
     :accessor default
     :initform nil)
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

(define-configuration-schema-option-type :list (&rest options)
  (make-instance
   'list-configuration-schema-option-type
   :options (mapcar
	     (lambda (option)
	       (destructuring-bind (name title &rest args)
		   option
		 (apply #'make-configuration-schema-type-item
			name (cons title args))))
	     options)))

(define-configuration-schema-option-type :maybe (&rest args)
  (make-instance 'maybe-configuration-schema-option-type
		 :type (apply #'make-configuration-schema-option-type args)))  

(define-configuration-schema-option-type :boolean (&rest args)
  (declare (ignore args))
  (make-instance 'boolean-configuration-schema-option-type))

(define-configuration-schema-option-type :sexp (&rest args)
  (declare (ignore args))
  (make-instance 'sexp-configuration-schema-option-type))

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

(define-option-validator maybe-configuration-schema-option-type
    (value option)
  (or (null value)
      (%validate-configuration-option (type* (option-type option))
				      option))
  "Maybe type error")

(define-option-validator boolean-configuration-schema-option-type
    (value)
  (or (null value) (equalp t value))
  "~A should be nil or t" value)

(define-option-validator text-configuration-schema-option-type
    (value option)
  (stringp value)
  "~A should be a string" value)

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

(define-option-validator list-configuration-schema-option-type
    (value option)
  (some (lambda (v)
	  (find v value))
	(loop
	   for opt in (options (option-type (schema-option option)))
	   collect (name opt)))
  "~A value should be one of ~A"
  value
  (loop
     for opt in (options (option-type (schema-option option)))
     collect (name opt))
  option)

(defun valid-mail-address-p (address)
  "Simple e-mail address validation.  Given ADDRESS, as a string,
returns non-NIL if it appears to be valid."
  (declare (type string address))
  (if (and (>= (length address) 6)
	   (every (lambda (char)
		    (or (alphanumericp char)
			(find char "+-.@_")))
		  address)
	   (find #\@ address))
      t
      nil))

(define-option-validator email-configuration-schema-option-type
    (value option)
  (valid-mail-address-p value)
  "~A is not a valid email address in ~A" value option)

(define-option-processor pathname-configuration-schema-option-type
    (value)
  (pathname value))

(define-option-validator pathname-configuration-schema-option-type
    (value)
  (pathnamep value)
  "~A is not a pathname" value)

(define-option-processor sexp-configuration-schema-option-type
    (value)
  (with-input-from-string (s value)
    (let ((val (read s)))
      (lambda ()
	(eval val)))))