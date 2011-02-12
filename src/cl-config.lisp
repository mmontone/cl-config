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
   
(defun validation-error (target error-msg)
  (error 'validation-error
	 :target target
	 :error-msg error-msg))

(defgeneric %validate-configuration-schema-option
    (type configuration-schema-option))

(defun validate-configuration-schema-option (option)
  (%validate-configuration-schema-option (type option) option))

(defgeneric %process-configuration-schema-option-value
    (type configuration-schema-option))

(defclass standard-configuration-schema ()
  ())

(defvar *configuration-schemas* (make-hash-table :test #'equal))

(defclass configuration-schema ()
  ((parents :initarg :parents
	    :accessor parents
	    :initform '(standard-configuration-schema)
	    :documentation "Configuration-Schema mixins")
   (name :initarg :name
	 :accessor name
	 :initform (error "Provide a name for the configuration-schema")
	 :documentation "The configuration-schema name")
   (title :initarg :title
	  :accessor title
	  :initform (error "Provide a title for the configuration-schema")
	  :documentation "Configuration-Schema title")
   (sections :initarg :sections
	     :accessor sections
	     :documentation "Configuration-Schema sections")
   (documentation :initarg :documentation
		  :accessor documentation*
		  :documentation "Configuration-Schema documentation"))
  (:documentation "A configuration-schema"))

(defclass configuration-schema-section ()
  ((name :initarg :name
	 :accessor name
	 :initform (error "Provide a name for the section"))
   (title :initarg :title
	  :accessor title
	  :initform (error "Provide a title for the section"))
   (options :initarg :options
	    :accessor options)
   (documentation :initarg :documentation
		  :accessor documentation*))
  (:documentation "A configuration-schema section"))

(defclass configuration-schema-option ()
  ((name :initarg :name
	 :accessor name
	 :initform (error "Provide a name for the option")
	 :documentation "The option name")
   (title :initarg :title
	  :accessor title
	  :initform (error "Provide a title for the option"))
   (type :initarg :type
	 :accessor option-type
	 :initform (error "Provide the option type")
	 :documentation "The option type")
   (default :initarg :default
     :initform nil
     :accessor default
     :documentation "The default value if there is one")
   (advanced :initarg :advanced
	     :initform nil
	     :accessor advanced
	     :documentation "t when this is an advanced option")
   (documentation :initarg :documentation
		  :initform nil
		  :accessor documentation*
		  :documentation "The option documentation string"))
  (:documentation "A configuration-schema option"))

(defvar *option-types* (make-hash-table :test #'equal))

(defmacro define-configuration-schema-option-type (type args &body body)
  `(setf (gethash ',type *option-types*)
	 (lambda ,args ,@body)))

(defclass configuration-schema-option-type ()
  ())

(defun process-configuration-schema-option-value (option)
  (%process-configuration-schema-option-value (type option) option))

(defmethod %process-configuration-schema-option-value ((type configuration-schema-option-type)
						option)
  )

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

(defclass configuration-schema-option-value ()
  ((option :initarg :option
	   :documentation "The option referenced")
   (value :initarg :value
	  :accessor value
	  :documentation "The option value"))
  (:documentation "The value of a configuration-schema option"))

(defclass configuration-schema-option-type-item ()
  ((name :initarg :name
	 :accessor name
	 :initform (error "Provide the type-item name"))
   (title :initarg :title
	  :accessor title
	  :initform (error "Provide the type-item title"))
   (configuration :initarg :configuration
		  :accessor configuration
		  :documentation "How to configure this item-type")))

(define-configuration-schema-option-type :one-of (&rest options)
  (make-instance
   'one-of-configuration-schema-option-type
   :options (list (mapcar
		   (lambda (option)
		     (destructuring-bind (name title &rest args)
			 option
		       (apply #'make-configuration-schema-type-item
			      name (cons title args))))
		   options))))

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

(defmacro path-name (&rest args)
  `(make-instance 'pathname-configuration-schema-option-type
		  ,@args))

(defun make-configuration-schema-type-item (name title &rest args)
  (apply #'make-instance 'configuration-schema-option-type-item
		 (append (list :name name
			       :title title)
			 args)))

(defmethod %validate-configuration-schema-option ((type text-configuration-schema-option-type)
					  option)
  (if (not (stringp (value option)))
      (validation-error option
			(format nil "~A should be a string for ~A" option type))))

(defmethod initialize-instance :after ((option configuration-schema-option-value) &rest initargs)
  (declare (ignore initargs))
  (validate-configuration-schema-option option))

(defun filter (predicate list)
  (reduce (lambda (result elem)
	    (if (funcall predicate elem)
		(cons elem result)
		result))
	  list :initial-value nil))

(defmacro define-configuration-schema (name parents &rest args)
  (let ((sections (filter (lambda (elem)
			    (equalp (first elem) :section))
			  args))
	(title (first (filter (lambda (elem)
				(equalp (first elem) :title))
			      args)))
	(documentation (first (find :documentation args :key #'first))))
    `(setf (gethash ',name *configuration-schemas*)
	   (make-instance 'configuration-schema
		    :name ',name
		    :parents (list ,@parents)
		    :title ,(second title)
		    :documentation ,(if documentation
					documentation
					"")
		    :sections (mapcar (lambda (section)
					(destructuring-bind (_ name title &rest body)
					    section
					  (declare (ignore _))
					  (make-configuration-schema-section name title body)))
				      ',sections)))))

(defun make-configuration-schema-section (name title args)
  (let ((documentation (find :documentation args :key #'first))
	(options (filter (lambda (elem)
			   (not (equalp (first elem) :documentation)))
			 args)))
    (make-instance 'configuration-schema-section
		   :name name
		   :title title
		   :documentation documentation
		   :options
		   (mapcar
		    (lambda (option)
		      (destructuring-bind (name title type-spec &rest rest)
			  option
			(apply #'make-configuration-schema-option
			       (append (list name title type-spec)
				       rest))))
		    options))))

(defun make-configuration-schema-option (name title type-spec &rest args)
  (apply #'make-instance 'configuration-schema-option
	 (append
	  (list :name name
		:title title
		:type (make-configuration-schema-option-type type-spec))
	  args)))

(defun make-configuration-schema-option-type (type-spec)
  (multiple-value-bind (key params)
      (if (listp type-spec)
	  (values
	   (first type-spec)
	   (rest type-spec))
	  (values
	   type-spec nil))
    (multiple-value-bind (builder found)
	(gethash key *option-types*)
      (if (not found)
	  (error "Found no definition for type ~A" key)
	  (apply builder params)))))
		 