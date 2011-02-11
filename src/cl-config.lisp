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

(defgeneric %validate-configuration-option
    (type configuration-option))

(defun validate-configuration-option (option)
  (%validate-configuration-option (type option) option))

(defgeneric %process-configuration-option-value
    (type configuration-option))

(defclass standard-configuration ()
  ())

(defclass configuration ()
  ((parents :initarg :parents
	    :accessor parents
	    :initform '(standard-configuration)
	    :documentation "Configuration mixins")
   (name :initarg :name
	 :accessor name
	 :initform (error "Provide a name for the configuration")
	 :documentation "The configuration name")
   (title :initarg :title
	  :accessor title
	  :initform (error "Provide a title for the configuration")
	  :documentation "Configuration title")
   (sections :initarg :sections
	     :accessor sections
	     :documentation "Configuration sections")
   (documentation :initarg :documentation
		  :accessor documentation*
		  :documentation "Configuration documentation"))
  (:documentation "A configuration"))

(defclass configuration-section ()
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
  (:documentation "A configuration section"))

(defclass configuration-option ()
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
	 :documentation "The option type"))
  (:documentation "A configuration option"))

(defclass configuration-option-type ()
  ((default :initarg :default
     :accessor default
     :documentation "The default value if there is one")))

(defun process-configuration-option-value (option)
  (%process-configuration-option-value (type option) option))

(defmethod %process-configuration-option-value ((type configuration-option-type)
						option)
  )  

(defclass one-of-configuration-option-type (configuration-option-type)
  ((options :accessor options))
  (:documentation "Choose one of the options"))

(defclass text-configuration-option-type (configuration-option-type)
  ()
  (:documentation "Fill in some text"))

(defclass list-configuration-option-type (configuration-option-type)
  ((options :accessor options))
  (:documentation "Choose a list of options"))

(defclass email-configuration-option-type (configuration-option-type)
  ()
  (:documentation "Fill an email address"))

(defclass url-configuration-option-type (configuration-option-type)
  ()
  (:documentation "Fill in an URL"))

(defclass pathname-configuration-option-type (configuration-option-type)
  ()
  (:documentation "Fill in a pathaname"))

(defclass configuration-option-value ()
  ((option :initarg :option
	   :documentation "The option referenced")
   (value :initarg :value
	  :accessor value
	  :documentation "The option value"))
  (:documentation "The value of a configuration option"))

(defclass configuration-option-type-item ()
  ((name :initarg :name
	 :accessor name
	 :initform (error "Provide the type-item name"))
   (title :initarg title
	  :accessor title
	  :initform (error "Provide the type-item title"))
   (configuration :initarg :configuration
		  :accessor configuration
		  :documentation "How to configure this item-type")))

(defmacro one-of (options &rest args)
  `(make-instance 'one-of-configuration-option-type
		  :options (list (mapcar #'make-configuration-type-item ',options))
		  ,@args))

(defmacro some-of (options &rest args)
  `(make-instance 'list-configuration-option-type
		  :options (list (mapcar #'make-configuration-type-item ',options))
		  ,@args))

(defmacro text (&rest args)
  `(make-instance 'text-configuration-option-type
		  ,@args))

(defmacro url (&rest args)
  `(make-instance 'url-configuration-option-type
		  ,@args))

(defmacro email (&rest args)
  `(make-instance 'email-configuration-option-type
		  ,@args))

(defmacro path-name (&rest args)
  `(make-instance 'pathname-configuration-option-type
		  ,@args))

(defun make-configuration-type-item (name title &rest args)
  (apply #'make-instance 'configuration-option-type-item
		 (append (list :name name
			       :title title)
			 args)))

(defmethod %validate-configuration-option ((type text-configuration-option-type)
					  option)
  (if (not (stringp (value option)))
      (validation-error option
			(format nil "~A should be a string for ~A" option type))))

(defmethod initialize-instance :after ((option configuration-option-value) &rest initargs)
  (declare (ignore initargs))
  (validate-configuration-option option))

(defun filter (predicate list)
  (reduce (lambda (result elem)
	    (if (funcall predicate elem)
		(cons elem result)
		result))
	  list :initial-value nil))

(defmacro define-configuration (name parents &rest args)
  (let ((sections (filter (lambda (elem)
			    (equalp (first elem) :section))
			  args))
	(title (first (filter (lambda (elem)
				(equalp (first elem) :title))
			      args)))
	(documentation (first (find :documentation args :key #'first))))
    `(make-instance 'configuration
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
					  (make-configuration-section name title body)))
				      ',sections))))

(defun make-configuration-section (name title args)
  (let ((documentation (find :documentation args :key #'first))
	(options (filter (lambda (elem)
			   (not (equalp (first elem) :documentation)))
			 args)))
    (make-instance 'configuration-section
		   :name name
		   :title title
		   :documentation documentation
		   :options (mapcar (lambda (option)
				      (destructuring-bind (name title expr)
					  option
					(make-configuration-option name title expr)))
				    options))))

(defun make-configuration-option (name title expr)
  (make-instance 'configuration-option
		 :name name
		 :title title
		 :type (eval expr)))
		 