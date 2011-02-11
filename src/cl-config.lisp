(in-package :cl-config)

(define-condition validation-error ()
  ((target :initarg :target
	   :initform (error "Set up the target")
	   :accessor target)
   (error-msg :initarg :error-msg
	      :initform (error "Provide the error message"))
   
(defun validation-error (target error-msg &rest args)
  (apply #'make-instance 'validation-error
	 (append
	  (list :target target
		:error-msg error-msg)
	  args)))

(defgeneric validate-configuration-option
    (configuration-option)
  )
	 
(defclass standard-configuration ()
  ())

(defclass configuration ()
  ((supers :initarg supers
	   :accessor supers
	   :initarg '(standard-configuration)
	   :documentation "Configuration mixins")
   (title :initarg :title
	  :accessor title
	  :initform (error "Provide a title for the configuration")
	  :documentation "Configuration title")
   (sections :accessor sections
	     :documentation "Configuration sections"))
  (:documentation "A configuration"))

(defclass configuration-section ()
  ((name :initarg :name
	 :accessor name
	 :initform (error "Provide a name for the section"))
   (title :initarg :title
	  :accessor title
	  :initform (error "Provide a title for the section"))
   (options :accessor options))
  (:documentation "A configuration section"))

(defclass configuration-option ()
  ((name :initarg :name
	 :accessor name
	 :initform (error "Provide a name for the option")
	 :documentation "The option name")
   (type :initarg :type
	 :accessor type
	 :initform (error "Provide the option type")
	 :documentation "The option type"))
  (:documentation "A configuration option"))

(defclass configuration-option-type ()
  ((default :accessor default
     :documentation "The default value if there is one")))

(defclass one-of-configuration-option-type (configuration-option-type)
  ((options :accessor options))
  (:documentation "Choose one of the options"))

(defclass text-configuration-option-type (configuration-option-type)
  ()
  (:documentation "Fill in some text"))

(defclass list-configuration-option-type (configuration-option-type)
  ((options :accessor options))
  (:documentation "Choose a list of options"))

(defclass configuration-option-value ()
  ((option :initarg :option
	   :documentation "The option referenced")
   (value :initarg :value
	  :documentation "The option value"))
  (:documentation "The value of a configuration option"))

(defclass configuration-option-type-item ()
  ((name :accessor name
	 :initarg (error "Provide the type-item name"))
   (title :accessor title
	  :initarg (error "Provide the type-item title"))
   (configuration :accessor configuration
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

(defun make-configuration-type-item ((name title &rest args))
  (apply #'make-instance 'configuration-option-type-item
		 (append (list :name name
			       :title title)
			 args)))

(defmethod validate-configuration-option ((type text-configuration-option-type)
					  option)
  (if (not (stringp (value option)))
      (validation-error (format nil "~A should be a string for ~A" option type))))

(defmethod initialize-instance :after ((option configuration-option-value))
  (validate-configuration-option (type option) option))

(defun filter (list predicate)
  (reduce (lambda (elem result)
	    (if (funcall predicate elem)
		(cons elem result)
		result))
	  list :initial-value nil))

(defmacro define-configuration (name supers &rest args)
  (let ((sections (filter (lambda (elem)
			    (equalp (first elem) :section))
			  args))
	(title (first (filter (lambda (elem)
				(equalp (first elem) :title))
			      args)))
	(documentation (first (filter (lambda (elem)
					(equalp (first elem) :documentation))
				      args))))
    `(make-instance 'configuration
		    :name ',name
		    :supers (list ,@supers)
		    :title ,title
		    :documentation ,documentation
		    :sections (mapcar (lambda (section)
					(destructuring-bind (:section name title &rest body)
					    section
					  (make-configuration-sectino name title body)))))))

(defun make-configuration-section (name title args)
  (let ((documentation (first (filter (lambda (elem)
					(equalp (first elem) :documentation))
				      args)))
	(options (filter (lambda (elem)
			   (not (equalp (first elem) :documentation)))
			 args)))
    (make-instance 'configuration-section
		 :name name
		 :title title
		 :documentation documentation
		 :options (mapcar #'make-configuration-option options))))  