(in-package :cl-config)

(defclass configuration ()
  ((name :initarg :name
	 :accessor name
	 :initform (error "Provide a name for the configuration")
	 :documentation "The configuration name")
   (parents :initarg :parents
	    :accessor parents
	    :initform nil)
   (title :initarg :title
	  :accessor title
	  :initform (error "Provide a title for the configuration")
	  :documentation "Configuration title")
   (configuration-schema :initarg :configuration-schema
			 :accessor configuration-schema
			 :initform (error "Provide the configuration schema"))
   (direct-sections :initarg :direct-sections
		    :accessor direct-sections
		    :initform (error "Provide the direct-sections"))
   (documentation :initarg :documentation
		  :accessor documentation*
		  :initform "")))

(defmethod print-object ((configuration configuration) stream)
  (print-unreadable-object (configuration stream :type t :identity t)
    (format stream "~A ~S"
	    (name configuration)
	    (title configuration))))

(defclass configuration-section ()
  ((name :initarg :name
	 :accessor name
	 :initform (error "Provide a name for the section"))
   (options :initarg :options
	    :accessor options
	    :initform (make-hash-table :test #'equalp))))

(defclass configuration-option ()
  ((schema-option :initarg :schema-option
		  :accessor schema-option
		  :initform (error "Set up the schema-option")
		  :documentation "The schema option referenced")
   (value :initarg :value
	  :accessor value
	  :initform (error "Provide the option value")
	  :documentation "The option value"))
  (:documentation "The value of a configuration-schema option"))

(defgeneric %validate-configuration-option
    (type configuration-option))

(defun validate-configuration-option (option)
  (%validate-configuration-option (option-type (schema-option option)) option))

(defgeneric %process-configuration-option
    (type configuration-option))

(defun process-configuration-option (option)
  (%process-configuration-option (option-type (schema-option option)) option))

(defmethod %process-configuration-option ((type configuration-schema-option-type)
					  option)
  )

(defvar *configuration* nil)

(defmethod initialize-instance :around ((configuration configuration) &rest initargs)
  (declare (ignore initargs))
  (let ((*configuration* configuration))
    (call-next-method)))

(defmethod initialize-instance :after ((option configuration-option) &rest initargs)
  (declare (ignore initargs))
  (process-configuration-option option)
  (validate-configuration-option option))

(defmethod initialize-instance :after ((section configuration-section) &rest initargs)
  (let ((options (make-hash-table :test #'equalp)))
    (loop for (name value) in (getf initargs :options)
	 do (setf (gethash name options)
		  (make-instance 'configuration-option
				 :schema-option (find-configuration-schema-option
						 (configuration-schema *configuration*)
						 (list (name section) name))
				 :value value)))
    (setf (options section) options)))

(defun find-configuration-section (configuration section-name)
  (multiple-value-bind (section found)
      (gethash section-name (sections configuration))
    (if (not found)
	(error "Section ~A not found in ~A" section-name configuration)
	section)))

(defun find-configuration-section-option (configuration-section option-name)
  (multiple-value-bind (option found)
      (gethash option-name (options configuration-section))
    (if (not found)
	(error "Option ~A not found in ~A" option-name configuration-section)
	option)))

(defun find-configuration-option (configuration option-path)
  (destructuring-bind (section-name option-name)
      option-path
    (let ((section (find-configuration-section configuration section-name)))
      (find-configuration-section-option section option-name))))

(defmethod print-object ((section configuration-section) stream)
  (print-unreadable-object (section stream :type t :identity t)
    (format stream "~A"
	    (name section))))

(defun validate-configuration (configuration)
  "Validates a configuration against its schema"
  )

(defmethod initialize-instance :after ((configuration configuration) &rest initargs)
  (setf (direct-sections configuration)
	(loop for section-spec in (getf initargs :direct-sections)
	   for section = (destructuring-bind (_ name &rest options) section-spec
			   (declare (ignore _))
			   (make-instance 'configuration-section
					  :name name
					  :options options))
	   collect section))
  (validate-configuration configuration))

(defvar *configurations* (make-hash-table :test #'equalp))

(defmacro define-configuration (name parents &rest args)
  (let ((direct-sections (filter (lambda (elem)
			    (equalp (first elem) :section))
			  args))
	(title (second (find :title args :key #'first)))
	(configuration-schema (second (find :configuration-schema args :key #'first)))
	(documentation (second (find :documentation args :key #'first))))
    `(setf (gethash ',name *configurations*)
	   (make-instance 'configuration
			  :name ',name
			  :parents ',parents
			  :title ,title
			  :configuration-schema (find-configuration-schema ',configuration-schema)
			  :direct-sections ',direct-sections
			  :documentation ,documentation))))
			       
(defun get-option-value (option-path configuration)
  (assert (listp option-path))
  (let ((section-name (first option-path))
	(option-name (second option-path)))
    (let ((sections (find-sections section-name configuration)))
      (loop for section in sections
	   do (multiple-value-bind (value found)
		  (get-section-option-value option-name section)
		(if found
		    (return-from get-option-value (values value t)))))))
  (values nil nil))

(defun get-section-option-value (option section)
  (loop for section-option in section
     do (let ((option-name (first section-option)))
	  (when (equalp option-name option)
	    (return-from get-section-option-value
	      (values (second section-option) t)))))
  (values nil nil))

(defun make-configuration-section (name options)
  (make-instance 'configuration-section
		 :name name
		 :options
		 (mapcar
		  (lambda (option)
		    (destructuring-bind (name value)
			option
		      (make-configuration-option name value)))
		  options)))

(defun make-configuration-option (name title type-spec &rest args)
  (apply #'make-instance 'configuration-schema-option
	 (append
	  (list :name name
		:title title
		:type (make-configuration-schema-option-type type-spec))
	  args)))