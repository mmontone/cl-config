(in-package :cl-config)

(defvar *configuration* nil "The current configuration. Use with-configuration macro to set this")

(defvar *configurations* (make-hash-table :test #'equalp)
  "The defined configurations. Use find-configuration to access configurations by name")

(defvar *schema-validation* t "Whether to validate a configuration (on creation) or not. Use with-schema-validation to set this")

(defvar *configuration-validators* (make-hash-table :test #'equalp)
  "Collection of configurations validators. Use define-configuration-validator to define new validators on a configuration")

(defmacro with-configuration (configuration-name &body body)
  "Executes body in the context of the given configuration
   Example:

   (with-configuration test-configuration
       (cfg (:database-configuration :username)))"
  `(let ((*configuration* ',configuration-name))
     ,@body))

(defmacro define-configuration-validator (configuration-schema (configuration) &body body)
  "Defines a validator on a configuration.

   Example:

   (cfg::define-configuration-validator postgres-database-configuration (configuration)
     (cfg:with-configuration-section :database-configuration 
       (cfg:with-configuration-values
            (database-name username password host) configuration
           (handler-bind 
	     (postmodern:connect database-name username password host)
	        (postmodern:database-error (error)
		  (cfg::validation-error		   
		   (cl-postgres::message error)))))))"
  
  (let ((validator-name (intern (format nil "~A-VALIDATOR" configuration-schema))))
    `(flet ((,validator-name (,configuration)
	      ,@body))
       (setf (gethash ',configuration-schema *configuration-validators*)
	     #',validator-name))))

(defun %with-schema-validation (value func)
  (let ((*schema-validation* value))
    (funcall func)))

(defmacro with-schema-validation ((&optional value) &body body)
  "Executes body validating or or not the configurations created in body context (depending the value of value).
   The default when using this macro is to not validate.
   This macro is more commonly used for internal implementation options.

   Example:

   (with-schema-validation (nil)
       (setf (cfg :database-configuration.username) 2323))"
  `(%with-schema-validation ,value (lambda () ,@body)))

(defun find-configuration (name)
  "Get a configuration by its name"
  (multiple-value-bind (configuration found-p)
      (gethash name *configurations*)
    (if found-p
	configuration
	(error "Configuration ~A not defined" name))))

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

(defmethod name ((option configuration-option))
  (name (schema-option option)))

(defclass standard-configuration-option (configuration-option)
  ())

(defclass list-configuration-option (configuration-option)
  ((inherit :initarg :inherit
	    :accessor inherit
	    :initform nil
	    :documentation "Whether to inherit or not list elements from parent option")))

(defclass one-of-configuration-option (configuration-option)
  ((value2 :initarg :value2
	   :accessor value2
	   :initform nil
	   :documentation "Chosen option value")))

(defgeneric option-class (option-type)
  (:method ((option-type configuration-schema-option-type))
    'standard-configuration-option))

(defmethod option-class ((option-type list-configuration-schema-option-type))
  'list-configuration-option)

(defmethod option-class ((option-type one-of-configuration-schema-option-type))
  'one-of-configuration-option)

(defmethod print-object ((option configuration-option) stream)
  (print-unreadable-object (option stream :type t :identity t)
    (format stream "~A ~A" (schema-option option) (value option))))

(defmethod print-object ((option list-configuration-option) stream)
  (print-unreadable-object (option stream :type t :identity t)
    (format stream "~A ~A inherit: ~A"
	    (schema-option option)
	    (value option)
	    (inherit option))))

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

(defmethod initialize-instance :around ((configuration configuration) &rest initargs)
  (let ((*configuration* configuration))
    (call-next-method)))

(defmethod initialize-instance :after ((configuration configuration) &rest initargs)
  (let ((direct-sections (make-hash-table :test #'equalp)))
    (loop for section-spec in (getf initargs :direct-sections)
	  do
	 (destructuring-bind (_ name &rest options) section-spec
	   (declare (ignore _))
	   (setf (gethash name direct-sections)
		 (make-instance 'configuration-section
				:name name
				:options options))))
    (setf (direct-sections configuration) direct-sections)
    (if (and *schema-validation*
	     (not (partial (configuration-schema configuration))))
	(validate-configuration configuration))))

(defmethod initialize-instance :after ((option configuration-option) &rest initargs)
  (declare (ignore initargs))
  (when *schema-validation*
    (process-configuration-option option)
    (validate-configuration-option option)))

(defmethod initialize-instance :after ((section configuration-section) &rest initargs)
  (let ((options (make-hash-table :test #'equalp)))
    (loop for opt in (getf initargs :options)
       do (destructuring-bind (name value &rest args) opt
	    (let ((schema-option
		   (find-configuration-schema-option
		    (configuration-schema *configuration*)
		    (list (name section) name))))
	      (setf (gethash name options)
		    (apply #'make-instance
			   (option-class (option-type schema-option))
			   (append
			    (list :schema-option schema-option
				  :value value)
			    args)))))
    (setf (options section) options))))

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

(defun validate-configuration-validators (configuration)
  "Apply validators to configuration"
  (let ((configuration-schema (configuration-schema configuration)))
    (loop for schema in (cons (cfg::name configuration-schema)
			      (mapcar #'cfg::name
				      (ordered-parents configuration-schema)))
       when (gethash schema *configuration-validators*)
       do (funcall (gethash schema *configuration-validators*)
		   configuration))))

(defun validate-configuration-values (configuration)
  "Validate values"
  (loop for section being the hash-values of (direct-sections configuration)
       do
       (loop for option being the hash-values of (options section)
	  do (validate-configuration-option option)))
  ;(loop for parent in (mapcar #'find-configuration (parents configuration))
  ;   do (validate-configuration-values parent))
  )

(defun validate-configuration-schema (configuration)
  "Validates a configuration against its schema"
  (loop for schema-section being the hash-values of
       (sections (configuration-schema configuration))
       do (loop for option-schema being the hash-values of
	       (direct-options schema-section)
	       do (if (not (or (optional option-schema)
			       (default option-schema)))
		      (let ((option-path (list (name schema-section)
					       (name option-schema))))
			(handler-case
			    (get-option-value
			     option-path
			     configuration)
			  (option-value-not-found-error ()
			    (validation-error configuration
					      "Value for ~A not found"
					      option-path))))))))

(defun validate-configuration (configuration)
  (validate-configuration-schema configuration)
  (validate-configuration-values configuration)
  (validate-configuration-validators configuration))

(defmacro make-configuration (name parents &rest args)
  "Create a configuration without registering globally"
  (let ((direct-sections (filter (lambda (elem)
			    (equalp (first elem) :section))
			  args))
	(title (second (find :title args :key #'first)))
	(configuration-schema (second (find :configuration-schema args :key #'first)))
	(documentation (second (find :documentation args :key #'first))))
    `(make-instance 'configuration
		    ,@(if name
			  `(:name ',name))
		    :parents ',parents
		    ,@(if title
			  `(:title ,title))
		    ,@(if configuration-schema
			  `(:configuration-schema (find-configuration-schema ',configuration-schema)))
		    :direct-sections ',direct-sections
		    ,@(if documentation
			  `(:documentation ,documentation)))))

(defmacro define-configuration (name parents &rest args)
  "Create and register a configuration
   Example:

   (define-configuration debug-configuration (standard-configuration)
    (:configuration-schema standard-configuration)
    (:title \"Debug configuration\")
    (:section :database-configuration
        (:database-name \"debug-database\"))
    (:section :logging-configuration
       (:output-location :standard-output)
       (:active-layers (:debugging :database))
       (:debugging-levels (:info :warning :error)))
    (:section :webapp-configuration
	      (:catch-errors nil))
    (:documentation \"Debugging configuration scheme\"))"
  (let ((direct-sections (filter (lambda (elem)
			    (equalp (first elem) :section))
			  args))
	(title (second (find :title args :key #'first)))
	(configuration-schema (second (find :configuration-schema args :key #'first)))
	(documentation (second (find :documentation args :key #'first))))
    `(setf (gethash ',name *configurations*)
	   (make-instance 'configuration
			  ,@(if name
				`(:name ',name))
			  :parents ',parents
			  ,@(if title
				`(:title ,title))
			  ,@(if configuration-schema
				`(:configuration-schema (find-configuration-schema ',configuration-schema)))
			  :direct-sections ',direct-sections
			  ,@(if documentation
				`(:documentation ,documentation))))))

(defun option-path-section (option-path)
  (cond
    ((listp option-path) (first option-path))
    ((symbolp option-path) (intern (subseq (symbol-name option-path) 0
					   (position #\. (symbol-name option-path)))
				    :keyword))
    (t (error "Invalid option path ~A" option-path))))

(defun option-path-option (option-path)
  (cond
    ((listp option-path) (second option-path))
    ((symbolp option-path) (intern (subseq (symbol-name option-path)
					   (1+ (position #\. (symbol-name option-path))))
				   :keyword))
    (t (error "Invalid option path ~A" option-path))))

(defun get-option-value (option-path configuration
			 &optional (option-not-found :error))
  (let ((section-name (option-path-section option-path))
	(option-name (option-path-option option-path)))
    (loop for conf in (cons configuration (ordered-parents configuration))
	  for section = (gethash section-name (direct-sections conf))
	  when (and section (gethash option-name (options section)))
	  do (let ((option (gethash option-name (options section))))
	       (return-from get-option-value
		 (values (value option) option section conf))))
    ;; Try default
    (let ((section
	      (gethash section-name
		       (sections (configuration-schema configuration)))))
      (if (not section)
	  (error "Could not find section ~A in schema ~A"
		 section-name
		 (configuration-schema configuration))
	  (let ((option (gethash (second option-path)
				 (cfg::direct-options section))))
	    (if (not option)
		(error "Could not find option ~A in schema section ~A"
		 option-name
		 section)
		(if (default option)
		    (values (default option) nil section :default)
		    (case option-not-found
		      (:error
		       (option-value-not-found-error option-path configuration))
		      (t (values nil nil nil nil))))))))))

(defun (setf get-option-value) (value option-path configuration)
  (let ((section-name (option-path-section option-path))
	(option-name (option-path-option option-path)))
    (let ((section (or (gethash section-name (direct-sections configuration))
		       (let ((new-section
			      (make-instance 'configuration-section
					     :name section-name)))
			 (setf (gethash section-name (direct-sections configuration))
			       new-section)
			 new-section))))
      (let ((schema-option
	     (find-configuration-schema-option
	      (configuration-schema configuration)
	      (list section-name option-name))))
	(setf (gethash option-name (options section))
	    (make-instance
	     (option-class (option-type schema-option))
	     :schema-option schema-option
	     :value value))))))

(defun unset-option (option-path configuration)
  (destructuring-bind (section-name option-name)
      option-path
    (let ((section (gethash section-name (direct-sections configuration))))
      (if (not section)
	  (error "Could not unset option ~A. Section ~A not found in ~A direct-sections" option-path section-name configuration)
	  (let ((option (gethash option-name (options section))))
	    (if (not option)
		(error "Could not unset option ~A. Option ~A not found in ~A"
		       option-path option-name section)
		(remhash option-name (options section))))))))

(defun get-section-option-value (option section)
  (loop for section-option in section
     do (let ((option-name (first section-option)))
	  (when (equalp option-name option)
	    (return-from get-section-option-value
	      (values (second section-option) t)))))
  (values nil nil))

(defmethod ordered-parents ((configuration configuration))
  (let ((parents (mapcar #'find-configuration
			 (parents configuration))))
    (append parents
	    (apply #'append (mapcar #'ordered-parents parents)))))

;; Copy configuration

(defun copy-configuration (configuration)
  (let ((copy (allocate-instance (class-of configuration))))
    (when (slot-boundp configuration 'name)
      (setf (slot-value copy 'name)
	    (slot-value configuration 'name)))
    (when (slot-boundp configuration 'parents)
      (setf (slot-value copy 'parents)
	    (if (null (slot-value configuration 'parents))
		nil
		(alexandria:copy-sequence 'cons
					  (slot-value configuration 'parents)))))
    (when (slot-boundp configuration 'title)
      (setf (slot-value copy 'title)
	    (slot-value configuration 'title)))
    (when (slot-boundp configuration 'configuration-schema)
      (setf (slot-value copy 'configuration-schema)
	    (slot-value configuration 'configuration-schema)))
    (when (slot-boundp configuration 'documentation)
      (setf (slot-value copy 'documentation)
	    (slot-value configuration 'documentation)))
    (when (slot-boundp configuration 'direct-sections)
      (setf (slot-value copy 'direct-sections)
	    (make-hash-table :test #'equalp))
      (maphash (lambda (name section)
		 (setf (gethash name (slot-value copy 'direct-sections))
		       (copy-configuration-section section)))
	       (slot-value configuration 'direct-sections)))
    copy))

(defun copy-configuration-section (section)
  (let ((copy (allocate-instance (class-of section))))
    (when (slot-boundp section 'name)
      (setf (slot-value copy 'name)
	    (slot-value section 'name)))
    (when (slot-boundp section 'options)
      (setf (slot-value copy 'options)
	    (make-hash-table :test #'equalp))
      (maphash (lambda (name option)
		 (setf (gethash name (slot-value copy 'options))
		       (copy-configuration-option option)))
	       (slot-value section 'options)))
    copy))

(defmethod copy-configuration-option ((option configuration-option))
  (let ((copy (allocate-instance (class-of option))))
    (when (slot-boundp option 'schema-option)
      (setf (slot-value copy 'schema-option)
	    (slot-value option 'schema-option)))
    (when (slot-boundp option 'value)
      (setf (slot-value copy 'value)
	    (copy-option-value (slot-value option 'value))))
    copy))

(defmethod copy-configuration-option ((option list-configuration-option))
  (let ((copy (call-next-method)))
    (when (slot-boundp option 'inherit)
      (setf (slot-value copy 'inherit)
	    (slot-value option 'inherit)))
    copy))

(defmethod copy-configuration-option ((option one-of-configuration-option))
  (let ((copy (call-next-method)))
    (when (slot-boundp option 'value2)
      (setf (slot-value copy 'value)
	    (copy-option-value (slot-value option 'value2))))
    copy))

(defmethod copy-option-value (value)
  ;; warning: we should probably copy the value too
  value)