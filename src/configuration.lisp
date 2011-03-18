(in-package :cl-config)

(defvar *configuration* nil "The current configuration")
(defvar *configurations* (make-hash-table :test #'equalp))
(defvar *schema-validation* t)

(defun %with-schema-validation (value func)
  (let ((*schema-validation* value))
    (funcall func)))

(defmacro with-schema-validation ((value) &body body)
  `(%with-schema-validation ,value (lambda () ,@body)))

(defun find-configuration (name)
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

(defmethod initialize-instance :after ((section configuration-section) &rest initargs)
  (declare (ignore initargs))
  (when *schema-validation*
    (process-configuration-section (name section) section)
    (validate-configuration-section (name section) section)))

(defgeneric process-configuration-section (name section)
  (:method (name section)
    t))

(defgeneric validate-configuration-section (name section)
  (:method (name section)
    t))

(defmacro define-configuration-section-processor (name (section) &body body)
  (let ((name-var (gensym "NAME-")))
    `(defmethod process-configuration-section ((,name-var (eql ',name)) ,section)
       ,@body)))

(defmacro define-configuration-section-validator (name (section) &body body)
  (let ((name-var (gensym "NAME-")))
    `(defmethod validate-configuration-section ((,name-var (eql ',name)) ,section)
       ,@body)))

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
  (process-configuration-option option)
  (validate-configuration-option option))

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

(defun validate-configuration (configuration)
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
  "Create and register a configuration"
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

(defun get-option-value (option-path configuration &optional (option-not-found :error))
  (assert (listp option-path))
  (let ((section-name (option-path-section option-path))
	(option-name (option-path-option option-path)))
    (loop for conf in (cons configuration (ordered-parents configuration))
	  for section = (gethash section-name (direct-sections conf))
	  when (and section (gethash option-name (options section)))
	  do (return-from get-option-value (values (value
						    (gethash option-name
							     (options section)))
						   conf)))
    ;; Try default
    (let ((section
	      (gethash section-name
		       (sections (configuration-schema configuration)))))
      (if (not section)
	  (error "Could not find section ~A in schema ~A"
		 section-name
		 (configuration-schema configuration))
	  (let ((option (gethash (second option-path) (cfg::direct-options section))))
	    (if (not option)
		(error "Could not find option ~A in schema section ~A"
		 option-name
		 section)
		(if (default option)
		    (values (default option) :default)
		    (case option-not-found
		      (:error
		       (option-value-not-found-error option-path configuration))
		      (t nil)))))))))

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