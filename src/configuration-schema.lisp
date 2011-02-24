(in-package :cl-config)

(defvar *configuration-schemas* (make-hash-table :test #'equal))

(defun find-configuration-schema (name)
  (multiple-value-bind (configuration-schema found-p)
      (gethash name *configuration-schemas*)
    (if found-p
	configuration-schema
	(error "Configuration schema ~A not defined" name))))

(defclass configuration-schema ()
  ((name :initarg :name
	 :accessor name
	 :initform (error "Provide a name for the configuration-schema")
	 :documentation "The configuration-schema name")
   (parents :initarg :parents
	    :accessor parents
	    :initform nil
	    :documentation "Configuration-Schema mixins")
   (title :initarg :title
	  :accessor title
	  :initform (error "Provide a title for the configuration-schema")
	  :documentation "Configuration-Schema title")
   (direct-sections :initarg :direct-sections
	     :accessor direct-sections
	     :initform (make-hash-table :test #'equalp)
	     :documentation "Configuration-Schema direct-sections")
   (documentation :initarg :documentation
		  :accessor documentation*
		  :documentation "Configuration-Schema documentation"))
  (:documentation "A configuration-schema"))

(defmethod print-object ((configuration-schema configuration-schema) stream)
  (print-unreadable-object (configuration-schema stream :type t :identity t)
    (format stream "~A ~S"
	    (name configuration-schema)
	    (title configuration-schema))))

(defmethod initialize-instance :after ((configuration-schema configuration-schema) &rest initargs)
  (let ((direct-sections (make-hash-table :test #'equalp)))
    (loop for section in (getf initargs :direct-sections)
	  do
	 (setf (gethash (name section) direct-sections) section))
    (setf (direct-sections configuration-schema) direct-sections))
  (setf (parents configuration-schema) (mapcar #'find-configuration-schema (getf initargs :parents))))

;; from Alexandria:
(defun copy-hash-table (table &key key test size
                                   rehash-size rehash-threshold)
  "Returns a copy of hash table TABLE, with the same keys and values
as the TABLE. The copy has the same properties as the original, unless
overridden by the keyword arguments.

Before each of the original values is set into the new hash-table, KEY
is invoked on the value. As KEY defaults to CL:IDENTITY, a shallow
copy is returned by default."
  (setf key (or key 'identity))
  (setf test (or test (hash-table-test table)))
  (setf size (or size (hash-table-size table)))
  (setf rehash-size (or rehash-size (hash-table-rehash-size table)))
  (setf rehash-threshold (or rehash-threshold (hash-table-rehash-threshold table)))
  (let ((copy (make-hash-table :test test :size size
                               :rehash-size rehash-size
                               :rehash-threshold rehash-threshold)))
    (maphash (lambda (k v)
               (setf (gethash k copy) (funcall key v)))
             table)
    copy))

(defun merge-options (more-specific-options less-specific-options)
  (let ((options (copy-hash-table less-specific-options)))
    (maphash (lambda (name option)
	       (setf (gethash name options) option))
	     more-specific-options)
    options))

(defun merge-sections (more-specific-section less-specific-section)
  (assert (equalp (name more-specific-section) (name less-specific-section)))
  (let ((options (merge-options (direct-options more-specific-section)
				(direct-options less-specific-section))))
    (make-instance 'configuration-schema-section
		   :name (name more-specific-section)
		   :title (title more-specific-section)
		   :documentation (documentation* more-specific-section)
		   :direct-options (loop for option being the hash-values of options
				      collect option))))

(defun merge-section-tables (more-specific-section-table less-specific-section-table)
  (let ((sections (copy-hash-table less-specific-section-table)))
    (maphash (lambda (name section)
	       (multiple-value-bind (parent-section found-p)
		   (gethash name more-specific-section-table)
		 (if found-p
		     (setf (gethash name sections)
			   (merge-sections section parent-section))
		     (setf (gethash name sections)
			   section))))
	     more-specific-section-table)
    sections))

(defun flip (function)
  (lambda (x y)
    (funcall function y x)))

(defmethod sections ((configuration-schema configuration-schema))
  (let ((parent-sections (loop for parent in (parents configuration-schema)
			    collect (sections parent))))
    (if parent-sections
	(merge-section-tables (direct-sections configuration-schema)
			      (reduce (flip #'merge-section-tables) parent-sections))
	(direct-sections configuration-schema))))

(defclass configuration-schema-section ()
  ((name :initarg :name
	 :accessor name
	 :initform (error "Provide a name for the section"))
   (title :initarg :title
	  :accessor title
	  :initform (error "Provide a title for the section"))
   (direct-options :initarg :direct-options
	    :accessor direct-options
	    :initform (make-hash-table :test #'equalp))
   (documentation :initarg :documentation
		  :accessor documentation*))
  (:documentation "A configuration-schema section"))

(defmethod print-object ((section configuration-schema-section) stream)
  (print-unreadable-object (section stream :type t :identity t)
    (format stream "~A ~S"
	    (name section)
	    (title section))))

(defmethod initialize-instance :after ((configuration-schema-section configuration-schema-section) &rest initargs)
  (let ((direct-options (make-hash-table :test #'equalp)))
    (loop for option in (getf initargs :direct-options)
	  do
	 (setf (gethash (name option) direct-options) option))
    (setf (direct-options configuration-schema-section) direct-options)))

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
   (optional :initarg :optional
	     :initform nil
	     :accessor optional
	     :documentation "t when the parameter is optional")
   (default :initarg :default
     :initform nil
     :accessor default
     :documentation "The default value if there is one")
   (advanced :initarg :advanced
	     :initform nil
	     :accessor advanced
	     :documentation "t when this is an advanced option")
   (validate :initarg :validate
	     :initform #'validate-configuration-option
	     :accessor validate
	     :documentation "Function to use to validate the configuration option")
   (error-msg :initarg :error-msg
	      :initform nil
	      :accessor error-msg
	      :documentation "The error message to show when the option is not valid")
   (documentation :initarg :documentation
		  :initform nil
		  :accessor documentation*
		  :documentation "The option documentation string"))
  (:documentation "A configuration-schema option"))

(defmethod print-object ((option configuration-schema-option) stream)
  (print-unreadable-object (option stream :type t :identity t)
    (format stream "~A ~S"
	    (name option)
	    (title option))))

(defun filter (predicate list)
  (reduce (lambda (result elem)
	    (if (funcall predicate elem)
		(cons elem result)
		result))
	  list :initial-value nil))

(defmacro define-configuration-schema (name parents &rest args)
  (let ((direct-sections (filter (lambda (elem)
			    (equalp (first elem) :section))
			  args))
	(title (second (find :title args :key #'first)))
	(documentation (second (find :documentation args :key #'first))))
    `(setf (gethash ',name *configuration-schemas*)
	   (make-instance 'configuration-schema
		    :name ',name
		    :parents  ',parents
		    :title ,title
		    :documentation ,(if documentation
					documentation
					"")
		    :direct-sections (mapcar (lambda (section)
					(destructuring-bind (_ name title &rest body)
					    section
					  (declare (ignore _))
					  (make-configuration-schema-section name title body)))
				      ',direct-sections)))))

(defun make-configuration-schema-section (name title args)
  (let ((documentation (second (find :documentation args :key #'first)))
	(direct-options (filter (lambda (elem)
			   (not (equalp (first elem) :documentation)))
			 args)))
    (make-instance 'configuration-schema-section
		   :name name
		   :title title
		   :documentation documentation
		   :direct-options
		   (mapcar
		    (lambda (option)
		      (destructuring-bind (name title type-spec &rest rest)
			  option
			(apply #'make-configuration-schema-option
			       (append (list name title type-spec)
				       rest))))
		    direct-options))))

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

(defun find-configuration-schema-section (configuration-schema section-name)
  (multiple-value-bind (section found)
      (gethash section-name (sections configuration-schema))
    (if (not found)
	(error "Section ~A not found in ~A" section-name configuration-schema)
	section)))

(defun find-configuration-schema-section-option (configuration-schema-section option-name)
  (multiple-value-bind (option found)
      (gethash option-name (direct-options configuration-schema-section))
    (if (not found)
	(error "Option ~A not found in ~A" option-name configuration-schema-section)
	option)))

(defun find-configuration-schema-option (configuration-schema option-path)
  (destructuring-bind (section-name option-name) option-path
    (let ((section (find-configuration-schema-section
		    configuration-schema
		    section-name)))
      (find-configuration-schema-section-option section
						option-name))))

(defmethod ordered-parents ((configuration-schema configuration-schema))
  (loop for parents in (parents configuration-schema)
        appending parents))