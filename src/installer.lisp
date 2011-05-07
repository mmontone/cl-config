(in-package :cfg)

(defvar *installer* nil)
(defvar *installers* (make-hash-table :test #'equalp))

(defun find-installer (name)
  "Gets an installer by name"
  (gethash name *installers*))

(defun register-installer (installer)
  "Registers an installer. The installer can then be obtained via find-installer"
  (setf (gethash (name installer) *installers*) installer))

(defclass installer ()
  ((name :initarg :name
	 :initform (error "Provide a name")
	 :accessor name)
   (title :initarg :title
	  :initform (error "Provide the title")
	  :accessor title)
   (install-function :initarg :install-function
		     :initform (error "Provide the install function")
		     :accessor install-function)
   (documentation :initarg :documentation
		  :initform ""
		  :accessor documentation*))
  (:metaclass sb-mop:funcallable-standard-class)
  (:documentation "The main installer class. Installer instances are funcallable"))

(defmethod print-object ((installer installer) stream)
  (print-unreadable-object (installer stream :type t :identity t)
    (format stream "~A ~S"
	    (name installer)
	    (title installer))))

(defmethod initialize-instance :after ((installer installer) &rest initargs)
  (declare (ignore initargs))
  (sb-mop:set-funcallable-instance-function
   installer
   (lambda ()
     (funcall (install-function installer) installer))))

(defclass wizard-installer (installer)
  ((continuations :initform nil
		  :accessor continuations)
   (current-continuation :initform nil
			 :accessor current-continuation))
  (:metaclass sb-mop:funcallable-standard-class)
  (:documentation "The class for wizard installers"))

(defmethod initialize-instance :after ((installer wizard-installer) &rest initargs)
  (declare (ignore initargs))
  (sb-mop:set-funcallable-instance-function
   installer
   (lambda (&rest args)
     (if (current-continuation installer)
	 (apply (current-continuation installer) args)
	 (apply (install-function installer) args))))
  (setf (install-function installer)
	(let ((install-function (install-function installer)))
	  (lambda (&rest args)
	    (let ((*installer* installer))
	      (handler-bind
		  ((error (lambda (c)
			    (install-error c)
			    (continue c))))
	      (cl-cont:with-call/cc
		(let ((result (apply install-function args)))
		    (reset-installer installer)
		    result))))))))

(defmethod reset-installer ((installer wizard-installer))
  (setf (current-continuation installer) nil)
  (setf (continuations installer) nil))

(defmethod go-back ((installer wizard-installer))
  "Returns to a previous section in a wizard installer"
  (if (continuations installer)
      (progn
	(pop (continuations installer))
	(setf (current-continuation installer)
	      (first (continuations installer))))
      ;; else
      (reset-installer installer)))

(defclass configuration-installer (installer)
  ((configuration-schema :initarg :configuration-schema
			 :initform (error "Provide the configuration schema")
			 :accessor configuration-schema)
   (backend :initarg :backend
	    :initform (error "Provide the backend")
	    :accessor backend)
   (output-file :initarg :output-file
		:initform (error "Provide the output file")
		:accessor output-file)
   (configuration :accessor configuration
		  :documentation "Configuration being installed"))
  (:metaclass sb-mop:funcallable-standard-class)
  (:documentation "The class for configuration installers"))

(defmethod initialize-instance :after ((installer configuration-installer) &rest initargs)
  (declare (ignore initargs))
  (setf (configuration installer)
	(cfg:with-schema-validation (nil)
	  (make-instance 'configuration
			 :name 'install-config
			 :parents nil
			 :title "Install config"
			 :configuration-schema
			 (find-configuration-schema (configuration-schema installer))
			 :direct-sections nil))))

(defclass standard-installer (wizard-installer configuration-installer)
  ()
  (:metaclass sb-mop:funcallable-standard-class)
  (:documentation "standard-intallers are installers used for installing configurations in a wizard fashion"))

(defmacro define-installer (name (&key title documentation) &body body)
  "Define a vanilla installer"
  `(register-installer
    (make-instance 'installer :name ',name
		   :title ,title
		   :documentation ,documentation
		   :install-function (lambda (installer)
				       (let ((*installer* installer)) 
					 ,@body)))))

(defmacro define-wizard-installer (name (&key title documentation) &body body)
  "Define a wizard installer"
  `(register-installer
    (make-instance 'wizard-installer
		   :name ',name
		   :title ,title
		   :documentation ,documentation
		   :install-function (cl-cont:lambda/cc () ,@body))))

(cl-cont:defun/cc install-configuration-section (section-name configuration)
  (let ((section (gethash section-name (sections configuration))))
    (start-section (name section) (title section))
    (collecting-validation-errors (errors found-p)
	(input (mapcar #'name (options section))
	       (lambda (options-values)
		 (loop for option in (options section)
		    for option-value = (getf options-values (name option))
		    do (setf (get-option-value (list (name section)
						     (name option)))
			     option-value))))
      (when found-p
	(install-errors errors)
	(install-configuration-section section-name configuration)))))
	       
(cl-cont:defun/cc install-configuration (configuration)
  (loop
     for section being the hash-values of (sections configuration)
     do
       (install-configuration-section (name section) configuration)))

(defmacro generate-configuration-schema-install-function (configuration-schema)
  (let ((sections (cfg::sections (find-configuration-schema configuration-schema))))
    `(cl-cont:lambda/cc ()
       (let ((configuration (cfg:with-schema-validation (nil)
			    (make-instance 'configuration
					   :parents nil
					   :name :install-config
					   :title "Install config"
					   :configuration-schema (find-configuration-schema ',configuration-schema)
					   :direct-sections nil))))
	 ,@(loop for section being the hash-values of sections
	      appending (let ((fname (gensym "CONFIGURE-SECTION-")))
			  `((start-section ,(cfg::name section)
					 ,(cfg::title section))
			    (labels ((,fname ()
				       (with-input ,(loop for option being the hash-values of (cfg::direct-options section)
						       collect (intern (symbol-name (cfg::name option))))
					 (cfg::collecting-validation-errors (errors found-p)
					     (progn
					       ,@(loop for option being the hash-values of (cfg::direct-options section)
						    collect `(setf (cfg::get-option-value ',(list (cfg::name section)
												  (cfg::name option)) configuration)
								   ,(intern (symbol-name (cfg:name option))))))
					   (when found-p
					     (install-errors errors)
					     (,fname))))))
			      (,fname)))))))))

(defmacro define-standard-installer (name (&key title
						documentation
						configuration-schema
						backend output-file)
					  &body body)
  "Defines a standard-installer"
  (declare (ignore body))
  `(register-installer
    (make-instance 'standard-installer
		   :name ',name
		   :title ,title
		   :documentation ,documentation
		   :configuration-schema ',configuration-schema
		   :backend ,backend
		   :output-file ,output-file
		   :install-function (generate-configuration-schema-install-function ,configuration-schema))))

(defun installer-continuation (c)
  (let ((installer *installer*))
    (cl-cont:lambda/cc (&rest args)
      (let ((*installer* installer))
	(apply c args)))))

;; Wizard installer operations

(defmacro idefun (name args &body body)
  "Defines a installer function"
  `(cl-cont:defun/cc ,name ,args ,@body))

(defmacro with-input (bindings &body body)
  "Wizard installer operation. Asks for input."
  `(input ',bindings
	  (lambda (&key ,@bindings)
	    ,@body)))

(cl-cont:defun/cc start-section (&optional name title)
  "Wizard operation. Indicates the installer starts a new section"
  (cl-cont:call/cc
   (lambda (c)
     ;; Keep current continuation
     (push (installer-continuation c)
	   (continuations *installer*))

     (setf (current-continuation *installer*)
	   (installer-continuation c))
     
     ;; Return the section data
     (append (list :section)
	     (when name
	       (list name))
	     (when title
	       (list title))))))

(cl-cont:defun/cc input (vars function)
  "Wizard operation. Ask for input"
  (cl-cont:call/cc
   (lambda (c)
     ;; Set current continuation to installer
     (setf (current-continuation *installer*)
	   (installer-continuation
	    (cl-cont:lambda/cc (&rest args)
	      (apply function args)
	      (funcall c))))
     
     ;; Return which are the required arguments
     (list :input vars))))

(cl-cont:defun/cc choose (message &rest options)
  "Wizard operation. Ask the client to choose one of the options"
  (cl-cont:call/cc
   (lambda (c)
     ;; Set current continuation to installer
     (setf (current-continuation *installer*)
	   (installer-continuation c))
			
      ;; Return what to do
     (list :choose message options))))

(cl-cont:defun/cc question (message)
  "Wizard operation. As a question to the client"
  (cl-cont:call/cc
   (lambda (c)
     ;; Set current continuation to installer
     (setf (current-continuation *installer*)
	   (installer-continuation c))
			
     ;; Return what to do
     (list :question message))))

(cl-cont:defun/cc alert (message)
  "Wizard operation. Alert a message"
  (cl-cont:call/cc
   (lambda (c)
     ;; Set current continuation to installer
     (setf (current-continuation *installer*)
	   (installer-continuation c))
			
     ;; Return what to do
     (list :alert message))))

(cl-cont:defun/cc prompt (message)
  "Wizard operation. Ask the user to enter a string"
  (cl-cont:call/cc
   (lambda (c)
     ;; Set current continuation to installer
     (setf (current-continuation *installer*)
	   (installer-continuation c))
			
     ;; Return what to do
     (list :prompt message))))

(cl-cont:defun/cc install-warning (message &rest args)
  "Wizard operation. Warn the user about something"
  (cl-cont:call/cc
   (lambda (c)
     ;; Set current continuation to installer
     (setf (current-continuation *installer*)
	   (installer-continuation c))
			
      ;; Return what to do
     (list :warning (format nil message args)))))

(cl-cont:defun/cc install-error (message &rest args)
  "Wizard operation. Throws an installation error"
  (cl-cont:call/cc
   (lambda (c)
     ;; Set current continuation to installer
     (setf (current-continuation *installer*)
	   (installer-continuation c))
     
     ;; Return what to do
     (list :error (format nil message args)))))

(cl-cont:defun/cc install-errors (errors)
  "Wizard operation. Throws several installation errors"
  (cl-cont:call/cc
   (lambda (c)
      ;; Set current continuation to installer
      (setf (current-continuation *installer*)
	    (installer-continuation c))
			
      ;; Return what to do
      (list :errors errors))))