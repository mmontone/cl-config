(in-package :cl-config)

(defvar *section* nil)

(defmacro with-configuration-section (section-name &body body)
  `(let ((*section* ',section-name))
     ,@body))

(defun read-configuration-option (path &optional (configuration *configuration*))
  (if *section*
      (%read-configuration-option (if (listp path)
				      (cons *section* path)
				      (intern (format nil "~A.~A" *section* path)
					      :keyword))
				  configuration)
      (%read-configuration-option path configuration)))

(defmethod %read-configuration-option (path (configuration symbol))
  (get-option-value path (find-configuration configuration)))

(defmethod %read-configuration-option (path (configuration configuration))
  (get-option-value path configuration))

(defmacro cfg (path &optional (configuration '*configuration*))
  `(read-configuration-option ',path ,configuration))

(defun cfg* (path &optional (configuration *configuration*))
  (read-configuration-option path configuration))

(defmacro define-configurable-function (name args &body body)
  (let ((pos (position '&configuration args)))
    (let ((conf-spec (when pos
		       (nth (1+ pos) args))))
      (if (not (listp conf-spec))
	  (error "Configuration spec not valid"))
      (destructuring-bind (conf-var conf-type) conf-spec
	(let ((new-args (remove '&configuration
				(remove (nth (1+ pos) args) args)))
	      (conf-args (gensym "CONF-ARGS-")))
	  `(defun ,name (,@new-args &rest ,conf-args)
	     (let ((,conf-var (apply #'make-configuration ',conf-type
				     ,conf-args)))
	       ,@body)))))))

(defmacro with-configuration-values (values configuration &body body)
  `(let ,(loop for value in values
	      collect
	      (if (listp value)
		  (destructuring-bind (var option-path) value
		    (list var `(cfg ,option-path ,configuration)))
		  (list value `(cfg ,(intern (symbol-name value)
					     :keyword) ,configuration))))
     ,@body))

(defun complete-symbol-name (symbol)
  (format nil "~A::~A" 
	  (package-name (symbol-package symbol))
	  (symbol-name symbol)))

(defun read-symbol (string)
  (with-input-from-string (s string)
    (read s)))