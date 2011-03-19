(in-package :cl-config)

(defun read-configuration-option (path &optional (configuration *configuration*))
  (%read-configuration-option path configuration))

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
  `(let ,(loop for (var option-path) in values
	      collect (list var `(cfg ,option-path ,configuration)))
     ,@body))

(defun complete-symbol-name (symbol)
  (format nil "~A::~A" 
	  (package-name (symbol-package symbol))
	  (symbol-name symbol)))

(defun read-symbol (string)
  (with-input-from-string (s string)
    (read s)))