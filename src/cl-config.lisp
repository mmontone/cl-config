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