(in-package :cl-config)

(defvar *configuration* nil "The current configuration")
(defvar *configurations* (make-hash-table :test #'equalp))

(defun read-configuration-option (path &optional (configuration *configuration*))
  (get-option-value path configuration))

(defmacro cfg (path &optional (configuration '*configuration*))
  `(read-configuration-option ',path ,configuration))