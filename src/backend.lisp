(in-package :cl-config)

(defclass output-backend ()
  ())

(defparameter *output-backend* nil)
(defparameter *input-backend* nil)

(defun serialize (configuration stream &optional (output-backend *output-backend*))
  (run output-backend configuration stream))

