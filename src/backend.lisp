(in-package :cl-config)

(defclass output-backend ()
  ())

(defparameter *output-backend* nil)
(defparameter *input-backend* nil)

(defun serialize (configuration stream &optional (output-backend *output-backend*))
  (run output-backend configuration stream))

(defun serialize-configuration-schemas (stream &optional (output-backend *output-backend*))
  (loop for configuration-schema being the hash-values of *configuration-schemas*
       do (serialize configuration-schema stream output-backend)))

(defun serialize-configurations (stream &optional (output-backend *output-backend*))
  (loop for configuration being the hash-values of *configurations*
       do (serialize configuration stream output-backend)))