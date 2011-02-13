(in-package :cl-config)

(define-condition validation-error ()
  ((target :initarg :target
	   :initform (error "Set up the target")
	   :accessor target)
   (error-msg :initarg :error-msg
	      :initform (error "Provide the error message")
	      :accessor error-msg))
  (:report (lambda (c s)
	     (format s "~A on ~A" (error-msg c) (target c)))))
   
(defun validation-error (target error-msg)
  (error 'validation-error
	 :target target
	 :error-msg error-msg))
