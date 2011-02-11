(in-package :cl-config)

(defclass attribute ()
  ((name :initarg :name :initform (error "Provide a name for the attribute")
	 :documentation "The attribute name"))
  (:documentation "A configuration attribute"))

(defclass attribute-value ()
  ((attribute :initarg :attribute
	      :documentation "The attribute referenced")
   (value :initarg :value
	  :documentation "The attribute value"))
  (:documentation "The value of a configuration attribute"))

