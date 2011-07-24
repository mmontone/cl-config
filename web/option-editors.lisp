(in-package :cfg.web)

(defgeneric render-option-editor (type option-schema option value stream &key writer)
  )


(defmethod render-option-editor ((type cfg::text-configuration-schema-option-type)
				 option-schema
				 option
				 value
				 stream &key writer)
  (let (input-value)
    (with-form-field (field :writer (lambda (val)
				      (setf input-value val)))
      (with-change-notifier (field stream :on-change
				   (lambda ()
				     (funcall writer input-value)))
	(with-html-output (stream)
	  (:input :type "text"
		  :id field
		  :name field
		  :value (if value value)))))))

(defmethod render-option-editor ((type cfg::integer-configuration-schema-option-type)
				 option-schema
				 option
				 value
				 stream &key writer)
  (let (input-value)
    (with-form-field (field :writer (lambda (val)
				      (setf input-value val))
			    :reader (lambda (val)
				      (parse-integer val :junk-allowed t)))
      (with-change-notifier (field stream :on-change (lambda ()
						       (funcall writer input-value)))
	(with-html-output (stream)
	  (:input :type "text"
		  :name field
		  :id field
		  :value (if value value)))))))

(defmethod render-option-editor ((type cfg::one-of-configuration-schema-option-type)
				 option-schema
				 option
				 value
				 stream &key writer)
  (let (input-value)
    (with-form-field (field :writer (lambda (val)
				      (setf input-value val))
			    :reader #'cfg::read-symbol)
      (with-change-notifier (field stream :on-change (lambda ()
						       (funcall writer input-value)))
	(with-html-output (stream)
	  (htm
	   (:select :name field
		    :id field
		    (loop for opt in (cfg::options type)
		       do (htm
			   (:option :value (cfg::complete-symbol-name (cfg::name opt))
				    :selected (if (equalp value (cfg::name opt))
						  "selected")
				    (str (cfg::title opt))))))))))))

(defmethod render-option-editor ((type cfg::list-configuration-schema-option-type)
				 option-schema
				 option
				 value
				 stream &key writer)
  (with-form-field (field :writer (lambda (val)
				    (when (not (equalp val value))
				      (funcall writer val)))
			  :reader (lambda (val)
				    (if (listp val)
					(mapcar #'cfg::read-symbol val)
					(list (cfg::read-symbol val)))))
    (with-html-output (stream)
      (htm
       (:select :name field
		:id field
		:multiple "multiple"
		:class "multiselect"
		(loop for opt in value
		   do
		   (htm
		    (:option :value (cfg::complete-symbol-name opt)
			     :selected "selected"
			     (let ((option (find opt (cfg::options (cfg::option-type option-schema))
						 :key #'cfg::name)))
			       (assert option nil "~A not found in ~A"
				       opt
				       (cfg::options (cfg::option-type option-schema)))
			       (str (cfg::title option))))))
		(loop for opt in (cfg::options type)
		   when (not (some (lambda (val)
				     (equalp val (cfg::name opt)))
				   value))
		   do (htm
		       (:option :value (cfg::complete-symbol-name (cfg::name opt))
				(str (cfg::title opt)))))))
      (when option
	(with-form-field (inherit :writer (lambda (val)
					    (setf (cfg::inherit option) val))
				  :reader (lambda (val)
					    (if val t nil)))
	  (htm
	   (:input :type "checkbox"
		   :name inherit
		   :checked (if (cfg::inherit option)
				"checked"))))))))

(defmethod render-option-editor ((type cfg::boolean-configuration-schema-option-type)
				 option-schema
				 option
				 value
				 stream &key writer)
  (let (input-value)
    (with-form-field (field :writer (lambda (val)
				      (if val (setf input-value t))))
      (with-change-notifier (field stream
				   :on-change (lambda ()
						(funcall writer input-value)))
	(with-html-output (stream)
	  (htm
	   (:input :name field
		   :id field
		   :type "checkbox"
		   :checked (if value "checked"))))))))

(defmethod render-option-editor ((type cfg::pathname-configuration-schema-option-type)
				 option-schema
				 option
				 value
				 stream &key writer)
  (let (input-value)
    (with-form-field (field :writer (lambda (val)
				      (setf input-value val)))
      (with-change-notifier (field stream
				   :on-change (lambda ()
						(funcall writer input-value)))
	(with-html-output (stream)
	  (htm
	   (:input :type "file"
		   :name field
		   :id field
		   :value (if value value))))))))

(defmethod render-option-editor ((type cfg::email-configuration-schema-option-type)
				 option-schema
				 option
				 value
				 stream &key writer)
  (let (input-value)
    (with-form-field (field :writer (lambda (val)
				      (setf input-value val)))
      (with-change-notifier (field stream
				   :on-change (lambda ()
						(funcall writer input-value)))
	(with-html-output (stream)
	  (htm
	   (:input :type "text"
		   :name field
		   :id field
		   :value (if value value))))))))

(defmethod render-option-editor ((type cfg::url-configuration-schema-option-type)
				 option-schema
				 option
				 value
				 stream &key writer)
  (let (input-value)
    (with-form-field (field :writer (lambda (val)
				      (setf input-value val)))
      (with-change-notifier (field stream
				   :on-change (lambda ()
						(funcall writer input-value)))
	(with-html-output (stream)
	  (htm
	   (:input :type "text"
		   :name field
		   :id field
		   :value (if value value))))))))


(defmethod render-option-editor ((type cfg::email-configuration-schema-option-type)
				 option-schema
				 option
				 value
				 stream &key writer)
  (let (input-value)
    (with-form-field (field :writer (lambda (val)
				      (setf input-value val)))
      (with-change-notifier (field stream :on-change (lambda ()
						       (funcall writer input-value)))
	(with-html-output (stream)
	  (htm
	   (:input :type "text"
		   :name field
		   :id field
		   :value (if value value))))))))

(defmethod render-option-editor ((type cfg::sexp-configuration-schema-option-type)
				 option-schema
				 option
				 value
				 stream &key writer)
  (let (input-value)
    (with-form-field (field :writer (lambda (val)
				      (setf input-value val)))
      (with-change-notifier (field stream :on-change (lambda ()
						       (funcall writer input-value)))
	(with-html-output (stream)
	  (:textarea :name field
		     :id field
		     (if value (str value))))))))