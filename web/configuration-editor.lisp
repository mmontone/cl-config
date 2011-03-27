(in-package :cfg.web)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-change-notifier ((target stream &key (on-change #'identity))
				  &body body)
    (let* ((changed-checkbox (gensym "CHANGED-CHECKBOX-")))
      `(with-form-field (,changed-checkbox :writer (lambda (val)
						     (when (plusp (length val))
						       (funcall ,on-change))))
	 (with-html-output (,stream)
	   (htm
	    (:input :id ,changed-checkbox
	            :name ,changed-checkbox
		    :type "hidden"
		    :value "")
	    (:script :language "javascript"
		     (str
		      (ps*
		       `(chain ($ document)
			       (ready (lambda ()
					(chain ($ ,(format nil "#~A" ,target))
					       (change (lambda ()
							 (chain ($ ,(format nil "#~A" ,changed-checkbox))
								(val "true")))))
					))))))))
	 ,@body)))
  
  (defmacro with-jquery.ui-accordion ((&key (header :h2) (animate t))
				      &rest sections)
    (let ((widget-id (gensym "ACCORDION-")))
      `(htm
	(:div :id ,(symbol-name widget-id)
	      ,@(loop for section in sections
		     for title = (car section)
		     for content = (cadr section)
		     collect
		     `((,header (:a :href "#" (str ,title)))
		       (:div
			,content))))
	(:script :language "javascript"
		 (str (ps* `(chain ($ document)
				   (ready (lambda ()
					    (chain ($ ,(format nil "#~A" widget-id))
						   (accordion))))))))))))

(defun jquery.ui-accordion (stream sections)
  (let ((widget-id (gensym "ACCORDION-")))
    (with-html-output (stream)
      (htm
       (:div :id (symbol-name widget-id)
	     (loop for (title . content) in sections
		do (htm
		    (:h3 (:a :href "#" (str title)))
		    (:div (funcall content stream))))
	     (htm
	      (:script :language "javascript"
		       (str (ps* `(chain ($ document)
					 (ready (lambda ()
						  (chain ($ ,(format nil "#~A" widget-id))
							 (accordion))))))))))))))

(defun new-configuration (stream &optional errors)
  (let (name title schema parents documentation)
    (labels
	((render-form (stream errors)
	   (flet ((on-submit ()
		    (collecting-validation-errors (errors found-p)
			(progn
			  (if (zerop (length name))
			      (validation-error 'name "Name cannot be empty"))
			  (if (zerop (length title))
			      (validation-error 'title "Enter a title")))
		      (with-output-to-string (s)
			(with-main-page (s)
			  (if found-p
			      (render-form s errors)
			      (let ((configuration
				     (cfg::with-schema-validation (nil)
				       (make-instance 'cfg::configuration
						      :name (cfg::read-symbol name)
						      :title title
						      :parents parents
						      :configuration-schema (find-configuration-schema schema)
						      :direct-sections nil
						      :documentation documentation))))
				(edit-configuration configuration s :save-as-new nil))))))))
	     (with-html-output (stream)
	       (htm
		(:h2 (str "New configuration"))
		(if (zerop (hash-table-count *configuration-schemas*))
		    (htm (:p "No configuration schemas available to create configurations from"))
		    (with-form (action :on-submit #'on-submit)
		      (htm
		       (:form :action action
			      :method "post"
			      :id "newconf"
			      (:table
			       (:tbody
				(:tr
				 (:td (str "Name:"))
				 (:td 
				  (with-form-field (field :writer (lambda (val)
								    (setf name val)))
				    (htm
				     (:input :type "text"
					     :name field
					     :value name))))
				 (let ((error (find 'name errors :key (lambda (item)
									(getf item :target)))))
				   (if error
				       (htm (:td (str (getf error :error-msg))))))	    )
				(:tr
				 (:td (str "Title:"))
				 (:td
				  (with-form-field (field :writer (lambda (val)
								    (setf title val)))
				    (htm
				     (:input :type "text"
					     :name field
					     :value title))))
				 (let ((error (find 'title errors :key (lambda (item)
									 (getf item :target)))))
				   (if error
				       (htm (:td (str (getf error :error-msg)))))))
				(:tr
				 (:td (str "Schema:"))
				 (:td
				  (with-form-field (field :reader #'cfg::read-symbol
							  :writer (lambda (val)
								    (setf schema val)))
				    (htm
				     (:select :id "schema"
					      :name field
					      (loop for configuration-schema being the hash-values of *configuration-schemas*
						 do (htm
						     (:option :value (cfg::complete-symbol-name (cfg::name configuration-schema))
							      (str (cfg::title configuration-schema))))))))))
				(:tr
				 (:td (str "Parents:"))
				 (:td
				  (with-form-field (field :writer (lambda (val)
								    (setf parents val))
							  :reader (lambda (val)
								    (if (listp val)
									(mapcar #'cfg::read-symbol val)
									(list (cfg::read-symbol val)))))
				    (htm
				     (:select :id "parents"
					      :name field
					      :multiple "true"
					      (loop for configuration being the hash-values of *configurations*
						 do (htm
						     (:option :value (cfg::complete-symbol-name (cfg::name configuration))
							      (str (cfg::title configuration))))))))))
				(:tr
				 (:td (str "Documentation:"))
				 (:td
				  (with-form-field (field :writer (lambda (val)
								    (setf documentation val)))
				    (htm
				     (:textarea :name field
						(str documentation))))))))
			      (:input :type "submit" :value "Create"))))))))))
	 (render-form stream errors))))

(defun hash-table-values (hash-table &optional sort-predicate key)
  (let ((values nil))
    (maphash (lambda (key value)
	       (push value values))
	     hash-table)
    (if sort-predicate
	(sort values sort-predicate :key key)
	values)))

(defun configurations-editor (stream &optional selected-configuration)
  (with-html-output (stream)
    (htm
     (:h1 "Configurations editor")
     (if (zerop (hash-table-count *configurations*))
	 (htm
	  (:p "There are no configurations"))
	 (let ((selected-conf (or selected-configuration
				  (first (hash-table-values *configurations*)))))
	   (htm
	    (:div :id "configuration-selector"
		  (:p "Configuration:")
		  (:select :id "configuration-select"
			   :name "configuration-select"
			   (loop for conf being the hash-values of *configurations*
			      do  (htm
				    (:option :value (cfg::complete-symbol-name (cfg::name conf))
					     :selected (if (eql conf selected-conf)
							   "selected")
					     (str (cfg::title conf))))))
		  (:script :type "text/javascript"
			   :language "javascript"
			   (str
			    (ps
			     (chain ($ document)
				    (ready (lambda ()
					     (chain ($ "#configuration-select")
						    (change (lambda ()
							      (setf (@ window location)
								    (concatenate 'string "/?conf="
										 (chain ($ "#configuration-select") (val))))
							      t)))
					     t))))))))
	   (edit-configuration selected-conf stream)))))
  (new-configuration stream))

(defun edit-configuration (configuration stream &key (save-as-new t))
  (labels ((render-editor (errors stream)
	     (with-html-output (stream)
	       (with-form (action
			   :on-submit (lambda ()
					(collecting-validation-errors (errors found-p)
					    (cfg::validate-configuration configuration)
					  (with-output-to-string (s)
					    (with-main-page (s)
					      (render-editor errors s))))))
		 (htm
		  (:div :class "configuration-editor"
			(:div :class "title"
			      (:h2 (fmt "~A editor" (cfg::title configuration))))
			(:div :class "name"
			      (:p (fmt "Name: ~A" (cfg::complete-symbol-name
						   (cfg::name configuration)))))
			(:div :class "schema"
			      (:span (:p (str "Schema:")))
			      (:span (:a :href (format nil "/showsc?schema=~A"
						       (cfg::complete-symbol-name
							(cfg::name
							 (cfg::configuration-schema
							  configuration))))
					 (str (cfg::title
					       (cfg::configuration-schema configuration))))))
			(:form :action action
			       :method "post"
			       (:p "Documentation:")
			       (with-form-field (field :writer (lambda (val)
								 (setf (cfg::documentation* configuration) val)))
				 (htm
				  (:textarea :name field
					     (str (cfg::documentation* configuration)))))
			       (:p "Parents:")
			       (with-form-field (field :writer (lambda (val)
								 (setf (cfg::parents configuration) val))
						       :reader (lambda (val)
								 (if (listp val)
								     (mapcar #'cfg::read-symbol val)
								     (list (cfg::read-symbol val)))))
				 (htm
				  (:select :id "parents"
					   :name field
					   :multiple "true"
					   (loop for conf being the hash-values of *configurations*
					      when (not (eql conf configuration))
					      do (htm
						  (:option :value (cfg::complete-symbol-name (cfg::name conf))
							   :selected (if (find (cfg::name conf)
									       (cfg::parents configuration))
									 "selected")
							   (str (cfg::title conf))))))))

			       (jquery.ui-accordion stream
						    (loop for section being the hash-values of
							 (cfg::sections (cfg::configuration-schema configuration))
						       collect (cons (cfg::title section)
								     (let ((section* section))
								       (lambda (s)
									 (declare (ignore s))
									 (edit-configuration-section configuration section* stream))))))
				 (:input :type "submit" :value "Save")
				 (when save-as-new
				   (htm
				    (:div
				     (:table
				      (:tbody
				       (:tr
					(:td (:p "Name: ")) (:td (:input :type "text" :name "save-as-name")))
				       (:tr (:td (:p "Title: ")) (:td (:input :type "text" :name "save-as-title"))))))
				    (:input :type "submit" :value "Save as new"))))))))))
	   (render-editor nil stream)))

(defun edit-configuration-section (configuration section stream)
  (let ((direct-options (cfg::direct-options-list section
						  :exclude-advanced t))
	(advanced-options (cfg::advanced-options-list section))
	(advanced-section-id (gensym "ADVANCED-SECTION-")))
  (with-html-output (stream)
    (htm
     (:div :class "section"
	   :id (format nil "section#~A"
		       (cfg::complete-symbol-name (cfg::name section)))
	   (if (plusp (length advanced-options))
	       (let ((button-id (gensym "TOGGLE-ADVANCED-OPTIONS-")))
		 (htm
		  (:div :class "toggle-advanced-options"
			(:a :href "#" :id button-id
			    (str "Toggle advanced options"))
			(:script :language "javascript"
				 (str
				  (ps* `(chain ($ document)
					       (ready (lambda ()
							(chain ($ ,(format nil "#~A" advanced-section-id))
							       (hide))
							(chain ($ ,(format nil "#~A" button-id))
							       (click (lambda ()
									(chain ($ ,(format nil "#~A" advanced-section-id))
									       (toggle)))))))))))))))
	   (htm
	    (:table :class "section"
		    (:tbody
		     (:div :class "options"
			   (loop for option in direct-options
			      do (progn
				   (edit-configuration-option configuration
							      section
							      option
							      stream)
				   (switch-odd-even)))))))
	   (if (plusp (length advanced-options))
	       (htm
		(:div :class "advanced-options"
		      :id advanced-section-id
		      (loop for option in advanced-options
			 do (progn
			      (edit-configuration-option configuration
							 section
							 option
							 stream)
			      (switch-odd-even)))))))))))

(defun edit-configuration-option (configuration section option stream)
  (let ((odd-even (if (equalp *odd-even* :odd)
			"odd"
			"even")))
    (multiple-value-bind (value option-instance section-instance origin)
	(cfg::get-option-value
	  (list (cfg::name section)
		(cfg::name option))
	  configuration nil)
    (with-html-output (stream)
      (htm
       (:tr :class (format nil "option,~A~{,~A~}~{,~A~}"
			   odd-even
			   (if (cfg::optional option)
			       (list "optional"))
			   (if (cfg::advanced option)
			       (list "advanced")))
	    (:td :class "title"
		 (str (cfg::title option)))
	    (:td :class "editor"
		 (render-option-editor (cfg::option-type option)
					 option
					 option-instance
					 value
					 stream :writer (lambda (val)
							  (setf
							    (cfg:get-option-value
							     (list (cfg::name section)
								   (cfg::name option))
							     configuration)
							    val
							    ))))
	    (:td :class "unset"
		 (if (eql origin configuration)
		     (with-form-field (unset :writer (lambda (val)
						       (if val
							   (cfg::unset-option
							    (list (cfg::name section)
								  (cfg::name option))
							    configuration))))
		       (htm (:input :type "checkbox" :name unset)))))
	    (:td :class "origin"
		 (when origin
		   (fmt "(~A)"
			(if (equalp origin :default)
			    "Default"
			    (cfg::title origin)))))))))))

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
			    :reader #'parse-integer)
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
  (let (input-value)
    (with-form-field (field :writer (lambda (val)
				      (setf input-value val))
			    :reader (lambda (val)
				      (if (listp val)
					  (mapcar #'cfg::read-symbol val)
					  (list (cfg::read-symbol val)))))
      (with-change-notifier (field stream :on-change (lambda ()
						       (funcall writer input-value)))
	(with-html-output (stream)
	  (htm
	   (:select :name field
		    :id field
		    :multiple "multiple"
		    (loop for opt in (cfg::options type)
		       do (htm
			   (:option :value (cfg::complete-symbol-name (cfg::name opt))
				    :selected (if (some (lambda (val)
							  (equalp val (cfg::name opt)))
							value)
						  "selected")
				    (str (cfg::title opt)))))))
	  (with-form-field (inherit :writer (lambda (val)
					      (setf (cfg::inherit option) val))
				    :reader (lambda (val)
					      (if val t nil)))
	    (htm
	     (:input :type "checkbox"
		     :name inherit
		     :checked (if (cfg::inherit option)
				  "checked")))))))))

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