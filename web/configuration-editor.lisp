(in-package :cfg.web)

(defun new-configuration (stream &optional errors)
  (let (name title schema parents documentation)
    (labels
	((render-form (stream errors)
	   (flet ((on-submit ()
		    (cfg::collecting-validation-errors (errors found-p)
			(progn
			  (if (zerop (length name))
			      (cfg::validation-error 'name "Name cannot be empty"))
			  (if (zerop (length title))
			      (cfg::validation-error 'title "Enter a title")))
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
			      (:table :class "attributes-table"
			       (:tbody
				(:tr
				 (:td :class "title"
				      (:p (str "Name:")))
				 (:td :class "editor"
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
				 (:td :class "title"
				      (:p (str "Title:")))
				 (:td :class "editor"
				  (with-form-field (field :writer (lambda (val)
								    (cfg::with-schema-validation (nil)
								      (setf title val))))
				    (htm
				     (:input :type "text"
					     :name field
					     :value title))))
				 (let ((error (find 'title errors :key (lambda (item)
									 (getf item :target)))))
				   (if error
				       (htm (:td (str (getf error :error-msg)))))))
				(:tr
				 (:td :class "title"
				      (:p (str "Schema:")))
				 (:td :class "editor"
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
				 (:td :class "title"
				      (:p (str "Parents:")))
				 (:td :class "editor"
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
					      :class "multiselect"
					      (loop for configuration being the hash-values of *configurations*
						 do (htm
						     (:option :value (cfg::complete-symbol-name (cfg::name configuration))
							      (str (cfg::title configuration))))))))))
				(:tr
				 (:td :class "title"
				      (:p (str "Documentation:")))
				 (:td :class "editor"
				  (with-form-field (field :writer (lambda (val)
								    (setf documentation val)))
				    (htm
				     (:textarea :name field
						(str documentation))))))))
			      (:input :type "submit" :value "Create"))))))))))
	 (render-form stream errors))))

(defun configurations-editor (stream &optional selected-configuration)
  (with-html-output (stream)
    (htm
     (:h1 :class "title"
	  "Configurations editor")
     (if (zerop (hash-table-count *configurations*))
	 (htm
	  (:p "There are no configurations"))
	 (let ((selected-conf (or selected-configuration
				  (first (hash-table-values *configurations*)))))
	   (htm
	    (:div :id "configuration-selector"
		  (:p :class "option" "Configuration:")
		  (:select :id "configuration-select"
			   :name "configuration-select"
			   :class "option-value"
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
  (let ((configuration-copy (cfg::copy-configuration configuration))
	(save-as-name "")
	(save-as-title "")
	(delete-configuration? nil))
    (labels ((render-editor (errors stream)
	       (with-html-output (stream)
		 (with-form (action
			     :on-submit (lambda ()
					  (if delete-configuration?
					      (progn
						(remhash (cfg::name configuration) cfg::*configurations*)
						(with-output-to-string (s)
						  (with-main-page (s)
						    (configurations-editor s))))
					      ;; else
					      (if (plusp (length save-as-name))
						  (let ((new-conf-name
							 (ignore-errors
							   (cfg::read-symbol save-as-name))))
						    (if new-conf-name
							(let ((new-conf (cfg::copy-configuration configuration)))
							  (setf (cfg::name new-conf) new-conf-name)
							  (setf (cfg::title new-conf) save-as-title)
							  (setf (gethash new-conf-name cfg::*configurations*)
								new-conf)
							  (with-output-to-string (s)
							    (with-main-page (s)
							      (configurations-editor s new-conf))))
							(with-output-to-string (s)
							  (with-main-page (s)
							    (configurations-editor s configuration)))))
						  ;; else
						  (progn
						    (cfg::collecting-validation-errors (errors found-p)
							(cfg::validate-configuration configuration-copy)
						      (if found-p
							  (with-output-to-string (s)
							    (with-main-page (s)
							      (render-editor errors s)))
							  (progn
							    (setf (gethash (cfg::name configuration) cfg::*configurations*)
								  configuration-copy)
							    (with-output-to-string (s)
							      (with-main-page (s)
								(render-editor errors s)))))))))))
		   (htm
		    (:div :class "configuration-editor"
			  (when errors
			    (htm
			     (:div :class "errors"
				   (:ul
				    (loop for error in errors
				       do
					 (htm
					  (:li
					   (str (cfg::error-msg error)))))))))
			  (:div :class "title"
				(:h2 (fmt "~A editor" (cfg::title configuration-copy))))
			  (:form :action action
				 :method "post"
				 :id "edit-configuration-form"
				 (jquery.ui-accordion stream
						      (append
						       (loop for section being the hash-values of
							    (cfg::sections (cfg::configuration-schema configuration-copy))
							  collect (cons (cfg::title section)
									(let ((section* section))
									  (lambda (s)
									    (declare (ignore s))
									    (edit-configuration-section configuration-copy section* stream)))))
						       (list
							(cons "Advanced settings"
							     (lambda (s)
							       (declare (ignore s))
							       (htm
								(:table :class "attributes-table"
								   (:tbody
								    (:tr
								     (:td :class "title"
									  (:p (fmt "Name:")))
								     (:td :class "editor"
									  (:p (fmt (cfg::complete-symbol-name
										       (cfg::name configuration-copy))))))
								    (:tr
								     (:td :class "title"
									  (:p "Title:"))
								     (:td :class "editor"
									  (with-form-field (field :writer (lambda (val)
													    (setf (cfg::title configuration-copy) val)))
									    (htm
									     (:input :type "text" :name field :value (cfg::title configuration-copy))))))
								    (:tr
								     (:td :class "title"
									  (:p (str "Schema:")))
								     (:td :class "editor"
									  (:a :href (format nil "/showsc?schema=~A"
											    (cfg::complete-symbol-name
											     (cfg::name
											      (cfg::configuration-schema
											       configuration-copy))))
									      (str (cfg::title
										    (cfg::configuration-schema configuration-copy))))))
								    (:tr
								     (:td :class "title"
									  (:p "Documentation:"))
								     (:td :class "editor"
									  (with-form-field (field :writer (lambda (val)
													    (setf (cfg::documentation* configuration-copy) val)))
									    (htm
									     (:textarea :name field
											(str (cfg::documentation* configuration-copy)))))))
								    (:tr
								     (:td :class "title"
									  (:p "Parents:"))
								     (:td :class "editor"
									  (with-form-field (field :writer (lambda (val)
													    (setf (cfg::parents configuration-copy) val))
												  :reader (lambda (val)
													    (if (listp val)
														(mapcar #'cfg::read-symbol val)
														(list (cfg::read-symbol val)))))
									    (htm
									     (:select :id "parents"
										      :name field
										      :multiple "true"
										      :class "multiselect"
										      (loop for conf being the hash-values of *configurations*
											 when (not (eql conf configuration))
											 do (htm
											     (:option :value (cfg::complete-symbol-name (cfg::name conf))
												      :selected (if (find (cfg::name conf)
															  (cfg::parents configuration-copy))
														    "selected")
												      (str (cfg::title conf)))))))))))))

								)))))
							       
				 (:input :type "submit" :value "Save")
				 (when save-as-new
				   (htm
				    (:div :style "height:20px;")
				     (:table :class "attributes-table"
					     (:tbody
					      (:tr
					       (:td :class "title"
						    (:p "Name: "))
					       (:td :class "editor"
						    (with-form-field (field :writer (lambda (val)
										      (setf save-as-name val)))
						      (htm
						       (:input :type "text" :name field)))))
					      (:tr (:td :class "title"
							(:p "Title: "))
						   (:td :class "editor"
							(with-form-field (field :writer (lambda (val)
											  (setf save-as-title val)))
							  (htm
							   (:input :type "text" :name field)))))))
				    (:input :type "submit" :value "Save as new")
				    (with-form-field (field :writer (lambda (val)
								      (when (equalp val "true")
									(setf delete-configuration? t))))
				      (htm
				       (:input :type "hidden" :name field :id field)
				       (:input :type "button" :class "button" :value "Delete" :id "delete-configuration")
				       (:script :language "javascript"
						(str
						 (ps*
						  `(chain ($ document)
							  (ready (lambda ()
								   (chain ($ ,($id "delete-configuration"))
									  (click (lambda ()
										   (when (confirm "Delete this configuration?")
										     (chain ($ ,($id field))
											    (val "true"))
										     (chain ($ ,($id "edit-configuration-form"))
											    (submit)))))))))))))))))))))))
      (render-editor nil stream))))

(defun edit-configuration-section (configuration section stream)
  (let ((direct-options (cfg::direct-options-list section
						  :exclude-advanced t))
	(advanced-options (cfg::advanced-options-list section))
	(advanced-section-id (gensym "ADVANCED-SECTION-"))
	(section-id (format nil "section#~A"
			    (cfg::complete-symbol-name (cfg::name section)))))
  (with-html-output (stream)
    (htm
     (:div :class "section"
	   :id section-id
	   (if (plusp (length advanced-options))
	       (let ((button-id (gensym "TOGGLE-ADVANCED-OPTIONS-")))
		 (htm
		  (:div :class "toggle-advanced-options"
			(:a :href (format nil "#~A" section-id) :id button-id
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
	    (:table :class "section attributes-table"
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
       (:tr :class (format nil "~A~{,~A~}~{,~A~}"
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
							  (cfg::with-schema-validation (nil)
							    (setf
							     (cfg:get-option-value
							      (list (cfg::name section)
								    (cfg::name option))
							      configuration)
							     val)))))
	    (:td :class "unset"
		 (if (eql origin configuration)
		     (with-form-field (unset :writer (lambda (val)
						       (cfg::with-schema-validation (nil)
							 (if val
							     (cfg::unset-option
							      (list (cfg::name section)
								    (cfg::name option))
							      configuration)))))
		       (htm (:input :type "checkbox" :name unset)))))
	    (:td :class "origin"
		 (when origin
		   (fmt "(~A)"
			(if (equalp origin :default)
			    "Default"
			    (cfg::title origin)))))))))))