(in-package :cfg.web)

(defun new-configuration (stream &optional errors)
  (with-html-output (stream)
    (htm
     (:h2 (str "New configuration"))
     (if (zerop (hash-table-count *configuration-schemas*))
	 (htm (:p "No configuration schemas available to create configurations from"))
	 (htm
	  (:form :action "/newconf"
		 :method "post"
		 :id "newconf"
		 (:table
		  (:tbody
		   (:tr
		    (:td (str "Name:"))
		    (:td (:input :type "text"
				 :name "name"))
		    (let ((error (find 'name errors :key (lambda (item)
							   (getf item :target)))))
		      (if error
			(htm (:td (str (getf error :error-msg))))))
		    )
		   (:tr
		    (:td (str "Title:"))
		    (:td (:input :type "text"
				 :name "title"))
		    (let ((error (find 'title errors :key (lambda (item)
							   (getf item :target)))))
		      (if error
			(htm (:td (str (getf error :error-msg)))))))
		   (:tr
		    (:td (str "Schema:"))
		    (:td (:select :id "schema"
				  :name "schema"
				  (loop for configuration-schema being the hash-values of *configuration-schemas*
				     do (htm
					 (:option :value (cfg::complete-symbol-name (cfg::name configuration-schema))
						  (str (cfg::title configuration-schema))))))))
		   (:tr
		    (:td (str "Parents:"))
		    (select-parents nil
				    (loop
				       for conf being the hash-values of *configurations*
				       collect conf)
				    stream)
		    )
		   (:tr
		    (:td (str "Documentation:"))
		    (:td (:textarea :name "documentation")))
		    ))
		 (:input :type "submit" :value "Create")))))))

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

(defun edit-configuration (configuration stream)
  (with-html-output (stream)
    (htm
     (:div :class "configuration-editor"
	   (:div :class "title"
		 (:h2 (fmt "~A editor" (cfg::title configuration))))
	   (:div :class "name"
		 (:p (fmt "Name: ~A" (cfg::complete-symbol-name (cfg::name configuration)))))
	   (:div :class "schema"
		 (:span (:p (str "Schema:")))
		 (:span (:a :href (format nil "/showsc?schema=~A" (cfg::complete-symbol-name (cfg::name (cfg::configuration-schema configuration))))
			    (str (cfg::title (cfg::configuration-schema configuration))))))
	   (:form :action (format nil "/editcs?name=~A" (cfg::complete-symbol-name (cfg::name configuration)))
		  :method "post"
		  (:p "Documentation:") (:textarea :name "documentation"
						   (str (cfg::documentation* configuration)))
		  (:p "Parents:")
		  (let ((parents (mapcar #'cfg::find-configuration
					   (cfg::parents configuration))))
		      (select-parents parents
				    (loop
				       for conf being the hash-values of *configurations*
				       when (not (find conf parents))
				       collect conf)
				    stream))
		  (loop for section being the hash-values of
		       (cfg::sections (cfg::configuration-schema configuration))
		     do (edit-configuration-section configuration section stream))
		  (:input :type "submit" :value "Save")
		  (:div
		   (:table
		    (:tbody
		     (:tr
		      (:td (:p "Name: ")) (:td (:input :type "text" :name "save-as-name")))
		     (:tr (:td (:p "Title: ")) (:td (:input :type "text" :name "save-as-title"))))))
		  (:input :type "submit" :value "Save as new"))))))

;; (defun edit-configuration-schema (configuration-schema stream)
;;   (with-html-output (stream)
;;     (htm
;;      (:div :class "configuration-schema-editor"
;; 	   (:div :class "title"
;; 		 (:h2 (fmt "~A editor" (cfg::title configuration-schema))))
;; 	   (:form :action (format nil "/editcs?name=~A" (cfg::name configuration-schema))
;; 		  (loop for section being the hash-values of
;; 		       (cfg::sections configuration-schema)
;; 		     do (edit-configuration-schema-section section stream))
;; 		  (:input :type "submit" :value "Save"))))))

(defun edit-configuration-section (configuration section stream)
  (with-html-output (stream)
    (htm
     (:div :class "section"
	   :id (format nil "section#~A" (cfg::complete-symbol-name (cfg::name section)))
	   (:div :class "title"
		 (:h3 (str (cfg::title section))))
	   (:table :class "section"
		   (:tbody
		    (loop for option being the hash-values of (cfg::direct-options section)
		       do (progn
			    (edit-configuration-option configuration
						       section
						       option
						       stream)
			    (switch-odd-even)))))))))
       
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
		 (when value
		   (render-option-editor (cfg::option-type option)
					 option
					 option-instance
					 value
					 stream)))
	    (:td :class "unset"
		 (if (eql origin configuration)
		     (htm (:input :type "checkbox" :name "unset"))))
	    (:td :class "origin"
		 (when origin
		   (fmt "(~A)"
			(if (equalp origin :default)
			    "Default"
			    (cfg::title origin)))))))))))

(defgeneric render-option-editor (type option-schema option value stream)
  )


(defmethod render-option-editor ((type cfg::text-configuration-schema-option-type)
				 option-schema
				 option
				 value
				 stream)
  (with-html-output (stream)
    (:input :type "text"
	    :name (if value (cfg::complete-symbol-name (cfg::name option-schema)))
	    :value (if value value))))

(defmethod render-option-editor ((type cfg::integer-configuration-schema-option-type)
				 option-schema
				 option
				 value
				 stream)
  (with-html-output (stream)
    (:input :type "text"
	    :name (if value (cfg::complete-symbol-name (cfg::name option-schema)))
	    :value (if value value))))

(defmethod render-option-editor ((type cfg::one-of-configuration-schema-option-type)
				 option-schema
				 option
				 value
				 stream)
  (with-html-output (stream)
    (htm
     (:select :name (cfg::complete-symbol-name (cfg::name option-schema))
	      (loop for opt in (cfg::options type)
		 do (htm
		     (:option :value (cfg::complete-symbol-name (cfg::name opt))
			      :selected (if (equalp value (cfg::name opt))
					    "selected")
			      (str (cfg::title opt)))))))))

(defmethod render-option-editor ((type cfg::list-configuration-schema-option-type)
				 option-schema
				 option
				 value
				 stream)
  (with-html-output (stream)
    (htm
     (:select :name (cfg::complete-symbol-name (cfg::name option-schema))
	      :multiple "multiple"
	      (loop for opt in (cfg::options type)
		 do (htm
		     (:option :value (cfg::complete-symbol-name (cfg::name opt))
			      :selected (if (some (lambda (val)
						    (equalp val (cfg::name opt)))
						  value)
					    "selected")
			      (str (cfg::title opt))))))
     (:input :type "checkbox"
	     :name "inherit"
	     :checked (if (cfg::inherit option)
			  "checked")))))

;; (defmethod render-schema-option-editor ((type cfg::maybe-configuration-schema-option-type)
;; 					option
;; 					stream)
;;   (with-html-output (stream)
;;     (if (cfg::default option)
;; 	(htm
;; 	 (:input :name (cfg::name option)
;; 		 :checked "checked"
;; 		 :type "checkbox"
;; 		 (:div :class "maybe-option"
;; 		       (render-schema-option-editor (cfg::type* type)
;; 						    nil
;; 						    stream))))
;; 	(htm
;; 	 (:input :name (cfg::name option)
;; 		 :type "checkbox"
;; 		 (:div :class "maybe-option, disabled"
;; 		       (render-schema-option-editor (cfg::type* type)
;; 						    nil
;; 						    stream))))
;; 	)))

(defmethod render-option-editor ((type cfg::boolean-configuration-schema-option-type)
				 option-schema
				 option
				 value
				 stream)
  (with-html-output (stream)
    (htm
     (:input :name (cfg::complete-symbol-name (cfg::name option-schema))
	     :type "checkbox"
	     :checked (if value "checked")))))

(defmethod render-option-editor ((type cfg::pathname-configuration-schema-option-type)
				 option-schema
				 option
				 value
				 stream)
  (with-html-output (stream)
    (htm
     (:input :type "text"
	     :name (cfg::complete-symbol-name (cfg::name option-schema))
	     :value (if value value)))))

(defmethod render-option-editor ((type cfg::email-configuration-schema-option-type)
				 option-schema
				 option
				 value
				 stream)
  (with-html-output (stream)
    (htm
     (:input :type "text"
	     :name (cfg::complete-symbol-name (cfg::name option))
	     :value (if value value)))))

(defmethod render-option-editor ((type cfg::email-configuration-schema-option-type)
				 option-schema
				 option
				 value
				 stream)
  (with-html-output (stream)
    (htm
     (:input :type "text"
	     :name (cfg::complete-symbol-name (cfg::name option-schema))
	     :value (if value value)))))

(defmethod render-option-editor ((type cfg::sexp-configuration-schema-option-type)
				 option-schema
				 option
				 value
				 stream)
  (with-html-output (stream)
    (:textarea :name (cfg::complete-symbol-name (cfg::name option-schema))
	       (if value (str value)))))
