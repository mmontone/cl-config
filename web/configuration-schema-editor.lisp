(in-package :cfg.web)

(defun edit-configuration-schema (configuration-schema stream)
  (with-html-output (stream)
    (htm
     (:form :action "editschema"
	    :method "post"
	    (:p "Title:")
	    (:input :type "text"
		    :value (str (cfg::title configuration-schema)))
	    (:p "Documentation:")
	    (:textarea :name "documentation"
		       (str (cfg::documentation* configuration-schema)))
	    (:p "Parents:")
	    (:select :name "parents"
		     :multiple "multiple"
		     (loop for parent in (cfg::parents configuration-schema)
			do (htm
			    (:option :value (cfg::complete-symbol-name )(cfg::name parent)
				     (str (cfg::title parent))))))
	    (loop for section being the hash-values of
		 (cfg::sections configuration-schema)
	       do
		 (htm
		  (:h3 (str (cfg::title section)))
		  (:table
		   (:thead
		    (:td (str "Name"))
		    (:td (str "Title"))
		    (:td (str "Type"))
		    (:td (str "Optional"))
		    (:td (str "Default"))
		    (:td (str "Advanced"))
		    (:td (str "Documentation")))
		   (:tbody
		    (loop for option-schema being the hash-values of (cfg::direct-options section)
		       do (htm
			   (:tr
			    (:td (:input :type "text"
				  :name "name"
				  :value (cfg::complete-symbol-name (cfg::name option-schema))))
			    (:td (:input :type "text"
					 :name "title"
					 :value (cfg::title option-schema)))
			    (:td (esc
				  (render-schema-option-editor
				   (cfg::option-type option-schema)
				   stream)))
			    (:td
			     (if (cfg::optional option-schema)
				 (htm
				  (:input :type "checkbox"
					  :name "optional"
					  :checked "checked"))
				 (htm
				  (:input :type "checkbox"
					  :name "optional"))))
			    (:td (:input :type "text"
					 :name "default"
					 :value (if (cfg::default option-schema)
						    (cfg::default option-schema)
						    "--")))
			    (:td (if (cfg::advanced option-schema)
				     (htm
				      (:input :type "checkbox"
					      :name "advanced"
					      :checked "checked"))
				     (htm
				      (:input :type "checkbox"
					      :name "advanced"))))
			    (:td (:textarea :name "documentation"
					    (str (cfg::documentation* option-schema)))))))))))
	    (:input :type "submit" :value "Save")))))

(defun render-schema-option-editor (option stream)
  (format stream "Implement this!"))
  