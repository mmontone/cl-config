(in-package :cfg.web)

(defun web-installer (installer stream &key when-done)
  (let ((configuration
	 (cfg:with-schema-validation (nil)
	   (cfg:make-configuration :installer '()
				   (list
				    :title (cfg::title installer)
				    :configuration-schema (cfg::configuration-schema installer))))))
    (labels ((render-installer (errors stream)
	       (flet ((save-configuration ()
			(cfg::collecting-validation-errors (errors found-p)
			    (cfg::validate-configuration configuration)
			  (if found-p
			      (with-output-to-string (s)
				(with-main-page (s)
				  (render-installer errors s)))
			      (progn
				(with-open-file (file (cfg::output-file installer))
				  (serialize configuration file
					     :output-backend (ecase (cfg::output-backend installer)
							       (:xml (make-instance cfg::xml-writer))
							       (:sexp (make-instance cfg::sexp-writer)))))
				(funcall when-done))))))
		 (with-html-output (stream)
		   (with-form (action
			       :on-submit #'save-configuration)
		     (htm
		      (:div :class "configuration-installer"
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
				  (:h2 (fmt "~A installer" (cfg::title configuration))))
			    (:form :action action
				   :method "post"
				   :id "install-configuration-form"
				   (jquery.ui-accordion stream
							(loop for section being the hash-values of
							     (cfg::sections (cfg::configuration-schema configuration))
							   collect (cons (cfg::title section)
									 (let ((section* section))
									   (lambda (s)
									     (declare (ignore s))
									     (install-configuration-section configuration section* stream))))))
				   (:input :type "submit" :value "Install")
				   )))))))
	     (render-installer nil stream)))))

(defun install-configuration-section (configuration section stream)
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
	    (:table :class "section"
		    (:tbody
		     (:div :class "options"
			   (loop for option in direct-options
			      do (progn
				   (install-configuration-option configuration
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
			      (install-configuration-option configuration
							 section
							 option
							 stream)
			      (switch-odd-even)))))))))))

(defun install-configuration-option (configuration section option stream)
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
	    (:td :class "installer"
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
	    ))))))