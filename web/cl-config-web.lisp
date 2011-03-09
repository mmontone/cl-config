(in-package :cfg.web)

(setf hunchentoot::*catch-errors-p* nil) 

(defun start-configuration-editor ()
  (start (make-instance 'acceptor :port 4242)))

(define-easy-handler (main :uri "/")
    ()
  (with-output-to-string (s)
    (with-html-output (s)
      (htm
       (:h1 "Configurations editor"))
      (edit-configuration-schema
       (find-configuration-schema 'cfg::database-configuration-schema)
       s))))

(defun show-configuration (configuration))

(defun show-configuration-schema (configuration-schema))

(defun edit-configuration (configuration))

(defun edit-configuration-schema (configuration-schema stream)
  (with-html-output (stream)
    (htm
     (:div :class "configuration-schema-editor"
	   (:div :class "title"
		 (:h2 (fmt "~A editor" (cfg::title configuration-schema))))
	   (:form :action (format nil "/editcs?name=~A" (cfg::name configuration-schema))
		  (loop for section being the hash-values of
		       (cfg::sections configuration-schema)
		     do (edit-configuration-schema-section section stream))
		  (:input :type "submit"))))))

(defvar *odd-even* :odd)

(defun switch-odd-even ()
  (setf *odd-even* 
	(if (equalp *odd-even* :odd)
	    :even
	    :odd)))
       
(defun edit-configuration-schema-section (section stream)
  (with-html-output (stream)
    (htm
     (:div :class "section"
	   :id (format nil "section#~A" (cfg::name section))
	   (:div :class "title"
		 (:h3 (str (cfg::title section))))
	   (:table :class "section"
		   (:tbody
		    (loop for option being the hash-values of (cfg::direct-options section)
		       do (progn
			    (edit-configuration-schema-option option stream)
			    (switch-odd-even)))))))))

(defun edit-configuration-schema-option (option stream)
  (with-html-output (stream)
    (let ((odd-even (if (equalp *odd-even* :odd)
			"odd"
			"even")))
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
	       (render-schema-option-editor (cfg::option-type option)
					    option
					    stream)))))))

(defgeneric render-schema-option-editor (configuration-schema-option-type
					 option
					 stream)
  )

(defgeneric render-option-editor (configuration-option stream))

(defmethod render-schema-option-editor ((type cfg::text-configuration-schema-option-type)
					option
					stream)
  (if option
      (with-html-output (stream)
	(:input :type "text"
		:name (cfg::name option)))
      (with-html-output (stream)
	(:input :type "text"))))

(defmethod render-schema-option-editor ((type cfg::one-of-configuration-schema-option-type)
					option
					stream)
  (with-html-output (stream)
    (htm
     (:select :name (cfg::name option)
	      (loop for opt in (cfg::options type)
		 do (htm
		     (:option :value (cfg::name opt)
			      (str (cfg::title opt)))))))))

(defmethod render-schema-option-editor ((type cfg::list-configuration-schema-option-type)
					option
					stream)
  (with-html-output (stream)
    (htm
     (:select :name (cfg::name option)
	      :multiple "multiple"
	      (loop for opt in (cfg::options option)
		 do (htm
		     (:option (:value (cfg::name opt))
			      (str (cfg::title opt)))))))))

(defmethod render-schema-option-editor ((type cfg::maybe-configuration-schema-option-type)
					option
					stream)
  (with-html-output (stream)
    (if (cfg::default option)
	(htm
	 (:input :name (cfg::name option)
		 :checked "checked"
		 :type "checkbox"
		 (:div :class "maybe-option"
		       (render-schema-option-editor (cfg::type* type)
						    nil
						    stream))))
	(htm
	 (:input :name (cfg::name option)
		 :type "checkbox"
		 (:div :class "maybe-option, disabled"
		       (render-schema-option-editor (cfg::type* type)
						    nil
						    stream))))
	)))

(defmethod render-schema-option-editor ((type cfg::boolean-configuration-schema-option-type)
					option
					stream)
  (with-html-output (stream)
    (if (cfg::default option)
	(htm
	 (:input :name (cfg::name option)
		 :type "checkbox"
		 :checked "checked"))
	(htm
	 (:input :name (cfg::name option)
		 :type "checkbox")))))

(defmethod render-schema-option-editor ((type cfg::pathname-configuration-schema-option-type)
					option
					stream)
  (with-html-output (stream)
    (:input :type "text"
	    :name (cfg::name option))))

(defmethod render-schema-option-editor ((type cfg::email-configuration-schema-option-type)
					option
					stream)
  (with-html-output (stream)
    (:input :type "text"
	    :name (cfg::name option))))

(defmethod render-schema-option-editor ((type cfg::sexp-configuration-schema-option-type)
					option
					stream)
  (with-html-output (stream)
    (:textarea :name (cfg::name option))))