(in-package :cfg.web)

(setf hunchentoot::*catch-errors-p* nil) 

(defun start-configuration-editor ()
  (start (make-instance 'acceptor :port 4242)))

(define-easy-handler (main :uri "/")
    ()
  (with-output-to-string (s)
    (with-html-output (s)
      (htm
       (:html
	(:head
	 (:script :type "text/javascript"
		  :src "cl-config.js")
	 (:link :type "text/css"
		:src "cl-config.css"))
	(:body
	 (configurations-editor s)))))))

(defun configurations-editor (stream)
  (with-html-output (stream)
    (htm
     (:h1 "Configurations editor")
     (if (zerop (hash-table-count *configurations*))
	 (htm
	  (:p "There are no configurations"))
	 (progn
	   (htm
	    (:div :id "configuration-selector"
		  (:p "Configuration:")
		  (:select :id "configuration-select"
			   :name "configuration-select"
			   :onclick (format nil "javascript:window.location('/?conf=2');")
			   (loop for conf being the hash-values of *configurations*
			    do
				(htm
				 (:option :name (cfg::name conf)
					  (str (cfg::title conf))))))))
	   (maphash (lambda (name config)
		      (declare (ignore name))
		      (return-from configurations-editor
			(edit-configuration config stream)))
		    *configurations*
		    ))))))

(defun show-configuration (configuration)
  (declare (ignore configuration)))

(defun show-configuration-schema (configuration-schema)
  (declare (ignore configuration-schema)))

(defun edit-configuration (configuration stream)
  (with-html-output (stream)
    (htm
     (:div :class "configuration-editor"
	   (:div :class "title"
		 (:h2 (fmt "~A editor" (cfg::title configuration))))
	   (:form :action (format nil "/editcs?name=~A" (cfg::name configuration))
		  (loop for section being the hash-values of
		       (cfg::sections (cfg::configuration-schema configuration))
		     do (edit-configuration-section configuration section stream))
		  (:input :type "submit" :value "Save"))))))

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
		  (:input :type "submit" :value "Save"))))))

(defvar *odd-even* :odd)

(defun switch-odd-even ()
  (setf *odd-even* 
	(if (equalp *odd-even* :odd)
	    :even
	    :odd)))

(defun edit-configuration-section (configuration section stream)
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
			    (edit-configuration-option configuration
						       section
						       option
						       stream)
			    (switch-odd-even)))))))))
       
(defun edit-configuration-option (configuration section option stream)
  (let ((odd-even (if (equalp *odd-even* :odd)
			"odd"
			"even")))
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
				       (handler-case
					   (cfg::get-option-value
					    (list (cfg::name section)
						  (cfg::name option))
					    configuration)
					 (cfg::option-value-not-found-error ()
					   nil))
				       stream)))))))

(defgeneric render-option-editor (type option value stream)
  )


(defmethod render-option-editor ((type cfg::text-configuration-schema-option-type)
				 option
				 value
				 stream)
  (if value
      (with-html-output (stream)
	(:input :type "text"
		:name (cfg::name option)
		:value value))
      (with-html-output (stream)
	(:input :type "text"))))

(defmethod render-option-editor ((type cfg::one-of-configuration-schema-option-type)
				 option
				 value
				 stream)
  (with-html-output (stream)
    (htm
     (:select :name (cfg::name option)
	      (loop for opt in (cfg::options type)
		 do
		   (if (equalp value (cfg::name opt))
		       (htm
			(:option :value (cfg::name opt)
				 :selected "selected"
				 (str (cfg::title opt))))
		       (htm
			(:option :value (cfg::name opt)
				 (str (cfg::title opt))))))))))

(defmethod render-option-editor ((type cfg::list-configuration-schema-option-type)
				 option
				 value
				 stream)
  (with-html-output (stream)
    (htm
     (:select :name (cfg::name option)
	      :multiple "multiple"
	      (loop for opt in (cfg::options option)
		 do (if (equalp value (cfg::name opt))
			(htm
			 (:option :value (cfg::name opt)
				  :selected "selected"
				  (str (cfg::title opt))))
			(htm
			 (:option :value (cfg::name opt)
				  (str (cfg::title opt))))))))))

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
				 option
				 value
				 stream)
  (with-html-output (stream)
    (if value
	(htm
	 (:input :name (cfg::name option)
		 :type "checkbox"
		 :checked "checked"))
	(htm
	 (:input :name (cfg::name option)
		 :type "checkbox")))))

(defmethod render-option-editor ((type cfg::pathname-configuration-schema-option-type)
				 option
				 value
				 stream)
  (with-html-output (stream)
    (if value
	(htm
	 (:input :type "text"
		 :name (cfg::name option)
		 :value value))
	(htm
	 (:input :type "text"
		 :name (cfg::name option))))))

(defmethod render-option-editor ((type cfg::email-configuration-schema-option-type)
				 option
				 value
				 stream)
  (with-html-output (stream)
    (if value
	(htm
	 (:input :type "text"
		 :name (cfg::name option)
		 :value value))
	(htm
	 (:input :type "text"
		 :name (cfg::name option))))))

(defmethod render-option-editor ((type cfg::email-configuration-schema-option-type)
				 option
				 value
				 stream)
  (with-html-output (stream)
    (if value
	(htm
	 (:input :type "text"
		 :name (cfg::name option)
		 :value value))
	(htm
	 (:input :type "text"
		 :name (cfg::name option))))))

(defmethod render-option-editor ((type cfg::sexp-configuration-schema-option-type)
				 option
				 value
				 stream)
  (if value
      (with-html-output (stream)
	(:textarea :name (cfg::name option)
		   (str value)))
      (with-html-output (stream)
	(:textarea :name (cfg::name option)))))
      