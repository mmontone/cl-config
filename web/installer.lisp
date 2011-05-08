(in-package :cfg.web)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-web-installer-main-page ((installer stream) &body body)
    `(render-web-installer-main-page ,installer ,stream (lambda (,stream)
						      (with-html-output (,stream)
							(htm
							 ,@body))))))

(defun render-web-installer-main-page (web-installer stream body)
  (with-html-output (stream)
    (htm
     (:html
      (:head
       (:title (str (cfg::title web-installer)))
       (:meta :content "text/html; charset=UTF-8"
	      :http-equiv "Content-Type")
       (:meta :content "IE=8" :http-equiv "x-ua-compatible")
       (:script :type "text/javascript"
		:src  "/static/jquery-1.5.1.min.js")
       (:script :type "text/javascript"
		:src "/static/jquery-ui-1.8.11/js/jquery-ui-1.8.11.custom.min.js")
       (:link :type "text/css"
	      :rel "stylesheet"
	      :href"/static/jquery-ui-1.8.11/css/hot-sneaks/jquery-ui-1.8.11.custom.css")
       (:script :type "text/javascript"
		:src "/static/multiselect/js/ui.multiselect.js")
       (:script :type "text/javascript"
		:src  "/static/cl-config.js")
       (:link :type "text/css"
	      :rel "stylesheet"
	      :href "/static/cl-config.css")
       (:link :type "text/css"
	      :rel "stylesheet"
	      :href "/static/multiselect/css/common.css")
       (:link :type "text/css"
	      :rel "stylesheet"
	      :href "/static/multiselect/css/ui.multiselect.css"))
      (:body
       (:div :id "container" :class "container"
	     (:h1 (str (cfg::title web-installer)))
	     (funcall body stream)))))))

(defun start-web-installer (web-installer &optional (host "localhost")
						    (port 4243))
  "Starts a web installer"
  ;; Forms dispatcher
  (let ((acceptor (make-instance 'acceptor
				 :address host
				 :port port
				 :request-dispatcher (installer-request-dispatcher web-installer))))
    (start acceptor)
    acceptor))

(defun installer-request-dispatcher (web-installer)
  (flet ((request-dispatcher (request)
	   (start-session)
	   (initialize-continuations)
	   (when (cl-ppcre:scan "^/$"
				(hunchentoot:request-uri request))
	     (return-from request-dispatcher
	       (funcall web-installer)))

	   (when (and (eq (hunchentoot:request-method request) :get)
		      (cl-ppcre:scan "^/static/(.*)"
				     (hunchentoot:request-uri request)))
	     (cl-ppcre:register-groups-bind (filename) 
		 ("^/static/(.*)" (hunchentoot:request-uri request))
	       (let ((file-path (format nil "~A~A"
					(asdf:system-relative-pathname
					 :cl-config-web "web/static/")
					filename)))
		 (return-from request-dispatcher
		   (hunchentoot:handle-static-file file-path)))))

	   (handle-continuation-request)))
    #'request-dispatcher))

(defun make-web-configuration-installer (installer)
  (flet ((%web-configuration-installer ()
	   (with-output-to-string (s)
	     (web-configuration-installer installer s))))
    #'%web-configuration-installer))

(defparameter *data* nil)

(defun web-configuration-installer (configuration-installer stream)
  (labels ((render-installer-section (section title stream &optional errors)
	     (let ((data *data*))
	       (with-form (action :on-submit
			    (lambda ()
			      (let ((*data* *data*))
				(break "data: ~A" *data*)
				(let ((installer-output (apply configuration-installer data)))
				  (break "out: ~A" installer-output)
				  (if (equalp (car installer-output)
					      :errors)
				      (progn
					(funcall configuration-installer)
					(with-html-output-to-string (s)
					  (with-web-installer-main-page (configuration-installer s)
					    (render-installer-section section title s
								      (cadr installer-output)))))
				      ;; else
				      (let* ((section-name (cadr installer-output))
					     (title (caddr installer-output))
					     (section (gethash section-name
							       (cfg::sections
								(configuration-schema configuration-installer)))))
					(funcall configuration-installer)
					(let ((*data* nil))
					  (with-html-output-to-string (s)
					    (with-web-installer-main-page (configuration-installer s)
					      (render-installer-section section title s))))))))))
	       (with-html-output (stream)
		 (with-web-installer-main-page (configuration-installer stream)
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
					   (str (cfg::error-msg error))))))))))
			  (:form :action action
				 :method "post"
				 :id "install-configuration-form"
				 (install-configuration-section section stream)
				 (htm
				  (:input :type "submit" :value "Next")
				  (:a :class "button"
				      :href
				      (let ((data *data*))
					(action ()
					  (let ((*data* data))
					    (go-back configuration-installer)
					    (funcall configuration-installer)
					    (with-html-output-to-string (s)
					      (with-web-installer-main-page (configuration-installer s)
						(render-installer-section section title s))))))
					  (str "Previous")))))))))))
    (let* ((section-info (funcall configuration-installer))
	   (section-name (cadr section-info))
	   (title (caddr section-info))
	   (section (gethash section-name
			     (cfg::sections
			      (configuration-schema configuration-installer))))
	   (*data* nil))
      (funcall configuration-installer)
      (render-installer-section section title stream))))

(defun install-configuration-section (section stream)
  (let ((direct-options (cfg::direct-options-list section
						  :exclude-advanced t))
	(section-id (format nil "section#~A"
			    (cfg::complete-symbol-name (cfg::name section)))))
  (with-html-output (stream)
    (htm
     (:div :class "section"
	   :id section-id
	   (htm
	    (:div :class "title"
		  (:h2 (str (cfg::title section))))
	    (:table :class "section"
		    (:tbody
		     (:div :class "options"
			   (loop for option in direct-options
			      do (progn
				   (install-configuration-option option
								 stream)
				   (switch-odd-even)))))))
	   )))))

(defun install-configuration-option (option stream)
  (let ((odd-even (if (equalp *odd-even* :odd)
			"odd"
			"even"))
	(value (getf *data* (cfg::name option))))
    (with-html-output (stream)
      (htm
       (:tr :class (format nil "option ~A"
			   odd-even)
	    (:td :class "title"
		 (str (cfg::title option)))
	    (:td :class "installer"
		 (render-option-editor (cfg::option-type option)
				       option
				       nil
				       value
				       stream
				       :writer (lambda (val)
						 (setf (getf *data* (cfg::name option))
						       val)))))))))