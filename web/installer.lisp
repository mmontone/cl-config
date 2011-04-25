(in-package :cfg.web)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-main-web-installer-page ((stream) &body body)
    `(render-web-installer-main-page ,stream (lambda (,stream)
					       (with-html-output (,stream)
						 (htm
						  ,@body))))))

(defun render-web-installer-main-page (stream body)
  (start-session)
  (initialize-continuations)
  (setf *global-scripts* nil)
  (let ((web-installer (session-value 'web-installer)))
    (with-html-output (stream)
      (htm
       (:html
	(:head
	 (:title (str (title web-installer)))
	 (:meta :content "text/html; charset=UTF-8"
		:http-equiv "Content-Type")
	 (:meta :content "IE=8" :http-equiv "x-ua-compatible")
	 (:script :type "text/javascript"
		  :src  "/static/jquery-1.5.1.min.js")
	 (:script :type "text/javascript"
		  :src "/static/jquery-ui-1.8.11/js/jquery-ui-1.8.11.custom.min.js")
	 (:link :type "text/css"
		:rel "stylesheet"
		:href "/static/jquery-ui-1.8.11/css/hot-sneaks/jquery-ui-1.8.11.custom.css")
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
	 (funcall body stream)
	 (:script :language "javascript"
		  (loop for script in *global-scripts*
		     do
		       (htm (str script))))))))))

(defclass web-installer ()
  ((installer :initarg :installer
	      :initform (error "Provide the installer"))
   (welcome-rendering-function :initarg :welcome-rendering-function
			       :initform (error "Provide a welcome rendering function")
			       :accessor welcome-rendering-function)
   (install-steps-rendering-functions
    :initarg :install-steps-rendering-functions
    :initform (error "Provide install-steps rendering functions")
    :accessor install-steps-rendering-functions)
   (done-rendering-function :initarg :done-rendering-function
			    :initform (error "Provide a done rendering function")
			    :accessor done-rendering-function)))

(defclass standard-web-installer (web-installer)
  ())

(defmethod initialize-instance :after ((web-installer standard-web-installer) &rest initargs)
  (declare (ignore initargs))
  (let ((configuration (configuration web-installer)))
    (setf (install-steps-rendering-functions web-installer)
	  (mapcar (lambda (section)
		    (scaffold-install-step-rendering-function configuration section))
		  (cfg::direct-sections configuration)))))

(defvar *web-installer* nil)

(defun render-welcome-screen (web-installer)
  (with-html-output-to-string (s)
    (with-main-web-installer-page (s)
      (funcall (welcome-rendering-function web-installer) s)
      (htm
       (:a :class "button" :href "/install-step?step=1"
	   (str "Start"))))))

(defun render-install-step-screen (web-installer install-step)
  (with-html-output-to-string (s)
    (with-main-web-installer-page (s)
      (let ((install-step-rendering-function
	     (nth (1- install-step)
		  (install-step-rendering-functions web-installer))))
	(let ((next-action (if (equalp install-step (length (install-step-rendering-functions web-installer)))
			       "/done"
			       (funcall install-step-rendering-function s
				    :next 
				    (lambda ()
				      (render-install-step-screen web-installer (1+ install-step)))))))
	  (htm
	   (:a :class "button" :href next-action
	       "Next")
	   (if (plusp (1- install-step))
	       (:a :class "button" :href (format nil "/install-step?step=~A" (1- install-step))))))))))

(define-easy-handler (install-step :uri "/install-step") ((is :real-name "step"))
  (with-html-output-to-string (s)
    (with-main-web-installer-page (s)
      (render-step-screen (session-value 'web-installer)
			  (parse-integer is)))))

(defun start-web-installer (web-installer &key host port)
  "Starts a web installer

   Default arguments are in standard-cl-config-web-configuration

   Evaluate (cfg.web:start-cl-config-web) and point your browser to http://localhost:4242"
  ;; Static dispatcher
  (push 'static-dispatcher *dispatch-table*)
  ;; Forms dispatcher
  (push 'continuation-dispatcher *dispatch-table*)
  (setf *web-installer* (make-instance 'acceptor
				  :address host
				  :port port))
  (start *web-installer*)
  (start-session)
  (setf (session-value 'web-installer) web-installer))

(defun stop-web-installer ()
  "Stops the web installer"
  (stop *web-installer*)
  (setf *web-installer* nil)
  (setf *dispatch-table*
	(delete 'continuation-dispatcher
		(delete 'static-dispatcher *dispatch-table*))))

(define-easy-handler (web-installer-main :uri "/installer") ()
  (render-welcome-screen (session-value 'web-installer)))

(define-easy-handler (web-installer-done :uri "/done") ()
  (render-done-screen (session-value 'web-installer)))

(defun scaffold-step-rendering-function (configuration section)
  (lambda (stream)
    (install-configuration-section configuration section stream)))

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