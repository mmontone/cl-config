(in-package :cfg.web)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-main-page ((stream) &body body)
    `(render-main-page ,stream (lambda (,stream)
				 (with-html-output (,stream)
				   (htm
				    ,@body)))))
  (defmacro collecting-validation-errors ((errors found-p) expr &body body)
    `(multiple-value-bind (,errors ,found-p)
	 (%collecting-validation-errors
	  (lambda () ,expr))
       ,@body)))

(setf hunchentoot::*catch-errors-p* nil)

(defvar *configuration* nil)

(defun process-params (list)
  (let ((visited (make-hash-table :test #'equalp)))
    (loop for (arg . value) in list
       when (not (gethash arg visited))
       collect
	 (progn
	   (setf (gethash arg visited) t)
	   (cons arg
		 (let ((res
			(loop for (arg1 . value1) in list
			   when (equalp arg arg1)
			   collect value1)))
		   (if (equalp (length res) 1)
		       (car res)
		       res)))))))

(defun invalid-page-request ()
  (with-html-output-to-string (s)
    (htm
     (:html
      (:head
       (:meta :http-equiv "refresh"
	      :content "3; URL=http://localhost:4242/"))
      (:body
       (:h1
	(str "The request is invalid. Redirecting to http://localhost:4242")
       ))))))

(defun handle-continuation-request ()
  (let ((id (cdr (assoc "k" (get-parameters*) :test #'equalp))))
    (let ((cont (gethash id (session-value 'continuations))))
      (if (not cont)
	  (invalid-page-request)
	  ; else
	  (let ((params (remove "k"
				(append (get-parameters*)
					(post-parameters*))
				:key #'car
				:test #'equalp)))
	     ;; Invalidate the continuation
	    (setf (session-value 'continuations)
		  (make-hash-table :test #'equalp))
	    
	    ;; Process params to form lists if necessary
	    (funcall cont (process-params params)))))))

(defun continuation-dispatcher (request)
  (funcall
   (create-prefix-dispatcher
    "/do"
    'handle-continuation-request)
   request))

(defun static-dispatcher (request)
  (funcall
   (create-folder-dispatcher-and-handler
    "/static/"
    (asdf:system-relative-pathname
     :cl-config-web
     "web/static/"))
   request))

(defvar *acceptor* nil)

(defun start-cl-config-web (&optional (configuration
				       (find-configuration
					'standard-cl-config-web-configuration)))
  ;; Static dispatcher
  (push 'static-dispatcher *dispatch-table*)
  ;; Forms dispatcher
  (push 'continuation-dispatcher *dispatch-table*)
  (setf *acceptor* (make-instance 'acceptor
				  :address (cfg (:webapp-configuration :host) configuration)
				  :port (cfg (:webapp-configuration :port) configuration)))
  (start *acceptor*))

(defun stop-cl-config-web ()
  (stop *acceptor*)
  (setf *acceptor* nil)
  (setf *dispatch-table*
	(delete 'continuation-dispatcher
		(delete 'static-dispatcher *dispatch-table*))))
    
(defun render-main-page (stream body)
  (start-session)
  (initialize-continuations)
  (with-html-output (stream)
    (htm
     (:html
      (:head
       (:script :type "text/javascript"
		:src  "/static/jquery-1.5.1.min.js")
       (:script :type "text/javascript"
		:src "/static/jquery-ui-1.8.11/js/jquery-ui-1.8.11.custom.min.js")
       (:link :type "text/css"
	      :rel "stylesheet"
	      :href "/static/jquery-ui-1.8.11/css/ui-lightness/jquery-ui-1.8.11.custom.css")
       (:script :type "text/javascript"
		:src "/static/multiselect/js/ui.multiselect.js")
       (:script :type "text/javascript"
		:src "/static/selectmenu/jquery.ui.selectmenu.js")
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
	      :href "/static/multiselect/css/ui.multiselect.css")
       (:link :type "text/css"
	      :rel "stylesheet"
	      :href "/static/selectmenu/jquery.ui.selectmenu.css"))
      (:body
       (funcall body stream))))))

(defun root-page (&optional conf)
  (with-output-to-string (s)
    (with-main-page (s)
      (apply #'configurations-editor
	     s
	     (when conf
	       (list (find-configuration (cfg::read-symbol conf))))))))

(define-easy-handler (main :uri "/") (conf)
  (root-page conf))

(define-condition validation-error ()
  ((target :initarg :target
	   :reader target
	   :initform (error "Provide the target"))
   (error-msg :initarg :error-msg
	      :reader error-msg
	      :initform (error "Provide the error message")))
  (:report (lambda (c s)
	     (format s "~A in ~A"
		   (error-msg c)
		   (target c)))))

(defun validation-error (target error-msg &rest args)
  (with-simple-restart (continue "Continue")
    (error 'validation-error
	   :target target
	   :error-msg (format nil error-msg args))))

(defun %collecting-validation-errors (func)
  (let ((errors nil))
    (handler-bind
	((validation-error
	  (lambda (c)
	    (push `(:error-msg ,(error-msg c)
		    :target ,(target c))
		  errors)
	    (continue))))
      (funcall func))
    (values errors (plusp (length errors)))))

(defun schema-symbol (string)
  (cfg::read-symbol string))

(defun show-configuration (configuration stream)
  (with-html-output (stream)
    (htm
     (:h2 (str (cfg::title configuration)))
     (:p (str (cfg::documentation* configuration)))
     (:p (fmt "Parents: ~A" (slot-value configuration 'cfg::parents)))
     (loop for section being the hash-values of
	  (cfg::sections (cfg::configuration-schema configuration))
	do
	  (htm
	   (:h3 (str (cfg::title section)))
	   (:table
	    (:tbody
	     (loop for option being the hash-values of
		  (cfg::direct-options section)
		  for value = (or
			       (cfg::get-option-value
				(list (cfg::name section)
				      (cfg::name option))
				configuration nil)
			       (cfg::default option))
		  when value
		do (htm
		    (:tr
		     (:td (str (cfg::title option)))
		     (:td (str value))))))))))))
	  
(defun show-configuration-schema (configuration-schema stream)
  (with-html-output (stream)
    (htm
     (:h2 (str (cfg::title configuration-schema)))
     (:p (str (cfg::documentation* configuration-schema)))
     (:p (str "Parents: "))
     (let ((parents (slot-value configuration-schema 'cfg::parents)))
       (if parents
	   (loop for parent-name in parents
	      do
		(let ((parent (cfg::find-configuration-schema parent-name)))
		  (htm (:p (:a :href (format nil "/showsc?schema=~A"
					     (cfg::complete-symbol-name parent-name))
			       (str (cfg::title parent)))))))
	   (htm (:p "No parents"))))
     (:p (str "Direct sub-schemas: "))
     (let ((sub-schemas (cfg::direct-sub-schemas configuration-schema)))
       (if sub-schemas
	   (loop for sub-schema in sub-schemas
	      do
		(htm (:p (:a :href (format nil "/showsc?schema=~A"
					     (cfg::complete-symbol-name
					      (cfg::name sub-schema)))
			     (str (cfg::title sub-schema))))))
	   (htm (:p "No direct sub-schemas"))))
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
		     (:td (str (cfg::name option-schema)))
		     (:td (str (cfg::title option-schema)))
		     (:td (str (cfg::title (cfg::option-type option-schema))))
		     (:td (str (if (cfg::optional option-schema)
				   "Yes" "No")))
		     (:td (str (if (cfg::default option-schema)
				   (cfg::default option-schema)
				   "--")))
		     (:td (str (if (cfg::advanced option-schema)
				   "Yes" "No")))
		     (:td (str (cfg::documentation* option-schema)))))))))))))

(define-easy-handler (showsc :uri "/showsc") (schema)
  (with-output-to-string (s)
    (with-main-page (s)
      (show-configuration-schema (find-configuration-schema
				  (cfg::read-symbol schema))
				 s))))

(defvar *odd-even* :odd)

(defun switch-odd-even ()
  (setf *odd-even* 
	(if (equalp *odd-even* :odd)
	    :even
	    :odd)))