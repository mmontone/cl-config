(in-package :cfg.web)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-main-page ((stream) &body body)
    `(render-main-page ,stream (lambda (,stream)
				 (with-html-output (,stream)
				   (htm
				    ,@body)))))
  )

(setf hunchentoot::*catch-errors-p* nil)

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

(define-tabs
    (:configurations "Configurations" "/editconfs")
    (:schemas "Configuration schemas" "/schemas")
  (:import/export "Import/Export" "/import-export")
  (:about "About" "/about"))

(defun start-cl-config-web (&optional (configuration
				       (find-configuration
					'standard-cl-config-web-configuration)))
  "Starts the web configuration editor

   Default arguments are in standard-cl-config-web-configuration

   Evaluate (cfg.web:start-cl-config-web) and point your browser to http://localhost:4242"
  ;; Static dispatcher
  (push 'static-dispatcher *dispatch-table*)
  ;; Forms dispatcher
  (push 'continuation-dispatcher *dispatch-table*)
  (setf *acceptor* (make-instance 'acceptor
				  :address (cfg (:webapp-configuration :host) configuration)
				  :port (cfg (:webapp-configuration :port) configuration)))
  (start *acceptor*))

(defun stop-cl-config-web ()
  "Stops the web configuration editor"
  (stop *acceptor*)
  (setf *acceptor* nil)
  (setf *dispatch-table*
	(delete 'continuation-dispatcher
		(delete 'static-dispatcher *dispatch-table*))))
    
(defun render-main-page (stream body)
  (start-session)
  (initialize-continuations)
  (setf *global-scripts* nil)
  (with-html-output (stream)
    (htm
     (:html
      (:head
       (:title (str (cfg (:general-settings :title)
			 (find-configuration 'standard-cl-config-web-configuration))))
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
	     (:h1 (str (cfg (:general-settings :title)
			    'standard-cl-config-web-configuration)))
	     (funcall body stream))
       (:script :language "javascript"
	   (loop for script in *global-scripts*
		do
		(htm (str script)))))))))

(defun render-configurations-editor (&optional conf)
  (with-html-output-to-string (s)
      (with-main-page (s)
	(with-active-tab :configurations s
	  (apply #'configurations-editor
		 s
		 (when conf
		   (list (find-configuration (cfg::read-symbol conf)))))))))

(define-easy-handler (root :uri "/") ()
  (render-configurations-editor))

(define-easy-handler (editconfs :uri "/editconfs") (conf)
  (render-configurations-editor conf))

(define-easy-handler (about :uri "/about") ()
  (with-html-output-to-string (s)
    (with-main-page (s)
      (with-active-tab :about s
	(htm
	 (:div 
	(:h2 "Homepage")
	(:a :href "http://github.com/mmontone/cl-config"
	    :target "_blank"
	    (str "http://github.com/mmontone/cl-config")))
       (:div
	(:h2 "Documentation")
	(:ul
	 (:li (:a :href "/static/doc/cl-config.html"
		  :target "_blank"
		  (str "Manual in HTML format")))
	 (:li (:a :href "/static/doc/cl-config.pdf"
		  :target "_blank"
		  (str "Manual in PDF format")))))
       (:div
	(:h2 "License")
	(:p
	 (str "Copyright (c) 2011 Mariano Montone"))
	(:p
	 (str"
Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the \"Software\"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:"))
	(:p (str "
The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software."))
	(:p (str "
THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.")))
       
       )))))

(define-easy-handler (configuration-schemas :uri "/schemas") ()
  (with-html-output-to-string (s)
    (with-main-page (s)
      (with-active-tab :schemas s
	(htm
       (:ul
	(loop for name being the hash-keys of *configuration-schemas*
	      using (hash-value schema)
	     do
	     (htm
	      (:li (:a :href (format nil "/showsc?schema=~A"
				     (cfg::complete-symbol-name name))
		       (str (cfg::title schema))))))))))))
	      
(define-easy-handler (import/export :uri "/import-export") ()
  (with-html-output-to-string (s)
    (with-main-page (s)
      (with-active-tab :import/export s
      (edit-configuration (find-configuration 'standard-cl-config-web-configuration) s
			  :save-as-new nil
			  :show-title nil
			  :show-origin nil
			  :show-unset nil
			  :show-advanced-p nil
			  :include-section (lambda (section-name)
					     (equalp section-name :import/export)))))))

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
     (:table
      (:tbody
       (:tr
	(:td (:p (str "Parents: ")))
	(:td
	 (let ((parents (slot-value configuration-schema 'cfg::parents)))
	   (if parents
	       (htm
		(:p
		 (let ((parent (cfg::find-configuration-schema (first parents))))
		   (htm (:a :href (format nil "/showsc?schema=~A"
						  (cfg::complete-symbol-name (first parents)))
			       (str (cfg::title parent)))))
		 (loop for parent-name in (cdr parents)
		    do
		    (let ((parent (cfg::find-configuration-schema parent-name)))
		      (htm
		       (str ", ")
		       (:a :href (format nil "/showsc?schema=~A"
					 (cfg::complete-symbol-name parent-name))
				    (str (cfg::title parent))))))))
	       (htm (:p "No parents"))))))
       (:tr
	(:td
	 (:p (str "Direct sub-schemas: ")))
	(:td
	 (let ((sub-schemas (cfg::direct-sub-schemas configuration-schema)))
	   (if sub-schemas
	       (htm (:p
		     (let ((sub-schema (first sub-schemas)))
		       (htm
			(:a :href (format nil "/showsc?schema=~A"
					 (cfg::complete-symbol-name
					  (cfg::name sub-schema)))
			    (str (cfg::title sub-schema)))))
		     (loop for sub-schema in (cdr sub-schemas)
			do
			  (htm
			   (str ", ")
			   (:a :href (format nil "/showsc?schema=~A"
						 (cfg::complete-symbol-name
						  (cfg::name sub-schema)))
				   (str (cfg::title sub-schema)))))))
	       (htm (:p "No direct sub-schemas"))))))))
     (loop for section being the hash-values of
	  (cfg::sections configuration-schema)
	do
	  (htm
	   (:h3 (str (cfg::title section)))
	   (:table :class "prop-table"
	    (:thead :class "ui-widget-header"
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
		     (:td :class "prop-name ui-state-default"
		      (str (cfg::name option-schema)))
		     (:td :class "prop-title"
			  (str (cfg::title option-schema)))
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
      (with-active-tab :schemas s
	(show-configuration-schema (find-configuration-schema
				    (cfg::read-symbol schema))
				   s)))))

(defvar *odd-even* :odd)

(defun switch-odd-even ()
  (setf *odd-even* 
	(if (equalp *odd-even* :odd)
	    :even
	    :odd)))