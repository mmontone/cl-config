(in-package :cfg.web)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-main-page ((stream) &body body)
    `(render-main-page ,stream (lambda (,stream)
				 (with-html-output (,stream)
				   (htm
				    ,@body))))))

(setf hunchentoot::*catch-errors-p* nil) 

(defun start-configuration-editor ()
  (start (make-instance 'acceptor :port 4242)))

(defun render-main-page (stream body)
  (with-html-output (stream)
    (htm
     (:html
      (:head
       (:script :type "text/javascript"
		:src  "/static/jquery-1.5.1.min.js")
       (:script :type "text/javascript"
		:src  "/static/cl-config.js")
       (:link :type "text/css"
	      :rel "stylesheet"
	      :href "/static/cl-config.css"))
      (:body
       (esc (funcall body stream)))))))

(define-easy-handler (main :uri "/") ()
  (with-output-to-string (s)
    (with-main-page (s)
      (configurations-editor s))))

(push (create-folder-dispatcher-and-handler "/static/"
					    (asdf:system-relative-pathname
					     :cl-config-web
					     "web/static/"))
      *dispatch-table*)

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
	     (loop for option being the hash-values of (cfg::direct-options section)
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
     (:p (fmt "Parents: ~A" (slot-value configuration-schema 'cfg::parents)))
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

(defvar *odd-even* :odd)

(defun switch-odd-even ()
  (setf *odd-even* 
	(if (equalp *odd-even* :odd)
	    :even
	    :odd)))