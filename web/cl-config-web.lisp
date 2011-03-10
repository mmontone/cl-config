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
	 (configurations-editor s)
	 (new-configuration s)))))))

(defun show-configuration (configuration stream)
  (with-html-output (stream)
    (htm
     (:h2 (str (cfg::title configuration)))
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
	  
(defun show-configuration-schema (configuration-schema)
  (declare (ignore configuration-schema)))

(defvar *odd-even* :odd)

(defun switch-odd-even ()
  (setf *odd-even* 
	(if (equalp *odd-even* :odd)
	    :even
	    :odd)))