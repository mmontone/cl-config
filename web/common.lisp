(in-package :cfg.web)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-change-notifier ((target stream &key on-change)
				  &body body)
    (let* ((changed-checkbox (gensym "CHANGED-CHECKBOX-")))
      `(with-form-field (,changed-checkbox :writer (lambda (val)
						     (when (plusp (length val))
						       (funcall ,on-change))))
	 (with-html-output (,stream)
	   (htm
	    (:input :id ,changed-checkbox
	            :name ,changed-checkbox
		    :type "hidden"
		    :value "")
	    (:script :language "javascript"
		     (str
		      (ps*
		       `(chain ($ document)
			       (ready (lambda ()
					(chain ($ ,(format nil "#~A" ,target))
					       (change (lambda ()
							 (chain ($ ,(format nil "#~A" ,changed-checkbox))
								(val "true")))))
					))))))))
	 ,@body)))

  )

(defun with-jquery.ui-tabs (stream sections)
    (let ((widget-id (gensym "TABS-"))
	  (divs))
      (with-html-output (stream)
	(htm
	 (:div :id (symbol-name widget-id)
	      (:ul
	       (loop for section in sections
		  for title = (car section)
		  for content = (cadr section)
		  do (htm
		      (:li
		       (if (stringp content)
			   (htm
			    (:a :href content (str title)))
			   (let ((tab-id (gensym "TAB-")))
			     (push (cons tab-id content) divs)
			     (htm (:a :href ($id tab-id) (str title)))))))))
	      (loop for div in divs
		 do (htm
		     (:div :id (car div)
			   (funcall div stream)))))
	(:script :language "javascript"
		 (str (ps* `(chain ($ document)
				   (ready (lambda ()
					    (chain ($ ,(format nil "#~A" widget-id))
						   (tabs (create "load" (lambda (event ui)
									  (chain ($ ".open-in-tab" (@ ui panel))
										 (click (lambda ()
											  (chain ($ (@ ui panel))
												 (load (@ this href)))
											  (return nil))))))))))))))))))

(defvar *tabs*)

(defun make-tab (id title url)
  (list :id id :title title :url url))

(defmacro define-tabs (&rest tabs)
  `(setf *tabs* (mapcar (lambda (args)
			  (apply #'make-tab args))
			',tabs)))

(defun with-active-tab% (stream active-tab content)
  (let ((widget-id (gensym "TABS-")))
    (with-html-output (stream)
      (:div :id widget-id :class "ui-tabs ui-widget ui-widget-content ui-corner-all"
	    (:ul :class "ui-tabs-nav ui-helper-reset ui-helper-clearfix ui-widget-header ui-corner-all"
		 (loop for tab in *tabs*
		    for tab-id = (getf tab :id)
		    for title = (getf tab :title)
		    for url = (getf tab :url)
		    do
		      (htm
			(:li :class (format nil "ui-state-default ui-corner-top ~{~A~}"
					      (if (equalp tab-id active-tab)
						  (list "ui-tabs-selected ui-state-active")))
			       (:a :href url
				   (str title))))))
	      (:div :class "ui-tabs-panel ui-widget-content ui-corner-bottom"
		    (funcall content stream))))))

(defmacro with-active-tab (active-tab stream &body body)
  `(with-active-tab% ,stream ,active-tab (lambda (s) (with-html-output (s)
						  ,@body))))

(defun jquery.ui-accordion (stream sections)
  (let ((widget-id (gensym "ACCORDION-")))
    (with-html-output (stream)
      (htm
       (:div :id (symbol-name widget-id)
	     (loop for (title . content) in sections
		do (htm
		    (:h3 (:a :href "#" (str title)))
		    (:div (funcall content stream))))
	     (htm
	      (:script :language "javascript"
		       (str (ps* `(chain ($ document)
					 (ready (lambda ()
						  (chain ($ ,(format nil "#~A" widget-id))
							 (accordion))))))))))))))

(defun hash-table-values (hash-table &optional sort-predicate key)
  (let ((values nil))
    (maphash (lambda (key value)
	       (push value values))
	     hash-table)
    (if sort-predicate
	(sort values sort-predicate :key key)
	values)))

(defun $id (str)
  (format nil "#~A" str))

(defvar *global-scripts* ())

(defun add-global-script (string)
  (push string *global-scripts*))
