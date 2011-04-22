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
						   (tabs))))))))))))

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