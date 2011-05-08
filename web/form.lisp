(in-package :cfg.web)

(defun form-id (form-definition)
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence 
    :sha1 
    (sb-ext:string-to-octets (format nil "~A" form-definition) 
			     :external-format :utf8))))

(defvar *form* nil)

(defun initialize-continuations ()
  (if (not
       (hunchentoot:session-value 'continuations))
      (setf (hunchentoot:session-value 'continuations)
	    (make-hash-table :test #'equalp))))
  
(defun register-continuation (id closure)
  (setf (gethash id (hunchentoot:session-value 'continuations))
	closure))

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

(defmacro with-form ((action &key on-submit)
		&body body)
  (let* ((cont-id (form-id body))
	 (uri (format nil "/do?k=~A"
		      (hunchentoot:url-encode cont-id))))
    `(let ((*form* (make-hash-table :test #'equalp))
	   (,action ,uri))
	 ,@body
	 (let ((form *form*))
	   (register-continuation
	    ,cont-id
	    (lambda (args)
	      (loop for field being the hash-values of form
		   do
		   (let* ((arg (assoc (getf field :name) args :test #'equalp))
			  (arg-value (cdr arg)))
		     (funcall (getf field :writer)
			      (funcall (getf field :reader) arg-value))))
	      (funcall ,on-submit)))))))

(defmacro action (args &body body)
  (declare (ignore args))
  (let* ((cont-id (form-id body))
	 (uri (format nil "/do?k=~A"
		      (hunchentoot:url-encode cont-id))))
    `(progn
       (register-continuation ,cont-id (lambda (&rest args)
					 (declare (ignore args))
					 ,@body))
       ,uri)))
  
(defmacro with-form-field ((var &key
				(reader '#'identity)
				(writer '#'identity)
				(on-change '(lambda (x)
					     (declare (ignore x))
					     nil
					     )))
			   &body body)
  (let ((name (symbol-name var)))
    `(let ((,var (symbol-name (gensym ,name))))
       (setf (gethash ,var *form*)
	     (list :name ,var
	           :reader ,reader
		   :writer ,writer
		   :on-change ,on-change))
       ,@body)))