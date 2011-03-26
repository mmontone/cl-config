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

(defmacro with-form-field ((var &key
				(reader '#'identity)
				writer on-change)
			   &body body)
  (let ((name (symbol-name var)))
    `(let ((,var (symbol-name (gensym ,name))))
       (setf (gethash ,var *form*)
	     (list :name ,var
	           :reader ,reader
		   :writer ,writer
		   :on-change ,on-change))
       ,@body)))