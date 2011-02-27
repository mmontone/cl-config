(in-package :cfg.web)

(setf hunchentoot::*catch-errors-p* nil) 

(defun start-configuration-editor ()
  (start (make-instance 'acceptor :port 4242)))

(define-easy-handler (main :uri "/")
    ()
  (with-output-to-string (s)
    (with-html-output (s)
      (htm
       (:p "Configurations editor")))))