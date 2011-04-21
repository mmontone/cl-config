(require 'asdf)
#+sbcl
(require :sb-posix)
(require :cl-config-web)

(defpackage :cl-config.doc
  (:nicknames :cfg.doc)
  (:use :cl))

(in-package :cl-config.doc)

(defparameter +references-dir-path+ 
  (merge-pathnames 
   #p"doc/references/"
   (asdf:component-pathname (asdf:find-system 'cl-config))))

(defparameter +include-references-path+ 
  (merge-pathnames 
   #p"doc/references.texinfo"
   (asdf:component-pathname (asdf:find-system 'cl-config))))

(defparameter +docstrings-path+
  (merge-pathnames 
   #p"doc/docstrings.lisp"
   (asdf:component-pathname (asdf:find-system 'cl-config))))

(defparameter +docs-path+
  (merge-pathnames 
   #p"doc/"
   (asdf:component-pathname (asdf:find-system 'cl-config))))

(defparameter +texinfo-file+
  (merge-pathnames
   #p"cl-config.texinfo"
   +docs-path+))

(defparameter +info-dir-path+
  #p"/usr/share/info/")

(sb-posix:chdir +references-dir-path+)
(load +docstrings-path+)

(sb-texinfo:generate-includes +references-dir-path+
			      (find-package 'cl-config)
			      (find-package 'cl-config.web))

(with-open-file (f +include-references-path+
		   :if-does-not-exist :create
		   :if-exists :supersede
		   :direction :output)
  (loop for filepath in (directory (merge-pathnames +references-dir-path+
					 "*.texinfo"))
       do
       (format f "@include ~A/~A~%"
	       (car (last (pathname-directory +references-dir-path+)))
	       (file-namestring filepath))))

(sb-posix:chdir +docs-path+)
(sb-ext:run-program "/usr/bin/texi2html" (list "--css-include=cl-config.css"
					       (format nil "~A" +texinfo-file+))
		    :wait t)
(sb-ext:run-program "/usr/bin/texi2pdf" (list (format nil "~A" +texinfo-file+)))
(sb-ext:run-program "makeinfo" (list "--plaintext cl-config.texinfo -o cl-config.txt"))
(sb-ext:run-program "makeinfo" (list "cl-config.texinfo"))
(sb-ext:run-program "ginstall-info" (list (format nil "--info-dir=~A" +info-dir-path+)
					  "cl-config.info"))
(cl-user::quit)