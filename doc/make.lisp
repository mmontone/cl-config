(require 'asdf)
#+sbcl
(require :sb-posix)

(defpackage :cl-config.doc
  (:nicknames :cfg.doc)
  (:use :cl))

(in-package :gestalt.doc)

(defparameter +references-dir-path+ 
  (merge-pathnames 
   #p"doc/references/"
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

(sb-posix:chdir +references-dir-path+)
(load +docstrings-path+)

(sb-texinfo:generate-includes +references-dir-path+
			      (find-package 'cl-config))
(sb-posix:chdir +docs-path+)
(sb-ext:run-program "/usr/bin/texi2html" (list +texinfo-file+))
(sb-ext:run-program "/usr/bin/texi2pdf" (list +texinfo-file+))
(cl-user::quit)