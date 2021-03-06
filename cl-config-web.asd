(in-package :cl-user)

(defpackage cl-config-system
  (:use :cl :asdf))
  
(in-package :cl-config-system)

(defsystem cl-config-web
  :name "cl-config"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :version "0.1"
  :maintainer "Mariano Montone <marianomontone@gmail.com>"
  :licence "
Copyright (c) 2011 Mariano Montone

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the \"Software\"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE."
  :description "cl-config configuration web editor."
  :long-description "A web application for editing cl-config configurations."
  :components
  ((:module :web
	    :components
	    ((:file "package")
	     (:file "config")
	     (:file "common")
	     (:file "form")
	     (:file "cl-config-web")
	     (:file "option-editors")
	     (:file "configuration-editor")
	     (:file "configuration-schema-editor")
	     (:file "installer"))
	    :serial t))
  :depends-on (:cl-config :hunchentoot :cl-who
			  :parenscript :ironclad))