(in-package :cl-config)

;; Reading and writing configuration from and to xml streams

(defclass xml-writer (output-backend)
  ())

(defclass xml-reader (output-backend)
  ())
