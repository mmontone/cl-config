(in-package :cl-config)

;; Reading and writing configuration from and to binary streams

(defclass bin-writer (output-backend)
  ())

(defclass bin-reader (output-backend)
  ())

(defmethod run ((writer bin-writer)
		thing
		stream)
  (cl-store:store thing stream))

(defmethod run ((reader bin-reader)
		stream)
  (cl-store:restore stream))