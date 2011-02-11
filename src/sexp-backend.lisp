;; Reading and writing configuration from and to sexp streams

(defclass sexp-writer (output-backend)
  ())

(defclass sexp-reader (output-backend)
  ())