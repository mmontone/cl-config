(require 'regex)
(requrie 'srfi-1)

(define path-separator "/")

(define (file-name pathname)
  (last (vector->list (string-split path-separator pathname))))

(file-name "/home/foo")

(define (pathname-name pathname)
  (vector-ref (string-split "\\." (file-name pathname)) 0))

(pathname-name "asdf")
(pathname-name "foo.txt")

(define (file-extension pathname)
  (let ((file-name (file-name pathname)))
    (if (not (member #\. (string->list file-name)))
        #f
        (last (vector->list (string-split "\\." file-name))))))

(file-extension "asdf")
(file-extension "readme.txt")
