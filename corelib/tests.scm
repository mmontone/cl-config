(require 'syntax-case)
(require 'srfi-39)
(require 'define-record-type)
(require 'macro)
(set! *syntax-rules* #t)
(load "scm-testing.scm")
(load "cfg.scm")

(test-begin "testConfig")
(define c (make-config "testConfig"))
(test-equal (config-name c) "testConfig")
(test-end "testConfig")

