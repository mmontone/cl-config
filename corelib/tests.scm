(require 'syntax-case)
(require 'srfi-39)
(require 'define-record-type)
(require 'macro)
(set! *syntax-rules* #t)
(load "scm-testing.scm")
(load "cfg.scm")

(test-begin "testConfig")
(define cfg-test:c1 (make-config "testConfig"))
(test-equal (config-name cfg-test:c1) "testConfig")
(test-end "testConfig")

(define cfg-test:schema1
  (make-schema-from-spec
   'schema-1
   '(#f
     ((host string :default "localhost")
      (port integer :default 80)))))

(define cfg-test:config1
  (make-config-from-spec
   "Config1"
   '(#f
     ((host . "some")
      (port . 22)))))

(validate-with-schema cfg-test:config1 cfg-test:schema1)

(define cfg-test:config2
  (make-config-from-spec
   "Config2"
   '(#f
     ((host . 444)
      (port . 22)))))

(validate-with-schema cfg-test:config2 cfg-test:schema1)
