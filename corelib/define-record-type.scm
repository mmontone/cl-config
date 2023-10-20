(load "record.scm")

(define (expand-define-record-type name options)
  (let* ((constructor (list-ref options 0))
         (type-predicate (list-ref options 1))
         (fields (cddr options)))
    `(begin
       (define (,(car constructor) ,@(cdr constructor))
         (let ((record (make-record ',name ',(map car fields))))
           ,@(map (lambda (field-name)
                    `(set-field-value! record ',field-name ,field-name))
                  (cdr constructor))
           record))
       (define (,type-predicate rec)
         (and (vector? rec)
              ;;(>= (vector-size rec) 2)
              (eqv? (vector-ref rec 0) 'record)
              (eqv? (vector-ref rec 1) ',name)))
       ,@(apply append
                (map (lambda (field)
                       (let ((field-name (list-ref field 0))
                             (field-getter (list-ref field 1))
                             (field-setter (or (and (>= (length field) 3)
                                                    (list-ref field 2))
                                               #f)))
                         (if field-setter
                         `((define (,field-getter rec)
                             (get-field-value rec ',field-name))
                           (define (,field-setter rec val)
                             (set-field-value! rec ',field-name val)))
                         `((define (,field-getter rec)
                             (get-field-value rec ',field-name)))
                         )))
                     fields)))))

;; (expand-define-record-type
;;  '<config>
;;  '((%make-config name)
;;    config?
;;    (name config-name set-config-name!)
;;    (doc config-doc set-config-doc!)
;;    (schema config-schema set-config-schema!)
;;    (settings settings set-config-settings!)
;;    (parent config-parent set-config-parent!)
;;    (options config-options set-config-options!)))
       
(defmacro (define-record-type name . options)
  (expand-define-record-type name options))
