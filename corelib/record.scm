(define (list-index fn list)
  (let iter ((list list) (index 0))
    (if (null? list)
        -1
        (let ((item (car list)))
          (if (fn item)
              index
              (iter (cdr list) (+ index 1)))))))

(define (make-record name fields)
  (let ((record (make-vector 4)))
    (vector-set! record 0 'record) ;; tag
    (vector-set! record 1 name) ;; name
    (vector-set! record 2 fields) ;; field names
    (vector-set! record 3 (make-vector (length fields))) ;; field values
    record))

(define (curry f . c)
  (lambda x (apply f (append c x))))

(define (field-value record field-name)
  (let ((field-index (list-index (curry eqv? field-name)
                                 (vector-ref record 2))))
    (if (=? field-index -1)
        (error "Invalid field")
        (vector-ref (vector-ref record 3) field-index))))

(define (set-field-value! record field-name value)
  (let
      ((field-index (list-index (curry eqv? field-name)
                                (vector-ref record 2))))
    (if (=? field-index -1)
        (error "Invalid field")
        (begin
          (vector-set! (vector-ref record 3) field-index value)
          value))))

;;(define myrec
;;  (make-record 'my-record '(name lastname)))

;; (field-value myrec 'name)
;; (set-field-value! myrec 'name "Mariano")
;; (field-value myrec 'name)
;; (set-field-value! myrec 'name "Martin")
