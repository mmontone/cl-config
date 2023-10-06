;;;; "scmhob.scm" Scheme runtime support for hobbit.
;For interpretation of code meant for compilation by hobbit.  Never compile!

; bitwise operations: logical shift left and logical shift right
(define (logsleft x y) (ash x y)) (define (logsright x y) (ash x (- 0 y)))
(define logical:logand logand) (define logical:logior logior)
(define logical:logxor logxor) (define logical:lognot lognot)
; immediate-integer (30-bit signed int) versions of arithmetic primitives:
(define %number? number?) (define %eqv? eqv?) (define %zero? zero?)
(define %negative? negative?) (define %positive? positive?)
(define %= =) (define %< <) (define %> >) (define %<= <=) (define %>= >=)
(define %+ +) (define %- -) (define %* *) (define %/ /)
