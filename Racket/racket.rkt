#lang racket
; lambda are powerfull
((lambda(x) (+ x 3)) 6)

; boolean logic-like expressions
(if #false 'true 'false)

; pairs
(cons 42 43)
(car (cons 42 43))
(cdr (cons 42 43))

; ==
(equal? 42 (car (cons 42 43)))

; vectors
(define vec (vector 'sym "stringa" 42))
(vector-ref vec 0)
(vector-set! vec 0 13)
(vector->list vec)

; hash tables
(define ht (hash
    'foo "bar"
    '(1 2 3) "xyz"
    ))

; back to lambdas
; a simple redefinition of a generic lambda function

(define-syntax f
    (syntax-rules ()
        ; arity one case
        (
         (_ ((X x)) do_something)
         ((lambda (X) do_something) x)
        )

        ; more than one argument
        (
         (_ ((X x) . rest) do_something ...)
         ((lambda (X) (f rest do_something ...)) x)
        )
    )
)

; f as described here is included into racket
; as let*

(f ((x 3)(y 4)) (* y x))

(cond
    ((> 3 3) 'greater)
    ((< 3 3) 'smaller)
    (else 'equal)
)




