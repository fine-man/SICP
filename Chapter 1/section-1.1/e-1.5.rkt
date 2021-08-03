; procedure to distinguish between
; normal-order and applicative-order evaluation interpreter

(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

; when we run (test 0 (p))
; normal-order interpreter will give 0
; because it will check that x = 0 and will return 0.
; Applicative-order interpreter will be stuck in an infinite loop
; because it will try to process (p) first to be used as an
; actual parameter for the function "test"

