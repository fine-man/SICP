#lang sicp
; Solution to exercise 1.41
;
; Question :
; Define a procedure double that takes a procedure of one argument
; as argument and returns a procedure that applies the original procedure
; twice. For example, if inc is a procedure that adds 1 to its argument,
; then (double inc) should be a procedure that adds 2. What value is returned by
;
; (((double (double double)) inc) 5)

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

(display (((double (double double)) inc) 5))
; 21

; Analysis
; Perhaps it will be a little easier to first analyse the Procedure produced by
; (double double), when we expand this we get the following expression
; (double (double f)) = (double (f (f x))) = (f (f (f (f x))))
; let the procedure produced by (double double) = g, (define g (double double))
; when we apply the 3rd "double" we get
; (double g) = (g (g f)) = (double (double (double (double f)))), which when
; we open all out is f(f(f(f......f(x))))))) (16 times)
; so we expect the above expression to give an expression that adds 16 to it's arguments
; which is exactly what we see as 21 = 5 + 16
