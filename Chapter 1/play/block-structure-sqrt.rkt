; sqrt procedure in block form.
; In addition to internalizing the definitions of the procedures 
; we can simplify them
; since x is bound in the definition of the "find-sqrt" definition
; the procedures "good-enough?", "improve" and "sqrt-iter"
; which are now defined internally to "find-sqrt" are
; in the scope of the parameter "x" and so
; it is not necessary to pass "x" explicitly as a parameter to those procedures.

(define (find-sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (square x)
    (* x x))
  (define (average x y)
    (/ (+ x y) 2))
  (sqrt-iter 1.0))
