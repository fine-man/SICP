; solution to exercise 1.6
; Question :
; Alyssa P. Hacker doesn’t see why if needs to
; be provided as a special form. “Why can’t I just define it as
; an ordinary procedure in terms of cond?” she asks. Alyssa’s
; friend Eva Lu Ator claims this can indeed be done, and she
; defines a new version of if
; What happens when Alyssa aempts to use this to compute square roots? Explain.


; Because Lisp uses applicative-order evaluation so
; "then-clause" and "else-clause" will be evaluated first
; whenever "new-if" procedure is called

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

; self-referencing procedure so if <consequent> and <alternative>
; are evaluated all the time then it will result in an infinite loop

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (find-sqrt x)
  (sqrt-iter 1.0 x))
