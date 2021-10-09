#lang sicp
; Solution to exercise 1.46
;
; Question :
; Several of the numerical methods described in this chapter are instances of an
; extremely general computational strategy known as iterative improvement. Iterative
; improvement says that, to compute something, we start with an initial guess for the
; answer, test if the guess is good enough, and otherwise improve the guess and continue
; the process using the improved guess as the new guess. Write a procedure iterative-improve
; that takes two procedures as arguments: a method for telling whether a guess is good enough
; and a method for improving a guess. Iterative-improve should return as its value a procedure
; that takes a guess as argument and keeps improving the guess until it is good enough.
; Rewrite the sqrt procedure of section 1.1.7 and the fixed-point procedure of
; section 1.3.3 in terms of iterative-improve.

; Helper Procedures
(define (square x)
  (* x x))

(define (average a b)
  (/ (+ a b) 2))

; Iterative-improve Procedure
(define (iterative-improve good-enough? improve-guess)
  (define (try guess)
    (if (good-enough? guess)
        guess
        (try (improve-guess guess))))
  try)

; Sqrt Procedure using iterative-improve
(define (sqrt x)
  (define (close-enough? y)
    (< (abs (- x (square y))) 0.0001))
  (define (improve y)
    (average y (/ x y)))
  ((iterative-improve close-enough? improve) 1.0))

; (sqrt 16)
; 4.000000636692939

; fixed-point using iterative method, as improve is just applying f on the guess
; in this case so we don't define a separate improve procedure
(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) 0.0001))
  ((iterative-improve good-enough? f) first-guess))

; (fixed-point sin 1.0)
; 0.08430921999918502 

