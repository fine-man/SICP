#lang sicp
; Solution to exercise 1.40
;
; Question :
; Define a procedure cubic that can be used together with the newtons-method
; procedure in expressions of the form
;
; (newtons-method (cubic a b c) 1)
;
; to approximate zeros of the cubic x^3 + ax^2 + bx + c.
;
; --------------------------------------------------------

; Helper Procedures
(define dx 0.0000001)
(define tolerance 0.0001)

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Derivative Procedure 
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

; Newton's Method
(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

; Procedure to approximate zereos of any Cubic function
(define (solve a b c)
  (newton-method (cubic a b c) 1.0))
