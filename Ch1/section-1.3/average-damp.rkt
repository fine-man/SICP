#lang sicp

; Helper Procedures
(define (average a b)
  (/ (+ a b) 2))

(define (square x)
  (* x x))

(define tolerance 0.0001)

(define (close-enough? a b)
  (< (abs (- a b)) tolerance))


(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Average-damp Procedure
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point
   (average-damp (lambda (y) (/ x y)))
   1.0))

(define (cube-root x)
  (fixed-point
   (average-damp (lambda (y) (/ x (square y))))
   1.0))