#lang sicp

(define tolerance 0.0001)

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Sqrt procedure using fixed-point

(define (sqrt x)
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2))
               1.0))
