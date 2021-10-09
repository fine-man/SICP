#lang sicp
; Solution to exercise 1.35
;
; Show that the golden ratio Phi (section 1.2.2) is a fixed point of the
; transformation x ->  1 + 1/x, and use this fact to compute by means of the fixed-point procedure.
;
; ---------------------------


; Proof that golden ratio is a fixed point tranformation of x -> 1 + 1/x
; ----------------------------------------------------------------------------
; Definition of Golden Ratio :It is defined as the ratio of consecutive
; terms of the Fibonacci sequence as the number of terms go to infinity
;
; Another way of writing the above the definition is
;
; f(n)/f(n - 1) = f(n + 1)/f(n) as n -> infinity
; f(n)/f(n - 1) = (f(n) + f(n - 1))/f(n) [ f(n + 1) = f(n) + f(n - 1)]
; f(n)/f(n - 1) = 1 + f(n - 1)/f(n)
;
; let f(n)/f(n - 1) = phi = x, so
; x = 1 + 1/x, so we can find phi by finding the fixed-point of x -> 1 + 1/x
; ----------------------------------------------------------------------------

; Calculating phi using the Fixed Point method

(define tolerance 0.0001)

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next))))
  (try first-guess))

(define (calc-phi guess)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               guess))
