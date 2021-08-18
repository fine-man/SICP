#lang sicp

; Helper Procedures
(define (average a b)
  (/ (+ a b) 2))

(define (close-enough? a b)
  (< (abs (- b a)) 0.0001))

(define (identity x) x)

(define (cube x) (* x x x))

; Finding roots of functions using Half-Interval method
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))


(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (positive? a-value) (negative? b-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))


; Calculating pi
(half-interval-method sin 2.0 4.0)




        