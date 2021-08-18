#lang sicp

; Helper Procedures

(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (average a b)
  (/ (+ a b) 2))

(define (exp b n)
  (cond ((= n 0) 1)
        ((even? n) (exp (square b) (/ n 2)))
        (else (* b (exp b (- n 1))))))

(define (average-damp f)
  (lambda (x) (average x (f x))))

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

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond ((= n 1) f)
        (else (compose f (repeated f (- n 1))))))


; 4th root procedure

(define (four-root x)
  (fixed-point ((repeated average-damp 2)
                (lambda (y) (/ x (cube y))))
               1.0))

(define (nth-root x n)
  (fixed-point ((repeated average-damp 1)
                (lambda (y) (/ x (exp y (- n 1)))))
               1.0))


