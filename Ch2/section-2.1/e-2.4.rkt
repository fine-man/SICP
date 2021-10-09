#lang racket

; Solution to exercise 2.4 of SICP

; Alternate implementation of (cons x y)/pairs
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

; Test
(define A (cons 1 2))
A
; #<procedure>
; This defines 'A' to be procedure that will take in another procedure/argument
; 'm' and apply it to 'x' and 'y' which is 1 and 2 in this case.
;

(car A)
; output : 1
; The car procedure gives another procedure as an argument to 'A'
; The procedure that it provides to 'A' in turn takes 2 arguments
; and returns the 1st argument as the answer.

; Evaluation on running (car A)
; (car A)
; Giving the lambda procedure as an argument to 'A'
; (A (lambda (p q) p))
; Applying the (lambda (p q) p) procedure to arguments 'x' and 'y' which is 1 and 2 in this case
; ((lambda (p q) p) 1 2)
; Returning the result after processing the above experssion
; 1

(car (cons 2 3))
; 2

(cdr (cons 2 3))
; 3

; Conclusion : we saw that the above representation is a perfectly
; adequate way to represent pair as it fulfills the conditions :
; 1. (car (cons x y)) returns x
; 2. (cdr (cons x y)) returns y
; The above conditions are necessary for a representation to be called a pair.
