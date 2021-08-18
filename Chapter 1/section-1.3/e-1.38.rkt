#lang sicp

; Solution to exercise 1.38
; Question :
; In 1737, the Swiss mathematician Leonhard
; Euler published a memoir De Fractionibus Continuis, which
; included a continued fraction expansion for e − 2, where
; e is the base of the natural logarithms. In this fraction, the
; Ni are all 1, and the Di are successively 1, 2, 1, 1, 4, 1, 1,
; 6, 1, 1, 8, . . .. Write a program that uses your cont-frac
; procedure from Exercise 1.37 to approximate e, based on
; Euler’s expansion.

; Helper Procedures
(define (one x) 1.0)

(define (d x)
  (cond ((= (remainder x 3) 2) (* 2 (/ (+ x 1) 3)))
        (else 1.0)))

; Linear Iterative continued fraction procedure
(define (cont-frac n d k)
  (define (iter i result)
    (cond ((= i 0) result)
          (else
           (iter (- i 1) (/ (n i) (+ (d i) result))))))
  (iter (- k 1) (/ (n k) (d k))))

(define (calc-e k)
  (+ 2 (cont-frac one d k)))

(display (calc-e 100))
; 2.7182818284590455
(display (calc-e 1000))
; 2.7182818284590455


