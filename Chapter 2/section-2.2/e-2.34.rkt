#lang racket

; Solution to exercise 2.34 of SICP
; http://community.schemewiki.org/?sicp-ex-2.34

; https://en.wikipedia.org/wiki/Horner%27s_method
; Implementation of the algorithm Horner's Rule to evalute values of polynomials
; Accumulating elements of a list
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))
; 79

(horner-eval 2 (list 2 3 0 5 0 1))
; 80

(horner-eval 2 (list 2 3 0 5 0 2))
; 112