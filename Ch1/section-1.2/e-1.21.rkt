#lang sicp
; solution to exercise 1.21
; Question :
; Use the smallest-divisor procedure to find
; the smallest divisor of each of the following numbers: 199,
; 1999, 19999.

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

; (display smallest-divisor 199)
; 199
; (display smallest-divisor 1999)
; 1999
; (dispaly smallest-divisor 19999)
; 7














