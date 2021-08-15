#lang sicp

; Solution to exercise 1.27
; Question :
; Demonstrate that the Carmichael numbers
; listed in Footnote 1.47 really do fool the Fermat test.That is,
; write a procedure that takes an integer n and tests whether
; a^n is congruent to a modulo n for every a < n, and try your
; procedure on the given Carmichael numbers.

; Helper Procedures
 (define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square(expmod base (/ exp 2) m))
          m))
        (else (remainder
               (* base (expmod base (- exp 1) m))
               m))))

(define (square x) (* x x))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

; Fermat Test
(define (fermat-test n)
  (define (try-it a)
    (cond ((= a n) #t)
          ((= (expmod a n n) a) (try-it (+ a 1)))
          (else #f)))
  (try-it 2))

; Smallest Carmichael numbers : 561, 1105, 1729, 2465, 2851, 6601
(display (smallest-divisor 561))
(newline)
(display (fermat-test 561))
(newline)

(display (smallest-divisor 1105))
(newline)
(display (fermat-test 1105))
(newline)

(display (smallest-divisor 1729))
(newline)
(display (fermat-test 1729))
(newline)

(display (smallest-divisor 2465))
(newline)
(display (fermat-test 2465))
(newline)

(display (smallest-divisor 2821))
(newline)
(display (fermat-test 2821))
(newline)

(display (smallest-divisor 6601))
(newline)
(display (fermat-test 6601))
(newline)

; Output :
; 3
; #t
; 5
; #t
; 7
; #t
; 5
; #t
; 7
; #t
; 7
; #t
; Conclusion : Carmichael numbers pass the fermat-test evethough they are not prime.

