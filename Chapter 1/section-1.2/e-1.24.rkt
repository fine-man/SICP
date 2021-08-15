#lang sicp
; solution to exercise 1.24
; Question :
; Modify the timed-prime-test procedure of
; Exercise 1.22 to use fast-prime? (the Fermat method), and
; test each of the 12 primes you found in that exercise. Since
; the Fermat test has Θ(logn) growth, how would you expect
; the time to test primes near 1,000,000 to compare with the
; time needed to test primes near 1000? Do your data bear
; this out? Can you explain any discrepancy you find?

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

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

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ (random (- n 1)) 1)))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))





(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n) n))