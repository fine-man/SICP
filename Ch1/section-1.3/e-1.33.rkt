#lang sicp

; Solution to exercise 1.33
; Question
; You can obtain an even more general version of accumulate (exercise 1.32)
; by introducing the notion of a filter on the terms to be combined. That is,
; combine only those terms derived from values in the range that satisfy a
; specified condition. The resulting filtered-accumulate abstraction takes
; the same arguments as accumulate, together with an additional predicate of
; one argument that specifies the filter. Write filtered-accumulate as a procedure.
; Show how to express the following using filtered-accumulate:

; a. the sum of the squares of the prime numbers in the interval a to b
;    (assuming that you have a prime? predicate already written)

; b. the product of all the positive integers less than n that are relatively prime
;    to n (i.e., all positive integers i < n such that GCD(i,n) = 1).
;
; ----------------------------------------------------------------------------

; Helper Procedures
(define (expmod base exp m)
  (define (non-trivial-root? n)
    (cond ((or (= n 1) (= n (- m 1))) 1)
          ((= (remainder (square n) m) 1) 0)
          (else (remainder (square n) m))))  
  (cond ((= exp 0) 1)
        ((even? exp)
         (non-trivial-root? (expmod base (/ exp 2) m)))
        (else (remainder
               (* base (expmod base (- exp 1) m))
               m))))

(define (square x) (* x x))

(define (Miller-Rabin-Test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ (random (- n 1)) 1)))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((Miller-Rabin-Test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? a)
  (fast-prime? a 10))

(define (euclid-gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (gcd a b)
  (euclid-gcd a b))

(define (inc x) (+ x 1))
(define (identity x) x)

; Linear Iterative Filtered Accumulate
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (combiner res
                                 (if (filter a)
                                     (term a)
                                     null-value)))))
  (iter a null-value))


; Sum of Squares of Primes between 'a' and 'b'
(define (sum-square-prime a b)
  (filtered-accumulate prime? + 0 square a inc b))

; Product of all positive integers less than 'n' that have gcd(i, n) = 1
(define (product-coprime n)
  (define (check x)
    (if (= (gcd n x) 1) #t #f))
  (filtered-accumulate check * 1 identity 1 inc n))



