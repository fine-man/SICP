#lang sicp

; solution to exercise 1.22
; Question :
; Most Lisp implementations include a prim-
; itive called runtime that returns an integer that specifies
; the amount of time the system has been running (mea-
; sured, for example, in microseconds).The following timed-
; prime-test procedure, when called with an integern, prints
; n and checks to see if n is prime. If n is prime, the procedure
; prints three asterisks followed by the amount of time used
; in performing the test.
; ------------------------------------------------------------------

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

; -------------------------------------------------------------------------
; Using this procedure, write a procedure search-for-primes
; that checks the primality of consecutive odd integers in a
; specified range. Use your procedure to find the three small-
; est primes larger than 1000; larger than 10,000; larger than
; 100,000; larger than 1,000,000. Note the time needed to test
; each prime. Since the testing algorithm has order of growth
; of Θ(√n), you should expect that testing for primes around
; 10,000 should take about √10 times as long as testing for
; primes around 1000. Do your timing data bear this out?
; How well do the data for 100,000 and 1,000,000 support the
; Θ(√n) prediction? Is your result compatible with the notion
; that programs on your machine run in time proportional to
; the number of steps required for the computation?
; ---------------------------------------------------------------------------

(define (even? n)
  (= (remainder n 2) 0))

(define (search-for-primes a b)
  (cond ((> a b) (newline) (display "done") (newline))
        ((even? a) (search-for-primes (+ a 1) b))
        (else (timed-prime-test a)
              (search-for-primes (+ a 2) b))))

; First 3 primes greater than 1000
; (search-for-primes 1000 1100)
; 1009 *** 4
; 1013 *** 3
; 1019 *** 3
;
; First 3 primes greater than 10000
; (search-for-primes 10000 10100)
; 10007 *** 9
; 10009 *** 7
; 10037 *** 8
;
; First 3 primes greater than 100000
; (search-for-primes 100000 100100)
; 100003 *** 24
; 100019 *** 23
; 100043 *** 24
;
; First 3 primes greater than 1000000
; (search-for-primes 1000000 1000100)
; 1000003 *** 69
; 1000033 *** 67
; 1000037 *** 67
;
; First 3 primes greater than 10000000
; (search-for-primes 10000000 10001000)
; 10000019 *** 249
; 10000079 *** 216
; 10000103 *** 217
;
;
; The algorithm we use is O(sqrt(n)) in time so the time it takes
; for 10000 should be sqrt(10) times more than time required for 1000
; sqrt(10) = 3.1622776601683795
;
; Average time for 1000 = (4 + 3 + 3)/3 = 3.33
; Average time for 10000 = (9 + 7 + 8)/3 = 8
; Average time for 100000 = (24 + 23 + 24)/3 = 23.66
; Average time for 1000000 = (69 + 67 + 67)/3 = 67.66
; Average time for 10000000 = (249 + 216 + 217)/3 = 227.33
;
; (/ 8 3.33) = 2.4
; (/ 23.66 8) = 2.95
; (/ 67.66 23.66) = 2.85
; (/ 227.33 67.66) = 3.35
; From out data we see that initially the value is very far from
; sqrt(10) but as we get to bigger numbers the value get closer to
; sqrt(10). I think the large deviation for small numbers because
; constant factor of operations have more of an effect thus
; introducing errors.









