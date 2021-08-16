#lang sicp
; Solution to exercise 1.28


; Helper Functions
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

; Miller-Rabin-Prime-Test
(define (Miller-Rabin-Test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ (random (- n 1)) 1)))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((Miller-Rabin-Test n) (fast-prime? n (- times 1)))
        (else false)))

; Smallest Carmichael numbers : 561, 1105, 1729, 2465
; (display (fast-prime? 561 10))
; #f
; (display (fast-prime? 1105 10))
; #f
; (display (fast-prime? 1729 10))
; #f
; (display (fast-prime? 2465 10))
;
; Conclusion : Becuase of the extra (non-trivial-root?) checker of "expmod"
; Carmichael numbers cannot fool the Miller-Rabin-Test.
