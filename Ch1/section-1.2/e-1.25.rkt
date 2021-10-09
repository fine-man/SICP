#lang sicp
; solution to exercise 1.25
; Question :
; Alyssa P. Hacker complains that we went to
; a lot of extra work in writing expmod. After all, she says,
; since we already know how to compute exponentials, we
; could have simply written
;
; (define (expmod base exp m)
;  (remainder (fast-exp base exp) m))
;
; Is she correct? Would this procedure serve as well for our
; fast prime tester? Explain.

; Helper functions
(define (fast-exp a n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp a (/ n 2))))
        (else (* a (fast-exp a (- n 1))))))

(define (square x) (* x x))

(define (report-time start-time)
  (display "***")
  (display (- (runtime) start-time)))

; The original and modified expmod

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else (remainder
               (* base (expmod base (- exp 1) m))
               m))))

(define (modified-expmod base exp m)
  (remainder (fast-exp base exp) m))

; Test speed
(define start-time (runtime))

; (expmod 999999 1000000 1000000)
; (report-time start-time)
; -> 1 *** 353

; (modified-expmod 999999 1000000 1000000)
; (report-time start-time)
; -> 1 *** 6762716

; conclusion : we saw that both the procedure gave the same answer
; but the time if took for "modified-expmod" was around 20000 more than the
; time it took for "expmod". This is because when using the "modified-expmod"
; the "square" function had to multiply very large numbers as they are not getting
; reduced by taking the remainder first and as it takes time to multiply very numbers
; that is why "modified-expmod" takes so much time compared to "expmod"
; In "expmod" we always take the remainder mod "m" so the number that we have to
; square is always less than "m".












