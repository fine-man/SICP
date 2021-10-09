#lang sicp

; Solution to exercise 1.30
; The sum procedure above generates a linear recursion. 
; The procedure can be rewritten so that the sum is performed iteratively. 
; Show how to do this by filling in the missing expressions in the following definition:
;
; (define (sum term a next b)
;   (define (iter a result)
;     (if <??>
;         <??>
;         (iter <??> <??>)))
;   (iter <??> <??>))
; ---------------------------------

; One thing to note is that all arguments passed to sum procedure are available
; in internal iter procedure as well.

; Helper Procedures

(define (cube x)
  (* x x x))

(define (inc x)
  (+ x 1))


; Linear Iterative Sum-Procedure
(define (iterative-sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (+ result (term a)))))
  (iter a 0))

; Test procedure

(define (sum-cubes a b)
  (iterative-sum cube a inc b))

; (display (sum-cubes 1 10))
; 3025

