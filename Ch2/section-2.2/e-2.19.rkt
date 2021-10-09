#lang racket

; Solution to exercise 2.19 of SICP

(define (no-more? coin-list)
  (null? coin-list))

(define (except-first-denomination coin-list)
  (cdr coin-list))

(define (first-denomination coin-list)
  (car coin-list))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values))
                     coin-values)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

; Test
(cc 100 us-coins)
; 292

; Does the order of the list coin-values affect the answer produced by cc ?
; Ans :
; No, the order does not affect the answer because the procedure is producing
; all possible combination anyways.