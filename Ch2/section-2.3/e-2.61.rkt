#lang racket

; solution of exercise 2.61 of SICP
; http://community.schemewiki.org/?sicp-ex-2.61

; element-of-set? procedure
; worst case : O(n)
; on average : n/2 steps
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

; adjoin-set procedure
; worst case (biggest element is x) : O(n)
; average case : n/2 steps
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;; Test
(define evens '(0 2 4 6))
(define odds '(1 3 5))

(element-of-set? 2 evens)
; #t

(adjoin-set 7 odds)
; '(1 3 5 7)

(adjoin-set 1 '())
; '(1)

(adjoin-set 4 '(1 2 3 5 6))
; '(1 2 3 4 5 6)

(adjoin-set 0 '(1 2 3 4))
; '(0 1 2 3 4)

