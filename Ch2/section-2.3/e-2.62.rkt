#lang racket

; solution to exercise 2.62 of SICP
; http://community.schemewiki.org/?sicp-ex-2.62

; element-of-set? procedure
; worst case : O(n)
; on average : n/2 steps
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

; union-set procedure
; worst case(all elements are unique) : O(n + m)
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1
                             (union-set (cdr set1) (cdr set2))))
                      ((> x1 x2)
                       (cons x2
                             (union-set set1 (cdr set2))))
                      ((< x1 x2)
                       (cons x1
                             (union-set (cdr set1) set2))))))))

;; Test
(define evens '(0 2 4 6))
(define odds '(1 3 5))

(element-of-set? 2 evens)
; #t

(union-set '() evens)
; '(0 2 4 6)

(union-set odds '())
; '(1 3 5)

(union-set '() '())
; '()

(union-set evens '(1 2 3 4))
; '(0 1 2 3 4 6)

(union-set evens odds)
; '(0 1 2 3 4 5 6)

