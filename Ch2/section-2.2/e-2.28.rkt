#lang racket

; Solution to exercise 2.28 of SICP
; Todo : read up different solutions from :
; http://community.schemewiki.org/?sicp-ex-2.28

; Helper Procedures
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
            (append (cdr l1)
                    l2))))

(define (fringe tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(define A (list (list 1 2) (list 3 4)))

(fringe A)
; '(1 2 3 4)

(fringe (list A A))
; '(1 2 3 4 1 2 3 4)

(define B '((2 3)))
(fringe B)
; '(2 3)
                              