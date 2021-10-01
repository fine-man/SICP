#lang racket

; Solution to exercise 2.17

; Procedure for finding the nth element of the list using 0 indexing.
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

; Procedure to calculate the length of a list
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))


; Procedure to find the last element of a list
(define (last-pair items)
  (list (list-ref
         items
         (- (length items) 1))))


; Test
(last-pair (list 23 72 149 34))