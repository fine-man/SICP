#lang racket

; solution to exercise 2.59 of SICP

;; Helper procedure 
; equal Procedure from exercise 2.54
(define (equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        ((and (pair? l1) (pair? l2))
         (and (equal? (car l1) (car l2))
              (equal? (cdr l1) (cdr l2))))
        (else (eq? l1 l2))))

; element-of-set procedure
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; union-set procedure
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

;; Test
(define odds '(1 3 5))
(define evens '(0 2 4 6))

(union-set '() '())
'()

(union-set '() evens)
'(0 2 4 6)

(union-set evens evens)
'(0 2 4 6)

(union-set evens odds)
'(0 2 4 6 1 3 5)

(union-set odds evens)
'(1 3 5 0 2 4 6)
