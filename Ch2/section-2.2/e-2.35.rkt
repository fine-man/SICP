#lang racket

; Solution to exercise 2.35 of SICP

; Helper Procedures

; Map Procedure
(define (map proc lis)
  (if (null? lis)
      lis
      (cons (proc (car lis))
            (map proc (cdr lis)))))

(define nil '())

; Accumulating elements of a list
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (count-leaves-recursive tree)
  (accumulate + 0
            (map (lambda (x)
                   (cond ((null? x) 0)
                         ((not (pair? x)) 1)
                         (else (count-leaves-recursive x))))
                 tree)))

;; Test
(define tree (list 1 2 (list 3 4) (list 5 (list 6 7))))
(count-leaves-recursive tree)
; 7

(count-leaves-recursive '(1 () () (() ()) () () 2))
; 2

(count-leaves-recursive '(()))
; 0





