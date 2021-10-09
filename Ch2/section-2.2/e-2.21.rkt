#lang racket

; Solution to exercise 2.21 of SICP

; Helper Procedures
(define (map proc items)
  (if (null? items)
      items
      (cons (proc (car items))
            (map proc (cdr items)))))

(define nil '())

(define (square x) (* x x))

; Sqaure list procedure without using 'map' procedure
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list-map items)
  (map square items))

(square-list (list 1 2 3 4 5))
; '(1 4 9 16 25)

(square-list-map (list 1 2 3 4 5))
; '(1 4 9 16 25)


      