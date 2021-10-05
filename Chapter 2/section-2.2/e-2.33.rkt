#lang racket

; Solution to exercise 2.33 of SICP
; http://community.schemewiki.org/?sicp-ex-2.33

; Helper Procedures

; nil
(define nil '())

(define (square x) (* x x))

; Accumulating elements of a list
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
;-----------------------------------------------------------------------------

(define (map proc sequence)
  (accumulate (lambda (x y)
                (cons (proc x)
                      y))
              nil
              sequence))

(map square (list 1 2 3 4 5))
; '(1 4 9 16 25)

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5 6))
; '(1 2 3 4 5 6)

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length (list 1 2 3 4 5))
; 5
