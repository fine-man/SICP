#lang racket

; Solution to exercise 2.36 of SICP
; http://community.schemewiki.org/?sicp-ex-2.36

; Helper

(define nil '())

; Accumulating elements of a list
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Map Procedure
(define (map proc seq)
  (if (null? seq)
      nil
      (cons (proc (car seq))
            (map proc (cdr seq)))))
                  


; Accumulate-n Procedure
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

;; Test
(define t (list (list 1 2 3) (list 40 50 60) (list 700 800 900)))
(accumulate-n + 0 t)
; '(741 852 963)

(accumulate-n + 0 '((1 2 3) (4 5 6)))
; '(5 7 9)
             
