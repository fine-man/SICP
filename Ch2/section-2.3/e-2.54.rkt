#lang racket

; solution to exercise 2.54 of SICP
; http://community.schemewiki.org/?sicp-ex-2.54

(define (equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        ((and (pair? l1) (pair? l2))
         (and (equal? (car l1) (car l2))
              (equal? (cdr l1) (cdr l2))))
        (else (eq? l1 l2))))

;; TEST
; (equal? '(this is a list) '(this is a list))
; #t

; (equal? '(this is a list) '(this (is a) list))
; #f
