#lang racket

; Solution to exercise 2.25 of SICP
; http://community.schemewiki.org/?SICP-Solutions

; Q : Give combinations of cars and cdrs that will pick 7 from
; each of the following lists:
; (1 3 (5 7) 9)
; ((7))
; (1 (2 (3 (4 (5 (6 7))))))

(define (cadr x)
  (car (cdr x)))

(define A '(1 3 (5 7) 9))
(car (cdr (car (cdr (cdr A)))))
; 7

(define B '((7)))
(car (car B))
; 7

(define C '(1 (2 (3 (4 (5 (6 7)))))) )
(cadr (cadr (cadr (cadr (cadr (cadr C))))))
; 7
