#lang racket

; solution to exercise 2.48 of SICP
(require "e-2.46.rkt") ; for vector implementation

; constructor for a segment
(define (make-segment from to)
  (cons from to))

; selector for starting vector of a segment
(define (start-segment segment)
  (car segment))

; selector for ending vector of a segment
(define (end-segment segment)
  (cdr segment))

;; TEST
; (define z (make-vect 0 0))
; (define a (make-vect 1 1))

; (define seg (make-segment z a))

; (start-segment seg)
; '(0 . 0)

; (end-segment seg)
; '(1 . 1)
