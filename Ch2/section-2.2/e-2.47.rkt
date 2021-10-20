#lang racket

; solution to exercise 2.47 of SICP

; constructor for frames
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

; selector for origin from a frame
(define (origin-frame frame)
  (car frame))

; selector for edge1 of a frame
(define (edge1-frame frame)
  (car (cdr frame)))

; selector for edge2 of a frame
(define (edge2-frame frame)
  (car (cdr (cdr frame))))

#|
; alternate constructor of a frame
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

; selector for origin of a frame
(define (origin-frame frame)
  (car frame))

; selector for edge1 of a frame
(define (edge1-frame frame)
  (car (cdr frame)))

; selector for edge2 of a frame
(define (edge2-frame frame)
  (cdr (cdr frame)))
|#
