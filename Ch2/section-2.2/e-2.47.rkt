#lang racket

; solution to exercise 2.47 of SICP
; http://community.schemewiki.org/?sicp-ex-2.47
(require "e-2.46.rkt") ; for vector implementation

; Helper Vectors
(define zvec (make-vect 0.0 0.0))
(define ivec (make-vect 1.0 0.0))
(define jvec (make-vect 0.0 1.0))

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

;; TEST
; (define iden-frame (make-frame zvec ivec jvec))

; (origin-frame iden-frame)
; '(0.0 . 0.0)

; (edge1-frame iden-frame)
; '(1.0 . 0.0)

; (edge2-frame iden-frame)
; '(0.0 . 1.0)


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

;; TEST
; (define iden-frame (make-frame zvec ivec jvec))

; (origin-frame iden-frame)
; '(0.0 . 0.0)

; (edge1-frame iden-frame)
; '(1.0 . 0.0)

; (edge2-frame iden-frame)
; (0.0 . 1.0)
|# 
