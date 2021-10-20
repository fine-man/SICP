#lang racket

; solution to exercise 2.50 of SICP
; http://community.schemewiki.org/?sicp-ex-2.50
(require "pict-lang.rkt")

; Flip-horiz procedure
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; Procedure that rotates a painter by 180 degrees
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

; Procedre that rotates a painter by 270 degrees
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;; TEST
; (paint rogers)

; (paint (flip-horiz rogers))

; (paint (rotate180 rogers))

; (paint (rotate270 rogers))


