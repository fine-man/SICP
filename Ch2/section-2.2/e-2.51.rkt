#lang racket

; solution to exercise 2.51 of SICP
; http://community.schemewiki.org/?sicp-ex-2.51
(require "pict-lang.rkt")

; Helper Procedure

; Procedre that rotates a painter by 270 degrees
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; below procedure
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-up
           (transform-painter
            painter1
            split-point
            (make-vect 0.0 1.0)
            (make-vect 1.0 0.5)))
          (paint-down
           (transform-painter
            painter2
            (make-vect 0.0 0.0)
            split-point
            (make-vect 1.0 0.0))))
      (lambda (frame)
        (paint-up frame)
        (paint-down frame)))))


; Alternate representation of below
(define (below-2 painter1 painter2)
  (let ((rotated1 (rotate90 painter1))
        (rotated2 (rotate90 painter2)))
    (let ((combined (beside rotated1 rotated2)))
      (rotate270 combined))))

;; TEST
; (paint (below-2 einstein rogers))

