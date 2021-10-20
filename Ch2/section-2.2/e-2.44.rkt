#lang racket

; Solution to exercise 2.44 of SICP
(require sicp-pict)

(define wave einstein)

; Up-split procedure
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; TEST
(paint (up-split wave 1))

(paint (up-split wave 6))
