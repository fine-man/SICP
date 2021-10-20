#lang racket

; solution to exercise 2.45 of SICP
; http://community.schemewiki.org/?sicp-ex-2.45
(require sicp-pict)

(define wave einstein)

(define (split orig-placer split-placer)
  (define (rec painter n)
    (if (= n 0)
        painter
        (let ((smaller (rec painter (- n 1))))
          (orig-placer painter (split-placer smaller smaller)))))
  rec)

(define right-split (split beside below))

(define up-split (split below beside))


