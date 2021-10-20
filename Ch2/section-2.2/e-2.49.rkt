#lang racket

; solution to exercise 2.49 of SICP
; http://community.schemewiki.org/?sicp-ex-2.49
(require "pict-lang.rkt")

;; Helping vectors
(define zvec (make-vect 0 0))
(define ivec (make-vect 1 0))
(define jvec (make-vect 0 1))
(define tr-cor (make-vect 1 1))
(define A (make-vect 0.5 1))
(define B (make-vect 1 0.5))
(define C (make-vect 0.5 0))
(define D (make-vect 0 0.5))

;; Helping segments
(define bot-seg (make-segment zvec ivec))
(define lft-seg (make-segment zvec jvec))
(define top-seg (make-segment jvec tr-cor))
(define rgt-seg (make-segment ivec tr-cor))

; Helping frames
(define iden-frame (make-frame zvec ivec jvec))
(define diag-frame (make-frame zvec
                               (make-vect 0.5 0.25)
                               (make-vect 0.25 0.5)))

; Painter that takes a frame as an argument and
; draws the boundary of that frame
(define boundary (segment->painter (list bot-seg
                                          lft-seg
                                          top-seg
                                          rgt-seg)))

; Painter that takes a frame as an argument and draws
; the diagonals of the frame
(define diagonals (segment->painter (list (make-segment zvec tr-cor)
                                          (make-segment ivec jvec))))


; Procedure that takes a frame as an argument and draws
; a diamond in that frame
(define diamond (segment->painter (list (make-segment A B)
                                        (make-segment B C)
                                        (make-segment C D)
                                        (make-segment D A))))

;; TEST
; (diagonals iden-frame)

; (diagonals diag-frame)

; (boundary iden-frame)

; (boundary diag-frame)

; (diamond iden-frame)

; (diamond diag-frame)

