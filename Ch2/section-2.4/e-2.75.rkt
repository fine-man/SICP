#lang racket

; solution to exercise 2.75 of SICP
; http://community.schemewiki.org/?sicp-ex-2.75

; make-from-mag-ang procedure using message-passing
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)
