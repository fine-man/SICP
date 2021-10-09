#lang racket

; Solution to exercise 2.20 of SICP

; Procedure takes one or more integers and returns a list of all
; arguments that have the same even-odd parity as the first argument.
(define (same-parity p . items)
  (define (iter par items)
    (cond ((null? items) items)
          ((= par
              (remainder (car items) 2))
           (cons (car items)
                 (iter par (cdr items))))
          (else (iter par (cdr items)))))
  (cons p
        (iter (remainder p 2)
              items)))

(same-parity 1 2 3 4 5 6 7)
; '(1 3 5 7)

(same-parity 2 3 4 5 6 7)
; '(2 4 6)

(same-parity 2)
; '(2)
