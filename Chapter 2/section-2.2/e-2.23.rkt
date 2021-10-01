#lang racket

; Solution to exercise 2.23 of SICP

; One possible implementation of 'for-each' can be

(define (for-each proc items)
  (cond ((null? items) (newline)
                       (display "done"))
        (else (proc (car items))
              (for-each proc
                        (cdr items)))))


(for-each display (list 1 2 3 4))
(newline)
; 1234
; done

(for-each (lambda (x)
            (newline)
            (display x))
            (list 57 231 88))

; 57
; 231
; 88
; done


