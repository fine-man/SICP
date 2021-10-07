#lang racket

; Solution to exercise 2.39 of SICP
;

; Helper Procedure

;; -------------------------------------------------------------

; Procedure to append one list to another
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
            (append (cdr l1)
                    l2))))

(define nil '())

; Accumulating procedure redefined as fold-right
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

; Fold-left procedure
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
;;----------------------------------------------------------------

(define (reverse-using-right sequence)
  (fold-right (lambda (first already-reversed)
                (append already-reversed (cons first nil)))
              nil
              sequence))

(define (reverse-using-left sequence)
  (fold-left (lambda (result first) (cons first result)) nil sequence))

;; TEST
(reverse-using-right '(1 2 3))
; '(3 2 1)
(reverse-using-left '(1 2 3))
; '(3 2 1)

