#lang racket

; Solution to exercise 2.18

(define nil '())

(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
  (iter items nil))

(reverse (list 1 2 3 4))


