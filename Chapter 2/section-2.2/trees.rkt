#lang racket

(define (count-leaves x)
  (cond ((null? x) 0)
        ; pair? is a primitive procedure which tells if it's argument is a pair
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

