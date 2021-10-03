#lang racket

; Solution to exercise 2.32 of SICP


(define (subsets s)
  (if (null? s)
      (list s)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s)
                                  x))
                          rest)))))

(subsets '(1 2 3))