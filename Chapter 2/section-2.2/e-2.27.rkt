#lang racket

; Solution to exercise 2.27 of SICP
; Todo : read up other solutions from :
; http://community.schemewiki.org/?sicp-ex-2.27

; nil
(define nil '())

; Procedure to calculate the reverse of a list
(define (reverse-list items)
  (define (iter lis result)
    (if (null? lis)
        result
        (iter (cdr lis)
              (cons (car lis)
                    result))))
  (iter items nil))
 
; Procedure to calculate the deep-reverse of a list of nested lists
(define (deep-reverse tree)
  (define (iter items result)
    (cond ((null? items) result)
          ((not (pair? items)) items)
          (else (iter
                 (cdr items)
                 (cons (deep-reverse (car items))
                       result)))))
  (iter tree nil))

; The above Procedure is very similar to how we traditionally define
; the reverse procedure the only difference is that now we first perform
; the deep-reverse of (car items) as well (because it can be a nested list)
; and then combine it with the list "result" .
; We also added the base case of when the list is just a single number
; which is checked by the procedure (pair? items).

(define A (list (list 1 2) (list 3 4)))
(deep-reverse A)
; '((4 3) (2 1))

(define B '(1 2 (3 4) 5 (6 (7 8) 9) 10)) 
(deep-reverse B)
; '(10 (9 (8 7) 6) 5 (4 3) 2 1)

(define C (list (list 1 2) (list 3 4) (list 5 6 (list 7 8))))
(deep-reverse C)
; '(((8 7) 6 5) (4 3) (2 1))



 

