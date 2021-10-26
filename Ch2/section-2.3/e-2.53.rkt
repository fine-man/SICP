#lang racket

; Solution to exercise 2.53 of SICP
; http://community.schemewiki.org/?sicp-ex-2.53

; Helper Procedures

; Memq procedure
(define (memq item lis)
  (cond ((null? lis) false)
        ((eq? item (car lis)) lis)
        ; eq? is a procedure that two symbols as arguments and test whether they are same
        (else (memq item (cdr lis)))))

(memq 'apple '(pear banana prune)) ; #f
(memq 'apple '(x (apple sauce) y apple pear)) ; '(apple pear)

; Questions : 
(list 'a 'b 'c)
; '(a b c)

(list (list 'george))
; '((george))

(cdr '((x1 x2) (y1 y2)))
; '((y1 y2))

(cadr '((x1 x2) (y1 y2)))
; '(y1 y2)

(pair? (car '(a short list)))
; #f

(memq 'red '((red shoes) (blue socks)))
; #f

(memq 'red '(red shoes blue socks))
; '(red shoes blue socks)



