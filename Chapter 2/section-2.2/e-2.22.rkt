#lang racket

; Solution of exercise 2.22 of SICP

; Helper Procedures
(define (square x) (* x x))

(define nil '())

; Procedure that takes a list and returns a list with each element squared
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

; Test
(square-list (list 1 2 3 4 5))
; (25 16 9 4 1)

; Q : Why did the above iterative procedure produce the reversed square list ?
;
; The above procedure produces a reversed square list of all elements
; because whenever we square a new element we add it to the start of the
; resulting list called 'answer' and so when the procedure returns the
; list 'answer' we observe that all the elements are in reverse order
;
; Let's evaluate a simple example to see the above in action
;
; (square-list (list 1 2 3))
; square-list will call the procedure 'iter' with the necessary value of the
; formal parameters

; (iter (list 1 2 3) nil)
; as the list 'things' is not null currently so we will process the alternate
; part of the 'if' statement

; (iter (cdr things)
;       (cons (square (car things))
;             answer))
; evaluating the abve by replacing 'things' and 'answer'
; by (list 1 2 3) and '() respectively, we get

; (iter (list 2 3)
;       (cons 1 nil))
; when we evaluate the expression and again process the alternate part of 'if', we get

; (iter (list 3)
;      (cons 4 '(1 nil)))
; Again running the 'iter' procedure with given actual parameters :

; (iter '(3)
;       '(4 1 nil))
; Evaluating the alternate of 'if' again as list is not null

; (iter (cdr '(1))
;       (cons 9 '(4 1 nil)))
; evaluating the value of the parameters to get actual parameter

; (iter '()
;      '(9 4 1 nil))
; This time the list we pass is null so the Procedure will just return the
; list 'answer', which in this case is :
; '(9 4 1)


; Alternate representation of square-list
(define (alt-square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(alt-square-list (list 1 2 3 4 5))
; '(((((() . 1) . 4) . 9) . 16) . 25)
; 
; This representation appends the square of the new-element to the end of
; 'answer' but because 'answer' is a list so we get something like this :
; (list (list  ...) lastest-square).
; (list (() . 1) 4)
