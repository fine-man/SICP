#lang racket
(provide (all-defined-out))

#|
As an illustration of symbol manipulation and a further illustration of
data abstraction, consider the design of a procedure that performs symbolic 
differentiation of algebraic expressions.
we would like the procedure to take an algebraic expression and a variable
as an argument and find the derivative of the algebraic expression with
respect to the variable

We can do a bit of wishful-thinking and assume that we already have
a bunch of selectors, constructors and some useful predicates present

|#

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

#|
We can think of many ways to use lists to represent algebraic
expressions. One straightforward choice is to use the same
parenthesized prefix notation that Lisp uses for combinations; that is,
to represent ax + b as (+ (* a x) b), using this representation
we can define the required contructor, selector, predicates the following way:
|#

; Procedure to tell if expression is a variable
(define (variable? x) (symbol? x))

; Procedure to tell if two variables are same
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; Procedure to add two expressions
;(define (make-sum a1 a2) (list '+ a1 a2))

; Procedure to multiply two expressions
;(define (make-product m1 m2) (list '* m1 m2))

; Procedure to tell if a expression is a sum
(define (sum? x) (and (pair? x) (eq? (car x) '+)))

; Procedure to return the 2nd element of a sum
(define (addend s) (cadr s))

; Procedure to return the 3rd element of a sum
(define (augend s) (caddr s))

; Procedure to tell if an expression is a product
(define (product? x) (and (pair? x) (eq? (car x) '*)))

; Procedure to return the 2nd element of a product
(define (multiplier p) (cadr p))

; Procedure to return the 3rd element of a product
(define (multiplicand p) (caddr p))

;; test
; (deriv '(+ x 3) 'x)
; '(+ 1 0)
;
; (deriv '(* x 3) 'x)
;'(+ (* x 0) (* 1 3))
;
; (deriv '(* x y) 'x)
;'(+ (* x 0) (* 1 y))

#|
 The program outputs correct answers but they are not
 simplified, we want the program to know that 
x * 0 = 0, 1 * y = y and 0 + y = y
We will tackle this problem the same way we tackled the problem
of simplest-forms while implementing rational numbers. That is :
We will simplify the expressions while summing/making their products
|#

; Proceudre to check if a number is equal to an expression
(define (=number? exp num) (and (number? exp) (= exp num)))

; make-sum which also simplifies expressions
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

; make-product which also simplifies expressions
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; For Power-Rule implementation of differentiation:
; check solution of exercise 2.56

; For implementation that can handle multiple arguments
; in sum and product i.e - (+ a b c ...), (* a b c ...)
; check solution of exercise 2.57
