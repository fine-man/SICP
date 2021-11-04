#lang racket

; solution to exercise 2.58b of SICP
; Note : This solution is incomplete and slightly wrong
; check the link below for a complete one
; http://community.schemewiki.org/?sicp-ex-2.58, contains <3 solution

; Procedure to tell if expression is a variable
(define (variable? x) (symbol? x))

; Proceudre to check if a number is equal to an expression
(define (=number? exp num) (and (number? exp) (= exp num)))

; Procedure to tell if two variables are same
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; make-sum which also simplifies expressions
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))


; Procedure to tell if a expression is a sum
(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

; Procedure to return the 1st element of a sum
(define (addend s) (car s))

; Procedure to return the 3rd element of a sum
(define (augend s)
  (if (= (length s) 3)
      (caddr s)
      (cddr s)))

; make-product which also simplifies expressions
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

; Procedure to tell if an expression is a product
(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

; Procedure to return the 1st element of a product
(define (multiplier p) (car p))

; Procedure to return the 3rd element of a product
(define (multiplicand p)
  (if (= (length p) 3)
      (caddr p)
      (cddr p)))

; Derivative function
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
