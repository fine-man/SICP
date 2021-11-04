#lang racket
; solution to exercise 2.56 of SICP
; http://community.schemewiki.org/?sicp-ex-2.56

; All the required procedures to differentiaion
(require "sym-differentiation.rkt")

; Procedure to make an algebraic-exponent from an expression
(define (make-exponentiation exp n)
  (cond ((= n 0) 1)
        ((= n 1) exp)
        (else (list '** exp n))))

; Procedure to return the base of an exponent
(define (base exp) (cadr exp))

; Procedure to return the exponent of an exponential
(define (exponent exp) (caddr exp))

; Procedure to tell if an expression is an exponential?
(define (exponentiation? exp) (and (pair? exp) (eq? '** (car exp))))


; derivative procedure with power rule built in
(define (deriv exp var)
        ; derivative of a constant
  (cond ((number? exp) 0)
        ; derivative of a single variable
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ; sum-rule of differentiation 
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ; Product rule of differentiation 
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ; Power rule of differentiaion
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp) (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))


;; TEST

;(define ex (make-exponentiation 'x 5))
;(deriv ex 'x)
;'(* 5 (** x 4))

;(define a (make-exponentiation '(+ x 1) 5))
;(deriv a 'x)
;'(* 5 (** (+ x 1) 4))


