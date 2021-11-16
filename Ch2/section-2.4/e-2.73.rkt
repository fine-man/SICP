#lang racket

; Solution to exercise 2.73 of SICP
; http://community.schemewiki.org/?sicp-ex-2.73

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp)
               var))))

(define (operator exp) (car exp))
(define (operand exp) (cdr exp))

#|
(a) what we have done here is applied data-directed programming to
symbollic differentiation ,we filled the lookup table with
the derivate procedures for different types of expressions and indexed them
with ('deriv, '<operator symbol>) and then dispatch different types of expressions
accordingly. We cannot assimilate the predicates number? and variable? into the
data-directed dispatch because they are predicates and do no represent different types
of expressions and so there is nothing to dispatch in the first place.
|#

; (b)

(define (make-sum x y)
  (list '+ x y))

(define (make-product x y)
  (list '* x y))

(define install-sum-derivation
  (define (addend operands)
    (car operands))
  (define (augend operands)
    (cadr operands))
  (define (deriv-sum operands var)
    (make-sum
     (deriv (addend operands))
     (deriv (augend operands))))
  ; inserting the function into the table
  (put 'deriv '+ deriv-sum)
  'done)

(define install-product-derivation
  (define (multiplier operands)
    (car operands))
  (define (multiplicand operands)
    (cadr operands))
  (define (deriv-product operands var)
    (make-sum
     (make-product (multiplier operands)
                   (deriv (multiplicand operands) var))
     (make-product (multiplicand operands)
                   (deriv (multiplier operands) var))))
  ; inserting the function into the table
  (put 'deriv '* deriv-product)
  'done)

; (c)

; If we want to install new derivative rule, for example for exponents.
(define install-exponent-derivation
  (define (power operands)
    (cadr operands))
  (define (base operands)
    (car operands))
  (define (make-exponent b p)
    (list '** b p))
  (define (derive-exponent operands var)
    (make-product
      (make-product (power operands)
                    (make-exponent (base operands) (- (power operands) 1)))
      (deriv (base operands) var)))
  ; inserting the function into the table
  (put 'deriv '**  derive-exponent)
  'done)

; (d) as all we are doing is changing the order of <operation> and <type> in the "get"
; statement, we will just need to change the "put" statements to change the order as well
; so that the procedures are indexed correctly in the table.
