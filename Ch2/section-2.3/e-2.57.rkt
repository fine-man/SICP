#lang racket

; solution to exercise 2.57 of SICP
; http://community.schemewiki.org/?sicp-ex-2.57

; All the required procedures to differentiaion
(require "sym-differentiation.rkt")

; for helping list functions like accumulate
(require "../../common.rkt")

; Procedure to return true if the expression is not 0
(define (not-zero x)
  (not (=number? x 0)))

; Procedure which returns true if the expression is not 1
(define (not-one x)
  (not (=number? x 1)))

; Procedure which checks if there is 0 present in a list
(define (contain-zero? x)
  (cond ((null? x) #f)
        ((=number? (car x) 0) #t)
        (else (contain-zero? (cdr x)))))

; make-sum which handles multiple arguments
(define (make-sum a1 . a2)
  ; filtering all the non-number elements
  (define l1 (filter (lambda (x) (not (number? x))) (cons a1 a2)))
  ; summing all the elements which are numbers
  (define num (accumulate + 0 (filter number? (cons a1 a2))))
  ; combining num and l1
  (define l (if (= num 0) l1 (cons num l1)))
          ; if there are no non-zero elements, return 0
  (cond ((null? l) 0) 
        ; if the length of list is 1, return singleton element
        ((= (length l) 1) (car l)) 
        ; else append '+' symbol to the list
        (else (cons '+ l))))

; Procedure to return the 2nd element of sum
(define (addend x) (cadr x))

; Proceudre to return rest of the elements of sum
(define (augend x)
  (let ((p (cddr x)))
    (if (= (length p) 1)
        (car p)
        (cons '+ p))))

; make-product which handles multiple arguments
(define (make-product m1 . m2)
        ; making a list of all elements != 1
  (let ((p (filter not-one (cons m1 m2))))
          ; if the list is empty, return 1
    (cond ((null? p) 1)
          ; if the list contains any element = 0, return 0
          ((contain-zero? p) 0)
          ; if the length is 1, return single element
          ((= (length p) 1) (car p))
          ; else add '*' symbol to the list
          (else (cons '* p)))))

; Procedure which returns the 2nd element of a product
(define (multiplier x) (cadr x))

; Procedure which returns the rest of the elements of a product
(define (multiplicand x)
  (let ((p (cddr x)))
    (if (= (length p) 1)
        (car p)
        (cons '* p))))

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

;; Test

;(define a '(* x y (+ x 3)))
;(deriv a 'x)
;'(+ (* x y) (* y (+ x 3)))


