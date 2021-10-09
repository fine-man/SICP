#lang racket

; Solution to exercise 2.38 of Sicp
; http://community.schemewiki.org/?sicp-ex-2.38

(define nil '())

; Accumulating procedure redefined as fold-right
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

; Fold-left procedure
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
; 3/2

(fold-left / 1 (list 1 2 3))
; 1/6

(fold-right list nil (list 1 2 3))
; (list 1 (list 2 (list 3 nil)))
; '(1 (2 (3 ())))

(fold-left list nil (list 1 2 3))
; (list (list (list nil 1) 2) 3)
; '(((() 1) 2) 3)

; For simplicity in writing, we will be using the infix notation when expanding expressions,
; along with the following conventions :
; op = '.', initial = I, elements on seq = (A B C ....)
;
; There are two properties that are required by the procedure op,
; for (fold-left) and (fold-right) to be equal. These are :
; 1. (A . I) = (I . A) - (commutative with I)
; 2. (A . B) . C = A . (B . C) - (associativity)
; Note : strict commutativity is not neccesary, just having 1st condition is sufficient
;
; Proof for Commutativity with I :
; 
; let's calculate (fold-right) and (fold-right) with the singleton sequence '(A)
;   (fold-right . I (A))
; = (A . I) - (1)
;
;   (fold-left . I (A))
; = (I . A) - (2)
;
; For (fold-left) and (fold-right) to be equal (1) and (2) must be true so:
; (A . I) = (I . A)
;
;
; Proof for Associativity :
;
; let's calculate (fold-right) and (fold-left) with the sequence '(A B)
;  (fold-right op I '(A B))
;= (op A (fold-right op I '(B)))
;= (op A (op B I))
; In Infix notation :
;= (A . (B . I))
; as (B . I) = (I . B) (from 1st property)
;= (A . (I . B)) - (3)
;
;  (fold-left op I '(A B))
;= (iter (op I A) '(B))
;= (op (op I A) B)
; In Infix notation :
;= ((I . A) . B)
; as (I . A) = (A . I) (from 1st property)
;= ((A . I) . B) - (4)
;
; For statement (3) and (4) to be equal the operation op must be associative. qed


