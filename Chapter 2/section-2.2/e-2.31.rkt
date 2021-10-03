#lang racket

; Solution of exercise 2.31 of SICP

; Helper Procedures
(define (square x) (* x x))

; Tree-map Procedure

(define (tree-map proc tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))

(define (square-tree tree)
  (tree-map square tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))


; '(1 (4 (9 16) 25) (36 49))

(square-tree (list 1 2 3 4))
; '(1 4 9 16)
