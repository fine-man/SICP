#lang racket

; Solution of exercise 2.30 of SICP

; Helper Procedures

(define (square x) (* x x))

(define (map proc tree)
  (if (null? tree)
      tree
      (cons (proc (car tree))
            (map proc (cdr tree)))))

; Square-tree without map
(define (square-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

; '(1 (4 (9 16) 25) (36 49))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree))

(square-tree-map
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

; '(1 (4 (9 16) 25) (36 49))