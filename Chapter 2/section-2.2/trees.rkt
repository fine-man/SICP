#lang racket

; Helper Procedure
(define (map proc tree)
  (if (null? tree)
      tree
      (cons (proc (car tree))
            (map proc (cdr tree)))))

(define (count-leaves x)
  (cond ((null? x) 0)
        ; pair? is a primitive procedure which tells if it's argument is a pair
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

; Procedure which returns the leaves of a tree as a list
(define (enumerate-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (scale-tree tree factor)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree-map tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-map sub-tree factor)
             (* sub-tree factor)))
       tree))


; Tree-map Procedure, it applies a procedure on the leaves of a tree.
(define (tree-map proc tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))


