#lang racket

; Helper Procedure

; Map procedure as used by lists
(define (map proc tree)
  (if (null? tree)
      tree
      (cons (proc (car tree))
            (map proc (cdr tree)))))

; square procedure
(define (square x) (* x x))

; Just as we can represent sequences of numbers, similarly we can represent
; sequences of sequences. For example :
; (define tree1 (cons (list 1 2) (list 3 4)))
; tree1
; ((1 2) 3 4)
; if we represent this list a little differently, we can see it's heirarchical nature
#|
         ((1 2) 3 4)
            /    |\
           /     | \
    (1 2) /|     |  \
         / |     |   \
        1  2     3    4
|#
; The above representatio looks kind of like a tree, in fact this kind of
; heirarchical data representation is what is used to represent a
; "tree" data structure. The nodes which have no further children/expansion
; are called leaf nodes

; procedure to tell if a leaf has been found 
(define (leaf? tree)
  (not (pair? tree)))
;  pair? is a primitive procedure which tells if it's argument is a pair
; it basically gives false when the argument is null or a number and true otherwise

; General method of iterating/navigating trees
(define (iter-tree proc tree)
  (cond ((null? tree) tree) ; if tree is null/empty, return tree
        ((leaf? tree) (proc tree)) ; if tree is leaf, apply procedure on leaf
        (else (iter-tree proc (car tree)) ; otherwise, iterate over left sub-tree
              (iter-tree proc (cdr tree))))) ; iterate over right sub-tree

; Procedure the count the leaves in a tree
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

; Procedure that scales each leave of the tree by a factor 
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
  (cond ((null? tree) tree) ; if tree is null then return null
        ((not (pair? tree)) (proc tree)) ; if leaf? then apply procedure
        (else (cons (tree-map proc (car tree)) ; apply map to the left sub-tree
                    (tree-map proc (cdr tree)))))) ; apply map to right sub-tree


;; TEST
; (define tree2 (list (list 1 2) (list 3 4)))

; (enumerate-tree tree2)
; '(1 2 3 4)

; (count-leaves tree2)
; 4

; (scale-tree tree2 10)
; '((10 20) (30 40))

; (tree-map square tree2)
; '((1 4) (9 16))
