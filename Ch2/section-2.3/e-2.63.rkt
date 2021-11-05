#lang racket

; solution to exercise 2.63 of SICP
; http://community.schemewiki.org/?sicp-ex-2.63

; Binary Tree Representation :

; Selector for entry element of node:
(define (entry tree) (car tree))

; selector for left-branch of a noce:
(define (left-branch tree) (cadr tree))

; selector for right-branch of a node:
(define (right-branch tree) (caddr tree))

; constructor for a binary tree:
(define (make-tree entry left right)
  (list entry left right))

; element-of-set? procedure
; worst case(linear binary tree) : O(n) 
; Balanced binary tree : O(logn)
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

; adjoin-set procedure
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


; Procedure to convert a binary tree to a list : O(n * logn)
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))


; Procedure to convert a binary tree to a list : O(n)
(define (tree->list-2 tree)
  (define (copy-to-list tree result-tree)
    (if (null? tree)
        result-tree
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-tree)))))
  (copy-to-list tree '()))

; (a) part
(define fig2-16-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))) 
(define fig2-16-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))) 
(define fig2-16-3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))) 

(tree->list-1 fig2-16-1)
; '(1 3 5 7 9 11)

(tree->list-1 fig2-16-1)
; '(1 3 5 7 9 11)

(tree->list-1 fig2-16-2)
; '(1 3 5 7 9 11)

(tree->list-2 fig2-16-2)
; '(1 3 5 7 9 11)

(tree->list-1 fig2-16-3)
; '(1 3 5 7 9 11)

(tree->list-2 fig2-16-3)
; '(1 3 5 7 9 11)

; As we can see from above both procedures produce the same
; result for the figures in 2.16. Both the procedures produce
; the same result because they both perform in-order traversal
; and so they traverse the tree in the same order


#| (b) Part
Let T(n) be the time taken by the procedure for a balanced
tree with n nodes.

For tree->list-1 :
T(n) = 2 * T(n/2) + O(n/2) (as the procedure append takes linear time)
Solving the above equation we get T(n) = O(n * logn)

For tree->list-2 :
T(n) = 2 * T(n/2) + O(1) (becuase we are adding "entry element" to the result)
Solving the above equation will give us T(n) = O(n)

So tree->list-2 is faster than tree->list-1 as it is linear in time instead of logrithmic
|#

