#lang racket

; solution of exercise 2.65 of SICP

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

; ordered-list-union procedure to combine 2 ordered lists
; worst case(all elements are unique) : O(n + m)
(define (ordered-list-union set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1
                             (ordered-list-union (cdr set1) (cdr set2))))
                      ((> x1 x2)
                       (cons x2
                             (ordered-list-union set1 (cdr set2))))
                      ((< x1 x2)
                       (cons x1
                             (ordered-list-union (cdr set1) set2))))))))

; intersection-set procedure - O(n + m)
; To find the intersection of two ordered lists
(define (ordered-list-intersection set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (ordered-list-intersection (cdr set1) (cdr set2))))
              ((> x1 x2)
               (ordered-list-intersection set1 (cdr set2)))
              ((< x1 x2)
               (ordered-list-intersection (cdr set1) set2))))))

; Procedure to convert a binary tree to a list : O(n)
(define (tree->list tree)
  (define (copy-to-list tree result-tree)
    (if (null? tree)
        result-tree
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-tree)))))
  (copy-to-list tree '()))

; Procedure to convert a list into a balanced binary tree : O(n)
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

; Procedure that takes as argument an integer n and an
; ordered list of atleast n elements and constructs a balanced
; tree containing the first n elements of the list.
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

; Procedure to take two binary tree and find their union: O(n)
(define (union-set set1 set2)
  (let ((l1 (tree->list set1))
        (l2 (tree->list set2)))
    (let ((union (ordered-list-union l1 l2)))
      (list->tree union))))

; Procedure to take two binary trees and find their intersection: O(n)
(define (intersection-set set1 set2)
  (let ((l1 (tree->list set1))
        (l2 (tree->list set2)))
    (let ((intersection (ordered-list-intersection l1 l2)))
      (list->tree intersection))))

(union-set (list->tree '(1 2 3 5 7 9 46))
           (list->tree '(5 6 10 11 20 23 46))) 
; '(7 (3 (1 () (2 () ())) (5 () (6 () ()))) (11 (9 () (10 () ())) (23 (20 () ()) (46 () ()))))

(intersection-set (list->tree '(3 5 10)) 
                   (list->tree '(1 2 3 4 5 7))) 
; '(3 () (5 () ()))



