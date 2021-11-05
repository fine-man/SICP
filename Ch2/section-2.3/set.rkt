#lang racket

; section 2.3.3 : Representing Sets

#|
In the previous examples we built representation for compound 
data objects like rational numbers and algebraic expressions
Now the representation for these structures in terms of list
was pretty striaght forward but when we try to represent sets,
the choice of a representation is not so obvious and there are
many possible representation and they differ significantly from one another.

informally a set is just a collection of dintinct objects.
We can have a more precise definition using data abstraction i.e
we define the 'set' by specifying the operations that will be used
on sets and then defining what rules those procedures must follow
These procedures are : 'union-set', 'intersection-set', 'element-of-set?',
and 'adjoin-set'.
The rules that these procedures must follow are :
- For any object S and any object x, (element-of-set? x (adjoin-set x S)) is True
- For any set S and T and any object x, (element-of-set? x (union-set S T)) is
equal to (or (element-of-set? x S) (element-of-set? x T))
- For any object x, (element-of-set? x '()) is False

|#

;; Helper procedure 
; equal Procedure from exercise 2.54
(define (equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        ((and (pair? l1) (pair? l2))
         (and (equal? (car l1) (car l2))
              (equal? (cdr l1) (cdr l2))))
        (else (eq? l1 l2))))

#|
; Sets as Unordered lists
; one way to represent a set is a list of it's elements in which
; no element appears more than once. The empty list is just '().

; element-of-set procedure - O(n)
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; adjoin-set procedure - O(n)
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

; intersection-set procedure - O(n^2)
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; union-set procedure - O(n ^ 2)
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))
|#

#| 
Sets as ordered lists

In this sub-section we try to represent sets as ordered lists.

Ordering the objects in the list:
To order the elements of the set, we need some way to compare
two data objects so we can say which is bigger. We can either
do this lexigraphically or we can have a method of assigning 
a unique number to each element and then comparing those numbers

To keep the implementation simple we only use numbers and elements here
and we will be storing the elements in increasing order

; element-of-set? procedure
; worst case : O(n)
; on average : n/2 steps
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

; intersection-set procedure - O(n + m)
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((> x1 x2)
               (intersection-set set1 (cdr set2)))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))))))

; adjoin-set procedure
; worst case (biggest element is x) : O(n)
; average case : n/2 steps
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; union-set procedure
; worst case(all elements are unique) : O(n + m)
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1
                             (union-set (cdr set1) (cdr set2))))
                      ((> x1 x2)
                       (cons x2
                             (union-set set1 (cdr set2))))
                      ((< x1 x2)
                       (cons x1
                             (union-set (cdr set1) set2))))))))

;; Test
; (define evens '(0 2 4 6))
; (define odds '(1 3 5))

; (element-of-set? 2 evens)
; #t

; (adjoin-set 7 odds)
; '(1 3 5 7)

; (intersection-set '(1 2 3 4) evens)
; '(2 4)

; (union-set evens odds)
; '(0 1 2 3 4 5 6)

|#

#|
Sets as Binary Tree

We can represent a set as a binary tree by arranging
the elements of the set in the form of a tree.
Each node will of the tree will have one element of the set,called 
The "entry" at that node, and a link to two other(possibly empty) nodes.
The "left" link points to elements smaller than the one at the node,and
The "right" link points to elements bigger than the one at the node.

example - The set {1, 3, 5, 7, 9, 11} can be represented as:
            7                  5
           / \                / \
          3   9              3   9
         / \   \            /   / \
        1   5   11         1   7   11

The benefit of this representation is that if the tree is "balanced"
then searching for an element in the set will only take logn steps
and O(logn) time complexity.
|#

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

; Note : For implementation of union-set and intersection-set for
; balanced binary trees, check solution to exercises 2.63 - 2.65


