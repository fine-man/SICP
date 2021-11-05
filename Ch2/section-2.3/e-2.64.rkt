#lang racket

; solution to exercise 2.64 of SICP
; http://community.schemewiki.org/?sicp-ex-2.64

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

#|
partial-tree : This procedure takes as argument an integer n and an ordered list 
with atleast n elements and contructs a balanced binary tree containing the
the first n elements of the list. The function returns a pair(made with cons)
whose cars is the balanced tree and whose cdr is the list of remaining elements

Working:
1. If the size of the balanced tree to be contruced is 0(n = 0), return the empty tree
with remaining elements.
2. Otherwise make a variable left-size which is (n - 1)/2
3. contruct left-result which returns a balanced tree of size left-size and also
returns the remaining elements
4. name the returned balanced tree in (3) as "left-tree" and remaining elements as
"non-left-elts"
5. Take the first element of "non-left-elts" and call it "this-entry" which
will be the entry node element of our tree
6. with the cdr of "non-left-elts" and size of the remaining list(cdr non-left-elts)
construct another balanced tree and call the pair of tree and remaining elements
as right-result
7. Take the car of right-result and call it the right-tree.
8. return a tree made from "left-tree" "this-entry" and "right-tree" along with
the remaining elements as a pair (cons tree remaining-elts)
|#

(list->tree '(1 3 5 7 9 11))
; '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

#|
            5
           / \
          /   \
         1     9
          \   / \
           3 7   11
|#

#|
Time complexity :
Let T(n) be the time list->tree takes to turn a list of n elements to a balanced tree, then:
T(n) = 2 * T(n/2)[from making left-tree and right-tree] + O(n)[all the car/cdr operations]
T(n) = 2 * T(n/2) + O(1)
Solving the reccurrence relation above we get:
T(n) = O(n)
|#

