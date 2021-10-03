#lang racket

; Solution to exercise 2.29 part D of SICP

; Procedure to make-mobile with new representation
(define (make-mobile left right)
  (cons left right))

; Procedure to make branch with new representation
(define (make-branch length structure)
  (cons length structure))

; Procedure to get left-branch of a mobile
(define (left-branch mobile)
  (car mobile))

; Procedure to get right branch of a mobile
(define (right-branch mobile)
  (cdr mobile))

; Procedure to get the branch length
(define (branch-length branch)
  (car branch))

; Procedure to get the branch structure
(define (branch-structure branch)
  (cdr branch))

; Procedure to check if a branch contains a mobile
(define (contain-mobile? branch)
  (pair? (branch-structure branch)))

(define branch1 (make-branch 2 3))

(define nokia (make-mobile branch1 branch1))

; Procedure to find the weight of a mobile
(define (total-weight mobile)
  (define (branch-weight branch)
    (cond ((not (contain-mobile? branch)) (branch-structure branch))
          (else (total-weight (branch-structure branch)))))
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

; Test
(total-weight nokia)
; 6

; Procedure to tell if a mobile is balanced
(define (is-balanced? mobile)
  (define (balanced-branch? branch)
    (if (not (contain-mobile? branch))
        (cons #t (branch-structure branch))
        (is-balanced? (branch-structure branch))))
  
  (define lft-branch (left-branch mobile))
  (define rgt-branch (right-branch mobile))
  (define left (balanced-branch? lft-branch))
  (define right (balanced-branch? rgt-branch))
  (define result (cons (and (car left)
                           (car right)
                           (= (* (branch-length lft-branch) (cdr left))
                              (* (branch-length rgt-branch) (cdr right))))
                      (+ (cdr left) (cdr right))))
  result)


(define (balanced-mobile? mobile)
  (car (is-balanced? mobile)))

; Test
(balanced-mobile? nokia)
; #t

; As there is a layer of abstraction between the representation and usage
; of data-objects "mobiles" and "branch" (namely the constructor and selector procedures)
; so we can use the same procedures we defined for our earlier
; representation and we will just have to change the selector functions we defined.
