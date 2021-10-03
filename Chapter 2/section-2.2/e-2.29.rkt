#lang racket

; Solution to exercise 2.29 of SICP

; Constructor for making a mobile
(define (make-mobile left right)
  (list left right))

; Constructor for making a branch
(define (make-branch length structure)
  (list length structure))

; Selector to extract left branch of a mobile
(define (left-branch mobile)
  (car mobile))

; Selector to extract right branch of a mobile
(define (right-branch mobile)
  (car (cdr mobile)))

; Procedure to get the branch-length
(define (branch-length branch)
  (car branch))

; Procedure to get the branch structure
(define (branch-structure branch)
  (car (cdr branch)))

; Procedure to tell if if a branch contains a mobile as it's structure
(define (contain-mobile? branch)
  (pair? (branch-structure branch)))

; Procedure to find the total weight of a mobile
(define (total-weight mobile)
  ;(display "mobile: ")
  ;(display mobile)
  ;(newline)
  (define (branch-weight branch)
    ;(display "branch: ")
    ;(display branch)
    ;(newline)
    (cond ((not (contain-mobile? branch)) (branch-structure branch))
          (else (total-weight (branch-structure branch)))))
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define nokia '((1 3) (2 4)))
(total-weight nokia)
; 7

(define mobile1 '((3 6) (4 7)))
(define mobile2 '((8 10) (9 11)))
(define branch1 (make-branch 1 mobile1))
(define branch2 (make-branch 2 mobile2))

(define Apple (make-mobile branch1 branch2))
(display Apple)
(newline)
; ((1 ((3 6) (4 7))) (2 ((8 10) (9 11))))
(total-weight Apple)
; 34

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


(define branch3 (make-branch 1 Apple))
(define samsung (make-mobile branch3 branch3))
(balanced-mobile? samsung)
; #f

(define branch4 (make-branch 4 '((2 3) (3 2))))
(define branch5 (make-branch 5 '((1 3) (3 1))))
(define pine-phone (make-mobile branch4 branch5))

(balanced-mobile? pine-phone)
; #t, pine-phone is balanced : )

(define xiomi (make-mobile branch3 branch4))
(balanced-mobile? xiomi)
; #f



                      
