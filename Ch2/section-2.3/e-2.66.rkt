#lang racket

; solution of exercise 2.66 of SICP
; http://community.schemewiki.org/?sicp-ex-2.66

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

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) "record not found")
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))
        (else (lookup given-key (left-branch set-of-records)))))
