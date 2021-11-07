#lang racket

; solution to exercise 2.68 of SICP
; http://community.schemewiki.org/?sicp-ex-2.68

; for implementation of huffman-trees
(require "huffman-trees.rkt")

; Procedure to encode a message given a Huffman tree
(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

; element of set procedure
(define (element-of-set x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set x (cdr set)))))

; Encode-symbol procedure
(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((element-of-set symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "encode-symbol:symbol not found" symbol))))

; sample tree
(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

; sample message
(define sample-message '(A D A B B C A))

(encode sample-message sample-tree)
; '(0 1 1 0 0 1 0 1 0 1 1 1 0)

(decode '(0 1 1 0 0 1 0 1 0 1 1 1 0) sample-tree)
; '(A D A B B C A)

