#lang racket

; Solution to exercise 2.72 of SICP

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

; element of set procedure - O(len(set))
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

#|

The order of growth for encoding the most frequent letter is O(1) because
in (encode-symbol most-freq-symbol huffman-tree) we will chech if the 
most-freq-symbol comes in the left branch which it does and as 
(length (symbols (left-branch huffman-tree))) = 1, we only need to do one comparision

The order of growth for encoding the least frequent letter is O(n^2) because
in (encode-symbol least-freq-symbol huffman-tree), we will need to check if the symbol
comes in each branch which and since we have O(n) branches and around O(n) elements in each
branch we will get the complexity as O(n^2)

#|
