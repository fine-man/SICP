#lang racket

; solution to exercise 2.69 of SICP

; For implementation of Huffman Trees
(require "huffman-trees.rkt")

; Procedure to make a Huffman-Tree from
; A list of pairs of symbols and their frequency
(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

; Procedure to merge the set of tree to make Huffman Trees
(define (successive-merge set)
  (cond ((null? set) '())
        ((= (length set) 1) (car set)) ; return the tree
        (else (let ((t1 (car set))   ; 1st lightest tree
                    (t2 (cadr set))) ; 2nd lightest tree
                (successive-merge (adjoin-set
                                   ; combining 1st and 2nd element
                                   (make-code-tree t1 t2)
                                   (cddr set)))))))


(generate-huffman-tree '((A 4) (B 2) (D 1) (C 1)))
#|
'((leaf A 4)
  ((leaf B 2) ((leaf C 1) (leaf D 1) (C D) 2) (B C D) 4)
  (A B C D)
  8)
|#
