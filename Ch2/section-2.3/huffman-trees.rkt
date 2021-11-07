#lang racket

; Section 2.3.4 of SICP : Huffman Encoding Trees
(provide (all-defined-out))

#|
This section introduces us Huffman encoding tree as practice
for data-abstraction to manipulate sets and trees. Huffman trees
are used for representing data as sequences of ones and zereos.

For example ASCII code uses 8-bits to represent each symbol.
If we want to distinguish between n symbols, then we need logn bits
an example to represent the set {A, B, C, D, E, F, G, H} using 3 bits is:
A 000    C 010    E 100    G 100
B 001    D 011    F 101    H 111

Codes like ASCII or the one above which use a fixed number of bits per
character are known as "fixed-length" codes

It is sometimes advantageous to use variable-length code (e.g Morse Code)
which uses different number of bits for each character/symbol. The advantage
in using variable-length is when some symbols in our representation are used
very often so we can addign them shorter codes and also because they save significant space
Example :
A 0    C 1010    E 1100    G 1110
B 100  D 1010    F 1101    H 1111

One of the problems of variable-length code is that it is hard to know when
you have reached the end of a symbol in reading a message of ones and zereos
Two of ways to solve this are:
1. breaking the message after each symbol (like morse code uses pause after each symbol)
2. Design codes in such a way that no code comes in the prefix of another one (prefix codes)

Huffman Encoding :
One particular method of implementing 2nd is the Huffman Encoding method.
A Huffman code can be represented as a binary-tree whose leaves are the
A Huffman code can be represented as a binary tree whose leaves are the symbols 
that are encoded. At each non-leaf node of the tree there is a set containing all 
the symbols in the leaves that lie below the node. In addition, each symbol at a leaf 
is assigned a weight (which is its relative frequency), and each non-leaf node contains a 
weight that is the sum of all the weights of the leaves lying below it.

Example : Fig 2.18 of SICp
|#

#|
Generating Huffman Trees:

The algorithm for generating a Huffman tree is very simple. 
The idea is to arrange the tree so that the symbols with the lowest freq
come farthest from the root. The way we achieve this is :

1. Begin with the set of leaf nodes containing symbols and their frequency
2. Find two leaves with the lowest weight(n1 and n2) and combine them to make a
node(n0) whose wieght is the sum of weight of n1 and n2
3. make n1 and n2 the left and right children of n0 respectively
4. replace n1 and n2 with n0
5. repeat (2-4) untill there is only one node left
|#

;; Representing Huffman Trees:
; Note : here we use unordered list representation for sets

; constructor to make leaves:
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

; Predicate to check for a leaf
(define (leaf? object)
  (eq? (car object) 'leaf))

; Selector for symbol part of a leaf
(define (symbol-leaf x)
  (cadr x))

; Selector for weight part of a leaf
(define (weight-leaf x)
  (caddr x))

; Constructor for making the Huffman tree
; Each tree consists of : left-branch, right-branch,
; set of symbols from left and right, total weight
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

; Selector for left-branch of a huffman-tree
(define (left-branch tree) (car tree))

; Selector for right-branch of a huffman-tree
(define (right-branch tree) (cadr tree))

; Selector for set of symbols of a tree
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

; Selector for weight of a tree
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; Note : Procedures like "symbol" and "weight" are called "generic procedures"
; because they handle more than one type of data, in this case both leaf and trees

#|
Decoding Messages

To decode a string of ones and zeroes(bits) encoded using Huffman Trees,
we use the following algorithm
1. start with a message consisting of ones and zeroes
2. start at the root of the Huffman Tree and if the leading bit of the message
is 1 then go the right-branch otherwise go to the left branch.
3. remove the first bit of the message
4. continue traversing the tree with the same rule that if leading bit is 1
then go to the right-branch otherwise the left-branch.
5. Do (4) until you reach a leaf node at which point copy the symbol present at
the leaf node, this will be the first letter of the message.
6. start again from (2) until there are no more bits left in the message

|#

; procedure to decode a message
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

; Procedure to choose a branch in decode procedure
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))


; Procedure to adjoin a tree/leaf to a set of trees/leaves 
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; Procedure to make a ordered set of leaves from a list of pairs
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)   ; symbol
                               (cadr pair)) ; Frequency
                    (make-leaf-set (cdr pairs))))))

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

; Procedure to encode a symbol given a huffman tree 
(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((element-of-set symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "encode-symbol:symbol not found" symbol))))

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

;; Test
; (generate-huffman-tree '((A 4) (B 2) (D 1) (C 1)))
#|result
'((leaf A 4)
((leaf B 2) ((leaf C 1) (leaf D 1) (C D) 2) (B C D) 4)
(A B C D)
8)
|#

;; For more examples check exercise 2.70
