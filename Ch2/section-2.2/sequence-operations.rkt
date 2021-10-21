#lang racket

; Section 2.2.3 of SICP : Sequences as conventional interfaces
;
#|
; Consider example of squaring only odd leaves of the tree
(define (sum-odd-squares tree)
  (cond ((null? tree) 0) ; if we got to the end of the tree then 0
        ((not (pair? tree)) ; if we cam to the leaf of the tree than do something
         (if (odd? tree) ; if leaf is odd square it otherwise skip (by returning 0)
           (square tree) 0))
        (else (+ (sum-odd-squares (car tree)) ; preceed with new level
                 (sum-odd-squares (cdr tree))))))

|#
; (output (sum-odd-squares (list 1 2 3 4 5)))


#| and the second example of producing the list of even fibonacci
; sequences
(define (even-fib n)
  (define (next k)
    (if (> k n)
      nil
      (let ((f (fib k)))
        (if (even? f)
          (cons f (next (+ k 1)))
          (next (+ k 1))))))
  (next 0))
|#
; (output (even-fib 10))


; These two procedures are structuraly very different, but they share
; some commonality in the way the processes behind can be thought of.
;
; For the first one we do:
;  - enumerate the leave trees
;  - filter only the odd ones
;  - square them
;  - make sum of all of them
;
; For second:
;  - enumerate all integers from 0 to n
;  - compute Fibonacci number of every one
;  - filter out even ones
;  - accumulate all of them in one list

; This can be easily seen as a flow of data through the system blocks
; where data is processed in every block and passed to next block for
; processing.
; 1. enumeration (producing a signal)
; 2. filtering only interesting part of the signal
; 3. mapping a procedure to the filtered signal
; 4. accumulation for end result

; If we represent these signals as lists, then we can apply known list
; operations to manipulate them

; This section uses abstraction to make procedures that :
; 1. Enumerate a tree/list and return a list of all the leaves/elements
; 2. Filter the elements of list of leaves/elements and returns a new list
; 3. Maps each element of the list to another element using a procedure 'proc'
; 4. Accumulate the elements of the list using a procedure 'op' and returns the result

; Signal Flow diagram for tree :
; Task : Make a procedure that takes tree as an argument and computes the sum of the
; squares of the leaves are odd :
; | Enumerate : |        |Filter :|        |Map :  |        | Accumulate :|
; | tree-leaves | -----> |odd?    | -----> |square | -----> | +, 0        |
; |             |        |        |        |       |        |             |

; Signal Flow diagram for list :
; Task : Make a procedure that returns a list of all the even Fibonacci numbers :
; | Enumerate : |        |Map :   |        |Filter:|        | Accumulate :|
; | integers    | -----> |fib     | -----> |even?  | -----> | cons,()     |
; |             |        |        |        |       |        |             |


; Helper Procedures
;-------------------------------------------------------------------------
; nil
(define nil '())

; Procedure to append one list to another
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
            (append (cdr l1)
                    l2))))

; Recursive Procedure for linear iterative process, calculating Fibonacci.
(define (fib n)
  (define (fib-iter a b counter)
    (if (= counter n)
        a
        (fib-iter b
                  (+ a b)
                  (+ counter 1))))
  (fib-iter 0 1 0))

(define (square x) (* x x))

;-----------------------------------------------------------------------------

; enumerating integers
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

; enumerating tree
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

; Procedure to map the elements of a list
(define (map proc lis)
  (if (null? lis)
      lis
      (cons (proc (car lis))
            (map proc (cdr lis)))))

; Filtering a list
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

; Accumulating elements of a list
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; procedure that takes tree as an argument and computes the sum of the
; squares of the leaves that are odd :
(define (sum-odd-squares tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

; procedure that returns a list of all the even Fibonacci numbers
(define (even-fibs n)
  (accumulate
   cons
   nil
   (filter even? (map fib (enumerate-interval 0 n)))))
;-------------------------------------------------------------------------------

; Additional examples

; List of squares of the first n + 1 fibonacci numbers :
(define (list-fib-squares n)
  (accumulate
   cons
   nil
   (map square (map fib (enumerate-interval 0 n)))))

; (list-fib-squares 10)
; '(0 1 1 4 9 25 64 169 441 1156 3025)

; Product of squares of odd elements of a sequence
(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

; (product-of-squares-of-odd-elements (list 1 2 3 4 5))
; 225

;-------------------------------------------------------------------------------








