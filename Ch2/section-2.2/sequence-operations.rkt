#lang racket

; Section 2.2.3 of SICP : Sequence operations
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
; squares of the leaves are odd :
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

(list-fib-squares 10)
; '(0 1 1 4 9 25 64 169 441 1156 3025)

; Product of squares of odd elements of a sequence
(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5))
; 225

;-------------------------------------------------------------------------------








