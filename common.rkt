#lang racket

; Library of common procedures used in exercises which do not particularly participate
; in solution design but are used only as helpers and can be shared among.

(define true #t)
(define false #f)

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (div a b)
  (floor (/ a b)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (identity x) x)

(define close-enough?
  (lambda (a b delta) (< (abs (- a b)) delta)))

; average of 2 values
(define (average x y)
  (/ (+ x y) 2.0))

; avergae of 3.0
(define (average-of-3 x y z)
  (/ (+ x y z) 3.0))

(define (inc x)
  (+ x 1))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; define nil as empty list.
(define nil '())

; Procedure to append one list to another
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
            (append (cdr l1)
                    l2))))

;;; Standard Sequence operations

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

; Accumulate-n procedure that takes a list of lists each containing n elements and applying
; a certain procedure to 1st element of all the lists, 2nd element and so on and then
; accumulating the result
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs)) ; get cars of every list inside
            (accumulate-n op init (map cdr seqs))))) ; proceed with cadrs in next recursion

; Folding to left and right are standard operations
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; folding right is the standard accumulation
(define fold-right accumulate)

; Flatmap procedure :
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))