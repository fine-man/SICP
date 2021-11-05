#lang racket

; section 2.3.3 : Representing Sets

#|
In the previous examples we built representation for compound 
data objects like rational numbers and algebraic expressions
Now the representation for these structures in terms of list
was pretty striaght forward but when we try to represent sets,
the choice of a representation is not so obvious and there are
many possible representation and they differ significantly from one another.

informally a set is just a collection of dintinct objects.
We can have a more precise definition using data abstraction i.e
we define the 'set' by specifying the operations that will be used
on sets and then defining what rules those procedures must follow
These procedures are : 'union-set', 'intersection-set', 'element-of-set?',
and 'adjoin-set'.
The rules that these procedures must follow are :
- For any object S and any object x, (element-of-set? x (adjoin-set x S)) is True
- For any set S and T and any object x, (element-of-set? x (union-set S T)) is
equal to (or (element-of-set? x S) (element-of-set? x T))
- For any object x, (element-of-set? x '()) is False

|#

;; Helper procedure 
; equal Procedure from exercise 2.54
(define (equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        ((and (pair? l1) (pair? l2))
         (and (equal? (car l1) (car l2))
              (equal? (cdr l1) (cdr l2))))
        (else (eq? l1 l2))))

#|
; Sets as Unordered lists
; one way to represent a set is a list of it's elements in which
; no element appears more than once. The empty list is just '().
|#

; element-of-set procedure - O(n)
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; adjoin-set procedure - O(n)
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

; intersection-set procedure - O(n^2)
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; union-set procedure - O(n ^ 2)
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))









