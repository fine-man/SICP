#lang racket

; We can implement lists in lisp using 'cons' which is used to
; to make pairs of things. Now this is only possible because 'cons' can
; not only make pairs of numbers but it can also make pairs of already
; existing pairs or it can also make pairs of a number and an existing pair.
;
; In Math lingo we say that the set of data objects in Lisp is closed under
; The operation of forming pairs (in this case 'cons').
; (define A (cons 1 2))
; (define B (cons 3 4))
; Then we can combine A and B using 'cons' and it will still form a valid pair
; (define C (cons A B))
;
; Now, this is not always the case that the set of data-objects in a language,
; are always closed under an operation. For example : making Arrays is not
; a closed operation in Fortran, we can make arrays of numbers and strings
; but we cannot make arrays of arrays itself.
;
; Some examples of lists using 'cons'

(define A (cons (cons 1 2) (cons 3 4)))

(define B (cons 1 (cons 2 (cons 3 4))))

; Now, as we can see from the above that there are multiple ways to represent
; a list of numbers in lisp, so because of this Lisp has a particular
; convention to represent sequence of things as a chain of pairs.
; The below is the box and arrow representation for the list (1, 2, 3, 4)
;
;   |1| |----> |2| |----> |3| |----> |4|/|
;
; The end character '/' is just to represent the end of list.
;
; The above can be written in Lisp in the following way

(define nil '())
(define l (cons 1
                (cons 2
                      (cons 3
                            (cons 4 nil)))))

; nil is nothing but the empty list and it is used to indicate the end of a list
; (define nil '())
;
; We can access the elements of A using the same procedures 'car' and 'cdr'
; that we used to access numbers from a primitive pair.
;
(car l)
; 1
(car (cdr l))
; 2
(car (cdr (cdr l)))
; 3
(car (cdr (cdr (cdr l))))
; 4
(cdr (cdr (cdr (cdr l))))
; '()

; Now, we can see that it is combersome to write 'cons' so many times when making a list
; for this lisp has an operation called 'list' which just acts as a systactic sugar for
; the above

(define L (list 1 2 3 4))

; Procedures on Lists

; Procedure to scale ever element of the list by a number 'k';
(define (scale-list k items)
  (if (null? items)
      items
      (cons (* k (car items))
            (scale-list k (cdr items)))))

(scale-list 10 L)
; '(10 20 30 40)

; Now, what we can do is define a general procedure that takes a list and applies
; an operation to each element of the list as that way we are not just limited to
; multiplication but can do any operation of our choosing

(define (map p l)
  (if (null? l)
      l
      (cons (p (car l))
            (map p (cdr l)))))

; Then we can redefine scale-list using the general procedure 'map'
; (define (scale-list k items)
;   (map (lambda (x) (* k x)) items))

; To-do - Think of an Iterative implementation of map

; we saw that 'map' returns to us a new list containing the modified elements but what we
; also do is define another procedure that does operations on the list elements and
; does not make/return to us a list.

(define (for-each proc lis)
  (cond ((null? lis) "done")
        (else (proc (car lis))
              (for-each proc
                        (cdr lis)))))

(for-each display L)
; 1234"done"