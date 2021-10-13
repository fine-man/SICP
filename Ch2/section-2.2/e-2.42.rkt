#lang racket

; solution to exercise 2.42 of SICP
; http://community.schemewiki.org/?sicp-ex-2.42

; Helper Procedures

;; ---------------------------------------------------------------------------------
; nil
(define nil '())

; Procedure to append one list to another
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
            (append (cdr l1) l2))))

; enumerating integers
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

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

; Flatmap Procedure
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;;------------------------------------------------------------------------------------

; Procedure that gives a list of all possible solutions to the
; queen problem of board size n
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

; Empty-board
(define empty-board nil)


(define (safe? k positions)
  (define row (car (car positions)))
  (define (iter pos)
    (cond ((null? pos) #t)
          ((= row (car (car pos))) #f)
          ((= (abs (- row (car (car pos))))
              (abs (- k (cadr (car pos)))))
           #f)
          (else (iter (cdr pos)))))
  (iter (cdr positions)))

;; TEST
(queens 5)
(newline)
; '(((4 5) (2 4) (5 3) (3 2) (1 1))
;  ((3 5) (5 4) (2 3) (4 2) (1 1))
;  ((5 5) (3 4) (1 3) (4 2) (2 1))
;  ((4 5) (1 4) (3 3) (5 2) (2 1))
;  ((5 5) (2 4) (4 3) (1 2) (3 1))
;  ((1 5) (4 4) (2 3) (5 2) (3 1))
;  ((2 5) (5 4) (3 3) (1 2) (4 1))
;  ((1 5) (3 4) (5 3) (2 2) (4 1))
;  ((3 5) (1 4) (4 3) (2 2) (5 1))
;  ((2 5) (4 4) (1 3) (3 2) (5 1)))

(queens 6)
; '(((5 6) (3 5) (1 4) (6 3) (4 2) (2 1))
;  ((4 6) (1 5) (5 4) (2 3) (6 2) (3 1))
;  ((3 6) (6 5) (2 4) (5 3) (1 2) (4 1))
;  ((2 6) (4 5) (6 4) (1 3) (3 2) (5 1)))
