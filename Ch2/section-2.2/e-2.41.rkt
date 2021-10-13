#lang racket

; Solution to exercise 2.41 of SICP
; http://community.schemewiki.org/?sicp-ex-2.41

; Helper Procedures
; -----------------------------------------------------------------
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

;;------------------------------------------------------------------------

; Procedure to make unique pairs (i, j) such that 1 <= j < i <= n
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))


; Procedure to triples of number (i, j, k) such that 1 <= j < i < k <= n
(define (unique-triples n)
  (flatmap (lambda (k)
             (map (lambda (pair) (cons k pair))
                  (unique-pairs (- k 1))))
           (enumerate-interval 1 n)))

; Procedure to filter a list to triplets such that each has a sum < S
(define (triples-of-sum sum n)
  ; Procedure to tell if the sum of a triplet (i, j, k) <= S
  (define (sum-equal? triplet)
    (= (accumulate + 0 triplet) sum))
  (filter sum-equal? (unique-triples n)))

;; TEST
(unique-triples 5)
; '((3 2 1) (4 2 1) (4 3 1) (4 3 2) (5 2 1) (5 3 1) (5 3 2) (5 4 1) (5 4 2) (5 4 3))

(triples-of-sum 8 5)
; '((4 3 1) (5 2 1))

(triples-of-sum 7 3)
; '()

(triples-of-sum 3 7)
; '()


