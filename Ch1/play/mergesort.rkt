#lang racket

(define nil '())

; Procedure to extract elements from a list with a
; particular index range
(define (extract from to items)
  (define (iter idx items)
    (cond ((> idx to) nil)
          ((and (>= idx from) (<= idx to))
           (cons (car items)
                 (iter (+ idx 1) (cdr items))))
          (else
           (iter (+ idx 1) (cdr items)))))
  (iter 0 items))

(define (halve n)
  (if (even? n)
      (/ n 2)
      (/ (- n 1) 2)))

; Procedure to calculate the length of a list
(define (length lis)
  (if (null? lis)
      0
      (+ 1 (length (cdr lis)))))

; Procedure to merge two lists for mergesort
(define (merge l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((< (car l1) (car l2))
         (cons (car l1) (merge (cdr l1) l2)))
        (else (cons (car l2) (merge l1 (cdr l2))))))

; Procedure for mergesort
(define (mergesort lis)
  (define len (- (length lis) 1))
  (if (<= len 0)
      lis
      (merge (mergesort (extract 0 (halve len) lis))
             (mergesort (extract (+ 1 (halve len)) len lis)))))



; (define l (list 3 2 4 1))
; (define lst (list 1 2 3 4 5))

; (mergesort l)
; '(1 2 3 4)

; (mergesort lst)
; '(1 2 3 4 5)

; (mergesort (list 3 1 8 2 7 9))
; '(1 2 3 7 8 9)
