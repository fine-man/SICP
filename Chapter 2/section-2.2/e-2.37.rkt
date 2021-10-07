#lang racket

; Solution to exercise 2.37 of SICP
; http://community.schemewiki.org/?sicp-ex-2.37

; Helper Procedures

;; ---------------------------------------------------------------------------------
(define nil '())

; Accumulating elements of a list
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Accumulate-n Procedure
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))
;; -------------------------------------------------------------------------------

; Dot Product of two vectors
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; Multiplication of a matrix and a vector
(define (matrix-*-vector mat v)
  (map (lambda (mat-row) (dot-product mat-row v))
       mat))

; Transpose of a matrix
(define (transpose mat)
  (accumulate-n cons nil mat))

; Matrix multiplication of two matrices
(define (matrix-*-matrix m n)
  (let ((n-cols (transpose n)))
    (map (lambda (m-row) (matrix-*-vector n-cols m-row))
         m)))

;; TEST
(define matrix (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))

(dot-product (list 1 2 3) (list 4 5 6))
; 32

(matrix-*-vector matrix (list 2 3 4 5))
; '(40 96 152)

(transpose matrix)
; '((1 5 9) (2 6 10) (3 7 11) (4 8 12))

(matrix-*-matrix matrix (list (list 1 2) (list 1 2) (list 1 2) (list 1 2)))
; '((10 20) (26 52) (42 84))


