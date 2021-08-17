#lang sicp
; Solution to exercise 1.31
; Question :
; a.  The sum procedure is only the simplest of a vast number of similar abstractions
;     that can be captured as higher-order procedures. Write an analogous procedure
;     called product that returns the product of the values of a function at points over
;     a given range. Show how to define factorial in terms of product. Also use product
;     to compute approximations to value of pi using the formula.
;
;     pi   2 * 4 * 4 * 6 * 6 * 8 ...
;     -- = -------------------------
;     4    3 * 3 * 5 * 5 * 7 * 7 ...
;
; b.  If your product procedure generates a recursive process, write one that generates
;     an iterative process. If it generates an iterative process, write one that generates
;     a recursive process.
;
;     ----------------------------------------------------------------------------------


; Linear Recursive Product Procedure
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))


; Linear Iterative Product Procedure
(define (iterative-product term a next b)
  (define (prod-iter a result)
    (if (> a b)
        result
        (prod-iter (next a)
                   (* result (term a)))))
  (prod-iter a 1))

(define (identity x) x)
(define (inc x) (+ x 1))

; Factorial Procedure
(define (factorial n)
  (product identity 1 inc n))

; Calculating pi using the wallis Product
; https://en.wikipedia.org/wiki/Wallis_product
(define (calc-pi n)
  (define (pi-term n)
    (if (even? n)
        (/ (+ n 2)
           (+ n 1))
        (/ (+ n 1)
           (+ n 2))))
  (define (pi-next x)
    (+ x 1))
  (* 4.0 (product pi-term 1 pi-next n)))


