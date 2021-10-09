#lang sicp

; Solution to exercise 1.39

; Question
; A continued fraction representation of the tangent function was published
; in 1770 by the German mathematician J.H. Lambert:
;
;                 x
; tan x =  ---------------------
;                       x^2
;           1 -  ---------------
;                         x^3
;                 3 - ----------
;                       5 - ....
;
; where x is in radians.
;
; Define a procedure (tan-cf x k) that computes an approximation to the tangent
; function based on Lambert's formula. K specifies the number of terms to compute,
; as in exercise 1.37.
;

; Helper Procedures

(define (odd x)
  (- (* 2 x) 1.0))

(define (square x) (* x x))

; Linear Iterative continued fraction procedure
(define (cont-frac n d k)
  (define (iter i result)
    (cond ((= i 0) result)
          (else
           (iter (- i 1) (/ (n i) (+ (d i) result))))))
  (iter (- k 1) (/ (n k) (d k))))

; Procedure to calculate value of tan(x)
(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- (square x))))
             odd
             k))
