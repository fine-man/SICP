#lang sicp
; Solution to exercise 1.29
; Question :
; Exercise 1.29.  
;
; Simpson's Rule is a more accurate method of numerical integration than the method
; illustrated above. Using Simpson's Rule, the integral of a function f between a and b is approximated as
;
; S|a,b of f =  h/3[y0 + 4y1 + 2y2 + 4y3 + 2y4 + ... + 2yn-2 + 4yn-1 + yn]
;
; where h = (b - a)/n, for some even integer n, and yk = f(a + kh). 
; (Increasing n increases the accuracy of the approximation.) 
; Define a procedure that takes as arguments f, a, b, and n and returns the value of the integral, 
; computed using Simpson's Rule. Use your procedure to integrate cube between 0 and 1 
; (with n = 100 and n = 1000), and compare the results to those of the integral procedure shown above.
; ---------------------------------

; Here we see one bit of a difference comparing to already defined method which produces sums. 
; Here, depending on the value of 4 term can be multiplied by 2 or by 4 or by 1 fo special cases of 0 and n.
; Therefore we need modified sum that gets these cases.


; Helper Procedures

(define (inc x)
  (+ x 1))

(define (cube x)
  (* x x x))

; Summation Higer-order-procedure
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (simpson-term k)
    (define y (f (+ a (* k h))))
    (cond ((or (= k 0) (= k n)) y)
          ((even? k) (* 2.0 y))
          (else (* 4.0 y))))
  (* (/ h 3.0)
     (sum simpson-term 0 inc n)))


(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))


; (display (integral cube 0 1 0.01))
; 0.24998750000000042
; (display (integral cube 0 1 0.001))
; 0.249999875000001
;
; (display (simpson-integral cube 0 1 100))
; 0.24999999999999992
; (display (simpson-integral cube 0 1 1000))
; 0.2500000000000002
;
; conclusion : Simpson-interal Procedure converges to the actual answer much faster.



