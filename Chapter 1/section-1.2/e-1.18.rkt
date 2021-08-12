; Solution to exercise e-1.18
; Question :
; ----------------------------------------------------------------
; Using the results of Exercise 1.16 and Exer-
; cise 1.17, devise a procedure that generates an iterative pro-
; cess for multiplying two integers in terms of adding, dou-
; bling, and halving and uses a logarithmic number of steps
; -----------------------------------------------------------------

; Logrithmic Iterative Process
(define (fast-prod a b)
  (define (prod-iter c a b)
    (cond ((= b 0) 0)
          ((= b 1) (+ c a))
          ((even? b) (prod-iter c (+ a a) (/ b 2)))
          (else (prod-iter (+ c a) a (- b 1)))))
  (define (even? x)
    (= (remainder x 2) 0))
  (prod-iter 0 a b))
