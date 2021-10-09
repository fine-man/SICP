#lang racket

; Solution to exercise 2.5 of SICP

; Helper Functions
(define (fast-exp a n)
  (define (square x)
    (* x x))
  (define (even? x)
    (= (remainder x 2) 0))
  (cond ((= n 0) 1)
        ((even? n)
         (square (fast-exp a (/ n 2))))
        (else (* a (fast-exp a (- n 1))))))


; Procedure to calculate the maximum power of 2 in a number.
(define (max-power-of-two n)
  (define (power-iter power num)
    (cond ((> (remainder n num) 0) (- power 1))
          (else (power-iter (+ power 1) (* num 2)))))
  (power-iter 1 2))

; Procedure to calculate the maximum power of 3 in a number.
(define (max-power-of-three n)
  (define (power-iter power num)
    (cond ((> (remainder n num) 0) (- power 1))
          (else (power-iter (+ power 1) (* num 3)))))
  (power-iter 1 3))

; Constructor Procedure to make pairs of non-negative integers.
(define (cons x y)
  (* (fast-exp 2 x)
     (fast-exp 3 y)))

; Selector Procedure to return the first number in the pair
(define (car z) (max-power-of-two z))

; Selector Procedure to return the second number in the pair
(define (cdr z) (max-power-of-three z))

; Test
(define z (cons 3 2))

(car z)
; 3

(cdr z)
; 2

(car (cons 0 1))
; 0

(cdr (cons 0 1))
; 1

; Conclusion : The described representation above can be used as an
; accurate representation of pairs of non-negative integers but the
; time complexity of (cons x y) is O(log(x) + log(y)) and that of
; (car z) and (cdr z) is O(log(z)) which is very bad compared to the
; inbuilt representation of pairs.
