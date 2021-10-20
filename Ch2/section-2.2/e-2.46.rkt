#lang racket

(provide (all-defined-out))
; solution to exercise 2.46 of SICP
; community.schemewiki.org/?sicp-ex-2.46

; constructor for a vector
(define (make-vect x y)
  (cons x y))

; selector for x-coordinate of a vector
(define (xcor-vect vec)
  (car vec))

; selector for y-coordinate of a vector
(define (ycor-vect vec)
  (cdr vec))

; Procedure to add two vectors
(define (add-vect vec1 vec2)
  (make-vect (+ (xcor-vect vec1) (xcor-vect vec2))
             (+ (ycor-vect vec1) (ycor-vect vec2))))

; Procedure to subtract vectors
(define (sub-vect vec1 vec2)
  (make-vect (- (xcor-vect vec1) (xcor-vect vec2))
             (- (ycor-vect vec1) (ycor-vect vec2))))

; Procedure to scale a vector by a number k
(define (scale-vect vec k)
  (make-vect (* (xcor-vect vec) k)
             (* (ycor-vect vec) k)))

;; TEST
;(define a (make-vect 1 2))
;(define b (make-vect 2 1))

; (add-vect a b)
; '(3 . 3)

; (sub-vect a b)
; (-1 . 1)

; (scale-vect a 3)
; '(3 . 6)

