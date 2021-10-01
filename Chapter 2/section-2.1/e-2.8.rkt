#lang racket

; solution to exercise 2.8
(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x))


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

; sub-interval, going by the same analogy of add-interval procedure, we
; we notice that we should define sub-interval as the difference between
; the upper-bounds of the intervals and respectively the lower-bounds of the
; intervals. For eg :
; x = (a, b)
; y = (c, d)
; (sub-interval x y) = (a - c, b - d)

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))


(define (print-interval x)
  (newline)
  (display "(")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display ")"))

; Test

(define a (make-interval 1 2))

(define b (make-interval 3 4))

(print-interval (sub-interval b a))
; (2, 2)