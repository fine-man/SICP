#lang racket

; Solution to exercise 2.10

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x))


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

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

; Better definition of (div-interval x y) with error function which gives an
; error when we try to divide by an interval that spans 0

(define (new-div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Devision error (interval spans 0)" y)
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))

; Test

(define a (make-interval -1 1))

(define b (make-interval 2 3))

(define c (make-interval 1 2))

(print-interval (new-div-interval b c))
; (1.0, 3.0)

(new-div-interval b a)
; Devision error (interval spans 0) '(-1 . 1)



