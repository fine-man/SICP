#lang racket

; Helper Function
(define (average x y)
  (/ (+ x y) 2))

; Main Procedures
(define (make-point x y) (cons x y))

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment x1 y1 x2 y2)
  (cons (make-point x1 y1)
        (make-point x2 y2)))

(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint-segment s)
  (make-point
   (average (x-point (start-segment s))
            (x-point (end-segment s)))
   (average (y-point (start-segment s))
            (y-point (end-segment s)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(define line (make-segment 0 0 2 2))

(print-point (start-segment line))
; (0, 0)

(print-point (end-segment line))
; (2, 2)

(print-point (midpoint-segment line))
; (1, 1)