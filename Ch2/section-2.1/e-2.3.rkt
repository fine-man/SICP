#lang racket

; Helper Procedures
(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (make-point x y) (cons x y))

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment x1 y1 x2 y2)
  (cons (make-point x1 y1)
        (make-point x2 y2)))

(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

#|

  A ------- B
  |         |
  |         |
  D ------- C

|#

(define (make-rect a b c d)
  (cons (make-point a b)
        (make-point c d)))

(define (top-left rect) (car (car rect)))
(define (top-right rect) (cdr (car rect)))
(define (bottom-right rect) (car (cdr rect)))
(define (bottom-left rect) (cdr (cdr rect)))

(define (dis p1 p2)
  (sqrt (+
         (square (-
                  (x-point p1)
                  (x-point p2)))
         (square (-
                  (y-point p1)
                  (y-point p2))))))

(define (breadth rect)
  (dis (top-left rect)
       (top-right rect)))

(define (length rect)
  (dis (top-left rect)
       (bottom-left rect)))

(define (perimeter rect)
  (* 2 (+
        (breadth rect)
        (length rect))))

(define (area rect)
  (* (breadth rect)
     (length rect)))


; Test
(define rec (make-rect
             (make-point 0 1)
             (make-point 1 1)
             (make-point 1 0)
             (make-point 0 0)))

(display (area rec))
(newline)
; 1

(display (perimeter rec))
(newline)
; 4

; Alternate Representation of make-rect using top and bottom line segment
