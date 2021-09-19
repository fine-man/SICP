#lang racket
; Question :
; Define a better version of make-rat that han-
; dles both positive and negative arguments. make-rat should
; normalize the sign so that if the rational number is positive,
; both the numerator and denominator are positive, and if
; the rational number is negative, only the numerator is negative

; old make-rat
; (define (make-rat n d)
;   (let ((g (gcd n d)))
;     (cons (/ n g) (/ d g))))

; better make-rat
(define (make-rat n d)
  (let ((g (gcd n d))
        (sign (/ (* n d) (* (abs n) (abs d)))))
    (cons (/ (* sign (abs n)) g) (/ (abs d) g))))


(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; testing
(define a (make-rat 2 6))

(print-rat a)

(define b (make-rat 2 -6))

(print-rat b)

(define c (make-rat -2 6))

(print-rat c)

(define d (make-rat -2 -6))

(print-rat d)