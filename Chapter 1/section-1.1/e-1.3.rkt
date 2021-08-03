;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname e-1.3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; solution to exercise 1.3
#|Define a procedure that takes three numbers as
arguments and returns the sum of the squares of the two
larger numbers|#


(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x)
     (square y)))

(define (sum-large-squares x y z)
  (cond ( (and (< x y) (< x z) ) (sum-of-squares y z))
        ( (and (< y z) (< y x) ) (sum-of-squares z x))
        ( else (sum-of-squares x y))))



