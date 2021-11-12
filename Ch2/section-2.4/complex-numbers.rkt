#lang racket
; Complex Number Representation


#|
; section 2.4.1 : Representation for Complex Numbers

In this section we try a naive approach to design a system that
works with both rectangular and polar forms.

For addition of complex numbers it is useful to think in terms of rectangular form:
Real-part(Z1 + Z2) = Real-part(Z1) + Real-part(Z2)
Imaginary-part(Z1 + Z2) = Imaginary-part(Z1) + Imaginary-part(Z2)

For multiplication it is useful to think in terms of polar form:
Magintude(Z1.Z2) = Magintude(Z1) . Magintude(Z2)
Angle(Z1 . Z2) = Angle(Z1) + Angle(Z2)

To incorporate both these forms we use what we wishful-thinking like we used in 
data-abstraction to assume we already have the selector procedures -
'real-part', 'imag-part', 'magnitude' and 'angle' and have two constructor procedures -
'make-from-real-imag' and 'make-from-mag-ang'. The above procedures have the following 
property that :
(make-from-real-imag (real-part z) (imag-part z))

and

(make-from-mag-ang (magnitude z) (angle z))

produce the same complex number Z

; Procedure to add two complex numbers
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                      (+ (imag-part z1) (imag-part z2))))

; Procedure to subtract two complex numbers
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

; Procedure to multiple two complex numbers
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

; Procedure to divide two complex numbers
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))


Now the above representation works with both rectangular and polar forms
for example if we decide to use the rectangular forms as the base representation

;; Rectangular Form Representation

(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) (cons x y))

(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))


;; Polar Form Representation

(define (magnitude z) (car z))

(define (angle z) (cdr z))

(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (make-from-mag-ang r a) (cons r a))

The thing with this kind of representation is that we have made
generic-procedures(add-complex, sub-complex, mul-complex) that can handle
both representations but still we cannot use both representations in the same program
at the same time because our selectors are not generic-selectors yet
|#




