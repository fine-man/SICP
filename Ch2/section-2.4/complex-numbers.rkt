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

#| Tagged Data : section 2.4.2
One way to view data abstraction is an implementation of the
"principle of least commitment"

"The principle of least commitment" suggests us to postpone any choices we need to
make until the last moment. For example in data abstraction, The abstraction barrier
formed by the selectors and constructors permits us to defer to the last possible moment
the choice of a concrete representation of our data objects and thus retain maximum flexibility
in our system design

To solve the problem of having both rectangular and polar form representation in the same system,
we add type tags - the symbols "rectangular" or "polar" - as part of each complex number
so we can find what type of representation it uses.

|#

;;------------------------------------------------------------------------- 
;; To manipulate tagged data, we need to introduce some new procedures

; Procedure which attatches a tag to a data-object
(define (attach-tags type-tag contents)
  (cons type-tag contents))

; Procedure which extracts the tag from a data object
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

; Procedure to extract the actual data from a tagged data-object
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

; Procedure to tell if a complex number uses rectangular form representation
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

; Procedure to tell if a complex number uses polar form representation
(define (polar? z)
  (eq? (type-tags z) 'polar))
;;----------------------------------------------------------------------------

;; -------------------------------------------------------------------------
;; Rectangular representation

; Procedure to extract real part of a rectangular form complex number
(define (real-part-rectangular z) (car z))

; Procedure to extract imag part of a rectangular form complex number
(define (imag-part-rectangular z) (cdr z))

; Procedure to extract magnitude of a rectangular form complex number
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

; Procedure to extract angle of a rectangular form complex number
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

; make-from-real-imag-rectangular procedure
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

; make-from-mag-ang-rectangular procedure
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 
   'rectangular
   (cons (* r (cos a)) (* r (sin a)))))
;; --------------------------------------------------------------------------

;; --------------------------------------------------------------------------
;; Polar Representation

(define (real-part-polar z)
  (* (magnitude-polar z) 
     (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) 
     (sin (angle-polar z))))

(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 
   'polar
   (cons (sqrt (+ (square x) (square y)))
         (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; generic selectors for both rectangular and polar form representation

; generic selector for real part of a complex number
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type: REAL-PART" z))))

; generic selector for imaginary part of a complex number
(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type: IMAG-PART" z))))

; generic selector for magnitude of a complex number
(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type: 
               MAGNITUDE" z))))

; generic selector for angle of a complex number
(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type: 
               ANGLE" z))))
;;--------------------------------------------------------------------------

;;--------------------------------------------------------------------------
;; contructor for complex numbers

; make-from-real-imag procedure
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

; make-from-mag-ang procedure
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;;--------------------------------------------------------------------------

