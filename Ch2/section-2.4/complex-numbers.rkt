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

#|
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
|#

#| Data-Directed Programming and Additivity - section 2.4.3

The general stratedy of checking the type of a datum and calling an
appropriate procedure is called "dispatching on type". This is a powerful
stratedy for obtaining modularity in system design.

Implenting dispatch as in the above programs has two weeknesses
- Generic interface procedures (real-part, imag-part, magnitude and angle)
must know about all the different representations.
- The problem of name conflict while developing different representations of 
a data-object separately.

These issues can also be summarized by saying that the earlier technique for
implementing generic interfaces is not additive.

The issue can be solved by using lookup-table for efficient dispatching.
Each entry in the table are procedures that implements each operation on each type.

| Operation | Polar           | Rectangular           |
| _________ | _______________ | _____________________ |
| real-part | real-part-polar | real-part-rectangular |
| magnitude | magnitude-polar | magnitude-rectangular |

Data-directed programming is the technique of designing programs to work with such a table directly,
Instead of checking for each type in each generic selector porcedure, we can use a
single procedure for implementing the interface. The procedure will lookup the combination
of operation name and argument type to find the correct procedure to apply.

To do this we need two procedures "put" and "get":
- (put op type item) : install "item" in table at (op, type)
- (get op type) : get item at (op, type), return #f if no item is there.
Note : "put" and "get" are implemented in chapter 3

Now we can define rectangular and polar forms in different packages and use "put" to interface them.
|#

;;-------------------------------------------------------------------------------------
;; Package for Rectangular form representation

; install-rectangular-package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

; Note : Here the type tags are implemented as a list to give us the freedom
; to choose to have multi-type operations.
;;---------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; Package for Polar form representation

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

;;---------------------------------------------------------------------------

; To implement the generic interface, we need to define a procedure "apply-generic",
; which looks up the table for some operation and types of argument and apply the resuling
; procedure to arguments

; apple-generic procedure
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

;;---------------------------------------------------------------------------

;; Generic selector implemented using apply-generic

; real-part selector
(define (real-part z)
  (apply-generic 'real-part z))

; imaginary part selector
(define (imag-part z)
  (apply-generic 'imag-part z))

; magnitude selector
(define (magnitude z)
  (apply-generic 'magnitude z))

; angle selector
(define (angle z)
  (apple-generic 'angle z))

;; constructors

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 
        'rectangular) 
   x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 
        'polar) 
   r a))

#| Message Passing
An alternative implementation stratedy for dispatching is to decompose the table into
columns and, instead of "intelligent operations" that dispatch on data types, to work with
"intelligent data objects" that dispatch on operation names. We can do this by arranging things
so that a data objects, such as a rectangular number, is represented as a procedure that takes
as input the required operation name and returns the result of performing that operation
|#

;; make-from-real-imag procedure using message-passing
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op: 
            MAKE-FROM-REAL-IMAG" op))))
  dispatch)

; apply-generic procedure now simple becomes
(define (apply-generic op arg) (arg op))

#|
Note : One limitation of this organization is it permits only 
generic procedures of one argument.

This style of programming is called *message passing*. The name comes 
from the image that	a data-object is an entity that receives the requested
operation name as a "message" and then performs that operation on the data.
|#
