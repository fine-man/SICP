# Multiple Representation for Abstract Data

## Intro
Previously we used data abstraction, a way of structuring systems in such a way that the programs manipulating data can be
specified independent of the choices involed in implementing the data objects themselves.

For example we created data-objects for rational numbers and structured the procedures in such a way that for procedures like
(add-rat, mul-rat, sub-rat) the underlying representation of rational numbers didn't matter. The idea in implementing all our
data-abstractions was to erect an abstraction barrier between the representation(constructors and selectors) and the usage of 
compound-data (in the above case : add-rat, mul-rat, sub-rat)

These data-abstraction are powerful tools for controlling complexity of a system but there are some downsides as well.
- There may be more than one useful representation for a data object, for example complex numbers can have two representations
rectangular and polar forms, both of which are equally useful.

The way we solve the above problem is by using *generic procedures* which are procedures that can operate on data that may be
represented in more than one way.

we implement *generic procedures* the following way:
- data objects with *type tags*, that is data objects that include explicit information for how they are to be processed
- *generic selectors* - selectors which select parts of a data-object irrespective of their representation

## Tagged data
> One way to view data abstraction is an implementation of the "principle of least commitment"

To solve the problem of having both rectangular and polar form representation in the same system,
we add type tags - the symbols `rectangular` or `polar` - as part of each complex number
so we can find what type of representation it uses.

To manipulate tagged data, we need to introduce some new procedures
```scheme
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
```

Example using complex numbers :
```scheme
;; Rectangular representation

; Procedure to tell if a complex number uses rectangular form representation
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

; Procedure to extract real part of a rectangular form complex number
(define (real-part-rectangular z) (car z))

; make-from-real-imag-rectangular procedure
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
;; ----------------------------------------------------------------------------

;; Polar Representation

; Procedure to tell if a complex number uses polar form representation
(define (polar? z)
  (eq? (type-tags z) 'polar))

; Procedure to extract real part of a polar form complex number
(define (real-part-polar z)
  (* (magnitude-polar z) 
     (cos (angle-polar z))))

; make-from-real-imag-polar procedure
(define (make-from-real-imag-polar x y)
  (attach-tag 
   'polar
   (cons (sqrt (+ (square x) (square y)))
         (atan y x))))
;; ----------------------------------------------------------------------------

;; generic selectors for both rectangular and polar form representation

; generic selector for real part of a complex number
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type: REAL-PART" z))))
;; ----------------------------------------------------------------------------

;; contructor for complex numbers

; make-from-real-imag procedure
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

; make-from-mag-ang procedure
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))
;; ----------------------------------------------------------------------------

;; Complex number arithmetic 

; Procedure to add two complex numbers
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                      (+ (imag-part z1) (imag-part z2))))
```

The structure of the above program can be summarised by this :
<p align="center">
  <img src="./complex-number-structure.png" />
</p>

## Data-Directed Programming and Additivity
> The general stratedy of checking the type of a datum and calling an appropriate procedure is called *dispatching on type*.

Implenting dispatch as in the above programs has two weeknesses
- Generic interface procedures (`real-part`, `imag-part`, `magnitude` and `angle`)
must know about all the different representations.
- The problem of name conflict while developing different representations of 
a data-object separately.

These issues can also be summarized by saying that the earlier technique for
implementing generic interfaces is not *additive*.

The issue can be solved by using lookup-table for efficient dispatching.
Each entry in the table are procedures that implements each operation on each type.

```txt
| Operation | Polar           | Rectangular           |
| _________ | _______________ | _____________________ |
| real-part | real-part-polar | real-part-rectangular |
| magnitude | magnitude-polar | magnitude-rectangular |
```
Data-directed programming is the technique of designing programs to work with such a table directly,
Instead of checking for each type in each generic selector porcedure, we can use a
single procedure for implementing the interface. The procedure will lookup the combination
of operation name and argument type to find the correct procedure to apply.

To do this we need two procedures `put` and `get`:
- `(put op type item)` : install `item` in table at `(op, type)`
- `(get op type)` : get item at `(op, type)`, return `#f` if no item is there.
Note : `put` and `get` are implemented in chapter 3

Now we can define rectangular and polar forms in different packages and use `put` to interface them.
Example:
```scheme
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
```
Note : Here the type tags are implemented as a list to give us the freedom
to choose to have multi-type operations.

To implement the generic interface, we need to define a procedure `apply-generic`,
which looks up the table for some operation and types of argument and apply the resuling
procedure to arguments

```scheme
; apple-generic procedure
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))
```

Example of generic selector implemented using `apply-generic`:

```scheme
; real-part selector
(define (real-part z)
  (apply-generic 'real-part z))
```

## Message Passing
An alternative implementation stratedy for dispatching is to decompose the table into
columns and, instead of "intelligent operations" that dispatch on data types, to work with
"intelligent data objects" that dispatch on operation names. We can do this by arranging things
so that a data objects, such as a rectangular number, is represented as a procedure that takes
as input the required operation name and returns the result of performing that operation

Example using `make-from-real-imag` procedure:
```scheme
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
```

The `apply-generic` procedure now simply becomes:
```scheme
(define (apply-generic op arg) (arg op))
```
Note : One limitation of this organization is it permits only 
generic procedures of one argument.

This style of programming is called *message passing*. The name comes 
from the image that	a data-object is an entity that receives the requested
operation name as a "message" and then performs that operation on the data.

