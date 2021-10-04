# What is Data
We can think of *Data* as defined by some collection of *selectors* and *constructors* that have to fulfill some 
*conditions* for them to be a valid representation.

We can also think of those *conditions* as the *axioms* of that data object, which any valid representation of
that data object will have to follow.

For example, in our rational number implementation, `add-rat`, `mul-rat`, etc were implemented in terms of the 
procedures `make-rat`, `numer`, `denom`, but not any set of constructors and selectors would have worked for
our representation of rational numbers. Any set of contructors and selectors must fulfill the following condition/axioms :
```scheme
(define rat (make-rat n d))

(= (/ (numer rat) (denom rat))
   (/ n d))
```

Because our representation of `make-rat`, `numer` and `denom` :
```scheme
(define (make-rat n d)
  (cons n d))

(define (numer rat) (car rat))

(define (denom rat) (cdr rat))
```
Fulfills the conditions defined above, so it is a valid representation of rational numbers.

The above point of view about data is not only applicable for "high-level" data objects like our rational number
representation , but can also be applied to "low-level" data-objects. 

For example, the procedures `cons`, `cars`, `cdr`, which used to make pair, were included as primitives in scheme,
but any set of procedures that fulfill the following conditions:
```scheme
(define pair (cons x y))

(= (car pair) x)

(= (cdr pair y))
```
can be used as an adequate representation of `cons`, `cars` and `cdr`.

Here is one such implementation that only uses procedures instead of other data objects.
```scheme
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))
```
since this representaion follows the above conditions, so it is a valid representation.

Note : Here `cons` returns the procedure `dispatch`, which takes an input number and returns either `x` or `y`.

The above example demonstrates that the ability to manipulate procedures like objects (i.e : procedures returning other
procedures or taking other procedures as arguments) provides the ability to represent compound data.
This style of programming, which uses procedural representation of data is called *message passing*.

Here is an alternate representation of `cons`, `car` and `cdr` that also uses message passing :
```scheme
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))
```

