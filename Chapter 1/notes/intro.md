# 1.1 The Elements of Programming
Any Programming Language provide tools to combine simple ideas to make more complex ideas.
Every Powerful language should have these three mechanisms for providing those tools :

* **Primitive Expressions**, which represent the simplest entities the language is concerned with
* **means of combination**, by which compound elements are built from simpler ones 
* **means of abstractions**, by which compound elements can be named and be manipulated as units

Lisp in particular provides very few primitives and very powerful ways of combining them and creating new abstractions. `3` is a primitive as well as the operator `+`.

```lisp
(+ 3 4)
```
The above is a combination using the primitives `+`, `3`, `4` which evaluates to the sum of 3 and 4 which is 7. Here `()` acts as a means of combination of those primitives to make simple expressions. 

Lisp uses [Prefix Notation](https://en.wikipedia.org/wiki/Polish_notation) which basically means that the operator comes before the operands in any expression.

#### Some example expressions and their evaluated values :
```lisp
(+ 21 35 12 7)

> 75
```

```lisp
(+ (* 3 5) (- 10 6))

> 19
```

```lisp
(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))

> 57
```

To make new abstractions we need a way to define new primitives. In Lisp we do this by using the `define` keyword, for example `(define pi 3.14)` assigns the value 3.14 to the word `pi`

Using the same keyword we can also define procedures which perform a particular task, for e.g :

```lisp
(define (square x) (* x x))
```
Which is basically doing this

```lisp
(define square (lambda (x) (* x x)))
```
The above expressions assigns the name `square` to the process of taking a number and squaring it.


### Conditional expressions
Lisp provides us ways of making decisions inside procedures that we make. The way it is achieved is by using the keywords `cond` and `if`

The General form of a `cond` expression is :

```lisp
(cond (<p1> <e1>)
      (<p2> <e2>)
      ....
      (<pn> <en>))
```
where each pair `(<pn> <en>)` is called a clause and `<pn>` is called the predicate.

An Example which defines a procedure `abs` to find the absolute of a number
```lisp

(define (abs x)
  (cond ((< x 0) (-x))
        (else x)))

```

The evaluation of `cond` expressions is done by evaluating `<p1>` first. If the value of false, then `<p2>` is evaluated. If `<p2>` is false, then `<p3>` is evaluated. This process continues until a predicate is found whose value is true, in which case the interpreter returns the value of the corresponding consequent expression `<e>` as the value of the `cond` expression. If none of the `<p>` is true then value of the expression is undefined.

The General form of an `if` expression :

```lisp
(if <predicate> <consequent> <alternative>)
```

Example with the same procedure `abs`

```lisp
(define (abs x)
  (if (< x 0)
      (- x)
      x))

```
To evaluate an `if` expression, the interpreter starts by evaluating the
`⟨predicate⟩`. If the `⟨predicate⟩` evaluates to a true
value, the interpreter then evaluates the `⟨consequent⟩` and returns its
value. Otherwise it evaluates the `⟨alternative⟩` and returns its value.

