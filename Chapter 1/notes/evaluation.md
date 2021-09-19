# Evaluation 

To evaluate combination whose operators are primitive procedures i.e - `+, -, *, /`,
lisp follows the following rule :

1. Evaluate the subexpressions of the combination

2. Apply the procedure that is the value of the leftmost subexpression (the operator) to the arguments that are the values of the other subexpressions (the operands).

Example

```lisp
(* (+ 3 5 7)
   (+ 2 (* 4 5)))
```

would be evaluated as follows

```lisp
(* 15 (+ 2 (* 4 5)))
```

we saw that it evaluated `(+ 3 5 7)` part of our combination and next

```lisp
(* 15 (+ 2 20))
```

it evaluated the nested part of the second operand of our combination, finally

```lisp
(* 15 22)
```

the operands are evaluated and are reduced to their primitive forms and we can apply the 

```lisp
390
```
operator on the operands to give us the final answer

## Compound Procedures

To evaluate combinations whose operators name compound procedures we have to use something called `substitution models` which help us decide when to evaluate the value of a combination or basically when to "substitute"
the combination with it's value hence the name substitution model.

Here we will describe two such models : **Applicative order** and **Normal Order**
and show the evaluation of the following expression

```lisp
(f 5)
```
where `f` is defined as follows

```lisp
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (square x) (* x x))

```

### Applicative Order

In this subtitution model we follow these rules

1. Evaluate the operator to get the operator to be applied

2. Evaluate the operands to get the value of the arguments

3. Substitute the value of the formal parameters of the procedure with the actual values 
calculated from the operands

Example :

Evaluate the expression

```lisp
(f 5)
```
we start by retrieving the body of `f`

```lisp
(sum-of-squares (+ a 1) (* a 2))
```

Then we replace the formal parameter `a` by the argument 5

```lisp
(sum-of-squares (+ 5 1) (* 5 2))
```
Evaluate the operands and substitute their values

```lisp
(sum-of-squares 6 10)
```
Fetch the body of `sum-of-squares`

```lisp
(+ (square x) (square y))
```
Replace the value of formal parameter `x` with 6 and of `y` with 10

```lisp
(+ (square 6) (square 10))
```
Fetch the body of `square`

```lisp
(+ (* x x) (* y y))
```
Replace the value of formal parameter `x` with 6 and of `y` with 10

```lisp
(+ (* 6 6) (* 10 10))
```
Evaluate the operand expressions to get their value and subsitute them

```lisp
(+ 36 100)
```
Apply the primitive operator `+` to the primitive operands to get the value

```lisp
136
```

### Normal Order

In this subsitution model, we use the following rules

1. Evaluate The operator to get the body of the procedure to be applied

2. Substitute the formal parameters with the expression of the operands (basically copy paste the expression of operands into the formal parameters)

3. Only evaluate the operands when you have an expression involving only primitive operators

Example :

Evaluate the expression

```lisp
(f 5)
```

Evaluate `f` to get the body of the procedure to be applied

```lisp
(sum-of-squares (+ a 1) (* a 2))
```

Replace the value of the formal parameter `a` with 5

```lisp
(sum-of-squares (+ 5 1) (* 5 2))
```
Fetch the body of `sum-of-squares` to get the procedure to be applied

```lisp
(+ (square x) (square y))
```

Substitute the value of `x` with `(+ 5 1)` and of `y` with `(* 5 2)`

```lisp
(+ (square (+ 5 1)) (square (* 5 2)))
```

Fetch the body of `square`

```lisp
(+ (* x x) (* y y))
```

Replace `x` with `(+ 5 1)` and `y` with `(* 5 2)`

```lisp
(+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))
```

Now we can start evaluating the operands and reducing the expression, we start by evaluating the inner most (nested) sub-expressions

```lisp
(+ (* 6 6) (* 10 10))
```
Then evaluating the reduced sub-expressions to finally get the primitive expression

```lisp
(+ 36 100)
```
Now we can finally apply the operator to the operands to get the value

```lisp
136
```

