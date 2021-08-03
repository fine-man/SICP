; Solution to the exercise e-1.10

; The following procedure computes a mathematical
; function called Ackermann’s function.

; Definition :
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))


; What are the values of the following expressions?
;
; 1. (A 1 10)
;= 2^10 = 1024
;
; 2. (A 2 4)
;= 2 ^ (2 ^ (2 ^ 2)) = 2 ^ 16 = 65536
;
; 3. (A 3 3)
;= 2 ^ (2 ^ (2 ^ 2)) = 2 ^ 16 = 65536

; Consider the following procedures, where A is the proce-dure defined above:
;
; 1. (define (f n) (A 0 n)
; 2. (define (g n) (A 1 n))
; 3. (define (h n) (A 2 n))
; 4. (define (k n) (* 5 n n))
;
; Give concise mathematical definitions for the functions computed
; by the procedures f, g, and h for positive integer values
; of n. For example, (k n) computes 5n2
;
;
; 1. (define (f n) (A 0 n))
; we can easily see that F(n) = 2*n
;
; 2. (define (g n) (A 1 n))
; After recursively calculating we can see that G(n) = 2^n
;
; 3. (define (h n) (A 2 n))
; This represents the function which calculates iterated exponentiation (Tetration)
; These kinds of functions increase very quickly even for small values of n
; F(n) = 2 ^ 2 ^ 2 ^ 2....... (n number of 2's)


