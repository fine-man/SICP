#lang sicp

; Solution to exercise 1.26
; Question :
; Louis Reasoner is having great difficulty do-
; ing Exercise 1.24. His fast-prime? test seems to run more
; slowly than his prime? test. Louis calls his friend Eva Lu
; Ator over to help. When they examine Louis’s code, they
; find that he has rewrttien the expmod procedure to use an
; explicit multiplication, rather than calling square:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

; “I don’t see what difference that could make,” says Louis.
; “I do.” says Eva. “By writing the procedure like that, you
; have transformed the Θ(logn) process into a Θ(n) process.”
; Explain.
; -----------------------------------------------------------------------

; abrreviations:
; f = expmod
; b = base
;
; (display (expmod 3 4 4))
; Recursion tree with the modified expmod:
;
;                                     (f 3 4 4)
;                                      
;                 (f 3 2 4)                               (f 3 2 4)
;
;       (f 3 1 4)           (f 3 1 4)           (f 3 1 4)          (f 3 1 4)
;
; (f 3 0 4) (f 3 0 4) (f 3 0 4) (f 3 0 4) (f 3 0 4) (f 3 0 4) (f 3 0 4) (f 3 0 4)
;
;
; Recusrion tree with original expmod:
;
;                                     (f 3 4 4)
;
;                                     (f 3 2 4)
;
;                                     (f 3 1 4)
;
;                                     (f 3 0 4)
;
;
; Explanation : In the new "expmod" becuase of the line:
; (* (expmod base (/ exp 2) m)
;    (expmod base (/ exp 2) m))
; we have to calculate the same thing multiple times which we can
; also see from the recursion tree. At each level we have
; twice the number of function calls than the previous level
; and so the numbers of operations which we will have to do perform at the lower-most
; level will be 2 ^ (depth of tree) , depth of tree = log(exp) + 1
; No. of operations = Θ(2 ^ depth) = Θ(2 ^ (log(exp) + 1)) = Θ(2n) = Θ(n)

