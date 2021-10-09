; Solutiion to exercise e-1.16
; Question :
; Design a procedure that evolves an iterative
; exponentiation process that uses successive squaring
; and uses a logarithmic number of steps, as does fast-expt
;
; ---------------------------------------------------------
; (Hint: Using the observation that (bn/2)2 = (b2)n/2, keep,
; along with the exponent n and the base b, an additional
; state variable a, and define the state transformation in such
; a way that the product abn is unchanged from state to state.
; At the beginning of the process a is taken to be 1, and the
; answer is given by the value of a at the end of the process.
; In general, the technique of defining an invariant quantity
; that remains unchanged from state to state is a powerful
; way to think about the design of iterative algorithms.)
; ----------------------------------------------------------

(define (fast-exp b n)
  (define (exp-iter a b n)
    (cond ((= n 0) 1)
          ((= n 1) (* a b))
          ((even? n) (exp-iter a (* b b) (/ n 2)))
          (else (exp-iter (* a b) b (- n 1)))))
  (define (even? x)
    (= (remainder x 2) 0))
  (exp-iter 1 b n))
