; Solution to exercise 1.20
; Question :
; The process that a procedure generates is
; of course dependent on the rules used by the interpreter.
; As an example, consider the iterative gcd procedure given
; above. Suppose we were to interpret this procedure using
; normal-order evaluation, as discussed in Section 1.1.5. (The
; normal-order-evaluation rule for if is described in Exercise
; 1.5.) Using the substitution method (for normal order), illus-
; trate the process generated in evaluating (gcd 206 40) and
; indicate the remainder operations that are actually per-
; formed. How many remainder operations are actually per-
; formed in the normal-order evaluation of (gcd 206 40)?
; In the applicative-order evaluation
;
; Iterative procedure for calculating gcd
(define (euclid-gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(display (euclid-gcd 206 40))

; Normal order (expand and then reduce) :
; In Normal order evaluation we don't evaluate the operands
; instead we just subsitute them as the actual parameters
; and don't evaluate untill there is no more subsituition to do
; or it need to be evaluated for some primary operator like
; (if (= b 0) <this> <that>) will evaluate the value of 'b'
;
; abbreviations:
; gcd = (euclid-gcd)
; rem = remainder
;
; (gcd 206 40)
;
; evaluate :
; (if (= 40 0) 0) -> #f
;
; subsituiton
; (gcd 40 (rem 206 40)) -> gcd(40 6)
;
; evaluate
; (if (= (rem 206 40) 0) 0) -> #f, +1 for evaluating rem
;
; subituition
; (gcd (rem 206 40) (rem 206 40)) -> gcd(6 4)
;
; evaluate
; (if (= (rem 40 (rem 206 40)) 0) 0) -> #f, +2 for evaluating rem
;
; subsituition
; (gcd (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40)))) -> gcd(4, 2)
;
; evaluate
; (if (= (rem (rem 206 40) (rem 40 (rem 206 40))) 0) 0) -> #f, +4 for evaluating rem
;
; subsituition
; (gcd (rem (rem 206 40) (rem 40 (rem 206 40))) (rem (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40))))) -> gcd(2, 0)
;
; evaluate
; (if (= (rem (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40)))) 0) 0) -> #t, +7 for evaluating rem
;
; return
; (rem (rem 206 40) (rem 40 (rem 206 40))) -> 2, +4 for evaluating rem
;
; Total = 1 + 2 + 4 + 7 + 4 = 18 evaluations of rem
; -------------------------------------------------------------------------------------------------------------------------------------
;
; Applicative order (evaluate and then subsitute) :
; In Applicative order we evaluate the value of operands first and then we
; substitute them into the formal parameters.
;
; gcd(206, 40)
;
; evaluate
; (if (= 40 0) 206) -> #f
;
; substitute
; (gcd 40 (rem 206 40)) -> (gcd 40 6), +1 for evaluating rem
;
; evaluate
; (if (= 6 0) 40) -> #f
;
; substitute
; (gcd 6 (rem 40 6)) -> (gcd 6 4), +1 for evaluating rem
;
; evaluate
; (if (= 4 0) 6) -> #f
;
; substitute
; (gcd 4 (rem 6 4)) -> (gcd 4 2), +1 for evaluating rem
;
; evaluate
; (if (= 2 0) 4) -> #f
;
; substitute
; (gcd 2 (rem 4 2)) -> (gcd 2 0), +1 for evaluating rem
;
; evaluate
; (if (= 0 0) 2) -> 2
;
; Total = 1 + 1 + 1 + 1 = 4 evaluations of rem

