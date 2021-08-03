; Observe that our model of evaluation allows
; for combinations whose operators are compound expres-sions.
; Use this observation to describe the behavior of the following procedure

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; because of our evaluation model it is possible for the parameter "b"
; to choose the operator and the procedure gives the same answer as
; a + abs(b)

