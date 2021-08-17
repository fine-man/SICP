#lang sicp

; Solution to exercise 1.34
; Question :
; Suppose we define the procedure
;
; (define (f g) (g 2))
;
; Then we have

; (f square)
; 4
; (f (lambda (z) (* z (+ z 1))))
; 6
;
; What happens if we (perversely) ask the interpreter to evaluate
; the combination (f f)? Explain

(define (f g) (g 2))

(f f)
; Evaluation of the above expression
; (f f)
; (f 2)
; (2 2)
; When our process reaches the above stage we should expect an error message saying
; that it expected to receive a Procedure to apply on the argument '2' but instead we gave it
; the number 2 which it cannot apply on a number
;
; Actual Error Message :
; application: not a procedure; expected a procedure that can be applied to arguments given: 2. 
; Which is exactly what we expected it to give.
