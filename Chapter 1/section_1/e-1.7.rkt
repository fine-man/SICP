; solution to exercise 1.7

;Question :
#| the good-enough? test used in computing
square roots will not be very effective for finding the square
roots of very small numbers. Also, in real computers, arith-
metic operations are almost always performed with lim-
ited precision. is makes our test inadequate for very large
numbers. Explain these statements, with examples showing
how the test fails for small and large numbers. An alterna-
tive strategy for implementing good-enough? is to watch
how guess changes from one iteration to the next and to
stop when the change is a very small fraction of the guess.
Design a square-root procedure that uses this kind of end
test. Does this work beter for small and large numbers? |#

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (begin ; using this compound procedure to
        (display guess) ; able to display the guesses
        (newline)
      (sqrt-iter (improve guess x) x)
      )
  )
)

(define (find-sqrt x)
  (sqrt-iter 1.0 x))
; 

; better sqrt-procedure
(define (strict-good-enough? guess x)
  (< (abs (- guess (improve guess x))) 0.001))

(define (better-sqrt-iter guess x)
  (if (strict-good-enough? guess x)
      guess
      (begin
        (display guess)
        (newline)
        (better-sqrt-iter (improve guess x) x)
      )
  )
)

(define (better-sqrt x)
  (better-sqrt-iter 1.0 x))


; Problems with the first Procedure
;
; 1. Small Numbers :
;
; if we run something like (find-sqrt 0.00001) then we get the following guesses
;
; 1.0
; 0.500005
; 0.250012499900001
; 0.1250262489500585
; 0.06255311607712873
; 0.03135649010771716
;
; Obviously this is not the correct answer for the sqrt.
; The Procedure gave the wrong answer because the tolerance (0.001)
; is not small enough to provide any accurate answers as
; 0.00001 - (square 0.03135649010771716) < 0.001 so the first procedure
; ends our evaluation.
;
; evluating the same number with the second procedure (better-sqrt 0.00001)
; gives us 0.0034205558981478842 as the result which is an order of magnitude
; better than the 1st procedure
;
; 2. Big Numbers :




