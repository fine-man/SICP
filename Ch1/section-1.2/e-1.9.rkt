; solution to exercise 1.9
; Question :
; Each of the following two procedures defines
; a method for adding two positive integers in terms of the
; procedures inc, which increments its argument by 1, and
; dec, which decrements its argument by 1.

; Definition 1:
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a)
              b))))

; Definition 2:
(define (add a b)
  (if (= a 0) b
      (add (dec a)
           (inc b))))

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

; Using the substitution model, illustrate the process gener-ated
; by each procedure in evaluating (+ 4 5). Are these
; processes iterative or recursive?


; 1. Evolution of the process of Definition 1 using applicative subsitution model
; (+ 4 5)
; (inc (+ 3 5)
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9
; conclusion : It is a recursive procedure and
; a linear recursive process.
;
;
; 2. Evolution of the process of Definition 2 using applicative subsitution model
; (add 4 5)
; (add 3 6)
; (add 2 7)
; (add 1 8)
; (add 0 9)
; 9
; conclusion : It is a recursive procedure but
; a linear iterative process.