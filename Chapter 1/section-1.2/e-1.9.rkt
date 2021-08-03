; solution to exercise 1.9
; Question :
; Each of the following two procedures defines
; a method for adding two positive integers in terms of the
; procedures inc, which increments its argument by 1, and
; dec, which decrements its argument by 1.
; Using the substitution model, illustrate the process gener-ated
; by each procedure in evaluating (+ 4 5). Are these
; processes iterative or recursive?

(define (+ a b)
  (if (= a 0) b (inc (+ (dec a) b))))

(define (add a b)
  (if (= a 0) b (add (dec a) (inc b))))

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

; 1. Evolution of the process using the 1st procedure
; (+ 4 5)
; (inc (+ 3 5)
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; (inc 9)
; 10
; conclusion : It is a recursive procedure and
; a linear recursive process.
;
;
; 2. Evolution of the process using the 2nd procedure
; (add 4 5)
; (add 2 6)
; (add 1 7)
; (add 0 8)
; 8
; conclusion : It is a recursive procedure but
; a linear iterative process.