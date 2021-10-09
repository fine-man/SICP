; solution to exercise 1.8
; Newton’s method for cube roots is based on
; the fact that if y is an approximation to the cube root of x,
; then a better approximation is given by the value
; y_new = (x/y^2 + 2y)/3
; Use this formula to implement a cube-root procedure anal-ogous
; to the square-root procedure

(define (cube-root x)
  (cube-rt-iter 1.0 x))

(define (cube-rt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-rt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- guess (improve guess x))) 0.001))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))


      