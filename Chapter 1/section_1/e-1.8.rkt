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


      