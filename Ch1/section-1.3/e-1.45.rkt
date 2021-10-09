#lang sicp
; Solution to exercise 1.45
;
; Question :
; We saw in section 1.3.3 that attempting to compute square roots by naively finding a
; fixed point of y -> x/y does not converge, and that this can be fixed by average damping.
; The same method works for finding cube roots as fixed points of the average-damped y -> x/y^2.
; Unfortunately, the process does not work for fourth roots -- a single average damp is not
; enough to make a fixed-point search for y -> x/y3 converge. On the other hand, if we
; average damp twice (i.e., use the average damp of the average damp of y -> x/y3) the fixed-point
; search does converge. Do some experiments to determine how many average damps are required to
; compute nth roots as a fixed-point search based upon repeated average damping of y -> x/yn-1.
; Use this to implement a simple procedure for computing nth roots using fixed-point, average-damp,
; and the repeated procedure of exercise 1.43. Assume that any arithmetic operations you need
; are available as primitives.
;
; -------------------------

; Helper Procedures

(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (average a b)
  (/ (+ a b) 2))

(define (exp b n)
  (cond ((= n 0) 1)
        ((even? n) (exp (square b) (/ n 2)))
        (else (* b (exp b (- n 1))))))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define tolerance 0.0001)

(define (close-enough? a b)
  (< (abs (- a b)) tolerance))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond ((= n 1) f)
        (else (compose f (repeated f (- n 1))))))


; 4th root procedure

(define (four-root x)
  (fixed-point ((repeated average-damp 2)
                (lambda (y) (/ x (cube y))))
               1.0))

(define (nth-root x n damp)
  (fixed-point ((repeated average-damp damp)
                (lambda (y) (/ x (exp y (- n 1)))))
               1.0))

; After experimenting a bit
; (nth-root 32 4 2) -> converges to ~ 2
; (nth-root 32 5 2) -> converges to ~ 2
; (nth-rot 64 6 2) -> converges to ~ 2
; (nth-root 128 7 2) -> converges to ~ 2
; (nth-root 256 8 2) -> gets stuck in an infinite loop so does not converge
;
; (nth-root 256 8 3) -> converges to ~ 2
; (nth-root 512 9 3) -> converges to ~ 2
; .
; .
; .
; (nth-root 32768 15 3) -> converges to ~ 2
; (nth-root 65536 16 3) -> gets stuck in an infinite loop so does not converge
;
; (nth-root 65536 16 4) -> converges to ~ 2
; 
; from the above calculations we see that the no. of damps required increases
; by 1 at every power of 2 and stays constant between powers of 2
; so we can make the hypothesis that the required no. of damps is equal to
; the damps = log(n) base 2

; Procedure to get the damp value calcluated using the formula above
(define (get-damps n)
  (define (iter i power)
    (cond ((> power n) (- i 1))
          (else (iter (+ i 1) (* 2 power)))))
  (iter 0 1))

; Better nth-root which does not need damps to be given by user
(define (better-nth-root x n)
  (fixed-point ((repeated average-damp (get-damps n))
                (lambda (y) (/ x (exp y (- n 1)))))
               1.0))

; Test using large numbers
; (better-nth-root 4294967296 32) -> converges to ~ 2
; (better-nth-root 340282366920938463463374607431768211456 128) -> converges to ~ 2



