; Solution to exercise 1.11
; A function f is defined by the rule that
; F(n) = n : n < 3
; F(n) = F(n - 1) + 2 * F(n - 2) + 3 * F(n - 3) : n >= 3.
; Write a procedure that computes f by means of a recursive
; process. Write a procedure that computes f by means of an
; iterative process.

; Recursive Process
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* (f (- n 2)) 2)
         (* (f (- n 3)) 3))))

; Iterative Process

(define (func n)
  (define (func-iter a b c count)
    (if (= count n)
        a
        (func-iter b
                   c
                   (+ (* 3 a)
                      (* 2 b)
                      c)
                   (+ count 1))))
  (func-iter 0 1 2 0))
