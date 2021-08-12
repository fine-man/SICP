; Solution to Exercise 1.19
; Question :
; -----------------------------------------------------------------------------------------------------------------------------------
; There is a clever algorithm for computing the Fibonacci numbers in a logarithmic number of steps.
; Recall the transformation of the state variables a and b in the fib-iter process of section
; 1.2.2: a <- a + b and b <- a. Call this transformation T, and observe that applying T over 
; and over again n times, starting with 1 and 0, produces the pair Fib(n + 1) and Fib(n). 
; In other words, the Fibonacci numbers are produced by applying T^n, the nth power of the transformation T, 
; starting with the pair (1,0). Now consider T to be the special case of p = 0 and q = 1 in a family of 
; transformations Tpq, where Tpq transforms the pair (a,b) according to a <- bq + aq + ap and b <- bp + aq. 
;
; Show that if we apply such a transformation Tpq twice, the effect is the same as using a single transformation 
; Tp'q' of the same form, and compute p' and q' in terms of p and q. This gives us an explicit way to square these 
; transformations, and thus we can compute Tn using successive squaring, as in the fast-expt procedure. 
; Put this all together to complete the following procedure, which runs in a logarithmic number of steps
;
; (define (fib n)
;   (fib-iter 1 0 0 1 n))
; 
; (define (fib-iter a b p q count)
;   (cond ((= count 0) b)
;         ((even? count)
;          (fib-iter a
;                    b
;                    p' ; calculate p'
;                    q' ; calculate q'
;                    (div count 2)))
;         (else (fib-iter (+ (* b q) (* a q) (* a p))
;                         (+ (* b p) (* a q))
;                         p
;                         q
;                         (- count 1)))))
; -----------------------------------------------------------------------------------------------------------------------------------
;
; (p_in, q_in) = (0, 1)
; T = (p, q)
; Transformation :
; a <- aq + bq + ap - (1)
; b <- bp + aq - (2)
;
; on applyting the transformation twice and grouping terms we get:
; a <- a(q^2 + 2pq) + b(q^2 + 2pq) + a(p^2 + q^2) - (4)
; b <- b(p^2 + q^2) + a(q^2 + 2pq) - (5)
;
; (4) and (5) implies that we can choose p' and q';
; p' = p^2 + q^2
; q' = q^2 + 2pq
; such that we can apply the same transformation as (1) and (2)
; using (p', q') instead of (p, q).
; T^2 = (p', q')

(define (fib n)
  (define (fib-iter a b p q n)
    (cond ((= n 0) b)
          ((even? n)
           (fib-iter a
                     b
                     (+ (* p p) (* q q))
                     (+ (* q q) (* 2 p q))
                     (/ n 2)))
          (else
           (fib-iter (+ (* a q) (* b q) (* a p))
                     (+ (* b p) (* a q))
                     p
                     q
                     (- n 1)))))
  (define (even? n)
    (= (remainder n 2) 0))
  (fib-iter 1 0 0 1 n))
