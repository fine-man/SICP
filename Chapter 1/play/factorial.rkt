; Factorial Procedure

; Recursive procedure
(define (fac n)
  (if (= n 1)
      1
      (* n (fac (- n 1)))))

; iterative procedure

(define (fact n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-cnt)
  (if (> counter max-cnt)
      product
      (fact-iter (* counter product) (+ counter 1) max-cnt)))


; iterative with block-structure
(define (factorial n)
  (define (fac-iter product counter)
    (if (> counter n)
        product
        (fac-iter (* counter product)
                  (+ counter 1))))
  (fac-iter 1 1))
