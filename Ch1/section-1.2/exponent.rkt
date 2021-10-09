; Linear Recursive process
(define (exponent a n)
  (if (= n 0)
      1
      (* a (exponent a (- n 1)))))

; Linear Iterative Process
(define (expo a n)
  (define (expo-iter product counter)
    (if (= counter 0)
        product
        (expo-iter (* a product) (- counter 1))))
  (expo-iter 1 n))

; Logrithmic Recursive Process
(define (fast-exp a n)
  (define (square x)
    (* x x))
  (define (even? x)
    (= (remainder x 2) 0))
  (cond ((= n 0) 1)
        ((even? n)
         (square (fast-exp a (/ n 2))))
        (else (* a (fast-exp a (- n 1))))))

