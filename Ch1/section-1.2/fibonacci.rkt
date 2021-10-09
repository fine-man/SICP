; Fibonacci numbers

; Recursive Procedure for recursive process
(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1))
                 (fibonacci (- n 2))))))


; Recursive Procedure for linear iterative process
(define (fib n)
  (define (fib-iter a b counter)
    (if (= counter n)
        a
        (fib-iter b
                  (+ a b)
                  (+ counter 1))))
  (fib-iter 0 1 0))


