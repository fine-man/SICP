; Question :
; How many different
; ways can we make change of $1.00, given half-dollars, quarters,
; dimes, nickels, and pennies? More generally, can we write a procedure
; to compute the number of ways to change any given amount of money?

(define (count-ways-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+
               (cc amount (- kinds-of-coins 1))
               (cc (- amount (denominations kinds-of-coins))
                   kinds-of-coins)))))

(define (denominations kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
