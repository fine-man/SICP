; Implementing Euclid Algorithm for finding the gcd of two numbers 'a' and 'b'
; observation : gcd(a, b) = gcd(b, r) where r is the remainder(a, b)
;
(define (euclid-gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
