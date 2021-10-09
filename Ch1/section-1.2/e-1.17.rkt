; Solution to exercise 1.17
; -------------------------------------
; Question :
; The exponentiation algorithms in this sec-
; tion are based on performing exponentiation by means of
; repeated multiplication. In a similar way, one can perform
; integer multiplication by means of repeated addition. The
; following multiplication procedure (in which it is assumed
; that our language can only add, not multiply) is analogous
; -----------------------------------------------------------
; to the expt procedure:
; (define (* a b)
;  (if (= b 0)
;      0
;      (+ a (* a (- b 1)))))
; -----------------------------------------------------------
; this algorithm takes a number of steps that is linear in b.
; Now suppose we include, together with addition, opera-
; tions double, which doubles an integer, and halve, which
; divides an (even) integer by 2. Using these, design a mul-
; tiplication procedure analogous to fast-expt that uses a
; logarithmic number of steps.

; Logrithmic Recurisive Process
(define (fast-prod a b)
  (define (even? x)
    (= (remainder x 2) 0))
  (cond ((= b 0) 0)
        ((even? b)
         (fast-prod (+ a a) (/ b 2)))
        (else (+ a (fast-prod a (- b 1))))))










  
















