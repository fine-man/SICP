#lang sicp

; Solution to exercise 1.37

; Helper Procedures
(define (one x) 1.0)

; Linear Iterative continued fraction procedure
(define (cont-frac n d k)
  (define (iter i result)
    (cond ((= i 0) result)
          (else
           (iter (- i 1) (/ (n i) (+ (d i) result))))))
  (iter (- k 1) (/ (n k) (d k))))


; Linear Recursive continued fraction procedure
(define (cont-frac-recur n d k)
  (define (recurse i)
    (cond ((= i k) (/ (n k) (d k)))
          (else (/ (n k)
                   (+ (d k) (recurse (+ i 1)))))))
  (recurse 1))


; actual value of 1/phi = 0.618033988749895

; By manually applying Binary search we find that we get 4 digits of
; accuracy at just k = 11
;
; > (cont-frac one one 50)
; 0.6180339887498948
; > (cont-frac one one 25)
; 0.6180339887802426
; > (cont-frac one one 12)
; 0.6180257510729613
; > (cont-frac one one 6)
; 0.6153846153846154
; > (cont-frac one one 9)
; 0.6181818181818182
; > (cont-frac one one 10)
; 0.6179775280898876
; > (cont-frac one one 11)
; 0.6180555555555556


