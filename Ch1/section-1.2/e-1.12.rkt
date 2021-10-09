; Solution to Problem 1.12
; The following pattern of numbers is called Pascal’s triangle.
; ------------------------------------------------------------
;         1
;       1   1
;     1   2   1
;   1   3   3   1 
; 1   4   6   4   1
; ------------------------------------------------------------
; The numbers at the edge of the triangle are all 1, and each
; number inside the triangle is the sum of the two numbers
; above it.Write a procedure that computes elements of
; Pascal’s triangle by means of a recursive process.


(define (pascal n r)
  (cond ((= r 0) 1)
        ((< (- n r) r) (pascal n (- n r)))
        (else (+ (pascal (- n 1) r)
                 (pascal (- n 1) (- r 1))))))


(define (display-pascal-tri n)
  (define (pascal-iter r)
    (cond ((= r n) ())
          (else
           (display-row r)
           (newline)
           (pascal-iter (+ r 1)))))
  (define (display-row r)
    (define (row-iter c)
      (cond ((> c r) ())
          (else
           (display (pascal r c))
           (display " ")
           (row-iter (+ c 1)))))
    (row-iter 0))     
  (pascal-iter 0))


      
    


