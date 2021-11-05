#lang racket

; solution of exercise 2.60 of SICP
; http://community.schemewiki.org/?sicp-ex-2.60

;; Helper procedure 
; equal Procedure from exercise 2.54
(define (equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        ((and (pair? l1) (pair? l2))
         (and (equal? (car l1) (car l2))
              (equal? (cdr l1) (cdr l2))))
        (else (eq? l1 l2))))

; element-of-set procedure - O(n)
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; adjoin-set procedure - O(1)
(define (adjoin-set x set)
  (cons x set))

; union-set procedure - O(n + m)
(define (union-set set1 set2)
  (append set1 set2))

; Procedure that removes the first occurence of an element from set - O(n)
(define (remove-set-element x set)
  (define (iter res set)
    (cond ((null? set) res)
          ((equal? x (car set))
           (append res (cdr set)))
          (else (iter (cons (car set) res) (cdr set)))))
  (iter '() set))

; intersection of set procedure - O(n^2)
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1)
                                 (remove-set-element (car set1) set2))))
        (else (intersection-set (cdr set1) set2))))

;; Test
(intersection-set '(1 1 1 2 2) '(1 1 2 2 2 3 4 4))
; '(1 1 2 2)

(element-of-set? 1 '(1 1 1 2 2))
; #t

(adjoin-set 1 '(1 1 1 2 20))
; '(1 1 1 1 2 20)

(union-set '(1 1 1 2) '(1 2 3 1))
; '(1 1 1 2 1 2 3 1)

#| Conclusion :
Becuase we are now allowing duplicates, we can simplify the
adjoin-set procedure to just add the element without checking
reducing the complexity from O(n) to O(1). Similarly for the
union-set procedure, we can just append one set to another without
checking making the new complexity O(n + m) instead of O(n * m).
The time complexity of element-of-set? and intersection-set remains the same

We would prefer this representation over the non-duplicate one when we
need to perform multiple adjoin-set and union-set and not many element-of-set?
and intersection-set procedures as in the non-duplicate representation
adjoin-set and union-set are more expensive.

|#









