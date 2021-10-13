#lang racket

; solution to exercise 2.40 of SICP
; http://community.schemewiki.org/?sicp-ex-2.40

; Helper Procedures
;; ----------------------------------------------------------------

; Prime Test
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

; nil
(define nil '())

; Procedure to append one list to another
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
            (append (cdr l1) l2))))

; enumerating integers
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

; Procedure to map the elements of a list
(define (map proc lis)
  (if (null? lis)
      lis
      (cons (proc (car lis))
            (map proc (cdr lis)))))
; Filtering a list
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

; Accumulating elements of a list
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;;------------------------------------------------------------------------

; Flatmap Procedure
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; Procedure to make unique pairs (i, j) such that 1 <= j < i <= n
(define (unique-pairs n)
  (flatmap (lambda (i)
            (map (lambda (j) (list i j))
                 (enumerate-interval 1 (- i 1))))
          (enumerate-interval 1 n)))

;2. Procedure to find if a pair is prime or not
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;3. Procedure that takes a pair (i, j) and returns the pair (i, j, i + j)
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

; prime-sum-pairs defined with unique-pairs procedure
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;; TEST
(unique-pairs 5)
;'((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))

(prime-sum-pairs 5)
; '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7))

