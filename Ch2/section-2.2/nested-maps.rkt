#lang racket

; Helper Procedures

;; ---------------------------------------------------------------------------------

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

(define n 5)
  
;;------------------------------------------------------------------------------------

; Problem : Given a positive integer n, find all ordered pairs of distinct positive
; integers i and j, where 1 <= j < i <= n, such that i + j is a prime

; Solution :
; 1. first we try to make a list of all the unique-pairs 'i', 'j' s.t - 1 <= j < i <= n
; 2. we filter all the pairs such that i + j = prime
; 3. we make another list with all the pairs found in 2nd along with their respective sums


;1. Making unique pairs, we can generate the unique pairs using the following procedure:
; (accumulate append nil (map (lambda (i)
;                    (map (lambda (j) (list i j))
;                         (enumerate-interval 1 (- i 1))))
;                  (enumerate-interval 1 n)))

; Becuase the above kind of procedure is used very often, so we make a general procedure after it:
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; finding unique pairs in terms of 'flatmap'
; (flatmap (lambda (i)
;            (map (lambda (j) (list i j))
;                 (enumerate-interval 1 (- i 1))))
;          (enumerate-interval 1 n))

;2. Procedure to find if a pair is prime or not
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;3. Procedure that takes a pair (i, j) and returns the pair (i, j, i + j)
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;4. Putting all the above together
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j) (list i j))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

;; TEST
; (prime-sum-pairs 5)
; '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7))

;; Problem 2 : finding all the permuantations of a set S
; Solution : for each item x in S, recursively generate the sequence of permuantations
; of S - x, and adjoin x in front of each one.

; Procedure to find permuantations of a set S
(define (permuantations S)
  (if (null? S)
      (list nil)
      (flatmap (lambda (element)
                 (map (lambda (sub-perm)
                        (cons element sub-perm))
                      (permuantations (remove element S))))
               S)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item))) sequence))

;; TEST
; (permuantations (list 1 2 3))
; '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
