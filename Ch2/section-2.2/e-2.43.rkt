#lang racket
(require "./e-2.42.rkt")
; Procedure that gives a list of all possible solutions to the
; queen problem of board size n
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


; Louis's implementation
(define (queens-louis board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

; Analysis of original implementation of "queens"
; For a board-size = n, let T_1(i) be the time taken by (queen-cols i)
; For the sake of simplicity, we will also define another function:
; Q(i) = length of the list returned by (queen-cols i) for board-size = n
; in Scheme we can define Q(i) = (length (queen-cols i))
;
; Base Cases :
; T_1(0) = 1, as we will return the answer in constant time 
; Q(0) = 1, as list have one element which the is the 'nil' one
;
; we can write T_1(i) recursively as :
; T_1(i) = T_1(i - 1) + n * Q(i - 1)[enumerate] + n * i * Q(i - 1)[safe?]
; The first term comes because of the recursive call (queen-cols (- i 1)) 
; The second term comes because we enumerate over Q(i - 1) n/board-size times
; The third term comes from the safe? procedure which checks every-pair in the "positions" list
; solving for T_1(n) by summing over T_1(i): 
;
; T_1(n) = T_1(n - 1) + n * Q(n - 1) + n * n * Q(n - 1)
; T_1(n - 1) = T_1(n - 2) + n * Q(n - 2) + (n - 1) * n * Q(n - 2)
; ...
; ...
; T_1(1) = T_1(0) + n * Q(0) + 1 * n * Q(0)
; adding all the above, we get :
; T_1(n) = n + 1 + sigma{i from 0 to n - 1}(n * Q(i)) + sigma{i from 0 to n - 1}(n * (i + 1) * Q(i))
; as the second term is the sequence is dominated by the 3rd one
; so we can ignore the 2nd term and write it as :
; T_1(n) = n + sigma{i from 0 to n - 1}(n * (i + 1) * Q(i))


; Analysis of Louis's implementation of queens
; Let T_2(i) = time taken by (queen-cols i) with board-size = n
; Base cases will be same as T_1(i)

; writing T_2(i) recursively:
; T_2(i) = n * T_2(i - 1) + n * Q(i - 1)[enumerate] + n * i * Q(i - 1)[safe?]
; First term comes n times because in this implementation we call (queen-cols (- i 1)) n times
; second and third term is same as T_1(i)
;
; Solving for T_2(n) by summing over (n ^ (n - i)) * T_2(i):
; T_2(n) = n * T_2(n - 1) + n * Q(n - 1) + n * n * Q(n - 1)
; n * T_2(n - 1) = n * n * T_2(n - 1) + n^2 * Q(n - 1) + n^2 * (n - 1) * Q(n - 1)
; ...
; ...
; n^(n - 1) * T_2(1) = (n^n) * T_2(0) + (n^n) * Q(0) + (n^n) * 1 * Q(0)
; adding all the above, we get :
; T_2(n) = n^n + sigma{i from 0 to n - 1}(n^(n - i) * Q(i)) + sigma{i from 0 to n - 1}(n^(n - i) * (i + 1) * Q(i))
; as the second term is the sequence is dominated by the 3rd one
; so we can ignore the 2nd term and write it as :
; T_2(n) = n^n + sigma{i from 0 to n - 1}(n^(n - i) * (i + 1) * Q(i))
;
;
; After looking at T_1(n) and T_2(n), we can say that
; T_2(n)/T_1(n) = n^n/n approximately which becomes n^n for bigger values of n
; so T_2(n)/T_2(n) = n^n approximately (more of an upper-bound)



