#lang racket

; section 2.3.1 : Quotation

; All the compound data objects we used so far were constructed
; ultimately from numbers. To extend the representational capability
; of our language we need the ability to work with arbritrary symbol as data

; In order to manipulate symbols we need a new element in our language:
; The ability to quote an object lets us treat that object as data

; suppose we want to construct the list (a b). we can't do (list a b)
; as that will construct a list containing the values of 'a' and 'b'
; since the evaluation model of scheme is such that the arguments are
; evaluated before they are passed

; This same issue is well known in the context of natural language :
; "Say your name out load" and "say 'your name' out load" are two different things
; Just as we solve this problem in natural language using quotation, we can
; do the same for scheme to make lists such as (a b)

; now we can distinguish between symbols and their values :
(define a 1)
(define b 2)

(list a b) ;(1 2)
(list 'a 'b) ;(a b)
(list 'a b) ; (a 2)

; Strictly, our use of the quotation mark violates the general rule that all compound
; expressions in our language should be delimited by parentheses and look like lists. We
; can recover this consistency by introducing a special form quote, which serves the
; same purpose as the quotation mark.Thus, we would type (quote a) instead of 'a,
; general form of a quote procedure : (quote <expression>)

(quote (a b c)) ; '(a b c)

; good example : 
; (list 'car (list 'quote '(a b c)))
; (list 'car (quote '(a b c)))
; (list 'car ''(a b c))
; (car '(a b c))


; Memq procedure
(define (memq item lis)
  (cond ((null? lis) false)
        ((eq? item (car lis)) lis)
        ; eq? is a procedure that two symbols as arguments and test whether they are same
        (else (memq item (cdr lis)))))

(memq 'apple '(pear banana prune)) ; #f
(memq 'apple '(x (apple sauce) y apple pear)) ; '(apple pear)

