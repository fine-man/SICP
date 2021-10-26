#lang racket

; solution to exercise 2.55 of SICP
; http://community.schemewiki.org/?sicp-ex-2.55

; Evaluation of (car ''abracadabra)

 (car ''abracadabra)
; The above is treated by the interpreter as :
; (car (quote (quote abracadabra)))

; if we replace the first "quote" with "'" :
; (car '(quote abracadabra))

; now we can see what's really happening here
; and we are basically making a list (quote abracadabra)
; 
; in the end evaluating the expression we get : 
; 'quote
