#lang racket

; Solution to exercise 2.70 of SICP
; http://community.schemewiki.org/?sicp-ex-2.70

; For implementation of Huffman Trees
(require "huffman-trees.rkt")

; In this question we encode the 1950s rock song
; "Get A job" by The SILHOUETTES
; https://www.youtube.com/watch?v=ysKhbaLyIFw

(define sym-pairs '((a 2) (na 16) (boom 1) (sha 3) (get 2) (yip 9) (job 2) (wah 1)))

(define rocktree (generate-huffman-tree sym-pairs))

(display rocktree)
#|
((leaf na 16) 
((leaf yip 9) (((leaf a 2) ((leaf wah 1) (leaf boom 1) (wah boom) 2) (a wah boom) 4) ((leaf sha 3) ((leaf job 2) (leaf get 2) (job get) 4) (sha job get) 7) (a wah boom sha job get) 11) (yip a wah boom sha job get) 20) 
(na yip a wah boom sha job get) 
36) 
|#

(define rock-song '(
                  get a job sha na na na na na na na na
                  get a job sha na na na na na na na na
                  wah yip yip yip yip yip yip yip yip yip
                  sha boom
                  )) 

(define encoded-rock-song (encode rock-song rocktree))
#|
encoded-rock-song
'(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1
    1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)
|#

(length encoded-rock-song)
; 84

;; If we were to use fixed-length code then we would need 3 bits symbol, so
; the length of the encoded rock song would have been
(* 3 (length rock-song))
; 108, which is about 22% more than the previous one
