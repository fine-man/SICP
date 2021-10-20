#lang racket

(require sicp-pict)
(provide (combine-out (all-defined-out) (all-from-out sicp-pict)))
; https://docs.racket-lang.org/sicp-manual/SICP_Picture_Language.html

(define rogers (bitmap->painter "rogers.png"))
; (define wave2 (beside wave (flip-vert wave)))
; (define wave4 (below wave2 wave2))

; Flipped pairs procedure
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

; right-split procedure
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

; Up-split procedure
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))


; Corner-split = up(n - 1)|up(n - 1) | corner-split(n - 1)
;                ----------------------------------------------
;                    identity(n - 1) | right(n - 1)|right(n - 1)
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

; Square Limit = 4 corner-splits
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))


; An abstract procedure that takes 4 painter operations and produces
; a painter operation that transforms a given painter with those
; four operations and arranges the result in a sqaure.
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

#| flipped-pairs procedure defined using square of four general procedure

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                 identity flip-vert)))
    (combine4 painter)))

|#

#| Square limit procedure using the square-of-four procedure

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))
|#

;; VECTORS
; renaming the inbuilt procedures in sicp-pict package to fit SICP
; for internal implementation of vectors , check e-2.46.rkt
;
 (define make-vec make-vect)
; selector for x-coordinate of a vector
(define xcor-vect vector-xcor)

; selector for y-coordinate of a vector
(define ycor-vect vector-ycor)

; procedure to add 2 vectors
(define add-vect vector-add)

; procedure to subtract 2 vectors
(define sub-vect vector-sub)

; procedure to scale a vector
(define scale-vect vector-scale)


;; FRAMES
; renaming the inbuilt frame procedures in sicp-pict package
; for a possible internal implementation of the procedures, check e-2.47.rkt
;
; selector for origin of a frame
(define origin-frame frame-origin)

; selector for edge1 of a frame
(define edge1-frame frame-edge1)

; selector for edge2 of a frame
(define edge2-frame frame-edge2)

; Frame-coordinate procedure which is used to
; shift and scale images to fit a particular frame
; xi + yj -> origin(frame) + x * edge1(frame) + y * edge2(frame)
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))


;; SEGMENTS
; renaming the inbuilt procedures in sicp-pict package
; for a possible internal implementation of the procedures, check e-2.48.rkt
;
; selector for starting vector of a segment
(define start-segment segment-start)
;
; selector for ending vector of a segment
(define end-segment segment-end)


;; PAINTERS
; definition : A painter is represented as a procedure that, given a frame
; as argument, draws a particular image shifted and scaled to fit the frame.

; draw-line takes 2 vectors and draws a line btw them on the screen
(define (draw-line from to)
  (define lov (list from to))
  (define painter (vects->painter lov))
  (paint painter))

#| Procedure that makes a painter from a list of segments
; This procedure doesn't work tho for some reason
(define (segment->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))
|#

; Procedure that takes a painter and frame as an argument
; and returns a painter shifted to that particular frame
(define (change-painter painter frame)
  (transform-painter painter
                      (origin-frame frame)
                      (edge1-frame frame)
                      (edge2-frame frame)))

; Procedure that takes a list of segments and returns a procedure
; that takes a frame as an argument and returns a painter
; which can draw that list of segments shifted to that frame
(define (segment->painter segment-list)
  (lambda (frame)
    (paint
     (change-painter
      (segments->painter segment-list)
      frame))))

;; TEST
; (define vec1 (make-vect 1 2))
; (define vec2 (make-vect 2 1))
; (define vec3 (make-vect 1 1))
; (define zero-vec (make-vect 0 0))

; (define frame (make-frame vec3 vec1 vec2))

; ((frame-coord-map frame) zero-vec)
; (vect 1 1)
