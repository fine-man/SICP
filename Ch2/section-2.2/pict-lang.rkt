#lang racket

; Picture language is an interesting example of data abstraction,
; closure property and higher order functions
;
; It is based on manipulating images in order to make more complex
; patterns by transforming images and then combining those different
; images in complex ways using other procedures.
;
; The language only uses a single primitive called painter(described below)
; using which we can make primitive and then complex procedures to
; manipulate images

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
; Definition : A frame basically acts as a boundary of the image we are
; showing, it consists of three vectors - origin vector which specifies
; the offset of the frame's origin from some absolute origin in the plain,
; and 2 edge vectors specifiy the offset of the frame's corner from it's origin
;
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
;
; Note : Although SICP uses the above definition of a painter, the package
; "sicp-pict" that is used here does not allow frames to be given as
; arguments to painters.

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

#| implementation of transform-painter
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))
|#


;; PAINTER PROCEDURES
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (squash-center painter)
  (transform-painter painter
                     (make-vect (/ 1 3) (/ 1 3))
                     (make-vect (/ 2 3) (/ 1 3))
                     (make-vect (/ 1 3) (/ 2 3))))


#| implementation of beside procedure
(define (beside painter1 paiter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            split-point
            (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))
|#

;; TEST
; (define vec1 (make-vect 1 2))
; (define vec2 (make-vect 2 1))
; (define vec3 (make-vect 1 1))
; (define zero-vec (make-vect 0 0))

; (define frame (make-frame vec3 vec1 vec2))

; ((frame-coord-map frame) zero-vec)
; (vect 1 1)


;; Levels of Language for robust design
; The picture language implements some of the critical ideas that
; were introduced, namely - data abstraction with procedures and data
;
; 1. The fundamental data abstraction, painter, are implemented using
; procedural representation, enabling the language to handle different
; basic drawing capabilites in a unique way
;
; 2. The means of combination satisfy the closure property, which
; permits us to easily build up complex designs.
;
; 3. All the tools required for abstracting procedures are present,
; which can be used for abstracting means of combination of painters.
;
;
; Stratified design : It is the design philosophy that a complex system
; should be structured as a sequence of levels that are described using 
; a sequence of languages.
; Each level is constructed by combining parts that are regarded as primitives
; at that level, and the parts constructed at each level are used as 
; primitives at the next level. The language used at each level of a
; stratified design has primitives, means of combination and means of abstraction
; appropriate to that level of detail.
;
; Stratified design helps makes programs robust, that is, it makes it likely
; that small changes in a specification will require correspondingly small
; changes in the program


