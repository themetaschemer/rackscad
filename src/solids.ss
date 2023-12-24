#lang racket

(require "utils.ss")
(require "rackscad.ss")

;;-------------------------------------------------------------------------------------------
;; Solids have a shape and properties to go with it. 
;;-------------------------------------------------------------------------------------------

(struct solid* (properties))

(define-syntax (define-solid x)
  (syntax-case x ()
    [(_ (name arg ...) (key value) ...)
     #'(define (name arg ...)
         (let ([key value] ...)
           (validate-solid 'name
            `((key . ,key) ...))))]))

(define-syntax (solid x)
  (syntax-case x ()
    [(_ ((key value) ...))
     #'(let ([key value] ...)
         (validate-solid 'anonymous
            `((key . ,key) ...)))]))

(define (solid-shape s)
  (and (solid*? s)
       (or (dict-ref (solid*-properties s) 'shape #f)
           (shape-from-box s)
           (error 'solid-shape "Solid has no shape: ~a" s))))

(define (shape-from-box s)
  (let ([box (dict-ref (solid*-properties s) 'box #f)])
    (match box
      [`(,w ,d ,h) (cuboid #:x w #:y d #:z h)]
      [else #f])))

(define (solid-cutout s)
  (let ([cutout (dict-ref (solid*-properties s) 'cutout #f)])
    (match cutout
      [`(,w ,d, h) (cuboid #:x w #:y d #:z h)]
      [else #f])))

(define (solid-width s)
  (let ([box (dict-ref (solid*-properties s) 'box #f)])
    (match box
      [`(,w ,d ,h) w]
      [else #f])))

(define (solid-depth s)
  (let ([box (dict-ref (solid*-properties s) 'box #f)])
    (match box
      [`(,w ,d ,h) d]
      [else #f])))

(define (solid-height s)
  (let ([box (dict-ref (solid*-properties s) 'box #f)])
    (match box
      [`(,w ,d ,h) h]
      [else #f])))

(define (validate-solid name properties)
  (or (and (dict-ref properties 'box #f)
           (solid* properties))
      (error 'define-solid "Solids must have a box or shape associated with it: ~a" name)))


;;-------------------------------------------------------------------------------------------
;; Solids can be cut out one from the other. 
;; The first solid is the one from which the second solid will be cut
;;
;; There are positions where the cutting box will be placed:
;; x-axis: center, left, right, center-left, center-right
;; y-axis: center, front, back, center-front, center-back
;; z-axis: center, bottom, top, center-bottom, center-top
;;
;; center is the exact midpoint
;; center-D means align the center of the second object to the 1/4 point 
;;   depending upon D as follows: 
;;    left == quarter point to the left of the center of the x-axis of the first object
;;    right == quarter point to the right of the center of the x-axis of the first object
;;    front == quarter point to the front of the center of the y-axis of the first object
;;    back == quarter point to the back of the center of the y-axis of the first object
;;    bottom == quarter point to the bottom of the center of the z-axis of the first object
;;    top == quarter point to the top of the center of the z-axis of the first object
;;
;;

(define (cut-from anchor movable #:at [at '()] #:new-bounding-box [new-bb #f])
  (let* ([new-bb (or new-bb anchor)]
         [width  (solid-width new-bb)]
         [depth  (solid-depth new-bb)]
         [height (solid-height new-bb)])
    (solid ([box `(,width ,depth ,height)]
            [shape 
             (difference anchor 
                         (translate ((x-align at anchor movable)
                                     (y-align at anchor movable)
                                     (z-align at anchor movable))
                                    movable))]
            [components (list anchor movable)]))))


;;-------------------------------------------------------------------------------------------
;; Solids can be rotated by a given angle around a given axis.
;;
;; Rotating a solid changes its bounding box (which is dependent upon maximum extent in each
;; direction.
;;

(define (turn #:axis axis
              #:clockwise [clockwise #f]
              #:anti-clockwise [anti-clockwise #f]
              object)
  (let ([angle (if clockwise (- clockwise (* 2 pi)) anti-clockwise)]
        [w (solid-width object)]
        [d (solid-depth object)]
        [h (solid-height object)])
    (match axis
      ['z (solid ([box `(,(+ (* w (cos angle)) (* d (sin angle)))
                         ,(+ (* w (sin angle)) (* d (cos angle)))
                         ,h)]
                  [shape (rotate (0 0 angle) object)]
                  [components (list object)]))]
      ['y (solid ([box `(,(+ (* w (cos angle)) (* h (sin angle)))
                         ,h
                         ,(+ (* w (sin angle)) (* h (cos angle))))]
                  [shape (rotate (0 angle 0) object)]
                  [components (list object)]))]
      ['x (solid ([box `(,w
                         ,(+ (* d (cos angle)) (* h (sin angle)))
                         ,(+ (* d (sin angle)) (* h (cos angle))))]
                  [shape (rotate (0 0 angle) object)]
                  [components (list object)]))])))
    

;;-------------------------------------------------------------------------------------------

;; front-solid will be stacked in front of the back-solid
;; This stacking is along the Y axis.
;; The alignment list says how to align the two solids along the Z and X axes.


;;-------------------------------------------------------------------------------------------
;; Solids can be stacked.
;;
;; For algebraically composing solids, we have the following invariants.
;;   Origin of each object is in the center of its bounding box.
;; When two things are stacked, The new composite bounding box is computed.
;; Each of the things is then translated based on the alignments to achieve the stacking.

;; Alignments along each axis are as follows:
;; X: one of the two: right left. If neither is provided center is assumed. 
;; Y: one of the two: front back. If neither is provided center is assumed. 
;; Z: one of the two: top bottom. If neither is provided center is assumed. 

;;-------------------------------------------------------------------------------------------

;; front-solid will be stacked in front of the back-solid
;; This stacking is along the Y axis.
;; The alignment list says how to align the two solids along the Z and X axes.

(define (stack-front #:align alignments
                     back-solid
                     front-solid)
  (let ([width  (max (solid-width back-solid) (solid-width front-solid))]
        [depth  (+ (solid-depth back-solid) (solid-depth front-solid))]
        [height (max (solid-height back-solid) (solid-height front-solid))])
    (solid ([box `(,width ,depth ,height)]
            [shape (stack-front-shape alignments back-solid front-solid)]
            [components (list back-solid front-solid)]))))

(define (stack-right #:align alignments
                     left-solid
                     right-solid)
  (let ([width  (+ (solid-width left-solid) (solid-width right-solid))]
        [depth  (max (solid-depth left-solid) (solid-depth right-solid))]
        [height (max (solid-height left-solid) (solid-height right-solid))])
    (solid ([box `(,width ,depth ,height)]
            [shape (stack-right-shape alignments left-solid right-solid)]
            [components (list left-solid right-solid)]))))

(define (stack-top #:align alignments
                     bottom-solid
                     top-solid)
  (let ([width  (max (solid-width bottom-solid) (solid-width top-solid))]
        [depth  (max (solid-depth bottom-solid) (solid-depth top-solid))]
        [height (+ (solid-height bottom-solid) (solid-height top-solid))])
    (solid ([box `(,width ,depth ,height)]
            [shape (stack-top-shape alignments bottom-solid top-solid)]
            [components (list bottom-solid top-solid)]))))

(define (stack-front-shape alignments back front)
  (let* ([d-back (solid-depth back)]
         [d-front (solid-depth front)]
         [tx-d-back (- (* 0.5 d-front))]
         [tx-d-front (* 0.5 d-back)])
    (objects
     (translate (0 tx-d-back 0) back)
     (translate
      ((x-align alignments back front) tx-d-front (z-align alignments back front))
      front))))

(define (stack-right-shape alignments left right)
  (let* ([w-left (solid-width left)]
         [w-right (solid-width right)]
         [tx-w-left (- (* 0.5 w-right))]
         [tx-w-right (* 0.5 w-left)])
    (objects
     (translate (tx-w-left 0 0) left)
     (translate
      (tx-w-right (y-align alignments left right) (z-align alignments left right))
      right))))

(define (stack-top-shape alignments bottom top)
  (let* ([h-bottom (solid-width bottom)]
         [h-top (solid-width top)]
         [tx-h-bottom (- (* 0.5 h-top))]
         [tx-h-top (* 0.5 h-bottom)])
    (objects
     (translate (0 0 tx-h-bottom) bottom)
     (translate
      ((x-align alignments bottom top) (y-align alignments bottom top) tx-h-top)
      top))))

(define (x-align alignments anchor movable)
  ;; Move the movable object along the x-axis depending upon alignments
  (cond
   [(member 'left alignments) (/ (- (solid-width movable) (solid-width anchor)) 2)]
   [(member 'right alignments) (/ (- (solid-width anchor) (solid-width movable)) 2)]
   [else 0]))

(define (y-align alignments anchor movable)
  ;; Move the movable object along the x-axis depending upon alignments
  (cond
   [(member 'top alignments) (/ (- (solid-depth movable) (solid-depth anchor)) 2)]
   [(member 'botton alignments) (/ (- (solid-depth anchor) (solid-depth movable)) 2)]
   [else 0]))

(define (z-align alignments anchor movable)
  ;; Move the movable object along the x-axis depending upon alignments
  (cond
   [(member 'top alignments) (/ (- (solid-height movable) (solid-height anchor)) 2)]
   [(member 'botton alignments) (/ (- (solid-height anchor) (solid-height movable)) 2)]
   [else 0]))
  
(provide (all-defined-out))
