#lang racket

(require "utils.ss")
(require racket/include)
(require rackunit)

;;-------------------------------------------------------------------------------------------
;; Fundamental objects
;;-------------------------------------------------------------------------------------------


(struct/provide point (x y z) #:transparent)
(struct/provide 2d-point (x y) #:transparent)
(struct/provide edge (from to) #:transparent)
(struct/provide face (points) #:transparent)

;;-------------------------------------------------------------------------------------------
;; 2d objects
;;-------------------------------------------------------------------------------------------

(struct/provide circle-object (diameter) #:transparent)
(struct/provide rectangle-object (x y center) #:transparent)
(struct/provide polygon-object (points paths) #:transparent)
(struct/provide text-object (string size font halign valign spacing direction language script) #:transparent)
(define/provide (2d-object? x)
  (ormap (λ (f) (f x)) (list circle-object? rectangle-object? polygon-object? text-object?)))

;;-------------------------------------------------------------------------------------------
;; 3d objects
;;-------------------------------------------------------------------------------------------

(struct/provide sphere-object (diameter) #:transparent)
(struct/provide cuboid-object (width depth height center) #:transparent)
(struct/provide conic-object  (height base-diameter top-diameter center) #:transparent)
(struct/provide polyhedron-object (points faces convexity) #:transparent)
(define/provide (3d-object? x)
  (ormap (λ (f) (f x)) (list sphere-object? cuboid-object? conic-object? polyhedron-object?)))

;;-------------------------------------------------------------------------------------------
;; File ops
;;-------------------------------------------------------------------------------------------

(struct/provide import-object (file) #:transparent)
(struct/provide height-map-transform (file objects center convexity) #:transparent)
(define/provide (height-map-with-args file objects #:center [center #f] #:convexity [convexity 10])
  (height-map-transform file objects center convexity))
(define/provide import? import-object?)
(define/provide height-map? height-map-transform?)

;;-------------------------------------------------------------------------------------------
;; Global parameter modifications $fa, $fs and $fn
;;-------------------------------------------------------------------------------------------
(struct/provide parameters-transform (params objs) #:transparent)
(define/provide parameters? parameters-transform?)

;;-------------------------------------------------------------------------------------------
;; 3d->2d
;;-------------------------------------------------------------------------------------------

(struct/provide project-transform (cut objects) #:transparent)
(define/provide projection? project-transform?)

;;-------------------------------------------------------------------------------------------
;; 2d->3d
;;-------------------------------------------------------------------------------------------

(struct/provide linear-extrude-transform (objects height center convexity twist slices scale) #:transparent)
(struct/provide rotate-extrude-transform (objects angle convexity) #:transparent)
(define/provide (linear-extrude-with-args objs
                                          #:height height
                                          #:center [center #f]
                                          #:convexity [convexity 10]
                                          #:twist [twist 0]
                                          #:slices [slices 20]
                                          #:scale [scale 1.0])
  (linear-extrude-transform objs height center convexity twist slices scale))
(define/provide (rotate-extrude-with-args objs
                                          #:angle [angle 360]
                                          #:convexity [convexity 10])
  (rotate-extrude-transform objs angle convexity))
(define/provide (extrude? x) (or (linear-extrude-transform? x) (rotate-extrude-transform? x)))

;;-------------------------------------------------------------------------------------------
;; 3d->3d transformations
;;-------------------------------------------------------------------------------------------

(struct/provide basic-transform (name x y z objects) #:transparent)
(struct/provide basic-color-transform (r g b alpha objects) #:transparent)
(struct/provide offset-transform (radius depth chamfer objects) #:transparent)
(struct/provide affine-transform (matrix objects) #:transparent)
(define/provide (transform? x)
  (ormap (λ (f) (f x)) (list basic-transform? basic-color-transform? offset-transform? affine-transform?)))

;;-------------------------------------------------------------------------------------------
;; Set operations
;;-------------------------------------------------------------------------------------------

(struct/provide set-operation-object (op objects) #:transparent)
(define/provide set-operation? set-operation-object?)

(include "tests/parse-tree-test.ss")
