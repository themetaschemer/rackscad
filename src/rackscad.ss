#lang racket/base

(require "utils.ss")
(require "parse-tree.ss")
(require "svg-color.ss")
(require "sys.ss")
(require "compile.ss")
(require (for-syntax racket/base))

;;-------------------------------------------------------------------------------------------
;; Programming interface
;;-------------------------------------------------------------------------------------------

(define/provide (compile target-filename objs ...)
  (with-ports ([target-port #:output-file target-filename #:exists 'truncate])
     ((compile-expr (list objs ...)) target-port "")))

(define/provide (render filename)
  (define-shell-command !openscad "openscad")
  (!openscad filename))

;;-------------------------------------------------------------------------------------------
;; Grouping
;;-------------------------------------------------------------------------------------------
(define/provide (objects . objs) objs)

;;-------------------------------------------------------------------------------------------
;; Set operations
;;-------------------------------------------------------------------------------------------
(define/provide (union . objs)
  (set-operation-object 'union objs))

(define/provide (intersection . objs)
  (set-operation-object 'intersection objs))

(define/provide (difference . objs)
  (set-operation-object 'difference objs))

(define/provide (hull . objs)
  (set-operation-object 'hull objs))

(define/provide (minkowski . objs)
  (set-operation-object 'minkowski objs))

;;-------------------------------------------------------------------------------------------
;; File readers
;;-------------------------------------------------------------------------------------------


(define-syntax/provide (import x)
  (syntax-case x ()
    [(_ file)
     #'(import-object file)]))


(define-syntax/provide (height-map x)
  (syntax-case x ()
    [(_ (args ...) file objs ...)
     #'(height-map-with-args  file (list objs ...) args ...)]))

;;-------------------------------------------------------------------------------------------
;; scope
;;-------------------------------------------------------------------------------------------

(define-syntax/provide (render-with x)
  (syntax-case x ()
    [(_ ([var val] ...) body ...)
     (let ([vars (map syntax->datum (syntax->datum #'(var ...)))])
       (equal? (remove* '(minimum-angle minimum-size number-of-fragments) vars) '()))
     (with-syntax ([(vname ...) (map get-vname (map syntax->datum (syntax->datum #'(var ...))))])
       #'(parameters-transform `(('vname ,val) ...)
                               (list body ...)))]))


;;-------------------------------------------------------------------------------------------
;; projection objects
;;-------------------------------------------------------------------------------------------

(define-syntax/provide (project x)
  (syntax-case x ()
    [(_ #:cut objs ...)
     #'(project-transform #t (list objs ...))]
    [(_ objs ...)
     #'(project-transform #f (list objs ...))]))

;;-------------------------------------------------------------------------------------------
;; extrude objects
;;-------------------------------------------------------------------------------------------

(define-syntax/provide (extrude x)
  (syntax-case x ()
    [(_ #:linear (args ...) objs ...)
     #'(linear-extrude-with-args (list objs ...) args ...)]
    [(_ #:rotational (args ...) objs ...)
     #'(rotate-extrude-with-args (list objs ...) args ...)]))

;;-------------------------------------------------------------------------------------------
;; Transform objects
;;-------------------------------------------------------------------------------------------

(define-syntax/provide (translate x)
  (syntax-case x ()
    [(_ (x y z) objs ...)
     #'(basic-transform 'translate x y z (list objs ...))]))

(define-syntax/provide (rotate x)
  (syntax-case x ()
    [(_ (x y z) objs ...)
     #'(basic-transform 'rotate x y z (list objs ...))]))

(define-syntax/provide (scale x)
  (syntax-case x ()
    [(_ (x y z) objs ...)
     #'(basic-transform 'scale x y z (list objs ...))]))

(define-syntax/provide (mirror x)
  (syntax-case x ()
    [(_ (x y z) objs ...)
     #'(basic-transform 'mirror x y z (list objs ...))]))

(define-syntax/provide (resize x)
  (syntax-case x ()
    [(_ (x y z) objs ...)
     #'(basic-transform 'resize x y z (list objs ...))]))

(define-syntax/provide (color x)
  (syntax-case x ()
    [(_ (r g b a) objs ...)
     #'(basic-color-transform r g b a (list objs ...))]
    [(_ (color-name alpha) objs ...)
     (string? (syntax->datum #'color-name))
     #'(let ([rgb (color-name->rgb color-name)])
         (when (not rgb)
               (error 'color "Unknown color name: ~a~%" color-name))
         (apply (λ (r g b) (basic-color-transform r g b alpha (list objs ...))) rgb))]
    [(_ color-name objs ...)
     (string? (syntax->datum #'color-name))
     #'(let ([rgb (color-name->rgb color-name)])
         (when (not rgb)
               (error 'color "Unknown color name: ~a~%" color-name))
         (apply (λ (r g b) (basic-color-transform r g b 1.0 (list objs ...))) rgb))]))

(define-syntax/provide (offset x)
  (syntax-case x ()
    [(_ #:radius r #:chamfer chamfer objs ...)
     #'(offset-transform r #f chamfer (list objs ...))]
    [(_ #:delta d #:chamfer chamfer objs ...)
     #'(offset-transform #f d chamfer (list objs ...))]
    [(_ #:radius r objs ...)
     #'(offset-transform r #f #f (list objs ...))]
    [(_ #:delta d objs ...)
     #'(offset-transform #f d #f (list objs ...))]))

(define-syntax/provide (affine x)
  (syntax-case x ()
    [(_ matrix objs ...)
     #'(affine-transform matrix (list objs ...))]))
  

;;-------------------------------------------------------------------------------------------
;; Basic 2d objects
;;-------------------------------------------------------------------------------------------

(define/provide (circle #:radius [r #f] #:diameter [d #f])
  (cond
   [(and (not r) (not d)) (error 'circle "Must provide at least a radius or a diameter")]
   [r (circle-object (* 2 r))]
   [d (circle-object d)]))

(define/provide (rectangle x y #:center [center #f])
  (rectangle-object x y center))

(define/provide (square x #:center [center #f])
  (rectangle-object x x center))

(define/provide (polygon #:points points  #:paths [paths #f])
  (polygon-object points paths))

(define/provide (text
                 string
                 #:size [size 10]
                 #:font [font "Helvetica Neue:style=Regular"]
                 #:halign [halign "left"] ;; center, right
                 #:valign [valign "baseline"] ;; top, center, bottom, 
                 #:spacing [spacing 1] ;; scales the spacing between letters 
                 #:direction [direction "ltr"] ;; ltr, rtl, ttb (top to bottom), btt (bottom to top)
                 #:language [language "en"]    ;; 2 letter locale
                 #:script [script "latin"])    ;; script of the text
  (text-object string size font halign valign spacing direction language script))


;;-------------------------------------------------------------------------------------------
;; Basic 3d objects
;;-------------------------------------------------------------------------------------------

(define/provide (sphere #:radius [r #f] #:diameter [d #f])
  (cond
   [(and (not r) (not d)) (error 'sphere "Must provide at least a radius or a diameter")]
   [r (sphere-object (* 2 r))]
   [d (sphere-object d)]))

(define/provide (cuboid #:width w #:depth d #:height h #:center center)
  (cuboid-object w d h center))

(define/provide (cube #:side s #:center center)
  (cuboid-object s s s center))

(define/provide (conic #:height h 
                       #:base-radius [br #f] 
                       #:base-diameter [bd #f] 
                       #:top-radius [tr #f] 
                       #:top-diameter [td 0] 
                       #:center [center #f])
  (when (and (not br) (not bd))
        (error 'conic "Must provide at least a base radius or base diameter"))
  (conic-object h
                (or (and br (* 2 br)) bd)
                (or (and tr (* 2 tr)) td) center))


(define/provide (cylinder #:height h #:radius [r #f] #:diameter [d #f] #:center [center #f])
  (let ([dia (or (and r (* 2 r)) d)])
    (conic-object h dia dia center)))

(define/provide (polyhedron #:points points #:faces faces #:convexity [convexity 10])
  (ensure-points points)
  (ensure-faces-have-points faces points)
  (polyhedron-object points faces convexity))


;;-------------------------------------------------------------------------------------------
;; Miscellaneous helpers
;;-------------------------------------------------------------------------------------------

(define (ensure-points points)
  (andmap point? points))

(define (ensure-faces-have-points faces points)
  (for/fold ([ans #t]) ([face faces])
    (andmap (λ (face-point) (member face-point points)) (face-points face))))

(define-for-syntax (get-vname var)
  (case var
    ((minimum-angle) '$fa)
    ((minimum-size) '$fs)
    ((number-of-fragments) '$fn)
    (else (error 'render-with "Syntax error: parameters must be one of minimum-angle, minimum-size number-of-fragments"))))

       

