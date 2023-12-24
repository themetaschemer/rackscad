#lang racket

(require "utils.ss")
(require "parse-tree.ss")
(require "svg-color.ss")
(require racket/include)
(require rackunit)

(define/provide (compile-expr expr)
  (cond
   [(transform? expr)  (compile-transform expr)]
   [(extrude? expr)    (compile-extrude expr)]
   [(projection? expr) (compile-projection expr)]
   [(parameters? expr) (compile-parameters expr)]
   [(height-map? expr) (compile-height-map expr)]
   [(object? expr)     (compile-object expr)]
   [(list? expr)       (compile-group expr)]))


(define (compile-transform expr)
  (match expr
   [(basic-transform name x y z objects)
    (with-current-indent "~a([~a, ~a, ~a]) ~a" 
                         (compile-component name)
                         (compile-component x)
                         (compile-component y)
                         (compile-component z)
                         (compile-expr objects))] 
   [(basic-color-transform r g b alpha objects)
    (with-current-indent "color([~a, ~a, ~a, ~a]) ~a" 
                         (compile-component r)
                         (compile-component g)
                         (compile-component b)
                         (compile-component alpha)
                         (compile-expr objects))] 
   [(offset-transform radius depth chamfer objects)
    (cond
     [(and (not radius) (not depth))
      (error 'compile-transform "Offset transform must at least have radius or depth: ~a~%" expr)]
     [radius (with-current-indent "offset(r = ~a, chamfer = ~a) ~a" 
                                  (compile-component radius)
                                  (compile-component chamfer)
                                  (compile-expr objects))]
     [depth (with-current-indent "offset(d = ~a, chamfer = ~a) ~a" 
                                  (compile-component depth)
                                  (compile-component chamfer)
                                  (compile-expr objects))])]
   [(affine-transform matrix objects)
    (with-current-indent "multmatrix(~a) ~a" (compile-affine-matrix matrix)  (compile-expr objects))]
   [else (error 'compile-transform "Unknown transform: ~a~%" expr)]))

(define (compile-extrude expr)
  (match expr
    [(linear-extrude-transform objects height center convexity twist slices scale)
     (with-current-indent "linear_extrude(height = ~a, center = ~a, convexity = ~a, twist = ~a, slices = ~a, scale = ~a) ~a"
                          (compile-component height)
                          (compile-component center)
                          (compile-component convexity)
                          (compile-component twist)
                          (compile-component slices)
                          (compile-component scale)
                          (compile-expr objects))]
    [(rotate-extrude-transform objects angle convexity)
     (with-current-indent "rotate_extrude(angle = ~a, convexity = ~a) ~a"
                          (compile-component angle)
                          (compile-component convexity)
                          (compile-expr objects))]
    [else (error 'compile-extrude "Unknown extrude transform: ~a~%" expr)]))



(define (compile-projection expr)
  (match expr
    [(project-transform cut objects)
     (with-current-indent "projection(cut = ~a) ~a" (compile-component cut) (compile-expr objects))]
    [else (error 'compile-projection "Unknown projection transform: ~a~%" expr)]))

(define (compile-height-map expr)
  (match expr
    [(height-map-transform file objects center convexity)
     (with-current-indent "surface(file = ~a, center = ~a, convexity = ~a) ~a"
                          (compile-component file)
                          (compile-component center)
                          (compile-component convexity)
                          (compile-expr objects))]))

(define (compile-parameters expr)
  (match expr
    [(parameters-transform params objs)
     (with-current-indent 
      (++* (η "{\n")
           (with-indent-in 
            (apply ++* #:separator ";\n"
                   (append (map compile-parameter-assignment params)
                           (map compile-expr objs))))
           (η ";\n}")))]))

(define (object? expr)
  (or (2d-object? expr)
      (3d-object? expr)
      (set-operation? expr)
      (import? expr)))

(define (compile-object expr)
  (cond
   [(2d-object? expr)     (compile-2d-object expr)]
   [(3d-object? expr)     (compile-3d-object expr)]
   [(set-operation? expr) (compile-set-operation expr)]
   [(import? expr)        (compile-import expr)]))

(define (compile-set-operation expr)
  (match expr
    [(set-operation-object op objects)
     (with-current-indent "~a() ~a" (compile-component op) (compile-expr objects))]))

(define (compile-group exprs)
  (if (equal? 1 (length exprs))
      (++* (compile-expr (car exprs)) (η ";\n"))
      (with-current-indent 
       (η "{\n") 
       (with-indent-in
        (apply ++* #:separator ";\n" (map compile-expr exprs))) 
       (η ";\n}"))))

(define (compile-import expr)
  (match expr
    [(import-object file)
     (with-current-indent "import(~a)" (compile-component file))]))

(define (compile-2d-object obj)
  (match obj
    [(circle-object diameter)
     (with-current-indent "circle(d = ~a)" (compile-component diameter))]
    [(rectangle-object x y center)
     (with-current-indent "square([~a, ~a], ~a)" (compile-component x) (compile-component y) (compile-component center))]
    [(polygon-object points paths)
     (if (and paths (not (null? paths)))
         (with-current-indent "polygon(~a, ~a)" (as-vector points) (as-vector paths))
         (with-current-indent "polygon(~a)" (as-vector points)))]
    [(text-object string size font halign valign spacing direction language script)
     (with-current-indent
      "text(text = ~a, size = ~a, font = ~a, halign = ~a, valign = ~a, spacing = ~a, direction = ~a, language = ~a, script = ~a)"
      (compile-component string) (compile-component size) (compile-component font) 
      (compile-component halign) (compile-component valign) (compile-component spacing) 
      (compile-component direction) (compile-component language) (compile-component script))]
    [else (error 'compile-2d-object "Unknown 2d-object: ~a" obj)]))

(define (compile-3d-object obj)
  (match obj
    [(sphere-object diameter) (with-current-indent "sphere(d = ~a)" (compile-component diameter))]
    [(cuboid-object width depth height center)
     (with-current-indent "cube([~a, ~a, ~a], center = ~a)"
                          (compile-component width)
                          (compile-component depth)
                          (compile-component height)
                          (compile-component center))]
    [(conic-object height base-diameter top-diameter center)
     (with-current-indent "cylinder(~a, d1 = ~a, d2 = ~a, center = ~a)" 
                          (compile-component height)
                          (compile-component base-diameter)
                          (compile-component top-diameter)
                          (compile-component center))]
    [(polyhedron-object points faces convexity)
     (with-current-indent "polyhedron(points = ~a, faces = ~a, convexity = ~a)"
                          (as-vector points)
                          (as-vector faces)
                          (compile-component convexity))]
    [else (error 'compile-3d-object "Unknown 3d-object: ~a~%" obj)]))

;; Affine matrix is a list of list and has size 3x4
(define (compile-affine-matrix matrix)
  (ensure-affine-matrix matrix)
  (as-vector* (map as-vector matrix)))

(define (ensure-affine-matrix matrix)
  (or (and (list? matrix)
           (= (length matrix) 3)
           (andmap (λ (row) (and (list? row) (= (length row) 4))) matrix))
      (error 'ensure-affine-matrix "Not a 3d affine matrix: ~a~%" matrix)))


(define (component? expr)
  (or (equal? expr #t)
      (equal? expr #f)
      (point? expr)
      (2d-point? expr)
      (face? expr)
      (edge? expr)
      (number? expr)
      (string? expr)
      (list? expr)
      (symbol? expr)))

(define (compile-component obj)
  (match obj
    [#t             (η "true")]
    [#f             (η "false")]
    [(? integer?)   (η (format "~a" obj))]
    [(? number?)    (η (format "~a" (exact->inexact obj)))]
    [(? string?)    (η (format "~s" obj))]
    [(? symbol?)    (η (format "~a" obj))]
    [(? list?)      (as-vector obj)]
    [(point x y z)  (as-vector (list x y z))]
    [(2d-point x y) (as-vector (list x y))]
    [(face points)  (as-vector points)]
    [(edge from to) (as-vector (list from to))]))

(define (as-vector list)
  (++* (η "[") (apply ++* #:separator ", " (map compile-component list)) (η "]")))

(define (as-vector* list)
  (++* (η "[") (apply ++* #:separator ", " list) (η "]")))

(define (compile-parameter-assignment assgn)
  (with-current-indent
   (compile-component (car assgn))
   (η " = ")
   (compile-component (cadr assgn))))

;;---------------------------------------------------------------------------------
;; Monadic compilation
;;---------------------------------------------------------------------------------

;; The compilation monad carries around a port and the current indent level.
;; The monadic type expects a port and indent-level

;; The monadic type is: port x indent-level -> string , if port is #f
;;                      port x indent-level -> void ,   if port is valid

;; The code below is super cryptic.
;; the -m prefix indicates that value is of the monadic type.

;;---------------------------------------------------------------------------------
;; Basic monad
(define (η v)
  (λ (indent)
    (ensure-string v)))

(define (f* f v-m)
  (λ (indent-level)
    ((f (v-m indent-level)) indent-level)))

;;---------------------------------------------------------------------------------
;; String append in the monadic domain

(define (++-2* a-m b-m #:separator [sep ""])
  (λ (indent-level)
    ((f* (λ (a) (f* (λ (b) (η (++ a sep b))) b-m)) a-m) indent-level)))

(define (++* a-m #:separator [sep ""] . b-ms)
  (let loop ([b-ms b-ms]
             [res-m  a-m])
    (cond
     [(null? b-ms) res-m]
     [else (loop (cdr b-ms) (++-2* res-m (car b-ms) #:separator sep))])))

;;---------------------------------------------------------------------------------
;; String formatting in the monadic domain

(define-syntax (format-indent* x)
  (syntax-case x ()
    [(_ fmt-string (args ...))
     #'(λ (indent)
         (let ([str (add-indent-to-lines indent (format fmt-string args ...))])
           str))]
    [(_ fmt-string (args ...) arg-m arg-ms ...)
     (with-syntax ([v (datum->syntax #'_ (gensym))])
       #'(f* (λ (v) (format-indent* fmt-string (args ... v) arg-ms ...)) arg-m))]))

(define-syntax (append-indent* x)
  (syntax-case x ()
    [(_ (args ...))
     #'(λ (indent)
         (let ([str (add-indent-to-lines indent (++ args ...))])
           str))]
    [(_ (args ...) arg-m arg-ms ...)
     (with-syntax ([v (datum->syntax #'_ (gensym))])
       #'(f* (λ (v) (append-indent* (args ... v) arg-ms ...)) arg-m))]))

(define-syntax (with-current-indent x)
  (syntax-case x ()
    [(_ fmt-string args ...)
     (string? (syntax->datum #'fmt-string))
     #'(format-indent* fmt-string () args ...)]
    [(_ args ...)
     #'(append-indent* () args ...)]))

;;---------------------------------------------------------------------------------
;; Adjustment of indents

(define (with-indent-in x-m)
  (λ (indent) (x-m (++ indent "  "))))

(define (with-indent-out x-m)
  (λ (indent) (x-m (strip-one-indent indent))))
      
(define (add-indent-to-lines indent str)
  (apply string-append-with-separator "\n" 
         (map (λ (x) (if (equal? "" x) x (++ indent x))) (regexp-split #rx"(?m:\n^)" str))))

(define (strip-one-indent string)
  (match string
    [(regexp #px"^  (.*)$" (list _ remaining)) remaining]
    [else string]))

(include "tests/compile-test.ss")
