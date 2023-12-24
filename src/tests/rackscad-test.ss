(define (test-render test-name . objects)
  (let ([target-filename (++ (getenv "TMPDIR") "/" test-name ".scad")])
    (apply compile target-filename objects)
    (render-png target-filename
                #:camera-at (point 20 -20 20)
                #:look-at   (point 0 0 0)
                #:png-width 600
                #:png-height 600)))

(module+ test
  (check-true (ensure-points `(,(point 1 2 3) ,(point 2 3 4))))
  (check-exn exn? (λ () (ensure-points (point 1 2 3))))
  (check-false (ensure-points `(,#t ,(point 2 3 4))))

  (check-true
   (ensure-faces-have-points (list (face '(0 1 2 0)) (face '(1 2 3 1)))
                             (list (point 0 1 2) (point 2 3 4) (point 3 4 5) (point 4 5 6))))

  (define (test-str . objects) ((compile-expr objects) ""))

  ;; 3d objects
  (check-equal? (test-str (sphere #:radius 3) (sphere #:diameter 3)) "{\n  sphere(d = 6);\n  sphere(d = 3);\n}")
  (check-equal? (test-str (cuboid #:x 3 #:y 4 #:z 5 #:center #t)) "cube([3, 4, 5], center = true);\n") 
  (check-equal? (test-str (cuboid #:x 3 #:y 4 #:z 5 #:center #f)) "cube([3, 4, 5], center = false);\n") 
  (check-equal? (test-str (cube #:side 3 #:center #f)) "cube([3, 3, 3], center = false);\n") 

  (check-equal? (test-str (cylinder #:height 3 #:radius 3)) "cylinder(3, d1 = 6, d2 = 6, center = false);\n") 
  (check-equal? (test-str (cylinder #:height 3 #:diameter 3)) "cylinder(3, d1 = 3, d2 = 3, center = false);\n") 
  (check-equal? (test-str (conic #:height 3 #:base-radius 6 #:top-radius 3)) "cylinder(3, d1 = 12, d2 = 6, center = false);\n") 
  (check-equal? (test-str (conic #:height 3 #:base-diameter 6 #:top-diameter 3)) "cylinder(3, d1 = 6, d2 = 3, center = false);\n")

  (define cube-points
    (list 
     (point  0  0  0 ) ;;0
     (point 10  0  0 ) ;;1
     (point 10  7  0 ) ;;2
     (point  0  7  0 ) ;;3
     (point  0  0  5 ) ;;4
     (point 10  0  5 ) ;;5
     (point 10  7  5 ) ;;6
     (point  0  7  5 ))) ;;7
  
  (define cube-faces (list 
                      (face (list 0 1 2 3)) ;; bottom
                      (face (list 4 5 1 0)) ;; front
                      (face (list 7 6 5 4)) ;; top
                      (face (list 5 6 2 1)) ;; right
                      (face (list 6 7 3 2)) ;; back
                      (face (list 7 4 0 3))))

  (check-equal? (test-str (polyhedron #:points cube-points 
                                      #:faces cube-faces))
                (++ "polyhedron(points = [[0, 0, 0], [10, 0, 0], [10, 7, 0],"
                    " [0, 7, 0], [0, 0, 5], [10, 0, 5], [10, 7, 5], [0, 7, 5]],"
                    " faces = [[0, 1, 2, 3], [4, 5, 1, 0], [7, 6, 5, 4],"
                    " [5, 6, 2, 1], [6, 7, 3, 2], [7, 4, 0, 3]], convexity = 10);\n"))
  

  ;; 2d objects
  (check-equal? (test-str (circle #:radius 3)) "circle(d = 6);\n")
  (check-equal? (test-str (rectangle #:x 3 #:y 4)) "square([3, 4], false);\n")
  (check-equal? (test-str (square #:side 4 #:center #t)) "square([4, 4], true);\n")
  (check-equal? (test-str (polygon #:points (list (2d-point 0 0) (2d-point 1 0) (2d-point 1 1) (2d-point 0 1))
                                   #:paths  (list (list 0 1 2 3))))
                "polygon([[0, 0], [1, 0], [1, 1], [0, 1]], [[0, 1, 2, 3]]);\n")
  (check-equal? (test-str (text "FOO")) 
                (++ "text(text = \"FOO\", size = 10, font = \"Helvetica Neue:style=Regular\","
                    " halign = \"left\", valign = \"baseline\", spacing = 1, direction = \"ltr\","
                    " language = \"en\", script = \"latin\");\n"))
  (define affine-matrix '((1 0 0 1)
                          (0 1 0 1)
                          (0 0 1 1)))
  (check-equal? (test-str (affine affine-matrix (cube #:side 3))) 
                "multmatrix([[1, 0, 0, 1], [0, 1, 0, 1], [0, 0, 1, 1]]) cube([3, 3, 3], center = false);\n;\n")

  (check-equal? (test-str (offset #:radius 3 #:chamfer #t (square #:side 4))) 
                "offset(r = 3, chamfer = true) square([4, 4], false);\n;\n")
  (check-equal? (test-str (offset #:delta 1 #:chamfer #f (square #:side 4))) 
                "offset(d = 1, chamfer = false) square([4, 4], false);\n;\n")
  (check-equal? (test-str (offset #:radius 3 (square #:side 4))) 
                "offset(r = 3, chamfer = false) square([4, 4], false);\n;\n")
  (check-equal? (test-str (offset #:delta 1 (square #:side 4))) 
                "offset(d = 1, chamfer = false) square([4, 4], false);\n;\n")

  (check-equal? (test-str (color #:rgba (0.5 0.5 0.8 1.0) (cube #:side 3))) 
                "color([0.5, 0.5, 0.8, 1.0]) cube([3, 3, 3], center = false);\n;\n")

  (check-equal? (test-str (color #:name "LightBlue"#:alpha 1.0 (cube #:side 3))) 
                "color([0.6784313725490196, 0.8470588235294118, 0.9019607843137255, 1.0]) cube([3, 3, 3], center = false);\n;\n")

  (check-equal? (test-str (color #:name "LightBlue" (cube #:side 3))) 
                "color([0.6784313725490196, 0.8470588235294118, 0.9019607843137255, 1.0]) cube([3, 3, 3], center = false);\n;\n")

  (check-equal? (test-str (resize (2 3 4) (cube #:side 3) (sphere #:diameter 2)))
                "resize([2, 3, 4]) {\n  cube([3, 3, 3], center = false);\n  sphere(d = 2);\n};\n")

  (check-equal? (test-str (mirror (2 3 4) (cube #:side 3) (sphere #:diameter 2)))
                "mirror([2, 3, 4]) {\n  cube([3, 3, 3], center = false);\n  sphere(d = 2);\n};\n")

  (check-equal? (test-str (scale (2 3 4) (cube #:side 3) (sphere #:diameter 2)))
                "scale([2, 3, 4]) {\n  cube([3, 3, 3], center = false);\n  sphere(d = 2);\n};\n")

  (check-equal? (test-str (rotate (25 35 45) (cube #:side 3) (sphere #:diameter 2)))
                "rotate([25, 35, 45]) {\n  cube([3, 3, 3], center = false);\n  sphere(d = 2);\n};\n")

  (check-equal? (test-str (translate (2 3 4) (cube #:side 3) (sphere #:diameter 2)))
                "translate([2, 3, 4]) {\n  cube([3, 3, 3], center = false);\n  sphere(d = 2);\n};\n")


  (check-equal? (test-str (extrude #:rotate (translate (10 0 0) (circle #:radius 3))))
                "rotate_extrude(angle = 360, convexity = 10) translate([10, 0, 0]) circle(d = 6);\n;\n;\n")

  (check-equal? (test-str (extrude #:height 10 #:twist 20 (translate (10 0 0) (circle #:radius 1))))
                (++ "linear_extrude(height = 10, center = false, convexity = 10, twist = 20, slices = 20, scale = 1.0)"
                    " translate([10, 0, 0]) circle(d = 2);\n;\n;\n"))
  
  (check-equal? (test-str (project #:cut (extrude #:height 10 #:twist 20 (translate (0 0 0) (square #:side 1 #:center #t)))))
                (++ "projection(cut = true) linear_extrude"
                    "(height = 10, center = false, convexity = 10, twist = 20, slices = 20, scale = 1.0)"
                    " translate([0, 0, 0]) square([1, 1], true);\n;\n;\n;\n"))

  (check-equal? (test-str (smoothness ([minimum-size 0.1][minimum-angle 0.1]) (sphere #:radius 3)))
                "{\n  $fs = 0.1;\n  $fa = 0.1;\n  sphere(d = 6);\n};\n")

  (check-equal? (test-str (union (sphere #:radius 3) (cuboid #:x 8 #:y 3 #:z 3 #:center #t)))
                "union() {\n  sphere(d = 6);\n  cube([8, 3, 3], center = true);\n};\n")

  (check-equal? (test-str (intersection (sphere #:radius 3) (cuboid #:x 8 #:y 3 #:z 3 #:center #t)))
                "intersection() {\n  sphere(d = 6);\n  cube([8, 3, 3], center = true);\n};\n")    

  (check-equal? (test-str (difference (sphere #:radius 3) (cuboid #:x 8 #:y 3 #:z 3 #:center #t)))
                "difference() {\n  sphere(d = 6);\n  cube([8, 3, 3], center = true);\n};\n")    

  (check-equal? (test-str (hull (sphere #:radius 3) (cuboid #:x 8 #:y 3 #:z 3 #:center #t)))
                "hull() {\n  sphere(d = 6);\n  cube([8, 3, 3], center = true);\n};\n")

  (check-equal? (test-str (minkowski (sphere #:radius 3) (cuboid #:x 8 #:y 3 #:z 3 #:center #t)))
                "minkowski() {\n  sphere(d = 6);\n  cube([8, 3, 3], center = true);\n};\n")

  (check-equal? (test-str (import "file")) "import(\"file\");\n")
  (check-equal? (test-str (height-map "file" #:center #t #:convexity 10 (sphere #:radius 3)))
                "surface(file = \"file\", center = true, convexity = 10) sphere(d = 6);\n;\n")
  (check-equal? (test-str (height-map "file" #:center #f (sphere #:radius 3)))
                "surface(file = \"file\", center = false, convexity = 10) sphere(d = 6);\n;\n")
  (check-equal? (test-str (height-map "file" (sphere #:radius 3))) 
                "surface(file = \"file\", center = false, convexity = 10) sphere(d = 6);\n;\n")
  (check-equal? (test-str (objects (sphere #:radius 3) (translate (10 0 0) (sphere #:radius 4)))) 
                "{\n  sphere(d = 6);\n  translate([10, 0, 0])   sphere(d = 8);\n;\n};\n")
  )


