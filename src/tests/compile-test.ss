(module+ test
  (check-equal? (strip-one-indent "  ") "")
  (check-equal? (strip-one-indent "") "")
  (check-equal? (strip-one-indent "foo") "foo")
  
  (check-equal? (add-indent-to-lines "  " "foo\nbar") "  foo\n  bar")
  (check-equal? (add-indent-to-lines "  " "foo\nbar\n") "  foo\n  bar\n")
  (check-equal? (add-indent-to-lines "  " "\nfoo\nbar\n") "\n  foo\n  bar\n")

  (check-equal? ((with-indent-in (η "foo")) "  ") "foo")
  (check-equal? ((with-indent-out (η "foo")) "  ") "foo")


  (check-equal? ((η "foo") " ") "foo")
  (check-equal? ((f* η (η "foo")) " ") ((η "foo") " "))

  (check-equal? ((with-current-indent "Foo: ~a" (η "bar")) "..") "..Foo: bar")
  (check-equal? ((with-current-indent "Foo: ~a ~a" (η "bar") (η "car"))  "..") "..Foo: bar car")
  (check-equal? ((with-current-indent "Foo: ~a~%~a~%" (η "bar") (η "car"))  "..") "..Foo: bar\n..car\n")
  (check-equal? ((with-current-indent (η "foo") (η "bar") (η "car"))  "..") "..foobarcar")

  (check-equal? ((++-2* (η "foo") (η "bar")) " ") "foobar")
  (check-equal? ((++* (η "foo")) " ") "foo")
  (check-equal? ((++* (η "foo") (η "bar")) " ") "foobar")
  (check-equal? ((++* (η "foo") (η "bar") (η "car")) " ") "foobarcar")
  (check-equal? ((++* (η "foo") (η "bar") (η "car") #:separator " " ) " ") "foo bar car")

  (check-equal? ((compile-parameter-assignment '($fn 20)) " ") " $fn = 20")

  (check-equal? ((as-vector* (list (η "true") (η "false"))) " ") "[true, false]")
  (check-equal? ((as-vector  (list "true" "false")) " ") "[\"true\", \"false\"]")


  (check-equal? ((compile-component #t) " ") "true")
  (check-equal? ((compile-component #f) " ") "false")
  (check-equal? ((compile-component 20) " ") "20")
  (check-equal? ((compile-component 1/2) " ") "0.5")
  (check-equal? ((compile-component "cat") " ") "\"cat\"")
  (check-equal? ((compile-component 'dog) " ") "dog")
  (check-equal? ((compile-component (point 0 2 3)) " ") "[0, 2, 3]")
  (check-equal? ((compile-component (2d-point 0 2)) " ") "[0, 2]")
  (check-equal? ((compile-component (face (list 0 1))) " ") "[0, 1]")

  (check-true (component? #t))
  (check-true (component? #f))
  (check-true (component? 20))
  (check-true (component? 20.0))
  (check-true (component? "cat"))
  (check-true (component? 'dog))
  (check-true (component? (point 2 2 2)))
  (check-true (component? (2d-point 2 2)))
  (check-true (component? (face (list 0 1))))

  (check-true (ensure-affine-matrix
               '((0 0 0 1)
                 (0 0 0 1)
                 (0 0 0 1))))

  (check-exn exn? (λ () (ensure-affine-matrix
                         '((0 0 0 1)
                           (0 0 0 1)))))

  (check-equal? ((compile-affine-matrix 
                  '((0 0 0 1)
                    (0 0 0 1)
                    (0 0 0 1))) " ")
                "[[0, 0, 0, 1], [0, 0, 0, 1], [0, 0, 0, 1]]")

  (check-equal? ((compile-3d-object (sphere-object 3)) " ") " sphere(d = 3)")
  (check-equal? ((compile-3d-object (cuboid-object 3 4 5 #f)) " ") " cube([3, 4, 5], center = false)")
  (check-equal? ((compile-3d-object (conic-object 3 4 5 #f)) " ") " cylinder(3, d1 = 4, d2 = 5, center = false)")
  (check-equal? 
   ((compile-3d-object 
     (polyhedron-object 
      (list (point 0 2 3) (point 0 7 7) (point 3 3 3) (point 4 4 4))
      (list (face (list 0 1 2))
            (face (list 1 2 3)))
      10)) " ")
   (++ " polyhedron(points = [[0, 2, 3], [0, 7, 7], [3, 3, 3], [4, 4, 4]],"
       " faces = [[0, 1, 2], [1, 2, 3]], convexity = 10)"))
  (check-exn exn? 
             (λ () (compile-3d-object #f)))

  (check-equal? ((compile-2d-object (circle-object 3)) " ") " circle(d = 3)")
  (check-equal? ((compile-2d-object (rectangle-object 3 4 #f)) " ") " square([3, 4], false)")
  (check-equal? 
   ((compile-2d-object 
     (polygon-object 
      (list (2d-point 2 3) (2d-point 7 7) (2d-point 3 3) (2d-point 4 4))
      (list (list 0 1 2) (list 1 2 3)))) " ")
   (++ " polygon([[2, 3], [7, 7], [3, 3], [4, 4]],"
       " [[0, 1, 2], [1, 2, 3]])"))
  (check-equal?
   ((compile-2d-object
     (text-object "foo" 10 "helvetica" "left" "top" 0.5 "ltr" "en" "no")) " ")
   (++ " text(text = \"foo\", size = 10, font = \"helvetica\", halign = \"left\","
       " valign = \"top\", spacing = 0.5, direction = \"ltr\", language = \"en\", script = \"no\")"))
  (check-exn exn? 
             (λ () (compile-2d-object #f)))

  (check-equal? ((compile-import (import-object "foo")) " ") " import(\"foo\")")

  (check-equal? ((compile-group (list (circle-object 3) (rectangle-object 3 4 #f))) "") 
                "{\n  circle(d = 3);\n  square([3, 4], false);\n}")

  (check-equal? ((compile-set-operation  (set-operation-object 'union (list (circle-object 3) (rectangle-object 3 4 #f)))) "")
                "union() {\n  circle(d = 3);\n  square([3, 4], false);\n}")


  (check-equal? ((compile-object (sphere-object 3)) " ") " sphere(d = 3)")
  (check-equal? ((compile-object (cuboid-object 3 4 5 #f)) " ") " cube([3, 4, 5], center = false)")
  (check-equal? ((compile-object (conic-object 3 4 5 #f)) " ") " cylinder(3, d1 = 4, d2 = 5, center = false)")
  (check-equal? 
   ((compile-object
     (polyhedron-object 
      (list (point 0 2 3) (point 0 7 7) (point 3 3 3) (point 4 4 4))
      (list (face (list 0 1 2))
            (face (list 1 2 3)))
      10)) " ")
   (++ " polyhedron(points = [[0, 2, 3], [0, 7, 7], [3, 3, 3], [4, 4, 4]],"
       " faces = [[0, 1, 2], [1, 2, 3]], convexity = 10)"))
  (check-equal? ((compile-object (circle-object 3)) " ") " circle(d = 3)")
  (check-equal? ((compile-object (rectangle-object 3 4 #f)) " ") " square([3, 4], false)")
  (check-equal? 
   ((compile-object
     (polygon-object 
      (list (2d-point 2 3) (2d-point 7 7) (2d-point 3 3) (2d-point 4 4))
      (list (list 0 1 2) (list 1 2 3)))) " ")
   (++ " polygon([[2, 3], [7, 7], [3, 3], [4, 4]],"
       " [[0, 1, 2], [1, 2, 3]])"))
  (check-equal?
   ((compile-object
     (text-object "foo" 10 "helvetica" "left" "top" 0.5 "ltr" "en" "no")) " ")
   (++ " text(text = \"foo\", size = 10, font = \"helvetica\", halign = \"left\","
       " valign = \"top\", spacing = 0.5, direction = \"ltr\", language = \"en\", script = \"no\")"))

  (check-equal? ((compile-object (import-object "foo")) " ") " import(\"foo\")")

  (check-equal? ((compile-object  (set-operation-object 'union (list (circle-object 3) (rectangle-object 3 4 #f)))) "")
                "union() {\n  circle(d = 3);\n  square([3, 4], false);\n}")
  (check-true (object? (circle-object 3)))
  (check-true (object? (sphere-object 3)))
  (check-true (object? (set-operation-object 'union  (list (circle-object 3) (rectangle-object 3 4 #f)))))
  (check-true (object? (import-object "foo")))

  (define group-of-objects (list (sphere-object 3) (sphere-object 5)))

  (check-equal? ((compile-parameters (parameters-transform '(($fa 3.0) ($fs 8.0) ($fn 20)) 
                                                           group-of-objects)) "")
                "{\n  $fa = 3.0;\n  $fs = 8.0;\n  $fn = 20;\n  sphere(d = 3);\n  sphere(d = 5);\n}")

  (check-equal? ((compile-height-map (height-map-transform "file" group-of-objects false 10)) "")
                "surface(file = \"file\", center = false, convexity = 10) {\n  sphere(d = 3);\n  sphere(d = 5);\n}")

  (check-equal? ((compile-projection (project-transform #t group-of-objects)) "")
                "projection(cut = true) {\n  sphere(d = 3);\n  sphere(d = 5);\n}")

  (check-equal? ((compile-extrude (linear-extrude-transform group-of-objects 3 #f 10 1.0 20 1.0)) "") 
                (++ "linear_extrude(height = 3, center = false, convexity = 10, twist = 1.0, slices = 20, scale = 1.0)"
                    " {\n  sphere(d = 3);\n  sphere(d = 5);\n}"))

  (check-equal? ((compile-extrude (rotate-extrude-transform group-of-objects 25 10)) "") 
                "rotate_extrude(angle = 25, convexity = 10) {\n  sphere(d = 3);\n  sphere(d = 5);\n}")

  (check-equal? ((compile-transform (basic-transform 'translate 2 3 4 group-of-objects)) "")
                "translate([2, 3, 4]) {\n  sphere(d = 3);\n  sphere(d = 5);\n}")

  (check-equal? ((compile-transform (basic-color-transform 255 255 255 1.0  group-of-objects)) "")
                "color([255, 255, 255, 1.0]) {\n  sphere(d = 3);\n  sphere(d = 5);\n}")

  (check-equal? ((compile-transform (offset-transform 10 #f #f group-of-objects)) "")
                "offset(r = 10, chamfer = false) {\n  sphere(d = 3);\n  sphere(d = 5);\n}")

  (check-equal? ((compile-transform (affine-transform '((0 0 0 1)
                                                        (0 0 0 1)
                                                        (0 0 0 1)) group-of-objects)) "")
                "multmatrix([[0, 0, 0, 1], [0, 0, 0, 1], [0, 0, 0, 1]]) {\n  sphere(d = 3);\n  sphere(d = 5);\n}")

  (check-equal? ((compile-expr (parameters-transform '(($fa 3.0) ($fs 8.0) ($fn 20)) 
                                                     group-of-objects)) "")
                "{\n  $fa = 3.0;\n  $fs = 8.0;\n  $fn = 20;\n  sphere(d = 3);\n  sphere(d = 5);\n}")

  (check-equal? ((compile-expr (height-map-transform "file" group-of-objects false 10)) "")
                "surface(file = \"file\", center = false, convexity = 10) {\n  sphere(d = 3);\n  sphere(d = 5);\n}")

  (check-equal? ((compile-expr (project-transform #t group-of-objects)) "")
                "projection(cut = true) {\n  sphere(d = 3);\n  sphere(d = 5);\n}")
  
  (check-equal? ((compile-expr (linear-extrude-transform group-of-objects 3 #f 10 1.0 20 1.0)) "") 
                (++ "linear_extrude(height = 3, center = false, convexity = 10, twist = 1.0, slices = 20, scale = 1.0)"
                    " {\n  sphere(d = 3);\n  sphere(d = 5);\n}"))
  
  (check-equal? ((compile-expr (rotate-extrude-transform group-of-objects 25 10)) "") 
                "rotate_extrude(angle = 25, convexity = 10) {\n  sphere(d = 3);\n  sphere(d = 5);\n}")

  (check-equal? ((compile-expr (basic-transform 'translate 2 3 4 group-of-objects)) "")
                "translate([2, 3, 4]) {\n  sphere(d = 3);\n  sphere(d = 5);\n}")

  (check-equal? ((compile-expr (basic-color-transform 255 255 255 1.0  group-of-objects)) "")
                "color([255, 255, 255, 1.0]) {\n  sphere(d = 3);\n  sphere(d = 5);\n}")

  (check-equal? ((compile-expr (offset-transform 10 #f #f group-of-objects)) "")
                "offset(r = 10, chamfer = false) {\n  sphere(d = 3);\n  sphere(d = 5);\n}")

  (check-equal? ((compile-expr (affine-transform '((0 0 0 1)
                                                   (0 0 0 1)
                                                   (0 0 0 1)) group-of-objects)) "")
                "multmatrix([[0, 0, 0, 1], [0, 0, 0, 1], [0, 0, 0, 1]]) {\n  sphere(d = 3);\n  sphere(d = 5);\n}")
  )
