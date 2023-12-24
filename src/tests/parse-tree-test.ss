(module+ test
  (check-true (2d-object? (circle-object 4)))
  (check-false (2d-object? (sphere-object 4)))
  (check-false (2d-object? #f))

  (check-false  (3d-object? (circle-object 4)))
  (check-true (3d-object? (sphere-object 4)))
  (check-false (3d-object? #f))

  (check-equal? (height-map-with-args "somefile" (list (sphere-object 4)) #:center #t #:convexity 12)
                (height-map-transform "somefile"(list (sphere-object 4)) #t 12))

  (check-equal? (linear-extrude-with-args (list (sphere-object 4))
                                          #:height 4
                                          #:center #t
                                          #:convexity 13
                                          #:twist #f
                                          #:slices 10
                                          #:scale 1.0)
                (linear-extrude-transform (list (sphere-object 4)) 4 #t 13 #f 10 1.0))

  (check-equal? (rotate-extrude-with-args (list (sphere-object 4))
                                          #:angle 20
                                          #:convexity 12)
                (rotate-extrude-transform (list (sphere-object 4)) 20 12))

  (check-true (extrude? (rotate-extrude-with-args (list (sphere-object 4))
                                                  #:angle 20
                                                  #:convexity 12)))

  (check-false (extrude? #f))
  
  (check-true  (extrude? (linear-extrude-with-args (list (sphere-object 4))
                                                   #:height 4
                                                   #:center #t
                                                   #:convexity 13
                                                   #:twist #f
                                                   #:slices 10
                                                   #:scale 1.0)))

  (check-true (transform? (basic-transform "name" 0 0 0 (list (sphere-object 4)))))
  (check-true (transform? (basic-color-transform  0 0 0 1.0 (list (sphere-object 4)))))
  (check-true (transform? (offset-transform  0 0 0 (list (sphere-object 4)))))
  (check-true (transform? (affine-transform  '((0 0 0 1)
                                               (0 0 0 1)
                                               (0 0 0 1))
                                             (list (sphere-object 4)))))
  )
