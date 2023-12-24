(module+ test
  (check-equal? (string-append-with-separator " ") "")
  (check-equal? (string-append-with-separator " " "a" "b" "c") "a b c")
  (check-exn exn? (λ () (string-append-with-separator " " 'a "b" "c")))
  (check-equal? (string-append-with-separator " " (ensure-string 'a) (ensure-string "b") (ensure-string '#\c))
                "a b c")
  (check-equal? (++ #:separator " " 'a "b" '#\c) "a b c")
  (check-equal? (++ 'a "b" '#\c) "abc")
  (check-equal? (++) "")
  (check-equal? (begin
                  (with-ports ([out #:output-file "/tmp/tout" #:exists 'truncate]) (display "foo" out))
                  (with-ports ([in  #:input-file  "/tmp/tout"]) (read in))) 'foo)
  (check-equal? (begin
                  (with-ports ([out1 #:output-file "/tmp/tout1" #:exists 'truncate]
                               [out2 #:output-file "/tmp/tout2" #:exists 'truncate]) 
                    (display "foo" out1)
                    (display "bar" out2))
                  (with-ports ([in1  #:input-file  "/tmp/tout1"]
                               [in2  #:input-file  "/tmp/tout2"]) 
                    (list (read in1) (read in2)))) '(foo bar))
  (check-equal? (begin
                  (with-handlers ([exn? (λ (exn) #t)])
                    (with-ports ([out #:output-file "/tmp/tout" #:exists 'truncate]) 
                      (display "foo" out)
                      (error "something-broke")
                      (display "shouldnotbethere")))
                  (with-ports ([in  #:input-file  "/tmp/tout"]) (read in))) 'foo)

  )


