(module+ test
  (check-equal? (strip-one-indent "  ") "")
  (check-equal? (strip-one-indent "") "")
  (check-equal? (strip-one-indent "foo") "foo")
  
  (check-equal? (add-indent-to-lines "  " "foo\nbar") "  foo\n  bar")
  (check-equal? (add-indent-to-lines "  " "foo\nbar\n") "  foo\n  bar\n")
  (check-equal? (add-indent-to-lines "  " "\nfoo\nbar\n") "\n  foo\n  bar\n")

  (check-equal? ((with-indent-in (η "foo")) #f "  ") "foo")
  (check-equal? ((with-indent-out (η "foo")) #f "  ") "foo")


  (check-equal? ((η "foo") #f " ") "foo")
  (check-equal? ((f* η (η "foo")) #f " ") ((η "foo") #f " "))

  (check-equal? ((with-current-indent "Foo: ~a" (η "bar")) #f "..") "..Foo: bar")
  (check-equal? ((with-current-indent "Foo: ~a ~a" (η "bar") (η "car")) #f "..") "..Foo: bar car")
  (check-equal? ((with-current-indent "Foo: ~a~%~a~%" (η "bar") (η "car")) #f "..") "..Foo: bar\n..car\n")
  (check-equal? ((with-current-indent (η "foo") (η "bar") (η "car")) #f "..") "..foobarcar")
  )
