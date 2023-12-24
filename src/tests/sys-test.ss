(module+ test
  (check-equal? (to-shell-arg "foo") "foo")
  (check-equal? (to-shell-arg '-i) "-i")
  (check-equal? (to-shell-arg '(escape "foo")) "\"foo\"")

  (case (system-type 'os)
    [(macosx unix)
     (check-equal? (path->string (find-executable "ls")) "/bin/ls")
     (define-shell-command !ls "ls")
     (check-true  (!ls -d / > /dev/null 2>&1))
     (check-false (!ls -d "/doesnotexist" > /dev/null 2>&1))
     (check-false (!ls -d (escape "/doesnotexist") > /dev/null 2>&1))
     (check-true  (!ls -i -d / > /dev/null 2>&1))
     (check-equal? (path->string (!ls #:path)) "/bin/ls")]))
