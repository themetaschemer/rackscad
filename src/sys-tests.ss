(module+ test
  (case (system-type 'os)
    ((macosx unix)
     (check-equal? (path->string (find-executable "ls")) "/bin/ls")
     (define-shell-command !ls "ls")
     (check-true  (!ls '-d "/" "> /dev/null 2>&1"))
     (check-false (!ls '-d "/doesnotexist" "> /dev/null 2>&1"))
     (check-equal? (path->string (!ls #:path)) "/bin/ls")
     ))
  )