#lang racket

(require "utils.ss")
(require racket/include)
(require rackunit)

(define/provide (bin-paths)
  (case (system-type 'os)
    ((unix windows) (string-split (getenv "PATH") ":"))
    ((macosx) (cons "/Applications/OpenSCAD.app/Contents/MacOS" (string-split (getenv "PATH") ":")))))

(define/provide (find-executable name #:case-sensitive [case-sensitive #f])
  (or
   (call/cc (λ (return)
              (for*/fold ([found? #f]) ([dir (bin-paths)]
                                        [p (if (directory-exists? dir) (directory-list dir) '())])
                (let ([path-string (path->string p)])
                  (if (if (not case-sensitive)
                          (equal? (string-downcase name) (string-downcase path-string))
                          (equal? name path-string))
                      (return (build-path dir p))
                      found?)))))
   (error 'find-executable "Could not find executable: ~a~%" name)))

(define-syntax/provide (define-shell-command x)
  (syntax-case x ()
    [(_ name filename)
     #'(define-syntax (name y)
         (syntax-case y ()
           [(name #:path)
            #'(find-executable filename)]
           [(name arg (... ...))
            #'(let ([cmd (apply ++ #:separator " " (find-executable filename) (map to-shell-arg `(arg (... ...))))])
                (system cmd))]
           ))]))

(define (to-shell-arg arg)
  (match arg
    (`(escape ,arg) (format "~s" arg))
    (-i  "-i")
    (else arg)))

;(provide (for-syntax define-shell-command))
    

(include "tests/sys-test.ss")
