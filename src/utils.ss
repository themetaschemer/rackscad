#lang racket

(require rackunit)
(require racket/include)

(define (string-append-with-separator sep . args)
  (cond
   [(null? args) ""]
   [else (for/fold ([str (car args)]) ([arg (cdr args)])
           (string-append str sep arg))]))

(define (ensure-string x)
  (if (string? x) x (format "~a" x)))

(define (++ #:separator [sep ""] . args)
  (apply string-append-with-separator sep (map ensure-string args)))

(define-syntax (define/provide x)
  (syntax-case x ()
    [(_ (name . args) body ...)
     #'(begin
         (provide name)
         (define (name . args) body ...))]
    [(_ name body ...)
     #'(begin
         (provide name)
         (define name body ...))]))

(define-syntax (struct/provide x)
  (syntax-case x ()
    [(_ name args ...)
     #'(begin
         (provide (struct-out name))
         (struct name args ...))]))

(define-syntax (define-syntax/provide x)
  (syntax-case x ()
    [(_ (name . args) body ...)
     #'(begin
         (provide name)
         (define-syntax (name . args) body ...))]))

(define-syntax (with-ports x)
  (syntax-case x ()
    [(_ [(port open-type fname args ...) ...] body ...)
     #'(let ([port #f] ...)
         (dynamic-wind
             (λ () (set! port (open-file open-type fname args ...)) ...)
             (λ () body ...)
             (λ () (close-file open-type port) ...)))]))

(define-syntax (open-file x)
  (syntax-case x ()
    [(_ #:input-file fname args ...)
     #'(open-input-file fname args ...)]
    [(_ #:output-file fname args ...)
     #'(open-output-file fname args ...)]))

(define-syntax (close-file x)
  (syntax-case x ()
    [(_ #:input-file port)
     #'(close-input-port port)]
    [(_ #:output-file port)
     #'(close-output-port port)]))

(include "utils-test.ss")
(provide (all-defined-out))
