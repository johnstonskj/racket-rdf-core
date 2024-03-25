#lang racket/base

(require net/url-string
         rackunit
         ;; --------------------------------------
         "../literal.rkt"
         "../statement.rkt"
         "../graph.rkt"
         "../io.rkt")

(provide core-io-test-suite)

(define core-io-test-suite
  (test-suite
   "Tests for module `io` -- simple I/O for the data model"

   (test-case
       "Test Statement Objects"
     (let ((test-data
            (list (cons 22 "22")
                  (cons 22.0 "22.0")
                  (cons 1/2 "1/2")
                  (cons #t "true")
                  (cons #f "false")
                  (cons "hello" "\"hello\"")
                  (cons (string->url "http://example.org/some/thing")
                        "<http://example.org/some/thing>")
                  (cons (make-language-string "Hi" "en")
                        "\"Hi\"@en")
                  (cons (make-typed-string "22" (string->url "https://xsd.com/integer"))
                        "\"22\"^^<https://xsd.com/integer>"))))
       (for-each
        (λ (pair)
          (let ((actual (object->turtle-string (car pair))))
            (check-equal? actual (cdr pair))))
        test-data)))

   ))

