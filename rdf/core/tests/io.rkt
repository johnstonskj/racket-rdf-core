#lang racket/base

(require net/url-string
         rackunit
         ;; --------------------------------------
         "../literal.rkt"
         "../statement.rkt"
         "../graph.rkt"
         "../io.rkt")

;; -------------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -------------------------------------------------------------------------------------------------

(define core-io-test-suite
  (test-suite
   "Tests for module `io` -- simple I/O for the data model"

   (test-case
       "Test Literals"
     (let ((test-data
            (list (cons (exact-integer->literal 22)
                        "\"22\"^^<http://www.w3.org/2001/XMLSchema#integer>")
                  (cons (flonum->literal 22.0)
                        "\"22.0\"^^<http://www.w3.org/2001/XMLSchema#double>")
                  (cons literal-true
                        "\"true\"^^<http://www.w3.org/2001/XMLSchema#boolean>")
                  (cons literal-false
                        "\"false\"^^<http://www.w3.org/2001/XMLSchema#boolean>")
                  (cons (make-untyped-literal "hello") "\"hello\"")
                  (cons (make-lang-string-literal "Hi" "en")
                        "\"Hi\"@en")
                  (cons (make-typed-literal "22" (string->url "http://www.w3.org/2001/XMLSchema#short"))
                        "\"22\"^^<http://www.w3.org/2001/XMLSchema#short>"))))
       (for-each
        (λ (pair)
          (let ((actual (literal->turtle-string (car pair))))
            (check-equal? actual (cdr pair))))
        test-data)))

   ))

;; -------------------------------------------------------------------------------------------------
;; Test Runner
;; -------------------------------------------------------------------------------------------------

(module+ test

  (require rackunit
           rackunit/text-ui)

  (run-tests core-io-test-suite))
