#lang racket/base

(require net/url-string
         ;; --------------------------------------
         rackunit
         rackunit/text-ui
         ;; --------------------------------------
         ;;"../dataset.rkt"
         ;;"../graph.rkt"
         "../io.rkt"
         "../literal.rkt"
         ;;"../quad.rkt"
         "../resource.rkt"
         "../statement.rkt"
         "../triple.rkt")

;; -------------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -------------------------------------------------------------------------------------------------

(define core-io-test-suite
  (test-suite
   "Test Module `io`"

   (test-case
       "function `literal->ntriple-string`"
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
                  (cons (make-typed-literal "22" (string->resource "http://www.w3.org/2001/XMLSchema#short"))
                        "\"22\"^^<http://www.w3.org/2001/XMLSchema#short>"))))
       (for-each
        (λ (pair)
          (let ((actual (literal->ntriple-string (car pair))))
            (check-equal? actual (cdr pair))))
        test-data)))

   (test-case
       "function `blank-node->ntriple-string`"
     (check-equal?
      (blank-node->ntriple-string (make-blank-node "B17"))
      "_:B17"))

   (test-case
       "function `resource->ntriple-string`"
     (check-equal?
      (resource->ntriple-string (string->resource "http://www.w3.org/2001/XMLSchema#integer"))
      "<http://www.w3.org/2001/XMLSchema#integer>"))

   (test-case
       "function `predicate->ntriple-string`"
     (check-equal?
      (predicate->ntriple-string (string->resource "http://example.com/v/people#hasScores"))
      "<http://example.com/v/people#hasScores>"))

   (test-case
       "function `subject->ntriple-string`"
     (check-equal?
      (subject->ntriple-string (string->resource "http://example.com/people/me"))
      "<http://example.com/people/me>")
     (check-equal?
      (subject->ntriple-string (make-blank-node "B36"))
      "_:B36"))

   (test-case
       "function `object->ntriple-string`"
     (check-equal?
      (object->ntriple-string (string->resource "http://example.com/people/me"))
      "<http://example.com/people/me>")
     (check-equal?
      (object->ntriple-string (make-blank-node "B36"))
      "_:B36")
     (check-equal?
      (object->ntriple-string literal-true)
       "\"true\"^^<http://www.w3.org/2001/XMLSchema#boolean>"))

   (test-case
       "function `statement->ntriple-string`"
     (check-equal?
      (statement->ntriple-string
       (triple (string->resource "http://example.com/people/me")
               (string->resource "http://example.com/v/people#hasScore")
               (->literal 42)))
      "<http://example.com/people/me> <http://example.com/v/people#hasScore> \"42\"^^<http://www.w3.org/2001/XMLSchema#integer> .\n"))

   (test-case
       "function `statement->nquad-string`"
     (check-equal?
      (statement->nquad-string
       (string->resource "http://example.com/g/friends")
       (triple (string->resource "http://example.com/people/me")
               (string->resource "http://example.com/v/people#hasScore")
               (->literal 42)))
      "<http://example.com/people/me> <http://example.com/v/people#hasScore> \"42\"^^<http://www.w3.org/2001/XMLSchema#integer> <http://example.com/g/friends> .\n"))

   (test-case
       "function `write-ntriple-graph`"
     (check-equal? 1 1))

   (test-case
       "function `write-nquad-graph`"
     (check-equal? 1 1))

   (test-case
       "function `write-trig-graph`"
     (check-equal? 1 1))

   (test-case
       "function `write-nquad-dataset`"
     (check-equal? 1 1))

   (test-case
       "function `write-trig-dataset`"
     (check-equal? 1 1))

   (test-case
       "function `write-statement-pattern`"
     (check-equal? 1 1))
    ))

;; -------------------------------------------------------------------------------------------------
;; Test Runner
;; -------------------------------------------------------------------------------------------------

(run-tests core-io-test-suite)
