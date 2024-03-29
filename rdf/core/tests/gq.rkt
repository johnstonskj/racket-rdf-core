#lang racket/base

(require rackunit
         racket/list
         ;; --------------------------------------
         "../namespace.rkt"
         "../literal.rkt"
         "../statement.rkt"
         "../graph.rkt"
         "../gq.rkt"
         "../io.rkt")

;; -------------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -------------------------------------------------------------------------------------------------

(define graph-query-test-suite
  (test-suite
   "Tests for module `gq` -- graph queries"

   (test-case
       "Example query"
     (let* ((ns (make-namespace "http://example.com/" "ex"))
            (graph
             (make-default-graph
              (list
               (make-statement (namespace-make-url ns "thing")
                               (namespace-make-url ns "hasName")
                               (->literal "bob"))
               (make-statement (namespace-make-url ns "thing")
                               (namespace-make-url ns "hasAge")
                               (->literal 42))
               (make-statement (namespace-make-url ns "other")
                               (namespace-make-url ns "hasName")
                               (->literal "sheila"))
               (make-statement (namespace-make-url ns "other")
                               (namespace-make-url ns "hasAge")
                               (->literal 24)))))
            (query-pattern
             (list (ignore)
                   (comparitor (namespace-make-url ns "hasName"))
                   (variable "name")))
            (results (graph-query graph query-pattern)))
       (check-equal? (statement-pattern->string query-pattern) "_ <http://example.com/hasName> ?name .\n")
       (check-equal? results `((("name" . ,(string->literal "bob"))) (("name" . ,(string->literal "sheila")))))))))

;; -------------------------------------------------------------------------------------------------
;; Test Runner
;; -------------------------------------------------------------------------------------------------

(module+ test

  (require rackunit
           rackunit/text-ui)

  (run-tests graph-query-test-suite))
