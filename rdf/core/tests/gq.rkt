#lang racket/base

(require rackunit
         ;; --------------------------------------
         "../name.rkt"
         "../namespace.rkt"
         "../literal.rkt"
         "../triple.rkt"
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
     (let* ((ns (string->namespace "http://example.com/"))
            (graph
             (unnamed-graph
              (list
               (triple (namespace+name->url ns (string->local-name "thing"))
                            (namespace+name->url ns (string->local-name "hasName"))
                            (->literal "bob"))
               (triple (namespace+name->url ns (string->local-name "thing"))
                            (namespace+name->url ns (string->local-name "hasAge"))
                            (->literal 42))
               (triple (namespace+name->url ns (string->local-name "other"))
                            (namespace+name->url ns (string->local-name "hasName"))
                            (->literal "sheila"))
               (triple (namespace+name->url ns (string->local-name "other"))
                            (namespace+name->url ns (string->local-name "hasAge"))
                            (->literal 24)))))
            (query-pattern
             (list (ignore)
                   (comparitor (namespace+name->url ns (string->local-name "hasName")))
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
