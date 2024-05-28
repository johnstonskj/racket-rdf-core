#lang racket/base

(require racket/set
         racket/stream
         ;; --------------------------------------
         rackunit
         rackunit/text-ui
         ;; --------------------------------------
         "../graph.rkt"
         "../io.rkt"
         "../literal.rkt"
         "../name.rkt"
         "../nsname.rkt"
         "../query.rkt"
         "../resource.rkt"
         "../triple.rkt")

;; -------------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -------------------------------------------------------------------------------------------------

(define graph-query-test-suite
  (test-suite
   "Test module `query`"

   (test-case
       "function `statement-pattern->string`"
     (let* ((ns (string->resource "http://example.com/"))
            (pattern (statement-pattern
                     (ignore)
                     (compare/equal? (resource-append-name ns (string->local-name "hasName")))
                     (variable "name"))))
       (check-equal? (statement-pattern->string pattern)
                     "_ <http://example.com/hasName> ?name\n")))

   (test-case
       "function `graph-query`"
     (let* ((ns (string->resource "http://example.com/"))
            (graph
             (unnamed-graph
              (list
               (triple (resource-append-name ns (string->local-name "thing"))
                       (resource-append-name ns (string->local-name "hasName"))
                       (->literal "bob"))
               (triple (resource-append-name ns (string->local-name "thing"))
                       (resource-append-name ns (string->local-name "hasAge"))
                       (->literal 42))
               (triple (resource-append-name ns (string->local-name "other"))
                       (resource-append-name ns (string->local-name "hasName"))
                       (->literal "sheila"))
               (triple (resource-append-name ns (string->local-name "other"))
                       (resource-append-name ns (string->local-name "hasAge"))
                       (->literal 24)))))
            (initial-pattern (statement-pattern
                              (ignore)
                              (compare/equal? (resource-append-name
                                               ns
                                               (string->local-name "hasName")))
                              (variable "name")))
            (query-pattern (graph-pattern (set initial-pattern)))
            (results (graph-pattern-query query-pattern graph)))
       (check-equal? (stream->list results)
                     `((("name" . ,(string->literal "bob")))
                       (("name" . ,(string->literal "sheila")))))))))

;; -------------------------------------------------------------------------------------------------
;; Test Runner
;; -------------------------------------------------------------------------------------------------

(run-tests graph-query-test-suite)
