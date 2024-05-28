#lang racket/base

(require racket/set
         ;; --------------------------------------
         net/url-string
         ;; --------------------------------------
         rackunit
         rackunit/text-ui
         ;; --------------------------------------
         "../name.rkt"
         "../nsname.rkt"
         "../literal.rkt"
         "../resource.rkt"
         "../statement.rkt"
         "../triple.rkt"
         "../graph.rkt")

;; -------------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -------------------------------------------------------------------------------------------------

(define graph-test-suite
  (test-suite
   "Test module `graph`"

   (test-case
       "function `make-sub-graph`"
     (check-equal?
      (make-statements
       "http://example.com/p/me"
       `(("http://example.com/v/people#hasFirstName" (,(string->literal "Alice")))
         ("http://example.com/v/people#hasLastName" (,(string->literal "Wonder")))
         ("http://example.com/v/people#hasScores" ,(map exact-integer->literal '(2 4 6)))))
      (set (triple (string->resource "http://example.com/p/me")
                   (string->resource "http://example.com/v/people#hasFirstName")
                   (->literal "Alice"))
           (triple (string->resource "http://example.com/p/me")
                   (string->resource "http://example.com/v/people#hasLastName")
                   (->literal "Wonder"))
           (triple (string->resource "http://example.com/p/me")
                   (string->resource "http://example.com/v/people#hasScores")
                   (->literal 2))
           (triple (string->resource "http://example.com/p/me")
                   (string->resource "http://example.com/v/people#hasScores")
                   (->literal 4))
           (triple (string->resource "http://example.com/p/me")
                   (string->resource "http://example.com/v/people#hasScores")
                   (->literal 6)))))

   (test-case
       "function `skolem-resource?`"
     (let ((test-data '("http://example.com/.well-known/skolem/001"
                        "https://example.com/.well-known/skolem/00/1")))
       (for-each
        (λ (url) (check-pred skolem-resource? url))
        (map string->resource test-data))))

   (test-case
       "function `graph-skolemize`"
     (let* ((ns (string->resource "http://example.com/"))
            (bnode-1 (make-blank-node))
            (bnode-2 (make-blank-node))
            (test-graph (unnamed-graph
                         (list
                          (triple (resource-append-name ns (string->local-name "thing"))
                                  (resource-append-name ns (string->local-name "hasName"))
                                  bnode-1)
                          (triple bnode-1
                                  (resource-append-name ns (string->local-name "firstName"))
                                  (string->literal "Spongebob"))
                          (triple bnode-1
                                  (resource-append-name ns (string->local-name "lastName"))
                                  (string->literal "Squarepants"))
                          (triple bnode-1
                                  (resource-append-name ns (string->local-name "hasFriend"))
                                  bnode-2)
                          (triple bnode-2
                                  (resource-append-name ns (string->local-name "firstName"))
                                  (string->literal "Patrick"))
                          (triple bnode-2
                                  (resource-append-name ns (string->local-name "lastName"))
                                  (string->literal "Star"))))))
       (check-true (ormap blank-node?
                          (set->list (set-union (graph-distinct-subjects test-graph)
                                                (graph-distinct-objects test-graph)))))
       (let ((skolem-graph (graph-skolemize test-graph "example.org")))
         (check-false (ormap blank-node?
                             (set->list (set-union (graph-distinct-subjects skolem-graph)
                                                   (graph-distinct-objects skolem-graph))))))))))

;; -------------------------------------------------------------------------------------------------
;; Test Runner
;; -------------------------------------------------------------------------------------------------

(run-tests graph-test-suite)
