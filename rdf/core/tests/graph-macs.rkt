#lang racket/base

(require racket/set
         ;; --------------------------------------
         rackunit
         rackunit/text-ui
         ;; --------------------------------------
         "../graph.rkt"
         "../literal.rkt"
         "../resource.rkt"
         "../statement.rkt"
         "../triple.rkt"
         ;; --------------------------------------
         "./shared.rkt")

;; -------------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -------------------------------------------------------------------------------------------------

(define macro-rdf-sub-graph-test-suite
  (test-suite
   "Tests macro `rdf-sub-graph`"

   (test-case
       "Single statement"
     (check-equal?
      (rdf-sub-graph "http://example.com/p/me"
                     "http://example.com/v/people#hasName" "Alice")
      (set (triple (string->resource "http://example.com/p/me")
                   (string->resource "http://example.com/v/people#hasName")
                   (->literal "Alice")))))

   (test-case
       "Single statement, using parens"
     (check-equal?
      (rdf-sub-graph "http://example.com/p/me"
                     ("http://example.com/v/people#hasName" ("Bob")))
      (set (triple (string->resource "http://example.com/p/me")
                   (string->resource "http://example.com/v/people#hasName")
                   (->literal "Bob")))))

   (test-case
       "Multiple predicates with single objects"
     (check-equal?
      (rdf-sub-graph "http://example.com/p/me"
                     ("http://example.com/v/people#hasFirstName" "Carol")
                     ("http://example.com/v/people#hasLastName" "Craig"))
      (set (triple (string->resource "http://example.com/p/me")
                   (string->resource "http://example.com/v/people#hasFirstName")
                   (->literal "Carol"))
           (triple (string->resource "http://example.com/p/me")
                   (string->resource "http://example.com/v/people#hasLastName")
                   (->literal "Craig")))))

   (test-case
       "Tests for multiple predicates with single objects, using parens"
     (check-equal?
      (rdf-sub-graph "http://example.com/p/me"
                     ("http://example.com/v/people#hasFirstName" ("Dan"))
                     ("http://example.com/v/people#hasLastName" ("Davids")))
      (set (triple (string->resource "http://example.com/p/me")
                   (string->resource "http://example.com/v/people#hasFirstName")
                   (->literal "Dan"))
           (triple (string->resource "http://example.com/p/me")
                   (string->resource "http://example.com/v/people#hasLastName")
                   (->literal "Davids")))))

   (test-case
       "Multiple predicates each with multiple objects"
     (check-equal?
      (rdf-sub-graph "http://example.com/p/me"
                     ("http://example.com/v/people#hasName" ("Erin" "Eves"))
                     ("http://example.com/v/people#hasScores" (2 4 6)))
      (set (triple (string->resource "http://example.com/p/me")
                   (string->resource "http://example.com/v/people#hasName")
                   (->literal "Erin"))
           (triple (string->resource "http://example.com/p/me")
                   (string->resource "http://example.com/v/people#hasName")
                   (->literal "Eves"))
           (triple (string->resource "http://example.com/p/me")
                   (string->resource "http://example.com/v/people#hasScores")
                   (->literal 2))
           (triple (string->resource "http://example.com/p/me")
                   (string->resource "http://example.com/v/people#hasScores")
                   (->literal 4))
           (triple (string->resource "http://example.com/p/me")
                   (string->resource "http://example.com/v/people#hasScores")
                   (->literal 6)))))

   ;; ----------------------------------------------------------------------------

   (test-case
       "Anonymous subject and single statement"
     (let ((label-maker (test-bnode-label-maker)))
       (parameterize ((blank-node-label-maker label-maker))
         (check-equal?
          (rdf-sub-graph "http://example.com/v/people#hasName" "Alice")
          (set (triple (make-blank-node "test-1")
                       (string->resource "http://example.com/v/people#hasName")
                       (->literal "Alice")))))))

   (test-case
       "Anonymous subject and single statement, using parens"
     (let ((label-maker (test-bnode-label-maker)))
       (parameterize ((blank-node-label-maker label-maker))
         (check-equal?
          (rdf-sub-graph ("http://example.com/v/people#hasName" ("Bob")))
          (set (triple (make-blank-node "test-1")
                       (string->resource "http://example.com/v/people#hasName")
                       (->literal "Bob")))))))

   (test-case
       "Anonymous subject and multiple predicates with single objects"
     (let ((label-maker (test-bnode-label-maker)))
       (parameterize ((blank-node-label-maker label-maker))
         (check-equal?
          (rdf-sub-graph ("http://example.com/v/people#hasFirstName" "Carlos")
                         ("http://example.com/v/people#hasLastName" "Craig"))
          (set (triple (make-blank-node "test-1")
                       (string->resource "http://example.com/v/people#hasFirstName")
                       (->literal "Carlos"))
               (triple (make-blank-node "test-1")
                       (string->resource "http://example.com/v/people#hasLastName")
                       (->literal "Craig")))))))

   (test-case
       "Anonymous subject and multiple predicates with single objects, using parens"
     (let ((label-maker (test-bnode-label-maker)))
       (parameterize ((blank-node-label-maker label-maker))
         (check-equal?
          (rdf-sub-graph ("http://example.com/v/people#hasFirstName" ("Dan"))
                         ("http://example.com/v/people#hasLastName" ("Davids")))
          (set (triple (make-blank-node "test-1")
                       (string->resource "http://example.com/v/people#hasFirstName")
                       (->literal "Dan"))
               (triple (make-blank-node "test-1")
                       (string->resource "http://example.com/v/people#hasLastName")
                       (->literal "Davids")))))))

   (test-case
       "Anonymous subject and multiple predicates each with multiple objects"
     (let ((label-maker (test-bnode-label-maker)))
       (parameterize ((blank-node-label-maker label-maker))
         (check-equal?
          (rdf-sub-graph ("http://example.com/v/people#hasName" ("Erin" "Eves"))
                         ("http://example.com/v/people#hasScores" (2 4 6)))
          (set (triple (make-blank-node "test-1")
                       (string->resource "http://example.com/v/people#hasName")
                       (->literal "Erin"))
               (triple (make-blank-node "test-1")
                       (string->resource "http://example.com/v/people#hasName")
                       (->literal "Eves"))
               (triple (make-blank-node "test-1")
                       (string->resource "http://example.com/v/people#hasScores")
                       (->literal 2))
               (triple (make-blank-node "test-1")
                       (string->resource "http://example.com/v/people#hasScores")
                       (->literal 4))
               (triple (make-blank-node "test-1")
                       (string->resource "http://example.com/v/people#hasScores")
                       (->literal 6)))))))))

(define macro-rdf-graph-test-suite
  (test-suite
   "Test macro `rdf-graph`"

   (test-case
       "Single statement in default graph"
     (check-equal?
      (rdf-graph "http://example.com/p/me" "http://example.com/v/people#hasName" "Alice")
      (unnamed-graph
       (set (triple (string->resource "http://example.com/p/me")
                    (string->resource "http://example.com/v/people#hasName")
                    (->literal "Alice"))))))

   (test-case
       "Single statement in named graph"
     (check-equal?
      (rdf-graph "http://example.com/p/peeps" =>
                 "http://example.com/p/me" "http://example.com/v/people#hasName" "Bob")
      (named-graph
       (string->resource "http://example.com/p/peeps")
       (set (triple (string->resource "http://example.com/p/me")
                    (string->resource "http://example.com/v/people#hasName")
                    (->literal "Bob"))))))

   (test-case
       "Multi statement in default graph"
     (check-equal?
      (rdf-graph "http://example.com/p/me"
                 ("http://example.com/v/people#hasFirstName" "Charlie")
                 ("http://example.com/v/people#hasLastName" "Craig"))
      (unnamed-graph
       (set (triple (string->resource "http://example.com/p/me")
                    (string->resource "http://example.com/v/people#hasFirstName")
                    (->literal "Charlie"))
            (triple (string->resource "http://example.com/p/me")
                    (string->resource "http://example.com/v/people#hasLastName")
                    (->literal "Craig"))))))))

;; -------------------------------------------------------------------------------------------------
;; Test Runner
;; -------------------------------------------------------------------------------------------------

(run-tests macro-rdf-sub-graph-test-suite)
(run-tests macro-rdf-graph-test-suite)
