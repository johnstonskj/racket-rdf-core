#lang racket/base

(require rackunit
         ;; --------------------------------------
         "../literal.rkt"
         "../statement.rkt"
         "../graph.rkt"
         "../io.rkt")

(provide macro-rdf-sub-graph-test-suite
         macro-rdf-graph-test-suite)

(define macro-rdf-sub-graph-test-suite
  (test-suite
   "Tests macro `rdf-sub-graph`"

   (test-case
       "Single statement"
     (display
      (graph->ntriple-string
       (make-default-graph
        (rdf-sub-graph "http://example.com/p/me"
                       "http://example.com/v/people#hasName" "Me!")))))

   (test-case
       "Single statement, using parens"
     (display
      (graph->ntriple-string
       (make-default-graph
        (rdf-sub-graph "http://example.com/p/me"
                       ("http://example.com/v/people#hasName" ("Me!")))))))

   (test-case
       "Multiple predicates with single objects"
     (display
      (graph->ntriple-string
       (make-default-graph
        (rdf-sub-graph "http://example.com/p/me"
                       ("http://example.com/v/people#hasFirstName" "Me")
                       ("http://example.com/v/people#hasLastName" "!"))))))

   (test-case
       "Tests for multiple predicates with single objects, using parens"
     (display
      (graph->ntriple-string
       (make-default-graph
        (rdf-sub-graph "http://example.com/p/me"
                       ("http://example.com/v/people#hasFirstName" ("Me"))
                       ("http://example.com/v/people#hasLastName" ("!")))))))

   (test-case
       "Multiple predicates each with multiple objects"
     (display
      (graph->ntriple-string
       (make-default-graph
        (rdf-sub-graph "http://example.com/p/me"
                       ("http://example.com/v/people#hasName" ("Me" "!"))
                       ("http://example.com/v/people#hasScores" (2 4 6)))))))

   ;; ----------------------------------------------------------------------------

   (test-case
       "Anonymous subject and single statement"
     (display
      (graph->ntriple-string
       (make-default-graph
        (rdf-sub-graph "http://example.com/v/people#hasName" "Me!")))))

   (test-case
       "Anonymous subject and single statement, using parens"
     (display
      (map statement->ntriple-string
           (rdf-sub-graph ("http://example.com/v/people#hasName" ("Me!"))))))

   (test-case
       "Anonymous subject and multiple predicates with single objects"
     (display
      (graph->nquad-string
       (make-default-graph
        (rdf-sub-graph ("http://example.com/v/people#hasFirstName" "Me")
                       ("http://example.com/v/people#hasLastName" "!"))))))

   (test-case
       "Anonymous subject and multiple predicates with single objects, using parens"
     (display
      (graph->nquad-string
       (make-default-graph
        (rdf-sub-graph ("http://example.com/v/people#hasFirstName" ("Me"))
                       ("http://example.com/v/people#hasLastName" ("!")))))))

   (test-case
       "Anonymous subject and multiple predicates each with multiple objects"
     (display
      (graph->nquad-string
       (make-default-graph
        (rdf-sub-graph ("http://example.com/v/people#hasName" ("Me" "!"))
                       ("http://example.com/v/people#hasScores" (2 4 6)))))))

   ))

(define macro-rdf-graph-test-suite
  (test-suite
   "Test macro `rdf-graph`"

   (test-case
       "Single statement in default graph"
     (display
      (graph->ntriple-string
       (rdf-graph "http://example.com/p/me" "http://example.com/v/people#hasName" "Me!"))))

   (test-case
       "Single statement in named graph"
     (display
      (graph->nquad-string
       (rdf-graph "http://example.com/p/peeps" =>
                  "http://example.com/p/me" "http://example.com/v/people#hasName" "Me!"))))

   (test-case
       "Multi statement in default graph"
     (display
      (graph->nquad-string
       (rdf-graph "http://example.com/p/me"
                  ("http://example.com/v/people#hasFirstName" "Me")
                  ("http://example.com/v/people#hasLastName" "!")))))

   ))

;; -------------------------------------------------------------------------------------------------
;; Test Runner
;; -------------------------------------------------------------------------------------------------

(module+ test

  (require rackunit
           rackunit/text-ui)

  (run-tests macro-rdf-sub-graph-test-suite)
  (run-tests macro-rdf-graph-test-suite))
