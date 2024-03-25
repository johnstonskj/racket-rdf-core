#lang racket/base

(require rackunit
         ;; --------------------------------------
         "../statement.rkt"
         "../graph.rkt"
         "../io.rkt")

(provide macro-rdf-sub-graph-test-suite
         macro-rdf-graph-test-suite)

(define macro-rdf-sub-graph-test-suite
  (test-suite
   "Tests for macro `rdf-sub-graph`"

   (test-case
       "Tests for single statement"
     (display
      (graph->ntriple-string
       (make-default-graph
        (rdf-sub-graph "http://example.com/p/me"
                       "http://example.com/v/people#hasName" "Me!")))))

   (test-case
       "Tests for single statement, using parens"
     (display
      (graph->ntriple-string
       (make-default-graph
        (rdf-sub-graph "http://example.com/p/me"
                       ("http://example.com/v/people#hasName" "Me!"))))))

   (test-case
       "Tests for multiple predicates with single objects"
     (display
      (graph->nquad-string
       (make-default-graph
        (rdf-sub-graph "http://example.com/p/me"
                       ("http://example.com/v/people#hasFirstName" "Me")
                       ("http://example.com/v/people#hasLastName" "!"))))))

   (test-case
       "Tests for multiple predicates with single objects, using parens"
     (display
      (graph->nquad-string
       (make-default-graph
        (rdf-sub-graph "http://example.com/p/me"
                       ("http://example.com/v/people#hasFirstName" ("Me"))
                       ("http://example.com/v/people#hasLastName" ("!")))))))

   (test-case
       "Tests for multiple predicates each with multiple objects"
     (display
      (graph->nquad-string
       (make-default-graph
        (rdf-sub-graph "http://example.com/p/me"
                       ("http://example.com/v/people#hasName" ("Me" "!"))
                       ("http://example.com/v/people#hasScores" (2 4 6)))))))

   ;; ----------------------------------------------------------------------------

   (test-case
       "Tests for anonymous subject and single statement"
     (display
      (graph->ntriple-string
       (make-default-graph
        (rdf-sub-graph "http://example.com/v/people#hasName" "Me!")))))

   (test-case
       "Tests for anonymous subject and single statement, using parens"
     (display
      (rdf-sub-graph ("http://example.com/v/people#hasName" "Me!"))))

   (test-case
       "Tests for anonymous subject and multiple predicates with single objects"
     (display
      (graph->nquad-string
       (make-default-graph
        (rdf-sub-graph ("http://example.com/v/people#hasFirstName" "Me")
                       ("http://example.com/v/people#hasLastName" "!"))))))

   (test-case
       "Tests for anonymous subject and multiple predicates with single objects, using parens"
     (display
      (graph->nquad-string
       (make-default-graph
        (rdf-sub-graph ("http://example.com/v/people#hasFirstName" ("Me"))
                       ("http://example.com/v/people#hasLastName" ("!")))))))

   (test-case
       "Tests for anonymous subject and multiple predicates each with multiple objects"
     (display
      (graph->nquad-string
       (make-default-graph
        (rdf-sub-graph ("http://example.com/v/people#hasName" ("Me" "!"))
                       ("http://example.com/v/people#hasScores" (2 4 6)))))))

   ))

(define macro-rdf-graph-test-suite
  (test-suite
   "Tests for macro `rdf-graph`"

   (test-case
       "Tests for single statement in default graph"
     (display
      (graph->ntriple-string
       (rdf-graph "http://example.com/p/me" "http://example.com/v/people#hasName" "Me!"))))

   (test-case
       "Tests for single statement in named graph"
     (display
      (graph->nquad-string
       (rdf-graph "http://example.com/p/peeps" =>
                  "http://example.com/p/me" "http://example.com/v/people#hasName" "Me!"))))

   (test-case
       "Tests for multi statement in default graph"
     (display
      (graph->nquad-string
       (rdf-graph "http://example.com/p/me"
                  ("http://example.com/v/people#hasFirstName" "Me")
                  ("http://example.com/v/people#hasLastName" "!")))))

   ))
