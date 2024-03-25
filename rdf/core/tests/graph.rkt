#lang racket/base

(require net/url-string
         rackunit
         ;; --------------------------------------
         "../namespace.rkt"
         "../statement.rkt"
         "../graph.rkt"
         ;; --------------------------------------
         (only-in "../io.rkt" graph->ntriple-string))

(provide graph-test-suite)

(define graph-test-suite
  (test-suite
   "Test for module `graph`"

   (test-case
       "Tests for function `make-sub-graph`"
     (display
      (graph->ntriple-string
       (make-default-graph
        (make-statement-list "http://example.com/p/me"
                             '(("http://example.com/v/people#hasFirstName" ("Me"))
                               ("http://example.com/v/people#hasLastName" ("!"))
                               ("http://example.com/v/people#hasScores" (2 4 6))))))))
   (test-case
       "Skolem URI check -- correct"
     (let ((test-data '("http://example.com/.well-known/skolem/001"
                        "https://example.com/.well-known/skolem/00/1")))
       (for-each
        (λ (url) (check-pred skolem-url? url))
        (map string->url test-data))))

   (test-case
       "Skolemization"
     (let* ((ns (make-namespace "http://example.com/" "ex"))
            (bnode-1 (make-blank-node))
            (bnode-2 (make-blank-node))
            (test-graph (make-default-graph
                         (list
                          (make-statement (namespace-make-url ns "thing") (namespace-make-url ns "hasName") bnode-1)
                          (make-statement bnode-1 (namespace-make-url ns "firstName") "spongebob")
                          (make-statement bnode-1 (namespace-make-url ns "lastName") "squarepants")
                          (make-statement bnode-1 (namespace-make-url ns "hasFriend") bnode-2)
                          (make-statement bnode-2 (namespace-make-url ns "firstName") "patrick")
                          (make-statement bnode-2 (namespace-make-url ns "lastName") "star")))))
       (printf "~a" (graph->ntriple-string test-graph))
       (printf "~a" (graph->ntriple-string (graph-skolemize test-graph "example.org")))))))
