#lang racket/base

(require net/url-string
         rackunit
         ;; --------------------------------------
         "../namespace.rkt"
         "../literal.rkt"
         "../statement.rkt"
         "../graph.rkt"
         ;; --------------------------------------
         (only-in "../io.rkt" graph->ntriple-string))

;; -------------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -------------------------------------------------------------------------------------------------

(define graph-test-suite
  (test-suite
   "Test > module `graph`"

   (test-case
       "function `make-sub-graph`"
     (display
      (graph->ntriple-string
       (make-default-graph
        (make-statement-list "http://example.com/p/me"
                             `(("http://example.com/v/people#hasFirstName" (,(string->literal "Me")))
                               ("http://example.com/v/people#hasLastName" (,(string->literal "!")))
                               ("http://example.com/v/people#hasScores" ,(map exact-integer->literal '(2 4 6)))))))))
   (test-case
       "function `skolem-url?` -- correct"
     (let ((test-data '("http://example.com/.well-known/skolem/001"
                        "https://example.com/.well-known/skolem/00/1")))
       (for-each
        (λ (url) (check-pred skolem-url? url))
        (map string->url test-data))))

   (test-case
       "function `graph-skolemize`"
     (let* ((ns (make-namespace "http://example.com/" "ex"))
            (bnode-1 (make-blank-node))
            (bnode-2 (make-blank-node))
            (test-graph (make-default-graph
                         (list
                          (make-statement (namespace-make-url ns "thing") (namespace-make-url ns "hasName") bnode-1)
                          (make-statement bnode-1 (namespace-make-url ns "firstName") (string->literal "spongebob"))
                          (make-statement bnode-1 (namespace-make-url ns "lastName") (string->literal "squarepants"))
                          (make-statement bnode-1 (namespace-make-url ns "hasFriend") bnode-2)
                          (make-statement bnode-2 (namespace-make-url ns "firstName") (string->literal "patrick"))
                          (make-statement bnode-2 (namespace-make-url ns "lastName") (string->literal "star"))))))
       (printf "~a" (graph->ntriple-string test-graph))
       (printf "~a" (graph->ntriple-string (graph-skolemize test-graph "example.org")))))))

;; -------------------------------------------------------------------------------------------------
;; Test Runner
;; -------------------------------------------------------------------------------------------------

(module+ test

  (require rackunit
           rackunit/text-ui)

  (run-tests graph-test-suite))
