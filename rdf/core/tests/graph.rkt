#lang racket/base

(require net/url-string
         rackunit
         ;; --------------------------------------
         "../name.rkt"
         "../namespace.rkt"
         "../literal.rkt"
         "../statement.rkt"
         "../triple.rkt"
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
       (unnamed-graph
        (statement-list "http://example.com/p/me"
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
     (let* ((ns (string->namespace "http://example.com/"))
            (bnode-1 (make-blank-node))
            (bnode-2 (make-blank-node))
            (test-graph (unnamed-graph
                         (list
                          (triple (namespace+name->url ns (string->local-name "thing"))
                                       (namespace+name->url ns (string->local-name "hasName"))
                                       bnode-1)
                          (triple bnode-1
                                       (namespace+name->url ns (string->local-name "firstName"))
                                       (string->literal "spongebob"))
                          (triple bnode-1
                                       (namespace+name->url ns (string->local-name "lastName"))
                                       (string->literal "squarepants"))
                          (triple bnode-1
                                       (namespace+name->url ns (string->local-name "hasFriend"))
                                       bnode-2)
                          (triple bnode-2
                                       (namespace+name->url ns (string->local-name "firstName"))
                                       (string->literal "patrick"))
                          (triple bnode-2
                                       (namespace+name->url ns (string->local-name "lastName"))
                                       (string->literal "star"))))))
       (printf "~a" (graph->ntriple-string test-graph))
       (printf "~a" (graph->ntriple-string (graph-skolemize test-graph "example.org")))))))

;; -------------------------------------------------------------------------------------------------
;; Test Runner
;; -------------------------------------------------------------------------------------------------

(module+ test

  (require rackunit
           rackunit/text-ui)

  (run-tests graph-test-suite))
