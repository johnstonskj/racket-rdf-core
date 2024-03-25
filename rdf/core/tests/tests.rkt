#lang racket/base

(require rackunit
         rackunit/text-ui
         ;; --------------------------------------
         "./namespace.rkt"
         "./statement.rkt"
         "./graph.rkt"
         "./graph-macs.rkt"
         "./gq.rkt"
         "./io.rkt")

(run-tests namespace-test-suite)
(run-tests name-test-suite)
(run-tests ncname-test-suite)
(run-tests statement-test-suite)
(run-tests graph-test-suite)
(run-tests graph-query-test-suite)
(run-tests macro-rdf-sub-graph-test-suite)
;;(run-tests macro-rdf-graph-test-suite)
(run-tests core-io-test-suite)
