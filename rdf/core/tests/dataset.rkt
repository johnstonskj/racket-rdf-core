#lang racket/base

(require racket/list
         racket/set
         ;; --------------------------------------
         rackunit
         rackunit/text-ui
         ;; --------------------------------------
         "../dataset.rkt"
         "../graph.rkt"
         "../literal.rkt"
         "../nsname.rkt"
         "../quad.rkt"
         "../statement.rkt"
         "../v/rdf.rkt")

;; -------------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -------------------------------------------------------------------------------------------------

(define dataset-test-suite
  (test-suite
   "Test module `dataset`"

   (test-case
       "function `quad-set->dataset`"
     (let* ((predicate (nsname->resource rdf:value))
            (quad-list (map
                        (λ (input)
                          (quad (first input)
                                predicate
                                (exact-integer->literal (second input))
                                (third input)))
                        (cartesian-product
                         (list (make-blank-node "g1") (make-blank-node "g2") (make-blank-node "g3"))
                         '(11 22 33)
                         (list (make-blank-node "s1") (make-blank-node "s2") (make-blank-node "s3")))))
            (dataset (quad-set->dataset (list->set quad-list))))
       (check-equal? (dataset-count dataset) 3)
       (for-each (λ (graph) (check-equal? (graph-count graph) 9))
                 (dataset-values dataset))))
   ))

;; -------------------------------------------------------------------------------------------------
;; Test Runner
;; -------------------------------------------------------------------------------------------------

(run-tests dataset-test-suite)
