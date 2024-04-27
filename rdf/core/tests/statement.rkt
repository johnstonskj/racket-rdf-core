#lang racket/base

(require rackunit
         ;; --------------------------------------
         "../statement.rkt"
         "../io.rkt")

;; -------------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -------------------------------------------------------------------------------------------------

(define statement-test-suite
  (test-suite
   "Tests for module `statement` -- statements"

   (test-case
       "Test Blank Nodes"
     (let ((bnode (make-blank-node)))
       (check-pred blank-node? bnode)
       (check-equal? (substring (blank-node->ntriple-string bnode) 0 2) "_:")
       (check-not-equal? bnode (make-blank-node))))

   ))

;; -------------------------------------------------------------------------------------------------
;; Test Runner
;; -------------------------------------------------------------------------------------------------

(module+ test

    (require rackunit
             rackunit/text-ui)

  (run-tests statement-test-suite))
