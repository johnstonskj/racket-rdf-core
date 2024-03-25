#lang racket/base

(require rackunit
         ;; --------------------------------------
         "../namespace.rkt"
         "../statement.rkt"
         "../io.rkt")

(provide statement-test-suite)

(define statement-test-suite
  (test-suite
   "Tests for module `statement` -- statements"

   (test-case
       "Test Blank Nodes"
     (let ((bnode (make-blank-node)))
       (check-pred blank-node? bnode)
       (check-equal? (substring (blank-node->turtle-string bnode) 0 2) "_:")
       (check-not-equal? bnode (make-blank-node))))

   ))

