#lang racket/base

(require racket/list
         ;; --------------------------------------
         rackunit
         rackunit/text-ui
         ;; --------------------------------------
         "../statement.rkt"
         "../io.rkt"
         ;; --------------------------------------
         "./shared.rkt")

;; -------------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -------------------------------------------------------------------------------------------------

(define statement-test-suite
  (test-suite
   "Test module `statement`"

   (test-case
       "function `make-blank-node` (auto)"
     (let ((bnode (make-blank-node)))
       (check-pred blank-node? bnode)
       (check-equal? (substring (blank-node->ntriple-string bnode) 0 2) "_:")
       (check-not-equal? bnode (make-blank-node))))

   (test-case
       "function `make-blank-node` (assigned)"
     (let ((bnode (make-blank-node "hello")))
       (check-pred blank-node? bnode)
       (check-equal? (blank-node->ntriple-string bnode) "_:hello")
       (check-equal? bnode (make-blank-node "hello"))))

   (test-case
       "parameter `blank-node-label-maker`"
     (let ((label-maker (test-bnode-label-maker)))
       (parameterize ((blank-node-label-maker label-maker))
         (for-each (λ (x) (check-equal?
                           (blank-node->string (make-blank-node))
                           (format "test-~a" x)))
                   (range 1 10)))))

   (test-case
       "function gen:equal?"
     (let ((b1 (make-blank-node "B1"))
           (b2 (make-blank-node "B2"))
           (b3 (make-blank-node "B1")))
       (check-equal? (equal? b1 b2) #f)
       (check-equal? (= (equal-hash-code b1) (equal-hash-code b2)) #f)
       (check-equal? (equal? b1 b3) #t)
       (check-equal? (= (equal-hash-code b1) (equal-hash-code b3)) #t)))

   (test-case
       "function blank-node<?"
     (let ((b1 (make-blank-node "B1"))
           (b2 (make-blank-node "B2"))
           (b3 (make-blank-node "B3")))
       (check-equal? (blank-node<? b1 b2) #t)
       (check-equal? (blank-node<? b1 b3) #t)
       (check-equal? (blank-node<? b1 b1) #f)
       (check-equal? (blank-node<? b2 b1) #f)
       (check-equal? (blank-node<? b3 b1) #f)))

   ))

;; -------------------------------------------------------------------------------------------------
;; Test Runner
;; -------------------------------------------------------------------------------------------------

(run-tests statement-test-suite)
