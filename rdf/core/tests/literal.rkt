#lang racket/base

(require rackunit
         ;; --------------------------------------
         "../namespace.rkt"
         "../literal.rkt"
         (prefix-in xsd: (except-in "../v/xsd.rkt" *namespace*)))

;; -------------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -------------------------------------------------------------------------------------------------

(define literal-test-suite
  (test-suite
   "Tests for literal structure"

   (test-case
       "Check Boolean to Literal"
     (let ((literal (boolean->literal #t)))
       (check-equal? (literal-lexical-form literal) "true")
       (check-equal? (literal-datatype-iri literal) (name->url xsd:boolean))
       (check-equal? (literal-language-tag literal) #f))
     (let ((literal (boolean->literal #f)))
       (check-equal? (literal-lexical-form literal) "false")
       (check-equal? (literal-datatype-iri literal) (name->url xsd:boolean))
       (check-equal? (literal-language-tag literal) #f)))
   ))

;; -------------------------------------------------------------------------------------------------
;; Test Runner
;; -------------------------------------------------------------------------------------------------

(module+ test

  (require rackunit
           rackunit/text-ui)

  (run-tests literal-test-suite))
