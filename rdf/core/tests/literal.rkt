#lang racket/base

(require rackunit
         rackunit/text-ui
         ;; --------------------------------------
         "../nsname.rkt"
         "../literal.rkt"
         "../v/xsd.rkt")

;; -------------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -------------------------------------------------------------------------------------------------

(define literal-test-suite
  (test-suite
   "Tests module `literal`"

   (test-case
       "function `boolean->literal`"
     (let ((literal (boolean->literal #t)))
       (check-equal? (literal-lexical-form literal) "true")
       (check-equal? (literal-datatype-iri literal) (nsname->resource xsd:boolean))
       (check-equal? (literal-language-tag literal) #f))
     (let ((literal (boolean->literal #f)))
       (check-equal? (literal-lexical-form literal) "false")
       (check-equal? (literal-datatype-iri literal) (nsname->resource xsd:boolean))
       (check-equal? (literal-language-tag literal) #f))
     (let ((literal (->literal #t)))
       (check-equal? (literal-lexical-form literal) "true")
       (check-equal? (literal-datatype-iri literal) (nsname->resource xsd:boolean))
       (check-equal? (literal-language-tag literal) #f))
     (let ((literal (->literal #f)))
       (check-equal? (literal-lexical-form literal) "false")
       (check-equal? (literal-datatype-iri literal) (nsname->resource xsd:boolean))
       (check-equal? (literal-language-tag literal) #f)))))

;; -------------------------------------------------------------------------------------------------
;; Test Runner
;; -------------------------------------------------------------------------------------------------

(run-tests literal-test-suite)
