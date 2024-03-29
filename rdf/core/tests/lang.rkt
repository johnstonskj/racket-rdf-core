#lang racket/base

(require rackunit
         ;; --------------------------------------
         "../lang.rkt")

;; -------------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -------------------------------------------------------------------------------------------------

(define language-tag-test-suite
  (test-suite
   "Tests for predicate function `language-tag?`"

   (test-case
       "Test Grandfathered identifiers -- set 1"
     (for-each
      (λ (ex) (check-true (language-tag? ex)))
      '("en-GB-oed" "i-ami" "i-bnn" "i-default" "i-enochian" "i-hak" "i-klingon" "i-lux" "i-mingo" "i-navajo" "i-pwn" "i-tao" "i-tay" "i-tsu" "sgn-BE-FR" "sgn-BE-NL" "sgn-CH-DE")))

   (test-case
       "Test Grandfathered identifiers -- set 2"
     (for-each
      (λ (ex) (check-true (language-tag? ex)))
      '("art-lojban" "cel-gaulish" "no-bok" "no-nyn" "zh-guoyu" "zh-hakka" "zh-min" "zh-min-nan" "zh-xiang")))

   ))

;; -------------------------------------------------------------------------------------------------
;; Test Runner
;; -------------------------------------------------------------------------------------------------

(module+ test

  (require rackunit
           rackunit/text-ui)

  (run-tests language-tag-test-suite))
