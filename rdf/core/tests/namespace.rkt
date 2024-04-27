#lang racket/base

(require net/url-string
         rackunit
         ;; --------------------------------------
         "../name.rkt"
         "../namespace.rkt")

;; -------------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -------------------------------------------------------------------------------------------------

;;(define (check-nil? val) (check-pred null? val))

(define namespace-test-suite
  (test-suite
   "Tests for structure type `namespaces`"

   (test-case
       "Constructor -- bad URL"
     (check-exn exn:fail:contract? (λ ()  (string->namespace "")))
     (check-exn exn:fail:contract? (λ ()  (string->namespace "/foo"))))

   (test-case
       "Accessor `namespace->url`"
     (check-exn exn:fail:contract? (λ () (namespace->url "I am not a namespace")))
     (let ((namespace (string->namespace "http://www.w3.org/2000/xmlns/")))
       (check-equal? (url->string (namespace->url namespace)) "http://www.w3.org/2000/xmlns/")))

   (test-case
       "Additional constructor functions"
     (let ((namespace (string->namespace "http://www.w3.org/2000/xmlns/")))
       (check-equal? (url->string (namespace+name->url namespace (string->local-name "base")))
                     "http://www.w3.org/2000/xmlns/base")
       ;; TODO (check-equal? (namespace-make-qname namespace "base") "xmlns:base")
       ;; TODO (check-equal? (namespace-make-default-qname namespace) "xmlns:")
       ))
   ))

;; -------------------------------------------------------------------------------------------------
;; Test Runner
;; -------------------------------------------------------------------------------------------------

(module+ test

    (require rackunit
             rackunit/text-ui)

  (run-tests namespace-test-suite))
