#lang racket/base

(require net/url-string
         rackunit
         ;; --------------------------------------
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
     (check-exn exn:fail:contract? (λ ()  (make-namespace "" "xmlns")))
     (check-exn exn:fail:contract? (λ ()  (make-namespace "/foo" "xmlns")))
     (check-exn exn:fail:contract? (λ ()  (make-namespace "http://www.w3.org/2000/xmlns/" ""))))

   (test-case
       "Constructor -- bad prefix"
     (check-exn exn:fail:contract? (λ ()  (make-namespace "http://www.w3.org/2000/xmlns/" "")))
     (check-exn exn:fail:contract? (λ ()  (make-namespace "http://www.w3.org/2000/xmlns/" "0day"))))

   (test-case
       "Constructor -- correct"
     (check-pred namespace? (make-namespace "http://www.w3.org/2000/xmlns/" "xmlns"))
     (check-pred namespace? (make-namespace "http://www.w3.org/2000/xmlns/" "_xmlns_"))
     (check-pred namespace? (make-namespace "http://www.w3.org/2000/xmlns/" "day0")))

   (test-case
       "Accessor `namespace-prefix`"
     (check-exn exn:fail:contract? (λ () (namespace-prefix "I am not a namespace")))
     (let ((namespace (make-namespace "http://www.w3.org/2000/xmlns/" "xmlns")))
       (check-equal? (namespace-prefix namespace) "xmlns")))

   (test-case
       "Accessor `namespace-url`"
     (check-exn exn:fail:contract? (λ () (namespace-url "I am not a namespace")))
     (let ((namespace (make-namespace "http://www.w3.org/2000/xmlns/" "xmlns")))
       (check-equal? (url->string (namespace-url namespace)) "http://www.w3.org/2000/xmlns/")))

   (test-case
       "Additional constructor functions"
     (let ((namespace (make-namespace "http://www.w3.org/2000/xmlns/" "xmlns")))
       (check-equal? (url->string (namespace-make-url namespace "base")) "http://www.w3.org/2000/xmlns/base")
       (check-equal? (namespace-make-qname namespace "base") "xmlns:base")
       (check-equal? (namespace-make-default-qname namespace) "xmlns:")))

   ;;(test-case
   ;;    "Print-like functions"
   ;;  (let ((namespace (make-namespace "http://www.w3.org/2000/xmlns/" "xmlns")))
   ;;    (check-equal? (namespace-make-import-statement namespace 'ttl)
   ;;                  "@prefix xmlns: <http://www.w3.org/2000/xmlns/>.\n")))
   ))

(define name-test-suite
  (test-suite
   "Tests for structure type `name`"

   (test-case
       "Constructor -- bad namespace"
     (check-exn exn:fail:contract? (λ ()  (make-name "I am not a namespace" 'thing)))
     (check-exn exn:fail:contract? (λ ()  (make-name (make-namespace "" "") 'thing))))

   (test-case
       "Constructor -- bad name"
     (check-exn exn:fail:contract? (λ ()  (make-name (make-namespace "http://example.com/" "x") "")))
     (check-exn exn:fail:contract? (λ ()  (make-name (make-namespace "http://example.com/" "x") "0"))))

   (test-case
       "Constructor -- correct"
     (check-pred name? (make-name (make-namespace "http://example.com/" "x") "X")))

   (test-case
       "Accessor `name-namespace`"
     (let ((name (make-name (make-namespace "http://example.com/" "x") "X")))
       (check-pred namespace? (name-namespace name))))

   (test-case
       "Accessor `namespace-name`"
     (let ((name (make-name (make-namespace "http://example.com/" "x") "X")))
       (check-equal? (name-name name) "X")))

   (test-case
       "Additional conversion functions"
     (let ((name (make-name (make-namespace "http://example.com/" "x") "X")))
       (check-equal? (url->string (name->url name)) "http://example.com/X")
       (check-equal? (name->qname name) "x:X")))))

(define ncname-test-suite
  (test-suite
   "Tests for predicate/type `ncame?`"

   (test-case
       "Tests for ncname? -- correct names"
     (let ((test-data '("hello" hello "rdf" "Abc" "aBc_1" "abC-1" "ver-0.1.0-stable")))
       (for-each
        (lambda (elt) (check-pred ncname? elt))
        test-data)))))

(define qname-test-suite
  (test-suite
   "Tests for predicate/type `qname?`"

   (test-case
       "Tests for qname? -- correct names"
     (let ((test-data '("hello:world" "rdf:" ":Abc")))
       (for-each
        (lambda (elt) (check-pred ncname? elt))
        test-data)))

   (test-case
       "Tests for qname? -- invalid names"
     (let ((test-data '("goodbye:cruel:world" "Boo")))
       (for-each
        (lambda (elt) (check-pred ncname? elt))
        test-data)))))

;; -------------------------------------------------------------------------------------------------
;; Test Runner
;; -------------------------------------------------------------------------------------------------

(module+ test

    (require rackunit
             rackunit/text-ui)

  (run-tests namespace-test-suite)
  (run-tests name-test-suite)
  (run-tests ncname-test-suite))
