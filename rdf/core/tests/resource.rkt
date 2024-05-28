#lang racket/base

(require racket/list
         ;; --------------------------------------
         rackunit
         rackunit/text-ui
         ;; --------------------------------------
         "../resource.rkt")

;; -------------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -------------------------------------------------------------------------------------------------

(define test-url-values
  '("http://example.com"
    "http://example.org:80"
    "http://example.com/"
    "http://example.org:80/"
    "http://example.com/ns/people"
    "http://example.org/ns/people/"
    "http://example.com/ns/people#"
    "http://example.org/ns/people#me"
    "https://example.com"
    "https://example.org:443"
    "https://example.com/"
    "https://example.org:443/"
    "https://example.com/ns/people"
    "https://example.org/ns/people/"
    "https://example.com/ns/people#"
    "https://example.org/ns/people#me"
    "/ns/people"
    "/ns/people/"
    "/ns/people#"
    "/ns/people#me"
    "ns/people"
    "ns/people/"
    "ns/people#"
    "ns/people#me"
    "name"
    "name#more"
    "#you"))

(define (check-resource-fn check-fn (results #f))
  (let ((results (if results
                     results
                     (make-list (length test-url-values) #f))))
    (for-each
     (λ (url result) (display ".") (check-fn url result))
     test-url-values
     results)
    (newline)))

(define resource-test-suite
  (test-suite
   "Module `resource`"

   (test-case
       ""
     (test-case
       "function `string->resource` and `resource->string`"
     (check-resource-fn
      (λ (url _) (check-equal? (resource->string (string->resource url)) url)))))

   (test-case
       "function `resource-scheme`"
     (check-resource-fn
      (λ (url result) (let ((resource (string->resource url)))
                        (check-equal? (resource-scheme resource) result)))
      (append (make-list 8 "http")
              (make-list 8 "https")
              (make-list 11 #f))))

   (test-case
       "function `resource-path-absolute?`"
     (check-resource-fn
      (λ (url result) (let ((resource (string->resource url)))
                        (check-equal? (resource-path-absolute? resource) result)))
      (append (make-list 20 #t)
              (make-list 7 #f))))

   (test-case
       "function `resource-absolute?`"
     (check-resource-fn
      (λ (url result) (let ((resource (string->resource url)))
                        (check-equal? (resource-absolute? resource) result)))
      (append (make-list 16 #t)
              (make-list 11 #f))))

   (test-case
       "function `resource-maybe-namespace?`"
     (check-resource-fn
      (λ (url result) (let ((resource (string->resource url)))
                        (check-equal? (resource-maybe-namespace? resource) result)))
      '(#f #f #t #t #f #t #t #f
        #f #f #t #t #f #t #t #f
        #f #f #f #f #f #f #f #f
        #f #f #f)))

   (test-case
       "function `resource-maybe-nsname?`"
     (check-resource-fn
      (λ (url result) (let ((resource (string->resource url)))
                        (check-equal? (resource-maybe-nsname? resource) result)))
      '(#f #f #f #f #t #f #f #t
        #f #f #f #f #t #f #f #t
        #t #f #f #t #t #f #f #t
        #t #t #t)))

   (test-case
       "function `resource->namespace+name`"
     (check-resource-fn
      (λ (url result) (let ((resource (string->resource url)))
                        (let-values (((ns name) (resource->namespace+name resource)))
                          (check-equal? (cons ns name) result))))
      (list (cons (string->resource "http://example.com") #f)
            (cons (string->resource "http://example.org:80") #f)
            (cons (string->resource "http://example.com/") #f)
            (cons (string->resource "http://example.org:80/") #f)
            (cons (string->resource "http://example.com/ns/") "people")
            (cons (string->resource "http://example.org/ns/people/") #f)
            (cons (string->resource "http://example.com/ns/people#") #f)
            (cons (string->resource "http://example.org/ns/people#") "me")
            (cons (string->resource "https://example.com") #f)
            (cons (string->resource "https://example.org:443") #f)
            (cons (string->resource "https://example.com/") #f)
            (cons (string->resource "https://example.org:443/") #f)
            (cons (string->resource "https://example.com/ns/") "people")
            (cons (string->resource "https://example.org/ns/people/") #f)
            (cons (string->resource "https://example.com/ns/people#") #f)
            (cons (string->resource "https://example.org/ns/people#") "me")
            (cons (string->resource "/ns/") "people")
            (cons (string->resource "/ns/people/") #f)
            (cons (string->resource "/ns/people#") #f)
            (cons (string->resource "/ns/people#") "me")
            (cons (string->resource "ns/") "people")
            (cons (string->resource "ns/people/") #f)
            (cons (string->resource "ns/people#") #f)
            (cons (string->resource "ns/people#") "me")
            (cons (string->resource "") "name")
            (cons (string->resource "name#") "more")
            (cons (string->resource "#") "you"))))))

;; -------------------------------------------------------------------------------------------------
;; Test Runner
;; -------------------------------------------------------------------------------------------------

(run-tests resource-test-suite)
