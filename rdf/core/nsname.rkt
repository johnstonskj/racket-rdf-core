#lang racket/base

(require racket/bool
         racket/contract
         racket/function
         racket/list
         racket/string
         ;; --------------------------------------
         (only-in net/url-structs
                  path/param
                  path/param-path)
         ;; --------------------------------------
         "./name.rkt"
         "./resource.rkt")

(provide (contract-out (struct nsname ((namespace resource-absolute?)
                                       (name local-name?)))
                       (make-nsname
                        (-> (or/c string? resource-absolute?)
                            (or/c local-name-string? local-name?)
                            nsname?))
                       (resource->nsname (-> resource-absolute? (or/c nsname? #f)))
                       (nsname-make-nsname
                        (-> nsname? (or/c local-name-string? local-name?) nsname?))
                       (nsname->resource (-> nsname? resource-absolute?))
                       (nsname->string (-> nsname? string?))))

;; -------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------

(struct nsname (namespace name) #:transparent)

(define (make-nsname ns name)
  (nsname (if (string? ns)
              (string->resource ns)
              ns)
          (if (local-name? name)
              name
              (string->local-name name))))

(define (resource->nsname url)
  (let-values (((namespace name) (resource->namespace+name url)))
    (if (and namespace name)
        (make-nsname namespace name)
        #f)))

(define (nsname-make-nsname from name)
  (nsname (nsname-namespace from)
          (if (local-name? name) name (string->local-name name))))

(define (nsname->resource ns)
  (resource-append-name (nsname-namespace ns) (nsname-name ns)))

(define nsname->string (compose resource->string nsname->resource))
