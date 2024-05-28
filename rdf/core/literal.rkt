#lang racket/base

(require racket/bool
         racket/contract
         racket/date
         racket/string
         ;; --------------------------------------
         langtag
         ;; --------------------------------------
         "./nsname.rkt"
         "./resource.rkt"
         "./v/rdf.rkt"
         "./v/xsd.rkt")

(provide (contract-out (struct literal
                         ((lexical-form string?)
                          (datatype-iri (or/c resource? #f))
                          (language-tag (or/c language-tag? #f)))
                         #:omit-constructor)
                       (make-untyped-literal (-> string? literal?))
                       (make-typed-literal
                        (-> string? (or/c resource-absolute? resource-absolute?) literal?))
                       (make-lang-string-literal (-> string? language-tag? literal?))
                       ;; --------------------------------------
                       (has-datatype-iri? (-> literal? boolean?))
                       (has-language-tag? (-> literal? boolean?))
                       (has-xsd-datatype? (-> literal? boolean?))
                       (is-a? (-> literal? resource-absolute? boolean?))
                       (literal-less-than? (-> literal? literal? boolean?))
                       ;; --------------------------------------
                       (boolean->literal (-> boolean? literal?))
                       (bytes->literal (-> bytes? literal?))
                       (date->literal (-> date? literal?))
                       (datetime->literal (-> date? literal?))
                       (exact-integer->literal (-> exact-integer? literal?))
                       (flonum->literal (-> flonum? literal?))
                       (inexact->literal (-> inexact? literal?))
                       (string->literal (-> string? literal?))
                       (time->literal (-> date? literal?))
                       (->literal
                        (-> (or/c boolean? bytes? date? string? exact-integer? flonum? inexact?)
                            literal?))
                       ;; --------------------------------------
                       (literal-true literal?)
                       (literal-false literal?)
                       (literal-empty-string literal?)
                       (literal-exact-zero literal?)
                       (literal-flonum-zero literal?)
                       (literal-inexact-zero literal?)))

;; ------------------------------------------------------------------------------------------------
;; Literal structure
;; ------------------------------------------------------------------------------------------------

(struct literal (lexical-form datatype-iri language-tag)
  #:transparent
  #:constructor-name internal-make-literal
  #:guard (struct-guard/c string? (or/c resource? #f) (or/c language-tag? #f)))

(define (make-untyped-literal form)
  (internal-make-literal form #f #f))

(define (make-typed-literal form datatype)
  (internal-make-literal form datatype #f))

(define (make-lang-string-literal form language)
  (internal-make-literal form (nsname->resource rdf:langString) language))

;; ------------------------------------------------------------------------------------------------
;; Literal predicates
;; ------------------------------------------------------------------------------------------------

(define (has-datatype-iri? val)
  (not (false? (literal-datatype-iri val))))

(define (has-language-tag? val)
  (not (false? (literal-language-tag val))))

(define (has-xsd-datatype? val)
  (if (has-datatype-iri? val)
      (let-values (((namespace name) (resource->namespace+name (literal-datatype-iri val))))
        (and (string=? (resource->string namespace) xsd-namespace-string)
             (not (false? name))))
      #f))

(define (is-a? val datatype)
  (equal? (literal-datatype-iri val) datatype))

(define (literal-less-than? v1 v2)
  (andmap string<?
          (list (list (literal-lexical-form v1)
                      (literal-lexical-form v2))
                (list (resource->string (literal-datatype-iri v1))
                      (resource->string (literal-datatype-iri v2))
                      (list (literal-language-tag v1) (literal-language-tag v2))))))

;; ------------------------------------------------------------------------------------------------
;; Literal conversions
;; ------------------------------------------------------------------------------------------------

(define (boolean->literal val)
  (make-typed-literal (if (false? val) "false" "true") (nsname->resource xsd:boolean)))

(define (bytes->literal val)
  (make-typed-literal val (nsname->resource xsd:hex-binary)))

(define (date->literal val)
  (parameterize ((date-display-format 'iso-8601))
    (make-typed-literal (date->string val) val (nsname->resource xsd:date))))

(define (datetime->literal val)
  (parameterize ((date-display-format 'iso-8601))
    (make-typed-literal (date->string val #t) val (nsname->resource xsd:date-time))))

(define (exact-integer->literal val)
  (make-typed-literal (format "~a" val) (nsname->resource xsd:integer)))

(define (flonum->literal val)
  (make-typed-literal (format "~a" val)
                      (nsname->resource (if (single-flonum? val) xsd:float xsd:double))))

(define (inexact->literal val)
  (make-typed-literal (format "~a" val) (nsname->resource xsd:decimal)))

(define (string->literal val)
  (make-typed-literal val (nsname->resource xsd:string)))

(define (time->literal val)
  (make-typed-literal (format "~a" val) (nsname->resource xsd:time)))

(define (->literal val)
  (cond
    ((boolean? val) (boolean->literal val))
    ((bytes? val) (bytes->literal val))
    ((date? val) (datetime->literal val))
    ((string? val) (string->literal val))
    ((exact-integer? val) (exact-integer->literal val))
    ((flonum? val) (flonum->literal val))
    ((inexact? val) (inexact->literal val))))

;; ------------------------------------------------------------------------------------------------
;; Literal values
;; ------------------------------------------------------------------------------------------------

(define literal-true (boolean->literal #t))
(define literal-false (boolean->literal #f))
(define literal-empty-string (string->literal ""))
(define literal-exact-zero (exact-integer->literal 0))
(define literal-flonum-zero (flonum->literal 0.0))
(define literal-inexact-zero (inexact->literal 0.0))
