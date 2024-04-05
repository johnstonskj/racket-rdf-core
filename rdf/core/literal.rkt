#lang racket/base

(require racket/bool
         racket/contract
         racket/date
         racket/string
         ;; --------------------------------------
         net/url-string
         net/url-structs
         ;; --------------------------------------
         langtag
         ;; --------------------------------------
         "./namespace.rkt"
         "./v/rdf.rkt"
         "./v/xsd.rkt")

(provide literal?
         literal-lexical-form
         literal-datatype-iri
         literal-language-tag
         make-untyped-literal
         make-typed-literal
         make-lang-string-literal
         ;; --------------------------------------
         has-datatype-iri?
         has-language-tag?
         has-xsd-datatype?
         is-a?
         ;; --------------------------------------
         boolean->literal
         bytes->literal
         date->literal
         datetime->literal
         exact-integer->literal
         flonum->literal
         inexact->literal
         string->literal
         time->literal
         ->literal
         ;; --------------------------------------
         literal-true
         literal-false
         literal-empty-string
         literal-exact-zero
         literal-flonum-zero
         literal-inexact-zero)

;; ------------------------------------------------------------------------------------------------
;; Literal structure
;; ------------------------------------------------------------------------------------------------

(struct literal (lexical-form datatype-iri language-tag)
  #:sealed
  #:transparent
  #:constructor-name internal-make-literal
  #:guard (struct-guard/c string? (or/c url? #f) (or/c language-tag? #f)))

(define/contract (make-untyped-literal form)
  (-> string? literal?)
  (internal-make-literal form #f #f))

(define/contract (make-typed-literal form datatype)
  (-> string? url-absolute? literal?)
  (internal-make-literal form datatype #f))

(define/contract (make-lang-string-literal form language)
  (-> string? language-tag? literal?)
  (internal-make-literal form (name->url rdf:lang-String) language))

;; ------------------------------------------------------------------------------------------------
;; Literal predicates
;; ------------------------------------------------------------------------------------------------

(define/contract (has-datatype-iri? val)
  (-> literal? boolean?)
  (not (false? (literal-datatype-iri val))))

(define/contract (has-language-tag? val)
  (-> literal? boolean?)
  (not (false? (literal-language-tag val))))

(define/contract (has-xsd-datatype? val)
  (-> literal? boolean?)
  (if (has-datatype-iri? val)
      (string-prefix? (url->string (literal-datatype-iri val))
                      (url->string (namespace-url (name-namespace xsd:any-type))))
      #f))

(define/contract (is-a? val datatype)
  (-> literal? url-absolute? boolean?)
  (equal? (literal-datatype-iri val) datatype))

;; ------------------------------------------------------------------------------------------------
;; Literal conversions
;; ------------------------------------------------------------------------------------------------

(define/contract (boolean->literal val)
  (-> boolean? literal?)
  (make-typed-literal (if (false? val) "false" "true") (name->url xsd:boolean)))

(define/contract (bytes->literal val)
  (-> bytes? literal?)
  (make-typed-literal val (name->url xsd:hex-binary)))

(define/contract (date->literal val)
  (-> date? literal?)
  (parameterize ((date-display-format 'iso-8601))
    (make-typed-literal (date->string val) val (name->url xsd:date))))

(define/contract (datetime->literal val)
  (-> date? literal?)
  (parameterize ((date-display-format 'iso-8601))
    (make-typed-literal (date->string val #t) val (name->url xsd:date-time))))

(define/contract (exact-integer->literal val)
  (-> exact-integer? literal?)
  (make-typed-literal (format "~a" val) (name->url xsd:integer)))

(define/contract (flonum->literal val)
  (-> flonum? literal?)
  (make-typed-literal (format "~a" val)
                      (name->url (if (single-flonum? val) xsd:float xsd:double))))

(define/contract (inexact->literal val)
  (-> inexact? literal?)
  (make-typed-literal (format "~a" val) (name->url xsd:decimal)))

(define/contract (string->literal val)
  (-> string? literal?)
  (make-typed-literal val (name->url xsd:string)))

(define/contract (time->literal val)
  (-> date? literal?)
  (make-typed-literal (format "~a" val) (name->url xsd:time)))

(define/contract (->literal val)
  (-> (or/c boolean? bytes? date? string? exact-integer? flonum? inexact?) literal?)
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
