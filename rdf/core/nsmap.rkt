#lang racket/base

(require racket/bool
         racket/contract
         racket/set
         ;; --------------------------------------
         net/url-structs
         ;; --------------------------------------
         "./name.rkt"
         "./nsname.rkt"
         "./resource.rkt"
         (only-in "./private/sparql-names.rkt"
                  prefixed-name-separator
                  prefixed-name-split
                  prefixed-name-string?
                  prefix-name-string?
                  prefix-string?))

(provide (contract-out (prefix-string? (-> any/c boolean?))
                       (prefix-name-string? (-> any/c boolean?))
                       (prefix? (-> any/c boolean?))
                       (prefix-empty? (-> prefix? boolean?))
                       (string->prefix (-> (or/c prefix-string? prefix-name-string?) prefix?))
                       (empty-prefix  (-> prefix?))
                       (prefix->string (-> prefix? prefix-string?))
                       (prefix->name-string (-> prefix? prefix-name-string?))
                       (prefix+name->nsname
                        (-> prefix? local-name? nsmap? (or/c nsname? #f)))
                       (prefix+name->resource
                        (-> prefix? local-name? nsmap? (or/c resource-absolute? #f)))
                       ;; --------------------------------------
                       (prefixed-name-separator char?)
                       (prefixed-name-string? (-> any/c boolean?))
                       (struct prefixed-name ((prefix prefix?)
                                              (name local-name?)))
                       (make-prefixed-name
                        (-> (or/c prefix-string? prefix?)
                            (or/c local-name-string? local-name?) prefixed-name?))
                       (string->prefixed-name (-> prefixed-name-string? (or/c prefixed-name? #f)))
                       (prefixed-name->string (-> prefixed-name? prefixed-name-string?))
                       (nsname->prefixed-name
                        (-> nsname? nsmap? (or/c prefixed-name? #f)))
                       (namespace+name->prefixed-name
                        (-> resource? local-name? nsmap? (or/c prefixed-name? #f)))
                       (prefixed-name->nsname
                        (-> prefixed-name? nsmap? (or/c nsname? #f)))
                       (prefixed-name->resource
                        (-> prefixed-name? nsmap? (or/c resource-absolute? #f)))
                       ;; --------------------------------------
                       (struct nsmap ((mapping (hash/c (or/c prefix? #f) resource?))))
                       (make-common-nsmap (-> nsmap?))
                       (make-rdf-only-nsmap (-> nsmap?))
                       (make-rdf+xsd-nsmap (-> nsmap?))
                       (make-nsmap (->* () ((listof (cons/c prefix? resource?))) nsmap?))
                       ;; --------------------------------------
                       (nsmap-shorten (-> nsmap? resource? (or/c prefixed-name? prefix? resource?)))
                       ;; --------------------------------------
                       (nsmap-empty?  (-> nsmap? boolean?))
                       (nsmap-count (-> nsmap? exact-nonnegative-integer?))
                       (nsmap-has-prefix? (-> nsmap? prefix? boolean?))
                       (nsmap-has-default? (-> nsmap? boolean?))
                       (nsmap-ref (-> nsmap? prefix? (or/c resource? #f)))
                       (nsmap-ref-default  (-> nsmap? (or/c resource? #f)))
                       (nsmap-ref! (-> nsmap? prefix? resource? resource?))
                       (nsmap-set! (-> nsmap? prefix? resource? void?))
                       (nsmap-set-default! (-> nsmap? resource? void?))
                       (nsmap-remove! (-> nsmap? prefix? void?))
                       (nsmap-update! (-> nsmap? prefix? (-> resource? resource?) void?))
                       (nsmap-clear! (-> nsmap? void?))
                       ;; --------------------------------------
                       (nsmap-has-namespace? (-> nsmap? resource? boolean?))
                       (nsmap-find-namespace
                        (-> nsmap? resource? (or/c (cons/c prefix? resource?) #f)))
                       (nsmap-prefix-ref (-> nsmap? resource? (or/c prefix? #f)))
                       ;; --------------------------------------
                       (nsmap-map (->* (nsmap? (-> prefix? resource? any)) (boolean?) any/c))
                       (nsmap-prefixes (-> nsmap? (listof prefix?)))
                       (nsmap-namespaces (-> nsmap? (listof resource?)))
                       (nsmap->list (-> nsmap? (listof (cons/c prefix? resource?))))))

;; -------------------------------------------------------------------------------------------------
;; Namespace prefix
;; -------------------------------------------------------------------------------------------------

(define empty-prefix-string (string prefixed-name-separator))

(struct prefix (str) #:transparent)

(define (empty-prefix)
  (prefix empty-prefix-string))

(define (string->prefix str)
  (prefix (if (prefix-name-string? str)
              (string-append str empty-prefix-string)
              str)))

(define (prefix-empty? nsprefix)
  (string=? (prefix-str nsprefix) empty-prefix-string))

(define (prefix->string nsprefix)
  (prefix-str nsprefix))

(define (prefix->name-string nsprefix)
  (let ((prestr (prefix-str nsprefix)))
    (substring prestr 0 (- (string-length prestr) 1))))

(define (prefix+name->nsname prefix name nsmap)
  (let ((ns (nsmap-ref nsmap prefix)))
    (if ns (make-nsname ns name) #f)))

(define (prefix+name->resource prefix name nsmap)
  (nsname->resource (prefix+name->nsname prefix name nsmap)))

;; -------------------------------------------------------------------------------------------------
;; Prefixed Names
;; -------------------------------------------------------------------------------------------------

(struct prefixed-name (prefix name) #:transparent)

(define (make-prefixed-name prefix name)
  (prefixed-name
   (if (prefix? prefix) prefix (string->prefix prefix))
   (if (local-name? name) name (string->local-name name))))

(define (string->prefixed-name s)
  (let ((namespace+name (prefixed-name-split s)))
    (if namespace+name
        (prefixed-name
         (string->prefix (car namespace+name))
         (string->local-name (cdr namespace+name)))
        #f)))

(define (prefixed-name->string pname)
  (string-append (prefix->string (prefixed-name-prefix pname))
                 (local-name->string (prefixed-name-name pname))))

(define (prefixed-name->nsname pname nsmap)
  (prefix+name->nsname nsmap (prefixed-name-prefix pname) (prefixed-name-name pname)))

(define (prefixed-name->resource pname nsmap)
  (prefix+name->resource nsmap (prefixed-name-prefix pname) (prefixed-name-name pname)))

(define (nsname->prefixed-name nsname nsmap)
  (namespace+name->prefixed-name nsmap
                                 (nsname-namespace nsname)
                                 (nsname-name nsname)))

(define (namespace+name->prefixed-name ns name nsmap)
  (let ((prefix (nsmap-prefix-ref nsmap ns)))
    (if prefix (make-prefixed-name prefix name) #f)))

;; -------------------------------------------------------------------------------------------------
;; Namespace maps
;; -------------------------------------------------------------------------------------------------

(struct nsmap ((mapping #:mutable)) #:transparent)

(define (make-nsmap (assocs '()))
  (nsmap (make-hash assocs)))

(define (make-common-nsmap)
  (make-nsmap
   (list (cons (string->prefix "rdf:")
               (string->resource "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
         (cons (string->prefix "rdfs")
               (string->resource "http://www.w3.org/2000/01/rdf-schema#"))
         (cons (string->prefix "xsd")
               (string->resource "http://www.w3.org/2001/XMLSchema#"))
         (cons (string->prefix "owl")
               (string->resource "http://www.w3.org/2002/07/owl#"))
         (cons (string->prefix "dc")
               (string->resource "http://purl.org/dc/elements/1.1/"))
         (cons (string->prefix "dcterms")
               (string->resource "http://purl.org/dc/terms/")))))

(define (make-rdf-only-nsmap)
  (make-nsmap
   (list (cons (string->prefix "rdf:")
               (string->resource "http://www.w3.org/1999/02/22-rdf-syntax-ns#")))))

(define (make-rdf+xsd-nsmap)
  (make-nsmap
   (list (cons (string->prefix "rdf:")
               (string->resource "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
         (cons (string->prefix "xsd")
               (string->resource "http://www.w3.org/2001/XMLSchema#")))))

(define (nsmap-empty? map)
  (hash-empty? (nsmap-mapping map)))

(define (nsmap-count map)
  (hash-count (nsmap-mapping map)))

(define (nsmap-shorten map resource)
  (let ((prefix (if (resource-maybe-namespace? resource)
                    (nsmap-prefix-ref map resource)
                    #f)))
    (let-values (((namespace name) (resource->namespace+name resource)))
      (cond
      (prefix prefix)
      ((and namespace name)
       (let ((prefix (nsmap-prefix-ref map namespace)))
         (if prefix
             (prefixed-name prefix (string->local-name name))
             resource)))
      (else resource)))))

;; -------------------------------------------------------------------------------------------------

(define (nsmap-has-default? map)
  (nsmap-has-prefix? map (empty-prefix)))

(define (nsmap-ref-default map)
  (nsmap-ref map (empty-prefix)))

(define (nsmap-has-prefix? map prefix)
  (hash-has-key? (nsmap-mapping map) prefix))

(define (nsmap-ref map prefix)
  (hash-ref (nsmap-mapping map) prefix #f))

(define (nsmap-ref! map prefix to-set)
  (hash-ref! (nsmap-mapping map) prefix to-set))

(define (nsmap-set! map prefix url)
  (hash-set! (nsmap-mapping map) prefix url))

(define (nsmap-set-default! map url)
  (nsmap-set! map (empty-prefix) url))

(define (nsmap-remove! map prefix)
  (hash-remove! (nsmap-mapping map) prefix))

(define (nsmap-update! map prefix updater)
  (let ((new-hash (hash-update! (nsmap-mapping map) prefix updater #f)))
    (when new-hash
      (set-nsmap-mapping! map new-hash))
    (if new-hash #t #f)))

(define (nsmap-clear! map)
  (hash-clear! (nsmap-mapping map)))

;; -------------------------------------------------------------------------------------------------

(define (nsmap-find-namespace map ns)
  (findf (λ (pair) (equal? (cdr pair) ns))
         (nsmap->list map)))

(define (nsmap-has-namespace? map ns)
  (not (false? (nsmap-find-namespace map ns))))

(define (nsmap-prefix-ref map ns)
  (let ((maybe (nsmap-find-namespace map ns)))
    (if maybe (car maybe) #f)))

;; -------------------------------------------------------------------------------------------------

(define (nsmap-map map proc (try-order? #f))
  (hash-map (nsmap-mapping map) proc try-order?))

(define (nsmap-prefixes map (try-order? #f))
  (hash-keys (nsmap-mapping map) try-order?))

(define (nsmap-namespaces map (try-order? #f))
  (hash-values (nsmap-mapping map) try-order?))

(define (nsmap->list map (try-order? #f))
  (hash->list (nsmap-mapping map) try-order?))
