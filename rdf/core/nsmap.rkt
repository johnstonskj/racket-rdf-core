#lang racket/base

(require racket/bool
         racket/contract
         racket/list
         ;; --------------------------------------
         "./name.rkt"
         "./namespace.rkt"
         (only-in "./private/strings.rkt"
                  string-empty?)
         (only-in "./private/sparql-names.rkt"
                  prefixed-name-separator
                  prefixed-name-split
                  prefixed-name-string?
                  prefix-name-string?
                  prefix-string?))

(provide (contract-out
          (prefix-string? (-> any/c boolean?))
          (prefix-name-string? (-> any/c boolean?))
          (prefix? (-> any/c boolean?))
          (prefix-empty? (-> prefix? boolean?))
          (string->prefix (-> (or/c prefix-string? prefix-name-string?) prefix?))
          (empty-prefix  (-> prefix?))
          (prefix->string (-> prefix? prefix-string?))
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
          (namespaced-name->prefixed-name
           (-> nsmap? nsname? (or/c prefixed-name? #f)))
          (namespace+name->prefixed-name
           (-> nsmap? namespace? local-name? (or/c prefixed-name? #f)))
          (prefixed-name->nsname
           (-> nsmap? local-name? (or/c nsname? #f)))
          (prefix+name->nsname
           (-> nsmap? prefix? local-name? (or/c nsname? #f)))
          (prefixed-name->url
           (-> nsmap? local-name? (or/c url-absolute? #f)))
          (prefix+name->url
           (-> nsmap? prefix? local-name? (or/c url-absolute? #f)))
          ;; --------------------------------------
          ;; --------------------------------------
          (struct nsmap ((mapping (hash/c (or/c prefix? #f) namespace?))))
          (make-common-nsmap (-> nsmap?))
          (make-nsmap (->* () ((listof (cons/c prefix? namespace?))) nsmap?))
          (nsmap-empty?  (-> nsmap? boolean?))
          (nsmap-count (-> nsmap? exact-nonnegative-integer?))
          (nsmap-has-prefix? (-> nsmap? prefix? boolean?))
          (nsmap-has-default? (-> nsmap? boolean?))
          (nsmap-ref (-> nsmap? prefix? (or/c namespace? #f)))
          (nsmap-ref-default  (-> nsmap? (or/c namespace? #f)))
          (nsmap-ref! (-> nsmap? prefix? namespace? namespace?))
          (nsmap-set! (-> nsmap? prefix? namespace? void?))
          (nsmap-remove! (-> nsmap? prefix? void?))
          (nsmap-update! (-> nsmap? prefix? (-> namespace? namespace?) void?))
          (nsmap-clear! (-> nsmap? void?))
          (nsmap-has-namespace? (-> nsmap? namespace? boolean?))
          (nsmap-find-namespace (-> nsmap? namespace? (or/c (cons/c prefix? namespace?) #f)))
          (nsmap-prefix-ref (-> nsmap? namespace? (or/c prefix? #f)))
          (nsmap-map (->* (nsmap? (-> (cons/c prefix? namespace?))) (boolean?) any/c))
          (nsmap-names (-> nsmap? (listof prefix?)))
          (nsmap-values (-> nsmap? (listof namespace?)))
          (nsmap->list (-> nsmap? (listof (cons/c prefix? namespace?))))))

;; -------------------------------------------------------------------------------------------------
;; Namespace prefix
;; -------------------------------------------------------------------------------------------------

(define empty-prefix-string (string prefixed-name-separator))

;; prefix-string?

;; prefix-name-string?

(struct prefix (str)
  #:transparent
  #:guard (struct-guard/c prefix-string?))

(define (prefix-empty? nsprefix)
  (string=? (prefix-str nsprefix) empty-prefix-string))

(define (string->prefix str)
  (prefix (if (prefix-name-string? str)
              (string-append str empty-prefix-string)
              str)))

(define (empty-prefix)
  (prefix empty-prefix-string))

(define (prefix->string nsprefix)
  (prefix-str nsprefix))

;; -------------------------------------------------------------------------------------------------
;; Prefixed Names
;; -------------------------------------------------------------------------------------------------

;; prefixed-name-separator

;; prefixed-name-string?

(struct prefixed-name (prefix name)
  #:transparent
  #:guard (struct-guard/c prefix? local-name?))

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

(define (prefixed-name->string name)
  (string-append (prefix->string (prefixed-name-prefix name))
                 (local-name->string (prefixed-name-name name))))

(define (prefixed-name->nsname nsmap name)
  (prefix+name->nsname nsmap (prefixed-name-prefix name) (prefixed-name-name name)))

(define (prefix+name->nsname nsmap prefix name)
  (let ((ns (nsmap-ref nsmap prefix)))
    (if ns (make-nsname ns name) #f)))

(define (prefixed-name->url nsmap name)
  (prefix+name->url nsmap (prefixed-name-prefix name) (prefixed-name-name name)))

(define (prefix+name->url nsmap prefix name)
  (let ((ns (nsmap-ref nsmap prefix)))
    (if ns (namespace+name->url ns name) #f)))

(define (namespaced-name->prefixed-name nsmap nsname)
  (namespace+name->prefixed-name nsmap
                                 (nsname-namespace nsname)
                                 (nsname-name nsname)))

(define (namespace+name->prefixed-name nsmap ns name)
  (let ((prefix (nsmap-prefix-ref nsmap ns)))
    (if prefix (make-prefixed-name prefix name) #f)))

;; -------------------------------------------------------------------------------------------------
;; Namespace maps
;; -------------------------------------------------------------------------------------------------

(struct nsmap ((mapping #:mutable))
  #:transparent
  #:guard (struct-guard/c (hash/c (or/c prefix? #f) namespace?)))

(define (make-nsmap (assocs '()))
  (nsmap (make-hash assocs)))

(define (make-common-nsmap)
  (make-nsmap
   (list (cons (string->prefix "rdf:")
               (string->namespace "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
         (cons (string->prefix "rdfs")
               (string->namespace "http://www.w3.org/2000/01/rdf-schema#"))
         (cons (string->prefix "xsd")
               (string->namespace "http://www.w3.org/2001/XMLSchema#"))
         (cons (string->prefix "owl")
               (string->namespace "http://www.w3.org/2002/07/owl#"))
         (cons (string->prefix "dc")
               (string->namespace "http://purl.org/dc/elements/1.1/"))
         (cons (string->prefix "dcterms")
               (string->namespace "http://purl.org/dc/terms/")))))

(define (nsmap-empty? map)
  (hash-empty? (nsmap-mapping map)))

(define (nsmap-count map)
  (hash-count (nsmap-mapping map)))

;; -------------------------------------------------------------------------------------------------

(define (nsmap-has-prefix? map prefix)
  (hash-has-key? (nsmap-mapping map) prefix))

(define (nsmap-has-default? map)
  (hash-has-key? (nsmap-mapping map) #f))

(define (nsmap-ref-default map)
  (hash-ref (nsmap-mapping map) #f #f))

(define (nsmap-ref map prefix)
  (hash-ref (nsmap-mapping map) prefix #f))

(define (nsmap-ref! map prefix to-set)
  (hash-ref! (nsmap-mapping map) prefix to-set))

(define (nsmap-set! map prefix url)
  (hash-set! (nsmap-mapping map) prefix url))

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

(define (nsmap-names map (try-order? #f))
  (hash-keys (nsmap-mapping map) try-order?))

(define (nsmap-values map (try-order? #f))
  (hash-values (nsmap-mapping map) try-order?))

(define (nsmap->list map (try-order? #f))
  (hash->list (nsmap-mapping map) try-order?))
