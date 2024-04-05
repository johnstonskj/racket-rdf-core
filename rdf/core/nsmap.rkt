#lang racket/base

(require racket/bool
         racket/contract
         racket/list
         ;; --------------------------------------
         "./namespace.rkt")

(provide (except-out (struct-out nsmap) internal-make-nsmap)
         make-nsmap
         nsmap-empty?
         nsmap-count
         nsmap-has-prefix?
         nsmap-has-default?
         nsmap-ref
         nsmap-ref-default
         nsmap-ref!
         nsmap-set!
         nsmap-set-from!
         nsmap-remove!
         nsmap-update!
         nsmap-clear!
         nsmap-has-url?
         nsmap-prefix-ref
         nsmap-map
         nsmap-names
         nsmap-values
         nsmap->list)

;; -------------------------------------------------------------------------------------------------
;; Namespace maps
;; -------------------------------------------------------------------------------------------------

(struct nsmap ((nshash #:mutable))
  #:sealed
  #:transparent
  #:constructor-name internal-make-nsmap
  #:guard (struct-guard/c (hash/c (or/c ncname? #f) url-absolute?)))

(define/contract (make-nsmap (ns-list '()))
  (->* () ((listof namespace?)) nsmap?)
  (internal-make-nsmap
   (make-hash (map (λ (ns) (cons (namespace-url ns) (namespace-prefix ns)))
                   ns-list))))

;; -------------------------------------------------------------------------------------------------

(define/contract (nsmap-empty? map)
  (-> nsmap? boolean?)
  (hash-empty? (nsmap-nshash map)))

(define/contract (nsmap-count map)
  (-> nsmap? exact-nonnegative-integer?)
  (hash-count (nsmap-nshash map)))

;; -------------------------------------------------------------------------------------------------

(define/contract (nsmap-has-prefix? map prefix)
  (-> nsmap? ncname? boolean?)
  (hash-has-key? (nsmap-nshash map) prefix))

(define/contract (nsmap-has-default? map)
  (-> nsmap? boolean?)
  (hash-has-key? (nsmap-nshash map) #f))

(define/contract (nsmap-ref-default map)
  (-> nsmap? (or/c url-absolute? #f))
  (hash-ref (nsmap-nshash map) #f #f))

(define/contract (nsmap-ref map prefix)
  (-> nsmap? ncname? (or/c url-absolute? #f))
  (hash-ref (nsmap-nshash map) prefix #f))

(define/contract (nsmap-ref! map prefix to-set)
  (-> nsmap? ncname? url-absolute? url-absolute?)
  (hash-ref! (nsmap-nshash map) prefix to-set))

(define/contract (nsmap-set! map prefix url)
  (-> nsmap? ncname? url-absolute? void?)
  (hash-set! (nsmap-nshash map) prefix url))

(define/contract (nsmap-set-from! map ns)
  (-> nsmap? namespace? void?)
  (hash-set! (nsmap-nshash map) (namespace-prefix ns) (namespace-url ns)))

(define/contract (nsmap-remove! map prefix)
  (-> nsmap? ncname? void?)
  (hash-remove! (nsmap-nshash map) prefix))

(define (nsmap-update! map prefix updater)
  (let ((new-hash (hash-update! (nsmap-nshash map) prefix updater #f)))
    (when new-hash
      (set-nsmap-nshash! map new-hash))
    (if new-hash #t #f)))

(define/contract (nsmap-clear! map)
  (-> nsmap? void?)
  (hash-clear! (nsmap-nshash map)))

;; -------------------------------------------------------------------------------------------------

(define (nsmap-find-url map url)
  (findf (λ (pair) (equal? (second pair) url))
         (nsmap->list map)))

(define (nsmap-has-url? map url)
  (not (false? (nsmap-find-url map url))))

(define (nsmap-prefix-ref map url)
  (let ((maybe (nsmap-find-url map url)))
    (if maybe (first maybe) maybe)))

;; -------------------------------------------------------------------------------------------------

(define (nsmap-map map proc (try-order? #f))
  (hash-map (nsmap-nshash map) proc try-order?))

(define (nsmap-names map (try-order? #f))
  (hash-keys (nsmap-nshash map) try-order?))

(define (nsmap-values map (try-order? #f))
  (hash-values (nsmap-nshash map) try-order?))

(define (nsmap->list map (try-order? #f))
  (hash->list (nsmap-nshash map) try-order?))
