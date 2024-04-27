#lang racket/base

(require racket/contract
         racket/list
         racket/set
         ;; --------------------------------------
         "./graph.rkt"
         "./statement.rkt"
         "./triple.rkt"
         (only-in "./v/sd.rkt" sd:Dataset))


(provide (contract-out
          (struct dataset ((name (or/c resource? #f))
                           (graphs (hash/c graph-name? graph?))))
          (named-dataset (-> resource? (hash/c graph-name? graph?) dataset?))
          (unnamed-dataset (-> (hash/c graph-name? graph?) dataset?))
          (dataset-empty? (-> dataset? boolean?))
          (dataset-count (-> dataset? exact-nonnegative-integer?))
          (dataset-has-named? (-> dataset? subject? boolean?))
          (dataset-has-default? (-> dataset? boolean?))
          (dataset-ref (-> dataset? graph-name? (or/c graph? #f)))
          (dataset-ref-default (-> dataset?  (or/c graph? #f)))
          (dataset-ref! (-> dataset? graph-name? graph? graph?))
          (dataset-set! (-> dataset? graph? void?))
          (dataset-remove! (-> dataset? graph-name? void?))
          (dataset-update! (-> dataset? graph-name? (-> graph? graph?) void?))
          (dataset-clear! (-> dataset? void?))
          ;; --------------------------------------
          (dataset-map (->* (dataset? (-> graph-name? graph? any/c)) (boolean?) (listof any/c)))
          (dataset-names (->* (dataset?) (boolean?) (listof graph-name?)))
          (dataset-values (->* (dataset?) (boolean?) (listof graph?)))
          (dataset->list (-> dataset? (listof (cons/c graph-name? graph?))))
          ;; --------------------------------------
          (dataset-distinct-subjects (-> dataset? (set/c subject?)))
          (dataset-distinct-predicates (-> dataset? (set/c predicate?)))
          (dataset-distinct-objects (-> dataset? (set/c object?)))
          ;; --------------------------------------
          (describe-dataset (-> dataset? (listof statement?)))))

(struct dataset (name (graphs #:mutable))
  #:sealed
  #:transparent
  #:guard (struct-guard/c (or/c resource? #f) (hash/c graph-name? graph?)))

;; -------------------------------------------------------------------------------------------------

(define (unnamed-dataset graphs)
  (dataset #f graphs))

(define (named-dataset name graphs)
  (dataset name graphs))

(define (dataset-empty? ds)
  (hash-empty? (dataset-graphs ds)))

;; -------------------------------------------------------------------------------------------------

(define (dataset-count ds)
  (hash-count (dataset-graphs ds)))

(define (dataset-order graph)
  (set-count (set-union (dataset-distinct-subjects graph) (dataset-distinct-objects graph))))

;; -------------------------------------------------------------------------------------------------

(define (dataset-has-named? ds name)
  (hash-has-key? (dataset-graphs ds) name))

(define (dataset-has-default? ds)
  (hash-has-key? (dataset-graphs ds) #f))

(define (dataset-ref-default ds)
  (hash-ref (dataset-graphs ds) #f #f))

(define (dataset-ref ds name)
  (hash-ref (dataset-graphs ds) name #f))

(define (dataset-ref! ds name to-set)
  ;; TODO assert (equal? (graph-name to-set) name)
  (hash-ref! (dataset-graphs ds) name to-set))

(define (dataset-set! ds graph)
  (hash-set! (dataset-graphs ds) (graph-name graph) graph))

(define (dataset-remove! ds name)
  (hash-remove! (dataset-graphs ds) name))

(define (dataset-update! ds name updater)
  (let ((new-hash (hash-update! (dataset-graphs ds) name updater #f)))
    (when new-hash
      (set-dataset-graphs! ds new-hash))
    (if new-hash #t #f)))

(define (dataset-clear! ds)
  (hash-clear! (dataset-graphs ds)))

;; -------------------------------------------------------------------------------------------------

(define (dataset-map ds proc (try-order? #f))
  (hash-map (dataset-graphs ds) proc try-order?))

(define (dataset-names ds (try-order? #f))
  (hash-keys (dataset-graphs ds) try-order?))

(define (dataset-values ds (try-order? #f))
  (hash-values (dataset-graphs ds) try-order?))

(define (dataset->list ds (try-order? #f))
  (hash->list (dataset-graphs ds) try-order?))

;; -------------------------------------------------------------------------------------------------

(define (dataset-distinct-subjects ds)
  (apply set-union (map (λ (graph) (graph-distinct-subjects graph)) (dataset-graphs ds))))

(define (dataset-distinct-predicates ds)
  (apply set-union (map (λ (graph) (graph-distinct-predicates graph)) (dataset-graphs ds))))

(define (dataset-distinct-objects ds)
  (apply set-union (map (λ (graph) (graph-distinct-objects graph)) (dataset-graphs ds))))

;; -------------------------------------------------------------------------------------------------

(define (describe-dataset ds)
  (append
   (list (type-statement (dataset-name ds) sd:Dataset))
     (flatten
      (map (λ (graph) (describe-graph graph))
           (dataset-graphs ds)))))
