#lang racket/base

(require racket/contract
         racket/list
         ;; --------------------------------------
         "./graph.rkt"
         "./statement.rkt"
         "./triple.rkt"
         (prefix-in sd: (only-in "./v/sd.rkt" Dataset)))

(provide (struct-out dataset)
         dataset-empty?
         dataset-count
         dataset-has-key?
         dataset-has-default?
         dataset-ref
         dataset-ref-default
         dataset-ref!
         dataset-set!
         dataset-remove!
         dataset-update!
         dataset-clear!
         dataset-map
         dataset-names
         dataset-values
         dataset->list)

(struct dataset (name (graphs #:mutable))
  #:sealed
  #:transparent
  #:constructor-name make-dataset
  #:guard (struct-guard/c resource? (hash/c graph-name? graph?)))

(define/contract (dataset-empty? ds)
  (-> dataset? boolean?)
  (hash-empty? (dataset-graphs ds)))

(define/contract (dataset-count ds)
  (-> dataset? exact-nonnegative-integer?)
  (hash-count (dataset-graphs ds)))

(define/contract (dataset-has-key? ds name)
  (-> dataset? subject? boolean?)
  (hash-has-key? (dataset-graphs ds) name))

(define/contract (dataset-has-default? ds)
  (-> dataset? boolean?)
  (hash-has-key? (dataset-graphs ds) #f))

(define/contract (dataset-ref-default ds)
  (-> dataset? (or/c graph? #f))
  (hash-ref (dataset-graphs ds) #f #f))

(define/contract (dataset-ref ds name)
  (-> dataset? graph-name? (or/c graph? #f))
  (hash-ref (dataset-graphs ds) name #f))

(define/contract (dataset-ref! ds name to-set)
  (-> dataset? graph-name? graph? graph?)
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

(define (dataset-map ds proc (try-order? #f))
  (hash-map (dataset-graphs ds) proc try-order?))

(define (dataset-names ds (try-order? #f))
  (hash-keys (dataset-graphs ds) try-order?))

(define (dataset-values ds (try-order? #f))
  (hash-values (dataset-graphs ds) try-order?))

(define (dataset->list ds (try-order? #f))
  (hash->list (dataset-graphs ds) try-order?))

(define (describe-dataset ds)
  (append
     (list (make-type-statement (dataset-name ds) sd:Dataset))
     (flatten
      (map (λ (graph) (describe-graph graph))
           (dataset-graphs ds)))))
