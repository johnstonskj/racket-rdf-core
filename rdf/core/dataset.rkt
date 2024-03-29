#lang racket/base

(require racket/contract
         racket/list
         ;; --------------------------------------
         "./graph.rkt"
         "./statement.rkt"
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
  #:guard (struct-guard/c (hash/c graph-name? graph?)))

(define (dataset-empty? ds)
  (hash-empty? (dataset-graphs ds)))

(define (dataset-count ds)
  (hash-count (dataset-graphs ds)))

(define (dataset-has-key? ds name)
  (hash-has-key? (dataset-graphs ds) name))

(define (dataset-has-default? ds)
  (hash-has-key? (dataset-graphs ds) #f))

(define (dataset-ref-default ds)
  (hash-ref (dataset-graphs ds) #f))

(define (dataset-ref ds name)
  (hash-ref (dataset-graphs ds) name))

(define (dataset-ref! ds name to-set)
  (hash-ref! (dataset-graphs ds) name to-set))

(define (dataset-set! ds graph)
  (hash-set! (dataset-graphs ds) (graph-name graph) graph))

(define (dataset-remove! ds name)
  (hash-remove! (dataset-graphs ds) name))

(define (dataset-update! ds name updater)
  (hash-update! (dataset-graphs ds) name updater))

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

(define (describe-dataset ds (subject #f))
  (let ((subject (if (subject) subject (make-blank-node))))
    (append
     (list (make-type-statement subject sd:Dataset))
     (flatten
      (map (λ (graph) (describe-graph subject graph))
           (dataset-graphs ds))))))
