#lang racket/base

(require racket/bool
         racket/contract
         racket/list
         racket/set
         racket/stream
         ;; --------------------------------------
         "./graph.rkt"
         "./quad.rkt"
         "./resource.rkt"
         "./statement.rkt"
         "./triple.rkt"
         (only-in "./v/sd.rkt"
                  sd:Dataset))

(provide (contract-out (struct dataset ((name (or/c resource? #f))
                                        (graphs (hash/c graph-name? graph?))))
                       ;; --------------------------------------
                       (named-dataset (-> resource? (hash/c graph-name? graph?) dataset?))
                       (unnamed-dataset (-> (hash/c graph-name? graph?) dataset?))
                       (graph-list->dataset (-> (listof graph?) dataset?))
                       (quad-set->dataset (-> (set/c quad?) dataset?))
                       ;; --------------------------------------
                       (dataset-named? (-> dataset? boolean?))
                       (dataset-empty? (-> dataset? boolean?))
                       (dataset-count (-> dataset? exact-nonnegative-integer?))
                       ;; --------------------------------------
                       (dataset-has-named? (-> dataset? graph-name? boolean?))
                       (dataset-has-default? (-> dataset? boolean?))
                       (dataset-ref (-> dataset? graph-name? (or/c graph? #f)))
                       (dataset-ref-default (-> dataset?  (or/c graph? #f)))
                       (dataset-set! (-> dataset? graph? void?))
                       (dataset-remove! (-> dataset? graph-name? void?))
                       (dataset-update! (-> dataset? graph-name? (-> graph? graph?) void?))
                       (dataset-clear! (-> dataset? void?))
                       ;; --------------------------------------
                       (dataset-map (-> dataset? (-> graph-name? graph? any/c) (listof any/c)))
                       (dataset-names (-> dataset? (listof graph-name?)))
                       (dataset-values (-> dataset? (listof graph?)))
                       (dataset->stream (-> dataset? (stream/c (cons/c graph-name? graph?))))
                       ;; --------------------------------------
                       (dataset-distinct-subjects (-> dataset? (set/c subject?)))
                       (dataset-distinct-predicates (-> dataset? (set/c predicate?)))
                       (dataset-distinct-objects (-> dataset? (set/c object?)))
                       ;; --------------------------------------
                       (describe-dataset (-> dataset? (set/c statement?)))
                       (dataset-canonicalize (-> dataset? dataset?))))

(struct dataset (name (graphs #:mutable))
  #:sealed
  #:transparent
  #:guard (struct-guard/c (or/c resource? #f) (hash/c graph-name? graph?)))

;; -------------------------------------------------------------------------------------------------

(define (unnamed-dataset graphs)
  (dataset #f graphs))

(define (named-dataset name graphs)
  (dataset name graphs))

(define (graph-list->dataset lst)
  (dataset #f (make-hash (map (λ (graph) (cons (graph-name graph) graph)) lst))))

(define (quad-set->dataset quads)
  (let ((graphs (make-hash)))
    (set-for-each
     quads
     (λ (quad)
       (let* ((name (get-graph-name quad))
              (graph (hash-ref! graphs
                                name
                                (if name
                                    (named-graph name '())
                                    (unnamed-graph '())))))
         (graph-add graph quad))))
    (unnamed-dataset graphs)))

;; -------------------------------------------------------------------------------------------------

(define (dataset-named? ds)
  (not (false? (dataset-name ds))))

(define (dataset-empty? ds)
  (hash-empty? (dataset-graphs ds)))

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

(define (dataset-map ds proc)
  (hash-map (dataset-graphs ds) proc #f))

(define (dataset-names ds )
  (hash-keys (dataset-graphs ds) #f))

(define (dataset-values ds )
  (hash-values (dataset-graphs ds) #f))

(define (dataset->stream ds)
  (sequence->stream (dataset-graphs ds)))

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

(define (dataset-canonicalize in-dataset)
  (struct-copy dataset in-dataset
               (graphs
                (make-hash
                 (hash-map
                  (dataset-graphs in-dataset)
                  (λ (name graph) (cons name (graph-canonicalize graph))))))))
