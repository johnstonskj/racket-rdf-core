#lang racket/base

(require (for-syntax racket/base)
         racket/bool
         racket/contract
         racket/list
         racket/sequence
         racket/set
         racket/stream
         racket/string
         ;; --------------------------------------
         (only-in net/url-structs
                  path/param-path)
         ;; --------------------------------------
         rx
         uuid
         ;; --------------------------------------
         "./literal.rkt"
         "./nsmap.rkt"
         "./resource.rkt"
         "./statement.rkt"
         "./triple.rkt"
         ;; --------------------------------------
         "./v/sd.rkt"
         "./v/void.rkt")

(provide (except-out (struct-out graph)
                     internal-make-graph
                     graph-indices)
         (contract-out (graph-name? flat-contract?)
                       (statement-set? contract?)
                       (unnamed-graph
                        (->* ((or/c statement-list? statement-set?))
                             (nsmap? #:asserted boolean?)
                             graph?))
                       (named-graph
                        (->* (graph-name? (or/c statement-list? statement-set?))
                             (nsmap? #:asserted boolean?)
                             graph?))
                       ;; --------------------------------------
                       (graph-name-or-blank (-> graph? graph-name?))
                       (graph-named? (-> graph? boolean?))
                       (graph-empty? (-> graph? boolean?))
                       (graph-count (-> graph? exact-nonnegative-integer?))
                       (graph-order (-> graph? exact-nonnegative-integer?))
                       ;; --------------------------------------
                       (graph-member? (-> graph? statement? boolean?))
                       (graph-add (-> graph? statement? graph?))
                       (graph-add-all (-> graph? (sequence/c statement?) graph?))
                       (graph-remove (-> graph? statement? graph?))
                       (graph-remove-all (-> graph? (sequence/c statement?) graph?))
                       (graph-clear (-> graph? graph?))
                       (graph-make-blank-node (-> graph? blank-node?))
                       ;; --------------------------------------
                       (graph->stream (-> graph? (stream/c statement?)))
                       (graph-distinct-subjects (-> graph? (set/c subject?)))
                       (graph-distinct-predicates (-> graph? (set/c predicate?)))
                       (graph-distinct-objects (-> graph? (set/c object?)))
                       (graph-filter (-> graph? (-> statement? boolean?) (set/c statement?)))
                       (graph-filter-by-subject (-> graph? subject? (set/c statement?)))
                       (graph-filter-by-predicate (-> graph? predicate? (set/c statement?)))
                       (graph-filter-by-object (-> graph? object? (set/c statement?)))
                       ;; --------------------------------------
                       (graph-index-kind/c flat-contract?)
                       (graph-has-index? (-> graph? graph-index-kind/c boolean?))
                       (graph-indexes (-> graph? (listof graph-index-kind/c)))
                       (graph-index-ref (-> graph? graph-index-kind/c (or/c graph-index/c #f)))
                       (graph-index-create (-> graph? graph-index-kind/c void?))
                       (graph-index-drop (-> graph? graph-index-kind/c void?))
                       ;; --------------------------------------
                       (graph-skolemize (->* (graph?) (string?) graph?))
                       (graph-skolemize! (->* (graph?) (string?) graph?))
                       (skolem-resource? (-> resource? boolean?))
                       ;; --------------------------------------
                       (describe-graph (->* (graph?) ((or/c subject? #f)) statement-set?))
                       (graph-canonicalize (-> graph? graph?)))
         ;; --------------------------------------
         ;; Macros
         rdf-sub-graph
         rdf-graph)

;; -------------------------------------------------------------------------------------------------
;; Internals
;; -------------------------------------------------------------------------------------------------

(define (sequence->set seq)
  (for/set ((s seq)) s))

;; -------------------------------------------------------------------------------------------------
;; `graph-index` types
;; -------------------------------------------------------------------------------------------------

(define statement-set? (set/c statement?))

(define graph-index/c (hash/c any/c statement-set?))

(define graph-index-kind/c (one-of/c 'subject 'predicate 'object
                                     'subject-predicate-object
                                     'subject-predicate 'predicate-object))

(define graph-index-map/c (hash/c graph-index-kind/c graph-index/c))

;; -------------------------------------------------------------------------------------------------
;; `graph` types
;; -------------------------------------------------------------------------------------------------

(define graph-name? (or/c #f subject?))

(struct graph (name (namespace-map #:mutable) (statements #:mutable) asserted indices)
  #:transparent
  #:constructor-name internal-make-graph
  #:guard (struct-guard/c graph-name? nsmap? statement-set? boolean? graph-index-map/c))

(define (unnamed-graph statements (namespace-map (make-rdf-only-nsmap)) #:asserted (asserted #t))
  (internal-make-graph
   #f
   namespace-map
   (if (set? statements) statements (sequence->set statements))
   asserted
   (make-hash)))

(define (named-graph name statements (namespace-map (make-rdf-only-nsmap)) #:asserted (asserted #t))
  (internal-make-graph
   name
   namespace-map
   (if (set? statements) statements (sequence->set statements))
   asserted
   (make-hash)))

(define (graph-name-or-blank graph)
  (if (graph-named? graph)
      (graph-name graph)
      (make-blank-node)))

(define (all-blank-nodes graph)
  (let ((s-index (graph-index-ref graph 'subject))
        (o-index (graph-index-ref graph 'object)))
    (list->set
     (append
      (if s-index
          (filter blank-node? (hash-keys s-index))
          (filter blank-node? (graph-distinct-subjects graph)))
      (if o-index
          (filter blank-node? (hash-keys o-index))
          (filter blank-node? (graph-distinct-objects graph)))))))

(define (find-blank-node existing)
  (let ((bnode (make-blank-node)))
    (if (not (set-member? bnode existing))
        bnode
        (find-blank-node existing))))

(define (graph-make-blank-node graph)
  (let* ((existing (all-blank-nodes)))
    (find-blank-node existing)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Indices
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (statement-key stmt index-kind)
  (cond
    ((symbol=? index-kind 'subject) (get-subject stmt))
    ((symbol=? index-kind 'predicate) (get-predicate stmt))
    ((symbol=? index-kind 'object) (get-object stmt))
    ((symbol=? index-kind 'subject-predicate-object) (list (get-subject stmt) (get-predicate stmt) (get-object stmt)))
    ((symbol=? index-kind 'subject-predicate) (list (get-subject stmt) (get-predicate stmt)))
    ((symbol=? index-kind 'predicate-object) (list (get-predicate stmt) (get-object stmt)))))

(define (graph-index-add-one index kind statement)
  (let* ((key (statement-key statement kind))
         (key-list (hash-ref index key '())))
    (hash-set! index key (cons key key-list))))

(define (graph-index-remove-one index kind statement)
  (let* ((key (statement-key statement kind))
         (key-list (hash-ref index key '()))
         (new-key-list (remove key key-list)))
    (if (empty? new-key-list)
        (hash-remove! index key)
        (hash-set! index key new-key-list))))

(define (make-graph-index graph index-kind)
  (let ((index (make-hash)))
    (map (lambda (stmt) (graph-index-add-one index index-kind stmt))
         (graph-statements graph))
    index))

(define (graph-has-index? graph index-kind)
  (hash-has-key? (graph-indices graph) index-kind))

(define (graph-indexes graph)
  (hash-keys (graph-indices graph)))

(define (graph-index-ref graph index-kind)
  (hash-ref (graph-indices graph) index-kind #f))

(define (graph-index-create graph index-kind)
  (unless (graph-has-index? graph index-kind)
    (hash-set! (graph-indices graph) (make-graph-index graph index-kind))))

(define (graph-index-drop graph index-kind)
  (when (graph-has-index? graph index-kind)
    (hash-remove! (graph-indices graph) index-kind)))

(define (graph-index-add graph statement)
  (hash-for-each (lambda (kind index) (graph-index-add-one index kind statement))
                 (graph-indices graph)))

(define (graph-index-remove graph statement)
  (hash-for-each (lambda (kind index) (graph-index-remove-one index kind statement))
                 (graph-indices graph)))

(define (graph-index-clear graph)
  (for-each (lambda (index) (hash-clear! index)) (graph-indices graph)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Predicates & Properties
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (graph-named? graph)
  (not (false? (graph-name graph))))

(define (graph-empty? graph)
  (empty? (graph-statements graph)))

(define (graph-count graph)
  (set-count (graph-statements graph)))

(define (graph-order graph)
  (set-count (set-union (graph-distinct-subjects graph) (graph-distinct-objects graph))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Statement Members
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (graph-member? graph statement)
  (member graph (graph-statements graph)))

(define (graph-add graph statement)
  (graph-add-all graph (list statement)))

(define (graph-add-all graph statements)
  (set-graph-statements!
   graph
   (set-union (graph-statements graph)
              (if (set? statements)
                  statements
                  (sequence->set statements))))
  graph)

(define (graph-remove graph statement)
  (graph-remove-all (list statement)))

(define (graph-remove-all graph statements)
  (set-graph-statements!
   graph
   (set-subtract (graph-statements graph)
                 (if (set? statements)
                     statements
                     (sequence->set statements))))
  graph)

(define (graph-clear graph)
  (set-graph-statements! graph (set))
  graph)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Iterator & Query
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (graph->stream graph)
  (set->stream (graph-statements graph)))

(define (graph-distinct-subjects graph)
  (if (graph-has-index? graph 'subject)
      (list->set (hash-keys (graph-index-ref 'subject)))
      (for/set ((stmt (graph-statements graph)))
        (get-subject stmt))))

(define (graph-distinct-predicates graph)
  (if (graph-has-index? graph 'predicate)
      (list->set (hash-keys (graph-index-ref 'predicate)))
      (for/set ((stmt (graph-statements graph)))
        (get-predicate stmt))))

(define (graph-distinct-objects graph)
  (if (graph-has-index? graph 'object)
      (list->set (hash-keys (graph-index-ref 'object)))
      (for/set ((stmt (graph-statements graph)))
        (get-object stmt))))

(define (graph-filter proc graph)
  (sequence->set (sequence-filter proc (graph-statements graph))))

(define (graph-filter-by-subject graph val)
  (if (graph-has-index? graph 'subject)
      (hash-ref (graph-index-ref 'subject) val)
      (graph-filter (λ (stmt) (equal? (get-subject stmt) val)) graph)))

(define (graph-filter-by-predicate graph val)
  (if (graph-has-index? graph 'predicate)
      (hash-ref (graph-index-ref 'predicate) val)
      (graph-filter (λ (stmt) (equal? (get-predicate stmt) val)) graph)))

(define (graph-filter-by-object graph val)
  (if (graph-has-index? graph 'object)
      (hash-ref (graph-index-ref 'object) val)
      (graph-filter (λ (stmt) (equal? (get-object stmt) val)) graph)))

;; -------------------------------------------------------------------------------------------------
;; Skolemization
;; -------------------------------------------------------------------------------------------------

(define (skolemize node label-map base-resource)
  (cond ((blank-node? node)
         (let ((old-label (blank-node->string node)))
          (if (hash-has-key? label-map old-label)
              (hash-ref label-map old-label)
              (let ((new-resource (combine-resource/relative base-resource (uuid-string))))
                (hash-set! label-map old-label new-resource)
                new-resource))))
        (else node)))

(define single-name
  (rx/or-group
   rx/match-alnum ;; single char name
   (rx/and (rx/repeat (rx/and rx/match-alnum ;; << may not start with "-"
                              (rx/match rx/range-alnum "-"))
                      #:upper 61)
           ;; v- may not end with "-"
           rx/match-alnum)))

(define host-name-regexp
  (pregexp
   (rx/string-exactly
    (rx/and single-name (rx/and-group "." single-name #:repeat 'one-or-more)))))

(define (skolem-resource? resource)
  (and (resource? resource)
       (string-prefix? (resource-scheme resource) "http")
       (regexp-match host-name-regexp (resource-host resource))
       (resource-path-absolute? resource)
       (let ((path (resource-path resource)))
         (and (>= (length path) 3)
              (equal? (path/param-path (first path)) ".well-known")
              (equal? (path/param-path (second path)) "skolem")))))

(define (graph-skolemize-statements statements (domain-name "example.com"))
  (let ((base-resource (string->resource (format "https://~a/.well-known/skolem/" domain-name)))
        (label-map (make-hash)))
    (set-map statements
             (λ (stmt)
               (let ((subject (skolemize (get-subject stmt) label-map base-resource))
                     (object (skolemize (get-object stmt) label-map base-resource)))
                 (triple subject (get-predicate stmt) object))))))

(define (graph-skolemize graph (domain-name "example.com"))
  (let ((new-statements (graph-skolemize-statements (graph-statements graph) domain-name)))
    (if (graph-named? graph)
        (named-graph (graph-name graph) new-statements)
        (unnamed-graph new-statements))))

(define (graph-skolemize! graph (domain-name "example.com"))
  (let ((new-statements (graph-skolemize-statements (graph-statements graph) domain-name)))
    (set-graph-statements! graph new-statements))
  graph)

;; -------------------------------------------------------------------------------------------------
;; Describe
;; -------------------------------------------------------------------------------------------------

(define (describe-graph graph (subject #f))
  (let ((subject (if (false? (subject)) (make-blank-node) subject)))
    (set-union
     (if (graph-named? graph)
         (set
          (type-statement subject sd:NamedGraph)
          (triple subject sd:name (graph-name graph)))
         (set
          (type-statement subject sd:Graph)))
     (let ((graph-node (make-blank-node)))
       (set
        (triple subject sd:graph graph-node)
        (type-statement graph-node sd:Graph)
        (triple graph-node void:triples (graph-count graph))
        (triple graph-node void:distinct-subjects (count (graph-distinct-subjects graph)))
        (triple graph-node void:properties (count (graph-distinct-predicates graph)))
        (triple graph-node void:distinct-objects (count (graph-distinct-objects graph))))))))

;; -------------------------------------------------------------------------------------------------
;; Canonicalize
;; -------------------------------------------------------------------------------------------------

(define (canonicalize-statements/blank stmts)
  '())

(define (canonicalize-statements stmts)
  (let-values (((with without) (partition
                                (λ (stmt) (or (blank-node? (get-subject stmt))
                                              (blank-node? (get-object stmt))))
                               stmts)))
    (append
     (canonicalize-statements/blank with)
     (sort without))))

(define (graph-canonicalize in-graph)
  ;; TODO: implement
  (unnamed-graph))

;; -------------------------------------------------------------------------------------------------
;; Macros
;; -------------------------------------------------------------------------------------------------

(define-syntax (rdf-sub-graph stx)
  (syntax-case stx (_:)
    ;;((_ subject (predicate (blank inner-predicate object ...) ...) ...)
    ;; #'(let (())
    ;;     (make-sub-graph subject (list (list predicate (list object ...)) ...))))
    ;; ----------------------------------------------------------------------------
    ((_ subject (predicate (object ...)) ...) (not (list? (syntax->datum #'subject)))
     #'(make-statements subject (list (list predicate (list object ...)) ...)))
    ((_ subject (predicate object) ...) (not (list? (syntax->datum #'subject)))
     #'(make-statements subject (list (list predicate (list object)) ...)))
    ((_ subject predicate object) (not (list? (syntax->datum #'subject)))
     #'(make-statements subject (list (list predicate (list object)))))
    ;; ----------------------------------------------------------------------------
    ((_ (predicate (object ...)) ...)
     #'(make-statements (make-blank-node) (list (list predicate (list object ...)) ...)))
    ((_ (predicate object) ...)
     #'(make-statements (make-blank-node) (list (list predicate (list object)) ...)))
    ((_ predicate object)
     #'(make-statements (make-blank-node) (list (list predicate (list object)))))))

(define-syntax (rdf-graph stx)
  (syntax-case stx (=>)
    ((_ named => subject predicate object)
     #'(named-graph
        (if (string? named) (string->resource named) named)
        (list
         (triple
          (if (string? subject) (string->resource subject) subject)
          (if (string? predicate) (string->resource predicate) predicate)
          (if (literal? object) object (->literal object))))))
    ((_ subject (predicate object) ...)
     #'(let ((common-subject (if (string? subject) (string->resource subject) subject)))
         (unnamed-graph
          (map
           (λ (pair)
             (let ((this-predicate (car pair))
                   (this-object (cadr pair)))
               (triple
                common-subject
                (if (string? this-predicate) (string->resource this-predicate) this-predicate)
                (if (literal? this-object) this-object (->literal this-object)))))
           (list (list predicate object) ...)))))
    ((_ subject predicate object)
     #'(unnamed-graph
        (list
         (triple
          (if (string? subject) (string->resource subject) subject)
          (if (string? predicate) (string->resource predicate) predicate)
          (if (literal? object) object (->literal object))))))))

