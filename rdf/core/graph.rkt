#lang racket/base

(require (for-syntax racket/base)
         racket/bool
         racket/contract
         racket/list
         racket/set
         racket/string
         ;; --------------------------------------
         net/url-string
         net/url-structs
         ;; --------------------------------------
         uuid
         ;; --------------------------------------
         "./namespace.rkt" ;; for tests only
         "./statement.rkt"
         (prefix-in sd: (except-in "./v/sd.rkt" *namespace*))
         (prefix-in void:
                    (except-in "./v/void.rkt" *namespace*))
         (rename-in "./v/void.rkt" (*namespace* void:)))

(provide (except-out (struct-out graph)
                     internal-make-graph)
         make-default-graph
         make-named-graph
         graph-name?
         graph-named?
         graph-empty?
         graph-count
         graph-order
         graph-distinct-subjects
         graph-distinct-predicates
         graph-distinct-objects
         graph-member?
         graph-has-duplicates?
         graph-add
         graph-add-all
         graph-remove
         graph-remove-all
         graph-remove*
         graph-remove-all*
         graph-clear
         ;; --------------------------------------
         graph-filter
         graph-filter-by-subject
         graph-filter-by-predicate
         graph-filter-by-object
         ;; --------------------------------------
         graph-skolemize
         graph-skolemize!
         skolem-url?
         ;; --------------------------------------
         describe-graph
         ;; --------------------------------------
         rdf-sub-graph
         rdf-graph)

;; -------------------------------------------------------------------------------------------------
;; `graph` types
;; -------------------------------------------------------------------------------------------------

(define graph-name? (or/c #f url?))

(struct graph (name (statements #:mutable))
  #:sealed
  #:constructor-name internal-make-graph
  #:guard (struct-guard/c graph-name? statement-list?))

(define (make-default-graph statements)
  (internal-make-graph #f statements))

(define (make-named-graph name statements)
  (when (url? name)
   (internal-make-graph name statements)))

(define (graph->quads graph)
  (let ((graph-name (graph-name graph)))
    (map (λ (stmt) (cons graph-name (statement->triple stmt))) (graph-statements graph))))

;; -------------------------------------------------------------------------------------------------

(define (graph-named? graph)
  (not (false? (graph-name graph))))

(define (graph-empty? graph)
  (empty? (graph-statements graph)))

(define (graph-has-duplicates? graph)
  (not (false? (check-duplicates (graph-statements graph)))))

(define (graph-count graph)
  (length (graph-statements graph)))

(define (graph-order graph)
  (set-count (set-union (graph-distinct-subjects graph) (graph-distinct-objects graph))))

(define (graph-distinct-subjects graph)
  (list->set (map (λ (stmt) (statement-subject stmt)) (graph-statements graph))))

(define (graph-distinct-predicates graph)
  (list->set (map (λ (stmt) (statement-predicate stmt)) (graph-statements graph))))

(define (graph-distinct-objects graph)
  (list->set (map (λ (stmt) (statement-object stmt)) (graph-statements graph))))

;; -------------------------------------------------------------------------------------------------

(define (graph-member? graph statement)
  (when (statement? statement)
    (member graph (graph-statements graph))))

(define (graph-add graph statement)
  (graph-add-all (list statement)))

(define (graph-add-all graph statements)
  (when (and (list? statements) (andmap statement? statements))
    (set-graph-statements! graph (append statements (graph-statements graph))))
  graph)

(define (graph-remove graph statement)
  (when (statement? statement)
    (set-graph-statements! graph (remove statement (graph-statements graph))))
  graph)

(define (graph-remove-all graph statements)
  (when (and (list? statements) (andmap statement? statements))
    (for-each (λ (statement) (graph-remove graph statement)) statements))
  graph)

(define (graph-remove* graph statement)
  (when (statement? statement)
    (graph-remove-all* (list statement)))
  graph)

(define (graph-remove-all* graph statements)
  (when (and (list? statements) (andmap statement? statements))
    (set-graph-statements! graph (remove* statements (graph-statements graph))))
  graph)

(define (graph-clear graph)
  (set-graph-statements! graph '())
  graph)

;; -------------------------------------------------------------------------------------------------

(define (graph-filter proc graph)
  (filter proc (graph-statements graph)))

(define (graph-filter-by-subject graph obj)
  (graph-filter
   (λ (stmt) (equal? (statement-subject stmt) obj))
   graph))

(define (graph-filter-by-predicate graph obj)
  (graph-filter
   (λ (stmt) (equal? (statement-predicate stmt) obj))
   graph))

(define (graph-filter-by-object graph obj)
  (graph-filter
   (λ (stmt) (equal? (statement-object stmt) obj))
   graph))

;; -------------------------------------------------------------------------------------------------
;; Skolemization
;; -------------------------------------------------------------------------------------------------

(define (skolemize node id-map base-url)
  (cond ((blank-node? node)
         (let ((old-id (blank-node-id node)))
          (if (hash-has-key? id-map old-id)
              (hash-ref id-map old-id)
              (let ((new-url (combine-url/relative base-url (uuid-string))))
                (hash-set! id-map old-id new-url)
                new-url))))
        (else node)))

(define/contract (skolem-url? url)
  (-> url? boolean?)
  (and (url? url)
       (string-prefix? (url-scheme url) "http")
       (url-path-absolute? url)
       (let ((path (url-path url)))
         (and (>= (length path) 3)
              (equal? (path/param-path (first path)) ".well-known")
              (equal? (path/param-path (second path)) "skolem")))))

(define (graph-skolemize-statements statements (domain-name "example.com"))
  (let ((base-url (string->url (format "https://~a/.well-known/skolem/" domain-name)))
        (id-map (make-hash)))
    (map (λ (stmt)
            (let ((subject (skolemize (statement-subject stmt) id-map base-url))
                  (object (skolemize (statement-object stmt) id-map base-url)))
              (make-statement subject (statement-predicate stmt) object)))
          statements)))

(define/contract (graph-skolemize graph (domain-name "example.com"))
  (->* (graph?) (string?) graph?)
  (let ((new-statements (graph-skolemize-statements (graph-statements graph))))
    (if (graph-named? graph)
      (make-named-graph (graph-name graph) new-statements)
      (make-default-graph new-statements))))

(define/contract (graph-skolemize! graph (domain-name "example.com"))
  (->* (graph?) (string?) graph?)
  (let ((new-statements (graph-skolemize-statements (graph-statements graph))))
    (set-graph-statements! graph new-statements))
  graph)

;; -------------------------------------------------------------------------------------------------
;; Describe
;; -------------------------------------------------------------------------------------------------

(define/contract (describe-graph graph (subject #f))
  (->* (graph?) ((or/c subject? #f)) statement-list?)
  (let ((subject (if (false? (subject)) (make-blank-node) subject)))
    (append
     (if (graph-named? graph)
         (list
          (make-type-statement subject sd:NamedGraph)
          (make-statement subject sd:name (graph-name graph)))
         (list
          (make-type-statement subject sd:Graph)))
     (let ((graph-node (make-blank-node)))
       (list
        (make-statement subject sd:graph graph-node)
        (make-type-statement graph-node sd:Graph)
        (make-statement graph-node void:triples (graph-count graph))
        (make-statement graph-node void:distinctSubjects (count (graph-distinct-subjects graph)))
        (make-statement graph-node void:properties (count (graph-distinct-predicates graph)))
        (make-statement graph-node void:distinctObjects (count (graph-distinct-objects graph))))))))

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
     #'(make-statement-list subject (list (list predicate (list object ...)) ...)))
    ((_ subject (predicate object) ...) (not (list? (syntax->datum #'subject)))
     #'(make-statement-list subject (list (list predicate (list object)) ...)))
    ((_ subject predicate object) (not (list? (syntax->datum #'subject)))
     #'(make-statement-list subject (list (list predicate (list object)))))
    ;; ----------------------------------------------------------------------------
    ((_ (predicate (object ...)) ...)
     #'(make-statement-list (make-blank-node) (list (list predicate (list object ...)) ...)))
    ((_ (predicate object) ...)
     #'(make-statement-list (make-blank-node) (list (list predicate (list object)) ...)))
    ((_ predicate object)
     #'(make-statement-list (make-blank-node) (list (list predicate (list object)))))))

(define-syntax (rdf-graph stx)
  (syntax-case stx (=>)
    ((_ named => subject predicate object)
     #'(make-named-graph
        (if (string? named) (string->url named) named)
        (list
         (make-statement
          (if (string? subject) (string->url subject) subject)
          (if (string? predicate) (string->url predicate) predicate)
          object))))
    ((_ subject (predicate object) ...)
     #'(let ((common-subject (if (string? subject) (string->url subject) subject)))
         (make-default-graph
          (map
           (λ (pair)
             (let ((this-predicate (car pair))
                   (this-object (cadr pair)))
               (make-statement
                common-subject
                (if (string? this-predicate) (string->url this-predicate) this-predicate)
                this-object)))
           (list (list predicate object) ...)))))
    ((_ subject predicate object)
     #'(make-default-graph
        (list
         (make-statement
          (if (string? subject) (string->url subject) subject)
          (if (string? predicate) (string->url predicate) predicate)
          object))))
    ))

