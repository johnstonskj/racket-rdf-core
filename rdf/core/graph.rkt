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
         "./namespace.rkt"
         "./literal.rkt"
         "./statement.rkt"
         "./triple.rkt"
         ;; --------------------------------------
         "./v/sd.rkt"
         "./v/void.rkt")

(provide (except-out (struct-out graph)
                     internal-make-graph)
         graph-name-or-blank
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

(define graph-name? (or/c #f subject?))

(struct graph (name (statements #:mutable))
  #:sealed
  #:transparent
  #:constructor-name internal-make-graph
  #:guard (struct-guard/c graph-name? statement-list?))

(define (make-default-graph statements)
  (internal-make-graph #f statements))

(define (make-named-graph name statements)
  (when (url? name)
   (internal-make-graph name statements)))

(define (graph-name-or-blank graph)
  (if (graph-named? graph)
      (graph-name graph)
      (make-blank-node)))

;; -------------------------------------------------------------------------------------------------

(define (graph-named? graph)
  (not (false? (graph-name graph))))

(define (graph-empty? graph)
  (empty? (graph-statements graph)))

(define (graph-has-duplicates? graph)
  (not (false? (check-duplicates (graph-statements graph)))))

;; -------------------------------------------------------------------------------------------------

(define (graph-count graph)
  (length (graph-statements graph)))

(define (graph-order graph)
  (set-count (set-union (graph-distinct-subjects graph) (graph-distinct-objects graph))))

(define (graph-distinct-subjects graph)
  (list->set (map (λ (stmt) (get-subject stmt)) (graph-statements graph))))

(define (graph-distinct-predicates graph)
  (list->set (map (λ (stmt) (get-predicate stmt)) (graph-statements graph))))

(define (graph-distinct-objects graph)
  (list->set (map (λ (stmt) (get-object stmt)) (graph-statements graph))))

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
   (λ (stmt) (equal? (get-subject stmt) obj))
   graph))

(define (graph-filter-by-predicate graph obj)
  (graph-filter
   (λ (stmt) (equal? (get-predicate stmt) obj))
   graph))

(define (graph-filter-by-object graph obj)
  (graph-filter
   (λ (stmt) (equal? (get-object stmt) obj))
   graph))

;; -------------------------------------------------------------------------------------------------
;; Skolemization
;; -------------------------------------------------------------------------------------------------

(define (skolemize node label-map base-url)
  (cond ((blank-node? node)
         (let ((old-label (blank-node-label node)))
          (if (hash-has-key? label-map old-label)
              (hash-ref label-map old-label)
              (let ((new-url (combine-url/relative base-url (uuid-string))))
                (hash-set! label-map old-label new-url)
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
        (label-map (make-hash)))
    (map (λ (stmt)
            (let ((subject (skolemize (get-subject stmt) label-map base-url))
                  (object (skolemize (get-object stmt) label-map base-url)))
              (make-triple subject (get-predicate stmt) object)))
          statements)))

(define/contract (graph-skolemize graph (domain-name "example.com"))
  (->* (graph?) (string?) graph?)
  (let ((new-statements (graph-skolemize-statements (graph-statements graph) domain-name)))
    (if (graph-named? graph)
      (make-named-graph (graph-name graph) new-statements)
      (make-default-graph new-statements))))

(define/contract (graph-skolemize! graph (domain-name "example.com"))
  (->* (graph?) (string?) graph?)
  (let ((new-statements (graph-skolemize-statements (graph-statements graph) domain-name)))
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
          (make-triple subject sd:name (graph-name graph)))
         (list
          (make-type-statement subject sd:Graph)))
     (let ((graph-node (make-blank-node)))
       (list
        (make-triple subject sd:graph graph-node)
        (make-type-statement graph-node sd:Graph)
        (make-triple graph-node void:triples (graph-count graph))
        (make-triple graph-node void:distinct-subjects (count (graph-distinct-subjects graph)))
        (make-triple graph-node void:properties (count (graph-distinct-predicates graph)))
        (make-triple graph-node void:distinct-objects (count (graph-distinct-objects graph))))))))

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
         (make-triple
          (if (string? subject) (string->url subject) subject)
          (if (string? predicate) (string->url predicate) predicate)
          (if (literal? object) object (->literal object))))))
    ((_ subject (predicate object) ...)
     #'(let ((common-subject (if (string? subject) (string->url subject) subject)))
         (make-default-graph
          (map
           (λ (pair)
             (let ((this-predicate (car pair))
                   (this-object (cadr pair)))
               (make-triple
                common-subject
                (if (string? this-predicate) (string->url this-predicate) this-predicate)
                (if (literal? this-object) this-object (->literal this-object)))))
           (list (list predicate object) ...)))))
    ((_ subject predicate object)
     #'(make-default-graph
        (list
         (make-triple
          (if (string? subject) (string->url subject) subject)
          (if (string? predicate) (string->url predicate) predicate)
          (if (literal? object) object (->literal object))))))
    ))

