#lang racket/base

(require (for-syntax racket/base racket/syntax)
         racket/bool
         racket/contract
         racket/function
         racket/list
         racket/port
         racket/set
         racket/string
         ;; --------------------------------------
         "./dataset.rkt"
         "./graph.rkt"
         "./literal.rkt"
         "./nsname.rkt"
         "./nsmap.rkt"
         "./query.rkt"
         "./resource.rkt"
         "./statement.rkt"
         "./tree.rkt")

(provide (contract-out (write-ntriple-literal
                        (->* (literal?) (#:nsmap (or/c nsmap? #f) output-port?) void?))
                       (literal->ntriple-string
                        (->* (literal?) (#:nsmap (or/c nsmap? #f)) string?))
                       ;; --------------------------------------
                       (write-ntriple-blank-node
                        (->* (blank-node?) (output-port?) void?))
                       (blank-node->ntriple-string
                        (-> blank-node? string?))
                       (write-ntriple-resource
                        (->* (resource?) (#:nsmap (or/c nsmap? #f) output-port?) void?))
                       (resource->ntriple-string
                        (->* (resource?) (#:nsmap (or/c nsmap? #f)) string?))
                       ;; --------------------------------------
                       (write-ntriple-subject
                        (->* (subject?) (#:nsmap (or/c nsmap? #f) output-port?) void?))
                       (subject->ntriple-string
                        (->* (subject?) (#:nsmap (or/c nsmap? #f)) string?))
                       (write-ntriple-predicate
                        (->* (predicate?) (#:nsmap (or/c nsmap? #f) output-port?) void?))
                       (predicate->ntriple-string
                        (->* (predicate?) (#:nsmap (or/c nsmap? #f)) string?))
                       (write-ntriple-object
                        (->* (object?) (#:nsmap (or/c nsmap? #f) output-port?) void?))
                       (object->ntriple-string
                        (->* (object?) (#:nsmap (or/c nsmap? #f)) string?))
                       ;; --------------------------------------
                       (write-ntriple-statement
                        (->* (statement?) (#:nsmap (or/c nsmap? #f) output-port?) void?))
                       (statement->ntriple-string
                        (->* (statement?) (#:nsmap (or/c nsmap? #f)) string?))
                       (write-nquad-statement
                        (->* (subject? statement?) (#:nsmap (or/c nsmap? #f) output-port?) void?))
                       (statement->nquad-string
                        (->* (subject? statement?) (#:nsmap (or/c nsmap? #f)) string?))
                       ;; --------------------------------------
                       (write-turtle-nsmap (->* (nsmap?) (output-port?) void?))
                       (nsmap->turtle-string (-> nsmap? string?))
                       (write-sparql-nsmap (->* (nsmap?) (output-port?) void?))
                       (nsmap->sparql-string (-> nsmap? string?))
                       ;; --------------------------------------
                       (write-ntriple-graph (->* (graph?) (output-port?) void?))
                       (graph->ntriple-string (-> graph? string?))
                       (write-nquad-graph (->* (graph?) (output-port?) void?))
                       (graph->nquad-string (-> graph? string?))
                       (write-trig-graph (->* (graph?) (output-port?) void?))
                       (graph->trig-string (-> graph? string?))
                       ;; --------------------------------------
                       (write-nquad-dataset (->* (dataset?) (output-port?) void?))
                       (dataset->nquad-string (-> dataset? string?))
                       (write-trig-dataset (->* (dataset?) (output-port?) void?))
                       (dataset->trig-string (-> dataset? string?))
                       ;; --------------------------------------
                       (write-statement-pattern (->* (statement-pattern?) (output-port?) void?))
                       (statement-pattern->string (-> statement-pattern? string?))))

;; -------------------------------------------------------------------------------------------------
;; Internal
;; -------------------------------------------------------------------------------------------------

(define-syntax (writer->to-string stx)
  (syntax-case stx (mapped)
    ((_ thing)
     (with-syntax ((to-string-name (format-id #'thing "~a->string" (syntax-e #'thing)))
                   (writer-name (format-id #'thing "write-~a" (syntax-e #'thing))))
      #'(define (to-string-name val) (with-output-to-string (λ () (writer-name val))))))
    ((_ thing syntax-name)
     (with-syntax ((to-string-name (format-id #'thing "~a->~a-string" (syntax-e #'thing) (syntax-e #'syntax-name)))
                   (writer-name (format-id #'thing "write-~a-~a" (syntax-e #'syntax-name) (syntax-e #'thing))))
       #'(define (to-string-name val) (with-output-to-string (λ () (writer-name val))))))
    ((_ thing mapped)
     (with-syntax ((to-string-name (format-id #'thing "~a->string" (syntax-e #'thing)))
                   (writer-name (format-id #'thing "write-~a" (syntax-e #'thing))))
       #'(define (to-string-name val #:nsmap (namespace-map #f))
           (with-output-to-string (λ () (writer-name val #:nsmap namespace-map))))))
    ((_ thing syntax-name mapped)
     (with-syntax ((to-string-name (format-id #'thing "~a->~a-string" (syntax-e #'thing) (syntax-e #'syntax-name)))
                   (writer-name (format-id #'thing "write-~a-~a" (syntax-e #'syntax-name) (syntax-e #'thing))))
       #'(define (to-string-name val #:nsmap (namespace-map #f))
           (with-output-to-string (λ () (writer-name val #:nsmap namespace-map))))))))

;; -------------------------------------------------------------------------------------------------
;; Namespaces
;; -------------------------------------------------------------------------------------------------

(define (nsmap->import-statements map style out)
  (hash-for-each
   (nsmap-mapping map)
   (λ (prefix ns) (namespace->import-statement prefix ns style out))))

(define (namespace->import-statement prefix ns style out)
  (fprintf out "~a ~a <~a>~a~n"
          (cond
            ((symbol=? style 'ttl) "@prefix")
            ((symbol=? style 'sparql) "PREFIX"))
          (if prefix (prefix->string prefix) ":")
          (resource->string ns)
          (if (symbol=? style 'ttl) " ." "")))

(define (write-turtle-nsmap map (out (current-output-port)))
  (nsmap->import-statements map 'ttl out))

(writer->to-string nsmap turtle)

(define (write-sparql-nsmap map (out (current-output-port)))
  (nsmap->import-statements map 'sparql out))

(writer->to-string nsmap sparql)

;; -------------------------------------------------------------------------------------------------
;; Printing literals
;; -------------------------------------------------------------------------------------------------

(define (write-ntriple-literal val #:nsmap (namespace-map #f) (out (current-output-port)))
  (let ((lexical-form (literal-lexical-form val))
        (language-tag (literal-language-tag val))
        (datatype-iri (literal-datatype-iri val)))
    (cond
      (language-tag (fprintf out "~s@~a"
                             lexical-form
                             language-tag))
      (datatype-iri (fprintf out "~s^^~a"
                             lexical-form
                             (resource->ntriple-string datatype-iri #:nsmap namespace-map)))
      (else (fprintf out "~s" (literal-lexical-form val))))))

(writer->to-string literal ntriple mapped)

;; -------------------------------------------------------------------------------------------------
;; Printing "atoms"
;; -------------------------------------------------------------------------------------------------

(define (write-ntriple-blank-node val (out (current-output-port)))
  (fprintf out "_:~a" (blank-node->string val)))

(writer->to-string blank-node ntriple)

(define (write-ntriple-resource val #:nsmap (namespace-map #f) (out (current-output-port)))
  (let* ((resource->ntriple-string (λ (resource) (string-append "<" (resource->string resource) ">")))
         (val-str (if (false? namespace-map)
                      (resource->ntriple-string val)
                      (let ((short (nsmap-shorten namespace-map val)))
                        (cond ((resource? short)
                               (resource->ntriple-string short))
                              ((prefix? short)
                               (prefix->string short))
                              ((prefixed-name? short)
                               (prefixed-name->string short)))))))
    (display val-str out)))

(writer->to-string resource ntriple mapped)

(define (write-ntriple-subject val #:nsmap (namespace-map #f) (out (current-output-port)))
  (cond
    ((resource? val) (write-ntriple-resource val #:nsmap namespace-map out))
    ((blank-node? val) (write-ntriple-blank-node val out))))

(writer->to-string subject ntriple mapped)

(define (write-ntriple-predicate val #:nsmap (namespace-map #f) (out (current-output-port)))
  (write-ntriple-resource val #:nsmap namespace-map out))

(writer->to-string predicate ntriple mapped)

(define (write-ntriple-object val #:nsmap (namespace-map #f) (out (current-output-port)))
  (cond
    ((resource? val) (write-ntriple-resource val #:nsmap namespace-map out))
    ((blank-node? val) (write-ntriple-blank-node val out))
    ((literal? val) (write-ntriple-literal val #:nsmap namespace-map out))))

(writer->to-string object ntriple mapped)

;; -------------------------------------------------------------------------------------------------
;; Printing statements
;; -------------------------------------------------------------------------------------------------

(define (write-inner-whitespace (out (current-output-port)))
  (display " " out))

(define (write-end-of-statement (out (current-output-port)))
  (displayln " ." out))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define/contract (write-ntriple-statement stmt #:nsmap (namespace-map #f) (out (current-output-port)))
  (->* (statement?) (#:nsmap (or/c nsmap? #f) output-port?) void?)
  (write-ntriple-subject (get-subject stmt) #:nsmap namespace-map out)
  (write-inner-whitespace out)
  (write-ntriple-predicate (get-predicate stmt) #:nsmap namespace-map out)
  (write-inner-whitespace out)
  (write-ntriple-object (get-object stmt) #:nsmap namespace-map out)
  (write-end-of-statement out))

(writer->to-string statement ntriple mapped)

(define/contract (write-nquad-statement graph-name stmt #:nsmap (namespace-map #f) (out (current-output-port)))
  (->* (subject? statement?) (#:nsmap (or/c nsmap? #f) output-port?) void?)
  (write-ntriple-subject (get-subject stmt) #:nsmap namespace-map out)
  (write-inner-whitespace out)
  (write-ntriple-predicate (get-predicate stmt) #:nsmap namespace-map out)
  (write-inner-whitespace out)
  (write-ntriple-object (get-object stmt) #:nsmap namespace-map out)
  (write-inner-whitespace out)
  (write-ntriple-subject graph-name #:nsmap namespace-map out)
  (write-end-of-statement out))

(define (statement->nquad-string graph-name stmt #:nsmap (namespace-map #f))
  (with-output-to-string (λ () (write-nquad-statement graph-name stmt #:nsmap namespace-map))))

;; -------------------------------------------------------------------------------------------------
;; Printing graphs
;; -------------------------------------------------------------------------------------------------

(define (write-ntriple-graph graph (out (current-output-port)))
  (->* (graph?) (output-port?) void?)
  (set-for-each
   (graph-statements graph)
   (λ (stmt) (write-ntriple-statement stmt out))))

(writer->to-string graph ntriple)

(define/contract (write-nquad-graph graph (out (current-output-port)))
  (->* (graph?) (output-port?) void?)
  (let ((graph-name (graph-name-or-blank graph)))
    (set-for-each
     (graph-statements graph)
     (λ (stmt) (write-nquad-statement graph-name stmt out)))))

(writer->to-string graph nquad)

(define (write-turtle-graph graph (out (current-output-port)))
  (->* (graph?) (output-port?) void?)
  (let ((namespace-map (graph-namespace-map graph))
        (tree (graph->tree graph)))
    (write-turtle-nsmap namespace-map out)
    (newline out)
    (hash-for-each
     tree
     (λ (subject predicate+object)
       (write-ntriple-subject subject #:nsmap namespace-map out)
       (newline out)
       (display
        (string-join
         (flatten
          (hash-map
           predicate+object
           (λ (predicate objects)
             (map
              (λ (object)
                (format "    ~a ~a"
                        (predicate->ntriple-string predicate #:nsmap namespace-map)
                        (object->ntriple-string object #:nsmap namespace-map)))
              objects))))
         " ;\n"))
       (displayln " .\n" out)))))

(writer->to-string graph turtle)

(define/contract (write-trig-graph graph (out (current-output-port)))
  (->* (graph?) (output-port?) void?)
  (when (graph-named? graph)
    (write-ntriple-subject (graph-name graph) out)
    (displayln " " out))
  (displayln "{" out)
  (write-turtle-graph graph out)
  (displayln "}" out))

(writer->to-string graph trig)

;; -------------------------------------------------------------------------------------------------
;; Printing data sets
;; -------------------------------------------------------------------------------------------------

(define/contract (write-trig-dataset dataset (out (current-output-port)))
  (->* (dataset?) (output-port?) void?)
  (for-each
   (curryr write-trig-graph out)
   (dataset-graphs dataset)))

(writer->to-string dataset trig)

(define/contract (write-nquad-dataset dataset (out (current-output-port)))
  (->* (dataset?) (output-port?) void?)
  (for-each (λ (graph) (write-nquad-graph graph out))
            (dataset-values dataset)))

(writer->to-string dataset nquad)

;; -------------------------------------------------------------------------------------------------
;; Printing statement patterns
;; -------------------------------------------------------------------------------------------------

(define (write-pattern-component pattern (out (current-output-port)))
  (let ((kind (component-pattern-kind pattern))
        (actual (component-pattern-inner pattern)))
    (cond
    ((eq? 'ignore kind) (display "_" out))
    ((eq? 'compare kind) (write-ntriple-object (comparitor-value actual) out))
    ((eq? 'variable kind) (fprintf out "?~a" actual))
    (else (error)))))

(define (write-statement-pattern pattern (out (current-output-port)))
  (->* (statement-pattern?) (output-port?) void?)
  (write-pattern-component (statement-pattern-subject pattern) out)
  (write-inner-whitespace out)
  (write-pattern-component (statement-pattern-predicate pattern) out)
  (write-inner-whitespace out)
  (write-pattern-component (statement-pattern-object pattern) out)
  (newline out))

(writer->to-string statement-pattern)
