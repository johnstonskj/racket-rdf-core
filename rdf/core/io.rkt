#lang racket/base

(require (for-syntax racket/base racket/syntax)
         racket/bool
         racket/contract
         racket/list
         racket/port
         ;; --------------------------------------
         net/url-string
         ;; --------------------------------------
         "./namespace.rkt"
         "./nsmap.rkt"
         "./literal.rkt"
         "./statement.rkt"
         "./graph.rkt"
         "./dataset.rkt"
         "./gq.rkt")

(provide import-style/c
         namespace->import-statement
         ;; --------------------------------------
         write-ntriple-literal
         literal->ntriple-string
         ;; --------------------------------------
         write-ntriple-blank-node
         blank-node->ntriple-string
         write-ntriple-resource
         resource->ntriple-string
         write-ntriple-subject
         subject->ntriple-string
         write-ntriple-predicate
         predicate->ntriple-string
         write-ntriple-object
         object->ntriple-string
         ;; --------------------------------------
         write-ntriple-statement
          statement->ntriple-string
         write-nquad-statement
         statement->nquad-string
         ;; --------------------------------------
         write-ntriple-graph
         graph->ntriple-string
         write-nquad-graph
         graph->nquad-string
         ;; --------------------------------------
         write-trig-dataset
         dataset->trig-string
         ;; --------------------------------------
         write-statement-pattern
         statement-pattern->string)

(define-syntax (writer->to-string stx)
  (syntax-case stx ()
    ((_ thing)
     (with-syntax ((to-string-name (format-id #'thing "~a->string" (syntax-e #'thing)))
                   (writer-name (format-id #'thing "write-~a" (syntax-e #'thing))))
      #'(define (to-string-name val) (with-output-to-string (λ () (writer-name val))))))
    ((_ thing syntax-name)
     (with-syntax ((to-string-name (format-id #'thing "~a->~a-string" (syntax-e #'thing) (syntax-e #'syntax-name)))
                   (writer-name (format-id #'thing "write-~a-~a" (syntax-e #'syntax-name) (syntax-e #'thing))))
      #'(define (to-string-name val) (with-output-to-string (λ () (writer-name val))))))))

;; -------------------------------------------------------------------------------------------------
;; Namespaces
;; -------------------------------------------------------------------------------------------------

(define import-style/c (or/c 'ttl 'sparql))

(define/contract (namespace->import-statement prefix ns style)
  (-> (or/c prefix? #f) namespace? import-style/c string?)
  (format "~a ~a <~a>~a~n"
          (cond
            ((symbol=? style 'ttl) "@prefix")
            ((symbol=? style 'sparql) "PREFIX"))
          (if prefix (prefix->string prefix) ":")
          (namespace->string ns)
          (if (symbol=? style 'ttl) " ." "")))

;; -------------------------------------------------------------------------------------------------
;; Printing literals
;; -------------------------------------------------------------------------------------------------

(define/contract (write-ntriple-literal val (out (current-output-port)))
  (->* (literal?) (output-port?) void?)
  (let ((lexical-form (literal-lexical-form val))
        (language-tag (literal-language-tag val))
        (datatype-iri (literal-datatype-iri val)))
    (cond
      (language-tag (fprintf out "~s@~a" lexical-form language-tag))
      (datatype-iri (fprintf out "~s^^<~a>" lexical-form (url->string datatype-iri)))
      (else (fprintf out "~s" (literal-lexical-form val))))))

(writer->to-string literal ntriple)

;; -------------------------------------------------------------------------------------------------
;; Printing "atoms"
;; -------------------------------------------------------------------------------------------------

(define/contract (write-ntriple-blank-node val (out (current-output-port)))
  (->* (blank-node?) (output-port?) void?)
  (fprintf out "_:B~a" (blank-node->string val)))

(writer->to-string blank-node ntriple)

(define/contract (write-ntriple-resource val (out (current-output-port)))
  (->* (resource?) (output-port?) void?)
  (fprintf out "<~a>" (url->string val)))

(writer->to-string resource ntriple)

(define/contract (write-ntriple-subject val (out (current-output-port)))
  (->* (subject?) (output-port?) void?)
  (cond
    ((url? val) (write-ntriple-resource val out))
    ((blank-node? val) (write-ntriple-blank-node val out))))

(writer->to-string subject ntriple)

(define/contract (write-ntriple-predicate val (out (current-output-port)))
  (->* (predicate?) (output-port?) void?)
  (cond
    ((url? val) (write-ntriple-resource val out))))

(writer->to-string predicate ntriple)

(define/contract (write-ntriple-object val (out (current-output-port)))
  (->* (object?) (output-port?) void?)
  (cond
    ((url? val) (write-ntriple-resource val out))
    ((blank-node? val) (write-ntriple-blank-node val out))
    ((literal? val) (write-ntriple-literal val out))))

(writer->to-string object ntriple)

;; -------------------------------------------------------------------------------------------------
;; Printing statements
;; -------------------------------------------------------------------------------------------------

(define (write-inner-whitespace (out (current-output-port)))
  (display " " out))

(define (write-end-of-statement (out (current-output-port)))
  (displayln " ." out))

(define/contract (write-ntriple-statement stmt (out (current-output-port)))
  (->* (statement?) (output-port?) void?)
  (write-ntriple-subject (get-subject stmt) out)
  (write-inner-whitespace out)
  (write-ntriple-predicate (get-predicate stmt) out)
  (write-inner-whitespace out)
  (write-ntriple-object (get-object stmt) out)
  (write-end-of-statement out))

(writer->to-string statement ntriple)

(define/contract (write-nquad-statement graph-name stmt (out (current-output-port)))
  (->* (subject? statement?) (output-port?) void?)
  (write-ntriple-subject (get-subject stmt) out)
  (write-inner-whitespace out)
  (write-ntriple-predicate (get-predicate stmt) out)
  (write-inner-whitespace out)
  (write-ntriple-object (get-object stmt) out)
  (write-inner-whitespace out)
  (write-ntriple-subject graph-name out)
  (write-end-of-statement out))

(define (statement->nquad-string graph-name val)
  (with-output-to-string (λ () write-nquad-statement graph-name val)))

;; -------------------------------------------------------------------------------------------------
;; Printing graphs
;; -------------------------------------------------------------------------------------------------

(define/contract (write-ntriple-graph graph (out (current-output-port)))
  (->* (graph?) (output-port?) void?)
  (for-each
   (λ (stmt) (write-ntriple-statement stmt out))
   (graph-statements graph)))

(writer->to-string graph ntriple)

(define/contract (write-nquad-graph graph (out (current-output-port)))
  (->* (graph?) (output-port?) void?)
  (let ((graph-name (graph-name-or-blank graph)))
    (for-each
     (λ (stmt) (write-nquad-statement graph-name stmt out))
     (graph-statements graph))))

(writer->to-string graph nquad)

;; -------------------------------------------------------------------------------------------------
;; Printing data sets
;; -------------------------------------------------------------------------------------------------

(define/contract (write-trig-dataset dataset (out (current-output-port)))
  (->* (dataset?) (output-port?) void?)
  (for-each
   (λ (graph)
     (if (graph-named? graph)
         (fprintf out "~a {~n" (write-ntriple-subject (graph-name graph) out))
         (displayln "{" out))
     (write-ntriple-graph graph out)
     (displayln "}" out))
   (dataset-graphs dataset)))

(writer->to-string dataset trig)

;; -------------------------------------------------------------------------------------------------
;; Printing statement patterns
;; -------------------------------------------------------------------------------------------------

(define (write-pattern-component pattern (out (current-output-port)))
  (cond
    ((ignore? pattern) (display "_" out))
    ((comparitor? pattern) (write-ntriple-object (car pattern) out))
    ((variable? pattern) (fprintf out "?~a" pattern))))

(define (write-statement-pattern pattern (out (current-output-port)))
  (->* (statement-pattern?) (output-port?) void?)
  (write-pattern-component (first pattern) out)
  (write-inner-whitespace out)
  (write-pattern-component (second pattern) out)
  (write-inner-whitespace out)
  (write-pattern-component (third pattern) out)
  (write-end-of-statement out))

(writer->to-string statement-pattern)
