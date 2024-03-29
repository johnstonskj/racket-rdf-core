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
         "./lang.rkt"
         "./literal.rkt"
         "./statement.rkt"
         "./graph.rkt"
         "./dataset.rkt"
         "./gq.rkt")

(provide import-style/c
         namespace->import-statement
         ;; --------------------------------------
         write-turtle-literal
         literal->turtle-string
         ;; --------------------------------------
         write-turtle-blank-node
         blank-node->turtle-string
         write-turtle-resource
         resource->turtle-string
         write-turtle-subject
         subject->turtle-string
         write-turtle-predicate
         predicate->turtle-string
         write-turtle-object
         object->turtle-string
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
         write-ntriple-dataset
         dataset->ntriple-string
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

(define import-style/c (or/c 'ttl))

(define/contract (namespace->import-statement ns style)
  (-> namespace? import-style/c string?)
  (when (and (namespace? ns) (import-style/c style))
    (cond
      ((symbol=? style 'ttl)
       (format "@prefix ~a: <~a>.~n" (namespace-prefix ns) (url->string (namespace-url ns)))))))

;; -------------------------------------------------------------------------------------------------
;; Printing literals
;; -------------------------------------------------------------------------------------------------

(define (write-turtle-literal val (out (current-output-port)))
  ;;(->* (literal?) (output-port?) void?)
  (let ((lexical-form (literal-lexical-form val))
        (language-tag (literal-language-tag val))
        (datatype-iri (literal-datatype-iri val)))
    (cond
      (language-tag (fprintf out "~s@~a" lexical-form language-tag))
      (datatype-iri (fprintf out "~s^^<~a>" lexical-form (url->string datatype-iri)))
      (else (fprintf out "~s" (literal-lexical-form val))))))

(writer->to-string literal turtle)

;; -------------------------------------------------------------------------------------------------
;; Printing "atoms"
;; -------------------------------------------------------------------------------------------------

(define (write-turtle-blank-node val (out (current-output-port)))
  ;;(-> blank-node? output-port? void?)
  (fprintf out "_:~X" (blank-node-label val)))

(writer->to-string blank-node turtle)

(define (write-turtle-resource val (out (current-output-port)))
  ;;(-> resource? output-port? void?)
  (fprintf out "<~a>" (url->string val)))

(writer->to-string resource turtle)

(define (write-turtle-subject val (out (current-output-port)))
  ;;(-> subject? output-port? void?)
  (cond
    ((url? val) (write-turtle-resource val out))
    ((blank-node? val) (write-turtle-blank-node val out))))

(writer->to-string subject turtle)

(define (write-turtle-predicate val (out (current-output-port)))
  ;;(-> resource? output-port? void?)
  (cond
    ((url? val) (write-turtle-resource val out))))

(writer->to-string predicate turtle)

(define/contract (write-turtle-object val (out (current-output-port)))
  (->* (object?) (output-port?) void?)
  (cond
    ((url? val) (write-turtle-resource val out))
    ((blank-node? val) (write-turtle-blank-node val out))
    ((literal? val) (write-turtle-literal val out))
    (else (fprintf out "WTF: ~s?" val))))

(writer->to-string object turtle)

;; -------------------------------------------------------------------------------------------------
;; Printing statements
;; -------------------------------------------------------------------------------------------------

(define (write-inner-whitespace (out (current-output-port)))
  (display " " out))

(define (write-end-of-statement (out (current-output-port)))
  (displayln " ." out))

(define (write-ntriple-statement stmt (out (current-output-port)))
  ;;(-> statement? output-port? void?)
  (write-turtle-subject (statement-subject stmt) out)
  (write-inner-whitespace out)
  (write-turtle-predicate (statement-predicate stmt) out)
  (write-inner-whitespace out)
  (write-turtle-object (statement-object stmt) out)
  (write-end-of-statement out))

(writer->to-string statement ntriple)

(define (write-nquad-statement graph-name stmt (out (current-output-port)))
  ;;(-> resource? statement? output-port? void?)
  (write-turtle-subject (statement-subject stmt) out)
  (write-inner-whitespace out)
  (write-turtle-predicate (statement-predicate stmt) out)
  (write-inner-whitespace out)
  (write-turtle-object (statement-object stmt) out)
  (write-inner-whitespace out)
  (write-turtle-subject graph-name out)
  (write-end-of-statement out))

(define (statement->nquad-string graph-name val)
  (with-output-to-string (λ () write-nquad-statement graph-name val)))

;; -------------------------------------------------------------------------------------------------
;; Printing graphs
;; -------------------------------------------------------------------------------------------------

(define (write-ntriple-graph graph (out (current-output-port)))
  ;;(-> graph? outout-port?? void?)
  (for-each
   (λ (stmt) (write-ntriple-statement stmt out))
   (graph-statements graph)))

(writer->to-string graph ntriple)

(define (write-nquad-graph graph (out (current-output-port)))
  ;;(-> graph? outout-port?? void?)
  (let ((graph-name (if (false? (graph-name graph)) (make-blank-node) (graph-name graph))))
    (for-each
     (λ (stmt) (write-nquad-statement graph-name stmt out))
     (graph-statements graph))))

(writer->to-string graph nquad)

;; -------------------------------------------------------------------------------------------------
;; Printing data sets
;; -------------------------------------------------------------------------------------------------

(define (write-ntriple-dataset dataset (out (current-output-port)))
  ;;(-> dataset? outout-port?? void?)
  (for-each
   (λ (graph)
     (when (graph-named? graph)
       (fprintf out "graph ~a {~n" (write-turtle-subject (graph-name graph) out)))
     (write-ntriple-graph graph out)
     (displayln "}" out))
   (dataset-graphs dataset)))

(writer->to-string dataset ntriple)

;; -------------------------------------------------------------------------------------------------
;; Printing statement patterns
;; -------------------------------------------------------------------------------------------------

(define (write-match-component pattern (out (current-output-port)))
  (cond
    ((ignore? pattern) (display "_" out))
    ((comparitor? pattern) (write-turtle-object (car pattern) out))
    ((variable? pattern) (fprintf out "?~a" pattern))))

(define (write-statement-pattern pattern (out (current-output-port)))
  ;;(-> statement-pattern? output-port? void?)
  (write-match-component (first pattern) out)
  (write-inner-whitespace out)
  (write-match-component (second pattern) out)
  (write-inner-whitespace out)
  (write-match-component (third pattern) out)
  (write-end-of-statement out))

(writer->to-string statement-pattern)
