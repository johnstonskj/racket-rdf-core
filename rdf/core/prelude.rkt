#lang racket/base

(require "./namespace.rkt"
         "./literal.rkt"
         "./statement.rkt"
         "./triple.rkt"
         "./graph.rkt"
         "./dataset.rkt"
         "./io.rkt")

(provide ncname?
         string->ncname
         symbol->ncname
         make-namespace
         namespace?
         make-name
         name?
         ;; --------------------------------------
         literal?
         literal-lexical-form
         literal-datatype-iri
         literal-language-tag
         make-untyped-literal
         make-typed-literal
         make-lang-string-literal
         write-ntriple-literal
         ;; --------------------------------------
         make-blank-node
         blank-node?
         subject?
         predicate?
         object?
         statement?
         get-subject
         get-predicate
         get-object
         ;; --------------------------------------
         make-triple
         triple?
         write-ntriple-statement
         write-nquad-statement
         ;; --------------------------------------
         make-default-graph
         make-named-graph
         graph?
         graph-named?
         graph-empty?
         graph-member?
         graph-add
         graph-add-all
         graph-remove
         graph-remove-all
         write-ntriple-graph
         write-nquad-graph
         ;; --------------------------------------
         make-dataset
         dataset?
         dataset-empty?
         dataset-has-key?
         dataset-has-default?
         dataset-ref
         dataset-ref-default
         dataset-set!
         dataset-remove!)
