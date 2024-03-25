#lang racket/base

(require "./namespace.rkt"
         "./literal.rkt"
         "./statement.rkt"
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
         make-language-string
         make-typed-string
         literal?
         ;; --------------------------------------
         make-blank-node
         blank-node?
         subject?
         predicate?
         object?
          ;; --------------------------------------
         make-statement
         statement?
         statement-subject
         statement-predicate
         statement-object
         write-ntriple-statement
         write-nquad-statement
         ;; --------------------------------------
         make-default-graph
         make-named-graph
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
         dataset-empty?
         dataset-has-key?
         dataset-has-default?
         dataset-ref
         dataset-ref-default
         dataset-set!
         dataset-remove!)
