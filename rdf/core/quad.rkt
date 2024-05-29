#lang racket/base

(require racket/bool
         racket/contract
         racket/function
         racket/set
         ;; --------------------------------------
         "./graph.rkt"
         "./statement.rkt")

(provide (contract-out (quad? (-> any/c boolean?))
                       (quad (-> subject? predicate? object? graph-name? quad?))
                       (rename quad-graph-name get-graph-name (-> quad? graph-name?))
                       (statement->quad (-> statement? graph-name? quad?))
                       (graph->quads (-> graph? (set/c quad?)))))

;; -------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------

(struct quad (subject predicate object graph-name)
  #:transparent
  #:methods gen:statement
  ((define (get-subject triple) (quad-subject triple))
   (define (get-predicate triple) (quad-predicate triple))
   (define (get-object triple) (quad-object triple))))

(define (quad<? v1 v2)
  (and (statement<? v1 v2)
       (if (and v1 v2) (subject<? v1 v2) (false? v1))))

(define (statement->quad stmt graph-name)
  (quad (get-subject stmt)
        (get-predicate  stmt)
        (get-object stmt)
        graph-name))

(define (graph->quads graph)
  (let ((graph-name (if (graph-named? graph) (graph-name graph) (make-blank-node))))
    (set-map (curryr statement->quad graph-name) (graph-statements graph))))
