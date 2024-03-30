#lang racket/base

(require racket/contract
         ;; --------------------------------------
         "./graph.rkt"
         "./literal.rkt"
         "./statement.rkt")

(provide (struct-out quad)
         statement->quad
         graph->quads)

(struct quad (subject predicate object graph-name)
  #:sealed
  #:transparent
  #:constructor-name make-quad
  #:guard (struct-guard/c resource? blank-node? literal? graph-name?)
  #:methods gen:statement
  ((define (get-subject triple) (quad-subject triple))
   (define (get-predicate triple) (quad-predicate triple))
   (define (get-object triple) (quad-object triple))))

(define/contract (statement->quad stmt graph-name)
  (-> statement? graph-name? quad?)
  (make-quad (get-subject stmt)
             (get-predicate  stmt)
             (get-object stmt)
             graph-name))

(define (graph->quads graph)
  (let ((graph-name (if (graph-named? graph) (graph-name graph) (make-blank-node))))
    (map (λ (stmt) (statement->quad stmt graph-name)) (graph-statements graph))))
