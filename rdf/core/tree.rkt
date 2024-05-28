#lang racket/base

(require racket/contract
         racket/function
         racket/set
         ;; --------------------------------------
         "./graph.rkt"
         "./statement.rkt"
         "./triple.rkt")

(provide (contract-out (graph-tree-twigs/c contract?)
                       (graph-tree-branch/c contract?)
                       (graph-tree/c contract?)
                       ;; --------------------------------------
                       (graph->tree (-> graph? graph-tree/c))
                       (statements->tree (-> statement-set? graph-tree/c))
                       (tree->statements (-> graph-tree/c statement-set?))))

;; -------------------------------------------------------------------------------------------------
;; Tree types
;; -------------------------------------------------------------------------------------------------

(define graph-tree-twigs/c (listof object?))

(define graph-tree-branch/c (hash/c predicate? graph-tree-twigs/c))

(define graph-tree/c (hash/c subject? graph-tree-branch/c))

;; -------------------------------------------------------------------------------------------------
;; From graph to tree
;; -------------------------------------------------------------------------------------------------

(define (tree-add-statement root statement)
  (let ((branch (hash-ref root (get-subject statement) #f)))
    (if branch
        (let ((twigs (hash-ref branch (get-predicate statement) #f)))
          (hash-set! branch
                     (get-predicate statement)
                     (if twigs
                         (cons (get-object statement) twigs)
                         (list (get-object statement)))))
        (hash-set! root (get-subject statement)
                   (make-hash (list
                               (cons (get-predicate statement)
                                     (list (get-object statement)))))))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (statements->tree statements)
  (let ((root (make-hash)))
    (set-for-each statements (curry tree-add-statement root))
    root))

(define (graph->tree graph)
  (statements->tree (graph-statements graph)))

;; -------------------------------------------------------------------------------------------------
;; From graph to tree
;; -------------------------------------------------------------------------------------------------

(define (tree-twigs->statements subject predicate twigs)
  (set-map (λ (twig) (triple subject predicate twig)) twigs))

(define (tree-branch->statements subject branch)
  (hash-map branch (curry tree-twigs->statements subject)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (tree->statements tree)
  (hash-map tree tree-branch->statements))
