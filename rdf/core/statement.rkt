#lang racket/base

(require racket/contract
         racket/generic
         ;; --------------------------------------
         "./literal.rkt"
         "./namespace.rkt"
         "./v/rdf.rkt")

(provide (except-out (struct-out blank-node)
                     internal-make-blank-node)
         make-blank-node
         ;; --------------------------------------
         resource?
         subject?
         predicate?
         object?
         ;; --------------------------------------
         (rename-out (gen:any-statement gen:statement))
         (rename-out (any-statement? statement?))
         statement/c
         get-subject
         get-predicate
         get-object
         ;; --------------------------------------
         statement->list
         statement->reified-list
         ;; --------------------------------------
         statement-list?)

;; -------------------------------------------------------------------------------------------------
;; `blank-node` types
;; -------------------------------------------------------------------------------------------------

(define shared-blank-node-counter 1)
(define shared-blank-node-sem (make-semaphore 1))

(define (make-blank-node-label)
  (semaphore-wait shared-blank-node-sem)
  (let ((next-label shared-blank-node-counter))
    (set! shared-blank-node-counter (add1 next-label))
    (semaphore-post shared-blank-node-sem)
    next-label))

(struct blank-node (label)
  #:sealed
  #:transparent
  #:constructor-name internal-make-blank-node)

(define (make-blank-node)
  (internal-make-blank-node (make-blank-node-label)))

;; -------------------------------------------------------------------------------------------------
;; Predicates
;; -------------------------------------------------------------------------------------------------

(define resource? url-absolute?)

(define subject? (or/c resource? blank-node?))

(define predicate? resource?)

(define object? (or/c resource? blank-node? literal?))

;; -------------------------------------------------------------------------------------------------
;; `statement` generic type
;; -------------------------------------------------------------------------------------------------

(define-generics any-statement
  (get-subject any-statement)
  (get-predicate any-statement)
  (get-object any-statement)
  #:requires (get-subject get-predicate get-object))

(define statement/c
  (any-statement/c (get-subject (-> any-statement? subject?))
                   (get-predicate (-> any-statement? predicate?))
                   (get-object (-> any-statement? object?))))

;; -------------------------------------------------------------------------------------------------

(define (statement->list stmt)
  (list (get-subject stmt)
        (get-predicate stmt)
        (get-object stmt)))

(define (statement->reified-list stmt)
  (let ((blank-node (make-blank-node)))
    (list
     (list blank-node rdf:type rdf:Statement)
     (list blank-node rdf:subject (get-subject stmt))
     (list blank-node rdf:predicate (get-predicate stmt))
     (list blank-node rdf:object (get-object stmt)))))

;; -------------------------------------------------------------------------------------------------
;; Statement Lists
;; -------------------------------------------------------------------------------------------------

(define statement-list? (listof any-statement?))
