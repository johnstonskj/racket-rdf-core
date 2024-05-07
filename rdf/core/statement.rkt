#lang racket/base

(require racket/contract
         racket/generic
         ;; --------------------------------------
         "./literal.rkt"
         (only-in "./name.rkt" local-name-string?)
         "./namespace.rkt"
         "./v/rdf.rkt"
         ;; --------------------------------------
         (only-in "./private/sparql-names.rkt"
                  blank-node-string?
                  blank-node-label-string?))

(provide gen:statement
         (rename-out (new-statement/c statement/c))
         (contract-out
          (blank-node-string? (-> any/c boolean?))
          (blank-node-label-string? (-> any/c boolean?))
          (blank-node? (-> any/c boolean?))
          (make-blank-node (->* () ((or/c blank-node-label-string? #f)) blank-node?))
          (blank-node->string (-> blank-node? local-name-string?))
          ;; --------------------------------------
          (resource? (-> any/c boolean?))
          (subject? (-> any/c boolean?))
          (predicate? (-> any/c boolean?))
          (object? (-> any/c boolean?))
          ;; --------------------------------------
          (statement? (-> any/c boolean?))
          (get-subject (-> statement? subject?))
          (get-predicate (-> statement? predicate?))
          (get-object (-> statement? object?))
          (statement-constructor/c contract?)
          ;; --------------------------------------
          (statement-list? (-> any/c boolean?))
          (statement->list (-> statement? (list/c subject? predicate? object?)))
          (statement->reified-list (-> statement? (listof statement?)))))

;; -------------------------------------------------------------------------------------------------
;; `blank-node` types
;; -------------------------------------------------------------------------------------------------

;; pick a random starting point so clients don't assume we start from 1 every time!
(define shared-blank-node-counter (random 1 1000))
(define shared-blank-node-sem (make-semaphore 1))

(define (make-blank-node-label)
  (call-with-semaphore
   shared-blank-node-sem
   (λ () (let ((next-label shared-blank-node-counter))
           (set! shared-blank-node-counter (add1 next-label))
           (format "B~X"next-label)))))

(struct blank-node (label)
  #:transparent
  #:constructor-name internal-make-blank-node)

(define (make-blank-node (label #f))
  (internal-make-blank-node (if label label (make-blank-node-label))))

(define (blank-node->string blank)
  (blank-node-label blank))

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

(define-generics statement
  (get-subject statement)
  (get-predicate statement)
  (get-object statement)
  #:requires (get-subject get-predicate get-object))

(define new-statement/c
  (statement/c (get-subject (-> statement? subject?))
               (get-predicate (-> statement? predicate?))
               (get-object (-> statement? object?))))

(define statement-constructor/c
  (-> subject? predicate? object? new-statement/c))

(define statement-list? (listof statement?))

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
