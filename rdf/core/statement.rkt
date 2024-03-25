#lang racket/base

(require racket/contract
         racket/list
         ;; --------------------------------------
         net/url-string
         net/url-structs
         ;; --------------------------------------
         "./literal.rkt"
         (prefix-in rdf: (except-in "./v/rdf.rkt" *namespace*)))

(provide (except-out (struct-out blank-node)
                     internal-make-blank-node)
         make-blank-node
         ;; --------------------------------------
         resource?
         subject?
         predicate?
         object?
         ;; --------------------------------------
         (struct-out statement)
         make-statement
         make-reified-statement
         statement-reify
         statement->triple
         ;; --------------------------------------
         statement-list?
         make-statement-list
         make-anon-statement-list
         ;; --------------------------------------
         statement-add-to-subject
         statement-add-to-object
         make-type-statement
         statement-add-type-to-subject)

;; -------------------------------------------------------------------------------------------------
;; `blank-node` types
;; -------------------------------------------------------------------------------------------------

(define shared-blank-node-counter 1)
(define shared-blank-node-sem (make-semaphore 1))

(define (next-blank-node-id)
  (semaphore-wait shared-blank-node-sem)
  (let ((next-id shared-blank-node-counter))
    (set! shared-blank-node-counter (add1 next-id))
    (semaphore-post shared-blank-node-sem)
    next-id))

(struct blank-node (id)
  #:sealed
  #:constructor-name internal-make-blank-node)

(define (make-blank-node)
  (internal-make-blank-node (next-blank-node-id)))

;; -------------------------------------------------------------------------------------------------
;; `statement` types
;; -------------------------------------------------------------------------------------------------

(define resource? url?)

(define subject? (or/c resource? blank-node?))

(define predicate? resource?)

(define object? (or/c resource? blank-node? literal?))

(struct statement (subject predicate object)
  #:sealed
  #:constructor-name make-statement
  #:guard (struct-guard/c subject? predicate? object?))

(define (make-reified-statement subject predicate object)
  (let ((blank-node (make-blank-node)))
    (list
     (make-statement blank-node rdf:type rdf:Statement)
     (make-statement blank-node rdf:subject subject)
     (make-statement blank-node rdf:predicate predicate)
     (make-statement blank-node rdf:object object))))

(define (statement-reify stmt)
  (make-reified-statement
   (statement-subject stmt)
   (statement-predicate stmt)
   (statement-object stmt)))

(define (statement->triple stmt)
  (list (statement-subject stmt)
        (statement-predicate stmt)
        (statement-object stmt)))

;; -------------------------------------------------------------------------------------------------
;; Statement Lists
;; -------------------------------------------------------------------------------------------------

(define statement-list? (listof statement?))

(define (make-statement-list subject predicate-object-list)
  (let ((common-subject (if (string? subject) (string->url subject) subject)))
    (flatten
     (map
      (λ (pair)
        (let* ((this-predicate (car pair))
               (common-predicate (if (string? this-predicate) (string->url this-predicate) this-predicate))
               (this-object-list (cadr pair)))
          (map (λ (this-object) (make-statement common-subject common-predicate this-object))
               this-object-list)))
      predicate-object-list))))

(define (make-anon-statement-list predicate-object-list)
  (make-statement-list (make-blank-node) predicate-object-list))

;; -------------------------------------------------------------------------------------------------
;; Additional Constructors
;; -------------------------------------------------------------------------------------------------

(define (statement-add-to-subject stmt predicate object)
  (make-statement (statement-subject stmt) predicate object))

(define (statement-add-to-object stmt predicate object)
  (make-statement (statement-object stmt) predicate object))

(define (make-type-statement subject object) (make-statement subject rdf:type object))

(define (statement-add-type-to-subject stmt object) (make-type-statement (statement-subject stmt) object))
