#lang racket/base

(require racket/contract
         racket/list
         ;; --------------------------------------
         net/url-string
         ;; --------------------------------------
         "./literal.rkt"
         "./statement.rkt"
         "./v/rdf.rkt")

(provide (contract-out
          (triple? (-> any/c boolean?))
          (triple (-> subject? predicate? object? triple?))
          (list->triple (-> (list/c subject? predicate? object?) triple?))
          (reify (-> subject? predicate? object? (listof triple?)))
          (statement->reified-triples (-> statement? (listof triple?)))
          ;; --------------------------------------
          (statement-list
           (-> (or/c subject? string?)
               (listof (list/c (or/c predicate? string?)
                               (or/c (or/c object? string? boolean? number?)
                                     (listof (or/c object? string? boolean? number?)))))
               (listof triple?)))
          (anon-statement-list
           (-> (listof (list/c (or/c predicate? string?)
                               (or/c (or/c object? string? boolean? number?)
                                     (listof (or/c object? string? boolean? number?)))))
               (listof triple?)))
          ;; --------------------------------------
          (statement-add-to-subject(-> statement? predicate? object? triple?))
          (statement-add-to-object (-> statement? predicate? object? triple?))
          (type-statement (-> subject? resource? triple?))
          (statement-add-type-to-subject (-> statement? resource? triple?))))

;; -------------------------------------------------------------------------------------------------
;; `triple` concrete type
;; -------------------------------------------------------------------------------------------------

(struct triple (subject predicate object)
  #:transparent
  #:guard (struct-guard/c subject? predicate? object?)
  #:methods gen:statement
  ((define (get-subject triple) (triple-subject triple))
   (define (get-predicate triple) (triple-predicate triple))
   (define (get-object triple) (triple-object triple))))

(define (type-statement subject type)
  (triple subject rdf:type type))

(define (list->triple stmt)
  (apply triple stmt))

(define (statement->reified-triples stmt)
  (map list->triple (statement->reified-list stmt)))

(define (reify subject predicate object)
  (statement->reified-list (triple subject predicate object)))

;; -------------------------------------------------------------------------------------------------
;; Statement Lists
;; -------------------------------------------------------------------------------------------------

(define (statement-list subject predicate-object-list)
  (let ((common-subject (if (string? subject) (string->url subject) subject)))
    (flatten
     (map
      (λ (pair)
        (let* ((this-predicate (car pair))
               (common-predicate (if (string? this-predicate)
                                     (string->url this-predicate)
                                     this-predicate))
               (this-object-list (cadr pair)))
          (map (λ (this-object) (triple
                                 common-subject
                                 common-predicate
                                 (if (literal? this-object) this-object (->literal this-object))))
               this-object-list)))
      predicate-object-list))))

(define (anon-statement-list predicate-object-list)
  (statement-list (make-blank-node) predicate-object-list))

;; -------------------------------------------------------------------------------------------------
;; Additional Constructors
;; -------------------------------------------------------------------------------------------------

(define (statement-add-to-subject stmt predicate object)
  (triple (get-subject stmt) predicate object))

(define (statement-add-to-object stmt predicate object)
  (triple (get-object stmt) predicate object))

(define (statement-add-type-to-subject stmt type)
  (type-statement (get-subject stmt) type))
