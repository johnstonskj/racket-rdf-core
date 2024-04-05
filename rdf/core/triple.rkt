#lang racket/base

(require racket/contract
         racket/list
         ;; --------------------------------------
         net/url-string
         ;; --------------------------------------
         "./literal.rkt"
         "./statement.rkt"
         "./v/rdf.rkt")

(provide (struct-out triple)
         make-triple
         make-reified-triples
         statement->reified-triples
         ;; --------------------------------------
         make-statement-list
         make-anon-statement-list
         ;; --------------------------------------
         statement-add-to-subject
         statement-add-to-object
         make-type-statement
         statement-add-type-to-subject)

;; -------------------------------------------------------------------------------------------------
;; `triple` concrete type
;; -------------------------------------------------------------------------------------------------

(struct triple (subject predicate object)
  #:sealed
  #:transparent
  #:constructor-name make-triple
  #:guard (struct-guard/c subject? predicate? object?)
  #:methods gen:statement
  ((define (get-subject triple) (triple-subject triple))
   (define (get-predicate triple) (triple-predicate triple))
   (define (get-object triple) (triple-object triple))))

(define/contract (list->triple stmt)
  (-> (list/c subject? predicate? object?) triple?)
  (apply make-triple stmt))

(define/contract (statement->reified-triples stmt)
  (-> statement? (listof triple?))
  (map list->triple (statement->reified-list stmt)))

(define/contract (make-reified-triples subject predicate object)
  (-> subject? predicate? object? (listof triple?))
  (statement->reified-list (make-triple subject predicate object)))

;; -------------------------------------------------------------------------------------------------
;; Statement Lists
;; -------------------------------------------------------------------------------------------------

(define (make-statement-list subject predicate-object-list)
  (let ((common-subject (if (string? subject) (string->url subject) subject)))
    (flatten
     (map
      (λ (pair)
        (let* ((this-predicate (car pair))
               (common-predicate (if (string? this-predicate)
                                     (string->url this-predicate)
                                     this-predicate))
               (this-object-list (cadr pair)))
          (map (λ (this-object) (make-triple
                                 common-subject
                                 common-predicate
                                 (if (literal? this-object) this-object (->literal this-object))))
               this-object-list)))
      predicate-object-list))))

(define (make-anon-statement-list predicate-object-list)
  (make-statement-list (make-blank-node) predicate-object-list))

;; -------------------------------------------------------------------------------------------------
;; Additional Constructors
;; -------------------------------------------------------------------------------------------------

(define/contract (statement-add-to-subject stmt predicate object)
  (-> statement? predicate? object? triple?)
  (make-triple (get-subject stmt) predicate object))

(define/contract (statement-add-to-object stmt predicate object)
  (-> statement? predicate? object? triple?)
  (make-triple (get-object stmt) predicate object))

(define/contract (make-type-statement subject type)
  (-> subject? resource? triple?)
  (make-triple subject rdf:type type))

(define/contract (statement-add-type-to-subject stmt type)
  (-> statement? resource? triple?)
  (make-type-statement (get-subject stmt) type))
