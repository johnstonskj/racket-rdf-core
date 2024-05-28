#lang racket/base

(require racket/contract
         racket/list
         racket/set
         ;; --------------------------------------
         "./literal.rkt"
         "./resource.rkt"
         "./statement.rkt"
         "./v/rdf.rkt")

(provide (contract-out (triple? (-> any/c boolean?))
                       (triple (-> subject? predicate? object? triple?))
                       (list->triple
                        (-> (list/c subject? predicate? object?) triple?))
                       (statement->reified-triples
                        (->* (statement?) ((or/c subject? #f)) (set/c triple?)))
                       ;; --------------------------------------
                       (make-triple
                        (-> (or/c subject? string?)
                            (or/c predicate? string?)
                            (or/c object? string? boolean? number?)
                            triple?))
                       (make-statements
                        (-> (or/c subject? string?)
                            (listof
                             (list/c (or/c predicate? string?)
                                     (or/c (or/c object? string? boolean? number?)
                                           (listof
                                            (or/c object? string? boolean? number?)))))
                            (set/c triple?)))
                       (make-anon-statements
                        (-> (listof
                             (list/c (or/c predicate? string?)
                                     (or/c (or/c object? string? boolean? number?)
                                           (listof
                                            (or/c object? string? boolean? number?)))))
                            (set/c triple?)))
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

(define (statement->reified-triples  stmt (new-subject #f))
  (set-map
   list->triple
   (statement->reified-set stmt new-subject)))

;; -------------------------------------------------------------------------------------------------
;; Statement Lists
;; -------------------------------------------------------------------------------------------------

(define (make-triple subject predicate object)
  (triple (cond ((subject? subject) subject)
                ((string? subject) (string->resource subject))
                (else (raise-argument-error 'subject "subject?" subject)))
          (cond ((predicate? predicate) predicate)
                ((string? predicate) (string->resource predicate))
                (else (raise-argument-error 'predicate "predicate?" predicate)))
          (cond ((object? object) object)
                (else (->literal object)))))

(define (make-statements subject predicate-object-list)
  (list->set
   (flatten
    (map (λ (predicate+object-list)
           (let ((objects (cadr predicate+object-list)))
             (if (list? objects)
               (map (λ (object)
                      (make-triple subject (car predicate+object-list) object))
                    objects)
               (make-triple subject (car predicate+object-list) objects))))
         predicate-object-list))))

(define (make-anon-statements predicate-object-list)
  (make-statements (make-blank-node) predicate-object-list))

;; -------------------------------------------------------------------------------------------------
;; Additional Constructors
;; -------------------------------------------------------------------------------------------------

(define (statement-add-to-subject stmt predicate object)
  (triple (get-subject stmt) predicate object))

(define (statement-add-to-object stmt predicate object)
  (triple (get-object stmt) predicate object))

(define (statement-add-type-to-subject stmt type)
  (type-statement (get-subject stmt) type))
