#lang racket/base

(require racket/contract
         racket/generic
         racket/set
         ;; --------------------------------------
         "./literal.rkt"
         (only-in "./name.rkt"
                  local-name-string?)
         "./nsname.rkt"
         "./resource.rkt"
         "./v/rdf.rkt"
         ;; --------------------------------------
         (only-in "./private/sparql-names.rkt"
                  blank-node-string?
                  blank-node-label-string?))

(provide gen:statement
         (rename-out (new-statement/c statement/c))
         (contract-out (blank-node-string? (-> any/c boolean?))
                       (blank-node-label-string? (-> any/c boolean?))
                       ;; - - - - - - - - - - - - - - - - - - -
                       (blank-node? (-> any/c boolean?))
                       (make-blank-node
                        (->* () ((or/c blank-node-label-string? #f)) blank-node?))
                       (blank-node->string (-> blank-node? local-name-string?))
                       (blank-node<? (-> blank-node? blank-node? boolean?))
                       ;; - - - - - - - - - - - - - - - - - - -
                       (blank-node-label-maker/c contract?)
                       (blank-node-label-maker (parameter/c blank-node-label-maker/c))
                       ;; --------------------------------------
                       (subject? (-> any/c boolean?))
                       (predicate? (-> any/c boolean?))
                       (object? (-> any/c boolean?))
                       (subject<? (-> subject? subject? boolean?))
                       (predicate<? (-> predicate? predicate? boolean?))
                       (object<? (-> object? object? boolean?))
                       ;; --------------------------------------
                       (statement? (-> any/c boolean?))
                       (statement<? (-> statement? statement? boolean?))
                       (get-subject (-> statement? subject?))
                       (get-predicate (-> statement? predicate?))
                       (get-object (-> statement? object?))
                       (statement-constructor/c contract?)
                       ;; --------------------------------------
                       (statement-list? (-> any/c boolean?))
                       (statement->list (-> statement? (list/c subject? predicate? object?)))
                       (statement->reified-set
                        (->* (statement?)
                             ((or/c subject? #f))
                             (set/c (list/c subject? predicate? object?))))))

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

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define blank-node-label-maker/c (-> blank-node-label-string?))

(define blank-node-label-maker
  (make-parameter make-blank-node-label #f 'blank-node-label-maker))

(struct blank-node (label)
  #:constructor-name internal-make-blank-node
  #:methods gen:equal+hash
  [
   (define equal-proc (λ (v1 v2 _) (string=? (blank-node-label v1) (blank-node-label v2))))
   (define hash-proc (λ (val _) (equal-hash-code (blank-node-label val))))
   (define hash2-proc (λ (val _) (- (equal-hash-code (blank-node-label val)))))
   ])

(define (make-blank-node (label #f))
  (internal-make-blank-node
   (if label label
        (let ((label-maker (blank-node-label-maker)))
          (label-maker)))))

(define (blank-node->string blank)
  (blank-node-label blank))

(define (blank-node<? v1 v2)
  (string<? (blank-node-label v1) (blank-node-label v2)))

;; -------------------------------------------------------------------------------------------------
;; Predicates
;; -------------------------------------------------------------------------------------------------

;; TODO: when do these become absolute?
(define subject? (or/c resource? blank-node?))

(define predicate? resource?)

(define object? (or/c resource? blank-node? literal?))

(define (subject<? v1 v2)
  (cond
    ((and (resource? v1) (resource? v2))
     (resource<? v1 v2))
    ((and (blank-node? v1) (blank-node? v2))
     (blank-node<? v1 v2))
    (else #f)))

(define predicate<? resource<?)

(define (object<? v1 v2)
  (cond
    ((and (resource? v1) (resource? v2))
     (resource<? v1 v2))
    ((and (blank-node? v1) (blank-node? v2))
     (blank-node<? v1 v2))
    ((and (literal? v1) (literal? v2))
     (literal<? v1 v2))
    (else #f)))

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

(define (statement<? v1 v2)
  (and (statement? v1)
       (statement? v2)
       (subject<? (get-subject v1) (get-subject v2))
       (predicate<? (get-predicate v1) (get-predicate v2))
       (object<? (get-object v1) (get-object v2))))

;; -------------------------------------------------------------------------------------------------

(define (statement->list stmt)
  (list (get-subject stmt)
        (get-predicate stmt)
        (get-object stmt)))

(define (statement->reified-set stmt (new-subject #f))
  (let ((new-subject (or new-subject (make-blank-node))))
    (set
     (list new-subject (nsname->resource rdf:type) (nsname->resource rdf:Statement))
     (list new-subject (nsname->resource rdf:subject) (get-subject stmt))
     (list new-subject (nsname->resource rdf:predicate) (get-predicate stmt))
     (list new-subject (nsname->resource rdf:object) (get-object stmt)))))
