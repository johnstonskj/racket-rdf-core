#lang racket/base

(require racket/bool
         racket/contract
         racket/function
         racket/list
         racket/set
         racket/string
         ;; --------------------------------------
         "./literal.rkt"
         "./name.rkt"
         "./nsname.rkt"
         "./resource.rkt"
         "./statement.rkt"
         "./triple.rkt"
         "./v/rdf.rkt"
         "./v/rdfs.rkt")

(provide (contract-out (member-name/c contract?)
                       (schema-subject/c contract?)
                       (schema-predicate/c contract?)
                       (schema-object-one/c contract?)
                       (schema-object/c contract?)
                       (schema-base? contract?)
                       (schema-member/c contract?)
                       ;; --------------------------------------
                       (struct assertion
                         ((predicate schema-predicate/c)
                          (object schema-object-one/c)))
                       (assert (-> schema-predicate/c schema-object-one/c assertion?))
                       (multi-valued-assertion? (-> assertion? boolean?))
                       ;; --------------------------------------
                       (struct thing
                         ((name member-name/c)
                          (assertions (listof assertion?))))
                       (individual
                        (->* (member-name/c)
                             (#:label schema-object/c
                              #:comment schema-object/c)
                             #:rest (listof assertion?)
                             thing?))
                       (class
                         (->* (member-name/c)
                              (#:label schema-object/c
                               #:comment schema-object/c
                               #:subClassOf schema-subject/c)
                              #:rest (listof assertion?)
                              thing?))
                       (property
                        (->* (member-name/c)
                             (#:label schema-object/c
                              #:comment schema-object/c
                              #:domain schema-subject/c
                              #:range schema-subject/c
                              #:subPropertyOf schema-subject/c)
                             #:rest (listof assertion?)
                             thing?))
                       (datatype
                        (->* (member-name/c)
                             (#:label schema-object/c
                              #:comment schema-object/c
                              #:restricts schema-subject/c)
                             #:rest (listof assertion?)
                             thing?))
                       (container-kind/c flat-contract?)
                       (container
                        (->* (member-name/c)
                             (#:label schema-object/c
                              #:comment schema-object/c
                              #:kind container-kind/c)
                             #:rest (listof assertion?)
                             thing?))
                       ;; --------------------------------------
                       (struct schema
                         ((base resource-absolute?)
                          (members (listof schema-member/c))))
                       (make-schema (->* ((or/c string? schema-base?))
                                    (#:label schema-object/c
                                     #:comment schema-object/c)
                                    #:rest (listof schema-member/c)
                                    schema?))
                       (schema->statements (-> schema? (set/c statement?)))
                       ;; --------------------------------------
                       (comment (-> schema-object/c assertion?))
                       (label (-> schema-object/c assertion?))
                       (see-also (-> schema-object/c assertion?))
                       (is-defined-by (-> schema-object/c assertion?))
                       (type (-> schema-object/c assertion?))
                       (value (-> schema-object/c assertion?))
                       (a (-> schema-object/c assertion?))))

;; -------------------------------------------------------------------------------------------------
;; Internals
;; -------------------------------------------------------------------------------------------------

(define (merge-assertions initial assocs)
  (append initial
          (filter-map
           (λ (assoc) (if (false? (cdr assoc))
                          #f
                          (assert (car assoc) (cdr assoc))))
           assocs)))

(define schema-subject-one/c (or/c resource-absolute? nsname? symbol? string?))

(define schema-subject/c (or/c  schema-subject-one/c (listof  schema-subject-one/c)))

(define schema-predicate/c schema-subject-one/c)

(define schema-object-one/c (or/c resource-absolute? nsname? symbol? literal? boolean? number? string?))

(define schema-object/c (or/c schema-object-one/c (listof schema-object-one/c)))

(define (->subject schema subject)
  (cond
    ((resource-absolute? subject) subject)
    ((nsname? subject) (nsname->resource subject))
    ((symbol? subject) (combine-resource/relative (schema-base schema) (symbol->string subject)))
    ((and (string? subject) (string-prefix? subject "_:")) (make-blank-node (substring subject 2)))
    ((string? subject) (string->resource subject))
    (else (raise-argument-error 'subject "schema-subject/c" subject))))

(define (->predicate schema predicate)
  (cond
    ((resource-absolute? predicate) predicate)
    ((nsname? predicate) (nsname->resource predicate))
    ((symbol? predicate) (combine-resource/relative (schema-base schema) (symbol->string predicate)))
    ((string? predicate) (string->resource predicate))
    (else (raise-argument-error 'predicate "schema-predicate/c" predicate))))

(define (->object schema object)
  (cond
    ((resource-absolute? object) object)
    ((nsname? object) (nsname->resource object))
    ((symbol? object) (combine-resource/relative (schema-base schema) (symbol->string object)))
    ((literal? object) object)
    ((or (boolean? object) (number? object)) (->literal object))
    ((string? object) (make-untyped-literal object))
    ((list? object) (map (curry ->object schema) object))
    (else (raise-argument-error 'object "schema-object/c" object))))

;; -------------------------------------------------------------------------------------------------
;; Schema Types -- Thing
;; -------------------------------------------------------------------------------------------------

(define member-name/c (or/c local-name-string? symbol?))

(struct thing (name assertions) #:transparent)

(define (individual name #:label (label #f) #:comment (comment #f) . assertions)
  (thing
   name
   (merge-assertions assertions
                     (list (cons rdfs:label label)
                           (cons rdfs:comment comment)))))

(define resource individual)

(define (thing->statements schema thing)
  (map (curry assertion->statements schema (->subject schema (thing-name thing)))
       (thing-assertions thing)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Additional constructors
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (class name #:label (label #f) #:comment (comment #f)
          #:subClassOf (super #f) . assertions)
  (thing
   name
   (merge-assertions assertions
                     (list (cons rdf:type rdfs:Class)
                           (cons rdfs:label label)
                           (cons rdfs:comment comment)
                           (cons rdfs:subClassOf super)))))

(define (property name #:label (label #f) #:comment (comment #f)
                  #:domain (domain #f) #:range (range #f)
                  #:subPropertyOf (super #f) . assertions)
  (thing
   name
   (merge-assertions assertions
                     (list (cons rdf:type rdf:Property)
                           (cons rdfs:label label)
                           (cons rdfs:comment comment)
                           (cons rdfs:subPropertyOf super)
                           (cons rdfs:domain domain)
                           (cons rdfs:range range)))))

(define (datatype name #:label (label #f) #:comment (comment #f) #:restricts (restricts #f) . assertions)
  (thing
   name
   (merge-assertions assertions
                     (list (cons rdf:type rdfs:Datatype)
                           (cons rdfs:label label)
                           (cons rdfs:comment comment)))))

(define container-kind/c (or/c 'alt 'bag 'seq #f))
(define (container name #:label (label #f) #:comment (comment #f) #:kind (kind #f) . assertions)
  (thing
   name
   (merge-assertions assertions
                     (list (cons rdf:type rdfs:Datatype)
                           (cons rdf:type (cond
                                            ((eq? kind 'alt) rdf:Alt)
                                            ((eq? kind 'bag) rdf:Bag)
                                            ((eq? kind 'seq) rdf:Seq)
                                            (else #f)))
                           (cons rdfs:label label)
                           (cons rdfs:comment comment)))))

;; -------------------------------------------------------------------------------------------------
;; Schema Types -- Assertion
;; -------------------------------------------------------------------------------------------------

(struct assertion (predicate object) #:transparent)

(define (assert predicate object)
  (assertion predicate object))

(define (multi-valued-assertion? assertion)
  (list? (assertion-object assertion)))

(define (assertion->statements schema subject assertion)
  (map
   (λ (object) (triple (->subject schema subject)
                       (->predicate schema (assertion-predicate assertion))
                       (->object schema object)))
   (if (multi-valued-assertion? assertion)
       (assertion-object assertion)
       (list (assertion-object assertion)))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Additional constructors
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comment object)
  (assert rdfs:comment object))

(define (label object)
  (assert rdfs:label object))

(define (see-also object)
  (assert rdfs:seeAlso object))

(define (is-defined-by object)
  (assert rdfs:isDefinedBy object))

(define (type object)
  (assert rdf:type object))

(define (value object)
  (assert rdf:value object))

(define a type)

;; -------------------------------------------------------------------------------------------------
;; Schema Types -- Schema
;; -------------------------------------------------------------------------------------------------

(define schema-base? resource-absolute?)

(define schema-member/c (or/c thing? assertion?))

(struct schema (base members) #:transparent)

(define (make-schema base #:label (label #f) #:comment (comment #f) . members)
  (schema (cond
            ((schema-base? base) base)
            ((string? base) (string->resource base))
            (else (error)))
          (merge-assertions members
                            (list (cons rdfs:label label)
                                  (cons rdfs:comment comment)))))

(define (schema-member->statements schema member)
  (cond ((thing? member) (thing->statements schema member))
        ((assertion? member) (assertion->statements schema (schema-base schema) member))
        (else (raise-argument-error 'member "schema-member/c" member))))

(define (schema->statements schema)
  (list->set
   (flatten
    (filter-map (λ (member) (let ((lst (schema-member->statements schema member)))
                              (if (empty? lst) #f lst)))
                (schema-members schema)))))
