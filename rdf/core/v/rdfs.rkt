#lang racket/base

;;
;; Vocabulary: RDF Schema 1.1
;;
;; Specification:    <https://www.w3.org/TR/rdf11-schema/>
;; Last Date:        2014-02-25
;;
;; Support status: complete
;;
;; See also: RDF 1.1 Concepts and Abstract Syntax (https://www.w3.org/TR/rdf11-concepts/)
;;

(require (only-in "../name.rkt"
                  string->local-name)
         (only-in "../namespace.rkt"
                  string->namespace
                  make-nsname)
         (only-in "../nsmap.rkt"
                  string->prefix
                  nsmap-set!))

(provide (all-defined-out))

;; ================================================================================================
;; Namespace definition
;; ================================================================================================

(define rdfs-prefix-string "rdfs")
(define rdfs-namespace-string "http://www.w3.org/2000/01/rdf-schema#")

(define rdfs: (string->namespace rdfs-namespace-string))

(define (nsmap-add-rdf-schema map)
  (nsmap-set! map
              (string->prefix rdfs-prefix-string)
              (string->namespace rdfs-namespace-string)))

;; ================================================================================================
;; Name definitions
;; ================================================================================================

(define rdfs:Resource
  (make-nsname rdfs: (string->local-name "Resource")))

(define rdfs:Class
  (make-nsname rdfs: (string->local-name "Class")))

(define rdfs:Literal
  (make-nsname rdfs: (string->local-name "Literal")))

(define rdfs:Datatype
  (make-nsname rdfs: (string->local-name "Datatype")))

(define rdfs:range
  (make-nsname rdfs: (string->local-name "range")))

(define rdfs:domain
  (make-nsname rdfs: (string->local-name "domain")))

(define rdfs:sub-class-of
  (make-nsname rdfs: (string->local-name "subClassOf")))

(define rdfs:sub-property-of
  (make-nsname rdfs: (string->local-name "subPropertyOf")))

(define rdfs:label
  (make-nsname rdfs: (string->local-name "label")))

(define rdfs:comment
  (make-nsname rdfs: (string->local-name "comment")))

(define rdfs:Container
  (make-nsname rdfs: (string->local-name "Container")))

(define rdfs:Container-Membership-Property
  (make-nsname rdfs: (string->local-name "ContainerMembershipProperty")))

(define rdfs:member
  (make-nsname rdfs: (string->local-name "member")))

(define rdfs:see-also
  (make-nsname rdfs: (string->local-name "seeAlso")))

(define rdfs:is-defined-by
  (make-nsname rdfs: (string->local-name "isDefinedBy")))
