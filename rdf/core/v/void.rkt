#lang racket/base

;;
;; Vocabulary: Describing Linked Datasets with the VoID Vocabulary
;;
;; Specification:    <http://vocab.deri.ie/void>
;; Last Date:        2011-03-06
;;
;; Support status: incomplete
;;
;; See also: Describing Linked Datasets with the VoID Vocabulary (https://www.w3.org/TR/void/)
;;

(require (only-in "../name.rkt"
                  string->local-name)
         (only-in "../nsmap.rkt"
                  string->prefix
                  nsmap-set!)
         (only-in "../nsname.rkt"
                  make-nsname)
         (only-in "../resource.rkt"
                  string->resource))

(provide (all-defined-out))

;; ================================================================================================
;; Namespace definition
;; ================================================================================================

(define void-prefix-string "void")
(define void-namespace-string "http://rdfs.org/ns/void#")

(define void: (string->resource void-namespace-string))

(define (nsmap-add-void map)
  (nsmap-set! map
              (string->prefix void-prefix-string)
              (string->resource void-namespace-string)))

;; ================================================================================================
;; Name definitions
;; ================================================================================================

(define void:Dataset
  (make-nsname void: (string->local-name "Dataset")))

(define void:DatasetDescription
  (make-nsname void: (string->local-name "DatasetDescription")))

(define void:Linkset
  (make-nsname void: (string->local-name "Linkset")))

(define void:TechnicalFeature
  (make-nsname void: (string->local-name "TechnicalFeature")))

(define void:class
  (make-nsname void: (string->local-name "class")))

(define void:class-partition
  (make-nsname void: (string->local-name "classPartition")))

(define void:classes
  (make-nsname void: (string->local-name "classes")))

(define void:data-dump
  (make-nsname void: (string->local-name "dataDump")))

(define void:distinct-objects
  (make-nsname void: (string->local-name "distinctObjects")))

(define void:distinct-subjects
  (make-nsname void: (string->local-name "distinctSubjects")))

(define void:documents
  (make-nsname void: (string->local-name "documents")))

(define void:entities
  (make-nsname void: (string->local-name "entities")))

(define void:example-resource
  (make-nsname void: (string->local-name "exampleResource")))

(define void:feature
  (make-nsname void: (string->local-name "feature")))

(define void:in-dataset
  (make-nsname void: (string->local-name "inDataset")))

(define void:link-predicate
  (make-nsname void: (string->local-name "linkPredicate")))

(define void:objects-target
  (make-nsname void: (string->local-name "objectsTarget")))

(define void:opensearch-description
  (make-nsname void: (string->local-name "openSearchDescription")))

(define void:properties
  (make-nsname void: (string->local-name "properties")))

(define void:property
  (make-nsname void: (string->local-name "property")))

(define void:property-partition
  (make-nsname void: (string->local-name "propertyPartition")))

(define void:root-resource
  (make-nsname void: (string->local-name "rootResource")))

(define void:sparql-endpoint
  (make-nsname void: (string->local-name "sparqlEndpoint")))

(define void:subjects-target
  (make-nsname void: (string->local-name "subjectsTarget")))

(define void:subset
  (make-nsname void: (string->local-name "subset")))

(define void:target
  (make-nsname void: (string->local-name "target")))

(define void:triples
  (make-nsname void: (string->local-name "triples")))

(define void:uri-lookup-endpoint
  (make-nsname void: (string->local-name "uriLookupEndpoint")))

(define void:uri-regex-pattern
  (make-nsname void: (string->local-name "uriRegexPattern")))

(define void:uri-space
  (make-nsname void: (string->local-name "uriSpace")))

(define void:vocabulary
  (make-nsname void: (string->local-name "vocabulary")))

;; ================================================================================================
;; Other definitions
;; ================================================================================================

(define void-wellknown-path "/.well-known/void")

