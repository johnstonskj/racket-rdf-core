#lang racket/base

;;
;; Vocabulary: Describing Linked Datasets with the VoID Vocabulary
;;
;; Namespace:        <http://rdfs.org/ns/void#>
;; Specification:    <http://vocab.deri.ie/void>
;; Last Date:        2011-03-06
;; Preferred prefix: `void`
;;
;; Support status: incomplete
;;
;; See also: Describing Linked Datasets with the VoID Vocabulary (https://www.w3.org/TR/void/)
;;

(require (only-in "../namespace.rkt" make-namespace make-name))

(provide (all-defined-out))

(define void: (make-namespace "http://rdfs.org/ns/void#" "void"))

;; well known end-point </.well-known/void>

(define void:Dataset (make-name void: "Dataset"))
(define void:DatasetDescription (make-name void: "DatasetDescription"))
(define void:Linkset (make-name void: "Linkset"))
(define void:TechnicalFeature (make-name void: "TechnicalFeature"))

(define void:class (make-name void: "class"))
(define void:class-partition (make-name void: "classPartition"))
(define void:classes (make-name void: "classes"))
(define void:data-dump (make-name void: "dataDump"))
(define void:distinct-objects (make-name void: "distinctObjects"))
(define void:distinct-subjects (make-name void: "distinctSubjects"))
(define void:documents (make-name void: "documents"))
(define void:entities (make-name void: "entities"))
(define void:example-resource (make-name void: "exampleResource"))
(define void:feature (make-name void: "feature"))
(define void:in-dataset (make-name void: "inDataset"))
(define void:link-predicate (make-name void: "linkPredicate"))
(define void:objects-target (make-name void: "objectsTarget"))
(define void:opensearch-description (make-name void: "openSearchDescription"))
(define void:properties (make-name void: "properties"))
(define void:property (make-name void: "property"))
(define void:property-partition (make-name void: "propertyPartition"))
(define void:root-resource (make-name void: "rootResource"))
(define void:sparql-endpoint (make-name void: "sparqlEndpoint"))
(define void:subjects-target (make-name void: "subjectsTarget"))
(define void:subset (make-name void: "subset"))
(define void:target (make-name void: "target"))
(define void:triples (make-name void: "triples"))
(define void:uri-lookup-endpoint (make-name void: "uriLookupEndpoint"))
(define void:uri-regex-pattern (make-name void: "uriRegexPattern"))
(define void:uri-space (make-name void: "uriSpace"))
(define void:vocabulary (make-name void: "vocabulary"))
