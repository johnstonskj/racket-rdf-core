#lang racket/base

;;
;; Vocabulary: SPARQL 1.1 Service Description
;;
;; Specification:    <https://www.w3.org/TR/sparql11-service-description/>
;; Last Date:        2013-03-21
;;
;; Support status: incomplete
;;
;; See also: Describing Linked Datasets with the VoID Vocabulary (https://www.w3.org/TR/void/)
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

(define sd-prefix-string "sd")
(define sd-namespace-string "http://www.w3.org/ns/sparql-service-description#")

(define sd: (string->namespace sd-namespace-string))

(define (nsmap-add-service-description map)
  (nsmap-set! map
              (string->prefix sd-prefix-string)
              (string->namespace sd-namespace-string)))

;; ================================================================================================
;; Name definitions
;; ================================================================================================

(define sd:Dataset
  (make-nsname sd: (string->local-name "Dataset")))

(define sd:Graph
  (make-nsname sd: (string->local-name "Graph")))

(define sd:NamedGraph
  (make-nsname sd: (string->local-name "NamedGraph")))

(define sd:GraphCollection
  (make-nsname sd: (string->local-name "GraphCollection")))

(define sd:defaultDataset
  (make-nsname sd: (string->local-name "defaultDataset")))

(define sd:defaultGraph
  (make-nsname sd: (string->local-name "defaultGraph")))

(define sd:graph
  (make-nsname sd: (string->local-name "graph")))

(define sd:name
  (make-nsname sd: (string->local-name "name")))

(define sd:namedGraph
  (make-nsname sd: (string->local-name "namedGraph")))
