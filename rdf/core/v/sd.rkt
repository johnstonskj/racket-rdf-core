#lang racket/base

;;
;; Vocabulary: SPARQL 1.1 Service Description
;;
;; Namespace:        <http://www.w3.org/ns/sparql-service-description#>
;; Specification:    <https://www.w3.org/TR/sparql11-service-description/>
;; Last Date:        2013-03-21
;; Preferred prefix: `sd`
;;
;; Support status: incomplete
;;
;; See also: Describing Linked Datasets with the VoID Vocabulary (https://www.w3.org/TR/void/)
;;

(require (only-in "../namespace.rkt" make-namespace make-name))

(provide (all-defined-out))

(define sd: (make-namespace "http://www.w3.org/ns/sparql-service-description#" "sd"))

(define sd:Dataset (make-name sd: "Dataset"))
(define sd:Graph (make-name sd: "Graph"))
(define sd:NamedGraph (make-name sd: "NamedGraph"))
(define sd:GraphCollection (make-name sd: "GraphCollection"))

(define sd:defaultDataset (make-name sd: "defaultDataset"))
(define sd:defaultGraph (make-name sd: "defaultGraph"))
(define sd:graph (make-name sd: "graph"))
(define sd:name (make-name sd: "name"))
(define sd:namedGraph (make-name sd: "namedGraph"))
