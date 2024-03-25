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

(define *namespace* (make-namespace "http://www.w3.org/ns/sparql-service-description#" "sd"))

(define Dataset (make-name *namespace* "Dataset"))
(define Graph (make-name *namespace* "Graph"))
(define NamedGraph (make-name *namespace* "NamedGraph"))
(define GraphCollection (make-name *namespace* "GraphCollection"))

(define defaultDataset (make-name *namespace* "defaultDataset"))
(define defaultGraph (make-name *namespace* "defaultGraph"))
(define graph (make-name *namespace* "graph"))
(define name (make-name *namespace* "name"))
(define namedGraph (make-name *namespace* "namedGraph"))
