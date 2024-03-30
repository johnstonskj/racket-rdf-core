#lang racket/base

;;
;; Vocabulary: RDF 1.1
;;
;; Namespace:        <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
;; Specification:    <https://www.w3.org/TR/rdf11-schema/>
;; Last Date:        2014-02-25
;; Preferred prefix: `rdf`
;;
;; Support status: complete
;;
;; See also: RDF 1.1 Concepts and Abstract Syntax (https://www.w3.org/TR/rdf11-concepts/)
;;

(require (only-in "../namespace.rkt" make-namespace make-name))

(provide (all-defined-out))

(define rdf: (make-namespace "http://www.w3.org/1999/02/22-rdf-syntax-ns#" "rdf"))

(define rdf:lang-String (make-name rdf: "langString"))
(define rdf:HTML (make-name rdf: "HTML"))
(define rdf:XML-Literal (make-name rdf: "XMLLiteral"))

(define rdf:Property (make-name rdf: "Property"))
(define rdf:type (make-name rdf: "type"))

(define rdf:Bag (make-name rdf: "Bag"))
(define rdf:Seq (make-name rdf: "Sequence"))
(define rdf:Alt (make-name rdf: "Alt"))

(define rdf:List (make-name rdf: "List"))
(define rdf:first (make-name rdf: "first"))
(define rdf:rest (make-name rdf: "rest"))
(define rdf:nil (make-name rdf: "nil"))

(define rdf:Statement (make-name rdf: "Statement"))
(define rdf:subject (make-name rdf: "subject"))
(define rdf:predicate (make-name rdf: "predicate"))
(define rdf:object (make-name rdf: "object"))

(define rdf:value (make-name rdf: "value"))
