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

(define *namespace* (make-namespace "http://www.w3.org/1999/02/22-rdf-syntax-ns#" "rdf"))

(define lang-String (make-name *namespace* "langString"))
(define HTML (make-name *namespace* "HTML"))
(define XML-Literal (make-name *namespace* "XMLLiteral"))

(define Property (make-name *namespace* "Property"))
(define type (make-name *namespace* "type"))

(define Bag (make-name *namespace* "Bag"))
(define Seq (make-name *namespace* "Sequence"))
(define Alt (make-name *namespace* "Alt"))

(define List (make-name *namespace* "List"))
(define first (make-name *namespace* "first"))
(define rest (make-name *namespace* "rest"))
(define nil (make-name *namespace* "nil"))

(define Statement (make-name *namespace* "Statement"))
(define subject (make-name *namespace* "subject"))
(define predicate (make-name *namespace* "predicate"))
(define object (make-name *namespace* "object"))

(define value (make-name *namespace* "value"))
