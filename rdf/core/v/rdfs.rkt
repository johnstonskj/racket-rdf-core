#lang racket/base

;;
;; Vocabulary: RDF Schema 1.1
;;
;; Namespace:        <http://www.w3.org/2000/01/rdf-schema#>
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

(define rdfs: (make-namespace "http://www.w3.org/2000/01/rdf-schema#" "rdfs"))

(define rdfs:Resource (make-name rdfs: "Resource"))
(define rdfs:Class (make-name rdfs: "Class"))
(define rdfs:Literal (make-name rdfs: "Literal"))
(define rdfs:Datatype (make-name rdfs: "Datatype"))

(define rdfs:range (make-name rdfs: "range"))
(define rdfs:domain (make-name rdfs: "domain"))
(define rdfs:sub-class-of (make-name rdfs: "subClassOf"))
(define rdfs:sub-property-of (make-name rdfs: "subPropertyOf"))
(define rdfs:label (make-name rdfs: "label"))
(define rdfs:comment (make-name rdfs: "comment"))

(define rdfs:Container (make-name rdfs: "Container"))
(define rdfs:Container-Membership-Property (make-name rdfs: "ContainerMembershipProperty"))
(define rdfs:member (make-name rdfs: "member"))

(define rdfs:see-also (make-name rdfs: "seeAlso"))
(define rdfs:is-defined-by (make-name rdfs: "isDefinedBy"))
