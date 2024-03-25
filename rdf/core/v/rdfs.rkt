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

(define *namespace* (make-namespace "http://www.w3.org/2000/01/rdf-schema#" "rdfs"))

(define Resource (make-name *namespace* "Resource"))
(define Class (make-name *namespace* "Class"))
(define Literal (make-name *namespace* "Literal"))
(define Datatype (make-name *namespace* "Datatype"))

(define range (make-name *namespace* "range"))
(define domain (make-name *namespace* "domain"))
(define sub-Class-Of (make-name *namespace* "subClassOf"))
(define sub-Property-Of (make-name *namespace* "subPropertyOf"))
(define label (make-name *namespace* "label"))
(define comment (make-name *namespace* "comment"))

(define Container (make-name *namespace* "Container"))
(define Container-Membership-Property (make-name *namespace* "ContainerMembershipProperty"))
(define member (make-name *namespace* "member"))

(define see-Also (make-name *namespace* "seeAlso"))
(define is-Defined-By (make-name *namespace* "isDefinedBy"))
