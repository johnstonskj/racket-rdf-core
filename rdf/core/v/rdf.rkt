#lang racket/base

;;
;; Vocabulary: RDF 1.1
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

(define rdf-prefix-string "rdf")
(define rdf-namespace-string "http://www.w3.org/1999/02/22-rdf-syntax-ns#")

(define rdf: (string->resource rdf-namespace-string))

(define (nsmap-add-rdf map)
  (nsmap-set! map
              (string->prefix rdf-prefix-string)
              (string->resource rdf-namespace-string)))

;; ================================================================================================
;; Name definitions
;; ================================================================================================

(define rdf:langString
  (make-nsname rdf: (string->local-name "langString")))

(define rdf:HTML
  (make-nsname rdf: (string->local-name "HTML")))

(define rdf:XMLLiteral
  (make-nsname rdf: (string->local-name "XMLLiteral")))

(define rdf:Property
  (make-nsname rdf: (string->local-name "Property")))

(define rdf:type
  (make-nsname rdf: (string->local-name "type")))

(define rdf:Bag
  (make-nsname rdf: (string->local-name "Bag")))

(define rdf:Seq
  (make-nsname rdf: (string->local-name "Seq")))

(define rdf:Alt
  (make-nsname rdf: (string->local-name"Alt")))

(define rdf:List
  (make-nsname rdf: (string->local-name "List")))

(define rdf:first
  (make-nsname rdf: (string->local-name "first")))

(define rdf:rest
  (make-nsname rdf: (string->local-name "rest")))

(define rdf:nil
  (make-nsname rdf: (string->local-name "nil")))

(define rdf:Statement
  (make-nsname rdf: (string->local-name "Statement")))

(define rdf:subject
  (make-nsname rdf: (string->local-name "subject")))

(define rdf:predicate
  (make-nsname rdf: (string->local-name "predicate")))

(define rdf:object
  (make-nsname rdf: (string->local-name "object")))

(define rdf:value
  (make-nsname rdf: (string->local-name "value")))

;; ================================================================================================
;; RDF/XML Additions
;; ================================================================================================

(define rdf:RDF
  (make-nsname rdf: (string->local-name "RDF")))

(define rdf:Description
  (make-nsname rdf: (string->local-name "Description")))

(define rdf:about
  (make-nsname rdf: (string->local-name "about")))

(define rdf:aboutEach
  (make-nsname rdf: (string->local-name "aboutEach")))

(define rdf:aboutEachPrefix
  (make-nsname rdf: (string->local-name "aboutEachPrefix")))

(define rdf:bagID
  (make-nsname rdf: (string->local-name "bagID")))

(define rdf:datatype
  (make-nsname rdf: (string->local-name "datatype")))

(define rdf:ID
  (make-nsname rdf: (string->local-name "ID")))

(define rdf:li
  (make-nsname rdf: (string->local-name "li")))

(define (rdf:_ n)
  (make-nsname rdf: (string->local-name (format "_~a" n))))

(define rdf:nodeID
  (make-nsname rdf: (string->local-name "nodeID")))

(define rdf:parseType
  (make-nsname rdf: (string->local-name "parseType")))

(define rdf:resource
  (make-nsname rdf: (string->local-name "resource")))
