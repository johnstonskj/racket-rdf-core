#lang racket/base

;;
;; Vocabulary: Extensible Markup Language (XML) 1.1 (Second Edition)
;;
;; Namespace:        <http://www.w3.org/XML/1998/namespace#>
;; Specification:    <https://www.w3.org/TR/xml11/>
;; Last Date:        2006-08-16
;; Preferred prefix: `xml`
;;
;; Support status: complete
;;

(require "../namespace.rkt")

(provide (all-defined-out))

(define xml: (make-namespace "http://www.w3.org/XML/1998/namespace#" "xml"))

;; See https://www.w3.org/TR/xml11/#sec-white-space
(define xml:space (make-name xml: "space"))

;; See https://www.w3.org/TR/xml11/#sec-lang-tag
(define xml:lang (make-name xml: "lang"))

;; This is an extension to XML 1.0 and therefore 1.1.
;; See: XML Base (Second Edition) (https://www.w3.org/TR/xmlbase/)
(define xml:base (make-name xml: "base"))
