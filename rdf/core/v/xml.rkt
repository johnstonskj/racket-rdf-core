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

(require (only-in "../namespace.rkt" make-namespace make-name))

(provide (all-defined-out))

(define *namespace* (make-namespace "xml" "http://www.w3.org/XML/1998/namespace#"))

;; See https://www.w3.org/TR/xml11/#sec-white-space
(define space (make-name *namespace* "space"))

;; See https://www.w3.org/TR/xml11/#sec-lang-tag
(define lang (make-name *namespace* "lang"))

;; This is an extension to XML 1.0 and therefore 1.1.
;; See: XML Base (Second Edition) (https://www.w3.org/TR/xmlbase/)
(define base (make-name *namespace* "base"))
