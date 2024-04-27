#lang racket/base

;;
;; Vocabulary: Extensible Markup Language (XML) 1.1 (Second Edition)
;;
;; Specification:    <https://www.w3.org/TR/xml11/>
;; Last Date:        2006-08-16
;;
;; Support status: complete
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

(define xml-prefix-string "xml")
(define xml-namespace-string "http://www.w3.org/XML/1998/namespace#")

(define xml: (string->namespace xml-namespace-string))

(define (nsmap-add-xml map)
  (nsmap-set! map
              (string->prefix xml-prefix-string)
              (string->namespace xml-namespace-string)))

;; ================================================================================================
;; Name definitions
;; ================================================================================================

;; See https://www.w3.org/TR/xml11/#sec-white-space
(define xml:space
  (make-nsname xml: (string->local-name "space")))

;; See https://www.w3.org/TR/xml11/#sec-lang-tag
(define xml:lang
  (make-nsname xml: (string->local-name "lang")))

;; This is an extension to XML 1.0 and therefore 1.1.
;; See: XML Base (Second Edition) (https://www.w3.org/TR/xmlbase/)
(define xml:base
  (make-nsname xml: (string->local-name "base")))
