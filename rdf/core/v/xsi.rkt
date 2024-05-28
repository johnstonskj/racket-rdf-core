#lang racket/base

;;
;; Vocabulary: XML Schema Part 1: Structures Second Edition, XML Schema Instance
;;
;; Specification:    <https://www.w3.org/TR/xmlschema-1/>
;; Last Date:        2004-10-28
;;
;; Support status: complete
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

(define xsi-prefix-string "xsi")
(define xsi-namespace-string "http://www.w3.org/2001/XMLSchema-instance#")

(define xsi: (string->resource xsi-namespace-string))

(define (nsmap-add-xsi map)
  (nsmap-set! map
              (string->prefix xsi-prefix-string)
              (string->resource xsi-namespace-string)))

;; ================================================================================================
;; Name definitions
;; ================================================================================================

(define xsi:type
  (make-nsname xsi: (string->local-name "type")))

(define xsi:nil
  (make-nsname xsi: (string->local-name "nil")))

(define xsi:schemaLocation
  (make-nsname xsi: (string->local-name "schemaLocation")))

(define xsi:noNamespaceSchemaLocation
  (make-nsname xsi: (string->local-name "noNamespaceSchemaLocation")))
