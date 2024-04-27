#lang racket/base

;;
;; Vocabulary: Namespaces in XML 1.0 (Third Edition)
;;
;; Specification:    <https://www.w3.org/TR/xml-names/>
;; Last Date:        2009-12-08
;;
;; Support status: complete
;;

(require (only-in "../namespace.rkt"
                  string->namespace)
         (only-in "../nsmap.rkt"
                  string->prefix
                  nsmap-set!))

(provide (all-defined-out))

;; ================================================================================================
;; Namespace definition
;; ================================================================================================

(define xmlns-prefix-string "xmlns")
(define xmlns-namespace-string "http://www.w3.org/2000/xmlns#")

(define xmlns: (string->namespace xmlns-namespace-string))

(define (nsmap-add-xmlns map)
  (nsmap-set! map
              (string->prefix xmlns-prefix-string)
              (string->namespace xmlns-namespace-string)))
