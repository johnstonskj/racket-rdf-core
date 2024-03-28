#lang racket/base

;;
;; Vocabulary: W3C XML Schema Definition Language (XSD) 1.1 Part 2
;;
;; Namespace:        <http://www.w3.org/2001/XMLSchema#>
;; Specification:    <https://www.w3.org/TR/xmlschema11-2/>
;; Last Date:        2012-04-05
;; Preferred prefix: `xsd`
;;
;; Support status: incomplete
;;
;; See also:
;; - : Datatypes ()
;; - W3C XML Schema Definition Language (XSD) 1.1 Part 1: Structures (https://www.w3.org/TR/xmlschema11-1/)
;;

(require (only-in "../namespace.rkt" make-namespace make-name))

(provide (all-defined-out))

(define *namespace* (make-namespace "http://www.w3.org/2001/XMLSchema#" "xsd"))
