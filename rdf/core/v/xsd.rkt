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

(require net/url-string
         "../namespace.rkt")

(provide (all-defined-out))

(define *namespace* (make-namespace "http://www.w3.org/2001/XMLSchema#" "xsd"))

;; ------------------------------------------------------------------------------------------------
;; Public Types ❱ "ur" types
;; ------------------------------------------------------------------------------------------------

(define any-type (make-name *namespace* "anyType"))
(define any-simple-type (make-name *namespace* "anySimpleType"))

;; ------------------------------------------------------------------------------------------------
;; Public Types ❱ built-in primitive types
;; ------------------------------------------------------------------------------------------------

(define any-uri (make-name *namespace* "anyURI"))
(define base64-binary (make-name *namespace* "base64Binary"))
(define boolean (make-name *namespace* "boolean"))
(define date (make-name *namespace* "date"))
(define date-time (make-name *namespace* "dateTime"))
(define decimal (make-name *namespace* "decimal"))
(define double (make-name *namespace* "double"))
(define duration (make-name *namespace* "duration"))
(define float (make-name *namespace* "float"))
(define g-day (make-name *namespace* "gDay"))
(define g-month (make-name *namespace* "gMonth"))
(define g-month-day (make-name *namespace* "gMonthDay"))
(define g-year (make-name *namespace* "gYear"))
(define g-year-month (make-name *namespace* "gYearMonth"))
(define hex-binary (make-name *namespace* "hexBinary"))
(define q-name (make-name *namespace* "QName"))
(define q-notation (make-name *namespace* "QNotation"))
(define string (make-name *namespace* "string"))
(define time (make-name *namespace* "time"))

;; ------------------------------------------------------------------------------------------------
;; Public Types ❱ built-in derived types
;; ------------------------------------------------------------------------------------------------

(define normalized-string (make-name *namespace* "normalizedString"))
(define token (make-name *namespace* "token"))
(define language (make-name *namespace* "language"))
(define name (make-name *namespace* "Name"))
(define nmtoken (make-name *namespace* "NMTOKEN"))
(define ncname (make-name *namespace* "NCNAME"))
(define id (make-name *namespace* "ID"))
(define id-ref (make-name *namespace* "IDREF"))
(define entity (make-name *namespace* "ENTITY"))

(define integer (make-name *namespace* "integer"))
(define non-positive-integer (make-name *namespace* "nonPositiveInteger"))
(define negative-integer (make-name *namespace* "negativeInteger"))
(define long (make-name *namespace* "long"))
(define int (make-name *namespace* "int"))
(define short (make-name *namespace* "short"))
(define byte (make-name *namespace* "byte"))
(define non-negative-integer (make-name *namespace* "nonNegativeInteger"))
(define unsigned-long (make-name *namespace* "unsignedLong"))
(define unsigned-int (make-name *namespace* "unsignedInt"))
(define unsigned-short (make-name *namespace* "unsignedShort"))
(define unsigned-byte (make-name *namespace* "unsignedByte"))
(define positive-integer (make-name *namespace* "positiveInteger"))

;; ------------------------------------------------------------------------------------------------
;; Public Types ❱ constraining facets
;; ------------------------------------------------------------------------------------------------

(define enumeration (make-name *namespace* "enumeration"))
(define fraction-digits (make-name *namespace* "fractionDigits"))
(define length (make-name *namespace* "length"))
(define max-exclusive (make-name *namespace* "maxExclusive"))
(define max-inclusive (make-name *namespace* "maxInclusive"))
(define max-length (make-name *namespace* "maxLength"))
(define min-exclusive (make-name *namespace* "minExclusive"))
(define min-inclusive (make-name *namespace* "minInclusive"))
(define min-length (make-name *namespace* "minLength"))
(define pattern (make-name *namespace* "pattern"))
(define total-digits (make-name *namespace* "totalDigits"))
(define white-space (make-name *namespace* "whiteSpace"))
