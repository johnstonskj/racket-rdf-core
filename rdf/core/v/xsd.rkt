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

(define xsd: (make-namespace "http://www.w3.org/2001/XMLSchema#" "xsd"))

;; ------------------------------------------------------------------------------------------------
;; Public Types ❱ "ur" types
;; ------------------------------------------------------------------------------------------------

(define xsd:any-type (make-name xsd: "anyType"))
(define xsd:any-simple-type (make-name xsd: "anySimpleType"))

;; ------------------------------------------------------------------------------------------------
;; Public Types ❱ built-in primitive types
;; ------------------------------------------------------------------------------------------------

(define xsd:any-uri (make-name xsd: "anyURI"))
(define xsd:base64-binary (make-name xsd: "base64Binary"))
(define xsd:boolean (make-name xsd: "boolean"))
(define xsd:date (make-name xsd: "date"))
(define xsd:date-time (make-name xsd: "dateTime"))
(define xsd:date-time-stamp (make-name xsd: "dateTimeStamp"))
(define xsd:day-time-duration (make-name xsd: "dayTimeDuration"))
(define xsd:decimal (make-name xsd: "decimal"))
(define xsd:double (make-name xsd: "double"))
(define xsd:duration (make-name xsd: "duration"))
(define xsd:float (make-name xsd: "float"))
(define xsd:g-day (make-name xsd: "gDay"))
(define xsd:g-month (make-name xsd: "gMonth"))
(define xsd:g-month-day (make-name xsd: "gMonthDay"))
(define xsd:g-year (make-name xsd: "gYear"))
(define xsd:g-year-month (make-name xsd: "gYearMonth"))
(define xsd:hex-binary (make-name xsd: "hexBinary"))
(define xsd:year-month-duration (make-name xsd: "yearMonthDuration"))
(define xsd:q-name (make-name xsd: "QName"))
(define xsd:q-notation (make-name xsd: "QNotation"))
(define xsd:string (make-name xsd: "string"))
(define xsd:time (make-name xsd: "time"))

;; ------------------------------------------------------------------------------------------------
;; Public Types ❱ built-in derived types
;; ------------------------------------------------------------------------------------------------

(define xsd:normalized-string (make-name xsd: "normalizedString"))
(define xsd:token (make-name xsd: "token"))
(define xsd:language (make-name xsd: "language"))
(define xsd:name (make-name xsd: "Name"))
(define xsd:nmtoken (make-name xsd: "NMTOKEN"))
(define xsd:ncname (make-name xsd: "NCNAME"))
(define xsd:id (make-name xsd: "ID"))
(define xsd:id-ref (make-name xsd: "IDREF"))
(define xsd:entity (make-name xsd: "ENTITY"))

(define xsd:integer (make-name xsd: "integer"))
(define xsd:non-positive-integer (make-name xsd: "nonPositiveInteger"))
(define xsd:negative-integer (make-name xsd: "negativeInteger"))
(define xsd:long (make-name xsd: "long"))
(define xsd:int (make-name xsd: "int"))
(define xsd:short (make-name xsd: "short"))
(define xsd:byte (make-name xsd: "byte"))
(define xsd:non-negative-integer (make-name xsd: "nonNegativeInteger"))
(define xsd:unsigned-long (make-name xsd: "unsignedLong"))
(define xsd:unsigned-int (make-name xsd: "unsignedInt"))
(define xsd:unsigned-short (make-name xsd: "unsignedShort"))
(define xsd:unsigned-byte (make-name xsd: "unsignedByte"))
(define xsd:positive-integer (make-name xsd: "positiveInteger"))

;; ------------------------------------------------------------------------------------------------
;; Public Types ❱ constraining facets
;; ------------------------------------------------------------------------------------------------

(define xsd:enumeration (make-name xsd: "enumeration"))
(define xsd:fraction-digits (make-name xsd: "fractionDigits"))
(define xsd:length (make-name xsd: "length"))
(define xsd:max-exclusive (make-name xsd: "maxExclusive"))
(define xsd:max-inclusive (make-name xsd: "maxInclusive"))
(define xsd:max-length (make-name xsd: "maxLength"))
(define xsd:min-exclusive (make-name xsd: "minExclusive"))
(define xsd:min-inclusive (make-name xsd: "minInclusive"))
(define xsd:min-length (make-name xsd: "minLength"))
(define xsd:pattern (make-name xsd: "pattern"))
(define xsd:total-digits (make-name xsd: "totalDigits"))
(define xsd:white-space (make-name xsd: "whiteSpace"))
