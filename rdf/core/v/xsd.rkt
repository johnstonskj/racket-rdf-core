#lang racket/base

;;
;; Vocabulary: W3C XML Schema Definition Language (XSD) 1.1 Part 2
;;
;; Specification:    <https://www.w3.org/TR/xmlschema11-2/>
;; Last Date:        2012-04-05
;;
;; Support status: incomplete
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

(define xsd-prefix-string "xsd")
(define xsd-namespace-string "http://www.w3.org/2001/XMLSchema#")

(define xsd: (string->namespace xsd-namespace-string))

(define (nsmap-add-xsd map)
  (nsmap-set! map
              (string->prefix xsd-prefix-string)
              (string->namespace xsd-namespace-string)))

;; ------------------------------------------------------------------------------------------------
;; Public Types ❱ "ur" types
;; ------------------------------------------------------------------------------------------------

(define xsd:any-type
  (make-nsname xsd: (string->local-name "anyType")))

(define xsd:any-simple-type
  (make-nsname xsd: (string->local-name "anySimpleType")))

;; ------------------------------------------------------------------------------------------------
;; Public Types ❱ built-in primitive types
;; ------------------------------------------------------------------------------------------------

(define xsd:any-uri
  (make-nsname xsd: (string->local-name "anyURI")))

(define xsd:base64-binary
  (make-nsname xsd: (string->local-name "base64Binary")))

(define xsd:boolean
  (make-nsname xsd: (string->local-name "boolean")))

(define xsd:date
  (make-nsname xsd: (string->local-name "date")))

(define xsd:date-time
  (make-nsname xsd: (string->local-name "dateTime")))

(define xsd:date-time-stamp
  (make-nsname xsd: (string->local-name "dateTimeStamp")))

(define xsd:day-time-duration
  (make-nsname xsd: (string->local-name "dayTimeDuration")))

(define xsd:decimal
  (make-nsname xsd: (string->local-name "decimal")))

(define xsd:double
  (make-nsname xsd: (string->local-name "double")))

(define xsd:duration
  (make-nsname xsd: (string->local-name "duration")))

(define xsd:float
  (make-nsname xsd: (string->local-name "float")))

(define xsd:g-day
  (make-nsname xsd: (string->local-name "gDay")))

(define xsd:g-month
  (make-nsname xsd: (string->local-name "gMonth")))

(define xsd:g-month-day
  (make-nsname xsd: (string->local-name "gMonthDay")))

(define xsd:g-year
  (make-nsname xsd: (string->local-name "gYear")))

(define xsd:g-year-month
  (make-nsname xsd: (string->local-name "gYearMonth")))

(define xsd:hex-binary
  (make-nsname xsd: (string->local-name "hexBinary")))

(define xsd:year-month-duration
  (make-nsname xsd: (string->local-name "yearMonthDuration")))

(define xsd:q-name
  (make-nsname xsd: (string->local-name "QName")))

(define xsd:q-notation
  (make-nsname xsd: (string->local-name "QNotation")))

(define xsd:string
  (make-nsname xsd: (string->local-name "string")))

(define xsd:time
  (make-nsname xsd: (string->local-name "time")))

;; ------------------------------------------------------------------------------------------------
;; Public Types ❱ built-in derived types
;; ------------------------------------------------------------------------------------------------

(define xsd:normalized-string
  (make-nsname xsd: (string->local-name "normalizedString")))

(define xsd:token
  (make-nsname xsd: (string->local-name "token")))

(define xsd:language
  (make-nsname xsd: (string->local-name "language")))

(define xsd:name
  (make-nsname xsd: (string->local-name "Name")))

(define xsd:nmtoken
  (make-nsname xsd: (string->local-name "NMTOKEN")))

(define xsd:ncname
  (make-nsname xsd: (string->local-name "NCNAME")))

(define xsd:id
  (make-nsname xsd: (string->local-name "ID")))

(define xsd:id-ref
  (make-nsname xsd: (string->local-name "IDREF")))

(define xsd:entity
  (make-nsname xsd: (string->local-name "ENTITY")))

(define xsd:integer
  (make-nsname xsd: (string->local-name "integer")))

(define xsd:non-positive-integer
  (make-nsname xsd: (string->local-name "nonPositiveInteger")))

(define xsd:negative-integer
  (make-nsname xsd: (string->local-name "negativeInteger")))

(define xsd:long
  (make-nsname xsd: (string->local-name "long")))

(define xsd:int
  (make-nsname xsd: (string->local-name "int")))

(define xsd:short
  (make-nsname xsd: (string->local-name "short")))

(define xsd:byte
  (make-nsname xsd: (string->local-name "byte")))

(define xsd:non-negative-integer
  (make-nsname xsd: (string->local-name "nonNegativeInteger")))

(define xsd:unsigned-long
  (make-nsname xsd: (string->local-name "unsignedLong")))

(define xsd:unsigned-int
  (make-nsname xsd: (string->local-name "unsignedInt")))

(define xsd:unsigned-short
  (make-nsname xsd: (string->local-name "unsignedShort")))

(define xsd:unsigned-byte
  (make-nsname xsd: (string->local-name "unsignedByte")))

(define xsd:positive-integer
  (make-nsname xsd: (string->local-name "positiveInteger")))

;; ------------------------------------------------------------------------------------------------
;; Public Types ❱ constraining facets
;; ------------------------------------------------------------------------------------------------

(define xsd:enumeration
  (make-nsname xsd: (string->local-name "enumeration")))

(define xsd:fraction-digits
  (make-nsname xsd: (string->local-name "fractionDigits")))

(define xsd:length
  (make-nsname xsd: (string->local-name "length")))

(define xsd:max-exclusive
  (make-nsname xsd: (string->local-name "maxExclusive")))

(define xsd:max-inclusive
  (make-nsname xsd: (string->local-name "maxInclusive")))

(define xsd:max-length
  (make-nsname xsd: (string->local-name "maxLength")))

(define xsd:min-exclusive
  (make-nsname xsd: (string->local-name "minExclusive")))

(define xsd:min-inclusive
  (make-nsname xsd: (string->local-name "minInclusive")))

(define xsd:min-length
  (make-nsname xsd: (string->local-name "minLength")))
(define xsd:pattern (make-nsname xsd: (string->local-name "pattern")))

(define xsd:total-digits
  (make-nsname xsd: (string->local-name "totalDigits")))

(define xsd:white-space
  (make-nsname xsd: (string->local-name "whiteSpace")))
