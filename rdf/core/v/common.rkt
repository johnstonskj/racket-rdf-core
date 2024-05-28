#lang racket/base

(require "../nsname.rkt"
         "../nsmap.rkt"
         "../resource.rkt")

(provide all-defined-out)

;; ================================================================================================
;; Semantic Web
;; ================================================================================================

 ;; semantic web publishing
(define (nsmap-add-swp/v1 map)
  (nsmap-set! map
              (string->prefix "swp1")
              (string->resource "http://www.w3.org/2004/03/trix/swp-1")))

(define (nsmap-add-swp/v2 map)
  (nsmap-set! map
              (string->prefix "swp")
              (string->resource "http://www.w3.org/2004/03/trix/swp-2")))

;; Linked Data Platform 1.0
;; https://www.w3.org/TR/ldp/
;; https://www.w3.org/TR/ldp-paging/
(define (nsmap-add-ldp map)
  (nsmap-set! map
              (string->prefix "ldp")
              (string->resource "http://www.w3.org/ns/ldp#")))

;; ================================================================================================
;; SWAP -- see https://www.w3.org/2000/10/swap/doc/CwmBuiltins.html
;; ================================================================================================

;; Semantic Web Application Platform - SWAP
;; https://www.w3.org/2000/10/swap/
;; https://refubium.fu-berlin.de/bitstream/handle/fub188/10062/06_Chapter6-SWP-Vocabulary.pdf?sequence=7&isAllowed=y
;; http://wbsg.informatik.uni-mannheim.de/bizer/wiqa/swp/SWP-UserManual.pdf
(define (nsmap-add-swap/crypto map)
  (nsmap-set! map
              (string->prefix "crypto")
              (string->resource "http://www.w3.org/2000/10/swap/crypto#")))

(define (nsmap-add-swap/list map)
  (nsmap-set! map
              (string->prefix "list")
              (string->resource "http://www.w3.org/2000/10/swap/list#")))

(define (nsmap-add-swap/log map)
  (nsmap-set! map
              (string->prefix "log")
              (string->resource "http://www.w3.org/2000/10/swap/log#")))

(define (nsmap-add-swap/math map)
  (nsmap-set! map
              (string->prefix "math")
              (string->resource "http://www.w3.org/2000/10/swap/math#")))

(define (nsmap-add-swap/os map)
  (nsmap-set! map
              (string->prefix "os")
              (string->resource "http://www.w3.org/2000/10/swap/os#")))

(define (nsmap-add-swap/string map)
  (nsmap-set! map
              (string->prefix "string")
              (string->resource "http://www.w3.org/2000/10/swap/string#")))

(define (nsmap-add-swap/time map)
  (nsmap-set! map
              (string->prefix "time")
              (string->resource "http://www.w3.org/2000/10/swap/time#")))

;; ================================================================================================
;; Annotations
;; ================================================================================================

(define (nsmap-add-skos map)
  (nsmap-set! map
              (string->prefix "skos")
              (string->resource "http://www.w3.org/2004/02/skos/core#")))

;; see https://www.w3.org/TR/2013/NOTE-prov-dc-20130430/
(define (nsmap-add-provenance map)
  (nsmap-set! map
              (string->prefix "prov")
              (string->resource "http://www.w3.org/ns/prov#")))

(define (nsmap-add-creative-commons map)
  (nsmap-set! map
              (string->prefix "cc")
              (string->resource "http://creativecommons.org/ns")))

(define (nsmap-add-changeset map)
  (nsmap-set! map
              (string->prefix "cs")
              (string->resource "http://purl.org/vocab/changeset/schema#")))

;; ================================================================================================
;; Domains
;; ================================================================================================

(define (nsmap-add-biblio map)
  (nsmap-set! map
              (string->prefix "biblio")
              (string->resource "http://purl.org/net/biblio#>")))

(define (nsmap-add-cv map)
  (nsmap-set! map
              (string->prefix "cv")
              (string->resource "http://rdfs.org/resume-rdf/")))

(define (nsmap-add-dbpedia map)
  (nsmap-set! map
              (string->prefix "dbo")
              (string->resource "http://dbpedia.org/ontology/")))

(define (nsmap-add-doap map)
  (nsmap-set! map
              (string->prefix "doap")
              (string->resource "http://usefulinc.com/ns/doap#")))

(define (nsmap-add-foaf map)
  (nsmap-set! map
              (string->prefix "foaf")
              (string->resource "http://xmlns.com/foaf/0.1/")))

(define (nsmap-add-geo-names map)
  (nsmap-set! map
              (string->prefix "geonames")
              (string->resource "http://www.geonames.org/ontology#")))

(define (nsmap-add-ical map)
  (nsmap-set! map
              (string->prefix "ical")
              (string->resource "http://www.w3.org/2002/12/cal/ical#")))

(define (nsmap-add-org map)
  (nsmap-set! map
              (string->prefix "org")
              (string->resource "http://www.w3.org/ns/org#")))

(define (nsmap-add-relationship map)
  (nsmap-set! map
              (string->prefix "rel")
              (string->resource "http://purl.org/vocab/relationship/")))

(define (nsmap-add-rev map) ;; TODO: more readable name
  (nsmap-set! map
              (string->prefix "rev")
              (string->resource "http://purl.org/stuff/rev#")))

(define (nsmap-add-vcard map)
  (nsmap-set! map
              (string->prefix "vcard")
              (string->resource "http://www.w3.org/2006/vcard/ns#")))

(define (nsmap-add-auth/acl map)
  (nsmap-set! map
              (string->prefix "acl")
              (string->resource "http://www.w3.org/ns/auth/acl#")))

(define (nsmap-add-gleif/base map)
  (nsmap-set! map
              (string->prefix "gleif")
              (string->resource "https://www.gleif.org/ontology/Base/")))

;; http://rdfs.org/sioc/spec/:
;; also http://sioc-project.org/
;;
;; - http://rdfs.org/sioc/ns# - SIOC Core Ontology Namespace
;; - http://rdfs.org/sioc/access# - SIOC Access Ontology Module Namespace
;; - http://rdfs.org/sioc/actions# - SIOC Actions Ontology Module Namespace
;; - http://rdfs.org/sioc/argument# - SIOC Argumentation Ontology Module Namespace
;; - http://rdfs.org/sioc/nepomuk# - SIOC-Nepomuk Ontology Mappings
;; - http://rdfs.org/sioc/quotes# - SIOC Quotes Ontology Module Namespace
;; - http://rdfs.org/sioc/services# - SIOC Services Ontology Module Namespace
;; - http://rdfs.org/sioc/swan# - SIOC-SWAN Ontology Mappings
;; - http://rdfs.org/sioc/types# - SIOC Types Ontology Module Namespace
;; - http://rdfs.org/sioc/wikitalk# - SIOC WikiTalk Ontology Module Namespace

;; Shapes Constraint Language (SHACL)
;; https://www.w3.org/TR/shacl/
;;
;; sh => http://www.w3.org/ns/shacl#

;; Semantic Web Rule Language
;; https://www.w3.org/submissions/SWRL/
;;
;; http://www.w3.org/2003/11/swrlx
;; http://www.w3.org/2003/11/ruleml

;; Rule Interchange Format
;; https://www.w3.org/TR/rif-overview/
;;
;; http://www.w3.org/2007/rif#

;; Protocol for Web Description Resources (POWDER)
;; https://www.w3.org/TR/2009/NOTE-powder-primer-20090901/
