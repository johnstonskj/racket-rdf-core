#lang racket/base

(require (only-in net/url-string string->url url->string))

(provide *common-prefix->common-url*
         *common-url->common-prefix*
         *common-url-string->common-prefix*)

(define *common-ns-assoc*
  (list ;list ; TriX Namespaces (see )
        (list "rdfg" (string->url "http://www.w3.org/2004/03/trix/rdfg-1"))
        (list "swp" (string->url "http://www.w3.org/2004/03/trix/swp-1"))
        ;list ; cwm Namespaces (see https://www.w3.org/2000/10/swap/doc/CwmBuiltins.html)
        (list "crypto" (string->url "http://www.w3.org/2000/10/swap/crypto#"))
        (list "log" (string->url "http://www.w3.org/2000/10/swap/log#"))
        (list "list" (string->url "http://www.w3.org/2000/10/swap/list#"))
        (list "math" (string->url "http://www.w3.org/2000/10/swap/math#"))
        (list "os" (string->url "http://www.w3.org/2000/10/swap/os#"))
        (list "string" (string->url "ttp://www.w3.org/2000/10/swap/string#"))
        (list "time" (string->url "http://www.w3.org/2000/10/swap/time#"))
        ;; Annotation/Metadata Vocabularies

        (list "skos" (string->url "http://www.w3.org/2004/02/skos/core#"))
        ;; Domain Vocabularies
        (list "biblio" (string->url "http://purl.org/net/biblio#>"))
        (list "cv" (string->url "http://rdfs.org/resume-rdf/"))
        (list "dbo" (string->url "http://dbpedia.org/ontology/"))
        (list "doap" (string->url "http://usefulinc.com/ns/doap#"))
        (list "foaf" (string->url "http://xmlns.com/foaf/0.1/"))
        (list "geonames" (string->url "http://www.geonames.org/ontology#"))
        (list "ical" (string->url "http://www.w3.org/2002/12/cal/ical#"))
        (list "org" (string->url "http://www.w3.org/ns/org#"))
        (list "rel" (string->url "http://purl.org/vocab/relationship/")) ;; between people
        (list "rev" (string->url "http://purl.org/stuff/rev#")) ;; reviews
        (list "vcard" (string->url "http://www.w3.org/2006/vcard/ns#"))
        ;; Technical Vocabularies
        (list "acl" (string->url "http://www.w3.org/ns/auth/acl#"))
        (list "cc" (string->url "http://creativecommons.org/ns"))
        (list "cs" (string->url "http://purl.org/vocab/changeset/schema#"))
        ;; maybe? (list "http" (string->url "http://www.w3.org/2007/ont/http#"))
        ;; maybe? (list "https" (string->url "http://www.w3.org/2007/ont/httph#"))
        ;; maybe? (list "iana" (string->url "http://www.iana.org/assignments/relation/"))
        ;; maybe? (list "ldp" (string->url "http://www.w3.org/ns/ldp#"))
        ;; maybe? (list "acrt" (string->url "http://privatealpha.com/ontology/certification/1#"))
        (list "prov" (string->url "http://www.w3.org/ns/prov#")) ; see also https://www.w3.org/TR/2013/NOTE-prov-dc-20130430/
        ;; GLEIF Vocabularies (see https://www.gleif.org/ontology/v1.0/Base/index-en.html)
        ;; (list "gleif-base" (string->url "https://www.gleif.org/ontology/Base/"))
       
        ))

(define *common-prefix->common-url*
  (make-immutable-hash *common-ns-assoc*))

(define *common-url->common-prefix*
  (make-immutable-hash (map (λ (pair) (list (cadr pair) (car pair))) *common-ns-assoc*)))

(define *common-url-string->common-prefix*
  (make-immutable-hash (map (λ (pair) (list (url->string (cadr pair)) (car pair))) *common-ns-assoc*)))
