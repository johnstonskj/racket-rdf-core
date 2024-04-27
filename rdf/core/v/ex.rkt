#lang racket/base

;;
;; Vocabulary: Domain Name Reservation Considerations for Example Domains
;;
;; Specification: <https://datatracker.ietf.org/doc/html/rfc2606>, §3
;; Specification: <https://datatracker.ietf.org/doc/html/rfc6761>, §6.5
;;
;; Status: complete
;;

(require racket/contract
         racket/string
         (only-in rdf/core/namespace
                  string->namespace)
         (only-in rdf/core/nsmap
                  nsmap-set!
                  string->prefix))

(provide (all-defined-out))

;; ================================================================================================
;; Namespace definition
;; ================================================================================================

(define top-level-domain/c (or/c 'com 'edu 'net 'org))

(define (example-namespace tld #:vocabulary (vocab-path #f))
  (string->namespace
   (format "https://example.~a/~a"
           (symbol->string tld)
           (if vocab-path
               (if (or (string-suffix? vocab-path "/")
                       (string-suffix? vocab-path "#"))
                   vocab-path
                   (string-append vocab-path "/"))
               ""))))

(define (nsmap-add-example map tld #:vocabulary (vocab-path #f))
  (nsmap-set! map
              (string->prefix "ex")
              (example-namespace tld #:vocabulary vocab-path)))
