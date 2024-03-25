#lang racket/base

;;
;; Vocabulary: Describing Linked Datasets with the VoID Vocabulary
;;
;; Namespace:        <http://rdfs.org/ns/void#>
;; Specification:    <http://vocab.deri.ie/void>
;; Last Date:        2011-03-06
;; Preferred prefix: `void`
;;
;; Support status: incomplete
;;
;; See also: Describing Linked Datasets with the VoID Vocabulary (https://www.w3.org/TR/void/)
;;

(require (only-in "../namespace.rkt" make-namespace make-name))

(provide (all-defined-out))

(define *namespace* (make-namespace "http://rdfs.org/ns/void#" "void"))

(define classes (make-name *namespace* "classes"))
(define distinctSubjects (make-name *namespace* "distinctSubjects"))
(define distinctObjects (make-name *namespace* "distinctObjects"))
(define documents (make-name *namespace* "documents"))
(define entities (make-name *namespace* "entities"))
(define properties (make-name *namespace* "properties"))
(define triples (make-name *namespace* "triples"))
