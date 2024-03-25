#lang info
(define name "rdf/core: RDF Core data-model")
(define deps '("base" "uuid"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/rdf-core.scrbl" ())))
(define pkg-desc "The RDF data model: resources, statements, graphs, and datasets.")
(define version "0.1.0")
(define pkg-authors '(johnstonskj))
(define license '(Apache-2.0))
