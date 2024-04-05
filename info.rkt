#lang info
(define collection 'multi)
(define deps '("base" "langtag" "rx" "uuid"))
(define build-deps '("scribble-lib" "sandbox-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("rdf/core/scribblings/rdf-core.scrbl" (multi-page) (library))))
(define test-omit-paths '("rdf/core/scribblings"))
(define license 'Apache-2.0)
