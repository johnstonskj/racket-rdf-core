Racket Package rdf-core
========
This is the core data model for RDF in Racket, it also includes core vocabularies (such as `rdf`, `rdfs`, `xml`, and `xsd`) as
well as some basic IO functions. The goal of this package is to provide the model allowing the user to create
statements, graphs and datasets in-memory. Additional Racket packages provide capabilities layered on top of this such
as support for OWL and SPARQL and additional vocabularies.

[![raco pkg install rdf-core](https://img.shields.io/badge/raco%20pkg%20install-rdf--core-blue.svg)](http://pkgs.racket-lang.org/package/rdf-core)
[![Documentation](https://img.shields.io/badge/raco%20docs-rdf--core-blue.svg)](http://docs.racket-lang.org/rdf-core/index.html)
[![Racket](https://github.com/johnstonskj/racket-rdf-core/actions/workflows/racket.yml/badge.svg)](https://github.com/johnstonskj/racket-rdf-core/actions/workflows/racket.yml)
[![GitHub release](https://img.shields.io/github/release/johnstonskj/racket-rdf-core.svg?style=flat-square)](https://github.com/johnstonskj/racket-rdf-core/releases)
[![GitHub stars](https://img.shields.io/github/stars/johnstonskj/racket-rdf-core.svg)](https://github.com/johnstonskj/racket-rdf-core/stargazers)

## Example

The following shows the construction of a *default* (unnamed) graph with a simple set of statements. This is followed by a
simple query pattern to return the first names of each subject.

``` racket
(define my-graph
        (make-default-graph
          (let ((ns (make-namespace "http://example.com/peeps" "xp"))
            (append
              (make-statement-list
                (namespace-make-url ns "spongebob")
                '(("http://example.com/v/people#hasFirstName" ("Spongebob"))
                  ("http://example.com/v/people#hasLastName" ("Squarepants"))
                  ("http://example.com/v/people#hasAge" (19))
                  ("http://example.com/v/people#hasScores" (2 4 6))))
              (make-statement-list
                (namespace-make-url ns "patrick")
                '(("http://example.com/v/people#hasFirstName" ("Patrick"))
                  ("http://example.com/v/people#hasLastName" ("Star"))
                  ("http://example.com/v/people#hasAge" (19))
                  ("http://example.com/v/people#hasScores" (1 1 0)))))))))

(define get-first-names
        (query-pattern
          (make-statement-pattern
            (ignore)
            (match-value (namespace-make-url ns "hasFirstName"))
            (return-variable "name"))))

(println (graph-query my-graph get-first-names))
;; => '((("name" . "Spongebob")) (("name" . "Patrick")))
```

## Changes

**Version 0.1.0**

- Initial version with very little documentation..

[![Racket Language](https://racket-lang.org/logo-and-text-1-2.png)](https://racket-lang.org/)
4;292;64M
