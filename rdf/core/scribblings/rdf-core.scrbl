#lang scribble/manual

@(require racket/sandbox
          scribble/eval
          rdf/core/namespace
          rdf/core/literal
          rdf/core/statement
          rdf/core/triple
          rdf/core/quad
          rdf/core/graph
          rdf/core/dataset
          rdf/core/gq
          rdf/core/io
          (for-label rdf/core/namespace
                     rdf/core/literal
                     rdf/core/statement
                     rdf/core/triple
                     rdf/core/quad
                     rdf/core/graph
                     rdf/core/dataset
                     rdf/core/gq
                     rdf/core/io
                     racket/contract))

@;{============================================================================}

@(define example-eval
   (make-base-eval '(require rdf/core/namespace
                             rdf/core/lang
                             rdf/core/literal
                             rdf/core/statement)))

@;{============================================================================}

@title[#:version  "0.1.0"]{RDF Core Data Model}
@author[(author+email "Simon Johnston" "johnstonskj@gmail.com")]

This is the core data model for RDF in Racket, it also includes core vocabularies such as `rdf`, `rdfs`, `xml`, and
`xsd` as well as some basic IO functions. The goal of this package is to provide the model allowing the user to create
statements, graphs and datasets in-memory. Additional Racket packages provide capabilities layered on top of this such
as support for OWL and SPARQL and additional vocabularies.

@table-of-contents[]

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module namespace}
@defmodule[rdf/core/namespace]

This package actually models XML namespaces where a namespace is an absolute URI (or IRI for RDF) and which may be
associated with a prefix name. Members in the namespace have names which are concatenated onto the namespace URI, or
may be referenced as qualified names (@italic{qnames}) in the form @tt{prefix:name}.

@defproc[#:kind "predicate"
         (ncname?
          [val any/c])
         boolean?]{
Predicate to check that the value @racket[val] is a string matching the XML grammar @tt{NCName} rule.

@examples[
  #:eval example-eval
  (ncname? "a-valid-name")
  (ncname? "?")
]
}

@defproc[#:kind "predicate"
         (qname?
          [val any/c])
         boolean?]{
Predicate to check that the value @racket[val] is a string matching the XML grammar @tt{QName} rule.

@examples[
  #:eval example-eval
  (qname? "xml:lang")
  (qname? "xml:")
  (qname? ":lang")
  (qname? "xml")
]
}

@defproc[(string->ncname
          [val string?])
         ncname?]{
Predicate to check that the value @racket[val] is a valid @racket[ncname?].
}

@defproc[(symbol->ncname
          [val symbol?])
         ncname?]{
Predicate to check that the value @racket[val] is a valid @racket[ncname?], and returns it's string form.
}

@;{----------------------------------------------------------------------------}

@defproc[#:kind "predicate"
         (url-absolute?
          [val any/c])
         boolean?]{
Predicate to check that the value @racket[val] is a @racket[url?] struct, and is absolute.
}

@;{----------------------------------------------------------------------------}

@defstruct*[namespace
            ([url url-absolute?]
            [prefix ncname?])]{
Models an XML namespace, and by extension an RDF namespace. It is primarily the URI for the namespace but also holds the
default prefix @tt{NCName}.
}

@defproc[#:kind "constructor"
         (make-namespace
          [url url-absolute?]
          [prefix ncname?])
         namespace?]{
Constructs a new @racket[namespace?] struct.
}

@defproc[#:kind "constructor"
         (namespace-make-url
          [ns namespace?]
          [name ncname?])
         url-absolute?]{
Constructs a new URI from the namespace URI and the name value.
}

@defproc[#:kind "constructor"
         (namespace-make-qname
          [ns namespace?]
          [name ncname?])
         qname?]{
Constructs a new XML @tt{QName} from the namespace prefix and the name value.
}

@defproc[#:kind "constructor"
         (namespace-make-default-qname
          [ns namespace?])
         string?]{
Constructs a new XML QName from the namespace prefix only.
}

@;{----------------------------------------------------------------------------}

@defstruct*[name
            ([namespace namespace?]
             [name ncname?])]{
Models a namespaced-name in XML.
}

@defproc[#:kind "constructor"
         (make-name
          [namespace namespace?]
          [name name?])
         name?]{
Constructs a new URL from the namespace URL and the name field's value.
}

@defproc[(name->url
          [name ncname?])
         url-absolute?]{
Constructs a new URL from the namespace URL and the name field's value.
}

@defproc[(name->qname
          [name ncname?])
         qname?]{
Constructs a new QName from the namespace prefix value and the name field's value.
}

@;{============================================================================}
@;{============================================================================}
@section[]{Module lang}
@defmodule[rdf/core/lang]

This module provides a single predicate that determines whether a given string is a valid @italic{Language Tag} as defined
by RFC5646 and used across HTTP, HTML, XML, RDF, and much more.

See BCP-47, RFC5646 @hyperlink["https://www.rfc-editor.org/info/rfc5646"]{Tags for Identifying Languages}. IANA Registries:
@hyperlink["https://www.iana.org/assignments/language-tags/language-tags.xhtml#language-tags-1"]{Language Tags (Assigned)},
@hyperlink["https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry"]{Language Subtags},
@hyperlink["https://www.iana.org/assignments/language-tag-extensions-registry/language-tag-extensions-registry"]{Language Tag
Extensions (UCD)}.

@defproc[#:kind "predicate"
         (language-tag?
          [val string?])
         boolean?]{
Determines if @racket[val] is a string conforming to the IETF BCP-47 rules for a language tag. These values are found
in XML in the @tt{xml:lang} attribute but also in the RDF @tt{rdf:langString} type.
}

@;{============================================================================}
@;{============================================================================}
@section[]{Module literal}
@defmodule[rdf/core/literal]

A literal in RDF is a three-part value, the @italic{Lexical Form}, @italic{Datatype IRI}, and @italic{Language Tag}. The lexical
form is a string representation of the value, and is the only required component (although the string may be empty).
The datatype IRI is used to identify the type of the value, and therefore how to evaluate the lexical form to produce a
final value. The language tag is used to identify the natural language used to write the string value in the lexical
form.

Note, there is only a single case where both datatype IRI and language tag are present at the same time, when the
language tag is present the value is @bold{by definition} typed as @tt{rdf:langString}.

@defstruct*[literal
            ([lexical-form string?]
             [datatype-iri url-absolute?]
             [language-tag language-tag?])]{
TBD
}

@defproc[#:kind "constructor"
         (make-untyped-literal
          [form string?])
         literal?]{
TBD
}

@defproc[#:kind "constructor"
         (make-typed-literal
          [form string?]
          [datatype-iri url-absolute?])
         literal?]{
TBD
}

@defproc[#:kind "constructor"
         (make-lang-string-literal
          [form string?]
          [language language-tag?])
         literal?]{
TBD
}

@;{----------------------------------------------------------------------------}

@defproc[#:kind "predicate"
         (has-datatype-iri?
          [val literal?])
         boolean?]{
TBD
}

@defproc[#:kind "predicate"
         (has-language-tag?
          [val literal?])
         boolean?]{
TBD
}

@defproc[#:kind "predicate"
         (has-xsd-datatype?
          [val literal?])
         boolean?]{
TBD
}

@defproc[#:kind "predicate"
         (is-a?
          [val literal?]
          [datatype url-absolute?])
         boolean?]{
TBD
}

@;{----------------------------------------------------------------------------}

@defproc[(boolean->literal
          [val boolean?])
         literal?]{
TBD
}

@defproc[(bytes->literal
          [val bytes?])
         literal?]{
TBD
}

@defproc[(date->literal
          [val date?])
         literal?]{
TBD
}

@defproc[(datetime->literal
          [val date?])
         literal?]{
TBD
}

@defproc[(exact-integer->literal
          [val exact-integer?])
         literal?]{
TBD
}

@defproc[(flonum->literal
          [val flonum?])
         literal?]{
TBD
}

@defproc[(inexact->literal
          [val inexact?])
         literal?]{
TBD
}

@defproc[(string->literal
          [val string?])
         literal?]{
TBD
}

@defproc[(time->literal
          [val date?])
         literal?]{
TBD
}

@defproc[(->literal
          [val (or/c boolean? bytes? date? string? exact-integer? flonum? inexact?)])
         literal?]{
TBD
}

@;{----------------------------------------------------------------------------}

@defthing[literal-true literal?]{The value @racket[#t] as a literal value.}
@defthing[literal-false literal?]{The value @racket[#f] as a literal value.}
@defthing[literal-empty-string literal?]{The value @racket[""] as a literal value.}
@defthing[literal-exact-zero literal?]{The value @racket[0] as a literal value.}
@defthing[literal-flonum-zero literal?]{The value @racket[0.0] as a literal value.}
@defthing[literal-inexact-zero literal?]{The value @racket[0.0] as a literal value.}

@;{============================================================================}
@;{============================================================================}
@section[]{Module statement}
@defmodule[rdf/core/statement]

Package Description Here

@defstruct*[blank-node
            ([label ncname?])]{
TBD
}

@defproc[#:kind "constructor"
         (make-blank-node)
         blank-node?]{
TBD
}

@;{----------------------------------------------------------------------------}

@defproc[#:kind "predicate"
         (resource?
          [val any/c])
         boolean?]{
TBD
}

@defproc[#:kind "predicate"
         (subject?
          [val any/c])
         boolean?]{
TBD
}

@defproc[#:kind "predicate"
         (predicate?
          [val any/c])
         boolean?]{
TBD
}

@defproc[#:kind "predicate"
         (object?
          [val any/c])
         boolean?]{
TBD
}

@;{----------------------------------------------------------------------------}

@defthing[gen:statement any/c]{
A @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{generic interface} (see
@secref[#:doc '(lib "scribblings/reference/reference.scrbl")]{struct-generics}) that defines a @deftech{statement}. To
supply method implementations, a struct should use the @racket[#:methods] form.
}

@defthing[statement/c contract?]{
TBD
}

@defproc[#:kind "predicate"
         (statement?
          [val any/c])
         boolean?]{
TBD
}

The generic interface @racket[gen:statement] has the following methods:

@nested[#:style 'inset]{

  @defproc[#:kind "method"
           (get-subject
            [stmt statement?])
           subject?]{
  Return the @italic{subject} component of the @racket[statement] structure.
  }

  @defproc[#:kind "method"
          (get-predicate
           [stmt statement?])
          predicate?]{
  Return the @italic{predicate} component of the @racket[statement] structure.
  }

  @defproc[#:kind "method"
           (get-object
            [stmt statement?])
           object?]{
  Return the @italic{object} component of the @racket[statement] structure.
  }

}

@;{----------------------------------------------------------------------------}

@defproc[(statement->list
          [stmt statement?])
         (list/c subject? predicate? object?)]{
TBD
}

@defproc[(statement->reified-list
          [stmt statement?])
         (listof (list/c subject? predicate? object?))]{
TBD
}

@;{----------------------------------------------------------------------------}

@defproc[#:kind "predicate"
         (statement-list?
          [val any/c])
         boolean?]{
TBD
}

@;{============================================================================}
@;{============================================================================}
@section[]{Module triple}
@defmodule[rdf/core/triple]

@defstruct*[triple
            ([subject subject?]
             [predicate predicate?]
             [object object?])]{
TBD

Implements the generic interface @racket[gen:statement].
}

@defproc[(make-triple
          [subject subject?]
          [predicate predicate?]
          [object object?])
         triple?]{
TBD
}

@defproc[(statement->reified-triples
          [stmt statement?])
         (listof triple?)]{
TBD
}

@defproc[(make-reified-triples
          [subject subject?]
          [predicate predicate?]
          [object object?])
         (listof triple?)]{
TBD
}

@;{----------------------------------------------------------------------------}

@defproc[(make-statement-list
          [subject subject?]
          [predicate-object-list (listof (list/c predicate? (or/c object? (listof object?))))])
         (listof triple?)]{
TBD
}

@defproc[(make-anon-statement-list
          [predicate-object-list (listof (list/c predicate? (or/c object? (listof object?))))])
         (listof triple?)]{
TBD
}

@;{----------------------------------------------------------------------------}

@defproc[(statement-add-to-subject
          [stmt statement?]
          [predicate predicate?]
          [object object?])
         triple?]{
TBD
}

@defproc[(statement-add-to-object
          [stmt statement?]
          [predicate predicate?]
          [object object?])
         triple?]{
TBD
}

@defproc[(make-type-statement
          [subject subject?]
          [type resource?])
         triple?]{
TBD
}

@defproc[(statement-add-type-to-subject
          [subject subject?]
          [type resource?])
         triple?]{
TBD
}

@;{============================================================================}
@;{============================================================================}
@section[]{Module graph}
@defmodule[rdf/core/graph]

Package Description Here

@defproc[#:kind "predicate"
         (graph-name?
          [val any/c])
         boolean?]{
A graph name is either a @racket[subject?] (which expands to @racket[resource?] or @racket[blank-node]) or @racket[#f]
for the default graph.
}

@defstruct*[graph
            ([name graph-name?]
             [(statements #:mutable) statement-list?])]{
TBD
}

@defproc[#:kind "constructor"
         (make-default-graph
          [statements statement-list?])
         graph?]{
TBD
}

@defproc[#:kind "constructor"
         (make-named-graph
          [name graph-name?]
          [statements statement-list?])
         graph?]{
TBD
}

@defproc[(graph-name-or-blank
          [graph graph?])
         subject?]{
TBD
}

@;{----------------------------------------------------------------------------}

@defproc[#:kind "predicate"
         (graph-named?
          [val graph?])
         boolean?]{
TBD
}

@defproc[#:kind "predicate"
         (graph-empty?
          [val graph?])
         boolean?]{
TBD
}

@defproc[#:kind "predicate"
         (graph-has-duplicates?
          [val graph?])
         boolean?]{
TBD
}

@;{----------------------------------------------------------------------------}

@defproc[(graph-count
          [val graph?])
         exact-positive-integer?]{
TBD
}

@defproc[(graph-order
          [val graph?])
         exact-positive-integer?]{
TBD
}

@defproc[(graph-distinct-subjects
          [val graph?])
         exact-positive-integer?]{
TBD
}

@defproc[(graph-distinct-predicates
          [val graph?])
         exact-positive-integer?]{
TBD
}

@defproc[(graph-distinct-objects
          [val graph?])
         exact-positive-integer?]{
TBD
}

@;{----------------------------------------------------------------------------}

@defproc[(graph-member?
          [val graph?]
          [statement statement?])
         boolean?]{
TBD
}

@defproc[(graph-add
          [val graph?]
          [statement statement?])
         graoh?]{
TBD
}

@defproc[(graph-add-all
          [val graph?]
          [statements statement-list?])
         graph?]{
TBD
}

@defproc[(graph-remove
          [val graph?]
          [statement statement?])
         graph?]{
TBD
}

@defproc[(graph-remove-all
          [val graph?]
          [statements statement-list?])
         graph?]{
TBD
}

@defproc[(graph-remove-all*
          [val graph?]
          [statements statement-list?])
         graph?]{
TBD
}

@defproc[(graph-clear
          [val graph?])
         graph?]{
TBD
}

@;{----------------------------------------------------------------------------}

@defproc[(graph-filter
          [proc (-> statement? boolean?)]
          [val graph?])
         statement-list?]{
TBD
}

@defproc[(graph-filter-by-subject
          [val graph?]
          [obj subject?])
         statement-list?]{
TBD
}

@defproc[(graph-filter-by-predicate
          [val graph?]
          [obj predicate?])
         statement-list?]{
TBD
}

@defproc[(graph-filter-by-object
          [val graph?]
          [obj object?])
         statement-list?]{
TBD
}

@;{----------------------------------------------------------------------------}

@defproc[(graph-skolemize
          [val graph?]
          [domain-name string? "example.com"])
         graph?]{
TBD
}

@defproc[(graph-skolemize!
          [val graph?]
          [domain-name string? "example.com"])
         graph?]{
TBD
}

@defproc[(skolem-url?
          [url url?])
         boolean?]{
TBD
}

@;{----------------------------------------------------------------------------}

@defproc[(describe-graph
          [graph graph?]
          [subject subject? #f])
         statement-list?]{
TBD
}

@;{----------------------------------------------------------------------------}

@; macro: rdf-graph

@; macro: rdf-sub-graph

@;{============================================================================}
@;{============================================================================}
@section[]{Module quad}
@defmodule[rdf/core/quad]

@defstruct*[quad
            ([subject subject?]
             [predicate predicate?]
             [object object?]
             [graph-name graph-name?])]{
TBD

Implements the generic interface @racket[gen:statement].
}

@defproc[(statement->quad
          [stmt statement?]
          [graph-name graph-name?])
         boolean?]{
TBD
}

@defproc[(graph->quads
          [graph graph?])
         (listof quad?)]{
TBD
}

@;{============================================================================}
@;{============================================================================}
@section[]{Module dataset}
@defmodule[rdf/core/dataset]

Package Description Here

@defstruct*[dataset
            ([name resource?]
             [(graphs #:mutable) (hash/c graph-name? graph?)])]{
TBD
}

@defproc[#:kind "constructor"
         (make-dataset
          [name resource?]
          [graphs (hash/c graph-name? graph?)])
         dataset?]{
TBD
}

@defproc[#:kind "predicate"
         (dataset-empty?
          [ds dataset?])
         boolean?]{
TBD
}

@defproc[(dataset-count
          [ds dataset?])
         exact-nonnegative-integer?]{
TBD
}

@defproc[(dataset-has-key?
          [ds dataset?]
          [name subject?])
         boolean?]{
TBD
}

@defproc[(dataset-has-default?
          [ds dataset?])
         boolean?]{
TBD
}

@defproc[(dataset-ref-default
          [ds dataset?])
         (or/c graph? #f)]{
TBD
}

@defproc[(dataset-ref
          [ds dataset?]
          [name graph-name?])
         (or/c graph? #f)]{
TBD
}

@defproc[(dataset-ref!
          [ds dataset?]
          [name graph-name?]
          [to-set graph?])
         (or/c graph? #f)]{
TBD
}

@defproc[(dataset-set!
          [ds dataset?]
          [graph graph?])
         void?]{
TBD
}

@defproc[(dataset-remove!
          [ds dataset?]
          [name graph-name?])
         void?]{
TBD
}

@defproc[(dataset-update!
          [ds dataset?]
          [name graph-name?]
          [updater (-> graph? graph?)])
         boolean?]{
TBD
}

@defproc[(dataset-clear!
          [ds dataset?])
         void?]{
TBD
}

@defproc[(dataset-map
          [ds dataset?]
          [proc (-> graph-name? graph? any/c)]
          [try-order? any/c #f])
         (listof any/c)]{
TBD
}

@defproc[(dataset-names
          [ds dataset?]
          [try-order? any/c #f])
         (listof graph-name?)]{
TBD
}

@defproc[(dataset-values
          [ds dataset?]
          [try-order? any/c #f])
         (listof graph?)]{
TBD
}

@defproc[(dataset->list
          [ds dataset?]
          [try-order? any/c #f])
         (listof (cons/c graph-name? graph?))]{
TBD
}

@defproc[(describe-dataset
          [ds dataset?])
         statement-list?]{
TBD
}

@;{============================================================================}
@;{============================================================================}
@section[]{Module gq}
@defmodule[rdf/core/gq]

Package Description Here

@defproc[#:kind "predicate"
         (pattern-component?
          [val any/c])
         boolean?]{
TBD
}

@defproc[(ignore)
         pattern-component?]{
TBD
}

@defproc[#:kind "predicate"
         (ignore?
          [val any/c])
         boolean?]{
TBD
}

@defproc[(comparitor
          [value object?]
          [operator procedure? equal?])
         pattern-component?]{
TBD
}

@defproc[#:kind "predicate"
         (comparitor?
          [val any/c])
         boolean?]{
TBD
}

@defproc[(variable
          [name ncname?])
         pattern-component?]{
TBD
}

@defproc[#:kind "predicate"
         (variable?
          [val any/c])
         boolean?]{
TBD
}

@;{----------------------------------------------------------------------------}

@defproc[#:kind "predicate"
         (statement-pattern?
          [val any/c])
         boolean?]{
TBD
}

@defproc[(statement-pattern-match
          [pattern statement-pattern?]
          [statement statement?])
         boolean?]{
TBD
}

@;{----------------------------------------------------------------------------}

@defproc[#:kind "predicate"
         (result-variable-value?
          [val any/c])
         boolean?]{
TBD
}

@defproc[#:kind "predicate"
         (result-statement?
          [val any/c])
         boolean?]{
TBD
}

@defproc[#:kind "predicate"
         (results?
          [val any/c])
         boolean?]{
TBD
}

@defproc[(graph-query
          [graph graph?]
          [patterns (listof statement-pattern?)])
         results?]{
TBD
}

@;{============================================================================}
@;{============================================================================}
@section[]{Module io}
@defmodule[rdf/core/io]

Package Description Here

@defthing[import-style/c flat-contract?]{TBD}

@defproc[(namespace->import-statement
          [graph graph?]
          [patterns (listof statement-pattern?)])
         results?]{
TBD
}

@;{----------------------------------------------------------------------------}

@deftogether[(
  @defproc[(write-ntriple-literal
            [val literal?]
            [out output-port? (current-output-port)])
           void?]
  @defproc[(literal->ntriple-string
            [val literal?]
            [out output-port? (current-output-port)])
           string?]
)]{
TBD
}

@;{----------------------------------------------------------------------------}

@deftogether[(
  @defproc[(write-ntriple-blank-node
            [val blank-node?]
            [out output-port? (current-output-port)])
           void?]
  @defproc[(blank-node->ntriple-string
            [val blank-node?]
            [out output-port? (current-output-port)])
           string?]
)]{
TBD
}

@deftogether[(
  @defproc[(write-ntriple-resource
            [val resource?]
            [out output-port? (current-output-port)])
           void?]
  @defproc[(resource->ntriple-string
            [val resource?]
            [out output-port? (current-output-port)])
           string?]
)]{
TBD
}

@deftogether[(
  @defproc[(write-ntriple-subject
            [val subject?]
            [out output-port? (current-output-port)])
           void?]
  @defproc[(subject->ntriple-string
            [val subject?]
            [out output-port? (current-output-port)])
           string?]
)]{
TBD
}

@deftogether[(
  @defproc[(write-ntriple-predicate
            [val predicate?]
            [out output-port? (current-output-port)])
           void?]
  @defproc[(predicate->ntriple-string
            [val predicate?]
            [out output-port? (current-output-port)])
           string?]
)]{
TBD
}

@deftogether[(
  @defproc[(write-ntriple-object
            [val object?]
            [out output-port? (current-output-port)])
           void?]
  @defproc[(object->ntriple-string
            [val object?]
            [out output-port? (current-output-port)])
           string?]
)]{
TBD
}

@;{----------------------------------------------------------------------------}

@deftogether[(
  @defproc[(write-ntriple-statement
            [val statement?]
            [out output-port? (current-output-port)])
           void?]
  @defproc[(statement->ntriple-string
            [val statement?]
            [out output-port? (current-output-port)])
           string?]
)]{
TBD
}

@deftogether[(
  @defproc[(write-nquad-statement
            [val object?]
            [graph-name graph-name?]
            [out output-port? (current-output-port)])
           void?]
  @defproc[(statement->nquad-string
            [val object?]
            [graph-name graph-name?]
            [out output-port? (current-output-port)])
           string?]
)]{
TBD
}

@;{----------------------------------------------------------------------------}

@deftogether[(
  @defproc[(write-ntriple-graph
            [val graph?]
            [out output-port? (current-output-port)])
           void?]
  @defproc[(graph->ntriple-string
            [val graph?]
            [out output-port? (current-output-port)])
           string?]
)]{
TBD
}

@deftogether[(
  @defproc[(write-nquad-graph
            [val graph?]
            [out output-port? (current-output-port)])
           void?]
  @defproc[(graph->nquad-string
            [val graph?]
            [out output-port? (current-output-port)])
           string?]
)]{
TBD
}

@;{----------------------------------------------------------------------------}

@deftogether[(
  @defproc[(write-ntriple-dataset
            [val dataset?]
            [out output-port? (current-output-port)])
           void?]
  @defproc[(dataset->ntriple-string
            [val dataset?]
            [out output-port? (current-output-port)])
           string?]
)]{
TBD
}

@;{----------------------------------------------------------------------------}

@deftogether[(
  @defproc[(write-statement-pattern
            [val dataset?]
            [out output-port? (current-output-port)])
           void?]
  @defproc[(statement-pattern->string
            [val dataset?]
            [out output-port? (current-output-port)])
           string?]
)]{
TBD
}

@;{============================================================================}
@;{============================================================================}
@section[]{Module prelude}
@defmodule[rdf/core/prelude]

The @tt{prelude} package combines imports and re-exports the most commonly used structures and procedures in the full
library.

From package @racket[rdf/core/namespace]:

@itemize[
  @item{@racket[ncname?]}
  @item{@racket[string->ncname]}
  @item{@racket[symbol->ncname]}
  @item{@racket[make-namespace]}
  @item{@racket[namespace?]}
  @item{@racket[make-name]}
  @item{@racket[name?]}
]

From package @racket[rdf/core/literal]:

@itemize[
  @item{@racket[literal?]}
  @item{@racket[literal-lexical-form]}
  @item{@racket[literal-datatype-iri]}
  @item{@racket[literal-language-tag]}
  @item{@racket[make-untyped-literal]}
  @item{@racket[make-typed-literal]}
  @item{@racket[make-lang-string-literal]}
  @item{@racket[write-ntriple-literal]}
]

From package @racket[rdf/core/statement]:

@itemize[
  @item{@racket[make-blank-node]}
  @item{@racket[blank-node?]}
  @item{@racket[subject?]}
  @item{@racket[predicate?]}
  @item{@racket[object?]}
  @item{@racket[statement?]}
  @item{@racket[get-subject]}
  @item{@racket[get-predicate]}
  @item{@racket[get-object]}
]

From package @racket[rdf/core/triple]:

@itemize[
  @item{@racket[make-triple]}
  @item{@racket[triple?]}
  @item{@racket[subject?]}
  @item{@racket[write-ntriple-statement]}
  @item{@racket[write-nquad-statement]}
]

From package @racket[rdf/core/graph]:

@itemize[
  @item{@racket[make-default-graph]}
  @item{@racket[make-named-graph]}
  @item{@racket[graph?]}
  @item{@racket[graph-named?]}
  @item{@racket[graph-empty?]}
  @item{@racket[graph-member?]}
  @item{@racket[graph-add]}
  @item{@racket[graph-add-all]}
  @item{@racket[graph-remove]}
  @item{@racket[graph-remove-all]}
  @item{@racket[write-ntriple-graph]}
  @item{@racket[write-nquad-graph]}
]

From package @racket[rdf/core/dataset]:

@itemize[
  @item{@racket[make-dataset]}
  @item{@racket[dataset?]}
  @item{@racket[dataset-empty?]}
  @item{@racket[dataset-has-key?]}
  @item{@racket[dataset-has-default?]}
  @item{@racket[dataset-ref]}
  @item{@racket[dataset-ref-default]}
  @item{@racket[dataset-set!]}
  @item{@racket[dataset-remove!]}
]

@;{============================================================================}
@;{============================================================================}
@section[]{Vocabulary Modules}

@;{============================================================================}
@subsection[]{RDF (v/rdf)}
@defmodule[rdf/core/v/rdf]

@defthing[rdf: namespace?]{The @racket[namespace] structure for this vocabulary.}

@nested[#:style 'inset]{
  This namespace defines the following class/type names:

  @defthing[Alt name?]{The @racket[name] structure corresponding to the class @tt{rdf:Alt}.}
  @defthing[Bag name?]{The @racket[name] structure corresponding to the class @tt{rdf:Bag}.}
  @defthing[HTML name?]{The @racket[name] structure corresponding to the class @tt{rdf:HTML}.}
  @defthing[List name?]{The @racket[name] structure corresponding to the class @tt{rdf:List}.}
  @defthing[Property name?]{The @racket[name] structure corresponding to the class @tt{rdf:Property}.}
  @defthing[Seq name?]{The @racket[name] structure corresponding to the class @tt{rdf:Seq}.}
  @defthing[Statement name?]{The @racket[name] structure corresponding to the class @tt{rdf:Statement}.}
  @defthing[XML-Literal name?]{The @racket[name] structure corresponding to the class @tt{rdf:XMLLiteral}.}

  This namespace defines the following property names:

  @defthing[first name?]{The @racket[name] structure corresponding to the property @tt{rdf:first}.}
  @defthing[lang-String name?]{The @racket[name] structure corresponding to the property @tt{rdf:langString}.}
  @defthing[nil name?]{The @racket[name] structure corresponding to the property @tt{rdf:nil}.}
  @defthing[object name?]{The @racket[name] structure corresponding to the property @tt{rdf:object}.}
  @defthing[predicate name?]{The @racket[name] structure corresponding to the property @tt{rdf:predicate}.}
  @defthing[rest name?]{The @racket[name] structure corresponding to the property @tt{rdf:rest}.}
  @defthing[subject name?]{The @racket[name] structure corresponding to the property @tt{rdf:subject}.}
  @defthing[type name?]{The @racket[name] structure corresponding to the property @tt{rdf:type}.}
  @defthing[value name?]{The @racket[name] structure corresponding to the property @tt{rdf:value}.}
}

@;{============================================================================}
@subsection[]{RDF Schema (v/rdfs)}
@defmodule[rdf/core/v/rdfs]

@defthing[rdfs: namespace?]{The @racket[namespace] structure for this vocabulary.}

@nested[#:style 'inset]{
  This namespace defines the following names:

  @defthing[rdfs:Class name?]{The @racket[name] structure corresponding to the class @tt{:Class}.}
  @defthing[rdfs:Container name?]{The @racket[name] structure corresponding to the class @tt{:Container}.}
  @defthing[rdfs:Container-Membership-Property name?]{The @racket[name] structure corresponding to the class @tt{:Container-Membership-Property}.}
  @defthing[rdfs:Datatype name?]{The @racket[name] structure corresponding to the class @tt{:Datatype}.}
  @defthing[rdfs:Literal name?]{The @racket[name] structure corresponding to the class @tt{:Literal}.}
  @defthing[rdfs:Resource name?]{The @racket[name] structure corresponding to the class @tt{:Resource}.}

  This namespace defines the following property names:

  @defthing[rdfs:comment name?]{The @racket[name] structure corresponding to the property @tt{:comment}.}
  @defthing[rdfs:domain name?]{The @racket[name] structure corresponding to the property @tt{:domain}.}
  @defthing[rdfs:is-defined-by name?]{The @racket[name] structure corresponding to the property @tt{:isDefinedBy}.}
  @defthing[rdfs:label name?]{The @racket[name] structure corresponding to the property @tt{:label}.}
  @defthing[rdfs:member name?]{The @racket[name] structure corresponding to the property @tt{:member}.}
  @defthing[rdfs:range name?]{The @racket[name] structure corresponding to the property @tt{:range}.}
  @defthing[rdfs:see-also name?]{The @racket[name] structure corresponding to the property @tt{:seeAlso}.}
  @defthing[rdfs:sub-class-of name?]{The @racket[name] structure corresponding to the property @tt{:sub-Class-Of}.}
  @defthing[rdfs:sub-property-of name?]{The @racket[name] structure corresponding to the property @tt{:sub-Property-Of}.}
}

@;{============================================================================}
@subsection[]{XML (v/xml)}
@defmodule[rdf/core/v/xml]

@defthing[xml: namespace?]{The @racket[namespace] structure for this vocabulary.}

@nested[#:style 'code-inset]{
  This namespace defines the following property names:

  @defthing[xml:base name?]{The @racket[name] structure corresponding to the property @tt{xml:base}.}
  @defthing[xml:lang name?]{The @racket[name] structure corresponding to the property @tt{xml:lang}.}
  @defthing[xml:space name?]{The @racket[name] structure corresponding to the property @tt{xml:space}.}
}

@;{============================================================================}
@subsection[]{XML Schema (v/xsd)}
@defmodule[rdf/core/v/xsd]

@defthing[*namespace* namespace?]{The namespace structure for the XML Schema (Part 2 -- Datatypes) vocabulary.}

@nested[#:style 'inset]{
  This namespace defines the following type/datatype names:

  @defthing[xsd:any-simple-type name?]{The @racket[name] structure corresponding to the type @tt{xsd:anySimpleType}.}
  @defthing[xsd:any-type name?]{The @racket[name] structure corresponding to the type @tt{xsd:anyType}.}
  @defthing[xsd:any-uri name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:anyURI}.}
  @defthing[xsd:base64-binary name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:base64Binary}.}
  @defthing[xsd:boolean name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:boolean}.}
  @defthing[xsd:byte name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:byte}.}
  @defthing[xsd:date name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:date}.}
  @defthing[xsd:date-time name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:dateTime}.}
  @defthing[xsd:decimal name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:decimal}.}
  @defthing[xsd:double name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:double}.}
  @defthing[xsd:duration name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:duration}.}
  @defthing[xsd:entity name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:ENTITY}.}
  @defthing[xsd:float name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:float}.}
  @defthing[xsd:g-day name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:gDay}.}
  @defthing[xsd:g-month name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:gMonth}.}
  @defthing[xsd:g-month-day name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:gMonthDay}.}
  @defthing[xsd:g-year name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:gYear}.}
  @defthing[xsd:g-year-month name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:gYearMonth}.}
  @defthing[xsd:hex-binary name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:hexBinary}.}
  @defthing[xsd:id name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:ID}.}
  @defthing[xsd:id-ref name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:IDREF}.}
  @defthing[xsd:int name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:int}.}
  @defthing[xsd:integer name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:integer}.}
  @defthing[xsd:language name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:language}.}
  @defthing[xsd:long name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:long}.}
  @defthing[xsd:name name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:Name}.}
  @defthing[xsd:ncname name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:NCNAME}.}
  @defthing[xsd:negative-integer name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:negativeInteger}.}
  @defthing[xsd:nmtoken name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:NMTOKEN}.}
  @defthing[xsd:non-negative-integer name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:nonNegativeInteger}.}
  @defthing[xsd:non-positive-integer name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:nonPositiveInteger}.}
  @defthing[xsd:normalized-string name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:normalizedString}.}
  @defthing[xsd:positive-integer name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:positiveInteger}.}
  @defthing[xsd:q-name name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:QName}.}
  @defthing[xsd:q-notation name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:QNotation}.}
  @defthing[xsd:short name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:short}.}
  @defthing[xsd:string name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:string}.}
  @defthing[xsd:time name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:time}.}
  @defthing[xsd:token name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:token}.}
  @defthing[xsd:unsigned-byte name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:unsignedByte}.}
  @defthing[xsd:unsigned-int name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:unsignedInt}.}
  @defthing[xsd:unsigned-long name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:unsignedLong}.}
  @defthing[xsd:unsigned-short name?]{The @racket[name] structure corresponding to the datatype @tt{xsd:unsignedShort}.}

  This namespace defines the following property names:

  @defthing[xsd:enumeration name?]{The @racket[name] structure corresponding to the property @tt{xsd:enumeration}.}
  @defthing[xsd:fraction-digits name?]{The @racket[name] structure corresponding to the property @tt{xsd:fractionDigits}.}
  @defthing[xsd:length name?]{The @racket[name] structure corresponding to the property @tt{xsd:length}.}
  @defthing[xsd:max-exclusive name?]{The @racket[name] structure corresponding to the property @tt{xsd:maxExclusive}.}
  @defthing[xsd:max-inclusive name?]{The @racket[name] structure corresponding to the property @tt{xsd:maxInclusive}.}
  @defthing[xsd:max-length name?]{The @racket[name] structure corresponding to the property @tt{xsd:maxLength}.}
  @defthing[xsd:min-exclusive name?]{The @racket[name] structure corresponding to the property @tt{xsd:minExclusive}.}
  @defthing[xsd:min-inclusive name?]{The @racket[name] structure corresponding to the property @tt{xsd:minInclusive}.}
  @defthing[xsd:min-length name?]{The @racket[name] structure corresponding to the property @tt{xsd:minLength}.}
  @defthing[xsd:pattern name?]{The @racket[name] structure corresponding to the property @tt{xsd:pattern}.}
  @defthing[xsd:total-digits name?]{The @racket[name] structure corresponding to the property @tt{xsd:totalDigits}.}
  @defthing[xsd:white-space name?]{The @racket[name] structure corresponding to the property @tt{xsd:whiteSpace}.}
}

@;{============================================================================}
@subsection[]{Vocabulary of Interlinked Datasets (v/void)}
@defmodule[rdf/core/v/void]

@defthing[void: namespace?]{The @racket[namespace] structure for this vocabulary.}

@nested[#:style 'inset]{
  This namespace defines the following class/type names:

  @defthing[void:Dataset name?]{The @racket[name] structure corresponding to the class @tt{void:Dataset}. A set of RDF triples that are published, maintained or aggregated by a single provider.}
  @defthing[void:Dataset-Description name?]{The @racket[name] structure corresponding to the class @tt{void:DatasetDescription}. A web resource whose @tt{foaf:primaryTopic} or @tt{foaf:topics} include @racket[void:Dataset]s.}
  @defthing[void:Linkset name?]{The @racket[name] structure corresponding to the class @tt{void:Linkset}. 	A collection of RDF links between two @racket[void:Dataset]s.}
  @defthing[void:Technical-Feature name?]{The @racket[name] structure corresponding to the class @tt{void:TechnicalFeature}. A technical feature of a @racket[void:Dataset], such as a supported RDF serialization format.}

  This namespace defines the following property names:

  @defthing[void:class name?]{The @racket[name] structure corresponding to the property @tt{void:class}. The @racket[rdfs:Class] that is the @racket[rdf:type] of all entities in a class-based partition.}
  @defthing[void:class-partition name?]{The @racket[name] structure corresponding to the property @tt{void:classPartition}. A subset of a @racket[void:Dataset] that contains only the entities of a certain @racket[rdfs:Class].}
  @defthing[void:classes name?]{The @racket[name] structure corresponding to the property @tt{void:classes}. The total number of distinct classes in a @racket[void:Dataset].}
  @defthing[void:data-dump name?]{The @racket[name] structure corresponding to the property @tt{void:dataDump}. An RDF dump, partial or complete, of a @racket[void:Dataset].}
  @defthing[void:distinct-objects name?]{The @racket[name] structure corresponding to the property @tt{void:distinctObjects}. The total number of distinct objects in a @racket[void:Dataset].}
  @defthing[void:distinct-subjects name?]{The @racket[name] structure corresponding to the property @tt{void:distinctSubjects}. The total number of distinct subjects in a @racket[void:Dataset].}
  @defthing[void:documents name?]{The @racket[name] structure corresponding to the property @tt{void:documents}. The total number of documents, for @racket[void:Dataset]s that are published as a set of individual RDF documents.}
  @defthing[void:entities name?]{The @racket[name] structure corresponding to the property @tt{void:entities}. The total number of entities that are described in a @racket[void:Dataset].}
  @defthing[void:example-resource name?]{The @racket[name] structure corresponding to the property @tt{void:exampleResource}. An example entity that is representative for the entities described in a @racket[void:Dataset].}
  @defthing[void:feature name?]{The @racket[name] structure corresponding to the property @tt{void:feature}. A @racket[void:TechnicalFeature] supported by a @racket[void:Datset].}
  @defthing[void:in-dataset name?]{The @racket[name] structure corresponding to the property @tt{void:inDataset}. Points to the @racket[void:Dataset] that a document is a part of.}
  @defthing[void:link-predicate name?]{The @racket[name] structure corresponding to the property @tt{void:linkPredicate}. Specifies the RDF property of the triples in a @racket[void:Linkset].}
  @defthing[void:objects-target name?]{The @racket[name] structure corresponding to the property @tt{void:objectsTarget}. The @racket[void:Dataset] that contains the resources in the object position of a @racket[void:Linkset]'s triples.}
  @defthing[void:opensearch-description name?]{The @racket[name] structure corresponding to the property @tt{void:openSearchDescription}. An OpenSearch description document for a free-text search service over a @racket[void:Dataset].}
  @defthing[void:properties name?]{The @racket[name] structure corresponding to the property @tt{void:properties}. The total number of distinct properties in a @racket[void:Dataset].}
  @defthing[void:property name?]{The @racket[name] structure corresponding to the property @tt{void:property}. The @racket[rdf:Property] that is the predicate of all triples in a property-based partition.}
  @defthing[void:property-partition name?]{The @racket[name] structure corresponding to the property @tt{void:propertyPartition}. A subset of a @racket[void:Dataset] that contains only the triples of a certain @racket[rdf:Property].}
  @defthing[void:root-resource name?]{The @racket[name] structure corresponding to the property @tt{void:rootResource}. A top concept or entry point for a @racket[void:Dataset] that is structured in a tree-like fashion.}
  @defthing[void:sparql-endpoint name?]{The @racket[name] structure corresponding to the property @tt{void:sparqlEndpoint}. A SPARQL protocol endpoint that allows SPARQL query access to a @racket[void:Dataset].}
  @defthing[void:subjects-target name?]{The @racket[name] structure corresponding to the property @tt{void:subjectsTarget}. The @racket[void:Dataset] that contains the resources in the subject position of this @racket[void:Linkset]'s triples.}
  @defthing[void:subset name?]{The @racket[name] structure corresponding to the property @tt{void:subset}. A @racket[void:Dataset] that is part of another @racket[void:Dataset].}
  @defthing[void:target name?]{The @racket[name] structure corresponding to the property @tt{void:target}. One of the two @racket[void:Datasets] connected by this @racket[void:Linkset].}
  @defthing[void:triples name?]{The @racket[name] structure corresponding to the property @tt{void:triples}. The total number of triples contained in a @racket[void:Dataset].}
  @defthing[void:uri-lookup-endpoint name?]{The @racket[name] structure corresponding to the property @tt{void:uriLookupEndpoint}. A protocol endpoint for simple URI lookups for a @racket[void:Dataset].}
  @defthing[void:uri-regex-pattern name?]{The @racket[name] structure corresponding to the property @tt{void:uriRegexPattern}. A regular expression that matches the URIs of a @racket[void:Dataset]'s entities.}
  @defthing[void:uri-space name?]{The @racket[name] structure corresponding to the property @tt{void:uriSpace}. A URI that is a common string prefix of all the entity URIs in a @racket[void:Datset].}
  @defthing[void:vocabulary name?]{The @racket[name] structure corresponding to the property @tt{void:vocabulary}. A vocabulary or @racket[owl:Ontology] whose classes or properties are used in a @racket[void:Dataset].}
}

@;{============================================================================}
@subsection[]{SPARQL Service Description (v/sd)}
@defmodule[rdf/core/v/sd]

@defthing[sd: namespace?]{The @racket[namespace] structure for this vocabulary.}

@nested[#:style 'inset]{
  This namespace defines the following class/type names:

  @defthing[sd:Dataset name?]{The @racket[name] structure corresponding to the class @tt{sd:Dataset}.}
  @defthing[sd:Graph name?]{The @racket[name] structure corresponding to the class @tt{sd:Graph}.}
  @defthing[sd:GraphCollection name?]{The @racket[name] structure corresponding to the class @tt{sd:GraphCollection}.}
  @defthing[sd:NamedGraph name?]{The @racket[name] structure corresponding to the class @tt{sd:NamedGraph}.}

  This namespace defines the following property names:

  @defthing[sd:defaultDataset name?]{The @racket[name] structure corresponding to the property @tt{sd:defaultDataset}.}
  @defthing[sd:defaultGraph name?]{The @racket[name] structure corresponding to the property @tt{sd:defaultGraph}.}
  @defthing[sd:graph name?]{The @racket[name] structure corresponding to the property @tt{sd:graph}.}
  @defthing[sd:name name?]{The @racket[name] structure corresponding to the property @tt{sd:name}.}
  @defthing[sd:namedGraph name?]{@racket[name] name structure corresponding to the property @tt{sd:namedGraph}.}
}

@;{============================================================================}
@;{============================================================================}
@index-section[]
