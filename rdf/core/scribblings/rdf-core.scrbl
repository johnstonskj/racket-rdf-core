#lang scribble/manual

@(require racket/sandbox
          scribble/eval
          rdf/core/namespace
          rdf/core/literal
          rdf/core/statement
          rdf/core/graph
          rdf/core/dataset
          rdf/core/gq
          rdf/core/io
          (for-label rdf/core/namespace
                     rdf/core/literal
                     rdf/core/statement
                     rdf/core/graph
                     rdf/core/dataset
                     rdf/core/gq
                     rdf/core/io
                     racket/contract))

@;{============================================================================}

@;(define example-eval (make-base-eval '(require rdf/core)))

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
@section[]{Module rdf/core/namespace}
@defmodule[rdf/core/namespace]

Package Description Here

@defproc[#:kind "predicate"
         (ncname?
          [val any/c])
         boolean?]{
Predicate to check that the value @racket[val] is a string matching the XML grammar NCName rule.
}

@defproc[#:kind "predicate"
         (qname?
          [val any/c])
         boolean?]{
Predicate to check that the value @racket[val] is a string matching the XML grammar QName rule.
}

@defproc[(string->ncname
          [val string?])
         ncname?]{
Predicate to check that the value @racket[val] is a valid @racket[ncname?].
}

@defproc[(symbol->ncname
          [val symbol?])
         ncname?]{
Predicate to check that the value @racket[val] is a valid @racket[url] and returns it's string form.
}

@;{----------------------------------------------------------------------------}

@defproc[#:kind "predicate"
         (url-absolute?
          [val any/c])
         boolean?]{
Predicate to check that the value @racket[val] is a @racket[url] struct, and is absolute.
}

@;{----------------------------------------------------------------------------}

@defstruct*[namespace
            ([url url-absolute?]
             [prefix ncname?])]{
Models an XML namespace, and by extension an RDF namespace. It is primarily the URL for the namespace but also holds the default prefix NCName.
}

@defproc[#:kind "constructor"
         (make-namespace
          [url url-absolute?]
          [prefix ncname?])
         namespace?]{
Constructs a new namespace struct.
}

@defproc[#:kind "constructor"
         (namespace-make-url
          [ns namespace?]
          [name ncname?])
         url-absolute?]{
Constructs a new URL from the namespace URL and the name value.
}

@defproc[#:kind "constructor"
         (namespace-make-qname
          [ns namespace?]
          [name ncname?])
         qname?]{
Constructs a new XML QName from the namespace prefix and the name value.
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
Constructs a new URL from the namespace URL and the name value.
}

@defproc[(name->url
          [name name?])
         url-absolute?]{
Constructs a new URL from the namespace URL and the name value.
}

@defproc[(name->url
          [name qname?])
         qname?]{
Constructs a new QName from the namespace prefix and the name value.
}

@;{============================================================================}
@;{============================================================================}
@section[]{Module rdf/core/literal}
@defmodule[rdf/core/literal]

Package Description Here

@defstruct*[language-string
            ([text string?]
             [language string?])]{
TBD
}

@defstruct*[typed-string
            ([text string?]
             [datatype url-absolute?])]{
TBD
}

@defproc[#:kind "predicate"
         (literal?
          [val any/c])
         boolean?]{
Determines if @racket[val] is a literal? value, that is a @racket[boolean?],
@racket[number?], @racket[string?], @racket[language-string?], or
@racket[typed-string?].
}

@;{============================================================================}
@;{============================================================================}
@section[]{Module rdf/core/statement}
@defmodule[rdf/core/statement]

Package Description Here


@defstruct*[blank-node
            ()]{
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

@defstruct*[statements
            ([subject subject?]
             [predicate predicate?]
             [object object?])]{
TBD
}

@defproc[(make-reified-statement
          [subject subject?]
             [predicate predicate?]
             [object object?])
         stateent-list?]{
TBD
}

@;{----------------------------------------------------------------------------}

@;{============================================================================}
@;{============================================================================}
@section[]{Module rdf/core/graph}
@defmodule[rdf/core/graph]

Package Description Here

@;{============================================================================}
@;{============================================================================}
@section[]{Module rdf/core/dataset}
@defmodule[rdf/core/dataset]

Package Description Here

@;{============================================================================}
@;{============================================================================}
@section[]{Module rdf/core/gq}
@defmodule[rdf/core/gq]

Package Description Here

@;{============================================================================}
@;{============================================================================}
@section[]{Module rdf/core/io}
@defmodule[rdf/core/io]

Package Description Here

@;{============================================================================}
@;{============================================================================}
@section[]{Module rdf/core/prelude}
@defmodule[rdf/core/prelude]

Package Description Here

@;{============================================================================}
@;{============================================================================}
@section[]{Vocabulary Modules}
@defmodule[rdf/core/v]

@defthing[*namespace* namespace?]{The @racket[namespace] structure for this vocabulary.}

@;subsubsection[]{Names}

@;{============================================================================}
@subsection[]{RDF Vocabulary .../v/rdf}
@defmodule[rdf/core/v/rdf]

@defthing[*namespace* namespace?]{The @racket[namespace] structure for this vocabulary.}

@;subsubsection[]{Names}

@;{============================================================================}
@subsection[]{RDF Schema Vocabulary .../v/rdfs}
@defmodule[rdf/core/v/rdfs]

@defthing[*namespace* namespace?]{The @racket[namespace] structure for this vocabulary.}

@;subsubsection[]{Names}

@;{============================================================================}
@subsection[]{XML Vocabulary .../v/xml}
@defmodule[rdf/core/v/xml]

@defthing[*namespace* namespace?]{The @racket[namespace] structure for this vocabulary.}

@;subsubsection[]{Names}

@defthing[base name?]{The @racket[name] structure corresponding to xml:base.}
@defthing[lang name?]{The @racket[name] structure corresponding to xml:lang.}
@defthing[space name?]{The @racket[name] structure corresponding to xml:space.}

@;{============================================================================}
@subsection[]{XML Schema Vocabulary .../v/xsd}
@defmodule[rdf/core/v/xsd]

@defthing[*namespace* namespace?]{The namespace structure for this vocabulary.}

@;subsubsection[]{Names}

@;{============================================================================}
@subsection[]{VOID Vocabulary .../v/void}
@defmodule[rdf/core/v/void]

@defthing[*namespace* namespace?]{The @racket[namespace] structure for this vocabulary.}

@;subsubsection[]{Names}

@defthing[classes name?]{The @racket[name] structure corresponding to void:classes.}
@defthing[distinctSubjects name?]{The @racket[name] structure corresponding to void:distinctSubjects.}
@defthing[distinctObjects name?]{The @racket[name] structure corresponding to void:distinctObjects.}
@defthing[documents name?]{The @racket[name] structure corresponding to void:documents.}
@defthing[entities name?]{The @racket[name] structure corresponding to void:entities.}
@defthing[properties name?]{The @racket[name] structure corresponding to void:properties.}
@defthing[triples name?]{The @racket[name] structure corresponding to void:triples.}

@;{============================================================================}
@subsection[]{SPARQL Service Description Vocabulary .../v/sd}
@defmodule[rdf/core/v/sd]

@defthing[*namespace* namespace?]{The @racket[namespace] structure for this vocabulary.}

@;subsubsection[]{Names}

@defthing[Dataset name?]{The @racket[name] structure corresponding to sd:Dataset.}
@defthing[Graph name?]{The @racket[name] structure corresponding to sd:Graph.}
@defthing[NamedGraph name?]{The @racket[name] structure corresponding to sd:NamedGraph.}
@defthing[GraphCollection name?]{The @racket[name] structure corresponding to sd:GraphCollection.}

@defthing[defaultDataset name?]{The @racket[name] structure corresponding to sd:defaultDataset.}
@defthing[defaultGraph name?]{The @racket[name] structure corresponding to sd:defaultGraph.}
@defthing[graph name?]{The @racket[name] structure corresponding to sd:graph.}
@defthing[name name?]{The @racket[name] structure corresponding to sd:name.}
@defthing[namedGraph name?]{@racket[name] name structure corresponding to sd:namedGraph.}
