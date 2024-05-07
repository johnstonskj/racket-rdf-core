#lang scribble/manual

@(require racket/sandbox
          scribble/core
          scribble/eval
          scribble/html-properties
          langtag
          rdf/core/namespace
          rdf/core/literal
          rdf/core/statement
          rdf/core/triple
          rdf/core/quad
          rdf/core/graph
          rdf/core/dataset
          rdf/core/gq
          rdf/core/io
          (for-label langtag
                     racket/contract
                     net/url-structs
                     rdf/core/name
                     rdf/core/namespace
                     rdf/core/nsmap
                     rdf/core/literal
                     rdf/core/statement
                     rdf/core/triple
                     rdf/core/graph
                     rdf/core/quad
                     rdf/core/dataset
                     rdf/core/gq
                     rdf/core/io))

@;{============================================================================}

@(define example-eval
   (make-base-eval '(require racket/bool
                             rdf/core/name
                             rdf/core/literal
                             rdf/core/namespace
                             rdf/core/nsmap)
                   '(define (implies x y) (if (boolean=? x #t)
                                              (if (not (boolean=? y #t))
                                                  (raise-argument-error 'y "#t" y)
                                                  #t)
                                              #f))
                   '(define (iff x y) (cond ((boolean=? x #t)
                                             (if (not (boolean=? y #t))
                                                 (raise-argument-error 'y "#t" y)
                                                 #t))
                                            ((boolean=? x #f)
                                             (if (not (boolean=? y #f))
                                                 (raise-argument-error 'y "#f" y)
                                                 #t))
                                          (else #f)))))

@(define compact-list
   (make-style "Compact"
   (list (make-css-addition "scribblings/compact.css"))))

@(define figure-flow
   (make-style "Figure"
   (list (make-css-addition "scribblings/figure.css"))))
V
@(define wide-flow
   (make-style "Wide"
   (list (make-css-addition "scribblings/wide.css"))))

@;{============================================================================}

@title[#:version  "0.1.0"]{RDF Core Data Model}
@author[(author+email "Simon Johnston" "johnstonskj@gmail.com")]

This is the core data model for the Resource Description Framework (RDF) @cite["RDF11CAS"] in Racket. It also includes
core vocabularies such as @tt{rdf}, @tt{rdfs}, @tt{xml}, and @tt{xsd} as well as some basic IO functions. The goal of
this package is to provide the model allowing the user to create statements, graphs and datasets in-memory. Additional
Racket packages provide capabilities layered on top of this such as support for OWL, SPARQL, and additional
vocabularies.

@table-of-contents[]

@;{============================================================================}
@;{============================================================================}
@section[]{Introduction}

The RDF Data Model, or Abstract Syntax @cite["RDF11CAS"], is fundamentally very simple.

Figure 1 is a representation of the data model, extended to include common elements beyond the core described above.
Elements in the darker shading are the core types of the RDF data model, those in lighter shading are extensions to
the core types, while those with no shading represent data types.

@nested[#:style figure-flow]{
  @image["scribblings/rdf-data-model.svg"]

  The RDF Data Model
}

The kernel of the Data Model is the statement, or @italic{triple}, as shown in the example below.

@verbatim|{
,---------+-----------+--------,
| subject | predicate | object |
'---------+-----------+--------'
}|

This very simple structure allows multiple statements to be seen as a directed graph where subjects and objects may act
as pointers between statement. For example in the following example we can see that the first three sentences on the
left share a common subject, they are all statements about the thing with IRI @tt{<S1>}. The last two sentences are
similarly related in that they are both statements about the thing with IRI @tt{<S2>}. The last statement is interesting
because it's object, @tt{<S1>} is the identifier of an existing thing, allowing us to interpret a subset of the
statements below as @italic{"a thing with name 'Steve' knows a thing with the name 'Simon'"}. Now if we look at the
two statements in the middle we see that they again share object/subject but the identity of this @italic{thing} is not
an IRI but is anonymous, interpreted as @italic{"a thing with name 'Simon' has a job, which has a title 'Nerd'"}.

@verbatim|{
,---------+-----------+---------,
| <S1>    | is-a      | Person  |
'---------+-----------+---------'
,---------+-----------+---------,
| <S1>    | name      | "Simon" |
'---------+-----------+---------'
,---------+-----------+---------,    ,---------+-----------+--------,
| <S1>    | job       | [?]     |    | [?]     | title     | "Nerd" |
'---------+-----------+---------'    '---------+-----------+--------'
,---------+-----------+---------,
| <S2>    | name      | "Steve" |
'---------+-----------+---------'
,---------+-----------+---------,
| <S2>    | knows     | <S1>    |
'---------+-----------+---------'
}|

A @italic{set} of statements is termed a graph to denote this linking behavior. We can therefore draw the list of
statements in a more graph-like form as follows. This representation clearly shows how a set of statements can easily
model very complex structures.

@verbatim|{
,---------,
| <S1>    |<-----------------------------------------------,
'--|------'                                                |
   |        ,-----------+---------,                        |
   |------->| is-a      | Person  |                        |
   |        '-----------+---------'                        |
   |        ,-----------+---------,                        |
   |------->| name      | "Simon" |                        |
   |        '-----------+---------'                        |
   |        ,-----------+---------,                        |
   '------->| job       | <?>     |                        |
            '-----------+--|------'                        |
                           |        ,-----------+--------, |
                           '------->| title     | "Nerd" | |
                                    '-----------+--------' |
,---------,                                                |
| <S2>    |                                                |
'--|------'                                                |
   |        ,-----------+---------,                        |
   |------->| name      | "Steve" |                        |
   |        '-----------+---------'                        |
   |        ,-----------+---------,                        |
   '------->| knows     | <S1> ----------------------------'
            '-----------+---------'
}|

@;{============================================================================}
@subsection[]{RDF Glossary}

@tabular[#:sep @hspace[2]
         #:column-properties '(top top)
         (list
          (list @bold{Blank Node}
                @para{A type that allows @italic{anonymous} subjects and objects where grouping is desired but there
                        is no need for a separate and addressable resource.})
          (list @bold{Dataset}
                @para{A collection of named graphs at most one unnamed graph refered to as the @italic{default graph}.
                        The dataset itself may also be named with an IRI.})
          (list @bold{Datatype}
                @para{See @cite["RDF11CAS"], section 5, Datatypes (Normative).})
          (list @bold{Graph}
                @para{A @italic{set} of statements, which may be relaxed to become a list of statements allowing
                        duplicates.})
          (list @bold{IRI}
                @para{An @italic{Internationalized Resource Identifiers} @cite["RFC3987"].})
          (list @bold{Language Tag}
                @para{See @cite["RFC5646"] (BCP-47).})
          (list @bold{Literal}
                @para{This represents a single, scalar, value usually corresponding to one of the data types in
                           @cite["XMLXSD2"].})
          (list @bold{Local Name}
                @para{A ...})
          (list @bold{Named Graph}
                @para{A graph with an associated name, this name is an IRI.})
          (list @bold{Namespace}
                @para{A resources containing @italic{some representation} of an RDF vocabulary.})
          (list @bold{Namespace Map}
                @para{A mapping between @italic{prefix names} and @italic{namespaces}, with at most one unnamed
                        namespace.})
          (list @bold{Object}
                @para{The value of a property, or target of a relation, between a subject and an object. A subject may
                          be either a @italic{resource}, @italic{blank node}, or @italic{literal}.})
          (list @bold{Predicate}
                @para{The name of a @italic{property of}, or @italic{relation between}, a subject and an object.})
          (list @bold{Representation}
                @para{A particular serialization form of a resource. In general it is assumed that if a resource may
                        have multiple representations, each representation is isomorphic.})
          (list @bold{Resource}
                @para{Some @italic{thing} identified by one or more IRIs and which is avilable in one or more
                           representations.})
          (list @bold{Prefix Name}
                @para{A ...})
          (list @bold{Schema}
                @para{A ... @cite["RDF11SCHEMA"], @cite["OWL2"].})
          (list @bold{Statement}
                @para{A triple comprising @italic{subject}, @italic{predicate}, and @italic{object}. In predicate logic
                        this would be written as @tt{(predicate subject object)}.})
          (list @bold{Subject}
                @para{The @italic{thing} about which a statement is made. A subject may be either a @italic{resource} or
                          @italic{blank node}.})
          (list @bold{Triple}
                @para{Another term for a statement.})
          (list @bold{Vocabulary}
                @para{A ...})
 )]

@;{============================================================================}
@subsection[]{Examples}

The example RDF used below is similar to that used in the figures above. It describes a simple five statement graph
using a Github profile as the subject. Using different representations of the graph we will see how different aspects
if the data model in figure 1 are implemented.

@bold{Example 1 -- N-Triples}

The N-Triple @cite["RDF11NT"] representation demonstrates the core elements of the RDF Data Model well as each line
contains an individual statement terminated by a @tt{"."} and newline.

@nested[#:style 'code-inset]{
@nested[#:style wide-flow]{
@verbatim|{
<https://github.com/johnstonskj> <http://www.w3.org/1999/02/22-rdf-syntax-ns#> <http://xmlns.com/foaf/0.1/Person> .
<http://github.com/johnstonskj> <http://xmlns.com/foaf/0.1/name> "Simon Johnston"@en .
<http://github.com/johnstonskj> <http://xmlns.com/foaf/0.1/givenname> "Simon" .
<http://github.com/johnstonskj> <http://xmlns.com/foaf/0.1/family_name> "Johnston" .
<http://github.com/johnstonskj> <http://github.com/rdf/followers> "22"^^<http://www.w3.org/2001/XMLSchema#int> .
}|
}
}

As we will see in most representations, IRI values are written between @tt{"<"} and @tt{">"}. Literal values are always
enclosed in double quotes but come on three forms:

@itemlist[#:style 'ordered

  @item{Plain literals are simple strings.}

  @item{String values may have attached language identifiers to allow for multiple-language variants of a value. The
               literal value @tt{"Simon Johnston"@"@en"} has a language tag @cite["RFC5646"] after the at-sign.}

  @item{A literal may be explicitly typed, allowing a parser to understand the string value as some other data type.
          The literal value @tt{"22"^^<...#int>} has the explicit XML Schema @tt{int} datatype.}
]

@bold{Example 2 -- Turtle}

The following example is identical to the N-Triple version, but using the Turtle @cite["RDF11TTL"] representation. This
representation shows the use of namespace prefixes to shorten IRIs and generally make the representation more readable.
The @tt{prefix} directive creates a mapping between a prefix, a string with a colon @tt{":"} suffix, and a namespace
IRI. Note that a prefix with no string before the colon is termed the @italic{default namespace}. Also note that this
format uses the semicolon character @tt{";"} to allow multiple predicate-object pairs for the same subject, similar to the
graph representation above. Each set of statements is still terminated by a @tt{"."} as in N-Triples.

@nested[#:style 'code-inset]{
@verbatim|{
@prefix : <http://github.com/>
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ghub: <http://github.com/rdf/>

:johnstonskj
  a foaf:Person ;
  foaf:name "Simon Johnston"@en ;
  foaf:givenname "Simon" ;
  foaf:family_name "Johnston" ;
  ghub:followers "22"^^<http://www.w3.org/2001/XMLSchema#int> .
}|
}

Any valid set of RDF statements, including the empty set, is a valid RDF graph. Therefore, the resource holding the
Turtle example above is a representation of an RDF graph.

@bold{Example 3 -- TriG}

Now we have a graph it is useful to be able to name graphs explicitly and also to document groups of graphs. RDF defines
a Dataset, described in the introduction to @cite["RDF11DS"] as:

@nested[#:style 'inset]{
@italic{
The Resource Description Framework (RDF) version 1.1 defines the concept of RDF datasets, a notion introduced first by
the SPARQL specification. An RDF dataset is defined as a collection of RDF graphs where all but one are named graphs
associated with an IRI or blank node (the graph name), and the unnamed default graph. Given that RDF is a data model
equipped with a formal semantics, it is natural to try and define what the semantics of datasets should be.
}
}

Using TriG @cite["RDF11TRIG"] representation, a super-set of Turtle, we can see a new structure of the form
@tt{graph-name {...}} which binds a graphs's name to the set of statements between @tt{"{"} and  @tt{"}"}. A graph name
may be either an IRI or a blank-node, in the same way as a statement's subject.

@nested[#:style 'code-inset]{
@verbatim|{
@prefix : <http://github.com/>
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix tiny: <https://tinyurl.com/> .

_:G1 {
  :johnstonskj
    a foaf:Person ;
    foaf:name "Simon Johnston"@en ;
    foaf:givenname "Simon" ;
    foaf:family_name "Johnston" ;
    ghub:followers "22"^^<http://www.w3.org/2001/XMLSchema#int> .
}
}|
}

@bold{Example 4 -- N-Quads}

The named graph labeled @tt{_:G1} can also be serialized into a format similar to the N-Triples above, N-Quads
@cite["RDF11NQ"]. The difference is that each line is now comprised of subject, predicate, object, and the graph's
label (IRI or blank node).

@nested[#:style 'code-inset]{
@nested[#:style wide-flow]{
@verbatim|{
<http://github.com/johnstonskj> <http://www.w3.org/1999/02/22-rdf-syntax-ns#> <http://xmlns.com/foaf/0.1/Person> _:G1 .
<http://github.com/johnstonskj> <http://xmlns.com/foaf/0.1/name> "Simon Johnston"@en _:G1 .
<http://github.com/johnstonskj> <http://xmlns.com/foaf/0.1/givenname> "Simon" _:G1 .
<http://github.com/johnstonskj> <http://xmlns.com/foaf/0.1/family_name> "Johnston" _:G1 .
<http://github.com/johnstonskj> <http://github.com/rdf/followers> "22"^^<http://www.w3.org/2001/XMLSchema#int> _:G1 .
}|
}
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module name}
@defmodule[rdf/core/name]

Figure 1 in the introduction included a both a @tt{PrefixName} and @tt{LocalName} datatype, but these are actually
more commonly used in combination, and with the datatype @tt{Namespace}. Figure 2 introduces the type @tt{PrefixedName}
which is a tuple of prefix and local-name, and the type @tt{NamespacedName} which is a tuple of namespace IRI and
local-name. Prefixed names can be translated into namespaced names by substituting the prefix name with the corresponding
namespace in a @tt{NamespaceMap}.

@nested[#:style figure-flow]{
  @image["scribblings/rdf-names.svg"]

  RDF Name Types
}

The value space for a local name is sometimes different in RDF implementations, depending on their age and
representation support. The definitions used in this package are those defined by SPARQL and adopted by Turtle, for
more information see @secref["Appendix__Names_Defined"].

@defproc[#:kind "predicate"
         (local-name-string?
          [v string?])
         boolean?]{
This predicate returns @racket[#t] if the value of @racket[v] is valid according to the SPARQL production @tt{PN_LOCAL}.

@examples[
  #:eval example-eval
  (local-name-string? "a-valid-name")
  (local-name-string? 'another-valid-name)
  (local-name-string? "?")
  (local-name-string? "")
]
}

@defstruct*[local-name () #:constructor-name string->local-name]{
This structure provides a safe and efficient way to wrap a string that conforms to the predicate
@racket[local-name-string?]. This ensures that the name cannot be mutated, and the predicate @racket[local-name?] is
more efficient than parsing the string.

@examples[
  #:eval example-eval
  (require racket/list racket/string)
  (let ((long-name (string-join (map (lambda (_) "this-is-a-long-name")
                                     (range 1 200))
                                "-also-")))
    (time (local-name-string? long-name))
    (let ((name (string->local-name long-name)))
      (time (local-name? name))))
]
}

@defproc[(local-name->string
          [v local-name?])
         local-name-string?]{
Returns the local name as a string.

@examples[
  #:eval example-eval
  (local-name->string (string->local-name "a-valid-name"))
]
}

The following table shows how different combinations of namespace, prefix, local name, and IRI can be constructed.

@tabular[
#:style 'boxed
#:sep @hspace[2]
#:row-properties '(bottom-border ())
#:column-properties '(top top top top (top center))
(list (list @bold{function} @bold{from} @bold{add} @bold{into} @bold{map?})

      (list @racket[namespace+name->url] @racket[namespace?] @racket[local-name?] @racket[url?] @para{No})
      (list @racket[namespace+name->prefixed-name] @racket[namespace?] @racket[local-name?] @racket[prefixed-name?] @para{Yes})

      (list @racket[nsname->url] @racket[nsname?] "" @racket[url?] @para{No})
      (list @racket[nsname-make-nsname] @racket[nsname?] @racket[local-name?] @racket[nsname?] @para{No})
      (list @racket[nsname->prefixed-name] @racket[nsname?] "" @racket[prefixed-name?] @para{Yes})

      (list @racket[prefix+name->nsname] @racket[prefix?] @racket[local-name?] @racket[nsname?] @para{Yes})
      (list @racket[prefix+name->url] @racket[prefix?] @racket[local-name?] @racket[url?] @para{Yes})

      (list @racket[prefixed-name->nsname] @racket[prefixed-name?] "" @racket[nsname?] @para{Yes})
      (list @racket[prefixed-name->url] @racket[prefixed-name?] "" @racket[url?] @para{Yes})
)
]

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module namespace}
@defmodule[rdf/core/namespace]

This module provides types corresponding to a namespace identifier (IRI), as well as a namespaced name.

@local-table-of-contents[]

@;{============================================================================}
@subsection[]{Namespaces}

@defproc[#:kind "predicate"
         (url-absolute?
          [val any/c])
         boolean?]{
Predicate to check that the value @italic{val} is a @racket[url?], and is an absolute URI, not relative.

@examples[
  #:eval example-eval
  (require net/url-string)
  (url-absolute? (string->url "http://example.com/some/path"))
  (url-absolute? (string->url "/some/other/path"))
]
}

@defproc[#:kind "predicate"
         (namespace-url?
          [val any/c])
         boolean?]{
Predicate to check that the value @italic{val} is a @racket[url-absolute?], @bold{and} either ends in a path separator
@tt{"/"} or an empty fragment identifier @tt{"#"}.

@examples[
  #:eval example-eval
  (require net/url-string)
  (namespace-url? (string->url "/some/other/path"))
  (namespace-url? (string->url "http://example.com/some/path"))
  (namespace-url? (string->url "http://example.org/schema/nspace#"))
  (namespace-url? (string->url "http://example.org/schema/nspace/"))
  (namespace-url? (string->url "http://example.org/"))
]
}

@; ---------- struct namespace  ----------

@defstruct*[namespace () #:constructor-name url->namespace]{
This structure provides a safe and efficient way to wrap a @racket[url?] that conforms to the predicate
@racket[namespace-url?]. This ensures that the namespace cannot be mutated, and the predicate @racket[namespace?] is
more efficient than parsing the URI (see @racket[local-name?] for a comparison).
}

@defproc[#:kind "constructor"
         (string->namespace
          [url string?])
         namespace?]{
Returns a new @racket[namespace?] structure by parsing the provided @racket[url] string.
}

@defproc[(namespace->url
          [ns namespace?])
         namespace-url?]{
Returns the namespace URI as a @racket[url?] structure.
}

@defproc[(namespace->string
          [ns namespace?])
         string?]{
Returns a string representation of the namespace @racket[ns] URI.
}

@defproc[(namespace+name->url
          [ns namespace?]
          [name (or/c local-name-string? local-name?)])
         url-absolute?]{
Returns a new URI by appending the value of @racket[name] to the URI value of @racket[ns].

@examples[
  #:eval example-eval
  (require net/url-string)
  (url->string
    (namespace+name->url
      (string->namespace "http://example.com/schema/things#")
      "ThingOne"))
]
}

@;{============================================================================}
@subsection[]{Namespaced Names}

A @racket[namespaced-name] comprises a name and corresponding namespace, commonly noted as a tuple
@tt{(namespace, local-name)}.

@defstruct*[nsname
            ([namespace namespace?]
             [name local-name?])]{
The @racket[namespace] and @racket[local-name] pair.

@examples[
  #:eval example-eval
  (require net/url-string)
  (let ((ex-name (nsname (string->namespace "http://example.org/schema/nspace/")
                         (string->local-name "Name"))))
    (displayln (nsname-namespace ex-name))
    (displayln (nsname-name ex-name)))
]
}

@defproc[#:kind "constructor"
         (make-nsname
          [namespace (or/c string? namespace-url? namespace?)]
          [name (or/c local-name-string? local-name?)])
         name?]{
Returns a new @racket[nsname] from the @racket[namespace] and the value of @racket[name].
This is a wrapper around the @racket[nsname] constructor and takes a relaxed set of types for ease of use.

@examples[
  #:eval example-eval
  (require net/url-string)
  (let ((ex-name (make-nsname "http://example.org/schema/nspace/" "Name")))
    (displayln (nsname-namespace ex-name))
    (displayln (nsname-name ex-name)))
]
}

@defproc[(nsname->url
          [nsname nsname?])
         url-absolute?]{
Returns a new URI concatenating the name's namespace IRI and local name values.

@examples[
  #:eval example-eval
  (nsname->url
   (url->nsname
    (string->url "http://example.org/schema/nspace#name")))
]
}

@defproc[(nsname->string
          [nsname nsname?])
         string?]{
Returns a new string concatenating the name's namespace IRI and local name values.

@examples[
  #:eval example-eval
  (nsname->string
   (url->nsname
    (string->url "http://example.org/schema/nspace#name")))
]
}

@defproc[(nsname-make-nsname
          [from nsname?]
          [name (or/c local-name-string? local-name?)])
         nsname?]{
Returns a new @racket[nsname?] concatenating the namespace IRI from @racket[from] and @racket[name].

@examples[
  #:eval example-eval
  (require net/url-string)
  (let* ((ex-name (make-nsname "http://example.org/schema/nspace/" "Name"))
         (new-name (nsname-make-nsname ex-name "New")))
    (displayln (format "~a => ~a"
                       (local-name->string (nsname-name ex-name))
                       (local-name->string (nsname-name new-name)))))
]
}

@defproc[(url->namespace+name
          [url url-absolute?])
         (or/c (cons/c url-absolute? string?) #f)]{
If the value of @racket[url] has a fragment part, return @racket[url] minus the fragment as the namespace and the
fragment value as the name. If the value of @racket[url] has a path with a final segment, return @racket[url] minus the
final path segment as the namespace and the path's final segment value as the name. If neither of these conditions are
met the URI is not a valid namespaced-name and the value @racket[#f] is returned.

@examples[
  #:eval example-eval
  (url->namespace+name (string->url "http://example.org/schema/nspace#name"))
  (url->namespace+name (string->url "http://example.org/schema/nspace#"))
  (url->namespace+name (string->url "http://example.org/schema/nspace/name"))
  (url->namespace+name (string->url "http://example.org/schema/nspace/name/"))
  (url->namespace+name (string->url "http://example.org/"))
  (url->namespace+name (string->url "http://example.org"))
]
}

@defproc[(url->nsname [url url-absolute?]) nsname?]{
Returns a new @racket[nsname] from the components returned by calling @racket[url->namespace+name] with the value
@racket[url].

@examples[
  #:eval example-eval
  (url->nsname (string->url "http://example.org/schema/nspace#name"))
  (url->nsname (string->url "http://example.org/schema/nspace/name"))
  (url->namespace+name (string->url "http://example.org/schema/nspace#"))
  (url->namespace+name (string->url "http://example.org/schema/nspace/name/"))
  (url->namespace+name (string->url "http://example.org/"))
  (url->namespace+name (string->url "http://example.org"))
]
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module nsmap}
@defmodule[rdf/core/nsmap]

Namespace maps are used to create qualified names in serialization formats such as Turtle, or SPARQL. A namespace map
allows bi-directional lookups between the @racket[namespace] URI and @racket[prefix].

@local-table-of-contents[]

@;{============================================================================}
@subsection[]{Prefixes}

A @racket[prefix] is the string mapped to a namespace in a namespace map. This prefix can be used in place of the
complete namespace URI.

@defproc[#:kind "predicate"
         (prefix-string? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a string and the string conforms to the SPARQL production @tt{PNAME_NS}.
}

@defproc[#:kind "predicate"
         (prefix-name-string? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a string and the string conforms to the SPARQL production @tt{PN_PREFIX}.
}

@defstruct*[prefix () #:omit-constructor]{
This structure provides a safe and efficient way to wrap either a string that conforms to the predicate
@racket[prefix-string?] or the @italic{empty} value. This ensures that the name cannot be mutated, and the predicate
@racket[prefix-name?] is more efficient than parsing the string.
}

@defproc[#:kind "constructor"
         (string->prefix
          [str (or/c prefix-string? prefix-name-string?)])
         prefix?]{
Returns a new @racket[prefix] if the value of @racket[str] is a valid @racket[prefix-string?].
}

@defproc[#:kind "constructor"
         (empty-prefix)
         prefix?]{
Returns an @italic{empty} prefix name.
}

@defproc[#:kind "predicate"
         (prefix-empty? [v any/c]) boolean?]{

@examples[
  #:eval example-eval
(prefix-empty? (string->prefix "rdf:"))
(prefix-empty? (string->prefix ":"))
(prefix-empty? (empty-prefix))
]
}

@defproc[(prefix->string
          [nsprefix prefix?])
         string?]{

@examples[
  #:eval example-eval
(prefix->string (string->prefix "rdf:"))
(prefix->string (string->prefix ":"))
(prefix->string (empty-prefix))
]
}

@defproc[(prefix+name->nsname
          [prefix namespace-prefix?]
          [name local-name-string?]
          [map nsmap?])
         nsname?]{
Returns a new @racket[nsname] if the @racket[prefix] is in @racket[map], else @racket[#f].

@examples[
  #:eval example-eval
  (let ((map (make-common-nsmap)))
    (url->string
     (nsname->url
      (prefix+name->nsname
       (string->prefix "dcterms:")
       (string->local-name "description")
       map))))
]
}

@defproc[(prefix+name->url
          [prefix namespace-prefix?]
          [name local-name-string?]
          [map nsmap?])
         url-absolute?]{
Returns a new @racket[url] if @racket[prefix] is in @racket[map], else @racket[#f].

@examples[
  #:eval example-eval
  (let ((map (make-common-nsmap)))
    (url->string
     (prefix+name->url
      (string->prefix "dcterms:")
      (string->local-name "description")
      map)))
]
}

@;{============================================================================}
@subsection[]{Prefixed Names}

A @racket[prefixed-name] comprises a name and corresponding prefix, commonly noted as a tuple @tt{(prefix, local-name)}.

@defthing[prefixed-name-separator char?]{
The colon character used to separate the prefix and local name parts of a prefixed name.
}

@defproc[#:kind "predicate"
         (prefixed-name-string? [v any/c]) boolean?]{
Returns @racket[#t] if @italic{v} is a string, and that string is a prefixed name according to the SPARQL production
@tt{PrefixedName}.

}

@defstruct*[prefixed-name
            ([prefix (or/c prefix-name? #f)]
             [name local-name? ])]{
The @racket[prefix] and @racket[local-name] pair.
}

@defproc[#:kind "constructor"
         (make-prefixed-name
          [prefix  (or/c prefix-string? prefix-name?)]
          [name (or/c local-name-string? local-name?)])
         prefixed-name?]{
TBD

@examples[
  #:eval example-eval
(prefixed-name->string
 (make-prefixed-name (string->prefix "rdf:") "Hello"))
(prefixed-name->string
 (make-prefixed-name ":" "Hello"))
(prefixed-name->string
 (make-prefixed-name (empty-prefix) "Hello"))
]
}

@defproc[#:kind "constructor"
         (string->prefixed-name
          [str prefixed-name-string?])
         prefixed-name?]{
TBD
}

@defproc[(prefixed-name->string
          [name prefixed-name?])
         prefixed-name-string?]{
TBD
}

@defproc[(prefixed-name->nsname
          [name prefixed-name?]
          [nsmap nsmap?])
         (or/c nsname? #f)]{
See @racket[prefix+name->nsname].
}

@defproc[(prefixed-name->url
          [name prefixed-name?]
          [nsmap nsmap?])
         (or/c url-absolute? #f)]{
See @racket[prefix+name->url].
}

@defproc[(nsname->prefixed-name
          [name nsname?]
          [map nsmap?])
         prefixed-name?]{
See @racket[namespace+name->prefixed-name].
}

@defproc[(namespace+name->prefixed-name
          [ns namespace?]
          [name local-name-string?]
          [map nsmap?])
         prefixed-name?]{
Returns a new @racket[prefixed-name] if @racket[ns] is in @racket[map], else @racket[#f].

@examples[
  #:eval example-eval
  (let ((map (make-common-nsmap)))
    (prefixed-name->string
     (namespace+name->prefixed-name
      (string->namespace "http://purl.org/dc/terms/")
      (string->local-name "description")
      map)))
]
}

@;{============================================================================}
@subsection[]{Namespace Map}

A namespace map is essential in serializing and deserializing RDF datasets, graphs, and statements.

@defstruct*[nsmap ()]{
This struct wraps a @racket[hash] between @racket[prefix?] and @racket[namespace?] values.
}

@defproc[#:kind "constructor"
         (make-rdf-only-nsmap)
         nsmap?]{
Returns a new @racket[nsmap] containing a mapping for the RDF namespace only.
}

@defproc[#:kind "constructor"
         (make-common-nsmap)
         nsmap?]{
Returns a new @racket[nsmap] containing mappings for commonly used namespaces.
}

@defproc[#:kind "constructor"
         (make-nsmap
          [assocs (listof (cons/c prefix? namespace?)) '()])
         nsmap?]{
Returns a new @racket[nsmap] containing the mappings in @racket[assocs].
}

@defproc[#:kind "predicate"
         (nsmap-empty?
          [map nsmap?])
         boolean?]{
Returns @racket[#t] if @racket[map] contains no mapping values.

@examples[
#:eval example-eval
(let ((map (make-nsmap)))
  (iff (nsmap-empty? map) (= (nsmap-count map) 0)))
]
}

@defproc[(nsmap-count
          [map nsmap?])
         exact-nonnegative-integer?]{
Returns the number of mapping values in @racket[map].

@examples[
#:eval example-eval
(let ((map (make-common-nsmap)))
  (iff (not (nsmap-empty? map)) (> (nsmap-count map) 0)))
]
}

@;{============================================================================}

@defproc[(nsmap-has-default?
          [map nsmap?])
         boolean?]{
Returns @racket[#t] iff the namespace @racket[map] contains a namespace value for the default prefix @tt{":"}.
}

@defproc[(nsmap-ref-default
          [map nsmap?])
         (or/c namespace? #f)]{
Returns the @racket[namespace?] associated with the default prefix @tt{":"}, or @racket[#f] if none exists.
}

@defproc[(nsmap-has-prefix?
          [map nsmap?]
          [prefix prefix?])
         boolean?]{
Returns @racket[#t] iff the @racket[map] contains a namespace value for @racket[prefix].
}

@defproc[(nsmap-ref
          [map nsmap?]
          [prefix prefix?])
         (or/c namespace? #f)]{
Returns the @racket[namespace?] associated with @racket[prefix], or @racket[#f] if none exists.
}

@defproc[(nsmap-ref!
          [map nsmap?]
          [prefix prefix?]
          [to-set namespace?])
         (or/c namespace? #f)]{
TBD
}

@defproc[(nsmap-set-default!
          [map nsmap?]
          [url namespace?])
         void?]{
TBD
}

@defproc[(nsmap-set!
          [map nsmap?]
          [prefix prefix?]
          [url namespace?])
         void?]{
TBD
}

@defproc[(nsmap-remove!
          [map nsmap?]
          [prefix prefix?])
         void?]{
TBD
}

@defproc[(nsmap-update!
          [map nsmap?]
          [prefix prefix?]
          [updater (-> namespace? namespace)])
         boolean?]{
TBD
}

@defproc[(nsmap-clear!
          [map nsmap?])
         void?]{
TBD
}

@defproc[(nsmap-has-url?
          [map nsmap?]
          [url namespace?])
         boolean?]{
TBD
}

@defproc[(nsmap-prefix-ref
          [map nsmap?]
          [url namespace?])
         (or/c prefix? #f)]{
TBD
}

@;{============================================================================}

@defproc[(nsmap-map
          [map nsmap?]
          [proc (-> ncname? namespace? any/c)]
          [try-order? any/c #f])
         (listof any/c)]{
TBD
}

@defproc[(nsmap-names
          [map nsmap?]
          [try-order? any/c #f])
         (listof prefix?)]{
TBD
}

@defproc[(nsmap-namespaces
          [map nsmap?]
          [try-order? any/c #f])
         (listof namespace?)]{
TBD
}

@defproc[(nsmap->list
          [map nsmap?]
          [try-order? any/c #f])
         (listof (cons/c prefix? namespace?))]{
TBD
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module literal}
@defmodule[rdf/core/literal]

A literal in RDF is a three-part value, the @italic{Lexical Form}, @italic{Datatype IRI}, and @italic{Language Tag}. The lexical
form is a string representation of the value, and is the only required component (although the string may be empty).
The datatype IRI is used to identify the type of the value, and therefore how to evaluate the lexical form to produce a
final value. The language tag is used to identify the natural language used to write the string value in the lexical
form.

Note, there is only a single case where both datatype IRI and language tag are present at the same time, when the
language tag is present the value is @bold{by definition} typed as @tt{rdf:langString}.

@local-table-of-contents[]

@;{============================================================================}
@subsection[]{Literal Type}

@defstruct*[literal
            ([lexical-form string?]
             [datatype-iri url-absolute?]
             [language-tag language-tag?])]{
This structure combines the three values @italic{lexical-form}, @italic{datatype-iri}, and @italic{language-tag}.
}

@defproc[#:kind "constructor"
         (make-untyped-literal
          [form string?])
         literal?]{
Returns a new @racket[literal] as an untyped, non-language-tagged string.

@examples[
  #:eval example-eval
  (make-untyped-literal "22")
]
}

@defproc[#:kind "constructor"
         (make-typed-literal
          [form string?]
          [datatype-iri url-absolute?])
         literal?]{
Returns a new @racket[literal] as a typed, non-language-tagged string.

@examples[
  #:eval example-eval
  (make-typed-literal "22" (string->url "http://www.w3.org/2001/XMLSchema#byte"))
]
}

@defproc[#:kind "constructor"
         (make-lang-string-literal
          [form string?]
          [language language-tag?])
         literal?]{
Returns a new @racket[literal] as an untyped, language-tagged string.

@examples[
  #:eval example-eval
  (make-lang-string-literal "22" "en")
]
}

@;{============================================================================}
@subsection[]{Literal Predicates}

@defproc[#:kind "predicate"
         (has-datatype-iri?
          [val literal?])
         boolean?]{
Returns true if this literal has a datatype URI.
}

@defproc[#:kind "predicate"
         (has-language-tag?
          [val literal?])
         boolean?]{
Returns true if this literal has a language tag.
}

@defproc[#:kind "predicate"
         (has-xsd-datatype?
          [val literal?])
         boolean?]{
Returns true if this literal has a datatype URI @italic{and} that URI is in the XML Schema namespace.
}

@defproc[#:kind "predicate"
         (is-a?
          [val literal?]
          [datatype url-absolute?])
         boolean?]{
Returns true if this literal has a datatype URI @italic{and} that URI is @racket[equal?] to @racket{datatype}.
}

@;{============================================================================}
@subsection[]{Conversion to Literal}

@defproc[(boolean->literal
          [val boolean?])
         literal?]{
Returns a new typed literal with the datatype @tt{xsd:boolean}.
}

@defproc[(bytes->literal
          [val bytes?])
         literal?]{
Returns a new typed literal with the datatype @tt{xsd:hexBinary}.
}

@defproc[(date->literal
          [val date?])
         literal?]{
Returns a new typed literal with the datatype @tt{xsd:date}.
}

@defproc[(datetime->literal
          [val date?])
         literal?]{
Returns a new typed literal with the datatype @tt{xsd:dateTime}.
}

@defproc[(exact-integer->literal
          [val exact-integer?])
         literal?]{
Returns a new typed literal with the datatype @tt{xsd:integer}.
}

@defproc[(flonum->literal
          [val flonum?])
         literal?]{
Returns a new typed literal with the datatype @tt{xsd:float} or @tt{xsd:double}.
}

@defproc[(inexact->literal
          [val inexact?])
         literal?]{
Returns a new typed literal with the datatype @tt{xsd:decimal}.
}

@defproc[(string->literal
          [val string?])
         literal?]{
Returns a new typed literal with the datatype @tt{xsd:string}.
}

@defproc[(time->literal
          [val date?])
         literal?]{
Returns a new typed literal with the datatype @tt{xsd:time}.
}

@defproc[(->literal
          [val (or/c boolean? bytes? date? string? exact-integer? flonum? inexact?)])
         literal?]{
Attempts to create a literal from @racket{val} if it matches one of the type predicates listed.
}

@;{============================================================================}
@subsection[]{Predefined Literals}

@defthing[literal-true literal?]{The value @racket[#t] as a literal value.}

@defthing[literal-false literal?]{The value @racket[#f] as a literal value.}

@defthing[literal-empty-string literal?]{The value @racket[""] as a literal value.}

@defthing[literal-exact-zero literal?]{The value @racket[0] as a literal value.}

@defthing[literal-flonum-zero literal?]{The value @racket[0.0] as a literal value.}

@defthing[literal-inexact-zero literal?]{The value @racket[0.0] as a literal value.}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module statement}
@defmodule[rdf/core/statement]

This module models a single RDF statement (@italic{subject}, @italic{predicate}, and @italic{object}) as a generic
interface that may be implemented by other concrete structures. It therefore also describes these components themselves
with predicates and structures of their own.

@local-table-of-contents[]

@;{============================================================================}
@subsection{Blank Nodes}

A blank node is used as either a subject or object in a graph that is able to link statements where no resource
represents the concept.

@defproc[#:kind "predicate"
         (blank-node-string?
          [val any/c])
         boolean?]{
TBD
}

@defproc[#:kind "predicate"
         (blank-node-label-string?
          [val any/c])
         boolean?]{
TBD
}

@defstruct*[blank-node () #:omit-constructor]{
This struct wraps a single @italic{label} value of type @racket[local-name?].
}

@defproc[#:kind "constructor"
         (make-blank-node
          [label (or/c blank-node-label-string? #f) #f ])
         blank-node?]{
Returns a new blank node with either the provided @racket[label], or a uniquely assigned label. The current
implementation guarantees that it will generate new unique identifiers within the same process, two processes
running separately may generate overlapping identifiers.
}

@defproc[(blank-node->string
          [node blank-node?])
         string?]{
TBD
}

@subsection[]{Component Predicates}

@defproc[#:kind "predicate"
         (resource?
          [val any/c])
         boolean?]{
A resource is identified by a URI, and so this test effectively ensures that @racket[val] is an absolute URI.
}

@defproc[#:kind "predicate"
         (subject?
          [val any/c])
         boolean?]{
A statement's subject may be a @racket[resource?] or a @racket[blank-node?].
}

@defproc[#:kind "predicate"
         (predicate?
          [val any/c])
         boolean?]{
A statement's predicate must be a @racket[resource?].
}

@defproc[#:kind "predicate"
         (object?
          [val any/c])
         boolean?]{
A statement's object may be either a @racket[resource?], @racket[blank-node?], or a @racket[literal?].
}

@;{============================================================================}
@subsection[]{Generic Interface}

@defthing[gen:statement any/c]{
A @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{generic interface} (see
@secref[#:doc '(lib "scribblings/reference/reference.scrbl")]{struct-generics}) that defines a @deftech{statement}. To
supply method implementations, a struct should use the @racket[#:methods] form.
}

@defthing[statement/c contract?]{
TBD
}

@defthing[statement-constructor/c contract?]{
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

@;{============================================================================}
@subsection[]{Statement Conversion}

@defproc[#:kind "predicate"
         (statement-list?
          [val any/c])
         boolean?]{
TBD
}

@defproc[(statement->list
          [stmt statement?])
         (list/c subject? predicate? object?)]{
Turn any statement into a list of three components.
}

@defproc[(statement->reified-list
          [stmt statement?])
         (listof (list/c subject? predicate? object?))]{
Reify the statement and return a list of lists.
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module triple}
@defmodule[rdf/core/triple]

The Triple type is a concrete implementation of the @racket[gen:statement] generic interface that is only the three
standard components.

@local-table-of-contents[]

@;{============================================================================}
@subsection[]{Triple Type}

@defstruct*[triple
            ([subject subject?]
             [predicate predicate?]
             [object object?])]{
TBD

Implements the generic interface @racket[gen:statement].
}

@defproc[(list->triple
          [stmt (list/c subject? predicate? object?)])
         triple?]{
Convert a list of three components into a triple structure. This is the opposite of the generic
@racket[statement->list].
}

@defproc[(reify
          [subject subject?]
          [predicate predicate?]
          [object object?])
         (listof triple?)]{
TBD
}

@defproc[(statement->reified-triples
          [stmt statement?])
         (listof triple?)]{
See @racket[reify].
}

@;{============================================================================}
@subsection[]{Additional Constructors}

@defproc[(statement-list
          [subject subject?]
          [predicate-object-list (listof (list/c predicate? (or/c object? (listof object?))))])
         (listof triple?)]{
TBD
}

@defproc[(anon-statement-list
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

@defproc[(type-statement
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
@section[#:style '(toc)]{Module graph}
@defmodule[rdf/core/graph]

A graph is a, possibly named, set of statements. While the RDF semantics define a graph as a @italic{set} of statements
this implementation uses a list which provides for duplicate statements. At any time the @racket[graph->distinct-graph]
function returns a version of the current graph with any duplicate statements removed.

@local-table-of-contents[]

@;{============================================================================}
@subsection[]{Graph Type}

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
         (unnamed-graph
          [statements statement-list?])
         graph?]{
Returns a new @racket[graph?] that has no name. In this case the value of @racket[graph-name] is @racket[#f].
}

@defproc[#:kind "constructor"
         (named-graph
          [name graph-name?]
          [statements statement-list?])
         graph?]{
Returns a new @racket[graph?] that has an explicit @italic{name}.
}

@defproc[(graph-name-or-blank
          [graph graph?])
         subject?]{
Return the name of the @italic{graph}, if it was created as a default graph return a newly created blank node instead.
}

@;{============================================================================}
@subsection[]{Graph Predicates & Properties}

@defproc[#:kind "predicate"
         (graph-named?
          [val graph?])
         boolean?]{
Return @racket[#t] if @italic{graph} was created as a named graph.
}

@defproc[#:kind "predicate"
         (graph-empty?
          [val graph?])
         boolean?]{
Return @racket[#t] if this graph contains no statements.
}

@defproc[#:kind "predicate"
         (graph-has-duplicates?
          [val graph?])
         boolean?]{
Returns @racket[#t] if the graph has duplicate statements. 
}

@defproc[(graph-count
          [graph graph?])
         exact-positive-integer?]{
Returns the number of statements in the graph, this does not take into account whether there are duplicate statements.
}

@defproc[(graph-order
          [graph graph?])
         exact-positive-integer?]{
Returns the @italic{order} of the graph, or the number of vertices. For RDF a vertex is defined as any
@racket[resource?] or @racket[blank-node?] in the graph.
}

@defproc[(graph->distinct-graph [graph graph?]) graph?]{
Returns a new @racket[graph] where the list of statements in the source @racket[graph] have been removed. The name of
the source is also preserved in the new copy.
}

@;{============================================================================}
@subsection[]{Indices}

@defthing[graph-index-kind/c]{
A contract defining the set of suport index types.
}

@defproc[#:kind "predicate"
         (graph-has-index?
          [graph graph?]
          [index-kind graph-index-kind/c])
         boolean?]{
Return @racket[#t] if @italic{graph} supports an index of kind @italic{index-kind}.
}

@defproc[(graph-indexes
          [graph graph?])
         (listof graph-index-kind/c)]{
Return a list of the index kinds supported by the @italic{graph}.
}

@defproc[(graph-index
          [graph graph?]
          [index-kind graph-index-kind/c])
         (or/c (hash/c any/c (listof statement?)) #f)]{
Return the index maintained by @italic{graph}, of kind @italic{index-kind} if it is supported, else @racket[#f].
}

@defproc[(graph-index-create
          [graph graph?]
          [index-kind graph-index-kind/c])
         void?]{
Create a new index in @italic{graph}, of kind @italic{index-kind}, if one does not already exist.
}

@defproc[(graph-index-drop
          [graph graph?]
          [index-kind graph-index-kind/c])
         void?]{
Remove an index in @italic{graph}, of kind @italic{index-kind}, if it exist.
}

@;{============================================================================}
@subsection[]{Statement Members}

@defproc[(graph-member?
          [graph graph?]
          [statement statement?])
         boolean?]{
Return @racket[#t] if @italic{statement} exists in @italic{graph}.
}

@defproc[(graph-add
          [graph graph?]
          [statement statement?])
         graph?]{
TBD
}

@defproc[(graph-add-all
          [graph graph?]
          [statements statement-list?])
         graph?]{
TBD
}

@defproc[(graph-remove
          [graph graph?]
          [statement statement?])
         graph?]{
TBD
}

@defproc[(graph-remove-all
          [graph graph?]
          [statements statement-list?])
         graph?]{
TBD
}

@defproc[(graph-remove-all*
          [graph graph?]
          [statements statement-list?])
         graph?]{
TBD
}

@defproc[(graph-clear
          [graph graph?])
         graph?]{
Returns a copy of this graph with all statements removed. This does not affect the graph's name if set.
}

@;{============================================================================}
@subsection[]{Graph Filters}

@defproc[(graph-distinct-statements
          [graph graph?])
         (set/c statement?)]{
TBD
}

@defproc[(graph-distinct-subjects
          [graph graph?])
         (set/c subject?)]{
TBD
}

@defproc[(graph-distinct-predicates
          [graph graph?])
         (set/c predicate?)]{
TBD
}

@defproc[(graph-distinct-objects
          [graph graph?])
         (set/c object?)]{
TBD
}

@defproc[(graph-filter
          [proc (-> statement? boolean?)]
          [graph graph?])
         statement-list?]{
TBD
}

@defproc[(graph-filter-by-subject
          [graph graph?]
          [obj subject?])
         statement-list?]{
TBD
}

@defproc[(graph-filter-by-predicate
          [graph graph?]
          [obj predicate?])
         statement-list?]{
TBD
}

@defproc[(graph-filter-by-object
          [graph graph?]
          [obj object?])
         statement-list?]{
TBD
}

@;{============================================================================}
@subsection[]{Graph Operations}

Skolemization is the process of replacing all blank nodes in a graph with specially constructed IRIs. This allows
simpler graph processing in some cases as the processor only has to deal with one object identifier type. The
constructed IRIs follow the template below and this library uses UUIDs in string form as the unique identifier part.

@nested[#:style 'code-inset]{
  @tt{"https://{{domain}}/.well-known/skolem/{{unique-id}}"}
}

@defproc[(graph-skolemize
          [graph graph?]
          [domain-name string? "example.com"])
         graph?]{
Returns a new graph with all blank nodes in the original having been replaced with skolem IRIs.
}

@defproc[(graph-skolemize!
          [graph graph?]
          [domain-name string? "example.com"])
         graph?]{
The same as @racket[graph-skolemize] but performs the change to @racket[graph] directly and therefore the returned
graph is the same object as the graph passed in as the source.
}

@defproc[(skolem-url?
          [url url?])
         boolean?]{
Returns @racket[#t] is a skolem IRI using the well-known path in the template above.
}


@;{----------------------------------------------------------------------------}

@defthing[graph-tree/c contract?]{
TBD
}

@defproc[(graph->tree
          [graph graph?])
         graph-tree/c]{
TBD
}

@defproc[(statement-list->tree
          [statements statement-list?])
         graph-tree/c]{
TBD
}

@;{----------------------------------------------------------------------------}

@defproc[(describe-graph
          [graph graph?]
          [subject subject? #f])
         statement-list?]{
Returns a list of statements that correspond to a basic description of the graph using both the SPARQL service
description and VOID vocabularies.
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
@section[#:style '(toc)]{Module dataset}
@defmodule[rdf/core/dataset]

Package Description Here

@local-table-of-contents[]

@;{============================================================================}
@subsection[]{Dataset Type}

@defstruct*[dataset
            ([name (or/c resource? #f)]
             [(graphs #:mutable) (hash/c graph-name? graph?)])]{
TBD
}

@defproc[#:kind "constructor"
         (unnamed-dataset
          [graphs (hash/c graph-name? graph?)])
         dataset?]{
TBD
}

@defproc[#:kind "constructor"
         (named-dataset
          [name (or/c resource? #f)]
          [graphs (hash/c graph-name? graph?)])
         dataset?]{
TBD
}

@defproc[#:kind "constructor"
         (graph-list->dataset
          [graphs (listof graph?)])
         dataset?]{
TBD
}

@;{============================================================================}
@subsection[]{Dataset Predicates & Properties}

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

@defproc[(dataset-order
          [val graph?])
         exact-positive-integer?]{
TBD
}

@;{============================================================================}
@subsection[]{Graph Members}

@defproc[(dataset-has-named?
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

@;{============================================================================}
@subsection[]{Dataset Filtering}

@defproc[(dataset-distinct-subjects
          [val dataset?])
         (set/c subject?)]{
TBD
}

@defproc[(dataset-distinct-predicates
          [val dataset?])
         (set/c predicate?)]{
TBD
}

@defproc[(dataset-distinct-objects
          [val dataset?])
         (set/c object?)]{
TBD
}

@;{============================================================================}
@subsection[]{Dataset Operations}

@defproc[(describe-dataset
          [ds dataset?])
         statement-list?]{
TBD
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module gq}
@defmodule[rdf/core/gq]

Package Description Here


@;{============================================================================}
@subsection[]{Pattern Component}

@defproc[#:kind "predicate"
         (pattern-component?
          [val any/c])
         boolean?]{
TBD
}

@defproc[#:kind "constructor"
         (comparitor
          [value object?]
          [operator procedure? equal?])
         pattern-component?]{
TBD
}

@defproc[#:kind "constructor"
         (ignore)
         pattern-component?]{
TBD
}

@defproc[#:kind "constructor"
         (variable
          [name ncname?])
         pattern-component?]{
TBD


From @cite["SPARQL11QL"], section 19.8 @italic{Grammar}:

@nested[#:style 'code-inset]{
@verbatim|{
[164]    PN_CHARS_BASE  ::= [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6]
                          | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF]
                          | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF]
                          | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD]
                          | [#x10000-#xEFFFF]
[165]    PN_CHARS_U     ::= PN_CHARS_BASE | '_'
[166]    VARNAME        ::= ( PN_CHARS_U | [0-9] )
                            ( PN_CHARS_U | [0-9] | #x00B7 | [#x0300-#x036F]
                            | [#x203F-#x2040] )*
}|
}
}

@defproc[#:kind "predicate"
         (ignore? [val any/c]) boolean?]{
TBD
}

@defproc[#:kind "predicate"
         (comparitor? [val any/c]) boolean?]{
TBD
}

@defproc[#:kind "predicate"
         (variable-string? [val any/c]) boolean?]{
TBD
}

@defproc[#:kind "predicate"
         (variable-name-string? [val any/c]) boolean?]{
TBD
}

@defproc[#:kind "predicate"
         (variable? [val any/c]) boolean?]{
TBD
}

@;{============================================================================}
@subsection[]{Statement Pattern}

@defproc[#:kind "predicate"
         (statement-pattern? [val any/c]) boolean?]{
TBD
}

@defproc[#:kind "constructor"
         (make-statement-pattern
          [subject pattern-component?]
          [predicate pattern-component?]
          [object pattern-component?])
         statement-pattern?]{
TBD
}

@;{============================================================================}
@subsection[]{Pattern Matching}

@defproc[(graph-query
          [graph graph?]
          [patterns (listof statement-pattern?)])
         results?]{
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
         (result-variable-value? [val any/c]) boolean?]{
TBD
}

@defproc[#:kind "predicate"
         (result-statement? [val any/c]) boolean?]{
TBD
}

@defproc[#:kind "predicate"
         (results? [val any/c]) boolean?]{
TBD
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module io}
@defmodule[rdf/core/io]

This package provides very basic capabilities for writing out RDF values useful for testing but incomplete in many
regards.

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

From package @racket[rdf/core/name]:

@nested[#:style 'inset]{
  @itemlist[
    #:style compact-list
    @item{@racket[local-name?]}
    @item{@racket[string->local-name]}
    @item{@racket[local-name->string]}
  ]
}

From package @racket[rdf/core/namespace]:

@nested[#:style 'inset]{
  @itemlist[
    #:style compact-list
    @item{@racket[namespace?]}
    @item{@racket[url->namespace]}
    @item{@racket[namespace->url]}
    @item{@racket[nsname?]}
    @item{@racket[nsname]}
    @item{@racket[make-nsname]}
    @item{@racket[nsname->url]}
  ]
}

From package @racket[rdf/core/nsmap]:

@nested[#:style 'inset]{
  @itemlist[
    #:style compact-list
    @item{@racket[prefix?]}
    @item{@racket[string->prefix]}
    @item{@racket[empty-prefix]}
    @item{@racket[prefix->string]}
    @item{@racket[prefixed-name?]}
    @item{@racket[prefixed-name]}
    @item{@racket[string->prefixed-name]}
    @item{@racket[prefixed-name->string]}
    @item{@racket[nsmap?]}
    @item{@racket[nsmap-empty?]}
    @item{@racket[make-nsmap]}
    @item{@racket[nsmap-has-prefix?]}
    @item{@racket[nsmap-has-default?]}
    @item{@racket[nsmap-ref]}
    @item{@racket[nsmap-ref-default]}
    @item{@racket[nsmap-set!]}
  ]
}

From package @racket[rdf/core/literal]:

@nested[#:style 'inset]{
  @itemlist[
    #:style compact-list
    @item{@racket[literal?]}
    @item{@racket[literal-lexical-form]}
    @item{@racket[literal-datatype-iri]}
    @item{@racket[literal-language-tag]}
    @item{@racket[make-untyped-literal]}
    @item{@racket[make-typed-literal]}
    @item{@racket[make-lang-string-literal]}
  ]
}

From package @racket[rdf/core/statement]:

@nested[#:style 'inset]{
  @itemlist[
    #:style compact-list
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
}

From package @racket[rdf/core/triple]:

@nested[#:style 'inset]{
  @itemlist[
    #:style compact-list
    @item{@racket[make-triple]}
    @item{@racket[triple?]}
    @item{@racket[subject?]}
  ]
}

From package @racket[rdf/core/graph]:

@nested[#:style 'inset]{
  @itemlist[
    #:style compact-list
    @item{@racket[graph?]}
    @item{@racket[unnamed-graph]}
    @item{@racket[named-graph]}
    @item{@racket[graph-named?]}
    @item{@racket[graph-empty?]}
    @item{@racket[graph-member?]}
    @item{@racket[graph-add]}
    @item{@racket[graph-add-all]}
    @item{@racket[graph-remove]}
    @item{@racket[graph-remove-all]}
  ]
}

From package @racket[rdf/core/quad]:

@nested[#:style 'inset]{
  @itemlist[
    #:style compact-list
    @item{@racket[quad?]}
    @item{@racket[quad?]}
    @item{@racket[statement->quad]}
    @item{@racket[graph->quads]}
  ]
}

From package @racket[rdf/core/dataset]:

@nested[#:style 'inset]{
  @itemlist[
    #:style compact-list
    @item{@racket[dataset?]}
    @item{@racket[unnamed-dataset]}
    @item{@racket[named-dataset]}
    @item{@racket[dataset-empty?]}
    @item{@racket[dataset-has-named?]}
    @item{@racket[dataset-has-default?]}
    @item{@racket[dataset-ref]}
    @item{@racket[dataset-ref-default]}
    @item{@racket[dataset-set!]}
    @item{@racket[dataset-remove!]}
  ]
}

From package @racket[rdf/core/io]:

@nested[#:style 'inset]{
  @itemlist[
    #:style compact-list
    @item{@racket[write-ntriple-literal]}
    @item{@racket[write-ntriple-statement]}
    @item{@racket[write-ntriple-graph]}
    @item{@racket[write-nquad-statement]}
    @item{@racket[write-nquad-graph]}
  ]
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Vocabulary Modules}


These modules have a common, simple, naming convention:

@itemlist[
  #:style compact-list
  @item{each module comprises one, and only one, namespace,}
  @item{the module name is the same as the commonly-used prefix string,}
  @item{the module contains a single @racket[namespace] value with the same prefix as name bit with the suffix ":",}
  @item{the members of the namespace are individual @racket[name] values with names as qnames.}
]

See the @secref["Vocabulary_Tool" #:doc '(lib "rdf/core/scribblings/rdf-core.scrbl")], @tt{mkns}, that allows for
creation of modules for other vocabularies.

@local-table-of-contents[]

@;{============================================================================}
@subsection[]{RDF}
@defmodule[rdf/core/v/rdf]

@defthing[rdf-prefix-string prefix-string? #:value "rdf"]{The preferred, or common, prefix for this namespace as a @racket[string?].}

@defthing[rdf-namespace-string string?]{The IRI for this namespace as a @racket[string?].}

@defthing[rdf: namespace?]{The @racket[namespace] structure for this vocabulary.}

@defproc[(nsmap-add-rdf
          [map nsmap?])
         void?]{
Add this namespace, and preferred prefix, to the namespace map @racket[map].
}

@subsubsection[#:style '(non-toc)]{RDF Datatypes}

@defthing[rdf:HTML nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{rdf:HTML}.}
@defthing[rdf:langString nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{rdf:langString}.}
@defthing[rdf:XMLLiteral nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{rdf:XMLLiteral}.}

@subsubsection[#:style '(non-toc)]{RDF Classes}

@defthing[rdf:Alt nsname?]{The @racket[nsname?] structure corresponding to the class @tt{rdf:Alt}.}
@defthing[rdf:Bag nsname?]{The @racket[nsname?] structure corresponding to the class @tt{rdf:Bag}.}
@defthing[rdf:List nsname?]{The @racket[nsname?] structure corresponding to the class @tt{rdf:List}.}
@defthing[rdf:Property nsname?]{The @racket[nsname?] structure corresponding to the class @tt{rdf:Property}.}
@defthing[rdf:Seq nsname?]{The @racket[nsname?] structure corresponding to the class @tt{rdf:Seq}.}
@defthing[rdf:Statement nsname?]{The @racket[nsname?] structure corresponding to the class @tt{rdf:Statement}.}

@subsubsection[#:style '(non-toc)]{RDF Properties}

@defthing[fdf:first nsname?]{The @racket[nsname?] structure corresponding to the property @tt{rdf:first}.}
@defthing[rdf:nil nsname?]{The @racket[nsname?] structure corresponding to the property @tt{rdf:nil}.}
@defthing[rdf:object nsname?]{The @racket[nsname?] structure corresponding to the property @tt{rdf:object}.}
@defthing[rdf:predicate nsname?]{The @racket[nsname?] structure corresponding to the property @tt{rdf:predicate}.}
@defthing[rdf:rest nsname?]{The @racket[nsname?] structure corresponding to the property @tt{rdf:rest}.}
@defthing[rdf:subject nsname?]{The @racket[nsname?] structure corresponding to the property @tt{rdf:subject}.}
@defthing[rdf:type nsname?]{The @racket[nsname?] structure corresponding to the property @tt{rdf:type}.}
@defthing[rdf:value nsname?]{The @racket[nsname?] structure corresponding to the property @tt{rdf:value}.}

@;{============================================================================}
@subsection[]{RDF Schema}
@defmodule[rdf/core/v/rdfs]

@defthing[rdfs-prefix-string prefix-string? #:value "rdfs"]{The preferred, or common, prefix for this namespace as a @racket[string?].}

@defthing[rdfs-namespace-string string?]{The IRI for this namespace as a @racket[string?].}

@defthing[rdfs: namespace?]{The @racket[namespace] structure for this vocabulary.}

@defproc[(nsmap-add-rdf-schema
          [map nsmap?])
         void?]{
Add this namespace, and preferred prefix, to the namespace map @racket[map].
}

@subsubsection[#:style '(non-toc)]{RDFS Classes}

@defthing[rdfs:Class nsname?]{The @racket[nsname?] structure corresponding to the class @tt{:Class}.}
@defthing[rdfs:Container nsname?]{The @racket[nsname?] structure corresponding to the class @tt{:Container}.}
@defthing[rdfs:Container-Membership-Property nsname?]{The @racket[nsname?] structure corresponding to the class @tt{:Container-Membership-Property}.}
@defthing[rdfs:Datatype nsname?]{The @racket[nsname?] structure corresponding to the class @tt{:Datatype}.}
@defthing[rdfs:Literal nsname?]{The @racket[nsname?] structure corresponding to the class @tt{:Literal}.}
@defthing[rdfs:Resource nsname?]{The @racket[nsname?] structure corresponding to the class @tt{:Resource}.}

@subsubsection[#:style '(non-toc)]{RDFS Properties}

@defthing[rdfs:comment nsname?]{The @racket[nsname?] structure corresponding to the property @tt{:comment}.}
@defthing[rdfs:domain nsname?]{The @racket[nsname?] structure corresponding to the property @tt{:domain}.}
@defthing[rdfs:is-defined-by nsname?]{The @racket[nsname?] structure corresponding to the property @tt{:isDefinedBy}.}
@defthing[rdfs:label nsname?]{The @racket[nsname?] structure corresponding to the property @tt{:label}.}
@defthing[rdfs:member nsname?]{The @racket[nsname?] structure corresponding to the property @tt{:member}.}
@defthing[rdfs:range nsname?]{The @racket[nsname?] structure corresponding to the property @tt{:range}.}
@defthing[rdfs:see-also nsname?]{The @racket[nsname?] structure corresponding to the property @tt{:seeAlso}.}
@defthing[rdfs:sub-class-of nsname?]{The @racket[nsname?] structure corresponding to the property @tt{:sub-Class-Of}.}
@defthing[rdfs:sub-property-of nsname?]{The @racket[nsname?] structure corresponding to the property @tt{:sub-Property-Of}.}

@;{============================================================================}
@subsection[]{XML}
@defmodule[rdf/core/v/xml]

@defthing[xml-prefix-string prefix-string? #:value "xml"]{The preferred, or common, prefix for this namespace as a @racket[string?].}

@defthing[xml-namespace-string string?]{The IRI for this namespace as a @racket[string?].}

@defthing[xml: namespace?]{The @racket[namespace] structure for this vocabulary.}

@defproc[(nsmap-add-xml
          [map nsmap?])
         void?]{
Add this namespace, and preferred prefix, to the namespace map @racket[map].
}

@subsubsection[#:style '(non-toc)]{XML Properties}

@defthing[xml:base nsname?]{The @racket[nsname?] structure corresponding to the property @tt{xml:base}.}
@defthing[xml:lang nsname?]{The @racket[nsname?] structure corresponding to the property @tt{xml:lang}.}
@defthing[xml:space nsname?]{The @racket[nsname?] structure corresponding to the property @tt{xml:space}.}

@;{============================================================================}
@subsection[]{XML Schema Datatypes}
@defmodule[rdf/core/v/xsd]

@defthing[xsd-prefix-string prefix-string? #:value "xsd"]{The preferred, or common, prefix for this namespace as a @racket[string?].}

@defthing[xsd-namespace-string string?]{The IRI for this namespace as a @racket[string?].}

@defthing[xsd: namespace?]{The namespace structure for the XML Schema (Part 2 -- Datatypes) vocabulary.}

@defproc[(nsmap-add-xml-schema-datatypes
          [map nsmap?])
         void?]{
Add this namespace, and preferred prefix, to the namespace map @racket[map].
}

@subsubsection[#:style '(non-toc)]{XSD Classes}

@defthing[xsd:any-type nsname?]{The @racket[nsname?] structure corresponding to the type @tt{xsd:anyType}.}

@subsubsection[#:style '(non-toc)]{XSD Datatypes}

@defthing[xsd:any-simple-type nsname?]{The @racket[nsname?] structure corresponding to the type @tt{xsd:anySimpleType}.}
@defthing[xsd:any-uri nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:anyURI}.}
@defthing[xsd:base64-binary nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:base64Binary}.}
@defthing[xsd:boolean nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:boolean}.}
@defthing[xsd:byte nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:byte}.}
@defthing[xsd:date nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:date}.}
@defthing[xsd:date-time nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:dateTime}.}
@defthing[xsd:decimal nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:decimal}.}
@defthing[xsd:double nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:double}.}
@defthing[xsd:duration nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:duration}.}
@defthing[xsd:entity nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:ENTITY}.}
@defthing[xsd:float nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:float}.}
@defthing[xsd:g-day nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:gDay}.}
@defthing[xsd:g-month nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:gMonth}.}
@defthing[xsd:g-month-day nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:gMonthDay}.}
@defthing[xsd:g-year nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:gYear}.}
@defthing[xsd:g-year-month nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:gYearMonth}.}
@defthing[xsd:hex-binary nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:hexBinary}.}
@defthing[xsd:id nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:ID}.}
@defthing[xsd:id-ref nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:IDREF}.}
@defthing[xsd:int nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:int}.}
@defthing[xsd:integer nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:integer}.}
@defthing[xsd:language nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:language}.}
@defthing[xsd:long nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:long}.}
@defthing[xsd:name nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:Name}.}
@defthing[xsd:ncname nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:NCNAME}.}
@defthing[xsd:negative-integer nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:negativeInteger}.}
@defthing[xsd:nmtoken nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:NMTOKEN}.}
@defthing[xsd:non-negative-integer nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:nonNegativeInteger}.}
@defthing[xsd:non-positive-integer nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:nonPositiveInteger}.}
@defthing[xsd:normalized-string nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:normalizedString}.}
@defthing[xsd:positive-integer nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:positiveInteger}.}
@defthing[xsd:q-name nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:QName}.}
@defthing[xsd:q-notation nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:QNotation}.}
@defthing[xsd:short nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:short}.}
@defthing[xsd:string nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:string}.}
@defthing[xsd:time nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:time}.}
@defthing[xsd:token nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:token}.}
@defthing[xsd:unsigned-byte nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:unsignedByte}.}
@defthing[xsd:unsigned-int nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:unsignedInt}.}
@defthing[xsd:unsigned-long nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:unsignedLong}.}
@defthing[xsd:unsigned-short nsname?]{The @racket[nsname?] structure corresponding to the datatype @tt{xsd:unsignedShort}.}

@subsubsection[#:style '(non-toc)]{XSD Properties}

@defthing[xsd:enumeration nsname?]{The @racket[nsname?] structure corresponding to the property @tt{xsd:enumeration}.}
@defthing[xsd:fraction-digits nsname?]{The @racket[nsname?] structure corresponding to the property @tt{xsd:fractionDigits}.}
@defthing[xsd:length nsname?]{The @racket[nsname?] structure corresponding to the property @tt{xsd:length}.}
@defthing[xsd:max-exclusive nsname?]{The @racket[nsname?] structure corresponding to the property @tt{xsd:maxExclusive}.}
@defthing[xsd:max-inclusive nsname?]{The @racket[nsname?] structure corresponding to the property @tt{xsd:maxInclusive}.}
@defthing[xsd:max-length nsname?]{The @racket[nsname?] structure corresponding to the property @tt{xsd:maxLength}.}
@defthing[xsd:min-exclusive nsname?]{The @racket[nsname?] structure corresponding to the property @tt{xsd:minExclusive}.}
@defthing[xsd:min-inclusive nsname?]{The @racket[nsname?] structure corresponding to the property @tt{xsd:minInclusive}.}
@defthing[xsd:min-length nsname?]{The @racket[nsname?] structure corresponding to the property @tt{xsd:minLength}.}
@defthing[xsd:pattern nsname?]{The @racket[nsname?] structure corresponding to the property @tt{xsd:pattern}.}
@defthing[xsd:total-digits nsname?]{The @racket[nsname?] structure corresponding to the property @tt{xsd:totalDigits}.}
@defthing[xsd:white-space nsname?]{The @racket[nsname?] structure corresponding to the property @tt{xsd:whiteSpace}.}

@;{============================================================================}
@subsection[]{Vocabulary of Interlinked Datasets}
@defmodule[rdf/core/v/void]

@defthing[void-prefix-string prefix-string? #:value "void"]{The preferred, or common, prefix for this namespace as a @racket[string?].}

@defthing[void-namespace-string string?]{The IRI for this namespace as a @racket[string?].}

@defthing[void: namespace?]{The @racket[namespace] structure for this vocabulary.}

@defproc[(nsmap-add-void
          [map nsmap?])
         void?]{
Add this namespace, and preferred prefix, to the namespace map @racket[map].
}

@subsubsection[#:style '(non-toc)]{VOID Classes}

@defthing[void:Dataset nsname?]{The @racket[nsname?] structure corresponding to the class @tt{void:Dataset}. A set of RDF triples that are published, maintained or aggregated by a single provider.}
@defthing[void:Dataset-Description nsname?]{The @racket[nsname?] structure corresponding to the class @tt{void:DatasetDescription}. A web resource whose @tt{foaf:primaryTopic} or @tt{foaf:topics} include @racket[void:Dataset]s.}
@defthing[void:Linkset nsname?]{The @racket[nsname?] structure corresponding to the class @tt{void:Linkset}. 	A collection of RDF links between two @racket[void:Dataset]s.}
@defthing[void:Technical-Feature nsname?]{The @racket[nsname?] structure corresponding to the class @tt{void:TechnicalFeature}. A technical feature of a @racket[void:Dataset], such as a supported RDF serialization format.}

@subsubsection[#:style '(non-toc)]{VOID Properties}

@defthing[void:class nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:class}. The @racket[rdfs:Class] that is the @racket[rdf:type] of all entities in a class-based partition.}
@defthing[void:class-partition nsname?]{The @racket[local-ame] structure corresponding to the property @tt{void:classPartition}. A subset of a @racket[void:Dataset] that contains only the entities of a certain @racket[rdfs:Class].}
@defthing[void:classes nlocal-ame?]{The @racket[nsname?] structure corresponding to the property @tt{void:classes}. The total number of distinct classes in a @racket[void:Dataset].}
@defthing[void:data-dump nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:dataDump}. An RDF dump, partial or complete, of a @racket[void:Dataset].}
@defthing[void:distinct-objects nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:distinctObjects}. The total number of distinct objects in a @racket[void:Dataset].}
@defthing[void:distinct-subjects nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:distinctSubjects}. The total number of distinct subjects in a @racket[void:Dataset].}
@defthing[void:documents nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:documents}. The total number of documents, for @racket[void:Dataset]s that are published as a set of individual RDF documents.}
@defthing[void:entities nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:entities}. The total number of entities that are described in a @racket[void:Dataset].}
@defthing[void:example-resource nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:exampleResource}. An example entity that is representative for the entities described in a @racket[void:Dataset].}
@defthing[void:feature nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:feature}. A @racket[void:TechnicalFeature] supported by a @racket[void:Datset].}
@defthing[void:in-dataset nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:inDataset}. Points to the @racket[void:Dataset] that a document is a part of.}
@defthing[void:link-predicate nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:linkPredicate}. Specifies the RDF property of the triples in a @racket[void:Linkset].}
@defthing[void:objects-target nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:objectsTarget}. The @racket[void:Dataset] that contains the resources in the object position of a @racket[void:Linkset]'s triples.}
@defthing[void:opensearch-description nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:openSearchDescription}. An OpenSearch description document for a free-text search service over a @racket[void:Dataset].}
@defthing[void:properties nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:properties}. The total number of distinct properties in a @racket[void:Dataset].}
@defthing[void:property nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:property}. The @racket[rdf:Property] that is the predicate of all triples in a property-based partition.}
@defthing[void:property-partition nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:propertyPartition}. A subset of a @racket[void:Dataset] that contains only the triples of a certain @racket[rdf:Property].}
@defthing[void:root-resource nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:rootResource}. A top concept or entry point for a @racket[void:Dataset] that is structured in a tree-like fashion.}
@defthing[void:sparql-endpoint nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:sparqlEndpoint}. A SPARQL protocol endpoint that allows SPARQL query access to a @racket[void:Dataset].}
@defthing[void:subjects-target nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:subjectsTarget}. The @racket[void:Dataset] that contains the resources in the subject position of this @racket[void:Linkset]'s triples.}
@defthing[void:subset nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:subset}. A @racket[void:Dataset] that is part of another @racket[void:Dataset].}
@defthing[void:target nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:target}. One of the two @racket[void:Datasets] connected by this @racket[void:Linkset].}
@defthing[void:triples nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:triples}. The total number of triples contained in a @racket[void:Dataset].}
@defthing[void:uri-lookup-endpoint nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:uriLookupEndpoint}. A protocol endpoint for simple URI lookups for a @racket[void:Dataset].}
@defthing[void:uri-regex-pattern nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:uriRegexPattern}. A regular expression that matches the URIs of a @racket[void:Dataset]'s entities.}
@defthing[void:uri-space nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:uriSpace}. A URI that is a common string prefix of all the entity URIs in a @racket[void:Datset].}
@defthing[void:vocabulary nsname?]{The @racket[nsname?] structure corresponding to the property @tt{void:vocabulary}. A vocabulary or @racket[owl:Ontology] whose classes or properties are used in a @racket[void:Dataset].}


@;{============================================================================}
@subsection[]{SPARQL Service Description}
@defmodule[rdf/core/v/sd]

@defthing[sd-prefix-string prefix-string? #:value "sd"]{The preferred, or common, prefix for this namespace as a @racket[string?].}

@defthing[sd-namespace-string string?]{The IRI for this namespace as a @racket[string?].}

@defthing[sd: namespace?]{The @racket[namespace] structure for this vocabulary.}

@defproc[(nsmap-add-sd
          [map nsmap?])
         void?]{
Add this namespace, and preferred prefix, to the namespace map @racket[map].
}

@subsubsection[#:style '(non-toc)]{SD Classes}

@defthing[sd:Dataset nsname?]{The @racket[nsname?] structure corresponding to the class @tt{sd:Dataset}.}
@defthing[sd:Graph name?]{The @racket[nsname?] structure corresponding to the class @tt{sd:Graph}.}
@defthing[sd:GraphCollection nsname?]{The @racket[nsname?] structure corresponding to the class @tt{sd:GraphCollection}.}
@defthing[sd:NamedGraph nsname?]{The @racket[nsname?] structure corresponding to the class @tt{sd:NamedGraph}.}

@subsubsection[#:style '(non-toc)]{SD Properties}

@defthing[sd:defaultDataset nsname?]{The @racket[nsname?] structure corresponding to the property @tt{sd:defaultDataset}.}
@defthing[sd:defaultGraph nsname?]{The @racket[nsname?] structure corresponding to the property @tt{sd:defaultGraph}.}
@defthing[sd:graph nsname?]{The @racket[nsname?] structure corresponding to the property @tt{sd:graph}.}
@defthing[sd:name nsname?]{The @racket[nsname?] structure corresponding to the property @tt{sd:name}.}
@defthing[sd:namedGraph nsname?]{@racket[nsname?] name structure corresponding to the property @tt{sd:namedGraph}.}


@;{============================================================================}
@subsection[]{Vocabulary Tool}

This package also includes a command-line tool @tt{mkns} that will generate a module in the same style as those above
from a set of command-line parameters.

@nested[#:style 'code-inset]{
@verbatim|{
❯ mkns -h
usage: mkns [ <option> ... ] <uri> <prefix>

  Create a new Racket module for an RDF vocabulary.

<option> is one of

  -n <name>, --name <name>
     This vocabulary's name
  -d <text>, --description <text>
     A description of this vocabulary
  -s <uri>, --spec-uri <uri>
     A Specification URI describing this vocabulary
  -p <date>, --spec-published <date>
     The publication date of the vocabulary specification
  -w <width>, --width <width>
     The width of the output written (default: 100)
/ -o <racket-file>, --output-file <racket-file>
|    The name of a file to write output to (default: stdout)
| -r <module>, --output-racket-module <module>
|    The name of a module to write output to
| -u, --use-prefix-name
\    Use the namespace prefix to determine the module name
* -m <member-names>, --member <member-names>
     The name of a member, or list of members, within this vocabulary
  --help, -h
     Show this help
  --
     Do not treat any remaining argument as a switch (at this level)

 *   Asterisks indicate options allowed multiple times.
 /|\ Brackets indicate mutually exclusive options.

 Multiple single-letter switches can be combined after
 one `-`. For example, `-h-` is the same as `-h --`.
}|
}

The only required properties are the URI and prefix for the namespace, which generates a very @italic{bare-bones}
module, as shown below.

@nested[#:style 'code-inset]{
@verbatim|{
❯ mkns http://example.org/schema/example# ex
#lang racket/base
;;
;; Status: not set
;;

(require (only-in rdf/core/namespace
                  make-namespace
                  make-name))

(provide (all-defined-out))

;; ==============================================================
;; Namespace definition
;; ==============================================================

(define ex:
  (make-namespace "http://example.org/schema/example#"
                  "ex"))

;; ==============================================================
;; Name Definitions
;; ==============================================================
}|
}

A more complete example specifies a name, description, a specification URI and date, as well as two member names to be
added to the namespace.

@nested[#:style 'code-inset]{
@verbatim|{
❯ mkns -n "An example namespace" \
       -d "Using the IETF example domain name, example.org, …" \
       -s https://www.rfc-editor.org/rfc/rfc2606.html \
       -p 1999-06 \
       -m ExampleClass \
       -m exampleProperty \
       -w 65 \
       http://example.org/schema/example# ex
#lang racket/base
;;
;; Name: An example namespace
;;
;; Using the IETF example domain name, example.org, this adds a
;; simple path which can be used to document RDF tools.
;;
;; Specification URI: https://www.rfc-editor.org/rfc/rfc2606.html
;;
;; Specification Date: 1999-06
;;
;; Status: not set
;;

(require (only-in rdf/core/namespace
                  make-namespace
                  make-name))

(provide (all-defined-out))

;; ==============================================================
;; Namespace definition
;; ==============================================================

(define ex:
  (make-namespace "http://example.org/schema/example#"
                  "ex"))

;; ==============================================================
;; Name Definitions
;; ==============================================================

(define ex:exampleProperty (make-name ex: "exampleProperty"))
(define ex:ExampleClass (make-name ex: "ExampleClass"))
}|
}


@;{============================================================================}
@;{============================================================================}
@section[]{Appendix: Names Defined}

Starting with the productions from @cite["SPARQL11QL"] section 19.8 @italic{Grammar}:

@nested[#:style 'code-inset]{
@verbatim|{
[6]    PrefixDecl      ::= 'PREFIX' PNAME_NS IRIREF

[136]  iri             ::= IRIREF | PrefixedName
[137]  PrefixedName    ::= PNAME_LN | PNAME_NS

[140]  PNAME_NS        ::= PN_PREFIX? ':'
[141]  PNAME_LN        ::= PNAME_NS PN_LOCAL

[164]  PN_CHARS_BASE   ::= [A-Z] | [a-z] | [#x00C0-#x00D6]
                         | [#x00D8-#x00F6] | [#x00F8-#x02FF]
                         | [#x0370-#x037D] | [#x037F-#x1FFF]
                         | [#x200C-#x200D] | [#x2070-#x218F]
                         | [#x2C00-#x2FEF] | [#x3001-#xD7FF]
                         | [#xF900-#xFDCF] | [#xFDF0-#xFFFD]
                         | [#x10000-#xEFFFF]
[165]  PN_CHARS_U      ::= PN_CHARS_BASE | '_'

[167]  PN_CHARS        ::= PN_CHARS_U | '-' | [0-9] | #x00B7
                         | [#x0300-#x036F] | [#x203F-#x2040]
[168]  PN_PREFIX       ::= PN_CHARS_BASE
                           ( ( PN_CHARS | '.' )* PN_CHARS )?
[169]  PN_LOCAL        ::= ( PN_CHARS_U | ':' | [0-9] | PLX )
                           ( ( PN_CHARS | '.' | ':' | PLX )*
                             ( PN_CHARS | ':' | PLX ) )?
[170]  PLX             ::= PERCENT | PN_LOCAL_ESC
[171]  PERCENT         ::= '%' HEX HEX
[172]  HEX             ::= [0-9] | [A-F] | [a-f]
[173]  PN_LOCAL_ESC    ::= '\' ( '_' | '~' | '.' | '-' | '!' | '$'
                               | '&' | "'" | '(' | ')' | '*' | '+'
                               | ',' | ';' | '=' | '/' | '?' | '#'
                               | '@' | '%' )
}|
}

As well as section 19.5 @italic{IRI References}:

@nested[#:style 'inset]{
Text matched by the @tt{IRIREF} production and @tt{PrefixedName} (after prefix expansion) production, after escape
processing, must conform to the generic syntax of IRI references in section 2.2 of RFC 3987 "ABNF for IRI References and
IRIs". For example, the @tt{IRIREF} @tt{<abc#def>} may occur in a SPARQL query string, but the @tt{IRIREF}
@tt{<abc##def>} must not.
}

This package has the following type mapping:

@tabular[
#:style 'boxed
#:sep @hspace[2]
#:row-properties '(bottom-border ())
#:column-properties '(top top)
(list (list @bold{Racket type}      @bold{SPARQL production})
      (list @racket[prefix-name?]   @tt{PNAME_NS})
      (list @racket[local-name?]    @tt{PN_LOCAL})
      (list @racket[prefixed-name?] @tt{PrefixedName}))
]

As well as the following string predicates:

@tabular[
#:style 'boxed
#:sep @hspace[2]
#:row-properties '(bottom-border ())
#:column-properties '(top top)
(list (list @bold{Racket type}             @bold{SPARQL production})
      (list @racket[prefix-string?]        @tt{PNAME_NS})
      (list @racket[prefix-name-string?]   @tt{PN_PREFIX})
      (list @racket[local-name-string?]    @tt{PN_LOCAL})
      (list @racket[prefixed-name-string?] @tt{PrefixedName}))
]

@subsection[]{Turtle}

The Turtle grammar, @cite["RDF11TTL"] section 6.5 @italic{Grammar} reuses the definitions in SPARQL. Note that in the
rule number @tt{[6s]} the suffix "s" denotes this specifically as a rule borrowed directly from SPARQL.

@nested[#:style 'code-inset]{
@verbatim|{
[4]     prefixID       ::= '@prefix' PNAME_NS IRIREF '.'

[6s]    sparqlPrefix   ::= "PREFIX" PNAME_NS IRIREF
}|
}

Also, section 7.2 @italic{RDF Term Constructors} contains a table, a subset of which is below.

@tabular[
#:style 'boxed
#:sep @hspace[2]
#:row-properties '(bottom-border ())
#:column-properties '(top top top)
(list (list @bold{production} @bold{type} @bold{procedure})
      (list @tt{PNAME_NS}
            @tt{prefix}
            @para{When used in a @tt{prefixID} or @tt{sparqlPrefix} production, the prefix is the potentially empty
                       unicode string matching the first argument of the rule is a key into the namespaces map.})
      (list @tt{PNAME_NS}
            @tt{IRI}
            @para{When used in a @tt{PrefixedName} production, the IRI is the value in the namespaces map corresponding
                       to the first argument of the rule.})
      (list  @tt{PNAME_LN}
             @tt{IRI}
             @para{A potentially empty prefix is identified by the first sequence, @tt{PNAME_NS}. The namespaces map
                     must have a corresponding namespace. The unicode string of the IRI is formed by unescaping the
                     reserved characters in the second argument, @tt{PN_LOCAL}, and concatenating this onto the
                     namespace.}))
]

Finally, @cite["RDF11TTL"] section 2.4 IRIs, includes

@nested[#:style 'inset]{
Prefixed names are a superset of XML QNames. They differ in that the local part of prefixed names may include:

@itemlist[
  @item{leading digits, e.g. @tt{leg:3032571} or @tt{isbn13:9780136019701}}
  @item{non leading colons, e.g. @tt{og:video:height}}
  @item{reserved character escape sequences, e.g. @tt{wgs:lat\-long}}
]
}

@subsection[]{RDF/XML}

The specification RDF 1.1 Syntax @cite["RDF11XML"] (commonly referred to as RDF/XML) is constrained by it's use of XML
to use the type @tt{NCName} (see @cite["XMLNAMES"], section 3, Declaring Namespaces) for both prefix and local names.
However, the value space of this type is smaller than the names above and the specification's @cite["RDF11XML"],
Section 8 -- Serializing an RDF Graph to RDF/XML, states:

@nested[#:style 'inset]{
There are some RDF Graphs as defined in @cite["RDF11CAS"] that cannot be serialized in RDF/XML. These are those that:

@bold{Use property names that cannot be turned into XML namespace-qualified names.}

@nested[#:style 'inset]{
An XML namespace-qualified name (QName) has restrictions on the legal characters
such that not all property URIs can be expressed as these names. It is recommended
that implementors of RDF serializers, in order to break a URI into a namespace name
and a local name, split it after the last XML non-NCName character, ensuring that
the first character of the name is a Letter or '_'. If the URI ends in a non-NCName
character then throw a "this graph cannot be serialized in RDF/XML" exception or error.
}

@bold{Use inappropriate reserved names as properties}

@nested[#:style 'inset]{
For example, a property with the same URI as any of the syntaxTerms production.
}

@bold{Use the @tt{rdf:HTML} datatype}

@nested[#:style 'inset]{
This datatype as introduced in RDF 1.1 @cite["RDF11CAS"].
}
}


@;{============================================================================}
@;{============================================================================}

@(bibliography

  (bib-entry #:key "CURIE"
             #:title "CURIE Syntax 1.0 -- A syntax for expressing Compact URIs"
             #:author "M. Birbeck, and S. McCarron"
             #:location "W3C"
             #:url "https://www.w3.org/TR/curie/"
             #:date "16 December 2010")

  (bib-entry #:key "OWL2"
             #:title "OWL 2 Web Ontology Language Document Overview (Second Edition)"
             #:location "W3C"
             #:url "https://www.w3.org/TR/owl2-overview/"
             #:date "11 December 2012")

  (bib-entry #:key "RDF11CAS"
             #:title "RDF 1.1 Concepts and Abstract Syntax"
             #:author "G. Klyne, J. J. Carroll, and B. McBride"
             #:location "W3C"
             #:url "https://www.w3.org/TR/rdf11-concepts/"
             #:date "25 February 2014")

  (bib-entry #:key "RDF11DS"
             #:title "RDF 1.1: On Semantics of RDF Datasets"
             #:author "A. Zimmermann"
             #:location "W3C"
             #:url "https://www.w3.org/TR/rdf11-datasets/"
             #:date "25 February 2014")

  (bib-entry #:key "RDF11MT"
             #:title "RDF 1.1 Semantics"
             #:author "P. J. Hayes, and P. F. Patel-Schneider"
             #:location "W3C"
             #:url "https://www.w3.org/TR/rdf11-mt/"
             #:date "25 February 2014")

  (bib-entry #:key "RDF11NQ"
             #:title "RDF 1.1 N-Quads -- A line-based syntax for RDF datasets"
             #:author "G. Carothers"
             #:location "W3C"
             #:url "https://www.w3.org/TR/n-quads/"
             #:date "25 February 2014")

  (bib-entry #:key "RDF11NT"
             #:title "RDF 1.1 N-Triples -- A line-based syntax for an RDF graph"
             #:author "P. J. Hayes, and P. F. Patel-Schneider"
             #:location "W3C"
             #:url "https://www.w3.org/TR/n-triples/"
             #:date "25 February 2014")

  (bib-entry #:key "RDF11SCHEMA"
             #:title "RDF Schema 1.1"
             #:author "D. Brickley, and . V. Guha"
             #:location "W3C"
             #:url "https://www.w3.org/TR/rdf11-schema/"
             #:date "25 February 2014")

  (bib-entry #:key "RDF11TRIG"
             #:title "RDF 1.1 TriG: RDF Dataset Language"
             #:author "G. Carothers, A. Seaborne, C. Bizer, and R. Cyganiak"
             #:location "W3C"
             #:url "https://www.w3.org/TR/trig/"
             #:date "25 February 2014")

  (bib-entry #:key "RDF11TTL"
             #:title "RDF 1.1 Turtle -- Terse RDF Triple Language"
             #:author "D. Beckett, T. Berners-Lee, E. Prud'hommeaux, and . Carothers"
             #:location "W3C"
             #:url "https://www.w3.org/TR/turtle/"
             #:date "25 February 2014")

  (bib-entry #:key "RDF11XML"
             #:title "RDF 1.1 Syntax"
             #:author "F. Gandon, and G. Schreiber"
             #:location "W3C"
             #:url "https://www.w3.org/TR/rdf-syntax-grammar/"
             #:date "25 February 2014")

  (bib-entry #:key "RFC3986"
             #:title "Uniform Resource Identifier (URI): Generic Syntax"
             #:author "T. Berners-Lee, R. Fielding, and L. Masinter"
             #:location "RFC"
             #:url "http://www.ietf.org/rfc/rfc3986.txt"
             #:date "January 2005")

  (bib-entry #:key "RFC3987"
             #:title "Internationalized Resource Identifiers (IRIs)"
             #:author "M. Duerst, and M. Suignard"
             #:location "RFC"
             #:url "http://www.ietf.org/rfc/rfc3987.txt"
             #:date "January 2005")

  (bib-entry #:key "RFC5646"
             #:title "Tags for Identifying Languages"
             #:author "A. Phillips, and M. Davis"
             #:location "RFC"
             #:url "http://www.ietf.org/rfc/rfc5646.txt"
             #:date "September 2009")

  (bib-entry #:key "SPARQL11OV"
             #:title "SPARQL 1.1 Overview"
             #:author "The W3C SPARQL Working Group"
             #:location "W3C"
             #:url "https://www.w3.org/TR/sparql11-overview/"
             #:date "21 March 2013")

  (bib-entry #:key "SPARQL11QL"
             #:title "SPARQL 1.1 Query Language"
             #:author "S. Harris, and A. Seaborne"
             #:location "W3C"
             #:url "https://www.w3.org/TR/sparql11-query/"
             #:date "21 March 2013")

  (bib-entry #:key "SPARQL11SD"
             #:title "SPARQL 1.1 Service Description"
             #:author "G. T. Williams"
             #:location "W3C"
             #:url "https://www.w3.org/TR/sparql11-service-description/"
             #:date "21 March 2013")

  (bib-entry #:key "SWBPXSD"
             #:title "XML Schema Datatypes in RDF and OWL"
             #:author "J. J. Carroll, and J. Z. Pan"
             #:location "W3C"
             #:url "https://www.w3.org/TR/swbp-xsch-datatypes/"
             #:date "14 March 2006")

  (bib-entry #:key "XML11"
             #:title "Extensible Markup Language (XML) 1.1 (Second Edition)"
             #:author "T. Bray, J. Paoli, C.M. Sperberg-McQueen, F Yergeau, and J. Cowan"
             #:location "W3C"
             #:url "https://www.w3.org/TR/xml11/"
             #:date "29 September 2006")

  (bib-entry #:key "XMLBASE"
             #:title "XML Base (Second Edition)"
             #:author "J Marsh, and R. Tobin"
             #:location "W3C"
             #:url "https://www.w3.org/TR/xmlbase/"
             #:date "28 January 2009")

  (bib-entry #:key "XMLNAMES"
             #:title "Namespaces in XML 1.1 (Second Edition)"
             #:author "T. Bray, D. Hollander, A. Layman, and R. Tobin"
             #:location "W3C"
             #:url "https://www.w3.org/TR/xml-names11/"
             #:date "16 August 2006")

  (bib-entry #:key "XMLXSD2"
             #:title "XML Schema Part 2: Datatypes Second Edition"
             #:author "P. V. Biron, and A. Malhotra"
             #:location "W3C"
             #:url "https://www.w3.org/TR/xmlschema-2/"
             #:date "28 October 2004")
)

@;{============================================================================}
@;{============================================================================}

@index-section[]
