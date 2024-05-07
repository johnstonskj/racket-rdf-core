# -*- mode: makefile-gmake -*-

# Core names, package and collection(s)
PACKAGENAME=rdf-core
COLLECTS=rdf/core

# The root for all directly generated files
BUILDOUTDIR=$(PWD)/build

# ==============================================================================
# The default target
# ==============================================================================
.PHONY: all
all: setup test

$(BUILDOUTDIR):
	mkdir -p $(BUILDOUTDIR)

# ==============================================================================
# Packaging targets
# ==============================================================================
PACKAGEFILE=$(BUILDOUTDIR)/$(PACKAGENAME).zip
CHECKSUMFILE=$(PACKAGEFILE).CHECKSUM

.PHONY: setup link unlink package

setup: copycss copyimages
	raco setup \
		--tidy \
		--check-pkg-deps \
		--unused-pkg-deps \
		--only $(COLLECTS)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

link:
	raco pkg install \
		--link \
		--auto \
		--name $(PACKAGENAME) \
		$(PWD)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

unlink:
	raco pkg remove $(PACKAGENAME)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

package: check setup test $(BUILDOUTDIR)
	raco pkg create \
		--built \
		--dest $(BUILDOUTDIR) \
		$(PWD)

# ==============================================================================
# Tests, and related targets
# ==============================================================================
TESTDIR=$(COLLECTS)/tests
COVEROUTDIR=$(BUILDOUTDIR)/coverage

.PHONY: test check

test:
	raco test \
		--make \
		--table \
		--collection $(COLLECTS)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

coverage: $(COVEROUTDIR)
	raco cover \
		--exclude-pkg-basics \
		--format html \
		--directory $(COVEROUTDIR) \
		--package $(PACKAGENAME)

$(COVEROUTDIR):
	mkdir -p $(COVEROUTDIR)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

check:
	fd . -t f -e rkt -x raco check-requires {} \;

# ==============================================================================
# Documentation generation targets
# ==============================================================================
SCRBLDIR=$(COLLECTS)/scribblings
SCRBLSRC=$(SCRBLDIR)/$(PACKAGENAME).scrbl

HTMLOUTDIR=$(BUILDOUTDIR)/html
PDFOUTFILE=$(BUILDOUTDIR)/$(PACKAGENAME).pdf
MARKDOWNOUTFILE=$(BUILDOUTDIR)/$(PACKAGENAME).md

.PHONY: doc viewdocs copycss

doc: $(PDFOUTFILE) $(MARKDOWNOUTFILE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

$(PDFOUTFILE): $(SCRBLSRC) $(BUILDOUTDIR)
	raco scribble \
		--make \
		--xelatex \
		--dest $(BUILDOUTDIR) \
		--redirect-main http://docs.racket-lang.org/ \
		$(SCRBLSRC)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

$(MARKDOWNOUTFILE): $(SCRBLSRC) $(BUILDOUTDIR)
	raco scribble \
		--make \
		--markdown \
		--dest $(BUILDOUTDIR) \
		--redirect-main http://docs.racket-lang.org/ \
		$(SCRBLSRC)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

viewdocs:
	raco docs

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

HTMLDOCOUTDIR = $(COLLECTS)/doc/$(PACKAGENAME)
CSSSRC := $(wildcard $(SCRBLDIR)/*.css)
CSSTGT := $(addprefix $(HTMLDOCOUTDIR)/,$(notdir $(CSSSRC)))

copycss: $(CSSTGT)

$(HTMLDOCOUTDIR)/%.css: $(SCRBLDIR)/%.css | $(HTMLDOCOUTDIR)
	cp $< $@

$(HTMLDOCOUTDIR):
	mkdir -p $(HTMLDOCOUTDIR)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

IMGSRC := $(wildcard $(SCRBLDIR)/*.svg)
IMGTGT := $(addprefix $(HTMLDOCOUTDIR)/,$(notdir $(IMGSRC)))

copyimages: $(IMGTGT)

$(HTMLDOCOUTDIR)/%.svg: $(SCRBLDIR)/%.svg | $(HTMLDOCOUTDIR)
	cp $< $@

# ==============================================================================
# Clean all temporary artifacts
# ==============================================================================
.PHONY: clean

clean:
	find . -name compiled -type d | xargs rm -rf
	rm -rf $(BUILDOUTDIR)
	rm -rf $(COLLECTS)/doc
