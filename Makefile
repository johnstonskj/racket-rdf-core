PACKAGENAME=rdf-core
PACKAGEFILE=$(PACKAGENAME).zip
COLLECTS=rdf/core
TESTS=$(COLLECTS)/tests
SCRBL=$(COLLECTS)/scribblings/rdf-core.scrbl
DOCDIR=$(COLLECTS)/doc

all: setup test

clean:
	find . -name compiled -type d | xargs rm -rf
	rm -rf $(DOCDIR)
	rm $(PACKAGEFILE)
	rm $(PACKAGEFILE).CHECKSUM

setup:
	raco setup --tidy $(COLLECTS)

link:
	raco pkg install --link -n $(COLLECTS) $$(pwd)

unlink:
	raco pkg remove $(COLLECTS)

package:
	raco pkg create $(COLLECTS)

test:
	raco test $(TESTS)

coverage:
	raco cover -b -f coveralls -p $(COLLECTS)

htmldocs: $(SCRBL)
	raco scribble --html --dest $(DOCDIR) --dest-name index ++main-xref-in --redirect-main http://docs.racket-lang.org/ $(SCRBL)

viewdocs:
	raco docs
