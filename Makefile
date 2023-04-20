# Makefile,v
# Copyright (c) INRIA 2007-2017

TOP=.
include $(TOP)/config/Makefile.top

WD=$(shell pwd)
DESTDIR=

SYSDIRS= pattern_parsetree.$(ocamlVERSION) helpers runtime quotations
OTHERCLEANDIRS=\
	adjusted-parsing.4.10.2 adjusted-parsing.4.11.2 adjusted-parsing.4.12.1 adjusted-parsing.4.13.1 adjusted-parsing.4.14.0 adjusted-parsing.5.0.0 \
	pattern_parsetree.4.10.2 pattern_parsetree.4.11.2 pattern_parsetree.4.12.1 pattern_parsetree.4.13.1 pattern_parsetree.4.14.0 pattern_parsetree.5.0.0 \

TESTDIRS= tests

all: sys
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) all; cd ..; done

sys:
	set -e; for i in $(SYSDIRS); do cd $$i; $(MAKE) all; cd ..; done

test: all mdx-test
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) test; cd ..; done

mdx-test:: README.asciidoc.TEST

META: sys
	$(JOINMETA) \
		-rewrite pa_ppx_parsetree_pattern_parsetree:pa_ppx_parsetree.pattern_parsetree \
		-wrap-subdir pattern_parsetree:pattern_parsetree.$(ocamlVERSION) \
		-rewrite pa_ppx_parsetree_official_paretree:pa_ppx_parsetree.helpers \
		-wrap-subdir helpers:helpers \
		-rewrite pa_ppx_parsetree_quotations:pa_ppx_parsetree.quotations \
		-wrap-subdir quotations:quotations \
		> META

install: META
	$(OCAMLFIND) remove pa_ppx_parsetree || true
	$(OCAMLFIND) install pa_ppx_parsetree META local-install/lib/*/*.*

uninstall:
	$(OCAMLFIND) remove pa_ppx_parsetree || true

clean::
	set -e; for i in $(SYSDIRS) $(TESTDIRS) $(OTHERCLEANDIRS); do cd $$i; $(MAKE) clean; cd ..; done
	rm -rf docs local-install $(BATCHTOP) META *.corrected

realclean:: clean
	set -e; for i in $(SYSDIRS) $(TESTDIRS) $(OTHERCLEANDIRS); do cd $$i; $(MAKE) realclean; cd ..; done

depend:
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) depend; cd ..; done
