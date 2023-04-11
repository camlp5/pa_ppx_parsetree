# Makefile,v
# Copyright (c) INRIA 2007-2017

TOP=.
include $(TOP)/config/Makefile.top

WD=$(shell pwd)
DESTDIR=

SYSDIRS= pattern_parsetree.$(ocamlVERSION) official_parsetree runtime via_camlp5 via_parsetree
OTHERCLEANDIRS=\
	adjusted-parsing.4.14.0 adjusted-parsing.5.0.0 \
	pattern_parsetree.4.14.0 pattern_parsetree.5.0.0 \

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
		-rewrite pa_ppx_parsetree_pattern_paretree:pa_ppx_parsetree.pattern_parsetree \
		-wrap-subdir pattern_parsetree:pattern_parsetree \
		-rewrite pa_ppx_parsetree_official_paretree:pa_ppx_parsetree.official_parsetree \
		-wrap-subdir official_parsetree:official_parsetree \
		-rewrite pa_ppx_parsetree_via_camlp5:pa_ppx_parsetree.via_camlp5 \
		-wrap-subdir via_camlp5:via_camlp5 \
		-rewrite pa_ppx_parsetree_via_parsetree:pa_ppx_parsetree.via_parsetree \
		-wrap-subdir via_parsetree:via_parsetree \
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
