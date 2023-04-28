# Makefile,v
# Copyright (c) INRIA 2007-2017

TOP=.
include $(TOP)/config/Makefile.top

WD=$(shell pwd)
DESTDIR=

GENERATED_SYSDIRS = \
	quotations_500 quotations_414 \
	quotations_413 quotations_412 \
	quotations_411 quotations_410


SYSDIRS= pattern_parsetree.5.0.0 pattern_parsetree.4.14.0 \
	pattern_parsetree.4.13.1 pattern_parsetree.4.12.1 \
	pattern_parsetree.4.11.2 pattern_parsetree.4.10.2 \
	helpers runtime \
	$(GENERATED_SYSDIRS)

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

setup:
	set -e ; for v in 500 414 413 412 411 410; do \
	rm -rf quotations_$$v && cp -r quotations.TMPL quotations_$$v; \
	perl -p -i -e 's,VERSION,'$$v',g' quotations_$$v/mk_meta.ML quotations_$$v/q_parsetree.ml quotations_$$v/reorg_parsetree.ML quotations_$$v/Makefile quotations_$$v/.depend; \
	done

META: sys
	$(JOINMETA) \
		-rewrite pa_ppx_parsetree_pattern_parsetree_410:pa_ppx_parsetree.pattern_parsetree_410 \
		-wrap-subdir pattern_parsetree_410:pattern_parsetree.4.10.2 \
		-rewrite pa_ppx_parsetree_pattern_parsetree_411:pa_ppx_parsetree.pattern_parsetree_411 \
		-wrap-subdir pattern_parsetree_411:pattern_parsetree.4.11.2 \
		-rewrite pa_ppx_parsetree_pattern_parsetree_412:pa_ppx_parsetree.pattern_parsetree_412 \
		-wrap-subdir pattern_parsetree_412:pattern_parsetree.4.12.1 \
		-rewrite pa_ppx_parsetree_pattern_parsetree_413:pa_ppx_parsetree.pattern_parsetree_413 \
		-wrap-subdir pattern_parsetree_413:pattern_parsetree.4.13.1 \
		-rewrite pa_ppx_parsetree_pattern_parsetree_414:pa_ppx_parsetree.pattern_parsetree_414 \
		-wrap-subdir pattern_parsetree_414:pattern_parsetree.4.14.0 \
		-rewrite pa_ppx_parsetree_pattern_parsetree_500:pa_ppx_parsetree.pattern_parsetree_500 \
		-wrap-subdir pattern_parsetree_500:pattern_parsetree.5.0.0 \
		-rewrite pa_ppx_parsetree_helpers:pa_ppx_parsetree.helpers \
		-wrap-subdir helpers:helpers \
		-rewrite pa_ppx_parsetree_quotations_410:pa_ppx_parsetree.quotations_410 \
		-wrap-subdir quotations_410:quotations_410 \
		-rewrite pa_ppx_parsetree_quotations_411:pa_ppx_parsetree.quotations_411 \
		-wrap-subdir quotations_411:quotations_411 \
		-rewrite pa_ppx_parsetree_quotations_412:pa_ppx_parsetree.quotations_412 \
		-wrap-subdir quotations_412:quotations_412 \
		-rewrite pa_ppx_parsetree_quotations_413:pa_ppx_parsetree.quotations_413 \
		-wrap-subdir quotations_413:quotations_413 \
		-rewrite pa_ppx_parsetree_quotations_414:pa_ppx_parsetree.quotations_414 \
		-wrap-subdir quotations_414:quotations_414 \
		-rewrite pa_ppx_parsetree_quotations_500:pa_ppx_parsetree.quotations_500 \
		-wrap-subdir quotations_500:quotations_500 \
		> META

install: META
	$(OCAMLFIND) remove pa_ppx_parsetree || true
	$(OCAMLFIND) install pa_ppx_parsetree META local-install/lib/*/*.*

uninstall:
	$(OCAMLFIND) remove pa_ppx_parsetree || true

clean::
	set -e; for i in $(SYSDIRS) $(TESTDIRS) $(OTHERCLEANDIRS); do if [ -d $$i ]; then cd $$i; $(MAKE) clean; cd ..; fi; done
	rm -rf docs local-install $(BATCHTOP) META *.corrected

realclean:: clean
	set -e; for i in $(SYSDIRS) $(TESTDIRS) $(OTHERCLEANDIRS); do if [ -d $$i ]; then cd $$i; $(MAKE) realclean; cd ..; fi; done
	rm -rf $(GENERATED_SYSDIRS)

depend:
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) depend; cd ..; done
