# Makefile,v
# Copyright (c) INRIA 2007-2017

TOP=.
include $(TOP)/config/Makefile.top

WD=$(shell pwd)
DESTDIR=

GENERATED_SYSDIRS = \
	quotations_505 \
	quotations_504 \
	quotations_503 \
	quotations_502 \
	quotations_501 \
	quotations_500 quotations_414 \
	quotations_413 quotations_412 \
	quotations_411 quotations_410 \
	pattern_parsetree_gen quotations_gen

SYSDIRS= tools \
	pattern_parsetree.5.5.0 quotations_505 \
	pattern_parsetree.5.4.0 quotations_504 \
	pattern_parsetree.5.3.0 quotations_503 \
	pattern_parsetree.5.2.0 quotations_502 \
	pattern_parsetree.5.1.0 quotations_501 \
	pattern_parsetree.5.0.0 quotations_500 \
	pattern_parsetree.4.14.0 quotations_414 \
	pattern_parsetree.4.13.1 quotations_413 \
	pattern_parsetree.4.12.1 quotations_412 \
	pattern_parsetree.4.11.2 quotations_411 \
	pattern_parsetree.4.10.2 quotations_410 \
	helpers runtime \
	pattern_parsetree_gen quotations_gen

OTHERCLEANDIRS=\
	adjusted-parsing.4.10.2 adjusted-parsing.4.11.2 adjusted-parsing.4.12.1 \
	adjusted-parsing.4.13.1 adjusted-parsing.4.14.0 adjusted-parsing.5.0.0 \
	adjusted-parsing.5.1.0 adjusted-parsing.5.2.0 adjusted-parsing.5.3.0 \
	adjusted-parsing.5.4.0 adjusted-parsing.5.5.0 \
	pattern_parsetree.4.10.2 pattern_parsetree.4.11.2 pattern_parsetree.4.12.1 \
	pattern_parsetree.4.13.1 pattern_parsetree.4.14.0 pattern_parsetree.5.0.0 \
	pattern_parsetree.5.1.0 pattern_parsetree.5.2.0 pattern_parsetree.5.3.0 \
	pattern_parsetree.5.4.0 pattern_parsetree.5.5.0 \

TESTDIRS= tests

all: sys
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) all; cd ..; done

sys:
	set -e; for i in $(SYSDIRS); do cd $$i; $(MAKE) all; cd ..; done

test: all mdx-test
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) test; cd ..; done

mdx-test:: README.asciidoc.TEST

version-parallel:
	set -e ; for v in 505 504 503 502 501 500 414 413 412 411 410; do \
	$(MAKE) $(MAKEFLAGS) version.$$v & \
	done; \
	wait

version.505:
	$(MAKE) -C pattern_parsetree.5.5.0
	$(MAKE) -C quotations_505

version.504:
	$(MAKE) -C pattern_parsetree.5.4.0
	$(MAKE) -C quotations_504

version.503:
	$(MAKE) -C pattern_parsetree.5.3.0
	$(MAKE) -C quotations_503

version.502:
	$(MAKE) -C pattern_parsetree.5.2.0
	$(MAKE) -C quotations_502

version.501:
	$(MAKE) -C pattern_parsetree.5.1.0
	$(MAKE) -C quotations_501

version.500:
	$(MAKE) -C pattern_parsetree.5.0.0
	$(MAKE) -C quotations_500

version.414:
	$(MAKE) -C pattern_parsetree.4.14.0
	$(MAKE) -C quotations_414

version.413:
	$(MAKE) -C pattern_parsetree.4.13.1
	$(MAKE) -C quotations_413

version.412:
	$(MAKE) -C pattern_parsetree.4.12.1
	$(MAKE) -C quotations_412

version.411:
	$(MAKE) -C pattern_parsetree.4.11.2
	$(MAKE) -C quotations_411

version.410:
	$(MAKE) -C pattern_parsetree.4.10.2
	$(MAKE) -C quotations_410


OVERS=$(shell $(TOP)/tools/extract-major-minor-ocaml-version $(ocamlVERSION))

tools::
	$(MAKE) -C tools all

setup: tools
	set -e ; for v in 505 504 503 502 501 500 414 413 412 411 410; do \
	rm -rf quotations_$$v && cp -r quotations.TMPL quotations_$$v; \
	perl -p -i -e 's,VERSION,'$$v',g' quotations_$$v/mk_meta.ML quotations_$$v/q_parsetree.ml quotations_$$v/reorg_parsetree.ML quotations_$$v/Makefile quotations_$$v/.depend; \
	done
	rm -rf quotations_gen && cp -r quotations_gen.TMPL quotations_gen && perl -p -i -e 's,VERSION,'$(OVERS)',g' quotations_gen/*
	rm -rf pattern_parsetree_gen && cp -r pattern_parsetree_gen.TMPL pattern_parsetree_gen && perl -p -i -e 's,VERSION,'$(OVERS)',g' pattern_parsetree_gen/*

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
		\
		-rewrite pa_ppx_parsetree_pattern_parsetree_501:pa_ppx_parsetree.pattern_parsetree_501 \
		-wrap-subdir pattern_parsetree_501:pattern_parsetree.5.1.0 \
		\
		-rewrite pa_ppx_parsetree_pattern_parsetree_502:pa_ppx_parsetree.pattern_parsetree_502 \
		-wrap-subdir pattern_parsetree_502:pattern_parsetree.5.2.0 \
		\
		-rewrite pa_ppx_parsetree_pattern_parsetree_503:pa_ppx_parsetree.pattern_parsetree_503 \
		-wrap-subdir pattern_parsetree_503:pattern_parsetree.5.3.0 \
		\
		-rewrite pa_ppx_parsetree_pattern_parsetree_504:pa_ppx_parsetree.pattern_parsetree_504 \
		-wrap-subdir pattern_parsetree_504:pattern_parsetree.5.4.0 \
		\
		-rewrite pa_ppx_parsetree_pattern_parsetree_505:pa_ppx_parsetree.pattern_parsetree_505 \
		-wrap-subdir pattern_parsetree_505:pattern_parsetree.5.5.0 \
		\
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
		\
		-rewrite pa_ppx_parsetree_quotations_501:pa_ppx_parsetree.quotations_501 \
		-wrap-subdir quotations_501:quotations_501 \
		\
		-rewrite pa_ppx_parsetree_quotations_502:pa_ppx_parsetree.quotations_502 \
		-wrap-subdir quotations_502:quotations_502 \
		\
		-rewrite pa_ppx_parsetree_quotations_503:pa_ppx_parsetree.quotations_503 \
		-wrap-subdir quotations_503:quotations_503 \
		\
		-rewrite pa_ppx_parsetree_quotations_504:pa_ppx_parsetree.quotations_504 \
		-wrap-subdir quotations_504:quotations_504 \
		\
		-rewrite pa_ppx_parsetree_quotations_505:pa_ppx_parsetree.quotations_505 \
		-wrap-subdir quotations_505:quotations_505 \
		\
		-rewrite pa_ppx_parsetree_quotations:pa_ppx_parsetree.quotations \
		-wrap-subdir quotations:quotations_gen \
		> META

install: META
	$(OCAMLFIND) remove pa_ppx_parsetree || true
	$(OCAMLFIND) install pa_ppx_parsetree META local-install/lib/*/*.*

uninstall:
	$(OCAMLFIND) remove pa_ppx_parsetree || true

clean::
	set -e; for i in $(SYSDIRS) $(TESTDIRS) $(OTHERCLEANDIRS); do if [ -d $$i ]; then cd $$i; $(MAKE) clean; cd ..; fi; done
	rm -rf docs local-install $(BATCHTOP) META *.corrected

realclean::
	set -e; for i in $(SYSDIRS) $(TESTDIRS) $(OTHERCLEANDIRS); do if [ -d $$i ]; then cd $$i; $(MAKE) realclean; cd ..; fi; done
	rm -rf $(GENERATED_SYSDIRS)

depend:
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) depend; cd ..; done
