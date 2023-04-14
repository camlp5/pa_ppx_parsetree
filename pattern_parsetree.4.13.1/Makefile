# Makefile,v
# Copyright (c) INRIA 2007-2017

SHELL := /bin/bash
WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := fmt,camlp5,re
TARGET_PACKAGE = pa_ppx_parsetree_pattern_parsetree
TARGET=$(TARGET_PACKAGE).cma

ML = clflags.ml location.ml pattern_misc.ml longident.ml asttypes.ml parsetree.ml docstrings.ml syntaxerr.ml \
	ast_helper.ml parser.ml lexer.ml pprintast.ml parse.ml printast.ml
MLI = clflags.mli location.mli longident.mli asttypes.mli parsetree.mli docstrings.mli syntaxerr.mli \
	ast_helper.mli lexer.mli pprintast.mli parse.mli printast.mli

TOOLSML = mk_meta.ml
ALLSRC=$(ML) $(MLI) $(TOOLSML)
ALLPACKAGES=$(PACKAGES)

OCAMLCFLAGS := $(OCAMLCFLAGS) -linkall -for-pack Pa_ppx_parsetree_pattern_parsetree

all: $(TARGET) $(TARGET:.cma=.cmxa) META
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

TARGET_INSTALL_FILES= $(ARCHIVE_TARGET_INSTALL_FILES)
install:: default_install

clean::
	rm -rf META mk_meta mk_meta.ml

realclean:: clean
	rm -f parser.ml parser.mli lexer.ml *_mock*.ml* *.conflicts parser_expanded.mly

parser.cmo: parser.ml
	$(LAUNCH) $(YAWRAP) $(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

parser.cmx: parser.ml
	$(LAUNCH) $(YAWRAP) $(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

parser.ml: parser.mly
	menhir --only-preprocess $< > parser_expanded.mly
	menhir --infer-write-query parser_mock.ml $<
	perl -p -e 's,# \d+.*,,' parser_mock.ml > parser_mock_nolines.ml
	ocamlfind ocamlc -package camlp5 -c parser_mock_nolines.ml || true
	ocamlfind ocamlc -package camlp5 -i parser_mock.ml > parser_mock.mli
	menhir --explain --infer-read-reply parser_mock.mli $<

lexer.ml: lexer.mll
	ocamllex lexer.mll

PARSINGSRC=../adjusted-parsing.4.13.1
%.COMPARE: %
	$(TOP)/tools/erase-vala $< | $(TOP)/tools/paren2ws | diff -Bbwiu <( $(TOP)/tools/paren2ws $(PARSINGSRC)/$< ) -

COMPARE= \
	location.ml.COMPARE location.mli.COMPARE \
	longident.ml.COMPARE longident.mli.COMPARE \
	asttypes.mli.COMPARE \
	parsetree.mli.COMPARE \
	docstrings.ml.COMPARE docstrings.mli.COMPARE \
	syntaxerr.ml.COMPARE syntaxerr.mli.COMPARE \
	ast_helper.ml.COMPARE ast_helper.mli.COMPARE \
	parser.mly.COMPARE parser.mly.COMPARE \
	parse.ml.COMPARE parse.ml.COMPARE \
	parse.mli.COMPARE parse.mli.COMPARE \
	pprintast.ml.COMPARE pprintast.mli.COMPARE \
	printast.ml.COMPARE printast.mli.COMPARE \

compare:: $(COMPARE)

.SUFFIXES: .COMPARE

$(TARGET_PACKAGE).cmo $(TARGET_PACKAGE).cmi: $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -o $(TARGET_PACKAGE).cmo $^

$(TARGET_PACKAGE).cmx: $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -o $(TARGET_PACKAGE).cmx $^

$(TARGET): $(TARGET_PACKAGE).cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $^ -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(TARGET_PACKAGE).cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $^ -a -o $(TARGET:.cma=.cmxa)