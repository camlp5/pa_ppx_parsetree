# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := fmt,compiler-libs.common
TARGET_PACKAGE = pa_ppx_parsetree_parsing_5_0_0
TARGET=$(TARGET_PACKAGE).cma

ML = location.ml longident.ml docstrings.ml syntaxerr.ml \
	ast_helper.ml parser.ml lexer.ml pprintast.ml parse.ml printast.ml
MLI = location.mli longident.mli asttypes.mli parsetree.mli docstrings.mli syntaxerr.mli \
	ast_helper.mli lexer.mli pprintast.mli parse.mli printast.mli

ALLSRC=$(ML) $(MLI) $(TOOLSML)
ALLPACKAGES=$(PACKAGES)

all: $(CMO)

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


PARSINGSRC=../parsing.4.14.0
%.COMPARE: %
	$(DIFF) -Bbwiu $(PARSINGSRC)/$< $< || true

COMPARE= \
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
