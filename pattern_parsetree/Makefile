# Makefile,v
# Copyright (c) INRIA 2007-2017

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
	rm -f parser.ml parser.mli lexer.ml *_mock*.ml* *.conflicts

parser.cmo: parser.ml
	$(LAUNCH) $(YAWRAP) $(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

parser.cmx: parser.ml
	$(LAUNCH) $(YAWRAP) $(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

parser.ml: parser.mly
	menhir --infer-write-query parser_mock.ml $<
	perl -p -e 's,# \d+.*,,' parser_mock.ml > parser_mock_nolines.ml
	ocamlfind ocamlc -package camlp5 -c parser_mock_nolines.ml || true
	ocamlfind ocamlc -package camlp5 -i parser_mock.ml > parser_mock.mli
	menhir --explain --infer-read-reply parser_mock.mli $<

lexer.ml: lexer.mll
	ocamllex lexer.mll

$(TARGET_PACKAGE).cmo $(TARGET_PACKAGE).cmi: $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -o $(TARGET_PACKAGE).cmo $^

$(TARGET_PACKAGE).cmx: $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -o $(TARGET_PACKAGE).cmx $^

$(TARGET): $(TARGET_PACKAGE).cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $^ -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(TARGET_PACKAGE).cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $^ -a -o $(TARGET:.cma=.cmxa)
