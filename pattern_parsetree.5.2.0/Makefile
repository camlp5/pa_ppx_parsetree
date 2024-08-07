# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub
SHELL=$(BASH)

DESTDIR=

PACKAGES := fmt,camlp5,re,menhirLib
TARGET_PACKAGE = pa_ppx_parsetree_pattern_parsetree_502
TARGET=$(TARGET_PACKAGE).cma

ML = shims.ml build_path_prefix_map.ml misc.ml warnings.ml clflags.ml location.ml pattern_misc.ml longident.ml asttypes.ml parsetree.ml syntaxerr.ml \
	ast_helper.ml parser.ml lexer.ml pprintast.ml parse.ml printast.ml
MLI = build_path_prefix_map.mli misc.mli warnings.mli clflags.mli location.mli longident.mli asttypes.mli parsetree.mli syntaxerr.mli \
	ast_helper.mli lexer.mli pprintast.mli parse.mli printast.mli

TOOLSML = mk_meta.ml
ALLSRC=$(ML) $(MLI) $(TOOLSML) parser_mock.ml parser_mock.out.mli
ALLPACKAGES=$(PACKAGES)

OCAMLCFLAGS := $(OCAMLCFLAGS) -linkall -for-pack Pa_ppx_parsetree_pattern_parsetree_502

all: $(TARGET) $(TARGET:.cma=.cmxa) META
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

DEFAULT_INSTALL_FILES:=
TARGET_INSTALL_FILES= $(PACKED_ARCHIVE_TARGET_INSTALL_FILES)
install:: default_install

clean::
	rm -rf META mk_meta mk_meta.ml

realclean:: clean
	rm -f parser.ml parser.mli lexer.ml *_mock* *.conflicts parser_expanded.mly shims.ml

parser.cmo: parser.ml
	$(LAUNCH) $(YAWRAP) $(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

parser.cmx: parser.ml
	$(LAUNCH) $(YAWRAP) $(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

parser.ml parser.mli: parser_mock.out.mli
	menhir --table --explain --infer-read-reply parser_mock.out.mli parser.mly

parser_mock.out.mli : \
    syntaxerr.cmi \
    parsetree.cmi \
    longident.cmi \
    location.cmi \
    clflags.cmi \
    asttypes.cmi \
    ast_helper.cmi


parser_mock.ml: parser.mly
	menhir --infer-write-query parser_mock.ml $<

parser_mock.out.mli: parser_mock.ml
	ocamlfind ocamlc -package camlp5 -i parser_mock.ml > parser_mock.out.mli.NEW && mv parser_mock.out.mli.NEW parser_mock.out.mli

lexer.ml: lexer.mll
	ocamllex lexer.mll

shims.ml: shims.ML
	cppo -V OCAML:$(ocamlVERSION) $< > $@.NEW && mv $@.NEW $@

PARSINGSRC=../adjusted-parsing.5.2.0
%.COMPARE: %
	$(TOP)/tools/erase-vala $< | $(TOP)/tools/paren2ws | $(DIFF) -Bbwiu <( $(TOP)/tools/paren2ws $(PARSINGSRC)/$< ) - || /bin/true

COMPARE= \
	ast_helper.ml.COMPARE ast_helper.mli.COMPARE \
	ast_iterator.ml.COMPARE \
	asttypes.mli.COMPARE \
	builtin_attributes.ml.COMPARE \ builtin_attributes.mli.COMPARE \
	docstrings.ml.COMPARE docstrings.mli.COMPARE \
	lexer.mll.COMPARE \
	location.ml.COMPARE location.mli.COMPARE \
	longident.ml.COMPARE longident.mli.COMPARE \
	parse.ml.COMPARE parse.mli.COMPARE \
	parser.mly.COMPARE \
	parsetree.mli.COMPARE \
	parsetree.ml.COMPARE parsetree.mli.COMPARE \
	pprintast.ml.COMPARE pprintast.mli.COMPARE \
	printast.ml.COMPARE printast.mli.COMPARE \
	syntaxerr.ml.COMPARE syntaxerr.mli.COMPARE \
	warnings.ml.COMPARE warnings.mli.COMPARE \


compare:: $(COMPARE)

.SUFFIXES: .COMPARE

$(TARGET:.cma=.cmo) $(TARGET:.cma=.cmi): $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -o $@ $^

$(TARGET:.cma=.cmx): $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -o $@ $^

$(TARGET): $(TARGET:.cma=.cmo)
	$(OCAMLFIND) ocamlc $(DEBUG) $^ -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(TARGET:.cma=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) $^ -a -o $(TARGET:.cma=.cmxa)
