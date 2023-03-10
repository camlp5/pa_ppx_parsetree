# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := fmt,camlp5,pa_ppx_parsetree_pattern_parsetree
TARGET_PACKAGE = pa_ppx_parsetree_via_parsetree
TARGET=$(TARGET_PACKAGE).cma

ML = reorg_parsetree.ml q_parsetree.ml

TOOLSML = mk_meta.ml
ALLSRC=$(ML) $(MLI) $(TOOLSML)
ALLPACKAGES=$(PACKAGES)

OCAMLCFLAGS := $(OCAMLCFLAGS) -linkall -for-pack Pa_ppx_parsetree_via_parsetree

export MIGRATE_OCAMLCFLAGS= \
	-ppopt -pa_passthru-debug \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \
	-ppopt -pa_import-I -ppopt . \

export MIGRATE_PACKAGES=pa_ppx.import,pa_ppx_migrate

all: $(TARGET) $(TARGET:.cma=.cmxa) META
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

TARGET_INSTALL_FILES= $(ARCHIVE_TARGET_INSTALL_FILES)
install:: default_install

clean::
	rm -rf META mk_meta mk_meta.ml

realclean:: clean
	rm -f parser.ml parser.mli lexer.ml *_mock.ml* *.conflicts

parser.cmo: parser.ml
	$(LAUNCH) $(YAWRAP) $(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

parser.cmx: parser.ml
	$(LAUNCH) $(YAWRAP) $(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

parser.ml: parser.mly
	menhir --infer-write-query parser_mock.ml $<
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
