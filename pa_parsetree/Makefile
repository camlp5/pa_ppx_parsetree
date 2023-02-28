# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := $(PACKAGES),fmt,pa_ppx.base,pa_ppx.deriving_plugins.std
TARGET_PACKAGE = pa_ppx_parsetree
TARGET=$(TARGET_PACKAGE).cma

ML = oCast.ml pattern_OCast.ml supportfuns.ml mLast2Pattern_OCast.ml import_ocast.ml pa_ocast.ml q_ocast.ml

TOOLSML = mk_meta.ml
ALLSRC=$(ML) $(TOOLSML)
ALLPACKAGES=$(PACKAGES)

OCAMLCFLAGS := $(OCAMLCFLAGS) -linkall -for-pack Pa_ppx_parsetree

export MIGRATE_OCAMLCFLAGS= -ppopt -pa_import-I -ppopt . -ppopt -pa_passthru-debug \
	-ppopt -pa_import-package -ppopt $(PACKAGES)
export MIGRATE_PACKAGES=pa_ppx.import,pa_ppx_migrate

all: $(TARGET) $(TARGET:.cma=.cmxa) camlp5.pa_ppx_parsetree camlp5.pa_ppx_parsetree.opt META
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

camlp5.pa_ppx_parsetree: $(CMO)
	$(MKCAMLP5) -package fmt,re,camlp5.pa_r,camlp5.pr_r,pa_ppx.base,pa_ppx_q_ast.runtime $(CMO) -o $@

camlp5.pa_ppx_parsetree.opt: $(CMX)
	$(MKCAMLP5OPT) -package fmt,re,camlp5.pa_r,camlp5.pr_r,pa_ppx.base,pa_ppx_q_ast.runtime $(CMX) -o $@

TARGET_INSTALL_FILES= $(ARCHIVE_TARGET_INSTALL_FILES)
install:: default_install

clean::
	rm -rf META camlp5.pa_ppx_parsetree* mk_meta mk_meta.ml

$(TARGET_PACKAGE).cmo $(TARGET_PACKAGE).cmi: $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -o $(TARGET_PACKAGE).cmo $^

$(TARGET_PACKAGE).cmx: $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -o $(TARGET_PACKAGE).cmx $^

$(TARGET): $(TARGET_PACKAGE).cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $^ -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(TARGET_PACKAGE).cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $^ -a -o $(TARGET:.cma=.cmxa)
