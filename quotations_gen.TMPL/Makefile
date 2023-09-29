# Makefile,v
# Copyright (c) INRIA 2007-2017

SHELL := $(BASH)
WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := pa_ppx_parsetree_quotations_VERSION
TARGET_PACKAGE = pa_ppx_parsetree_quotations

ML = pa_ppx_parsetree_quotations.ml

TOOLSML = mk_meta.ml
ALLSRC=$(ML) $(MLI) $(TOOLSML)
ALLPACKAGES=$(PACKAGES)

OCAMLCFLAGS := $(OCAMLCFLAGS) -linkall

all: $(FILES_TARGET_INSTALL_FILES) META
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

TARGET_INSTALL_FILES= $(FILES_TARGET_INSTALL_FILES)
install:: default_install

clean::
	rm -rf META mk_meta mk_meta.ml

realclean:: clean
