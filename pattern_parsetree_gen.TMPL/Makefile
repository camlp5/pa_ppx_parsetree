# Makefile,v
# Copyright (c) INRIA 2007-2017

SHELL := /bin/bash
WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := fmt,camlp5,re,menhirLib,pa_ppx_parsetree_pattern_parsetree_VERSION
TARGET_PACKAGE = pa_ppx_parsetree_pattern_parsetree

ML = pa_ppx_parsetree_pattern_parsetree.ml

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
