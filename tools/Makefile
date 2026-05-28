WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

NOT_OCAMLFIND=not-ocamlfind
OCAMLFIND=ocamlfind
PACKAGES=re,fmt,unix,bos,pa_ppx.base,pa_ppx_regexp

EXE=extract-major-minor-ocaml-version
ML=extract_major_minor_ocaml_version.ml

all: $(EXE)

extract-major-minor-ocaml-version: extract_major_minor_ocaml_version.ml
	$(OCAMLFIND) ocamlc -linkpkg -linkall -package $(PACKAGES) -syntax camlp5o $< -o $@

realclean clean::
	rm -f *.bak *.cm* $(EXE) META
