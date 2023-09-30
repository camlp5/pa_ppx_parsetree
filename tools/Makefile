WD=$(shell pwd)
TOP=..

NOT_OCAMLFIND=not-ocamlfind
OCAMLFIND=ocamlfind
PACKAGES=re,fmt,unix,bos,pa_ppx.base,pa_ppx_regexp

EXE=extract-major-minor-ocaml-version

all: $(EXE)

extract-major-minor-ocaml-version: extract_major_minor_ocaml_version.ml
	$(OCAMLFIND) ocamlc -linkpkg -linkall -package $(PACKAGES) -syntax camlp5o $< -o $@

realclean clean::
	rm -f *.bak *.cm* $(EXE) META
