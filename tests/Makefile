WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

PACKAGES := $(PACKAGES),ounit2,pa_ppx.utils,pa_ppx_parsetree_official_parsetree

ML= \
	via_camlp5_test.ml via_parsetree_test.ml

ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)

TESTS=via_camlp5_test.exe via_parsetree_test.exe

all: $(TESTS)


test:: all
	mkdir -p _build && ./via_camlp5_test.exe
	mkdir -p _build && ./via_parsetree_test.exe

via_camlp5_test.exe: via_camlp5_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

via_parsetree_test.exe: via_parsetree_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

clean::
	rm -rf *.cm* _build *.exe

