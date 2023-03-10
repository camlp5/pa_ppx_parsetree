WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

PACKAGES := $(PACKAGES),pa_ppx_parsetree_pa_parsetree,pa_ppx_parsetree_pattern_parsetree,ounit2,pa_ppx.utils

ML= \
	pa_parsetree_test.ml pattern_parsetree_test.ml

ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)

TESTS=pa_parsetree_test.exe pattern_parsetree_test.exe

all: $(TESTS)


test:: all

pa_parsetree_test.exe: pa_parsetree_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

pattern_parsetree_test.exe: pattern_parsetree_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

clean::
	rm -rf *.cm* _build *.exe

