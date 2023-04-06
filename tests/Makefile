WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

PACKAGES := $(PACKAGES),ounit,pa_ppx.utils,pa_ppx_parsetree_official_parsetree

ML= \
	via_camlp5_test.ml via_parsetree_test.ml test_show.ml

ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)

TESTS=via_camlp5_test.exe via_parsetree_test.exe test_show.exe

all: $(TESTS)

test:: all test1.TEST
	mkdir -p _build && ./via_camlp5_test.exe
	mkdir -p _build && ./via_parsetree_test.exe
	mkdir -p _build && ./test_show.exe

via_camlp5_test.exe: via_camlp5_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

via_parsetree_test.exe: via_parsetree_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

test_show.exe: test_show.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

export IMPORT_OCAMLCFLAGS := \
	-ppopt -pa_passthru-debug \
	-ppopt -pa_import-package -ppopt camlp5,pa_ppx_parsetree_via_parsetree \
	-ppopt -pa_import-I -ppopt . \

RPPFLAGS=-ppopt -l -ppopt 400 -ppopt -flag -ppopt D -ppopt -sep -ppopt '\n'
OPPFLAGS=-ppopt -l -ppopt 400 -ppopt -flag -ppopt M -ppopt -sep -ppopt '\n'
QUOTPACKAGES=pa_ppx_q_ast.quotation_test,pa_ppx_q_ast.test_cleanup
#QUOTPACKAGES=pa_ppx_q_ast.quotation_test

.SECONDARY:

%.TEST: %.patterns_out %.types_out
	diff -Bwiu $^ || true

%.types_out: %_types.ml
	$(LAUNCH) $(YAWRAP) $(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),camlp5.pr_r,$(QUOTPACKAGES) $(RPPFLAGS) $< > $@

%.patterns_out: %_patterns.ml
	$(LAUNCH) $(YAWRAP) $(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),camlp5.pr_r $(RPPFLAGS) $< > $@


clean::
	rm -rf *.cm* _build *.exe *_out

