WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

PACKAGES := $(PACKAGES),ounit2,pa_ppx.utils,pa_ppx_parsetree_helpers

ML= \
	quotations_test.ml quotations_noattr_test.ml antiquotation_lexing_test.ml test_show.ml

ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)

TESTS=quotations_test.exe quotations_noattr_test.exe antiquotation_lexing_test.exe test_show.exe

all: coverage-Makefile $(TESTS)

test:: all
	mkdir -p _build/coverage
	$(MAKE) test1.TEST
	./quotations_test.exe
	./quotations_noattr_test.exe
	./antiquotation_lexing_test.exe
	./test_show.exe

quotations_test.exe: quotations_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

antiquotation_lexing_test.exe: antiquotation_lexing_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

quotations_noattr_test.exe: quotations_noattr_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

test_show.exe: test_show.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

OVERS=$(shell $(TOP)/tools/extract-major-minor-ocaml-version $(ocamlVERSION))
IMPORT_PACKAGES := camlp5,pa_ppx_parsetree_quotations_$(OVERS)

RPPFLAGS=-ppopt -l -ppopt 400 -ppopt -flag -ppopt D -ppopt -sep -ppopt '\n'
OPPFLAGS=-ppopt -l -ppopt 400 -ppopt -flag -ppopt M -ppopt -sep -ppopt '\n'
QUOTPACKAGES=pa_ppx_q_ast.quotation_test,pa_ppx_q_ast.test_cleanup
#QUOTPACKAGES=pa_ppx_q_ast.quotation_test
#QUOTPACKAGES=

.SECONDARY:

%.TEST: %.patterns_out %.types_out
	$(DIFF) -Bwiu $^

%.types_out: %_types.ml
	$(LAUNCH) $(YAWRAP) $(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),camlp5.pr_r,$(QUOTPACKAGES),$(IMPORT_PACKAGES) $(RPPFLAGS) $< > $@.TMP && mv $@.TMP $@

%.patterns_out: %_patterns.ml
	$(LAUNCH) $(YAWRAP) $(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),camlp5.pr_r $(RPPFLAGS) $< > $@.TMP && mv $@.TMP $@

test1_patterns.ml: test1_patterns.ML
	cppo -D 'OVERS '$(OVERS) $< > $@.TMP && mv $@.TMP $@

test1_types.ml: test1_types.ML
	cppo -D 'OVERS '$(OVERS) -D 'MODULE Pa_ppx_parsetree_quotations_'$(OVERS) $< > $@

-include coverage-Makefile
coverage-Makefile: Makefile make-coverage
	./make-coverage 505 504 503 502 501 500 414 413 412 411 410 > $@.TMP && mv $@.TMP $@

quotations_test.ml: quotations_test.ML
	cppo -V OCAML:$(ocamlVERSION) $< > $@

test_show.ml: test_show.ML
	cppo -V OCAML:$(ocamlVERSION) $< > $@

clean::
	rm -rf *.cm* _build *.exe *_out test1_patterns.ml test1_types.ml quotations_test.ml test_show.ml

