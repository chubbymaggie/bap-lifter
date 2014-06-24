BUILDFLAGS=-cflags -principal,-strict-sequence,-warn-error,+a,-w,+a-4-44
OCB=ocamlbuild
OCBF=
OCF=ocamlfind
OCI=ocp-indent
LLVM_TREE=
LLVM_MODE=Debug+Asserts

# OASIS_START
# DO NOT EDIT (digest: a3c674b4239234cbbe53afe090018954)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

# Architecture generation rules
mc/%/reg.ml:
	$(LLVM_TREE)/$(LLVM_MODE)/bin/llvm-tblgen -I $(LLVM_TREE)/include -I $(LLVM_TREE)/lib/Target/$* $(LLVM_TREE)/lib/Target/$*/$*.td -gen-register-info | perl parse-tblgen/gen-register-info.pl > $@ 2>/dev/null

mc/%/opcode.ml:
	$(LLVM_TREE)/$(LLVM_MODE)/bin/llvm-tblgen -I $(LLVM_TREE)/include -I $(LLVM_TREE)/lib/Target/$* $(LLVM_TREE)/lib/Target/$*/$*.td -gen-instr-info | perl parse-tblgen/gen-instr-info.pl > $@ 2>/dev/null

mc/%/_tags:
	echo 'not <$*.cmx> : for-pack(Mc.$*)' > $@
	echo '<$.cmx> : for-pack(Mc)' >> $@

mc/%/%.mlpack:
	echo -e 'Cond\nEnum\nOpcode\nReg' > $@

LLVM_GENERATED=mc/ARM/reg.ml mc/ARM/opcode.ml

.PHONY: llvm-gen
llvm-gen: $(LLVM_GENERATED)
.PHONY: llvm-clean
llvm-clean:
	rm $(LLVM_GENERATED)

.PHONY: check
check: ocp-indent-check
ocp-indent-check:
	find . -name "*.ml" -print0 | xargs -n 1 -0 -Imlfile bash -c "$(OCI) mlfile | diff - mlfile"

.PHONY: ocp-indent-auto
ocp-indent-auto:
	find . -name "*.ml" -print0 | xargs -n 1 -0 -Imlfile $(OCI) -i mlfile
